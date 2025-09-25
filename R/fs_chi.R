# Load necessary packages
library(data.table)
library(furrr)
library(future)

# =========================
# Public API
# =========================

#' Chi-square feature selection for categorical features
#'
#' @description
#' Tests association between each categorical feature and a (categorical) target
#' via the chi-square test of independence. Handles missing values per-feature,
#' switches to simulation-based p-values for low expected counts, and supports
#' multiple-testing correction.
#'
#' @param data A data.frame or data.table with features and target.
#' @param target_col Character scalar: name of the target column.
#' @param sig_level Numeric threshold for significance (default 0.05).
#' @param continuity_correction NULL/TRUE/FALSE: apply Yates correction for 2x2.
#'   If NULL (default), auto-apply when table is 2x2.
#' @param p_adjust_method Character: one of \code{p.adjust.methods} (default "bonferroni").
#'   Set to "none" to disable multiple-testing correction.
#' @param simulation_B Integer reps for simulation-based p-values when expected
#'   counts are low (default 2000).
#' @param parallel Logical; if TRUE, run features in parallel using furrr.
#' @param temp_multisession Logical; if TRUE and \code{parallel=TRUE}, temporarily
#'   set a multisession plan and restore the previous plan on exit.
#' @param verbose Logical; if TRUE, prints informative messages.
#'
#' @return A list with:
#' \describe{
#'   \item{results}{data.frame with one row per feature: feature, n, df, p_value,
#'         adj_p_value, significant, method ("asymptotic" or "simulation"),
#'         correction_applied (TRUE/FALSE), min_expected (minimum expected count).}
#'   \item{significant_features}{Character vector of features with adj_p_value < sig_level.}
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' target <- factor(rep(c("Yes","No"), each=250))
#' f1 <- factor(c(rep(c("A","B"), c(200,50)), rep(c("A","B","C"), c(50,100,100))))
#' f2 <- factor(sample(c("X","Y"), 500, TRUE))
#' f3 <- factor(c(rep(c("M","N"), c(150,100)), rep(c("M","N"), c(100,150))))
#' d <- data.frame(f1, f2, f3, target)
#'
#' out <- fs_chi(d, "target", parallel = TRUE, temp_multisession = TRUE)
#' out$results
#' out$significant_features
#' }
#'
#' @importFrom data.table as.data.table
#' @importFrom furrr future_map
#' @importFrom stats chisq.test p.adjust p.adjust.methods
#' @export
fs_chi <- function(
    data,
    target_col,
    sig_level = 0.05,
    continuity_correction = NULL,
    p_adjust_method = "bonferroni",
    simulation_B = 2000,
    parallel = TRUE,
    temp_multisession = FALSE,
    verbose = FALSE
) {
  
  # ---- 1) Validate & prepare ----
  dt <- .fs_validate_and_prepare_data(data, target_col, verbose = verbose)
  
  # Identify categorical features (factors) to test
  feature_cols <- .fs_get_factor_features(dt, target_col)
  if (length(feature_cols) == 0L) {
    if (verbose) message("No categorical (factor) features found for testing.")
    empty <- data.frame(
      feature = character(0), n = integer(0), df = integer(0),
      p_value = numeric(0), adj_p_value = numeric(0),
      significant = logical(0), method = character(0),
      correction_applied = logical(0), min_expected = numeric(0),
      stringsAsFactors = FALSE
    )
    return(list(results = empty, significant_features = character(0)))
  }
  
  # ---- 2) Define worker for one feature ----
  worker <- function(feat) {
    .fs_test_feature(
      dt = dt,
      feature = feat,
      target_col = target_col,
      continuity_correction = continuity_correction,
      simulation_B = simulation_B,
      verbose = verbose
    )
  }
  
  # ---- 3) Possibly set a temporary parallel plan ----
  if (parallel && temp_multisession) {
    old_plan <- future::plan()
    on.exit({ try(future::plan(old_plan), silent = TRUE) }, add = TRUE)
    future::plan(multisession)
  }
  
  # ---- 4) Execute tests ----
  rows <- if (parallel) {
    furrr::future_map(feature_cols, worker, .options = furrr::furrr_options(seed = TRUE))
  } else {
    lapply(feature_cols, worker)
  }
  
  res <- .fs_bind_results(rows)
  
  # ---- 5) Adjust p-values & finalize ----
  res$adj_p_value <- .fs_adjust_pvalues(res$p_value, method = p_adjust_method)
  res$significant <- !is.na(res$adj_p_value) & (res$adj_p_value < sig_level)
  
  # stable ordering: by adjusted p-value then raw
  ord <- order(res$adj_p_value, res$p_value, na.last = TRUE)
  res <- res[ord, , drop = FALSE]
  
  list(
    results = res,
    significant_features = res$feature[res$significant]
  )
}


# =========================
# Internal helpers
# =========================

# Validate inputs, coerce characters to factor, ensure target is factor with >=2 levels
.fs_validate_and_prepare_data <- function(data, target_col, verbose = FALSE) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame or data.table.")
  if (!is.character(target_col) || length(target_col) != 1L)
    stop("`target_col` must be a single column name (character).")
  if (!target_col %in% names(data))
    stop("`target_col` must be a column in `data`.")
  
  dt <- as.data.table(data)
  
  # Coerce character columns (including target if character) to factor
  char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
  if (length(char_cols) > 0L) {
    dt[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]
  }
  
  # Ensure target is factor
  if (!is.factor(dt[[target_col]])) {
    if (verbose) message(sprintf("Coercing target '%s' to factor.", target_col))
    dt[[target_col]] <- as.factor(dt[[target_col]])
  }
  
  # Ensure target has >= 2 levels after removing NAs
  tgt <- droplevels(dt[[target_col]][!is.na(dt[[target_col]])])
  if (nlevels(tgt) < 2L) {
    stop("Target must have at least 2 non-NA levels for chi-square testing.")
  }
  
  dt
}

# Return names of factor features excluding target
.fs_get_factor_features <- function(dt, target_col) {
  candidates <- setdiff(names(dt), target_col)
  candidates[vapply(dt[, ..candidates], is.factor, logical(1))]
}

# Build a contingency table safely, dropping NAs and empty levels
.fs_build_contingency <- function(dt, feature, target_col) {
  valid <- !is.na(dt[[feature]]) & !is.na(dt[[target_col]])
  if (!any(valid)) return(NULL)
  
  x <- droplevels(dt[[feature]][valid])
  y <- droplevels(dt[[target_col]][valid])
  
  if (nlevels(x) < 2L || nlevels(y) < 2L) return(NULL)
  
  tab <- table(x, y)
  if (nrow(tab) < 2L || ncol(tab) < 2L) return(NULL)
  tab
}

# Decide whether to use simulation and whether to apply continuity correction
.fs_choose_test <- function(tab, continuity_correction, simulation_B) {
  # Initial test (no correction) to inspect expected counts
  init <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  expected <- init$expected
  min_expected <- min(expected)
  
  use_sim <- any(expected < 5)
  
  if (use_sim) {
    list(
      p = stats::chisq.test(tab, simulate.p.value = TRUE, B = simulation_B)$p.value,
      method = "simulation",
      correction = FALSE,
      min_expected = min_expected,
      df = init$parameter
    )
  } else {
    do_corr <- if (nrow(tab) == 2L && ncol(tab) == 2L) {
      if (is.null(continuity_correction)) TRUE else isTRUE(continuity_correction)
    } else {
      FALSE
    }
    test <- stats::chisq.test(tab, correct = do_corr)
    list(
      p = test$p.value,
      method = "asymptotic",
      correction = do_corr,
      min_expected = min_expected,
      df = test$parameter
    )
  }
}

# Test one feature and return a named list (row)
.fs_test_feature <- function(dt, feature, target_col, continuity_correction, simulation_B, verbose = FALSE) {
  tab <- .fs_build_contingency(dt, feature, target_col)
  if (is.null(tab)) {
    if (verbose) message(sprintf("Skipping '%s': not enough non-NA data or < 2 levels.", feature))
    return(list(
      feature = feature, n = 0L, df = NA_integer_,
      p_value = NA_real_, adj_p_value = NA_real_,
      significant = NA, method = NA_character_,
      correction_applied = NA, min_expected = NA_real_
    ))
  }
  
  n_obs <- sum(tab)
  res <- .fs_choose_test(tab, continuity_correction, simulation_B)
  
  list(
    feature = feature,
    n = as.integer(n_obs),
    df = as.integer(res$df),
    p_value = as.numeric(res$p),
    adj_p_value = NA_real_,     # filled later
    significant = NA,           # filled later
    method = res$method,
    correction_applied = isTRUE(res$correction),
    min_expected = as.numeric(res$min_expected)
  )
}

# Bind list-of-lists to a data.frame
.fs_bind_results <- function(rows) {
  as.data.frame(do.call(rbind, lapply(rows, function(x) {
    # ensure consistent types
    data.frame(
      feature = as.character(x$feature),
      n = as.integer(x$n),
      df = as.integer(x$df),
      p_value = as.numeric(x$p_value),
      adj_p_value = as.numeric(x$adj_p_value),
      significant = as.logical(x$significant),
      method = as.character(x$method),
      correction_applied = as.logical(x$correction_applied),
      min_expected = as.numeric(x$min_expected),
      stringsAsFactors = FALSE
    )
  })))
}

# Wrapper over p.adjust with guardrails
.fs_adjust_pvalues <- function(pvals, method = "bonferroni") {
  method <- match.arg(tolower(method), tolower(stats::p.adjust.methods))
  stats::p.adjust(pvals, method = method)
}
