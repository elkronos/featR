# Chi-square feature selection for categorical features.
# furrr and future are Suggests and are only touched when parallel = TRUE.

# =========================
# Public API
# =========================

#' Chi-square feature selection for categorical features
#'
#' @description
#' Tests association between each categorical feature and a (categorical) target
#' via the chi-square test of independence. Character columns are automatically
#' coerced to factors, and only factor features (excluding the target) are tested.
#' Handles missing values per-feature, switches to simulation-based p-values
#' when any expected cell count is < 5, and supports multiple-testing correction.
#'
#' @param data A data.frame or data.table with features and target. Character
#'   columns are coerced to factor. The input object is never modified.
#' @param target_col Character scalar: name of the target column.
#' @param sig_level Numeric threshold for significance, strictly between 0
#'   and 1 (default 0.05).
#' @param continuity_correction NULL/TRUE/FALSE: apply Yates correction for 2x2.
#'   If NULL (default), auto-apply when table is 2x2.
#' @param p_adjust_method Character: one of \code{stats::p.adjust.methods}
#'   (default "bonferroni"). Set to "none" to disable multiple-testing
#'   correction. Matching is case-insensitive.
#' @param simulation_B Whole number >= 100: replicates for simulation-based
#'   p-values when expected counts are low (default 2000).
#' @param parallel Logical; if TRUE, run features in parallel using the
#'   suggested furrr and future packages. Default FALSE.
#' @param temp_multisession Logical; if TRUE and \code{parallel = TRUE},
#'   temporarily set a \code{future::multisession} plan (capped at 2 workers)
#'   and restore the previous plan on exit. If FALSE, whatever plan the user
#'   has set is used.
#' @param seed Optional integer. Seeds the RNG locally (the previous RNG state
#'   is restored on exit), which makes simulation-based p-values reproducible
#'   in the sequential path. The parallel path draws from furrr's own
#'   L'Ecuyer-CMRG parallel streams (\code{furrr_options(seed = TRUE)}), so
#'   for the same \code{seed} parallel results are internally reproducible but
#'   differ from sequential results. Default NULL (never seeds).
#' @param verbose Logical; if TRUE, prints informative messages.
#'
#' @return A list with:
#' \describe{
#'   \item{results}{data.frame with one row per feature: feature; n (for
#'         tested features, the number of complete feature-target pairs; for
#'         skipped features, the feature's non-NA row count); df (NA for
#'         simulation-based tests, where the asymptotic degrees of freedom do
#'         not apply); p_value; adj_p_value; significant; method
#'         ("asymptotic" or "simulation"); correction_applied (TRUE/FALSE);
#'         min_expected (minimum expected cell count).}
#'   \item{significant_features}{Character vector of features with
#'         adj_p_value < sig_level.}
#' }
#'
#' @examples
#' d <- data.frame(
#'   f1 = factor(rep(c("A", "B", "A"), times = c(40, 40, 20))),
#'   f2 = factor(rep(c("X", "Y"), times = 50)),
#'   target = factor(rep(c("Yes", "No"), each = 50))
#' )
#' out <- fs_chi(d, "target")
#' out$results
#' out$significant_features
#' @export
fs_chi <- function(
    data,
    target_col,
    sig_level = 0.05,
    continuity_correction = NULL,
    p_adjust_method = "bonferroni",
    simulation_B = 2000,
    parallel = FALSE,
    temp_multisession = FALSE,
    seed = NULL,
    verbose = FALSE
) {

  # ---- 0) Validate scalar options ----
  assert_number(sig_level, "sig_level")
  if (sig_level <= 0 || sig_level >= 1) {
    stop("'sig_level' must be strictly between 0 and 1.")
  }
  if (!is.null(continuity_correction)) {
    assert_flag(continuity_correction, "continuity_correction")
  }
  assert_string(p_adjust_method, "p_adjust_method")
  simulation_B <- assert_count(simulation_B, "simulation_B", lower = 100L)
  assert_flag(parallel, "parallel")
  assert_flag(temp_multisession, "temp_multisession")
  assert_flag(verbose, "verbose")

  # ---- 1) Validate & prepare ----
  dt <- .fs_validate_and_prepare_data(data, target_col, verbose = verbose)

  # Optional local seeding (sequential simulation-based p-values reproducible);
  # the previous RNG state is restored when fs_chi() exits.
  local_seed(seed)

  # Identify categorical features (factors) to test
  feature_cols <- .fs_get_factor_features(dt, target_col)
  if (length(feature_cols) == 0L) {
    if (verbose) message("No categorical (factor) features found for testing.")
    empty <- data.frame(
      feature = character(0), n = integer(0), df = numeric(0),
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

  # ---- 3) Parallel backend (suggested packages, opt-in only) ----
  if (parallel) {
    fs_require(c("furrr", "future"), "parallel chi-square testing")
    if (temp_multisession) {
      # plan() invisibly returns the previous plan when setting a new one;
      # restore it no matter how we exit. Workers are capped at 2.
      old_plan <- future::plan(future::multisession, workers = 2L)
      on.exit(try(future::plan(old_plan), silent = TRUE), add = TRUE)
    }
  }

  # ---- 4) Execute tests ----
  rows <- if (parallel) {
    furrr::future_map(
      feature_cols,
      worker,
      .options = furrr::furrr_options(seed = TRUE)
    )
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

#' Validate inputs and prepare a working data.table
#'
#' Coerces character columns to factor and ensures the target is a factor with
#' at least 2 non-NA levels. Always operates on a copy of `data`.
#' @noRd
.fs_validate_and_prepare_data <- function(data, target_col, verbose = FALSE) {
  assert_data_frame(data, "data")
  assert_target(data, target_col, "target_col")

  dt <- as_dt(data)

  # Coerce character columns (including target if character) to factor
  char_cols <- names(dt)[vapply(dt, is.character, logical(1L))]
  if (length(char_cols) > 0L) {
    dt[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]
  }

  # Ensure target is factor
  if (!is.factor(dt[[target_col]])) {
    if (verbose) message(sprintf("Coercing target '%s' to factor.", target_col))
    data.table::set(dt, j = target_col, value = as.factor(dt[[target_col]]))
  }

  # Ensure target has >= 2 levels after removing NAs
  tgt <- droplevels(dt[[target_col]][!is.na(dt[[target_col]])])
  if (nlevels(tgt) < 2L) {
    stop("Target must have at least 2 non-NA levels for chi-square testing.")
  }

  dt
}

#' Names of factor features excluding the target
#' @noRd
.fs_get_factor_features <- function(dt, target_col) {
  candidates <- setdiff(names(dt), target_col)
  candidates[vapply(candidates, function(nm) is.factor(dt[[nm]]), logical(1L))]
}

#' Build a contingency table safely, dropping NAs and empty levels
#' @noRd
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

#' Decide simulation vs asymptotic test and run it
#'
#' For simulation-based p-values the asymptotic degrees of freedom do not
#' apply, so df is NA.
#' @noRd
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
      df = NA_real_
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
      df = as.numeric(test$parameter)
    )
  }
}

#' Test one feature and return a named list (one result row)
#'
#' Skipped features (no non-NA pairs, or fewer than 2 levels after dropping
#' empty levels) report the feature's non-NA row count as n.
#' @noRd
.fs_test_feature <- function(dt, feature, target_col, continuity_correction,
                             simulation_B, verbose = FALSE) {
  tab <- .fs_build_contingency(dt, feature, target_col)
  if (is.null(tab)) {
    if (verbose) message(sprintf("Skipping '%s': not enough non-NA data or < 2 levels.", feature))
    return(list(
      feature = feature,
      n = as.integer(sum(!is.na(dt[[feature]]))),
      df = NA_real_,
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
    df = as.numeric(res$df),
    p_value = as.numeric(res$p),
    adj_p_value = NA_real_,     # filled later
    significant = NA,           # filled later
    method = res$method,
    correction_applied = isTRUE(res$correction),
    min_expected = as.numeric(res$min_expected)
  )
}

#' Bind list-of-lists to a data.frame
#' @noRd
.fs_bind_results <- function(rows) {
  as.data.frame(do.call(rbind, lapply(rows, function(x) {
    # ensure consistent types
    data.frame(
      feature = as.character(x$feature),
      n = as.integer(x$n),
      df = as.numeric(x$df),
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

#' Wrapper over p.adjust with guardrails (case-insensitive, BH/BY safe)
#' @noRd
.fs_adjust_pvalues <- function(pvals, method = "bonferroni") {
  choices <- stats::p.adjust.methods
  idx <- match(tolower(method), tolower(choices))
  if (is.na(idx)) {
    stop(sprintf(
      "Invalid p_adjust_method '%s'. Must be one of: %s",
      method, paste(choices, collapse = ", ")
    ))
  }
  method <- choices[idx]
  stats::p.adjust(pvals, method = method)
}
