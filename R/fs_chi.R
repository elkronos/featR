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
#' @details
#' The question this answers is, per feature: does the joint distribution of
#' that feature and the target differ from what independence would predict?
#' Features are examined one at a time, so the result describes marginal
#' association only. It says nothing about interactions, and two features
#' carrying the same information are both reported as significant. Only factor
#' features are tested: numeric, logical and date columns are ignored entirely,
#' so convert or discretize them first if you want them included.
#'
#' Each feature is tested on its own complete cases (rows where both the
#' feature and the target are observed), so `n` can differ between features.
#' Levels left empty after that filtering are dropped; if either the feature or
#' the target then has fewer than two levels, the feature is skipped with an
#' `NA` p-value rather than tested.
#'
#' A feature is tested with the asymptotic chi-square statistic when every
#' expected cell count is at least 5. Otherwise the p-value comes from a
#' Monte-Carlo simulation with `simulation_B` replicates, which makes it
#' stochastic unless `seed` is set and leaves `df` as `NA`. Yates' continuity
#' correction applies only on the asymptotic path and only to 2x2 tables; it is
#' never applied to a simulated p-value or to a larger table, whatever
#' `continuity_correction` says.
#'
#' Finally, a p-value is evidence against independence, not an effect size:
#' with enough rows a negligible association still clears any `sig_level`.
#'
#' @param data A data.frame or data.table with features and target. Character
#'   columns are coerced to factor. The input object is never modified.
#' @param target Character scalar: name of the target column. It is coerced to
#'   a factor if necessary and must have at least 2 non-NA levels.
#' @param sig_level Numeric threshold for significance, strictly between 0
#'   and 1 (default 0.05).
#' @param continuity_correction NULL/TRUE/FALSE: apply Yates correction to 2x2
#'   tables tested asymptotically. If NULL (default), auto-apply to every such
#'   table; TRUE is equivalent, FALSE disables it. It has no effect on tables
#'   larger than 2x2 or on simulation-based p-values, neither of which is ever
#'   corrected.
#' @param p_adjust_method Character: one of `stats::p.adjust.methods`
#'   (default "bonferroni"). Set to "none" to disable multiple-testing
#'   correction. Matching is case-insensitive. The correction counts only the
#'   features that were actually tested: a skipped feature has an `NA` p-value,
#'   and `stats::p.adjust()` drops NAs before its default `n = length(p)` is
#'   evaluated, so skipped features keep an `NA` adjusted p-value and do not
#'   inflate the multiplier for the others. With `k` tested and `s` skipped
#'   features, "bonferroni" therefore multiplies by `k`, not by `k + s`.
#' @param simulation_B Whole number >= 100: replicates for the simulation-based
#'   p-value used when any expected cell count is < 5 (default 2000).
#' @param seed Optional integer. Seeds the RNG locally (the previous RNG state
#'   is restored on exit), which makes simulation-based p-values reproducible
#'   in the sequential path. The parallel path draws from furrr's own
#'   L'Ecuyer-CMRG parallel streams (`furrr_options(seed = TRUE)`), so for the
#'   same `seed` parallel results are internally reproducible but differ from
#'   sequential results. Default NULL (never seeds).
#' @param verbose Logical; if TRUE, emits informative messages (target
#'   coercion, skipped features, worker count). Default FALSE.
#' @param parallel Logical; if TRUE, run features in parallel using the
#'   suggested furrr and future packages. Default FALSE (sequential).
#' @param n_cores Whole number >= 1. Number of workers used when
#'   `parallel = TRUE`; requests are capped at the detected core count. A
#'   `future::multisession` plan is set for the duration of the call and the
#'   previous plan is restored on exit. Default 2.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector of features with
#'         adj_p_value < sig_level.}
#'   \item{scores}{Named numeric vector of adjusted p-values, one per candidate
#'         categorical feature and `NA` for any feature that had to be skipped
#'         (smaller is stronger evidence of association).}
#'   \item{method}{"chi".}
#'   \item{task}{"classification".}
#'   \item{model}{NULL; the chi-square filter fits no model.}
#'   \item{details}{A list with `results` (the full results data.frame, one row
#'         per candidate categorical feature, ordered by adj_p_value then
#'         p_value so that skipped features sort last, with columns: feature;
#'         n (for tested features, the number of complete feature-target pairs;
#'         for skipped features, the feature's non-NA row count); df (NA for
#'         simulation-based tests, where the asymptotic degrees of freedom do
#'         not apply, and for skipped features); p_value; adj_p_value;
#'         significant (TRUE only when adj_p_value < sig_level, so FALSE for
#'         skipped features); method ("asymptotic" or "simulation", NA when
#'         skipped); correction_applied (TRUE/FALSE, NA when skipped);
#'         min_expected (minimum expected cell count, NA when skipped)), plus
#'         `sig_level`, `p_adjust_method` (the method string as supplied), and
#'         `n_features` (the number of candidate categorical features,
#'         including any that were skipped).}
#'   \item{call}{The matched call.}
#' }
#'
#' @examples
#' d <- data.frame(
#'   f1 = factor(rep(c("A", "B", "A"), times = c(40, 40, 20))),
#'   f2 = factor(rep(c("X", "Y"), times = 50)),
#'   target = factor(rep(c("Yes", "No"), each = 50))
#' )
#' out <- fs_chi(d, "target")
#' out$selected
#' out$details$results
#' @export
fs_chi <- function(
    data,
    target,
    sig_level = 0.05,
    continuity_correction = NULL,
    p_adjust_method = "bonferroni",
    simulation_B = 2000,
    seed = NULL,
    verbose = FALSE,
    parallel = FALSE,
    n_cores = 2L
) {
  cl <- match.call()

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
  assert_flag(verbose, "verbose")
  assert_flag(parallel, "parallel")
  assert_count(n_cores, "n_cores")

  # ---- 1) Validate & prepare ----
  dt <- .fs_validate_and_prepare_data(data, target, verbose = verbose)

  # Optional local seeding (sequential simulation-based p-values reproducible);
  # the previous RNG state is restored when fs_chi() exits.
  local_seed(seed)

  # Identify categorical features (factors) to test
  feature_cols <- .fs_get_factor_features(dt, target)
  if (length(feature_cols) == 0L) {
    if (verbose) message("No categorical (factor) features found for testing.")
    empty <- data.frame(
      feature = character(0), n = integer(0), df = numeric(0),
      p_value = numeric(0), adj_p_value = numeric(0),
      significant = logical(0), method = character(0),
      correction_applied = logical(0), min_expected = numeric(0),
      stringsAsFactors = FALSE
    )
    return(new_fs_result(
      selected = character(0),
      scores   = stats::setNames(numeric(0), character(0)),
      method   = "chi",
      task     = "classification",
      model    = NULL,
      details  = list(
        results         = empty,
        sig_level       = sig_level,
        p_adjust_method = p_adjust_method,
        n_features      = 0L
      ),
      call = cl
    ))
  }

  # ---- 2) Define worker for one feature ----
  worker <- function(feat) {
    .fs_test_feature(
      dt = dt,
      feature = feat,
      target = target,
      continuity_correction = continuity_correction,
      simulation_B = simulation_B,
      verbose = verbose
    )
  }

  # ---- 3) Parallel backend (suggested packages, opt-in only) ----
  if (parallel) {
    fs_require(c("furrr", "future"), "parallel chi-square testing")
    workers <- resolve_cores(n_cores)
    if (verbose) {
      message(sprintf("Testing %d feature(s) on %d worker(s).",
                      length(feature_cols), workers))
    }
    # plan() invisibly returns the previous plan when setting a new one;
    # restore it no matter how we exit.
    old_plan <- future::plan(future::multisession, workers = workers)
    on.exit(try(future::plan(old_plan), silent = TRUE), add = TRUE)
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

  new_fs_result(
    selected = res$feature[res$significant],
    scores   = stats::setNames(as.numeric(res$adj_p_value),
                               as.character(res$feature)),
    method   = "chi",
    task     = "classification",
    model    = NULL,
    details  = list(
      results         = res,
      sig_level       = sig_level,
      p_adjust_method = p_adjust_method,
      n_features      = length(feature_cols)
    ),
    call = cl
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
.fs_validate_and_prepare_data <- function(data, target, verbose = FALSE) {
  assert_data_frame(data, "data")
  assert_target(data, target, "target")

  dt <- as_dt(data)

  # Coerce character columns (including target if character) to factor
  char_cols <- names(dt)[vapply(dt, is.character, logical(1L))]
  if (length(char_cols) > 0L) {
    dt[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]
  }

  # Ensure target is factor
  if (!is.factor(dt[[target]])) {
    if (verbose) message(sprintf("Coercing target '%s' to factor.", target))
    data.table::set(dt, j = target, value = as.factor(dt[[target]]))
  }

  # Ensure target has >= 2 levels after removing NAs
  tgt <- droplevels(dt[[target]][!is.na(dt[[target]])])
  if (nlevels(tgt) < 2L) {
    stop("Target must have at least 2 non-NA levels for chi-square testing.")
  }

  dt
}

#' Names of factor features excluding the target
#' @noRd
.fs_get_factor_features <- function(dt, target) {
  candidates <- setdiff(names(dt), target)
  candidates[vapply(candidates, function(nm) is.factor(dt[[nm]]), logical(1L))]
}

#' Build a contingency table safely, dropping NAs and empty levels
#'
#' Returns NULL (rather than a degenerate table) when no row has both values
#' observed, or when either margin is left with fewer than two levels.
#' @noRd
.fs_build_contingency <- function(dt, feature, target) {
  valid <- !is.na(dt[[feature]]) & !is.na(dt[[target]])
  if (!any(valid)) return(NULL)

  x <- droplevels(dt[[feature]][valid])
  y <- droplevels(dt[[target]][valid])

  if (nlevels(x) < 2L || nlevels(y) < 2L) return(NULL)

  tab <- table(x, y)
  if (nrow(tab) < 2L || ncol(tab) < 2L) return(NULL)
  tab
}

#' Decide simulation vs asymptotic test and run it
#'
#' Simulation is used when any expected cell count is < 5; for those
#' simulation-based p-values the asymptotic degrees of freedom do not apply, so
#' df is NA and no continuity correction is possible. Otherwise the asymptotic
#' test runs, with Yates' correction on 2x2 tables only.
#' @noRd
.fs_choose_test <- function(tab, continuity_correction, simulation_B) {
  # Initial test (no correction) to inspect expected counts. Only this call can
  # emit the "approximation may be incorrect" warning: the asymptotic branch
  # below is reached only when every expected count is >= 5, and
  # simulate.p.value = TRUE does not warn.
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
.fs_test_feature <- function(dt, feature, target, continuity_correction,
                             simulation_B, verbose = FALSE) {
  tab <- .fs_build_contingency(dt, feature, target)
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

#' Wrapper over p.adjust() with a case-insensitive method check
#'
#' Matches `method` against stats::p.adjust.methods ignoring case and passes
#' the canonical spelling on, so "bh" works as well as "BH"; anything else is
#' an error rather than a silent fallback.
#'
#' NA policy: NA p-values stay NA and are NOT counted as tests. `p.adjust()`
#' reassigns `p <- p[!is.na(p)]` before its lazily-evaluated default
#' `n = length(p)` is first forced, so the effective number of tests is the
#' number of non-NA p-values. Pass `n = length(pvals)` explicitly if skipped
#' features should ever count towards the correction.
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
