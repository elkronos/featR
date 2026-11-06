# Chi-square feature selection for categorical features.
# furrr and future are Suggests and are only touched when parallel = TRUE.

# =========================
# Public API
# =========================

#' Chi-squared tests for categorical features
#'
#' @description
#' Tests each categorical feature for association with a categorical target
#' using the chi-squared test of independence, adjusts the p-values for
#' multiple testing, and keeps the features whose adjusted p-value is below
#' `sig_level`.
#'
#' @details
#' For each feature, the test asks whether the feature and the target are
#' distributed jointly the way independence would predict. Features are
#' tested one at a time, so the result describes marginal association only.
#' A feature that matters only in combination with another is missed, and
#' two features carrying the same information are both reported as
#' significant.
#'
#' A p-value measures evidence against independence and says nothing about
#' the size of the effect. With enough rows a negligible association still
#' clears any `sig_level`.
#'
#' @section Which columns are tested:
#' Character columns are converted to factors, and then every factor column
#' other than the target is tested. Numeric, logical, and date columns are
#' ignored, so convert or discretize them first if you want them tested. If
#' no factor features remain, the result has an empty selection and no
#' error is raised. A numeric target is converted with `as.factor()`, so each
#' distinct value becomes its own class.
#'
#' @section Missing values and sparse tables:
#' Each feature is tested on its own complete cases (rows where both the
#' feature and the target are observed), so `n` can differ between features.
#' Levels left empty after that filtering are dropped. If the feature or the
#' target then has fewer than two levels, the feature is skipped: it gets an
#' `NA` p-value and is not selected.
#'
#' A feature is tested with the asymptotic chi-squared statistic when every
#' expected cell count is at least 5. Otherwise the p-value comes from a
#' Monte Carlo simulation with `simulation_B` replicates. A simulated p-value
#' changes from run to run unless `seed` is set, and its `df` is `NA`. Yates'
#' continuity correction applies only to 2x2 tables on the asymptotic path,
#' whatever `continuity_correction` says.
#'
#' @param data A data frame or data.table holding the features and the
#'   target. Column names must be unique, non-empty, and not `NA`. The input
#'   object is never modified.
#' @param target Name of the target column. It is converted to a factor if
#'   necessary and must have at least 2 levels among its non-missing values.
#' @param sig_level Significance threshold for the adjusted p-values, strictly
#'   between 0 and 1. Default 0.05.
#' @param continuity_correction `NULL`, `TRUE`, or `FALSE`. Controls Yates'
#'   correction for 2x2 tables tested asymptotically. `NULL` (the default)
#'   and `TRUE` both apply it; `FALSE` turns it off. Tables larger than 2x2
#'   and simulated p-values are never corrected.
#' @param p_adjust_method Multiple-testing correction, one of
#'   `stats::p.adjust.methods`, matched case-insensitively. Default
#'   `"bonferroni"`; `"none"` turns correction off. An invalid name is an
#'   error before any feature is tested. Only tested features count toward
#'   the correction: a skipped feature keeps an `NA` adjusted p-value and
#'   does not raise the multiplier for the others. With `k` tested and `s`
#'   skipped features, `"bonferroni"` multiplies by `k`, not `k + s`.
#' @param simulation_B Number of Monte Carlo replicates for the simulated
#'   p-value used when any expected cell count is below 5. A whole number of
#'   at least 100; default 2000. A simulated p-value is never below
#'   `1 / (simulation_B + 1)`, so after adjustment it may be unable to reach
#'   `sig_level` at all. With Bonferroni this happens once the number of
#'   tested features reaches `sig_level * (simulation_B + 1)`, which is 101
#'   at the defaults. A warning then names the affected features and the
#'   `simulation_B` you would need.
#' @param seed Optional integer. When supplied, the RNG is seeded for this
#'   call and the previous RNG state is restored on exit, which makes
#'   simulated p-values reproducible. With `parallel = TRUE` the simulations
#'   draw from furrr's parallel-safe streams instead
#'   (`furrr_options(seed = TRUE)`), so a given `seed` gives reproducible
#'   parallel results that differ from the sequential ones. Default `NULL`
#'   (featR does not touch the RNG).
#' @param verbose If `TRUE`, report target conversion, skipped features, and
#'   the worker count. Default `FALSE`.
#' @param parallel If `TRUE`, test features in parallel using the suggested
#'   furrr and future packages. Default `FALSE`.
#' @param n_cores Number of workers when `parallel = TRUE`, a whole number of
#'   at least 1, capped at the detected core count. Default 2. If the current
#'   future plan is sequential (the future default), featR sets a
#'   `future::multisession` plan with this many workers for the duration of
#'   the call and restores the previous plan on exit, even if worker start-up
#'   fails. If you have already set a non-sequential plan, for example
#'   `future::plan(future::multisession, workers = 4)`, featR uses it as is,
#'   leaves its workers running, and ignores `n_cores`.
#'
#' @return An `fs_result` object with:
#' \describe{
#'   \item{`selected`}{Features with an adjusted p-value below `sig_level`.}
#'   \item{`scores`}{Named numeric vector of adjusted p-values, one per
#'     candidate factor feature, `NA` for skipped features. Smaller means
#'     stronger evidence of association, the opposite of most featR
#'     scores.}
#'   \item{`method`}{`"chi"`.}
#'   \item{`task`}{`"classification"`.}
#'   \item{`model`}{`NULL`; no model is fitted.}
#'   \item{`details`}{A list with `results`, `sig_level`, `p_adjust_method`
#'     (as supplied), and `n_features` (the number of candidate factor
#'     features, skipped ones included). `results` is a data frame with one
#'     row per candidate feature, ordered by adjusted then raw p-value with
#'     skipped features last. Its columns are:
#'     \itemize{
#'       \item `feature`: the column name.
#'       \item `n`: for tested features, the number of complete
#'         feature-target pairs; for skipped features, the feature's count of
#'         non-missing values.
#'       \item `df`: degrees of freedom; `NA` for simulated and skipped tests.
#'       \item `p_value`, `adj_p_value`: raw and adjusted p-values.
#'       \item `significant`: `TRUE` when `adj_p_value < sig_level`, so
#'         `FALSE` for skipped features.
#'       \item `method`: `"asymptotic"` or `"simulation"`; `NA` when skipped.
#'       \item `correction_applied`: whether Yates' correction was actually
#'         applied. A 2x2 table whose counts equal their expected values
#'         gets no correction even when one was requested. `NA` when skipped.
#'       \item `min_expected`: the smallest expected cell count; `NA` when
#'         skipped.
#'     }}
#'   \item{`call`}{The matched call.}
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
  # Check the method before any (possibly Monte-Carlo) testing runs, and also
  # when there is nothing to test, so an invalid name is never stored.
  .fs_match_p_adjust(p_adjust_method)
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
    # A plan the caller already set (anything but sequential) is used as is:
    # replacing it and then "restoring" it by calling plan() again would shut
    # down the caller's workers and start fresh ones. Otherwise install a
    # temporary multisession plan, registering the restore BEFORE the new
    # plan is set, so a failed worker start-up cannot leak featR's plan.
    if (inherits(future::plan(), "sequential")) {
      old_plan <- future::plan("list")
      on.exit(try(future::plan(old_plan), silent = TRUE), add = TRUE)
      future::plan(future::multisession, workers = workers)
    } else if (verbose) {
      message("Using the future plan already set; 'n_cores' is ignored.")
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

  # A Monte-Carlo p-value is never below 1 / (simulation_B + 1). After the
  # multiplicity adjustment that floor can sit at or above sig_level (with
  # Bonferroni, once the number of tests reaches sig_level * (B + 1)), and
  # then a simulated feature is never selected however strong its
  # association. Say so instead of returning a silent false negative.
  sim <- !is.na(res$method) & res$method == "simulation"
  if (any(sim)) {
    p_floor <- res$p_value
    p_floor[sim] <- 1 / (simulation_B + 1)
    adj_floor <- .fs_adjust_pvalues(p_floor, method = p_adjust_method)
    stuck <- sim & !is.na(adj_floor) & adj_floor >= sig_level
    if (any(stuck)) {
      k <- sum(!is.na(res$p_value))
      shown <- utils::head(res$feature[stuck], 5L)
      warning(sprintf(paste(
        "%d feature(s) tested by simulation cannot reach sig_level = %g:",
        "a simulated p-value is never below 1/(simulation_B + 1) = %.3g, and",
        "after '%s' adjustment over %d tests even the smallest attainable",
        "value is not significant, so they are never selected however strong",
        "their association: %s%s. Increase simulation_B (for Bonferroni, to",
        "at least %d) or use a less conservative p_adjust_method."
      ), sum(stuck), sig_level, 1 / (simulation_B + 1), p_adjust_method, k,
      paste(shown, collapse = ", "), if (sum(stuck) > 5L) ", ..." else "",
      as.integer(floor(k / sig_level))), call. = FALSE)
    }
  }

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
  # Columns are looked up by name, so a duplicated name would test its first
  # copy twice and never the second (or fail inside data.table when the
  # copies are character), and an empty or NA name looks up NULL, so that
  # column would be silently left out. Reject rather than silently mis-test.
  assert_column_names(data, "fs_chi")

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
    # chisq.test() computes YATES <- min(0.5, abs(x - E)), so a 2x2 table
    # sitting exactly at expectation gets no correction even when one was
    # requested. Report what R actually did, not what we asked for.
    list(
      p = test$p.value,
      method = "asymptotic",
      correction = grepl("continuity correction", test$method, fixed = TRUE),
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
  stats::p.adjust(pvals, method = .fs_match_p_adjust(method))
}

#' Canonical spelling of a p.adjust() method, matched case-insensitively
#'
#' Errors on anything that is not one of stats::p.adjust.methods.
#' @noRd
.fs_match_p_adjust <- function(method) {
  choices <- stats::p.adjust.methods
  idx <- match(tolower(method), tolower(choices))
  if (is.na(idx)) {
    stop(sprintf(
      "Invalid p_adjust_method '%s'. Must be one of: %s",
      method, paste(choices, collapse = ", ")
    ), call. = FALSE)
  }
  choices[idx]
}
