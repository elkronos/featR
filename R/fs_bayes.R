# Bayesian feature selection via brms model comparison.
# brms, loo, and pbapply are Suggests and are only touched at run time.

###############################################################################
# Helpers
###############################################################################

#' Validate data columns and types for fs_bayes()
#'
#' Checks that the target, predictor, and (if provided) date columns exist
#' in the data. Performs minimal family-aware checks on the target.
#'
#' @param data A data.frame or data.table.
#' @param target Character. Name of the target column.
#' @param predictors Character vector. Names of predictor columns.
#' @param date_col Character or NULL. Name of the date column.
#' @param brm_family A model family used for basic type checks.
#' @return Invisibly TRUE if validation passes.
#' @noRd
bayes_validate_data <- function(data,
                                target,
                                predictors,
                                date_col = NULL,
                                brm_family = stats::gaussian()) {
  if (!target %in% names(data)) {
    stop("Response column '", target, "' not found in data.")
  }
  if (length(predictors) < 1L) {
    stop("At least one predictor column must be provided.")
  }
  if (any(!predictors %in% names(data))) {
    missing_preds <- predictors[!predictors %in% names(data)]
    stop("Missing predictor columns: ", paste(missing_preds, collapse = ", "))
  }
  if (target %in% predictors) {
    stop("'target' must not also appear in 'predictors'; otherwise one ",
         "candidate model regresses '", target, "' on itself.")
  }
  if (!is.null(date_col) && !date_col %in% names(data)) {
    stop("Date column '", date_col, "' not found in data.")
  }

  # A family object is a list carrying a single character `family`. Passing the
  # generator without parentheses (brms::bernoulli rather than
  # brms::bernoulli()) is a common slip and otherwise dies with the opaque
  # "object of type 'closure' is not subsettable".
  if (!is.list(brm_family) ||
      !is.character(brm_family$family) ||
      length(brm_family$family) != 1L ||
      is.na(brm_family$family)) {
    stop("'brm_family' must be a family object carrying a single character ",
         "'family' element, such as stats::gaussian() or brms::bernoulli(). ",
         "Note the parentheses: the function itself (for example ",
         "brms::bernoulli) is not a family object.", call. = FALSE)
  }

  # Minimal sanity checks re: family/response
  fam <- brm_family$family
  y <- data[[target]]

  if (identical(fam, "gaussian") && !is.numeric(y)) {
    stop(
      "gaussian() family requires a numeric response; got class: ",
      paste(class(y), collapse = "/")
    )
  }

  if (identical(fam, "bernoulli")) {
    uy <- unique(stats::na.omit(y))
    if (!is.numeric(y) && !is.logical(y)) {
      stop("bernoulli() family expects binary numeric or logical response.")
    }
    if (length(uy) > 2L || !all(uy %in% c(0, 1, TRUE, FALSE))) {
      stop("bernoulli() family expects values in {0,1} (or logical).")
    }
  }

  invisible(TRUE)
}

#' Map a brms family to a featR task label
#'
#' Only the families whose response is unambiguously categorical are reported
#' as classification; everything else (including unrecognised or malformed
#' family objects) falls back to "regression", which is what `fs_bayes()`
#' assumes by default.
#'
#' @param brm_family A family object accepted by brms::brm().
#' @return "classification" or "regression".
#' @noRd
bayes_task_from_family <- function(brm_family) {
  fam <- tryCatch(brm_family$family, error = function(e) NULL)
  if (is.null(fam) || !is.character(fam) || length(fam) != 1L || is.na(fam)) {
    return("regression")
  }
  categorical <- c("bernoulli", "binomial", "beta_binomial", "categorical",
                   "multinomial", "cumulative", "sratio", "cratio", "acat")
  if (fam %in% categorical) "classification" else "regression"
}

#' Add ISO week feature from a date column
#'
#' Converts the date column to Date class if needed, adds "iso_week_id"
#' (ISO year * 100 + ISO week) to the data.table, and appends it to the
#' predictors. Note: iso_week_id later enters the models as a continuous
#' covariate, which is a rough encoding of seasonality.
#'
#' @param data A data.table (modified by reference).
#' @param date_col Character. Name of the date column.
#' @param predictors Character vector. Current predictor columns.
#' @return A list with updated `data` and `predictors`.
#' @noRd
bayes_add_week_feature <- function(data, date_col, predictors) {
  if (!inherits(data[[date_col]], "Date")) {
    data.table::set(data, j = date_col, value = as.Date(data[[date_col]]))
  }

  iso_year <- as.integer(strftime(data[[date_col]], "%G"))
  iso_week <- as.integer(strftime(data[[date_col]], "%V"))

  data.table::set(data, j = "iso_week_id", value = iso_year * 100L + iso_week)
  predictors <- unique(c(predictors, "iso_week_id"))

  list(data = data, predictors = predictors)
}

#' Generate predictor combinations
#'
#' Enumerates every non-empty subset up to `max_comb_size` when that is
#' feasible, and otherwise draws a random sample of distinct subsets *without*
#' enumerating them.
#'
#' The distinction matters: the number of subsets is `sum(choose(n, k))` over
#' the allowed sizes, which passes a billion by about 30 predictors. Building
#' that list only to sample a few hundred from it exhausts memory, so
#' `sample_combinations` would fail at exactly the scale it exists to handle.
#' The sampled path instead draws a size `k` with probability proportional to
#' `choose(n, k)` -- which makes every subset equally likely -- then draws the
#' `k` members directly, keying each draw to reject duplicates.
#'
#' @param predictors Character vector of predictor names.
#' @param max_comb_size Integer or NULL. Maximum subset size. If NULL, uses
#'   `length(predictors)`. Must be at least 1 if non-NULL.
#' @param sample_combinations Integer or NULL. If not NULL, return at most this
#'   many subsets, sampled without enumeration when the full set is larger.
#' @param seed Optional seed for the sampling step, applied locally and
#'   restored on exit (see local_seed()). Default NULL: never seeds.
#' @param max_enumerate Ceiling on how many subsets will be materialized when
#'   no sample size was requested. Above it the caller is told to set a bound
#'   rather than being allowed to exhaust memory.
#' @return A list of character vectors (predictor subsets).
#' @noRd
bayes_generate_predictor_combinations <- function(predictors,
                                                  max_comb_size = NULL,
                                                  sample_combinations = NULL,
                                                  seed = NULL,
                                                  max_enumerate = 1e5) {
  n <- length(predictors)
  if (n < 1L) {
    stop("'predictors' must contain at least one predictor.")
  }

  if (!is.null(max_comb_size)) {
    max_comb_size <- assert_count(max_comb_size, "max_comb_size")
    max_comb_size <- min(max_comb_size, n)
  } else {
    max_comb_size <- n
  }
  if (!is.null(sample_combinations)) {
    sample_combinations <- assert_count(sample_combinations,
                                        "sample_combinations")
  }

  sizes <- seq_len(max_comb_size)
  # choose() returns doubles, so this stays finite far past the point where
  # the subsets themselves could be held in memory.
  size_counts <- choose(n, sizes)
  total <- sum(size_counts)

  # Exhaustive path: the caller wants everything, or everything is fewer than
  # the sample they asked for. Preserves the historical enumeration order.
  if (is.null(sample_combinations) || total <= sample_combinations) {
    if (total > max_enumerate) {
      stop(sprintf(
        paste0(
          "%.0f predictor combinations would have to be enumerated, which ",
          "will exhaust memory. Reduce 'max_comb_size' (currently %d) or set ",
          "'sample_combinations' to search a random subset instead."
        ),
        total, max_comb_size
      ), call. = FALSE)
    }
    comb_list <- lapply(
      X = sizes,
      FUN = function(i) utils::combn(predictors, i, simplify = FALSE)
    )
    return(unlist(comb_list, recursive = FALSE))
  }

  # Sampled path: draw distinct subsets directly, never building the full set.
  local_seed(seed)
  out <- vector("list", sample_combinations)
  seen <- new.env(hash = TRUE, parent = emptyenv())
  found <- 0L
  attempts <- 0L
  max_attempts <- 50L * sample_combinations + 1000L

  while (found < sample_combinations && attempts < max_attempts) {
    attempts <- attempts + 1L
    # sample.int() on the index avoids sample()'s length-1 "sample from 1:x"
    # trap, and normalizes `prob` itself.
    k <- sizes[sample.int(length(sizes), size = 1L, prob = size_counts)]
    idx <- sort(sample.int(n, size = k))
    key <- paste(idx, collapse = ",")
    if (exists(key, envir = seen, inherits = FALSE)) {
      next
    }
    assign(key, TRUE, envir = seen)
    found <- found + 1L
    out[[found]] <- predictors[idx]
  }

  if (found < sample_combinations) {
    stop(sprintf(
      paste0(
        "Could only draw %d distinct predictor combinations out of the %d ",
        "requested in %d attempts. Lower 'sample_combinations' or raise ",
        "'max_comb_size' to widen the pool."
      ),
      found, sample_combinations, attempts
    ), call. = FALSE)
  }

  out
}

#' Fit a Bayesian model using brms
#'
#' Applies featR's defaults (iter = 2000, adapt_delta = 0.99,
#' max_treedepth = 15, refresh = 0) and then lets `brm_args` override any of
#' them; `warmup` is left to brms' own default of iter / 2. Fitting warnings
#' are logged (when `verbose`) but never abort the fit; a fit that errors
#' returns NULL so that the remaining combinations still run.
#'
#' @param data A data.frame or data.table containing the data.
#' @param formula_str Character. The model formula as a string.
#' @param brm_family A model family accepted by brms::brm().
#' @param prior A brms prior specification, or NULL.
#' @param brm_args List. Additional arguments for brms::brm().
#' @param verbose Logical. Print progress and warnings.
#' @return A fitted brms model object, or NULL if fitting fails.
#' @noRd
bayes_fit_model <- function(data,
                            formula_str,
                            brm_family,
                            prior,
                            brm_args,
                            verbose = FALSE) {
  if (verbose) {
    message("Fitting model: ", formula_str)
  }

  default_args <- list(
    formula = stats::as.formula(formula_str),
    data    = data,
    family  = brm_family,
    prior   = prior,
    iter    = 2000,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    refresh = 0
  )

  model_args <- utils::modifyList(default_args, brm_args)

  # If using fixed_param, remove control settings, as they are not relevant
  if (!is.null(model_args$algorithm) && identical(model_args$algorithm, "fixed_param")) {
    model_args$control <- NULL
  }

  model <- tryCatch({
    # Capture and log warnings, but do not fail the fit because of them
    withCallingHandlers(
      do.call(brms::brm, model_args),
      warning = function(w) {
        if (verbose) {
          message("Warning [", formula_str, "]: ", conditionMessage(w))
        }
        invokeRestart("muffleWarning")
      }
    )
  }, error = function(e) {
    if (verbose) {
      message("Error [", formula_str, "]: ", conditionMessage(e))
    }
    NULL
  })

  if (!is.null(model) && verbose) {
    message("Successfully fitted: ", formula_str)
  }

  model
}

#' Is this a two-dimensional draws summary with an Estimate column?
#'
#' `fitted.brmsfit()` returns an n x summary matrix for univariate families,
#' but a 3-D array for categorical, multinomial, and multivariate responses.
#' Only the matrix form can be reduced to one fitted value per row.
#'
#' @param x The result of `fitted()` or `residuals()` on a brmsfit.
#' @return `TRUE` when `x[, "Estimate"]` is meaningful.
#' @noRd
bayes_is_estimate_matrix <- function(x) {
  d <- dim(x)
  !is.null(d) && length(d) == 2L &&
    !is.null(colnames(x)) && "Estimate" %in% colnames(x)
}

#' Append fitted metrics to data
#'
#' Adds fitted values and residual-based metrics to the data.table.
#'
#' For families whose `fitted()` is a 3-D array -- categorical, multinomial,
#' and multivariate responses -- there is no single fitted value per row on the
#' response scale, so no residual can be formed and in-sample MAE/RMSE are
#' undefined. Those families are supported: the columns are simply not added
#' and the caller reports `NA` metrics, rather than the function failing.
#'
#' @param data A data.table (modified by reference).
#' @param model A brmsfit object.
#' @param verbose Logical; explain when metrics are unavailable.
#' @return The data.table. When the family permits it, with the added columns
#'   `fitted_values`, `residuals`, `abs_residuals`, `squared_residuals`;
#'   otherwise unchanged.
#' @noRd
bayes_add_metrics_to_data <- function(data, model, verbose = FALSE) {
  fitted_raw <- stats::fitted(model)
  resid_raw  <- stats::residuals(model)

  if (!bayes_is_estimate_matrix(fitted_raw) ||
      !bayes_is_estimate_matrix(resid_raw)) {
    if (isTRUE(verbose)) {
      message(
        "This family's fitted values are not a single draws summary per row ",
        "(categorical, multinomial, and multivariate responses give a 3-D ",
        "array), so residuals and in-sample MAE/RMSE are undefined and are ",
        "reported as NA. Model selection is unaffected."
      )
    }
    return(data)
  }

  fitted_vals <- as.numeric(fitted_raw[, "Estimate"])
  resid_vals  <- as.numeric(resid_raw[, "Estimate"])

  data[, `:=`(
    fitted_values     = fitted_vals,
    residuals         = resid_vals,
    abs_residuals     = abs(resid_vals),
    squared_residuals = resid_vals^2
  )]

  data
}

#' Evaluate a predictor combination
#'
#' Fits and scores a model (LOO when applicable). The `loo` object itself is
#' returned alongside its elpd estimate so that `loo::loo_compare()` can be
#' run across every successful fit.
#'
#' @param preds Character vector of predictor names for this combination.
#' @param data A data.table.
#' @param target Name of the target column.
#' @param brm_family A model family accepted by brms::brm().
#' @param prior A brms prior specification, or NULL.
#' @param brm_args List. Additional arguments for brms::brm().
#' @param verbose Logical.
#' @return A list with elements preds, model, loo, loo_val, formula_str.
#' @noRd
bayes_evaluate_combination <- function(preds,
                                       data,
                                       target,
                                       brm_family,
                                       prior,
                                       brm_args,
                                       verbose = FALSE) {
  formula_str <- paste0(
    backtick(target), " ~ ",
    paste(backtick(preds), collapse = " + ")
  )
  model <- bayes_fit_model(
    data        = data,
    formula_str = formula_str,
    brm_family  = brm_family,
    prior       = prior,
    brm_args    = brm_args,
    verbose     = verbose
  )

  loo_obj <- NULL
  loo_val <- NA_real_

  if (!is.null(model)) {
    if (!is.null(brm_args$algorithm) && identical(brm_args$algorithm, "fixed_param")) {
      if (verbose) {
        message("Skipping LOO for fixed_param algorithm: ", formula_str)
      }
    } else {
      loo_obj <- tryCatch(
        loo::loo(model),
        error = function(e) {
          if (verbose) {
            message("LOO failed [", formula_str, "]: ", conditionMessage(e))
          }
          NULL
        }
      )
      if (!is.null(loo_obj)) {
        loo_val <- tryCatch(
          loo_obj$estimates["elpd_loo", "Estimate"],
          error = function(e) NA_real_
        )
      }
    }
  }

  list(
    preds       = preds,
    model       = model,
    loo         = loo_obj,
    loo_val     = loo_val,
    formula_str = formula_str
  )
}

#' Build the loo_compare() table across successful fits
#'
#' @param results List of `bayes_evaluate_combination()` results.
#' @param idx Integer positions in `results` that are usable (fitted model and
#'   a finite elpd).
#' @return The `loo::loo_compare()` table (a matrix or data.frame depending on
#'   the loo version), labelled `"model<i>"` for the corresponding position `i`
#'   in `results`. Where those labels live is version dependent: loo <= 2.9 put
#'   them in the rownames, loo >= 2.10.0 returns a data.frame with no rownames
#'   and a `"model"` column instead. Returns NULL when fewer than two
#'   comparable models exist or the comparison fails.
#' @noRd
bayes_loo_comparison <- function(results, idx) {
  if (length(idx) < 2L) {
    return(NULL)
  }
  loo_objs <- lapply(results[idx], function(x) x$loo)
  if (!all(vapply(loo_objs, function(l) inherits(l, "loo"), logical(1L)))) {
    return(NULL)
  }
  names(loo_objs) <- paste0("model", idx)
  tryCatch(loo::loo_compare(loo_objs), error = function(e) NULL)
}

#' Choose one model from the loo comparison
#'
#' `rule = "best"` keeps the raw elpd maximum (the first row of the
#' comparison). `rule = "1se"` keeps the most parsimonious model -- fewest
#' predictors, ties broken by the higher elpd -- among those whose elpd
#' difference from the best model is no larger than one standard error of
#' that difference (the `se_diff` column of `loo::loo_compare()`). When no
#' usable comparison table is available the raw maximum is used.
#'
#' @param results List of `bayes_evaluate_combination()` results.
#' @param idx Integer positions in `results` that are usable.
#' @param comparison A `loo::loo_compare()` table (matrix or data.frame,
#'   depending on the loo version), or NULL.
#' @param rule "1se" or "best".
#' @return A single integer position in `results`, or `NA_integer_` when
#'   `idx` is empty.
#' @noRd
bayes_pick_model <- function(results, idx, comparison = NULL,
                             rule = c("1se", "best")) {
  rule <- match.arg(rule)
  if (length(idx) == 0L) {
    return(NA_integer_)
  }

  elpd <- vapply(results[idx], function(x) as.numeric(x$loo_val), numeric(1L))
  n_pred <- vapply(results[idx], function(x) length(x$preds), integer(1L))
  raw_best <- idx[order(-elpd, n_pred)][1L]

  # loo::loo_compare() has returned a matrix in some loo versions and a
  # data.frame in others. Test for a two-dimensional object with the columns
  # we need rather than for a specific container class: gating on is.matrix()
  # here silently disabled the whole 1se rule under any loo release that
  # returns a data.frame.
  usable <- !is.null(comparison) &&
    !is.null(dim(comparison)) &&
    all(c("elpd_diff", "se_diff") %in% colnames(comparison))
  if (!usable) {
    return(raw_best)
  }

  # Which position in `results` each comparison row refers to. loo <= 2.9
  # carried bayes_loo_comparison()'s "model<i>" labels in the rownames; loo
  # >= 2.10.0 returns a data.frame that ends with `rownames(comp) <- NULL`
  # and puts the labels in a "model" column instead. Reading rownames() there
  # yields "1", "2", ... -- positions in the elpd-sorted table, not positions
  # in `results` -- which passed every guard below and made both rules select
  # the wrong model. So take the labels from the "model" column when it is
  # present, and insist they really are "model<i>" names before trusting them:
  # anything else is not a mapping we can invert, so fall back to raw_best.
  labels <- if ("model" %in% colnames(comparison)) {
    as.character(comparison[, "model"])
  } else {
    rownames(comparison)
  }
  if (length(labels) == 0L || !all(grepl("^model[0-9]+$", labels))) {
    return(raw_best)
  }
  row_idx <- as.integer(sub("^model", "", labels))
  if (anyNA(row_idx) || !all(row_idx %in% idx)) {
    return(raw_best)
  }

  if (rule == "best") {
    return(row_idx[1L])
  }

  elpd_diff <- as.numeric(comparison[, "elpd_diff"])
  se_diff <- as.numeric(comparison[, "se_diff"])
  within <- !is.na(elpd_diff) & !is.na(se_diff) & abs(elpd_diff) <= se_diff
  within[1L] <- TRUE

  candidates <- row_idx[within]
  cand_n_pred <- vapply(results[candidates], function(x) length(x$preds),
                        integer(1L))
  cand_elpd <- vapply(results[candidates], function(x) as.numeric(x$loo_val),
                      numeric(1L))
  candidates[order(cand_n_pred, -cand_elpd)][1L]
}

###############################################################################
# Main function
###############################################################################

#' Bayesian feature selection for model optimization
#'
#' Fits a brms model for every candidate predictor combination and compares
#' them with `loo::loo_compare()`, returning the selected combination as an
#' `fs_result`.
#'
#' @details
#' Use this when you want the predictor subset itself chosen by out-of-sample
#' predictive fit under a fully Bayesian model, and you can afford to fit one
#' model per subset. The search is exhaustive by default: with `p` candidate
#' predictors it fits every non-empty subset, `2^p - 1` models, and each one
#' compiles and samples its own Stan program. Use `max_comb_size` or
#' `sample_combinations` to bound the search.
#'
#' Model selection uses `loo::loo_compare()` rather than the raw elpd
#' maximum. With `rule = "1se"` (the default) the chosen model is the most
#' parsimonious one -- fewest predictors, ties broken by the higher elpd --
#' among those whose elpd difference from the best model is no larger in
#' absolute value than one standard error of that difference (the `se_diff`
#' column of the comparison table). With `rule = "best"` the raw elpd maximum
#' wins, which is the older behavior and is more prone to over-fitting the
#' comparison. When no usable comparison table is available, both rules fall
#' back to the raw elpd maximum, ties broken by fewer predictors.
#'
#' Combinations whose model fails to fit are excluded from selection and
#' counted in `details$n_failed_fits`, with a warning. If no fit yields a
#' finite `elpd_loo` at all, the first successfully fitted model is returned
#' with a warning; that is an arbitrary fallback, not a selection.
#'
#' Two caveats are worth stating plainly. `details$mae` and `details$rmse` are
#' in-sample errors computed on the same rows used to fit and to select, so
#' they are optimistic. And anything read off the returned `brmsfit`
#' (posterior intervals, effect sizes) is post-selection inference: the model
#' was chosen by looking at the same data it is reported on.
#'
#' @param data A data.frame or data.table. It is copied, never modified. Only
#'   `target`, `predictors` and `date_col` are carried forward, and rows with a
#'   missing value in any of them are dropped before any model is fitted.
#' @param target Character. Name of the target (response) column, which must
#'   exist in `data` and suit `brm_family` (numeric for `stats::gaussian()`,
#'   0/1 or logical for `brms::bernoulli()`).
#' @param predictors Character vector. Names of the candidate predictor
#'   columns; at least one, all present in `data`.
#' @param date_col Character or NULL. Name of a date column. When provided, an
#'   `iso_week_id` feature (ISO year * 100 + ISO week) is added to the
#'   predictors. Note: `iso_week_id` enters the models as a continuous
#'   covariate, which is a rough encoding of seasonality; a categorical or
#'   cyclic encoding is deferred to a future version. Default NULL (no date
#'   feature; the date column itself is never used as a predictor).
#' @param brm_family A model family accepted by brms::brm(), for example
#'   stats::gaussian() (default) or brms::bernoulli().
#' @param prior A brms prior specification (default NULL).
#' @param brm_args List. Extra arguments for brms::brm() (for example iter,
#'   warmup, seed), overriding featR's defaults of iter = 2000,
#'   adapt_delta = 0.99, max_treedepth = 15 and refresh = 0. `chains` and
#'   `cores` are respected when evaluating combinations sequentially (`cores`
#'   is capped at the detected core count; the defaults are 4 chains on 1
#'   core), but both are forced to 1 when `parallel_combinations = TRUE`.
#'   Default `list()`.
#' @param rule Selection rule: `"1se"` (default) keeps the most parsimonious
#'   model within one standard error of the best elpd; `"best"` keeps the raw
#'   elpd maximum.
#' @param max_comb_size Whole number >= 1, or NULL. Largest number of
#'   predictors allowed in a combination; values above the number of candidate
#'   predictors are capped rather than rejected. Default NULL (all sizes).
#' @param sample_combinations Whole number >= 1, or NULL. Randomly sample this
#'   many combinations instead of evaluating all of them; ignored when fewer
#'   combinations exist. Pass `seed` to make the draw reproducible. Default
#'   NULL (evaluate every combination).
#' @param parallel_combinations Logical. Evaluate predictor combinations in
#'   parallel via parallel::mclapply(). Not available on Windows (falls back
#'   to sequential evaluation with a message), and falls back to sequential
#'   evaluation when `n_cores` resolves to 1. Because each model is then held
#'   to a single MCMC chain, cross-chain convergence diagnostics such as R-hat
#'   become unavailable; a warning says so. Default FALSE.
#' @param seed Optional integer. When supplied, seeds the random sampling of
#'   combinations (see `sample_combinations`) locally; the previous RNG state
#'   is restored on exit. Default NULL: fs_bayes() never seeds the RNG unless
#'   asked. Note this does not seed the samplers; pass `seed` inside
#'   `brm_args` to control brms itself.
#' @param verbose Logical. Print progress information, and show a progress bar
#'   for sequential evaluation when the suggested pbapply package is
#'   installed. Default FALSE.
#' @param n_cores Whole number >= 1. Worker count used when
#'   `parallel_combinations = TRUE`, and ignored otherwise. Default 1
#'   (sequential); requests are capped at the detected core count.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector of the predictors in the chosen model.}
#'   \item{scores}{`NULL`: no per-feature score is comparable across
#'     combination models. `details$n_features` records how many candidate
#'     predictors were offered.}
#'   \item{method}{"bayes".}
#'   \item{task}{"regression", or "classification" when `brm_family` is one of
#'     the categorical brms families.}
#'   \item{model}{The selected `brmsfit`.}
#'   \item{details}{A list with `data` (the complete-case modeling data.table:
#'     `target`, `predictors`, `date_col` and `iso_week_id` where applicable,
#'     plus appended `fitted_values`, `residuals`, `abs_residuals` and
#'     `squared_residuals` columns), `mae` and `rmse` (in-sample,
#'     post-selection errors computed on the same rows used to fit and select,
#'     so they are optimistic), `formula` (the selected model's formula
#'     string), `best_elpd` (the selected model's elpd_loo, possibly `NA`),
#'     `loo_comparison` (the `loo::loo_compare()` table -- a matrix or
#'     data.frame depending on the loo version -- or `NULL` when fewer than two
#'     models could be compared or the comparison failed), `n_failed_fits`
#'     (how many combinations failed to fit) and `n_features` (the number of
#'     candidate predictors, including `iso_week_id` when `date_col` is
#'     supplied).}
#'   \item{call}{The matched call.}
#' }
#'
#' @examples
#' # Each candidate model compiles a Stan program, so this example is not run
#' # automatically (compilation alone takes far longer than a typical example
#' # budget). The same call is exercised by the package tests.
#' \dontrun{
#' x1 <- seq(-2, 2, length.out = 40)
#' x2 <- rep(c(-1, 1), 20)
#' d <- data.frame(
#'   y  = 1 + 2 * x1 + sin(seq_len(40)),
#'   x1 = x1,
#'   x2 = x2
#' )
#' res <- fs_bayes(
#'   d, target = "y", predictors = c("x1", "x2"),
#'   brm_args = list(chains = 1, iter = 500, refresh = 0),
#'   rule = "1se", verbose = FALSE
#' )
#' res$selected
#' res$details$loo_comparison
#' }
#' @export
fs_bayes <- function(data,
                     target,
                     predictors,
                     date_col = NULL,
                     brm_family = stats::gaussian(),
                     prior = NULL,
                     brm_args = list(),
                     rule = c("1se", "best"),
                     max_comb_size = NULL,
                     sample_combinations = NULL,
                     parallel_combinations = FALSE,
                     seed = NULL,
                     verbose = FALSE,
                     n_cores = 1L) {
  cl_call <- match.call()

  # ---- Input validation ----
  rule <- match.arg(rule)
  assert_data_frame(data, "data")
  assert_string(target, "target")
  if (!is.character(predictors) || length(predictors) < 1L ||
      anyNA(predictors)) {
    stop("'predictors' must be a character vector with at least one column name.")
  }
  if (!is.null(date_col)) {
    assert_string(date_col, "date_col")
  }
  if (!is.list(brm_args)) {
    stop("'brm_args' must be a list.")
  }
  assert_flag(parallel_combinations, "parallel_combinations")
  if (!is.null(max_comb_size)) {
    max_comb_size <- assert_count(max_comb_size, "max_comb_size")
  }
  if (!is.null(sample_combinations)) {
    sample_combinations <- assert_count(sample_combinations, "sample_combinations")
  }
  if (!is.null(seed)) {
    assert_number(seed, "seed")
  }
  assert_flag(verbose, "verbose")
  n_cores <- resolve_cores(n_cores)

  fs_require(c("brms", "loo"), "Bayesian feature selection")

  # Normalize to data.table (always a copy; user data is never mutated)
  data <- as_dt(data)

  # Validate (with family-aware checks)
  bayes_validate_data(
    data       = data,
    target     = target,
    predictors = predictors,
    date_col   = date_col,
    brm_family = brm_family
  )

  task <- bayes_task_from_family(brm_family)

  # Keep only required columns initially, drop NAs
  req_cols <- unique(c(target, predictors, date_col))
  data <- stats::na.omit(data[, req_cols, with = FALSE])
  if (nrow(data) == 0L) {
    stop("No complete cases in the data after removing missing values.")
  }

  # Add ISO week feature if requested
  if (!is.null(date_col)) {
    week <- bayes_add_week_feature(data, date_col, predictors)
    data <- week$data
    predictors <- week$predictors
    # Drop rows where iso_week_id could not be formed (should be rare)
    data <- data[!is.na(data[["iso_week_id"]])]
    if (nrow(data) == 0L) {
      stop("No rows remain after constructing iso_week_id and removing NAs.")
    }
  }

  n_candidates <- length(predictors)

  # Build combinations (optionally sampled; seeding is local and opt-in)
  combinations <- bayes_generate_predictor_combinations(
    predictors          = predictors,
    max_comb_size       = max_comb_size,
    sample_combinations = sample_combinations,
    seed                = seed
  )

  if (verbose) {
    message("Total combinations to evaluate: ", length(combinations))
  }

  if (length(combinations) == 0L) {
    stop("No predictor combinations generated. Check predictors/max_comb_size/sample_combinations.")
  }

  # ---- Resolve outer parallelism (opt-in, capped, POSIX only) ----
  if (parallel_combinations && .Platform$OS.type == "windows") {
    message("parallel_combinations = TRUE is not supported on Windows; ",
            "evaluating combinations sequentially.")
    parallel_combinations <- FALSE
  }
  n_cores_outer <- 1L
  if (parallel_combinations) {
    n_cores_outer <- n_cores
    if (n_cores_outer < 2L) {
      message("parallel_combinations = TRUE but 'n_cores' resolves to 1; ",
              "evaluating combinations sequentially.")
      parallel_combinations <- FALSE
    }
  }

  # ---- Decide brms chains/cores (never parallel unless requested) ----
  if (parallel_combinations) {
    # When parallelizing over combinations, keep each model single-chain and
    # single-core to avoid oversubscription.
    brms_chains <- 1L
    brms_cores  <- 1L
    warning("parallel_combinations = TRUE forces each model to a single MCMC chain; ",
            "cross-chain convergence diagnostics (such as R-hat) will be unavailable.")
  } else {
    brms_chains <- if (!is.null(brm_args$chains)) {
      assert_count(brm_args$chains, "brm_args$chains")
    } else {
      4L
    }
    brms_cores <- if (!is.null(brm_args$cores)) {
      resolve_cores(brm_args$cores, arg = "brm_args$cores")
    } else {
      1L
    }
  }

  # Finalize brm args (user overrides respected within the limits above)
  safe_brm_args <- brm_args
  safe_brm_args$chains <- brms_chains
  safe_brm_args$cores  <- brms_cores

  # Evaluation function for one combination
  eval_func <- function(preds) {
    bayes_evaluate_combination(
      preds      = preds,
      data       = data,
      target     = target,
      brm_family = brm_family,
      prior      = prior,
      brm_args   = safe_brm_args,
      verbose    = verbose
    )
  }

  # Start timer
  start_time <- Sys.time()

  # ---- Evaluate all combinations ----
  if (parallel_combinations) {
    results_list <- parallel::mclapply(combinations, eval_func,
                                       mc.cores = n_cores_outer)
  } else if (verbose && requireNamespace("pbapply", quietly = TRUE)) {
    old_pbo <- pbapply::pboptions(type = "txt")
    on.exit(pbapply::pboptions(old_pbo), add = TRUE)
    results_list <- pbapply::pblapply(combinations, eval_func)
  } else {
    results_list <- lapply(combinations, eval_func)
  }

  # End timer
  elapsed_time <- difftime(Sys.time(), start_time, units = "secs")
  if (verbose) {
    message(
      "Time elapsed for model evaluation: ",
      round(as.numeric(elapsed_time), 2),
      " seconds"
    )
  }

  # ---- Select best model ----
  # A crashed parallel worker yields a 'try-error' (not a list), on which
  # `x$model` would itself error; guard with is.list() before any `$` access.
  failed <- vapply(
    results_list,
    function(x) !is.list(x) || is.null(x$model),
    logical(1L)
  )
  n_failed_fits <- sum(failed)
  if (n_failed_fits > 0L) {
    warning(sprintf(
      "%d of %d model fits failed and were excluded from selection.",
      n_failed_fits, length(results_list)
    ))
  }

  usable <- vapply(
    results_list,
    function(x) is.list(x) && !is.null(x$model) && is.finite(x$loo_val),
    logical(1L)
  )
  usable_idx <- which(usable)

  loo_comparison <- NULL
  if (length(usable_idx) == 0L) {
    # If no finite LOO, fall back to any fitted model
    fitted_idx <- which(vapply(
      results_list,
      function(x) is.list(x) && !is.null(x$model),
      logical(1L)
    ))
    if (length(fitted_idx) == 0L) {
      stop("No valid models were fitted. Every candidate model failed to ",
           "sample, so there is nothing to select from. The usual causes are ",
           "a Stan toolchain that brms cannot use (installing brms is not ",
           "enough; a working C++ compiler must be configured), invalid ",
           "'brm_args' such as a 'warmup' that is not smaller than 'iter', ",
           "or a model specification the data cannot support. Re-run with ",
           "verbose = TRUE to see the per-model failure messages.")
    }
    warning("No finite LOO selection criterion was available; ",
            "returning the first successfully fitted model, which is an arbitrary choice.")
    best <- results_list[[fitted_idx[1L]]]
  } else {
    loo_comparison <- bayes_loo_comparison(results_list, usable_idx)
    chosen <- bayes_pick_model(results_list, usable_idx, loo_comparison, rule)
    best <- results_list[[chosen]]
    if (verbose) {
      message("Selected model formula (rule = '", rule, "'): ",
              best$formula_str)
      message("elpd_loo of the selected model: ", best$loo_val)
    }
  }

  # Append metrics and compute summary errors (in-sample, post-selection).
  # A brmsfit can exist without posterior draws when sampling itself failed
  # (bad brm_args, divergent initialisation); fitted() would then fail deep
  # inside brms, so surface an actionable error here instead.
  data_with_metrics <- tryCatch(
    bayes_add_metrics_to_data(data.table::copy(data), best$model,
                              verbose = verbose),
    error = function(e) {
      msg <- conditionMessage(e)
      # Only claim a draws problem when that is what brms actually reported;
      # relabelling every error here once disguised a shape mismatch as a
      # sampling failure.
      if (grepl("draws", msg, ignore.case = TRUE)) {
        stop("The selected model contains no usable posterior draws, so ",
             "fitted values could not be computed (", msg, "). This usually ",
             "means sampling failed for every candidate model; check ",
             "'brm_args' (for example that 'warmup' is smaller than 'iter') ",
             "and the model specification.", call. = FALSE)
      }
      stop("Could not compute fitted values for the selected model: ", msg,
           call. = FALSE)
    }
  )

  # NULL when the family has no single fitted value per row (see
  # bayes_add_metrics_to_data), in which case these metrics are undefined.
  mae <- if (is.null(data_with_metrics$abs_residuals)) {
    NA_real_
  } else {
    mean(data_with_metrics$abs_residuals, na.rm = TRUE)
  }
  rmse <- if (is.null(data_with_metrics$squared_residuals)) {
    NA_real_
  } else {
    sqrt(mean(data_with_metrics$squared_residuals, na.rm = TRUE))
  }

  if (verbose) {
    message("MAE: ", signif(mae, 6), " | RMSE: ", signif(rmse, 6))
  }

  new_fs_result(
    selected = best$preds,
    scores   = NULL,
    method   = "bayes",
    task     = task,
    model    = best$model,
    details  = list(
      data           = data_with_metrics,
      mae            = mae,
      rmse           = rmse,
      formula        = best$formula_str,
      best_elpd      = best$loo_val,
      loo_comparison = loo_comparison,
      n_failed_fits  = n_failed_fits,
      n_features     = n_candidates
    ),
    call = cl_call
  )
}
