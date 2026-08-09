# Bayesian feature selection via brms model comparison.
# brms, loo, and pbapply are Suggests and are only touched at run time.

###############################################################################
# Helpers
###############################################################################

#' Validate data columns and types for fs_bayes()
#'
#' Checks that the response, predictor, and (if provided) date columns exist
#' in the data. Performs minimal family-aware checks on the response.
#'
#' @param data A data.frame or data.table.
#' @param response_col Character. Name of the response column.
#' @param predictor_cols Character vector. Names of predictor columns.
#' @param date_col Character or NULL. Name of the date column.
#' @param brm_family A model family used for basic type checks.
#' @return Invisibly TRUE if validation passes.
#' @noRd
bayes_validate_data <- function(data,
                                response_col,
                                predictor_cols,
                                date_col = NULL,
                                brm_family = stats::gaussian()) {
  if (!response_col %in% names(data)) {
    stop("Response column '", response_col, "' not found in data.")
  }
  if (length(predictor_cols) < 1L) {
    stop("At least one predictor column must be provided.")
  }
  if (any(!predictor_cols %in% names(data))) {
    missing_preds <- predictor_cols[!predictor_cols %in% names(data)]
    stop("Missing predictor columns: ", paste(missing_preds, collapse = ", "))
  }
  if (!is.null(date_col) && !date_col %in% names(data)) {
    stop("Date column '", date_col, "' not found in data.")
  }

  # Minimal sanity checks re: family/response
  fam <- brm_family$family
  y <- data[[response_col]]

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

#' Add ISO week feature from a date column
#'
#' Converts the date column to Date class if needed, adds "iso_week_id"
#' (ISO year * 100 + ISO week) to the data.table, and appends it to the
#' predictors. Note: iso_week_id later enters the models as a continuous
#' covariate, which is a rough encoding of seasonality.
#'
#' @param data A data.table (modified by reference).
#' @param date_col Character. Name of the date column.
#' @param predictor_cols Character vector. Current predictor columns.
#' @return A list with updated `data` and `predictor_cols`.
#' @noRd
bayes_add_week_feature <- function(data, date_col, predictor_cols) {
  if (!inherits(data[[date_col]], "Date")) {
    data.table::set(data, j = date_col, value = as.Date(data[[date_col]]))
  }

  iso_year <- as.integer(strftime(data[[date_col]], "%G"))
  iso_week <- as.integer(strftime(data[[date_col]], "%V"))

  data.table::set(data, j = "iso_week_id", value = iso_year * 100L + iso_week)
  predictor_cols <- unique(c(predictor_cols, "iso_week_id"))

  list(data = data, predictor_cols = predictor_cols)
}

#' Generate predictor combinations
#'
#' Generates all non-empty combinations up to a maximum size and, optionally,
#' randomly samples a fixed number of combinations.
#'
#' @param predictor_cols Character vector of predictor names.
#' @param max_comb_size Integer or NULL. Maximum size of combinations.
#'   If NULL, uses length(predictor_cols). Must be at least 1 if non-NULL.
#' @param sample_combinations Integer or NULL. If not NULL, randomly sample
#'   this many combinations (must be >= 1).
#' @param seed Optional seed for the sampling step, applied locally and
#'   restored on exit (see local_seed()). Default NULL: never seeds.
#' @return A list of character vectors (predictor subsets).
#' @noRd
bayes_generate_predictor_combinations <- function(predictor_cols,
                                                  max_comb_size = NULL,
                                                  sample_combinations = NULL,
                                                  seed = NULL) {
  n <- length(predictor_cols)
  if (n < 1L) {
    stop("predictor_cols must contain at least one predictor.")
  }

  if (!is.null(max_comb_size)) {
    max_comb_size <- assert_count(max_comb_size, "max_comb_size")
    max_comb_size <- min(max_comb_size, n)
  } else {
    max_comb_size <- n
  }

  comb_list <- lapply(
    X = seq_len(max_comb_size),
    FUN = function(i) utils::combn(predictor_cols, i, simplify = FALSE)
  )
  combinations <- unlist(comb_list, recursive = FALSE)

  if (!is.null(sample_combinations)) {
    sample_combinations <- assert_count(sample_combinations, "sample_combinations")
    if (length(combinations) > sample_combinations) {
      local_seed(seed)
      combinations <- sample(combinations, sample_combinations)
    }
  }

  combinations
}

#' Fit a Bayesian model using brms
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
                            verbose = TRUE) {
  if (verbose) {
    message("Fitting model: ", formula_str)
  }

  default_args <- list(
    formula = stats::as.formula(formula_str),
    data    = data,
    family  = brm_family,
    prior   = prior,
    iter    = 2000,
    warmup  = 1000,
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

#' Append fitted metrics to data
#'
#' Adds fitted values and residual-based metrics to the data.table.
#'
#' @param data A data.table (modified by reference).
#' @param model A brmsfit object.
#' @return The data.table with added columns: fitted_values, residuals,
#'   abs_residuals, squared_residuals.
#' @noRd
bayes_add_metrics_to_data <- function(data, model) {
  fitted_vals <- as.numeric(stats::fitted(model)[, "Estimate"])
  resid_vals  <- as.numeric(stats::residuals(model)[, "Estimate"])

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
#' Fits and scores a model (LOO when applicable).
#'
#' @param preds Character vector of predictor names for this combination.
#' @param data A data.table.
#' @param response_col Name of the response column.
#' @param brm_family A model family accepted by brms::brm().
#' @param prior A brms prior specification, or NULL.
#' @param brm_args List. Additional arguments for brms::brm().
#' @param verbose Logical.
#' @return A list with elements preds, model, loo_val, formula_str.
#' @noRd
bayes_evaluate_combination <- function(preds,
                                       data,
                                       response_col,
                                       brm_family,
                                       prior,
                                       brm_args,
                                       verbose = TRUE) {
  formula_str <- paste0(
    backtick(response_col), " ~ ",
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

  loo_val <- NA_real_

  if (!is.null(model)) {
    if (!is.null(brm_args$algorithm) && identical(brm_args$algorithm, "fixed_param")) {
      if (verbose) {
        message("Skipping LOO for fixed_param algorithm: ", formula_str)
      }
      loo_val <- NA_real_
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
    loo_val     = loo_val,
    formula_str = formula_str
  )
}

###############################################################################
# Main function
###############################################################################

#' Bayesian feature selection for model optimization
#'
#' Evaluates predictor combinations with brms models and selects the best
#' combination by elpd_loo (leave-one-out expected log predictive density).
#'
#' @param data A data.frame or data.table.
#' @param response_col Character. Name of the response variable.
#' @param predictor_cols Character vector. Names of predictor variables.
#' @param date_col Character or NULL. Name of a date column. When provided, an
#'   `iso_week_id` feature (ISO year * 100 + ISO week) is added to the
#'   predictors. Note: `iso_week_id` enters the models as a continuous
#'   covariate, which is a rough encoding of seasonality; a categorical or
#'   cyclic encoding is deferred to a future version.
#' @param brm_family A model family accepted by brms::brm(), for example
#'   stats::gaussian() (default) or brms::bernoulli().
#' @param prior A brms prior specification (default NULL).
#' @param brm_args List. Extra arguments for brms::brm() (for example iter,
#'   warmup, seed). `chains` and `cores` are respected when evaluating
#'   combinations sequentially (`cores` is capped at the detected core
#'   count), but both are forced to 1 when `parallel_combinations = TRUE`.
#' @param parallel_combinations Logical. Evaluate predictor combinations in
#'   parallel via parallel::mclapply(). Not available on Windows (falls back
#'   to sequential evaluation with a message). Default FALSE.
#' @param n_cores Integer or NULL. Worker count used when
#'   `parallel_combinations = TRUE`. NULL (the default) means 1, i.e.
#'   sequential; requests are capped at the detected core count.
#' @param max_comb_size Integer or NULL. Max predictors in any combination.
#' @param sample_combinations Integer or NULL. Randomly sample this many
#'   combinations instead of evaluating all of them.
#' @param seed Optional integer. When supplied, seeds the random sampling of
#'   combinations (see `sample_combinations`) locally; the previous RNG state
#'   is restored on exit. Default NULL: fs_bayes() never seeds the RNG unless
#'   asked. Note this does not seed the samplers; pass `seed` inside
#'   `brm_args` to control brms itself.
#' @param show_progress Logical. Show a progress bar for sequential
#'   evaluation when the suggested pbapply package is installed. Default TRUE.
#' @param verbose Logical. Print progress information. Default TRUE.
#'
#' @return A list with:
#' \describe{
#'   \item{Model}{Best fitted brmsfit object.}
#'   \item{Data}{data.table with appended fitted values and residual metrics.}
#'   \item{MAE}{Mean absolute error of the selected model. This is an
#'     in-sample, post-selection error: it is computed on the same data used
#'     to fit and select the model, so it is optimistic and should not be
#'     read as an out-of-sample error estimate.}
#'   \item{RMSE}{Root mean squared error of the selected model; in-sample and
#'     post-selection, with the same caveat as MAE.}
#'   \item{SelectedPredictors}{Character vector of predictors in the best model.}
#'   \item{SelectedFormula}{Formula string for the best model.}
#'   \item{BestELPD}{elpd_loo for the best model (may be NA).}
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("brms", quietly = TRUE) &&
#'     requireNamespace("loo", quietly = TRUE)) {
#'   x1 <- seq(-2, 2, length.out = 40)
#'   x2 <- rep(c(-1, 1), 20)
#'   d <- data.frame(
#'     y  = 1 + 2 * x1 + sin(seq_len(40)),
#'     x1 = x1,
#'     x2 = x2
#'   )
#'   res <- fs_bayes(
#'     d, "y", c("x1", "x2"),
#'     brm_args = list(chains = 1, iter = 500, refresh = 0),
#'     show_progress = FALSE, verbose = FALSE
#'   )
#'   res$SelectedFormula
#' }
#' }
#' @export
fs_bayes <- function(data,
                     response_col,
                     predictor_cols,
                     date_col = NULL,
                     brm_family = stats::gaussian(),
                     prior = NULL,
                     brm_args = list(),
                     parallel_combinations = FALSE,
                     n_cores = NULL,
                     max_comb_size = NULL,
                     sample_combinations = NULL,
                     seed = NULL,
                     show_progress = TRUE,
                     verbose = TRUE) {
  # ---- Input validation ----
  assert_data_frame(data, "data")
  assert_string(response_col, "response_col")
  if (!is.character(predictor_cols) || length(predictor_cols) < 1L ||
      anyNA(predictor_cols)) {
    stop("'predictor_cols' must be a character vector with at least one column name.")
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
  assert_flag(show_progress, "show_progress")
  assert_flag(verbose, "verbose")

  fs_require(c("brms", "loo"), "Bayesian feature selection")

  # Normalize to data.table (always a copy; user data is never mutated)
  data <- as_dt(data)

  # Validate (with family-aware checks)
  bayes_validate_data(
    data           = data,
    response_col   = response_col,
    predictor_cols = predictor_cols,
    date_col       = date_col,
    brm_family     = brm_family
  )

  # Keep only required columns initially, drop NAs
  req_cols <- unique(c(response_col, predictor_cols, date_col))
  data <- stats::na.omit(data[, req_cols, with = FALSE])
  if (nrow(data) == 0L) {
    stop("No complete cases in the data after removing missing values.")
  }

  # Add ISO week feature if requested
  if (!is.null(date_col)) {
    res <- bayes_add_week_feature(data, date_col, predictor_cols)
    data <- res$data
    predictor_cols <- res$predictor_cols
    # Drop rows where iso_week_id could not be formed (should be rare)
    data <- data[!is.na(data[["iso_week_id"]])]
    if (nrow(data) == 0L) {
      stop("No rows remain after constructing iso_week_id and removing NAs.")
    }
  }

  # Build combinations (optionally sampled; seeding is local and opt-in)
  combinations <- bayes_generate_predictor_combinations(
    predictor_cols      = predictor_cols,
    max_comb_size       = max_comb_size,
    sample_combinations = sample_combinations,
    seed                = seed
  )

  if (verbose) {
    message("Total combinations to evaluate: ", length(combinations))
  }

  if (length(combinations) == 0L) {
    stop("No predictor combinations generated. Check predictor_cols/max_comb_size/sample_combinations.")
  }

  # ---- Resolve outer parallelism (opt-in, capped, POSIX only) ----
  if (parallel_combinations && .Platform$OS.type == "windows") {
    message("parallel_combinations = TRUE is not supported on Windows; ",
            "evaluating combinations sequentially.")
    parallel_combinations <- FALSE
  }
  n_cores_outer <- 1L
  if (parallel_combinations) {
    n_cores_outer <- resolve_cores(n_cores)
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
      preds        = preds,
      data         = data,
      response_col = response_col,
      brm_family   = brm_family,
      prior        = prior,
      brm_args     = safe_brm_args,
      verbose      = verbose
    )
  }

  # Start timer
  start_time <- Sys.time()

  # ---- Evaluate all combinations ----
  if (parallel_combinations) {
    results_list <- parallel::mclapply(combinations, eval_func,
                                       mc.cores = n_cores_outer)
  } else if (show_progress && requireNamespace("pbapply", quietly = TRUE)) {
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
  if (any(failed)) {
    warning(sprintf(
      "%d of %d model fits failed and were excluded from selection.",
      sum(failed), length(results_list)
    ))
  }

  valid_results <- Filter(
    f = function(x) is.list(x) && !is.null(x$model) && is.finite(x$loo_val),
    x = results_list
  )

  if (length(valid_results) == 0L) {
    # If no finite LOO, fall back to any fitted model
    fallback <- Filter(
      f = function(x) is.list(x) && !is.null(x$model),
      x = results_list
    )
    if (length(fallback) == 0L) {
      stop("No valid models were fitted. Please check your data and model specifications.")
    }
    warning("No finite LOO selection criterion was available; ",
            "returning the first successfully fitted model, which is an arbitrary choice.")
    best <- fallback[[1L]]
  } else {
    ord <- order(vapply(valid_results, function(x) x$loo_val, numeric(1L)),
                 decreasing = TRUE)
    best <- valid_results[[ord[1L]]]
    if (verbose) {
      message("Best Model Formula: ", best$formula_str)
      message("Best elpd_loo: ", best$loo_val)
    }
  }

  # Append metrics and compute summary errors (in-sample, post-selection)
  data_with_metrics <- bayes_add_metrics_to_data(data.table::copy(data), best$model)
  mae  <- mean(data_with_metrics$abs_residuals, na.rm = TRUE)
  rmse <- sqrt(mean(data_with_metrics$squared_residuals, na.rm = TRUE))

  if (verbose) {
    message("MAE: ", signif(mae, 6), " | RMSE: ", signif(rmse, 6))
  }

  list(
    Model              = best$model,
    Data               = data_with_metrics,
    MAE                = mae,
    RMSE               = rmse,
    SelectedPredictors = best$preds,
    SelectedFormula    = best$formula_str,
    BestELPD           = best$loo_val
  )
}
