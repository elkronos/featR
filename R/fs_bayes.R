###############################################################################
# Load Required Packages
###############################################################################
suppressPackageStartupMessages({
  library(brms)
  library(tidyverse)
  library(parallel)
  library(loo)
  library(data.table)
  library(pbapply)
})

###############################################################################
# Utility Functions
###############################################################################

#' Validate Data Columns and Types
#'
#' Checks that the response, predictor, and (if provided) date columns exist
#' in the data. Stops execution with informative messages if not.
#'
#' @param data A data.frame or data.table.
#' @param response_col Character. Name of the response column.
#' @param predictor_cols Character vector. Names of predictor columns.
#' @param date_col Character or NULL. Name of the date column.
#' @param brm_family A brms family (default gaussian()) used for basic type checks.
#' @return Invisibly TRUE if validation passes.
validate_data <- function(data, response_col, predictor_cols, date_col = NULL, brm_family = gaussian()) {
  if (!is.data.frame(data) && !data.table::is.data.table(data)) {
    stop("Input data must be a data.frame or data.table.")
  }
  if (!response_col %in% names(data)) {
    stop("Response column '", response_col, "' not found in data.")
  }
  if (length(predictor_cols) < 1) {
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
    stop("gaussian() family requires a numeric response; got class: ", paste(class(y), collapse = "/"))
  }
  if (identical(fam, "bernoulli")) {
    uy <- unique(na.omit(y))
    if (!is.numeric(y) && !is.logical(y)) {
      stop("bernoulli() family expects binary numeric or logical response.")
    }
    if (length(uy) > 2 || !all(uy %in% c(0, 1, TRUE, FALSE))) {
      stop("bernoulli() family expects values in {0,1} (or logical).")
    }
  }
  
  invisible(TRUE)
}

#' Add ISO Week Feature from Date Column
#'
#' Converts the date column to Date class if needed, adds "iso_week_id"
#' (year*100 + ISO week) to the data.table, and ensures it's added to predictors.
#'
#' @param data A data.table.
#' @param date_col Character. Name of the date column.
#' @param predictor_cols Character vector. Current predictor columns.
#' @return A list with updated `data` and `predictor_cols`.
add_week_feature <- function(data, date_col, predictor_cols) {
  if (!inherits(data[[date_col]], "Date")) {
    data[[date_col]] <- as.Date(data[[date_col]])
  }
  # ISO year/week; robust across years and boundaries
  iso_year <- as.integer(strftime(data[[date_col]], "%G"))
  iso_week <- as.integer(strftime(data[[date_col]], "%V"))
  data[, iso_week_id := iso_year * 100L + iso_week]
  predictor_cols <- unique(c(predictor_cols, "iso_week_id"))
  list(data = data, predictor_cols = predictor_cols)
}

###############################################################################
# Predictor Combination and Model Fitting Functions
###############################################################################

#' Generate Predictor Combinations
#'
#' Generates all non-empty combinations up to a maximum size and optionally samples.
generate_predictor_combinations <- function(predictor_cols, max_comb_size = NULL, sample_combinations = NULL, seed = 123) {
  n <- length(predictor_cols)
  max_comb_size <- if (is.null(max_comb_size)) n else min(max_comb_size, n)
  comb_list <- lapply(1:max_comb_size, function(i) combn(predictor_cols, i, simplify = FALSE))
  combinations <- unlist(comb_list, recursive = FALSE)
  
  if (!is.null(sample_combinations) && length(combinations) > sample_combinations) {
    set.seed(seed)
    combinations <- sample(combinations, sample_combinations)
  }
  combinations
}

#' Fit a Bayesian Model Using brms
#'
#' @param data A data.frame or data.table containing the data.
#' @param formula_str Character. The model formula as a string.
#' @param brm_family A brms family (default gaussian()).
#' @param prior A brms prior specification (default NULL).
#' @param brm_args List. Additional arguments for `brm()`.
#' @param verbose Logical.
#' @return A fitted brms model object, or NULL if fitting fails.
fit_model <- function(data, formula_str, brm_family, prior, brm_args, verbose = TRUE) {
  if (verbose) message("Fitting model: ", formula_str)
  
  default_args <- list(
    formula   = as.formula(formula_str),
    data      = data,
    family    = brm_family,
    prior     = prior,
    iter      = 2000,
    warmup    = 1000,
    control   = list(adapt_delta = 0.99, max_treedepth = 15),
    refresh   = 0
  )
  
  model_args <- modifyList(default_args, brm_args)
  
  # If using fixed_param, remove control settings
  if (!is.null(model_args$algorithm) && model_args$algorithm == "fixed_param") {
    model_args$control <- NULL
  }
  
  model <- tryCatch({
    do.call(brm, model_args)
  }, warning = function(w) {
    if (verbose) message("Warning [", formula_str, "]: ", conditionMessage(w))
    NULL
  }, error = function(e) {
    if (verbose) message("Error [", formula_str, "]: ", conditionMessage(e))
    NULL
  })
  
  if (!is.null(model) && verbose) {
    message("Successfully fitted: ", formula_str)
  }
  model
}

#' Append Fitted Metrics to Data
#'
#' Adds fitted values and residual-based metrics to the data.table.
add_metrics_to_data <- function(data, model) {
  # brms residuals()/fitted() provide "Estimate" column
  fitted_vals <- as.numeric(fitted(model)[, "Estimate"])
  resid_vals  <- as.numeric(residuals(model)[, "Estimate"])
  
  data[, `:=`(
    fitted_values     = fitted_vals,
    residuals         = resid_vals,
    abs_residuals     = abs(resid_vals),
    squared_residuals = resid_vals^2
  )]
  data
}

#' Evaluate a Predictor Combination
#'
#' Fits and scores a model (LOO when applicable).
evaluate_combination <- function(preds, data, response_col, brm_family, prior, brm_args, verbose = TRUE) {
  formula_str <- paste0(response_col, " ~ ", paste(preds, collapse = " + "))
  model <- fit_model(data, formula_str, brm_family, prior, brm_args, verbose = verbose)
  
  loo_val <- NA_real_
  if (!is.null(model)) {
    if (!is.null(brm_args$algorithm) && brm_args$algorithm == "fixed_param") {
      if (verbose) message("Skipping LOO for fixed_param algorithm: ", formula_str)
      loo_val <- NA_real_
    } else {
      loo_obj <- tryCatch(loo(model),
                          error = function(e) {
                            if (verbose) message("LOO failed [", formula_str, "]: ", conditionMessage(e))
                            NULL
                          })
      if (!is.null(loo_obj)) {
        loo_val <- tryCatch(loo_obj$estimates["elpd_loo", "Estimate"], error = function(e) NA_real_)
      }
    }
  }
  
  list(preds = preds, model = model, loo_val = loo_val, formula_str = formula_str)
}

###############################################################################
# Main Function: Bayesian Feature Selection and Model Optimization
###############################################################################

#' Bayesian Feature Selection for Model Optimization
#'
#' Evaluates predictor combinations with brms models and selects the best by elpd_loo.
#'
#' @param data A data.frame or data.table.
#' @param response_col Character. Name of the response variable.
#' @param predictor_cols Character vector. Names of predictor variables.
#' @param date_col Character or NULL. Name of the date column (if provided, an ISO week feature is added).
#' @param brm_family A brms family. Default gaussian().
#' @param prior A brms prior specification (default NULL).
#' @param brm_args List. Extra args for brm(); chains/cores/seed handled below.
#' @param early_stop_threshold Numeric. Reserved for future use (unused).
#' @param parallel_combinations Logical. Parallelize across predictor combinations.
#' @param max_comb_size Integer or NULL. Max predictors in any combination.
#' @param sample_combinations Integer or NULL. Randomly sample this many combinations.
#' @param show_progress Logical. Show progress bar when not parallelizing.
#' @param verbose Logical.
#' @return A list with:
#'   - Model: best brmsfit
#'   - Data: data.table with appended metrics
#'   - MAE: mean absolute error
#'   - RMSE: root mean squared error
fs_bayes <- function(
    data, response_col, predictor_cols, date_col = NULL,
    brm_family = gaussian(), prior = NULL, brm_args = list(), early_stop_threshold = 0.01,
    parallel_combinations = FALSE, max_comb_size = NULL, sample_combinations = NULL,
    show_progress = TRUE, verbose = TRUE
) {
  
  # Decide outer cores once
  n_cores_outer <- max(parallel::detectCores() - 1, 1)
  
  # Normalize data.table and basic cleaning
  data <- as.data.table(data)
  
  # Validate (with family-aware checks)
  validate_data(data, response_col, predictor_cols, date_col, brm_family = brm_family)
  
  # Keep only required columns initially, drop NAs
  req_cols <- unique(c(response_col, predictor_cols, date_col))
  data <- na.omit(data[, ..req_cols])
  if (nrow(data) == 0) stop("No complete cases in the data after removing missing values.")
  
  # Add ISO week feature if requested, then drop any new NAs
  if (!is.null(date_col)) {
    res <- add_week_feature(data, date_col, predictor_cols)
    data <- res$data
    predictor_cols <- res$predictor_cols
    # Drop rows where iso_week_id couldn't be formed (should be rare)
    data <- data[!is.na(iso_week_id)]
  }
  
  # Build combinations (optionally sampled, with reproducible seed)
  seed_default <- if (!is.null(brm_args$seed)) brm_args$seed else 1234L
  combinations <- generate_predictor_combinations(
    predictor_cols, max_comb_size, sample_combinations, seed = seed_default
  )
  if (verbose) message("Total combinations to evaluate: ", length(combinations))
  
  # Decide brms cores/chains based on outer parallelism (avoid oversubscription)
  chains_default <- 4L
  if (parallel_combinations) {
    brms_cores  <- 1L
    brms_chains <- 1L
  } else {
    # Let brms parallelize over chains using available cores
    brms_chains <- if (!is.null(brm_args$chains)) brm_args$chains else min(chains_default, n_cores_outer)
    brms_cores  <- if (!is.null(brm_args$cores))  brm_args$cores  else brms_chains
  }
  
  # Finalize brm args with safe defaults (but allow user overrides unless unsafe)
  safe_brm_args <- brm_args
  safe_brm_args$chains <- brms_chains
  safe_brm_args$cores  <- brms_cores
  if (is.null(safe_brm_args$seed)) safe_brm_args$seed <- seed_default
  
  # Evaluation function
  eval_func <- function(preds) {
    evaluate_combination(
      preds = preds,
      data = data,
      response_col = response_col,
      brm_family = brm_family,
      prior = prior,
      brm_args = safe_brm_args,
      verbose = verbose
    )
  }
  
  # Start timer
  start_time <- Sys.time()
  
  # Evaluate all combinations
  if (parallel_combinations) {
    if (.Platform$OS.type == "windows") {
      cl <- makeCluster(n_cores_outer)
      on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
      clusterEvalQ(cl, {
        library(brms); library(data.table); library(loo); library(tidyverse)
      })
      clusterExport(
        cl,
        varlist = c("data", "response_col", "prior", "safe_brm_args",
                    "brm_family", "verbose", "evaluate_combination",
                    "fit_model"),
        envir = environment()
      )
      results_list <- parLapply(cl, combinations, eval_func)
    } else {
      results_list <- mclapply(combinations, eval_func, mc.cores = n_cores_outer)
    }
  } else if (show_progress) {
    pboptions(type = "txt")
    results_list <- pblapply(combinations, eval_func)
  } else {
    results_list <- lapply(combinations, eval_func)
  }
  
  # End timer
  elapsed_time <- difftime(Sys.time(), start_time, units = "secs")
  if (verbose) message("Time elapsed for model evaluation: ", round(as.numeric(elapsed_time), 2), " seconds")
  
  # Select best model — require a fitted model and finite LOO
  valid_results <- Filter(function(x) !is.null(x$model) && is.finite(x$loo_val), results_list)
  
  if (length(valid_results) == 0L) {
    # If no finite LOO, fall back to any fitted model
    fallback <- Filter(function(x) !is.null(x$model), results_list)
    if (length(fallback) == 0L) {
      stop("No valid models were fitted. Please check your data and model specifications.")
    } else {
      if (verbose) message("No finite LOO available; selecting the first successfully fitted model.")
      best <- fallback[[1L]]
    }
  } else {
    ord <- order(vapply(valid_results, function(x) x$loo_val, numeric(1)), decreasing = TRUE)
    best <- valid_results[[ord[1L]]]
    if (verbose) {
      message("Best Model Formula: ", best$formula_str)
      message("Best elpd_loo: ", best$loo_val)
    }
  }
  
  # Append metrics and compute summary errors
  data_with_metrics <- add_metrics_to_data(copy(data), best$model)
  mae  <- mean(data_with_metrics$abs_residuals, na.rm = TRUE)
  rmse <- sqrt(mean(data_with_metrics$squared_residuals, na.rm = TRUE))
  
  if (verbose) message("MAE: ", signif(mae, 6), " | RMSE: ", signif(rmse, 6))
  
  list(Model = best$model, Data = data_with_metrics, MAE = mae, RMSE = rmse)
}
