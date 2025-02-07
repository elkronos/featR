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
#' @return Invisibly returns TRUE if validation passes.
validate_data <- function(data, response_col, predictor_cols, date_col = NULL) {
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
  invisible(TRUE)
}

#' Add Week Feature from Date Column
#'
#' Converts the date column to Date class if needed, adds a "week" column to the
#' data.table, and ensures that "week" is added to the predictors.
#'
#' @param data A data.table.
#' @param date_col Character. Name of the date column.
#' @param predictor_cols Character vector. Current predictor columns.
#' @return A list with updated `data` and `predictor_cols`.
add_week_feature <- function(data, date_col, predictor_cols) {
  if (!inherits(data[[date_col]], "Date")) {
    data[[date_col]] <- as.Date(data[[date_col]])
  }
  data[, week := as.integer(format(get(date_col), "%U"))]
  predictor_cols <- unique(c(predictor_cols, "week"))
  list(data = data, predictor_cols = predictor_cols)
}

###############################################################################
# Predictor Combination and Model Fitting Functions
###############################################################################

#' Generate Predictor Combinations
#'
#' Generates all non-empty combinations (subsets) of predictor variables up to
#' a maximum size. Optionally, a random subset of combinations can be returned.
#'
#' @param predictor_cols Character vector of predictor names.
#' @param max_comb_size Integer or NULL. Maximum number of predictors in a combination.
#' @param sample_combinations Integer or NULL. If provided, randomly sample this many combinations.
#' @return A list where each element is a character vector of predictors.
generate_predictor_combinations <- function(predictor_cols, max_comb_size = NULL, sample_combinations = NULL) {
  n <- length(predictor_cols)
  max_comb_size <- if (is.null(max_comb_size)) n else min(max_comb_size, n)
  
  comb_list <- lapply(1:max_comb_size, function(i) combn(predictor_cols, i, simplify = FALSE))
  combinations <- unlist(comb_list, recursive = FALSE)
  
  if (!is.null(sample_combinations) && length(combinations) > sample_combinations) {
    set.seed(123)  # For reproducibility; adjust or remove as needed.
    combinations <- sample(combinations, sample_combinations)
  }
  combinations
}

#' Fit a Bayesian Model Using brms
#'
#' Fits a Bayesian regression model using the provided formula string and additional arguments.
#'
#' @param data A data.frame or data.table containing the data.
#' @param formula_str Character. The model formula as a string.
#' @param n_cores Integer. Number of cores to use.
#' @param prior A brms prior specification (default is NULL).
#' @param brm_args List. Additional arguments for `brm()`.
#' @param verbose Logical. Whether to print informative messages.
#' @return A fitted brms model object, or NULL if fitting fails.
fit_model <- function(data, formula_str, n_cores, prior, brm_args, verbose = TRUE) {
  if (verbose) message("Fitting model with formula: ", formula_str)
  
  default_args <- list(
    formula   = as.formula(formula_str),
    data      = data,
    family    = gaussian(),
    prior     = prior,
    cores     = n_cores,
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
    if (verbose) message("Warning for formula [", formula_str, "]: ", conditionMessage(w))
    NULL
  }, error = function(e) {
    if (verbose) message("Error for formula [", formula_str, "]: ", conditionMessage(e))
    NULL
  })
  
  if (!is.null(model) && verbose) {
    message("Successfully fitted model for: ", formula_str)
  }
  model
}

#' Append Fitted Metrics to Data
#'
#' Computes fitted values and residuals from the fitted model and appends
#' several metrics to the data.table.
#'
#' @param data A data.table.
#' @param model A fitted brms model.
#' @return The data.table with additional columns: fitted_values, residuals,
#'         abs_residuals, and squared_residuals.
add_metrics_to_data <- function(data, model) {
  fitted_vals <- fitted(model)[, "Estimate"]
  resid_vals  <- resid(model)[, "Estimate"]
  
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
#' Fits a model for a given set of predictors and returns the model along with
#' its LOO metric. For models fitted with fixed parameters (where LOO is not applicable),
#' LOO evaluation is skipped.
#'
#' @param preds Character vector of predictor names.
#' @param data A data.table.
#' @param response_col Character. Name of the response column.
#' @param n_cores Integer. Number of cores.
#' @param prior A brms prior specification.
#' @param brm_args List. Additional arguments for `brm()`.
#' @param verbose Logical. Whether to print informative messages.
#' @return A list with elements: preds, model, loo_val (or NA if not available),
#'         and formula_str.
evaluate_combination <- function(preds, data, response_col, n_cores, prior, brm_args, verbose = TRUE) {
  formula_str <- paste0(response_col, " ~ ", paste(preds, collapse = " + "))
  model <- fit_model(data, formula_str, n_cores, prior, brm_args, verbose = verbose)
  
  loo_val <- NA
  if (!is.null(model)) {
    if (!is.null(brm_args$algorithm) && brm_args$algorithm == "fixed_param") {
      if (verbose) message("Skipping LOO for fixed_param algorithm for formula: ", formula_str)
      # LOO is not applicable when algorithm is "fixed_param"
      loo_val <- NA
    } else {
      loo_obj <- tryCatch(loo(model),
                          error = function(e) {
                            if (verbose) message("LOO failed for formula [", formula_str, "]: ", conditionMessage(e))
                            NULL
                          })
      if (!is.null(loo_obj)) {
        loo_val <- loo_obj$estimates["elpd_loo", "Estimate"]
      } else {
        loo_val <- -Inf  # Penalize models that fail LOO evaluation.
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
#' Evaluates various combinations of predictor variables by fitting Bayesian models.
#' The best model is selected based on the expected log predictive density (elpd_loo),
#' when available. (Note: The `early_stop_threshold` parameter is reserved for future enhancement
#' and is not currently used.)
#'
#' @param data A data.frame or data.table.
#' @param response_col Character. Name of the response variable.
#' @param predictor_cols Character vector. Names of predictor variables.
#' @param date_col Character or NULL. Name of the date column (if provided, a week feature is added).
#' @param prior A brms prior specification (default is NULL).
#' @param brm_args List. Additional arguments for `brm()` (e.g., iter, warmup, chains).
#' @param early_stop_threshold Numeric. Reserved for future use.
#' @param parallel_combinations Logical. Whether to evaluate predictor combinations in parallel.
#' @param max_comb_size Integer or NULL. Maximum number of predictors to include in any combination.
#' @param sample_combinations Integer or NULL. If provided, randomly sample this many combinations.
#' @param show_progress Logical. If TRUE and not using parallel evaluation, display a progress bar.
#' @param verbose Logical. Whether to print informative messages.
#' @return A list containing:
#'   - Model: the best fitted brms model,
#'   - Data: the data.table with appended metrics,
#'   - MAE: Mean Absolute Error,
#'   - RMSE: Root Mean Squared Error.
fs_bayes <- function(data, response_col, predictor_cols, date_col = NULL,
                     prior = NULL, brm_args = list(), early_stop_threshold = 0.01,
                     parallel_combinations = FALSE, max_comb_size = NULL, sample_combinations = NULL,
                     show_progress = TRUE, verbose = TRUE) {
  
  # Validate and prepare data
  validate_data(data, response_col, predictor_cols, date_col)
  data <- as.data.table(data)
  req_cols <- unique(c(response_col, predictor_cols, date_col))
  data <- na.omit(data[, ..req_cols])
  if (nrow(data) == 0) {
    stop("No complete cases in the data after removing missing values.")
  }
  
  if (!is.null(date_col)) {
    res <- add_week_feature(data, date_col, predictor_cols)
    data <- res$data
    predictor_cols <- res$predictor_cols
  }
  
  # Generate predictor combinations (if too many predictors, the sample_combinations option can be used)
  combinations <- generate_predictor_combinations(predictor_cols, max_comb_size, sample_combinations)
  if (verbose) message("Total combinations to evaluate: ", length(combinations))
  
  n_cores <- max(parallel::detectCores() - 1, 1)
  
  # Define evaluation function for one combination.
  eval_func <- function(preds) {
    evaluate_combination(preds, data, response_col, n_cores, prior, brm_args, verbose = verbose)
  }
  
  # Start timer
  start_time <- Sys.time()
  
  # Evaluate all combinations with optional progress bar.
  if (parallel_combinations) {
    if (.Platform$OS.type == "windows") {
      cl <- makeCluster(n_cores)
      # Load required packages on each worker.
      clusterEvalQ(cl, {
        library(brms)
        library(data.table)
        library(loo)
        library(tidyverse)
      })
      # Export necessary objects and functions.
      clusterExport(cl,
                    varlist = c("data", "response_col", "n_cores", "prior",
                                "brm_args", "verbose", "eval_func",
                                "evaluate_combination", "fit_model"),
                    envir = environment())
      results_list <- parLapply(cl, combinations, eval_func)
      stopCluster(cl)
    } else {
      results_list <- mclapply(combinations, eval_func, mc.cores = n_cores)
    }
  } else if (show_progress) {
    pboptions(type = "txt")
    results_list <- pblapply(combinations, eval_func)
  } else {
    results_list <- lapply(combinations, eval_func)
  }
  
  # End timer and display elapsed time.
  end_time <- Sys.time()
  elapsed_time <- difftime(end_time, start_time, units = "secs")
  if (verbose) message("Time elapsed for model evaluation: ", elapsed_time, " seconds")
  
  # Select best model based on LOO when available.
  valid_results <- Filter(function(x) !is.na(x$loo_val), results_list)
  if (length(valid_results) == 0) {
    if (verbose) message("LOO evaluation not available for any model; selecting first fitted model.")
    valid_results <- results_list[!sapply(results_list, function(x) is.null(x$model))]
    if (length(valid_results) == 0) {
      stop("No valid models were fitted. Please check your data and model specifications.")
    }
  }
  
  # Order valid results by elpd_loo in descending order.
  valid_results <- valid_results[order(sapply(valid_results, function(x) x$loo_val), decreasing = TRUE)]
  best <- valid_results[[1]]
  
  if (verbose) {
    message("Best Model Formula: ", best$formula_str)
    if (!is.na(best$loo_val))
      message("Best elpd_loo: ", best$loo_val)
  }
  
  data <- add_metrics_to_data(data, best$model)
  
  mae  <- mean(data$abs_residuals, na.rm = TRUE)
  rmse <- sqrt(mean(data$squared_residuals, na.rm = TRUE))
  
  if (verbose) {
    message("MAE: ", mae, " | RMSE: ", rmse)
  }
  
  list(Model = best$model, Data = data, MAE = mae, RMSE = rmse)
}
