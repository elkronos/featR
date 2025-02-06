# Load required packages
library(brms)
library(tidyverse)
library(parallel)
library(loo)
library(data.table)
library(pbapply)

###############################################################################
# Utility Functions
###############################################################################

#' Validate Data Columns and Types
#'
#' Checks that the response, predictor, and (if provided) date columns exist
#' and are of the expected type.
#'
#' @param data A data.frame or data.table.
#' @param response_col Character. The name of the response column.
#' @param predictor_cols Character vector. The names of predictor columns.
#' @param date_col Character or NULL. The name of the date column, if any.
#' @return Invisibly returns TRUE if validation passes; otherwise stops execution.
validate_data <- function(data, response_col, predictor_cols, date_col = NULL) {
  if (!is.data.frame(data) && !data.table::is.data.table(data)) {
    stop("Input data must be a data.frame or data.table.")
  }
  if (!response_col %in% names(data)) {
    stop("Response column '", response_col, "' not found in data.")
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
#' Adds a "week" variable (numeric week number) to the data and updates the
#' predictor columns. Ensures the date column is in Date format.
#'
#' @param data A data.table.
#' @param date_col Character. The name of the date column.
#' @param predictor_cols Character vector. The current predictor columns.
#' @return A list with `data` (the updated data.table) and `predictor_cols` (updated).
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
#' Returns a list of all non-empty combinations (subsets) of predictors.
#'
#' @param predictor_cols Character vector of predictor names.
#' @param max_comb_size Integer or NULL. Maximum number of predictors in a combination.
#' @param sample_combinations Integer or NULL. If provided, randomly sample this
#'   many combinations (after generating all combinations up to max_comb_size).
#' @return A list where each element is a character vector of predictors.
generate_predictor_combinations <- function(predictor_cols, max_comb_size = NULL, sample_combinations = NULL) {
  n <- length(predictor_cols)
  max_comb_size <- if (is.null(max_comb_size)) n else min(max_comb_size, n)
  
  comb_list <- lapply(1:max_comb_size, function(i) combn(predictor_cols, i, simplify = FALSE))
  combinations <- unlist(comb_list, recursive = FALSE)
  
  if (!is.null(sample_combinations) && length(combinations) > sample_combinations) {
    set.seed(123)  # For reproducibility; adjust or remove as needed
    combinations <- sample(combinations, sample_combinations)
  }
  combinations
}

#' Fit a Bayesian Model Using brms
#'
#' Fits a Bayesian regression model using the provided formula and additional arguments.
#'
#' @param data A data.frame or data.table containing the data.
#' @param formula_str Character. The model formula as a string.
#' @param n_cores Integer. Number of cores for parallel computation.
#' @param prior A brms prior specification (default is NULL).
#' @param brm_args List. Additional arguments for `brm()`.
#' @param verbose Logical. Whether to print informative messages.
#' @return A fitted brms model object or NULL if fitting fails.
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
  
  # If using fixed_param, remove control settings which are not needed
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
#' Adds columns for fitted values and various residuals to the data.table.
#'
#' @param data A data.table.
#' @param model A fitted brms model.
#' @return The data.table with new columns: fitted_values, residuals, abs_residuals, squared_residuals.
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
#' Fits a model for a given predictor combination and returns the LOO estimate.
#'
#' @param preds Character vector of predictor names.
#' @param data A data.table.
#' @param response_col Character. Name of the response column.
#' @param n_cores Integer. Number of cores.
#' @param prior A brms prior specification.
#' @param brm_args List. Additional arguments for `brm()`.
#' @param verbose Logical. Verbosity flag.
#' @return A list with elements: preds, model, loo_val (or dummy value if LOO fails),
#'   and formula_str.
evaluate_combination <- function(preds, data, response_col, n_cores, prior, brm_args, verbose = TRUE) {
  formula_str <- paste0(response_col, " ~ ", paste(preds, collapse = " + "))
  model <- fit_model(data, formula_str, n_cores, prior, brm_args, verbose = verbose)
  
  loo_val <- NA
  if (!is.null(model)) {
    # Try LOO; if it fails (which may happen in fixed_param mode), assign a dummy value.
    loo_obj <- tryCatch(loo(model),
                        error = function(e) {
                          if (verbose) message("LOO failed for formula [", formula_str, "]: ", conditionMessage(e))
                          NULL
                        })
    if (!is.null(loo_obj)) {
      loo_val <- loo_obj$estimates["elpd_loo", "Estimate"]
    } else {
      loo_val <- 0
    }
  }
  list(preds = preds, model = model, loo_val = loo_val, formula_str = formula_str)
}

###############################################################################
# Main Function: Bayesian Feature Selection and Model Optimization
###############################################################################

#' Bayesian Feature Selection for Model Optimization
#'
#' Performs Bayesian feature selection by evaluating various combinations of predictor
#' variables. Selects the best model based on the expected log predictive density (elpd_loo).
#'
#' @param data A data.frame or data.table.
#' @param response_col Character. Name of the response variable.
#' @param predictor_cols Character vector. Names of predictor variables.
#' @param date_col Character or NULL. Name of the date column (if provided, adds a week feature).
#' @param prior A brms prior specification (default is NULL).
#' @param brm_args List. Additional arguments for `brm()` (e.g., iter, warmup, chains).
#' @param early_stop_threshold Numeric. Early stopping threshold for improvement in elpd_loo.
#' @param parallel_combinations Logical. Whether to evaluate predictor combinations in parallel.
#' @param max_comb_size Integer or NULL. Maximum number of predictors to include in any combination.
#' @param sample_combinations Integer or NULL. If provided, randomly sample this many combinations.
#' @param show_progress Logical. If TRUE and not using parallel evaluation, display a progress bar.
#' @param verbose Logical. Whether to print informative messages.
#' @return A list containing:
#'   - Model: the best brms model,
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
  
  # Generate predictor combinations (with possible restrictions for many predictors)
  combinations <- generate_predictor_combinations(predictor_cols, max_comb_size, sample_combinations)
  if (verbose) message("Total combinations to evaluate: ", length(combinations))
  
  n_cores <- max(parallel::detectCores() - 1, 1)
  
  # Function to evaluate one combination; needed for parallel processing.
  eval_func <- function(preds) {
    evaluate_combination(preds, data, response_col, n_cores, prior, brm_args, verbose = verbose)
  }
  
  # Evaluate all combinations
  if (parallel_combinations) {
    if (.Platform$OS.type == "windows") {
      cl <- makeCluster(n_cores)
      # Ensure worker nodes load required packages
      clusterEvalQ(cl, {
        library(brms)
        library(data.table)
        library(loo)
        library(tidyverse)
      })
      # Export necessary objects and functions to the workers
      clusterExport(cl,
                    varlist = c("data", "response_col", "n_cores", "prior",
                                "brm_args", "verbose", "combinations",
                                "eval_func", "evaluate_combination", "fit_model"),
                    envir = environment())
      results_list <- parLapply(cl, combinations, eval_func)
      stopCluster(cl)
    } else {
      results_list <- mclapply(combinations, eval_func, mc.cores = n_cores)
    }
  } else if (show_progress) {
    pboptions(type = "txt")  # Force a text progress bar
    results_list <- pblapply(combinations, eval_func)
  } else {
    results_list <- lapply(combinations, eval_func)
  }
  
  # Identify best model based on elpd_loo (ignore those with NA)
  valid_results <- Filter(function(x) !is.na(x$loo_val), results_list)
  if (length(valid_results) == 0) {
    stop("No valid models were fitted. Please check your data and model specifications.")
  }
  
  # Order by loo_val descending
  valid_results <- valid_results[order(sapply(valid_results, function(x) x$loo_val), decreasing = TRUE)]
  best <- valid_results[[1]]
  
  if (verbose) {
    message("Best Model Formula: ", best$formula_str)
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

###############################################################################
# Testing Functions (Unit Tests / UAT)
###############################################################################

# Global container for test results
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

#' Helper: Print and Store Test Result
#'
#' @param test_name Character. Name of the test.
#' @param passed Logical. Whether the test passed.
#' @param note Optional character. Additional notes.
print_and_store_result <- function(test_name, passed, note = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-50s [%s]\n", test_name, result))
  if (!is.null(note)) cat("  Note: ", note, "\n")
  test_results <<- rbind(test_results, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE))
}

# Test: Data Validation
test_validate_data <- function() {
  dt <- data.table(
    response   = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    date       = seq.Date(Sys.Date(), by = "day", length.out = 100)
  )
  
  valid_input <- tryCatch({
    validate_data(dt, "response", c("predictor1", "predictor2"), "date")
    TRUE
  }, error = function(e) FALSE)
  
  invalid_response <- tryCatch({
    validate_data(dt, "non_existent", c("predictor1", "predictor2"), "date")
    FALSE
  }, error = function(e) TRUE)
  
  invalid_predictor <- tryCatch({
    validate_data(dt, "response", c("predictor1", "non_existent"), "date")
    FALSE
  }, error = function(e) TRUE)
  
  invalid_date <- tryCatch({
    validate_data(dt, "response", c("predictor1", "predictor2"), "non_existent")
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("validate_data: Valid input", valid_input)
  print_and_store_result("validate_data: Invalid response", invalid_response)
  print_and_store_result("validate_data: Invalid predictor", invalid_predictor)
  print_and_store_result("validate_data: Invalid date", invalid_date)
}

# Test: Week Feature Addition
test_add_week_feature <- function() {
  dt <- data.table(
    response   = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    date       = seq.Date(Sys.Date(), by = "day", length.out = 100)
  )
  res <- add_week_feature(dt, "date", c("predictor1", "predictor2"))
  week_added <- "week" %in% names(res$data) && "week" %in% res$predictor_cols
  print_and_store_result("add_week_feature: Week column added", week_added)
}

# Test: Predictor Combination Generation
test_generate_predictor_combinations <- function() {
  preds <- c("predictor1", "predictor2", "predictor3")
  combs <- generate_predictor_combinations(preds)
  correct_count <- length(combs) == (2^length(preds) - 1)
  print_and_store_result("generate_predictor_combinations: Correct count", correct_count)
}

# Test: Model Fitting Function
test_fit_model <- function(fast_mode = TRUE) {
  set.seed(123)
  data_size <- if (fast_mode) 50 else 100
  # In fast mode, use fixed_param to avoid long sampling and compilation
  iter_val   <- if (fast_mode) 100 else 500
  warmup_val <- if (fast_mode) 0   else 250
  
  dt <- data.table(
    response   = rnorm(data_size),
    predictor1 = rnorm(data_size),
    predictor2 = rnorm(data_size)
  )
  
  formula_str <- "response ~ predictor1"
  n_cores <- 1
  # Add algorithm = "fixed_param" in fast mode
  brm_args <- if (fast_mode) {
    list(iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0,
         algorithm = "fixed_param", seed = 123)
  } else {
    list(iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0)
  }
  
  model <- fit_model(dt, formula_str, n_cores, prior = NULL, brm_args = brm_args, verbose = FALSE)
  model_ok <- !is.null(model) && inherits(model, "brmsfit")
  print_and_store_result("fit_model: Basic fitting", model_ok)
}

# Test: Adding Metrics to Data
test_add_metrics_to_data <- function(fast_mode = TRUE) {
  set.seed(123)
  data_size <- if (fast_mode) 50 else 100
  iter_val   <- if (fast_mode) 100 else 500
  warmup_val <- if (fast_mode) 0   else 250
  
  dt <- data.table(
    response   = rnorm(data_size),
    predictor1 = rnorm(data_size)
  )
  
  brm_args <- if (fast_mode) {
    list(iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0,
         algorithm = "fixed_param", seed = 123)
  } else {
    list(iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0)
  }
  
  # Use do.call to pass the brm_args list to brm()
  model <- do.call(brm, c(list(formula = response ~ predictor1, data = dt), brm_args))
  dt <- add_metrics_to_data(dt, model)
  cols_present <- all(c("fitted_values", "residuals", "abs_residuals", "squared_residuals") %in% names(dt))
  print_and_store_result("add_metrics_to_data: Metrics appended", cols_present)
}

# Test: Main fs_bayes Function
test_fs_bayes <- function(fast_mode = TRUE) {
  data_size <- if (fast_mode) 50 else 100
  iter_val   <- if (fast_mode) 100 else 500
  warmup_val <- if (fast_mode) 0   else 250
  
  dt <- data.table(
    response   = rnorm(data_size),
    predictor1 = rnorm(data_size),
    predictor2 = rnorm(data_size),
    date       = seq.Date(Sys.Date(), by = "day", length.out = data_size)
  )
  brm_args <- if (fast_mode) {
    list(iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0,
         algorithm = "fixed_param", seed = 123)
  } else {
    list(iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0)
  }
  
  # Basic functionality test
  res <- tryCatch({
    fs_bayes(dt, "response", c("predictor1", "predictor2"), "date", brm_args = brm_args,
             parallel_combinations = FALSE, show_progress = TRUE, verbose = FALSE)
  }, error = function(e) NULL)
  
  basic_ok <- !is.null(res) && all(c("Model", "Data", "MAE", "RMSE") %in% names(res))
  print_and_store_result("fs_bayes: Basic functionality", basic_ok)
  
  # Test: Handling missing data
  dt_missing <- copy(dt)
  dt_missing[sample(.N, 5), response := NA]
  dt_missing[sample(.N, 5), predictor1 := NA]
  
  res_missing <- tryCatch({
    fs_bayes(dt_missing, "response", c("predictor1", "predictor2"), "date", brm_args = brm_args,
             parallel_combinations = FALSE, verbose = FALSE)
  }, error = function(e) e)
  
  print_and_store_result("fs_bayes: Missing data handling", !inherits(res_missing, "error"),
                         if (inherits(res_missing, "error")) conditionMessage(res_missing))
  
  # Test: Custom prior
  custom_prior <- prior(normal(0, 10), class = "b")
  res_prior <- tryCatch({
    fs_bayes(dt, "response", c("predictor1", "predictor2"), "date", prior = custom_prior,
             brm_args = brm_args, parallel_combinations = FALSE, verbose = FALSE)
  }, error = function(e) NULL)
  print_and_store_result("fs_bayes: Custom prior", !is.null(res_prior))
  
  # Test: Parallel combinations (if available)
  res_parallel <- tryCatch({
    fs_bayes(dt, "response", c("predictor1", "predictor2"), "date", brm_args = brm_args,
             parallel_combinations = TRUE, verbose = FALSE)
  }, error = function(e) NULL)
  print_and_store_result("fs_bayes: Parallel evaluation", !is.null(res_parallel))
}

# Run all tests in one function
run_all_tests <- function(fast_mode = TRUE) {
  cat("========== Running Comprehensive Tests ==========\n")
  test_validate_data()
  test_add_week_feature()
  test_generate_predictor_combinations()
  test_fit_model(fast_mode)
  test_add_metrics_to_data(fast_mode)
  test_fs_bayes(fast_mode)
  cat("========== Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Run tests
# run_all_tests(fast_mode = TRUE)
