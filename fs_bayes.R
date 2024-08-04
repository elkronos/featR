#' Load required packages
#'
#' @import brms
#' @import tidyverse
#' @import parallel
#' @import loo
#' @import data.table

# Load required packages
library(brms)
library(tidyverse)
library(parallel)
library(loo)
library(data.table)

#' Validate Data
#'
#' This function validates the presence of response, predictor, and date columns in the data.
#'
#' @param data A data frame or data table containing the data.
#' @param response_col A string specifying the response column.
#' @param predictor_cols A character vector specifying the predictor columns.
#' @param date_col A string specifying the date column (optional, default is NULL).
#' @return NULL if validation is successful, otherwise stops execution with an error message.
#' @examples
#' \dontrun{
#' validate_data(data, "response", c("predictor1", "predictor2"), "date")
#' }
validate_data <- function(data, response_col, predictor_cols, date_col = NULL) {
  if (!response_col %in% names(data)) {
    stop("Response column not found in data.")
  }
  if (any(!predictor_cols %in% names(data))) {
    stop("Some predictor columns not found in data.")
  }
  if (!is.null(date_col) && !date_col %in% names(data)) {
    stop("Date column not found in data.")
  }
}

#' Add Week Feature
#'
#' This function adds a week feature to the data based on the date column.
#'
#' @param data A data frame or data table containing the data.
#' @param date_col A string specifying the date column.
#' @param predictor_cols A character vector specifying the predictor columns.
#' @return A list containing the updated data and predictor columns.
#' @examples
#' \dontrun{
#' result <- add_week_feature(data, "date", c("predictor1", "predictor2"))
#' data <- result$data
#' predictor_cols <- result$predictor_cols
#' }
add_week_feature <- function(data, date_col, predictor_cols) {
  data[, week := as.integer(format(get(date_col), "%U"))]
  predictor_cols <- c(predictor_cols, "week")
  return(list(data = data, predictor_cols = predictor_cols))
}

#' Generate Predictor Combinations
#'
#' This function generates all possible combinations of predictor columns.
#'
#' @param predictor_cols A character vector specifying the predictor columns.
#' @return A list of character vectors, each representing a combination of predictors.
#' @examples
#' \dontrun{
#' combinations <- generate_predictor_combinations(c("predictor1", "predictor2", "predictor3"))
#' }
generate_predictor_combinations <- function(predictor_cols) {
  unlist(
    lapply(seq_along(predictor_cols),
           function(i) combn(predictor_cols, i, simplify = FALSE)),
    recursive = FALSE
  )
}

#' Fit Bayesian Model
#'
#' This function fits a Bayesian model using the provided formula and data.
#'
#' @param data A data frame or data table containing the data.
#' @param formula_str A string specifying the model formula.
#' @param n_cores An integer specifying the number of cores to use.
#' @param prior A prior distribution for the model parameters.
#' @param brm_args A list of additional arguments to pass to the brm function.
#' @param hyperparameter_grid A grid of hyperparameters for model tuning (optional).
#' @return A fitted model object.
#' @examples
#' \dontrun{
#' model <- fit_model(data, "response ~ predictor1", 1, prior(normal(0, 10), class = "b"), list(iter = 2000, warmup = 1000))
#' }
fit_model <- function(data, formula_str, n_cores, prior, brm_args, hyperparameter_grid = NULL) {
  cat("Fitting model with formula:", formula_str, "\n")
  
  model <- tryCatch({
    model_args <- modifyList(
      list(
        formula = as.formula(formula_str),
        data = data,
        family = gaussian(),
        prior = prior,
        cores = n_cores,
        iter = 4000,
        warmup = 2000,
        control = list(adapt_delta = 0.99, max_treedepth = 15)
      ),
      brm_args
    )
    cat("Model arguments:\n")
    print(model_args)
    do.call(brm, model_args)
  }, warning = function(w) {
    message("Warning for Model:", formula_str, ": ", conditionMessage(w))
    return(NULL)
  }, error = function(e) {
    message("Error for Model:", formula_str, ": ", conditionMessage(e))
    print(summary(data))
    return(NULL)
  })
  
  if (!is.null(model)) {
    cat("Model fitted successfully:", formula_str, "\n")
    print(summary(model))
  }
  
  return(model)
}

#' Add Fitted Values and Residuals to Data
#'
#' This function adds fitted values and residuals from the model to the data.
#'
#' @param data A data frame or data table containing the data.
#' @param model A fitted model object.
#' @return The updated data with fitted values and residuals.
#' @examples
#' \dontrun{
#' data <- add_metrics_to_data(data, model)
#' }
add_metrics_to_data <- function(data, model) {
  fitted_values <- fitted(model)[, "Estimate"]
  residuals <- resid(model)[, "Estimate"]
  
  data[, `:=`(fitted_values = fitted_values,
              residuals = residuals,
              abs_residuals = abs(residuals),
              squared_residuals = residuals^2)]
  
  return(data)
}

#' Bayesian Feature Selection for Model Optimization
#'
#' This function performs Bayesian feature selection for model optimization.
#'
#' @param data A data frame or data table containing the data.
#' @param response_col A string specifying the response column.
#' @param predictor_cols A character vector specifying the predictor columns.
#' @param date_col A string specifying the date column (optional, default is NULL).
#' @param prior A prior distribution for the model parameters (optional, default is NULL).
#' @param brm_args A list of additional arguments to pass to the brm function (optional, default is an empty list).
#' @param early_stop_threshold A numeric value specifying the threshold for early stopping (optional, default is 0.01).
#' @param hyperparameter_grid A grid of hyperparameters for model tuning (optional).
#' @return A list containing the best model, the updated data with metrics, MAE, and RMSE.
#' @examples
#' \dontrun{
#' result <- fs_bayes(data, "response", c("predictor1", "predictor2"), "date")
#' }
fs_bayes <- function(data, response_col, predictor_cols, date_col = NULL,
                     prior = NULL, brm_args = list(), early_stop_threshold = 0.01, hyperparameter_grid = NULL) {
  
  # Data validation
  validate_data(data, response_col, predictor_cols, date_col)
  
  # Convert to data.table for efficient handling
  data <- as.data.table(data)
  
  # Handle missing data
  data <- na.omit(data[, c(response_col, predictor_cols, date_col), with = FALSE])
  
  if (nrow(data) == 0) {
    stop("No complete cases in the data after removing missing values.")
  }
  
  # Add week feature if date column is provided
  if (!is.null(date_col)) {
    result <- add_week_feature(data, date_col, predictor_cols)
    data <- result$data
    predictor_cols <- result$predictor_cols
  }
  
  # Generate all possible combinations of predictor columns
  predictor_combinations <- generate_predictor_combinations(predictor_cols)
  
  best_model <- NULL
  best_loo <- Inf
  
  # Setup for processing (using only one core for debugging)
  n_cores <- 1
  
  for (predictor_comb in predictor_combinations) {
    formula_str <- paste0(response_col, " ~ ", paste(predictor_comb, collapse = " + "))
    model <- fit_model(data, formula_str, n_cores, prior, brm_args, hyperparameter_grid)
    
    if (!is.null(model)) {
      loo_val <- loo(model)$estimates["elpd_loo", "Estimate"]
      if (!is.na(loo_val) && loo_val < best_loo) {
        if (best_loo - loo_val > early_stop_threshold) {
          best_model <- model
          best_loo <- loo_val
          break
        }
        best_model <- model
        best_loo <- loo_val
      }
    } else {
      message("Model failed to fit. Formula: ", formula_str)
    }
  }
  
  if (is.null(best_model)) {
    stop("No valid model found. Check your data and model specifications.")
  }
  
  data <- add_metrics_to_data(data, best_model)
  
  cat("Best Model:", as.character(formula(best_model)), "\n")
  cat("Best LOO:", best_loo, "\n")
  
  mae <- mean(data$abs_residuals, na.rm = TRUE)
  rmse <- sqrt(mean(data$squared_residuals, na.rm = TRUE))
  
  cat("MAE:", mae, "\n")
  cat("RMSE:", rmse, "\n")
  
  return(list("Model" = best_model, "Data" = data, "MAE" = mae, "RMSE" = rmse))
}

#' Comprehensive UAT for all functions
#'
#' This script performs unit testing for all the functions in this module.
#'
#' @import brms
#' @import tidyverse
#' @import parallel
#' @import loo
#' @import data.table

# Initialize results data frame
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

# Helper function to print test results and store them
print_and_store_result <- function(test_name, passed, message = NULL) {
  result <- if(passed) "PASS" else "FAIL"
  cat(sprintf("%-40s [%s]\n", test_name, result))
  if (!is.null(message)) cat("  ", message, "\n")
  test_results <<- rbind(test_results, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE))
}

# Test validate_data function
test_validate_data <- function() {
  data <- data.table(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    date = seq.Date(Sys.Date(), by = "day", length.out = 100)
  )
  
  valid_test <- tryCatch({
    validate_data(data, "response", c("predictor1", "predictor2"), "date")
    TRUE
  }, error = function(e) FALSE)
  
  invalid_response <- tryCatch({
    validate_data(data, "non_existent", c("predictor1", "predictor2"), "date")
    FALSE
  }, error = function(e) TRUE)
  
  invalid_predictor <- tryCatch({
    validate_data(data, "response", c("predictor1", "non_existent"), "date")
    FALSE
  }, error = function(e) TRUE)
  
  invalid_date <- tryCatch({
    validate_data(data, "response", c("predictor1", "predictor2"), "non_existent")
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("validate_data: Valid input", valid_test)
  print_and_store_result("validate_data: Invalid response", invalid_response)
  print_and_store_result("validate_data: Invalid predictor", invalid_predictor)
  print_and_store_result("validate_data: Invalid date", invalid_date)
}

# Test add_week_feature function
test_add_week_feature <- function() {
  data <- data.table(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    date = seq.Date(Sys.Date(), by = "day", length.out = 100)
  )
  
  result <- add_week_feature(data, "date", c("predictor1", "predictor2"))
  
  week_added <- "week" %in% names(result$data) && "week" %in% result$predictor_cols
  print_and_store_result("add_week_feature: Week feature added", week_added)
}

# Test generate_predictor_combinations function
test_generate_predictor_combinations <- function() {
  predictor_cols <- c("predictor1", "predictor2", "predictor3")
  combinations <- generate_predictor_combinations(predictor_cols)
  
  correct_combinations <- length(combinations) == 7  # 2^3 - 1
  print_and_store_result("generate_predictor_combinations: Correct combinations", correct_combinations)
}

# Test fit_model function
test_fit_model <- function() {
  set.seed(123)
  data <- data.table(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  
  formula_str <- "response ~ predictor1"
  n_cores <- 1
  prior <- NULL
  brm_args <- list(iter = 2000, warmup = 1000)
  
  model <- fit_model(data, formula_str, n_cores, prior, brm_args)
  
  model_fitted <- !is.null(model) && inherits(model, "brmsfit")
  print_and_store_result("fit_model: Model fitted successfully", model_fitted)
}

# Test add_metrics_to_data function
test_add_metrics_to_data <- function() {
  set.seed(123)
  data <- data.table(
    response = rnorm(100),
    predictor1 = rnorm(100)
  )
  
  model <- brm(response ~ predictor1, data = data, iter = 2000, warmup = 1000)
  
  result <- add_metrics_to_data(data, model)
  
  metrics_added <- all(c("fitted_values", "residuals", "abs_residuals", "squared_residuals") %in% names(result))
  print_and_store_result("add_metrics_to_data: Metrics added successfully", metrics_added)
}

# Test fs_bayes function
test_fs_bayes <- function() {
  # Test Case 1: Basic functionality
  set.seed(123)
  data <- data.table(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    date = seq.Date(Sys.Date(), by = "day", length.out = 100)
  )
  
  result <- tryCatch({
    fs_bayes(data, "response", c("predictor1", "predictor2"), "date")
  }, error = function(e) NULL)
  
  print_and_store_result("fs_bayes: Basic functionality", 
                         !is.null(result) && all(c("Model", "Data", "MAE", "RMSE") %in% names(result)))
  
  # Test Case 2: Handle missing data
  set.seed(123)
  data_missing <- data.table(
    response = c(rnorm(95), rep(NA, 5)),
    predictor1 = c(rnorm(90), rep(NA, 10)),
    predictor2 = rnorm(100),
    date = seq.Date(Sys.Date(), by = "day", length.out = 100)
  )
  
  result_missing <- tryCatch({
    fs_bayes(data_missing, "response", c("predictor1", "predictor2"), "date")
  }, error = function(e) e)
  
  print_and_store_result("fs_bayes: Handle missing data", 
                         !inherits(result_missing, "error"),
                         if(inherits(result_missing, "error")) conditionMessage(result_missing))
  
  # Test Case 3: Different prior specifications
  prior <- prior(normal(0, 10), class = "b")
  
  result_prior <- tryCatch({
    fs_bayes(data, "response", c("predictor1", "predictor2"), "date", prior = prior)
  }, error = function(e) NULL)
  
  print_and_store_result("fs_bayes: Custom prior", !is.null(result_prior))
  
  # Test Case 4: Different brm_args
  brm_args <- list(iter = 3000, warmup = 1500, chains = 2)
  
  result_brm_args <- tryCatch({
    fs_bayes(data, "response", c("predictor1", "predictor2"), "date", brm_args = brm_args)
  }, error = function(e) NULL)
  
  print_and_store_result("fs_bayes: Custom brm_args", 
                         !is.null(result_brm_args) && result_brm_args$Model$fit@sim$iter == 3000)
  
  # Test Case 5: Early stopping
  data_early_stop <- data.table(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    predictor3 = rnorm(100),
    date = seq.Date(Sys.Date(), by = "day", length.out = 100)
  )
  
  result_early_stop <- tryCatch({
    fs_bayes(data_early_stop, "response", c("predictor1", "predictor2", "predictor3"), "date", early_stop_threshold = 1)
  }, error = function(e) NULL)
  
  print_and_store_result("fs_bayes: Early stopping", !is.null(result_early_stop))
  
  # Test Case 6: Large dataset
  set.seed(123)
  data_large <- data.table(
    response = rnorm(1000),
    predictor1 = rnorm(1000),
    predictor2 = rnorm(1000),
    predictor3 = rnorm(1000),
    predictor4 = rnorm(1000),
    predictor5 = rnorm(1000),
    date = seq.Date(Sys.Date(), by = "day", length.out = 1000)
  )
  
  start_time <- Sys.time()
  result_large <- tryCatch({
    fs_bayes(data_large, "response", c("predictor1", "predictor2", "predictor3", "predictor4", "predictor5"), "date")
  }, error = function(e) NULL)
  end_time <- Sys.time()
  
  execution_time <- difftime(end_time, start_time, units = "mins")
  print_and_store_result("fs_bayes: Large dataset", 
                         !is.null(result_large),
                         sprintf("Execution time: %.2f minutes", as.numeric(execution_time)))
  
  # Test Case 7: Invalid inputs
  result_invalid_response <- tryCatch({
    fs_bayes(data, "non_existent", c("predictor1", "predictor2"), "date")
  }, error = function(e) e)
  
  result_invalid_predictor <- tryCatch({
    fs_bayes(data, "response", c("predictor1", "non_existent"), "date")
  }, error = function(e) e)
  
  print_and_store_result("fs_bayes: Invalid response column", inherits(result_invalid_response, "error"))
  print_and_store_result("fs_bayes: Invalid predictor column", inherits(result_invalid_predictor, "error"))
}

# Run all tests
run_all_tests <- function() {
  cat("Running Comprehensive UAT\n")
  cat("==================================\n")
  test_validate_data()
  test_add_week_feature()
  test_generate_predictor_combinations()
  test_fit_model()
  test_add_metrics_to_data()
  test_fs_bayes()
  cat("==================================\n")
  cat("UAT completed\n\n")
  
  # Print summary
  cat("Test Summary:\n")
  cat("==================================\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Execute all tests
run_all_tests()
