###############################################################################
# Testing Infrastructure - Bayes
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
  # In fast mode, use fixed_param to avoid long sampling.
  iter_val   <- if (fast_mode) 100 else 500
  warmup_val <- if (fast_mode) 0   else 250
  
  dt <- data.table(
    response   = rnorm(data_size),
    predictor1 = rnorm(data_size),
    predictor2 = rnorm(data_size)
  )
  
  formula_str <- "response ~ predictor1"
  n_cores <- 1
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

#' Run All Tests
#'
#' Executes a series of tests to validate the functionality of the module.
#'
#' @param fast_mode Logical. If TRUE, uses settings for quick testing.
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

# Uncomment the following line to run all tests:
# run_all_tests(fast_mode = TRUE)
