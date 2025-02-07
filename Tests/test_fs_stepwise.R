###############################################################################
# Testing Infrastructure - Stepwise Regression
###############################################################################

# Global data frame to store test results
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

test_check_inputs <- function() {
  log_info("Running test_check_inputs.")
  
  # Test 1: Valid input
  data_valid <- data.frame(response = rnorm(100), predictor1 = rnorm(100))
  valid_result <- tryCatch({
    dep_var <- check_inputs(data_valid, "response", "both")
    identical(dep_var, "response")
  }, error = function(e) {
    log_error("Test check_inputs valid input failed: {e$message}")
    FALSE
  })
  print_and_store_result("check_inputs: Valid Input", valid_result)
  
  # Test 2: Data not a data.frame
  invalid_data <- tryCatch({
    check_inputs(list(a = 1, b = 2), "response", "both")
    FALSE
  }, error = function(e) {
    TRUE
  })
  print_and_store_result("check_inputs: Invalid Data", invalid_data)
  
  # Test 3: Dependent variable not found
  invalid_dep <- tryCatch({
    check_inputs(data_valid, "nonexistent", "both")
    FALSE
  }, error = function(e) {
    TRUE
  })
  print_and_store_result("check_inputs: Invalid Dependent Variable", invalid_dep)
  
  # Test 4: Invalid step_type
  invalid_step <- tryCatch({
    check_inputs(data_valid, "response", "invalid")
    FALSE
  }, error = function(e) {
    TRUE
  })
  print_and_store_result("check_inputs: Invalid step_type", invalid_step)
}

test_prepare_formula <- function() {
  log_info("Running test_prepare_formula.")
  
  data_test <- data.frame(response = rnorm(100), predictor1 = rnorm(100))
  formula_generated <- prepare_formula(data_test, "response")
  expected_formula <- reformulate(setdiff(names(data_test), "response"), response = "response")
  formula_test <- isTRUE(all.equal(formula_generated, expected_formula))
  print_and_store_result("prepare_formula: Correct Formula", formula_test)
}

test_train_stepwise_model <- function() {
  log_info("Running test_train_stepwise_model.")
  
  set.seed(123)
  data_test <- data.frame(
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    predictor3 = rnorm(100)
  )
  # Create response with known relationships
  data_test$response <- 2 * data_test$predictor1 + 3 * data_test$predictor2 + 0.5 * data_test$predictor3 + rnorm(100)
  formula_test <- reformulate(setdiff(names(data_test), "response"), response = "response")
  
  test_success <- tryCatch({
    model <- train_stepwise_model(formula_test, data_test, direction = "both", verbose = FALSE)
    !is.null(model)
  }, error = function(e) {
    FALSE
  })
  print_and_store_result("train_stepwise_model: Basic Functionality", test_success)
}

test_fs_stepwise <- function() {
  log_info("Running test_fs_stepwise.")
  
  set.seed(123)
  data_test <- data.frame(
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    predictor3 = rnorm(100)
  )
  data_test$response <- 2 * data_test$predictor1 + 3 * data_test$predictor2 + 0.5 * data_test$predictor3 + rnorm(100)
  
  # Test the fs_stepwise function with valid input
  test_success <- tryCatch({
    result <- fs_stepwise(data_test, "response", step_type = "both", seed = 123, verbose = FALSE, return_models = TRUE)
    !is.null(result$final_model)
  }, error = function(e) {
    FALSE
  })
  print_and_store_result("fs_stepwise: Basic Functionality", test_success)
  
  # Test with invalid data input
  invalid_test <- tryCatch({
    fs_stepwise(list(a = 1, b = 2), "response", step_type = "both", seed = 123, verbose = FALSE)
    FALSE
  }, error = function(e) {
    TRUE
  })
  print_and_store_result("fs_stepwise: Invalid Data", invalid_test)
}

# Run all tests and summarize
run_all_tests <- function() {
  log_info("==== Starting Comprehensive Tests ====")
  test_check_inputs()
  test_prepare_formula()
  test_train_stepwise_model()
  test_fs_stepwise()
  
  cat("\n===== Test Summary =====\n")
  print(table(test_results$Result))
  cat("\nDetailed Test Results:\n")
  print(test_results)
  
  log_info("Tests Summary: PASS = {sum(test_results$Result == 'PASS')}, FAIL = {sum(test_results$Result == 'FAIL')}")
  log_info("==== Testing Completed ====")
}

# Run tests
# run_all_tests()