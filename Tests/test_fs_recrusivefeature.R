###############################################################################
# Testing Infrastructure - Recursive Feature Elimination
###############################################################################

# Global object to store test results
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

#' Print and Store Test Result
#'
#' Helper function to print and record the outcome of a test.
#'
#' @param test_name A character string naming the test.
#' @param passed Logical. Whether the test passed.
#' @param message Optional additional information.
#'
#' @return None.
print_and_store_result <- function(test_name, passed, message = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-40s [%s]\n", test_name, result))
  if (!is.null(message)) {
    cat("  ", message, "\n")
  }
  assign("test_results", rbind(test_results, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)), envir = .GlobalEnv)
}

#' Test validate_control_params Function
#'
#' Tests the validate_control_params function with both valid and invalid inputs.
#'
#' @return None.
test_validate_control_params <- function() {
  valid_input <- tryCatch({
    validate_control_params(list(method = "cv", number = 5))
    TRUE
  }, error = function(e) FALSE)
  
  invalid_input_not_list <- tryCatch({
    validate_control_params("not_a_list")
    FALSE
  }, error = function(e) TRUE)
  
  invalid_input_missing_elements <- tryCatch({
    validate_control_params(list(method = "cv"))
    FALSE
  }, error = function(e) TRUE)
  
  invalid_method <- tryCatch({
    validate_control_params(list(method = "invalid_method", number = 5))
    FALSE
  }, error = function(e) TRUE)
  
  invalid_number <- tryCatch({
    validate_control_params(list(method = "cv", number = -1))
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("validate_control_params: Valid input", valid_input)
  print_and_store_result("validate_control_params: Not a list", invalid_input_not_list)
  print_and_store_result("validate_control_params: Missing elements", invalid_input_missing_elements)
  print_and_store_result("validate_control_params: Invalid method", invalid_method)
  print_and_store_result("validate_control_params: Invalid number", invalid_number)
}

#' Test handle_categorical_variables Function
#'
#' Tests the one-hot encoding functionality.
#'
#' @return None.
test_handle_categorical_variables <- function() {
  data <- data.frame(
    cat1 = factor(c("A", "B", "A")),
    cat2 = factor(c("C", "C", "D")),
    num = 1:3
  )
  result <- handle_categorical_variables(data)
  
  # Check that all columns are numeric and that new columns were added
  cat_converted <- all(sapply(result, is.numeric)) && (ncol(result) > ncol(data))
  print_and_store_result("handle_categorical_variables: One-hot encoding", cat_converted)
}

#' Test split_data Function
#'
#' Verifies that data is split into training and testing sets appropriately.
#'
#' @return None.
test_split_data <- function() {
  set.seed(123)
  data <- data.frame(
    response = rnorm(100),
    predictor = rnorm(100)
  )
  data_split <- split_data(data, 1, seed = 123)
  train_data <- data_split$trainData
  correct_split <- (nrow(train_data) == 80)
  print_and_store_result("split_data: Correct data split", correct_split)
}

#' Test perform_rfe Function
#'
#' Tests that RFE can run without errors on a basic dataset.
#'
#' @return None.
test_perform_rfe <- function() {
  set.seed(123)
  data <- data.frame(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  control_params <- list(method = "cv", number = 5)
  train_data <- split_data(data, 1, seed = 123)$trainData
  
  result <- tryCatch({
    # Use a dummy variable instead of "_" to avoid pipe placeholder issues
    dummy <- perform_rfe(train_data, 1, sizes = c(1, 2), control_params, feature_funcs = rfFuncs, parallel = FALSE, early_stop = FALSE)
    TRUE
  }, error = function(e) FALSE)
  
  print_and_store_result("perform_rfe: Basic functionality", result)
}

#' Test train_final_model Function
#'
#' Verifies that the final model is trained correctly.
#'
#' @return None.
test_train_final_model <- function() {
  set.seed(123)
  data <- data.frame(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  control_params <- list(method = "cv", number = 5)
  optimal_vars <- c("predictor1", "predictor2")
  
  final_model <- tryCatch({
    model <- train_final_model(data, optimal_vars, 1, control_params, model_method = "rf")
    !is.null(model) && inherits(model, "train")
  }, error = function(e) FALSE)
  
  print_and_store_result("train_final_model: Model training", final_model)
}

#' Test fs_recursivefeature Function
#'
#' Runs a full integration test of the fs_recursivefeature wrapper.
#'
#' @return None.
test_fs_recursivefeature <- function() {
  set.seed(123)
  data <- data.frame(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  
  result <- tryCatch({
    res <- fs_recursivefeature(data, response_var_index = 1, seed = 123,
                               control_params = list(method = "cv", number = 5),
                               sizes = c(1, 2), parallel = FALSE, return_final_model = TRUE,
                               model_method = "lm")
    !is.null(res$FinalModel) && inherits(res$FinalModel, "train")
  }, error = function(e) FALSE)
  
  print_and_store_result("fs_recursivefeature: Basic functionality", result)
}

#' Run All Tests
#'
#' Executes all unit tests and prints a summary of the results.
#'
#' @return None.
run_all_tests <- function() {
  # Reset the global test_results data frame
  assign("test_results", data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE), envir = .GlobalEnv)
  cat("Running Comprehensive UAT\n")
  cat("==================================\n")
  
  test_validate_control_params()
  test_handle_categorical_variables()
  test_split_data()
  test_perform_rfe()
  test_train_final_model()
  test_fs_recursivefeature()
  
  cat("==================================\n")
  cat("UAT completed\n\n")
  
  # Print summary
  cat("Test Summary:\n")
  cat("==================================\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Run tests
# run_all_tests()
