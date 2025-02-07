###############################################################################
# Testing Infrastructure - SVM
###############################################################################

# Global data frame to store test results
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

#' Print and Store a Test Result
#'
#' @param test_name Name of the test.
#' @param passed Logical indicating if the test passed.
#' @param message Optional message.
#'
#' @return Invisibly returns TRUE.
print_and_store_result <- function(test_name, passed, message = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-60s [%s]\n", test_name, result))
  if (!is.null(message)) cat("  ", message, "\n")
  test_results <<- rbind(test_results,
                         data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE))
  invisible(TRUE)
}

# Test for validate_inputs
test_validate_inputs <- function() {
  valid_test <- tryCatch({
    validate_inputs(iris, "Species", "classification", 0.7, 5)
    TRUE
  }, error = function(e) FALSE)
  
  invalid_data <- tryCatch({
    validate_inputs(matrix(1:10), "Species", "classification", 0.7, 5)
    FALSE
  }, error = function(e) TRUE)
  
  invalid_target <- tryCatch({
    validate_inputs(iris, "NonExistent", "classification", 0.7, 5)
    FALSE
  }, error = function(e) TRUE)
  
  invalid_task <- tryCatch({
    validate_inputs(iris, "Species", "invalid_task", 0.7, 5)
    FALSE
  }, error = function(e) TRUE)
  
  invalid_train_ratio <- tryCatch({
    validate_inputs(iris, "Species", "classification", 1.2, 5)
    FALSE
  }, error = function(e) TRUE)
  
  invalid_nfolds <- tryCatch({
    validate_inputs(iris, "Species", "classification", 0.7, 1)
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("validate_inputs: Valid input", valid_test)
  print_and_store_result("validate_inputs: Invalid data", invalid_data)
  print_and_store_result("validate_inputs: Invalid target", invalid_target)
  print_and_store_result("validate_inputs: Invalid task", invalid_task)
  print_and_store_result("validate_inputs: Invalid train_ratio", invalid_train_ratio)
  print_and_store_result("validate_inputs: Invalid nfolds", invalid_nfolds)
}

# Test for split_data
test_split_data <- function() {
  splits <- split_data(iris, "Species", 0.7, seed = 123)
  train_set <- splits$train_set
  test_set <- splits$test_set
  correct_split <- (nrow(train_set) == round(0.7 * nrow(iris))) &&
    (nrow(test_set) == nrow(iris) - nrow(train_set))
  print_and_store_result("split_data: Correct split", correct_split)
}

# Test for perform_feature_selection
test_perform_feature_selection <- function() {
  selected_features <- perform_feature_selection(iris, "Species", seed = 123)
  # In iris, Petal.Length and Petal.Width are typically important
  expected_features <- c("Petal.Length", "Petal.Width")
  correct_features <- all(expected_features %in% selected_features)
  print_and_store_result("perform_feature_selection: Correct features", correct_features)
}

# Test for handle_class_imbalance
test_handle_class_imbalance <- function() {
  balanced_data <- handle_class_imbalance(iris, "Species")
  counts <- table(balanced_data$Species)
  correct_balance <- length(unique(counts)) == 1
  print_and_store_result("handle_class_imbalance: Correct balance", correct_balance)
}

# Test for default_tune_grid
test_default_tune_grid <- function() {
  grid_class <- default_tune_grid("classification", "linear")
  grid_reg <- default_tune_grid("regression", "radial")
  correct_class_grid <- all(names(grid_class) == "C")
  correct_reg_grid <- all(names(grid_reg) %in% c("C", "sigma"))
  print_and_store_result("default_tune_grid: Classification grid", correct_class_grid)
  print_and_store_result("default_tune_grid: Regression grid", correct_reg_grid)
}

# Test for calculate_performance
test_calculate_performance <- function() {
  set.seed(123)
  # Classification test
  actuals <- factor(sample(c("A", "B"), 100, replace = TRUE))
  predictions <- factor(sample(c("A", "B"), 100, replace = TRUE), levels = levels(actuals))
  class_perf <- calculate_performance(predictions, actuals, "classification")
  correct_class_perf <- inherits(class_perf, "confusionMatrix")
  
  # Regression test
  actuals_reg <- rnorm(100)
  predictions_reg <- rnorm(100)
  reg_perf <- calculate_performance(predictions_reg, actuals_reg, "regression")
  correct_reg_perf <- all(c("R_squared", "RMSE", "MAE") %in% names(reg_perf))
  
  print_and_store_result("calculate_performance: Classification metrics", correct_class_perf)
  print_and_store_result("calculate_performance: Regression metrics", correct_reg_perf)
}

# Test for parallel processing functions
test_parallel_processing <- function() {
  cl <- setup_parallel_processing()
  correct_setup <- inherits(cl, "cluster")
  stop_parallel_processing(cl)
  
  # Trying to use the cluster after stopping should error
  correct_stop <- tryCatch({
    parallel::parLapply(cl, 1:2, function(x) x)
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("setup_parallel_processing: Setup", correct_setup)
  print_and_store_result("stop_parallel_processing: Stop", correct_stop)
}

# Test for fs_svm
test_fs_svm <- function() {
  # Classification Example
  class_out <- tryCatch({
    fs_svm(
      data = iris,
      target = "Species",
      task = "classification",
      train_ratio = 0.7,
      nfolds = 5,
      seed = 123,
      feature_select = TRUE,
      class_imbalance = TRUE,
      kernel = "radial"
    )
  }, error = function(e) NULL)
  
  correct_class <- !is.null(class_out) &&
    all(c("model", "test_set", "predictions", "performance") %in% names(class_out))
  print_and_store_result("fs_svm: Classification output", correct_class)
  
  # Regression Example
  reg_out <- tryCatch({
    fs_svm(
      data = mtcars,
      target = "mpg",
      task = "regression",
      train_ratio = 0.7,
      nfolds = 5,
      seed = 123,
      feature_select = TRUE,
      kernel = "linear"
    )
  }, error = function(e) NULL)
  
  correct_reg <- !is.null(reg_out) &&
    all(c("model", "test_set", "predictions", "performance") %in% names(reg_out))
  print_and_store_result("fs_svm: Regression output", correct_reg)
}

#' Run All Tests
#'
#' Executes all test functions and prints a summary.
#'
#' @export
run_all_tests <- function() {
  cat("Running Comprehensive Unit and Acceptance Tests\n")
  cat("====================================================\n")
  test_validate_inputs()
  test_split_data()
  test_perform_feature_selection()
  test_handle_class_imbalance()
  test_default_tune_grid()
  test_calculate_performance()
  test_parallel_processing()
  test_fs_svm()
  cat("====================================================\n")
  cat("UAT Completed\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Run tests
# run_all_tests()
