# Load packages
library(e1071)
library(caret)
library(dplyr)
library(parallel)
library(doParallel)

#' Validate Inputs for SVM Function
#'
#' This function validates the inputs for the main SVM function. It ensures that the data is a data frame, the target variable exists in the data, the task is either classification or regression, the train_ratio is between 0 and 1, and the number of folds is greater than 1.
#'
#' @param data A data frame containing the dataset.
#' @param target A string specifying the target variable.
#' @param task A string specifying the task, either "classification" or "regression".
#' @param train_ratio A numeric value indicating the proportion of data to be used for training. Must be between 0 and 1.
#' @param nfolds An integer specifying the number of folds for cross-validation. Must be greater than 1.
#'
#' @importFrom base is.data.frame
#' @importFrom base stop
#' @examples
#' \dontrun{
#' validate_inputs(iris, "Species", "classification", 0.7, 5)
#' }
validate_inputs <- function(data, target, task, train_ratio, nfolds) {
  if (!is.data.frame(data)) stop("Data must be a data frame.")
  if (!target %in% names(data)) stop("Target variable not found in the data.")
  if (!task %in% c("classification", "regression")) stop("Task must be 'classification' or 'regression'.")
  if (!is.numeric(train_ratio) || train_ratio <= 0 || train_ratio >= 1) stop("Train ratio must be between 0 and 1.")
  if (!is.numeric(nfolds) || nfolds <= 1) stop("Number of folds for cross-validation must be greater than 1.")
}

#' Split Data into Training and Testing Sets
#'
#' This function splits the dataset into training and testing sets based on the specified train_ratio.
#'
#' @param data A data frame containing the dataset.
#' @param target A string specifying the target variable.
#' @param train_ratio A numeric value indicating the proportion of data to be used for training. Must be between 0 and 1.
#' @param seed An optional integer for setting the random seed for reproducibility.
#'
#' @return A list containing the training and testing sets.
#' @importFrom caret createDataPartition
#' @importFrom base set.seed
#' @examples
#' \dontrun{
#' split_data(iris, "Species", 0.7, seed = 123)
#' }
split_data <- function(data, target, train_ratio, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  train_index <- createDataPartition(data[[target]], p = train_ratio, list = FALSE)
  list(train_set = data[train_index, ], test_set = data[-train_index, ])
}

#' Perform Feature Selection
#'
#' This function performs feature selection using recursive feature elimination (RFE) with random forest as the base method.
#'
#' @param train_set A data frame containing the training dataset.
#' @param target A string specifying the target variable.
#'
#' @return A vector of selected feature names.
#' @importFrom caret rfe rfeControl rfFuncs
#' @examples
#' \dontrun{
#' perform_feature_selection(iris, "Species")
#' }
perform_feature_selection <- function(train_set, target) {
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
  rfe_results <- rfe(train_set[, -which(names(train_set) == target)], train_set[[target]], sizes = c(1:5), rfeControl = control)
  predictors(rfe_results)
}

#' Handle Class Imbalance
#'
#' This function handles class imbalance by up-sampling the minority class in the training dataset.
#'
#' @param train_set A data frame containing the training dataset.
#' @param target A string specifying the target variable.
#'
#' @return A data frame with balanced classes.
#' @importFrom caret upSample
#' @examples
#' \dontrun{
#' handle_class_imbalance(iris, "Species")
#' }
handle_class_imbalance <- function(train_set, target) {
  upSample(x = train_set[, -which(names(train_set) == target)], y = train_set[[target]], yname = target)
}

#' Default Tuning Grid for Hyperparameter Tuning
#'
#' This function returns the default tuning grid for SVM hyperparameter tuning based on the specified task.
#'
#' @param task A string specifying the task, either "classification" or "regression".
#'
#' @return A data frame containing the tuning grid.
#' @importFrom base expand.grid
#' @examples
#' \dontrun{
#' default_tune_grid("classification")
#' default_tune_grid("regression")
#' }
default_tune_grid <- function(task) {
  if (task == "classification") {
    expand.grid(C = seq(0.1, 1, by = 0.1))
  } else {
    expand.grid(sigma = seq(0.01, 1, by = 0.1), C = seq(1, 10, by = 1))
  }
}

#' Calculate Performance Metrics
#'
#' This function calculates performance metrics for classification and regression tasks.
#'
#' @param predictions A vector of predicted values.
#' @param actuals A vector of actual values.
#' @param task A string specifying the task, either "classification" or "regression".
#'
#' @return A list containing performance metrics.
#' @importFrom caret confusionMatrix
#' @examples
#' \dontrun{
#' calculate_performance(predictions, actuals, "classification")
#' calculate_performance(predictions, actuals, "regression")
#' }
calculate_performance <- function(predictions, actuals, task) {
  if (task == "classification") {
    predictions <- factor(predictions, levels = levels(actuals))
    confusionMatrix(predictions, actuals)
  } else {
    rsq <- 1 - sum((predictions - actuals)^2) / sum((actuals - mean(actuals))^2)
    mse <- mean((predictions - actuals)^2)
    mae <- mean(abs(predictions - actuals))
    list(R_squared = rsq, MSE = mse, MAE = mae)
  }
}

#' Setup Parallel Processing
#'
#' This function sets up parallel processing by creating a cluster with one less than the number of available cores.
#'
#' @return A cluster object.
#' @importFrom parallel makeCluster detectCores
#' @importFrom doParallel registerDoParallel
#' @examples
#' \dontrun{
#' cl <- setup_parallel_processing()
#' }
setup_parallel_processing <- function() {
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  cl
}

#' Stop Parallel Processing
#'
#' This function stops parallel processing by stopping the cluster and registering the sequential backend.
#'
#' @param cl A cluster object.
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoSEQ
#' @examples
#' \dontrun{
#' stop_parallel_processing(cl)
#' }
stop_parallel_processing <- function(cl) {
  stopCluster(cl)
  registerDoSEQ()
}

#' Main SVM Function with Feature Selection and Class Imbalance Handling
#'
#' This function performs SVM classification or regression with optional feature selection and class imbalance handling.
#'
#' @param data A data frame containing the dataset.
#' @param target A string specifying the target variable.
#' @param task A string specifying the task, either "classification" or "regression".
#' @param train_ratio A numeric value indicating the proportion of data to be used for training. Must be between 0 and 1.
#' @param nfolds An integer specifying the number of folds for cross-validation. Must be greater than 1.
#' @param tune_grid An optional data frame containing the tuning grid for hyperparameter tuning.
#' @param seed An optional integer for setting the random seed for reproducibility.
#' @param feature_select A logical value indicating whether to perform feature selection. Defaults to FALSE.
#' @param class_imbalance A logical value indicating whether to handle class imbalance. Defaults to FALSE.
#'
#' @return A list containing the trained SVM model, the test set, predictions, and performance metrics.
#' @importFrom caret train trainControl
#' @importFrom base as.formula paste
#' @examples
#' \dontrun{
#' fs_svm(iris, "Species", "classification", train_ratio = 0.7, nfolds = 5, seed = 123, feature_select = TRUE, class_imbalance = TRUE)
#' }
fs_svm <- function(data, target, task, train_ratio = 0.7, nfolds = 5, tune_grid = NULL, seed = NULL, feature_select = FALSE, class_imbalance = FALSE) {
  # Validate inputs
  validate_inputs(data, target, task, train_ratio, nfolds)
  
  # Split data
  splits <- split_data(data, target, train_ratio, seed)
  train_set <- splits$train_set
  test_set <- splits$test_set
  
  # Feature selection
  if (feature_select) {
    selected_features <- perform_feature_selection(train_set, target)
    train_set <- train_set[, c(selected_features, target)]
    test_set <- test_set[, c(selected_features, target)]
  }
  
  # Handle class imbalance
  if (class_imbalance && task == "classification") {
    train_set <- handle_class_imbalance(train_set, target)
  }
  
  # Define method and pre-processing
  method <- if (task == "classification") "svmLinear" else "svmRadial"
  preProc <- if (task == "regression") c("center", "scale") else NULL
  
  # Default tuning grid if not provided
  if (is.null(tune_grid)) {
    tune_grid <- default_tune_grid(task)
  }
  
  # Train control with cross-validation
  trControl <- trainControl(method = "cv", number = nfolds)
  
  # Parallel processing setup
  cl <- setup_parallel_processing()
  
  # Train SVM model
  svm_fit <- train(as.formula(paste(target, "~ .")), data = train_set, method = method,
                   trControl = trControl, preProc = preProc, tuneGrid = tune_grid)
  
  # Stop parallel processing
  stop_parallel_processing(cl)
  
  # Predictions on test set
  predictions <- predict(svm_fit, newdata = test_set)
  
  # Calculate performance
  performance <- calculate_performance(predictions, test_set[[target]], task)
  
  # Return results
  list(model = svm_fit, test_set = test_set, predictions = predictions, performance = performance)
}

# Initialize results data frame
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

#' Print and Store Test Results
#'
#' This function prints and stores the results of a test.
#'
#' @param test_name A string specifying the name of the test.
#' @param passed A logical value indicating whether the test passed.
#' @param message An optional string with a message to print.
#' @examples
#' \dontrun{
#' print_and_store_result("Test Example", TRUE)
#' }
print_and_store_result <- function(test_name, passed, message = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-60s [%s]\n", test_name, result))
  if (!is.null(message)) cat("  ", message, "\n")
  test_results <<- rbind(test_results, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE))
}

#' Test validate_inputs Function
#'
#' This function tests the validate_inputs function.
#'
#' @examples
#' \dontrun{
#' test_validate_inputs()
#' }
test_validate_inputs <- function() {
  # Valid input
  valid_test <- tryCatch({
    validate_inputs(iris, "Species", "classification", 0.7, 5)
    TRUE
  }, error = function(e) FALSE)
  
  # Invalid data (not a data frame)
  invalid_data <- tryCatch({
    validate_inputs(matrix(1:10), "Species", "classification", 0.7, 5)
    FALSE
  }, error = function(e) TRUE)
  
  # Invalid target (not in data)
  invalid_target <- tryCatch({
    validate_inputs(iris, "NonExistent", "classification", 0.7, 5)
    FALSE
  }, error = function(e) TRUE)
  
  # Invalid task
  invalid_task <- tryCatch({
    validate_inputs(iris, "Species", "invalid_task", 0.7, 5)
    FALSE
  }, error = function(e) TRUE)
  
  # Invalid train_ratio
  invalid_train_ratio <- tryCatch({
    validate_inputs(iris, "Species", "classification", 1.2, 5)
    FALSE
  }, error = function(e) TRUE)
  
  # Invalid nfolds
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

#' Test split_data Function
#'
#' This function tests the split_data function.
#'
#' @examples
#' \dontrun{
#' test_split_data()
#' }
test_split_data <- function() {
  data <- iris
  target <- "Species"
  train_ratio <- 0.7
  
  splits <- split_data(data, target, train_ratio, seed = 123)
  train_set <- splits$train_set
  test_set <- splits$test_set
  
  correct_split <- nrow(train_set) == round(0.7 * nrow(data)) && nrow(test_set) == round(0.3 * nrow(data))
  print_and_store_result("split_data: Correct split", correct_split)
}

#' Test perform_feature_selection Function
#'
#' This function tests the perform_feature_selection function.
#'
#' @examples
#' \dontrun{
#' test_perform_feature_selection()
#' }
test_perform_feature_selection <- function() {
  data <- iris
  target <- "Species"
  
  selected_features <- perform_feature_selection(data, target)
  expected_features <- c("Petal.Length", "Petal.Width") # Based on typical feature importance in iris
  
  correct_features <- all(selected_features %in% expected_features)
  print_and_store_result("perform_feature_selection: Correct features", correct_features)
}

#' Test handle_class_imbalance Function
#'
#' This function tests the handle_class_imbalance function.
#'
#' @examples
#' \dontrun{
#' test_handle_class_imbalance()
#' }
test_handle_class_imbalance <- function() {
  data <- iris
  target <- "Species"
  
  balanced_data <- handle_class_imbalance(data, target)
  balanced_classes <- table(balanced_data[[target]])
  
  correct_balance <- all(balanced_classes == max(balanced_classes))
  print_and_store_result("handle_class_imbalance: Correct balance", correct_balance)
}

#' Test default_tune_grid Function
#'
#' This function tests the default_tune_grid function.
#'
#' @examples
#' \dontrun{
#' test_default_tune_grid()
#' }
test_default_tune_grid <- function() {
  classification_grid <- default_tune_grid("classification")
  regression_grid <- default_tune_grid("regression")
  
  correct_classification_grid <- all(names(classification_grid) == "C")
  correct_regression_grid <- all(names(regression_grid) %in% c("C", "sigma"))
  
  print_and_store_result("default_tune_grid: Classification grid", correct_classification_grid)
  print_and_store_result("default_tune_grid: Regression grid", correct_regression_grid)
}

#' Test calculate_performance Function
#'
#' This function tests the calculate_performance function.
#'
#' @examples
#' \dontrun{
#' test_calculate_performance()
#' }
test_calculate_performance <- function() {
  set.seed(123)
  actuals <- factor(sample(0:1, 100, replace = TRUE))
  predictions <- factor(sample(0:1, 100, replace = TRUE))
  
  classification_performance <- calculate_performance(predictions, actuals, "classification")
  correct_classification <- inherits(classification_performance, "confusionMatrix")
  
  actuals <- rnorm(100)
  predictions <- rnorm(100)
  
  regression_performance <- calculate_performance(predictions, actuals, "regression")
  correct_regression <- all(c("R_squared", "MSE", "MAE") %in% names(regression_performance))
  
  print_and_store_result("calculate_performance: Classification metrics", correct_classification)
  print_and_store_result("calculate_performance: Regression metrics", correct_regression)
}

#' Test Parallel Processing Functions
#'
#' This function tests the setup_parallel_processing and stop_parallel_processing functions.
#'
#' @examples
#' \dontrun{
#' test_parallel_processing()
#' }
test_parallel_processing <- function() {
  cl <- setup_parallel_processing()
  correct_setup <- inherits(cl, "cluster")
  
  stop_parallel_processing(cl)
  correct_stop <- tryCatch({
    # Check if the cluster is stopped by trying to use it
    parallel::parLapply(cl, 1:2, function(x) x)
    FALSE  # If the above line doesn't throw an error, the test should fail
  }, error = function(e) TRUE)  # If an error is thrown, it means the cluster is properly stopped
  
  print_and_store_result("setup_parallel_processing: Setup", correct_setup)
  print_and_store_result("stop_parallel_processing: Stop", correct_stop)
}

#' Test fs_svm Function
#'
#' This function tests the fs_svm function for both classification and regression tasks.
#'
#' @examples
#' \dontrun{
#' test_fs_svm()
#' }
test_fs_svm <- function() {
  # Classification example
  classification_output <- tryCatch({
    fs_svm(
      data = iris, 
      target = "Species", 
      task = "classification", 
      train_ratio = 0.7, 
      seed = 123, 
      feature_select = TRUE, 
      class_imbalance = TRUE
    )
  }, error = function(e) NULL)
  
  correct_classification_output <- !is.null(classification_output) && all(c("model", "test_set", "predictions", "performance") %in% names(classification_output))
  print_and_store_result("fs_svm: Classification output", correct_classification_output)
  
  # Regression example
  regression_output <- tryCatch({
    fs_svm(
      data = mtcars, 
      target = "mpg", 
      task = "regression", 
      train_ratio = 0.7, 
      seed = 123, 
      feature_select = TRUE
    )
  }, error = function(e) NULL)
  
  correct_regression_output <- !is.null(regression_output) && all(c("model", "test_set", "predictions", "performance") %in% names(regression_output))
  print_and_store_result("fs_svm: Regression output", correct_regression_output)
}

#' Run All Tests
#'
#' This function runs all tests and prints a summary of the results.
#'
#' @examples
#' \dontrun{
#' run_all_tests()
#' }
run_all_tests <- function() {
  cat("Running Comprehensive UAT\n")
  cat("==================================\n")
  test_validate_inputs()
  test_split_data()
  test_perform_feature_selection()
  test_handle_class_imbalance()
  test_default_tune_grid()
  test_calculate_performance()
  test_parallel_processing()
  test_fs_svm()
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
