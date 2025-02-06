# Load required packages
suppressPackageStartupMessages({
  library(e1071)
  library(caret)
  library(dplyr)
  library(parallel)
  library(doParallel)
})

###############################################################################
# Input Validation Function
###############################################################################

#' Validate Inputs for SVM Function
#'
#' Checks that:
#' - data is a data frame,
#' - target exists in data,
#' - task is either "classification" or "regression",
#' - train_ratio is between 0 and 1 (exclusive),
#' - nfolds is an integer greater than 1.
#'
#' @param data A data frame containing the dataset.
#' @param target A string specifying the target variable.
#' @param task A string specifying the task ("classification" or "regression").
#' @param train_ratio A numeric value between 0 and 1.
#' @param nfolds An integer greater than 1.
#'
#' @return Invisibly returns TRUE if inputs are valid.
#' @export
validate_inputs <- function(data, target, task, train_ratio, nfolds) {
  if (!is.data.frame(data)) stop("Data must be a data frame.")
  if (!(target %in% names(data))) stop("Target variable not found in the data.")
  if (!(task %in% c("classification", "regression"))) stop("Task must be either 'classification' or 'regression'.")
  if (!is.numeric(train_ratio) || train_ratio <= 0 || train_ratio >= 1) stop("train_ratio must be a numeric value between 0 and 1.")
  if (!is.numeric(nfolds) || nfolds <= 1) stop("nfolds must be an integer greater than 1.")
  invisible(TRUE)
}

###############################################################################
# Data Splitting Function
###############################################################################

#' Split Data into Training and Testing Sets
#'
#' Splits the dataset into training and testing sets based on the specified
#' train_ratio. An optional seed ensures reproducibility.
#'
#' @param data A data frame.
#' @param target The target variable name.
#' @param train_ratio Proportion of data used for training.
#' @param seed Optional integer for reproducibility.
#'
#' @return A list with elements "train_set" and "test_set".
#' @export
split_data <- function(data, target, train_ratio, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  train_index <- createDataPartition(data[[target]], p = train_ratio, list = FALSE)
  list(
    train_set = data[train_index, , drop = FALSE],
    test_set  = data[-train_index, , drop = FALSE]
  )
}

###############################################################################
# Feature Selection Function
###############################################################################

#' Perform Feature Selection via Recursive Feature Elimination (RFE)
#'
#' Uses random forest based RFE to select a subset of features. This function
#' can be computationally intensive; it is recommended to run with parallel processing
#' when dealing with large datasets.
#'
#' @param train_set The training data frame.
#' @param target The target variable name.
#' @param seed Optional integer for reproducibility.
#' @param rfe_folds Number of folds in the RFE cross-validation (default is 10).
#'
#' @return A character vector of selected feature names.
#' @export
perform_feature_selection <- function(train_set, target, seed = NULL, rfe_folds = 10) {
  if (!is.null(seed)) set.seed(seed)
  # Exclude the target variable
  predictors_data <- train_set[, setdiff(names(train_set), target), drop = FALSE]
  num_features <- ncol(predictors_data)
  sizes <- seq(1, num_features)
  
  control <- rfeControl(functions = rfFuncs, method = "cv", number = rfe_folds, verbose = FALSE)
  rfe_results <- rfe(
    x = predictors_data,
    y = train_set[[target]],
    sizes = sizes,
    rfeControl = control
  )
  predictors(rfe_results)
}

###############################################################################
# Class Imbalance Handling Function
###############################################################################

#' Handle Class Imbalance by Up-Sampling the Minority Class
#'
#' Uses caret's upSample function to balance classes in the training set.
#'
#' @param train_set The training data frame.
#' @param target The target variable name.
#'
#' @return A balanced training data frame.
#' @export
handle_class_imbalance <- function(train_set, target) {
  upSample(
    x = train_set[, setdiff(names(train_set), target), drop = FALSE],
    y = train_set[[target]],
    yname = target
  )
}

###############################################################################
# Default Tuning Grid Function
###############################################################################

#' Generate Default Tuning Grid for SVM
#'
#' Creates a default tuning grid based on the task and kernel type.
#'
#' @param task "classification" or "regression".
#' @param kernel Kernel type: "linear", "radial", "polynomial", or "sigmoid".
#'
#' @return A data frame containing the tuning grid.
#' @export
default_tune_grid <- function(task, kernel = "linear") {
  if (kernel == "linear") {
    grid <- expand.grid(C = seq(0.1, 1, by = 0.1))
  } else if (kernel == "radial") {
    grid <- expand.grid(sigma = 2^(-15:-5), C = 2^(5:15))
  } else if (kernel == "polynomial") {
    grid <- expand.grid(degree = 2:5, scale = c(0.1, 0.5, 1), C = seq(0.1, 1, by = 0.1))
  } else if (kernel == "sigmoid") {
    grid <- expand.grid(C = seq(0.1, 1, by = 0.1), scale = c(0.1, 0.5, 1))
  } else {
    stop("Invalid kernel specified. Choose from 'linear', 'radial', 'polynomial', or 'sigmoid'.")
  }
  grid
}

###############################################################################
# Performance Metrics Function
###############################################################################

#' Calculate Performance Metrics for SVM Predictions
#'
#' For classification, returns a confusion matrix. For regression, returns
#' R-squared, RMSE, and MAE.
#'
#' @param predictions Model predictions.
#' @param actuals Actual target values.
#' @param task "classification" or "regression".
#'
#' @return A list of performance metrics.
#' @export
calculate_performance <- function(predictions, actuals, task) {
  if (task == "classification") {
    # Ensure factor levels match for confusionMatrix
    predictions <- factor(predictions, levels = levels(actuals))
    confusionMatrix(predictions, actuals)
  } else {
    metrics <- postResample(pred = predictions, obs = actuals)
    list(R_squared = as.numeric(metrics["Rsquared"]),
         RMSE = as.numeric(metrics["RMSE"]),
         MAE = as.numeric(metrics["MAE"]))
  }
}

###############################################################################
# Parallel Processing Setup Functions
###############################################################################

#' Setup Parallel Processing
#'
#' Creates and registers a cluster with one less than the number of available cores.
#'
#' @return A cluster object.
#' @export
setup_parallel_processing <- function() {
  cores <- max(detectCores() - 1, 1)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  cl
}

#' Stop Parallel Processing
#'
#' Stops the cluster and resets to sequential processing.
#'
#' @param cl A cluster object.
#' @export
stop_parallel_processing <- function(cl) {
  stopCluster(cl)
  registerDoSEQ()
}

###############################################################################
# Main SVM Function: fs_svm
###############################################################################

#' fs_svm: SVM Modeling with Optional Feature Selection and Class Imbalance Handling
#'
#' Trains an SVM model (classification or regression) with options for feature selection,
#' class imbalance handling, and hyperparameter tuning using cross-validation. Parallel
#' processing is used during model training.
#'
#' @param data A data frame containing the dataset.
#' @param target A string specifying the target variable.
#' @param task A string specifying the task ("classification" or "regression").
#' @param train_ratio Proportion of data to be used for training (default is 0.7).
#' @param nfolds Number of folds for cross-validation (default is 5).
#' @param tune_grid Optional tuning grid data frame. If NULL, a default grid is generated.
#' @param seed Optional integer for reproducibility.
#' @param feature_select Logical indicating whether to perform feature selection (default FALSE).
#' @param class_imbalance Logical indicating whether to handle class imbalance (default FALSE).
#' @param kernel Kernel type ("linear", "radial", "polynomial", "sigmoid"). If NULL, a default is chosen:
#'        "linear" for classification and "radial" for regression.
#'
#' @return A list with components: model (trained SVM), test_set, predictions, and performance metrics.
#' @export
fs_svm <- function(data,
                   target,
                   task,
                   train_ratio = 0.7,
                   nfolds = 5,
                   tune_grid = NULL,
                   seed = NULL,
                   feature_select = FALSE,
                   class_imbalance = FALSE,
                   kernel = NULL) {
  
  # Validate inputs
  validate_inputs(data, target, task, train_ratio, nfolds)
  
  # Split the data
  splits <- split_data(data, target, train_ratio, seed)
  train_set <- splits$train_set
  test_set  <- splits$test_set
  
  # Optional Feature Selection
  if (feature_select) {
    selected_features <- perform_feature_selection(train_set, target, seed)
    cols <- c(selected_features, target)
    train_set <- train_set[, cols, drop = FALSE]
    test_set  <- test_set[, cols, drop = FALSE]
  }
  
  # Optional Class Imbalance Handling (only for classification)
  if (class_imbalance && task == "classification") {
    train_set <- handle_class_imbalance(train_set, target)
  }
  
  # Determine kernel and corresponding caret method
  if (is.null(kernel)) {
    kernel <- if (task == "classification") "linear" else "radial"
  }
  method <- switch(kernel,
                   "linear"     = "svmLinear",
                   "radial"     = "svmRadial",
                   "polynomial" = "svmPoly",
                   "sigmoid"    = "svmSigmoid",
                   stop("Invalid kernel specified."))
  
  # Preprocessing steps (for regression, centering/scaling is often recommended)
  preProc <- if (task == "regression") c("center", "scale") else NULL
  
  # Generate default tuning grid if not provided
  if (is.null(tune_grid)) {
    tune_grid <- default_tune_grid(task, kernel)
  }
  
  # Setup training control
  trControl <- trainControl(method = "cv", number = nfolds, allowParallel = TRUE)
  
  # Setup parallel processing
  cl <- setup_parallel_processing()
  if (!is.null(seed)) set.seed(seed)
  
  # Train the SVM model
  svm_fit <- train(as.formula(paste(target, "~ .")),
                   data = train_set,
                   method = method,
                   trControl = trControl,
                   preProc = preProc,
                   tuneGrid = tune_grid)
  
  # Stop parallel processing
  stop_parallel_processing(cl)
  
  # Make predictions on the test set
  predictions <- predict(svm_fit, newdata = test_set)
  
  # Compute performance metrics
  performance <- calculate_performance(predictions, test_set[[target]], task)
  
  # Return the results
  list(model = svm_fit,
       test_set = test_set,
       predictions = predictions,
       performance = performance)
}

###############################################################################
# Testing Infrastructure
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
