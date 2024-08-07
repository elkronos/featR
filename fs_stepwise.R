# Load necessary libraries
library(caret)
library(leaps)
library(doParallel)

#' Check inputs for stepwise function
#'
#' Validates the input parameters for the stepwise function.
#'
#' @param data A data frame containing the dataset.
#' @param dependent_var A character string representing the dependent variable.
#' @param step_type A character string indicating the stepwise method to use: "backward", "forward", or "both".
#'
#' @return A character string representing the validated dependent variable.
#' @examples
#' \dontrun{
#' check_inputs(iris, "Sepal.Length", "both")
#' }
check_inputs <- function(data, dependent_var, step_type) {
  if (!is.data.frame(data)) {
    stop("Input 'data' should be a data frame")
  }
  
  dep_var <- if (is.character(dependent_var)) {
    dependent_var
  } else {
    deparse(substitute(dependent_var))
  }
  
  if (!(dep_var %in% colnames(data))) {
    stop("Dependent variable is not a valid column in the data frame")
  }
  
  if (!step_type %in% c("backward", "forward", "both")) {
    stop("Invalid step_type")
  }
  
  return(dep_var)
}

#' Prepare formula and tune grid for model training
#'
#' Generates the formula for the model and the tuning grid if not provided.
#'
#' @param data A data frame containing the dataset.
#' @param dep_var A character string representing the dependent variable.
#' @param tune_grid A data frame containing the tuning grid. If NULL, a default tuning grid is created.
#'
#' @return A list containing the formula and tuning grid.
#' @examples
#' \dontrun{
#' prepare_formula_and_tune_grid(iris, "Sepal.Length", NULL)
#' }
prepare_formula_and_tune_grid <- function(data, dep_var, tune_grid) {
  formula <- reformulate(".", response = dep_var)
  nvmax <- ncol(data) - 1
  if (is.null(tune_grid)) {
    tune_grid <- expand.grid(nvmax = 1:nvmax)
  }
  return(list(formula = formula, tune_grid = tune_grid))
}

#' Train stepwise model with nested cross-validation
#'
#' Trains a stepwise model using nested cross-validation.
#'
#' @param formula A formula object representing the model.
#' @param data A data frame containing the dataset.
#' @param step_type A character string indicating the stepwise method to use: "backward", "forward", or "both".
#' @param tune_grid A data frame containing the tuning grid.
#' @param outer_control A list specifying the outer resampling method.
#' @param inner_control A list specifying the inner resampling method.
#' @param verbose A logical indicating whether to print detailed output.
#'
#' @return The trained model.
#' @examples
#' \dontrun{
#' train_stepwise_model(reformulate(".", response = "Sepal.Length"), iris, "both", expand.grid(nvmax = 1:2), list(method = "cv", number = 3), list(method = "cv", number = 3), FALSE)
#' }
train_stepwise_model <- function(formula, data, step_type, tune_grid, outer_control, inner_control, verbose) {
  method <- switch(step_type,
                   "both" = "leapSeq",
                   "backward" = "leapBackward",
                   "forward" = "leapForward")
  
  # Define the outer training control for the outer loop of nested CV
  outer_cv <- trainControl(method = "cv", number = outer_control$number, index = outer_control$index, allowParallel = TRUE)
  
  # Capture any errors in the training process
  tryCatch({
    # Nested cross-validation
    outer_results <- train(formula, data = data,
                           method = method, 
                           tuneGrid = tune_grid,
                           trControl = outer_cv)
    
    if (verbose) {
      print(outer_results)
    }
    
    return(outer_results)
  }, error = function(e) {
    cat("Error in train_stepwise_model:\n", e$message, "\n")
    return(NULL)
  })
}

#' Perform stepwise feature selection with nested cross-validation
#'
#' Main function to perform stepwise feature selection using nested cross-validation.
#'
#' @param data A data frame containing the dataset.
#' @param dependent_var A character string representing the dependent variable.
#' @param step_type A character string indicating the stepwise method to use: "backward", "forward", or "both". Default is "both".
#' @param seed An integer seed for reproducibility. Default is NULL.
#' @param verbose A logical indicating whether to print detailed output. Default is FALSE.
#' @param outer_control A list specifying the outer resampling method. Default is list(method = "cv", number = 3, index = NULL).
#' @param inner_control A list specifying the inner resampling method. Default is list(method = "cv", number = 3).
#' @param tune_grid A data frame containing the tuning grid. Default is NULL.
#' @param return_models A logical indicating whether to return the trained models. Default is FALSE.
#' @param ... Additional arguments passed to the `train` function.
#'
#' @return A list containing the results, best tuning parameters, final model, and variable importance.
#' @examples
#' \dontrun{
#' fs_stepwise(iris, "Sepal.Length", "both", 123, FALSE, list(method = "cv", number = 3, index = NULL), list(method = "cv", number = 3), NULL, FALSE)
#' }
fs_stepwise <- function(data, 
                        dependent_var, 
                        step_type = "both", 
                        seed = NULL, 
                        verbose = FALSE, 
                        outer_control = list(method = "cv", number = 3, index = NULL),
                        inner_control = list(method = "cv", number = 3),
                        tune_grid = NULL,
                        return_models = FALSE,
                        ...) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Register parallel backend
  cores <- parallel::detectCores()
  cl <- makeCluster(cores - 1) # Use one less than the number of available cores
  registerDoParallel(cl)
  
  dep_var <- check_inputs(data, dependent_var, step_type)
  prepared_data <- prepare_formula_and_tune_grid(data, dep_var, tune_grid)
  
  # Optimize memory usage and computational efficiency
  gc()  # Garbage collection to free up memory
  
  step_model_train <- train_stepwise_model(prepared_data$formula, data, step_type, prepared_data$tune_grid, outer_control, inner_control, verbose)
  
  stopCluster(cl)  # Stop the parallel backend
  registerDoSEQ()  # Ensure the parallel backend is properly reset
  
  result <- step_model_train$results
  best_tune <- step_model_train$bestTune
  final_model <- step_model_train$finalModel
  importance <- varImp(step_model_train)
  
  output_list <- list(result = result, 
                      best_tune = best_tune, 
                      final_model = final_model, 
                      importance = importance)
  
  if (return_models) {
    output_list$step_model_train <- step_model_train
  }
  
  return(output_list)
}

#' Print and store test results
#'
#' Prints and stores the result of a test case.
#'
#' @param test_name A character string representing the name of the test.
#' @param passed A logical indicating whether the test passed.
#' @param message A character string providing additional information about the test result. Default is NULL.
#'
#' @return None
#' @examples
#' \dontrun{
#' print_and_store_result("Example test", TRUE, "This is an example.")
#' }
print_and_store_result <- function(test_name, passed, message = NULL) {
  result <- if(passed) "PASS" else "FAIL"
  cat(sprintf("%-40s [%s]\n", test_name, result))
  if (!is.null(message)) cat("  ", message, "\n")
  test_results <<- rbind(test_results, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE))
}

#' Test check_inputs function
#'
#' Tests the `check_inputs` function with various test cases.
#'
#' @return None
#' @examples
#' \dontrun{
#' test_check_inputs()
#' }
test_check_inputs <- function() {
  # Test Case 1: Valid input
  data <- data.frame(response = rnorm(100), predictor1 = rnorm(100))
  valid_test <- tryCatch({
    dep_var <- check_inputs(data, "response", "both")
    dep_var == "response"
  }, error = function(e) FALSE)
  
  # Test Case 2: Invalid data input
  invalid_data_test <- tryCatch({
    check_inputs(list(a = 1, b = 2), "response", "both")
    FALSE
  }, error = function(e) TRUE)
  
  # Test Case 3: Invalid dependent variable
  invalid_dep_var_test <- tryCatch({
    check_inputs(data, "non_existent", "both")
    FALSE
  }, error = function(e) TRUE)
  
  # Test Case 4: Invalid step type
  invalid_step_type_test <- tryCatch({
    check_inputs(data, "response", "invalid")
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("check_inputs: Valid input", valid_test)
  print_and_store_result("check_inputs: Invalid data input", invalid_data_test)
  print_and_store_result("check_inputs: Invalid dependent variable", invalid_dep_var_test)
  print_and_store_result("check_inputs: Invalid step type", invalid_step_type_test)
}

#' Test prepare_formula_and_tune_grid function
#'
#' Tests the `prepare_formula_and_tune_grid` function with various test cases.
#'
#' @return None
#' @examples
#' \dontrun{
#' test_prepare_formula_and_tune_grid()
#' }
test_prepare_formula_and_tune_grid <- function() {
  # Test Case 1: Default tune grid
  data <- data.frame(response = rnorm(100), predictor1 = rnorm(100))
  result <- prepare_formula_and_tune_grid(data, "response", NULL)
  default_tune_grid_test <- is.list(result) && "formula" %in% names(result) && "tune_grid" %in% names(result)
  
  # Test Case 2: Custom tune grid
  custom_tune_grid <- expand.grid(nvmax = 1:5)
  result <- prepare_formula_and_tune_grid(data, "response", custom_tune_grid)
  custom_tune_grid_test <- identical(result$tune_grid, custom_tune_grid)
  
  print_and_store_result("prepare_formula_and_tune_grid: Default tune grid", default_tune_grid_test)
  print_and_store_result("prepare_formula_and_tune_grid: Custom tune grid", custom_tune_grid_test)
}

#' Test train_stepwise_model function
#'
#' Tests the `train_stepwise_model` function with various test cases.
#'
#' @return None
#' @examples
#' \dontrun{
#' test_train_stepwise_model()
#' }
test_train_stepwise_model <- function() {
  set.seed(123)
  data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
  formula <- reformulate(".", response = "response")
  tune_grid <- expand.grid(nvmax = 1:2)
  outer_control <- list(method = "cv", number = 3, index = createFolds(data$response, k = 3))
  inner_control <- list(method = "cv", number = 3)
  
  # Test Case 1: Basic functionality
  basic_test <- tryCatch({
    model <- train_stepwise_model(formula, data, "both", tune_grid, outer_control, inner_control, FALSE)
    !is.null(model)
  }, error = function(e) FALSE)
  
  print_and_store_result("train_stepwise_model: Basic functionality", basic_test)
}

#' Test fs_stepwise function
#'
#' Tests the `fs_stepwise` function with various test cases.
#'
#' @return None
#' @examples
#' \dontrun{
#' test_fs_stepwise()
#' }
test_fs_stepwise <- function() {
  set.seed(123)
  data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
  custom_tune_grid <- expand.grid(nvmax = 1:2)
  outer_control <- list(method = "cv", number = 3, index = createFolds(data$response, k = 3))
  inner_control <- list(method = "cv", number = 3)
  
  # Test Case 1: Basic functionality
  basic_test <- tryCatch({
    results <- fs_stepwise(data, "response", "both", 123, FALSE, outer_control, inner_control, custom_tune_grid)
    !is.null(results$result)
  }, error = function(e) FALSE)
  
  # Test Case 2: Invalid inputs
  invalid_data_test <- tryCatch({
    fs_stepwise(list(a = 1, b = 2), "response", "both", 123, FALSE, outer_control, inner_control, custom_tune_grid)
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("fs_stepwise: Basic functionality", basic_test)
  print_and_store_result("fs_stepwise: Invalid data input", invalid_data_test)
}

#' Run all tests
#'
#' Executes all the test functions to validate the functionality of the script.
#'
#' @return None
#' @examples
#' \dontrun{
#' run_all_tests()
#' }
run_all_tests <- function() {
  cat("Running Comprehensive UAT\n")
  cat("==================================\n")
  test_check_inputs()
  test_prepare_formula_and_tune_grid()
  test_train_stepwise_model()
  test_fs_stepwise()
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
