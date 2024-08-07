# Load packages
library(caret)
library(doParallel)

#' Validate Control Parameters
#'
#' Validates the structure and contents of the control parameters list.
#'
#' @param control_params A list containing control parameters. It must include 'method' and 'number' elements.
#'
#' @return None. This function throws an error if the validation fails.
#'
#' @examples
#' \dontrun{
#' validate_control_params(list(method = "cv", number = 5))
#' }
validate_control_params <- function(control_params) {
  if (!is.list(control_params)) stop("control_params should be a list.")
  if (!all(c("method", "number") %in% names(control_params))) {
    stop("control_params should contain 'method' and 'number' elements.")
  }
}

#' Handle Categorical Variables
#'
#' Converts categorical variables in the dataset to numeric values.
#'
#' @param data A data frame containing the dataset.
#'
#' @return A data frame with categorical variables converted to numeric.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(cat1 = as.factor(c("A", "B", "A")), cat2 = as.factor(c("C", "C", "D")), num = c(1, 2, 3))
#' handle_categorical_variables(data)
#' }
handle_categorical_variables <- function(data) {
  data <- data.frame(lapply(data, function(x) {
    if (is.factor(x)) as.numeric(as.factor(x)) else x
  }))
  return(data)
}

#' Split Data into Training and Testing Sets
#'
#' Splits the dataset into training and testing sets.
#'
#' @param data A data frame containing the dataset.
#' @param response_var_index An integer indicating the index of the response variable.
#' @param seed An integer used to set the random seed for reproducibility.
#'
#' @return A list with two elements: trainData and testData.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(response = rnorm(100), predictor = rnorm(100))
#' split_data(data, 1, 123)
#' }
split_data <- function(data, response_var_index, seed) {
  set.seed(seed)
  trainIndex <- createDataPartition(data[, response_var_index], p = .8, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  return(list(trainData = trainData, testData = testData))
}

#' Perform Recursive Feature Elimination (RFE)
#'
#' Performs Recursive Feature Elimination to select optimal features.
#'
#' @param trainData A data frame containing the training dataset.
#' @param response_var_index An integer indicating the index of the response variable.
#' @param sizes A numeric vector indicating the different subset sizes of features to evaluate.
#' @param control_params A list containing control parameters for the RFE process.
#' @param feature_funcs A list of functions to use for RFE, typically from the caret package.
#' @param parallel A logical value indicating whether to use parallel processing.
#' @param early_stop A logical value indicating whether to stop early based on resampling results.
#'
#' @return An object of class rfe containing the results of the RFE process.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
#' control_params <- list(method = "cv", number = 5)
#' trainData <- split_data(data, 1, 123)$trainData
#' perform_rfe(trainData, 1, c(1, 2), control_params, rfFuncs, FALSE, FALSE)
#' }
perform_rfe <- function(trainData, response_var_index, sizes, control_params, feature_funcs, parallel, early_stop) {
  ctrl <- rfeControl(functions = feature_funcs, method = control_params$method, number = control_params$number, 
                     verbose = TRUE, allowParallel = parallel)
  if (early_stop) {
    ctrl$returnResamp <- "final"
    ctrl$saveDetails <- TRUE
  }
  rfeProfile <- tryCatch({
    rfe(x = trainData[, -response_var_index], y = trainData[, response_var_index], 
        sizes = sizes, rfeControl = ctrl)
  }, error = function(e) {
    stop("Error during RFE: ", e$message)
  })
  return(rfeProfile)
}

#' Train the Final Model
#'
#' Trains the final model using the optimal set of features.
#'
#' @param data A data frame containing the dataset.
#' @param optimal_vars A character vector of optimal variable names.
#' @param response_var_index An integer indicating the index of the response variable.
#' @param control_params A list containing control parameters for model training.
#'
#' @return An object of class train containing the final model.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
#' control_params <- list(method = "cv", number = 5)
#' train_final_model(data, c("predictor1", "predictor2"), 1, control_params)
#' }
train_final_model <- function(data, optimal_vars, response_var_index, control_params) {
  final_model <- train(data[, optimal_vars], data[, response_var_index], method = "rf",
                       trControl = trainControl(method = control_params$method, number = control_params$number))
  return(final_model)
}

#' Recursive Feature Selection Wrapper
#'
#' A wrapper function for performing recursive feature selection and training the final model.
#'
#' @param data A data frame containing the dataset.
#' @param response_var_index An integer indicating the index of the response variable.
#' @param seed An integer used to set the random seed for reproducibility. Default is 123.
#' @param control_params A list containing control parameters for the RFE and model training processes. Default is list(method = "cv", number = 5).
#' @param sizes A numeric vector indicating the different subset sizes of features to evaluate. Default is NULL.
#' @param parallel A logical value indicating whether to use parallel processing. Default is FALSE.
#' @param feature_funcs A list of functions to use for RFE, typically from the caret package. Default is rfFuncs.
#' @param handle_categorical A logical value indicating whether to convert categorical variables to numeric. Default is FALSE.
#' @param return_final_model A logical value indicating whether to return the final trained model. Default is FALSE.
#' @param cross_validate_final_model A logical value indicating whether to cross-validate the final model. Default is TRUE.
#' @param early_stop A logical value indicating whether to stop early based on resampling results. Default is FALSE.
#'
#' @return A list containing the results of the recursive feature selection process and, optionally, the final model.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
#' fs_recursivefeature(data, 1, seed = 123, control_params = list(method = "cv", number = 5), sizes = c(1, 2), parallel = FALSE, return_final_model = TRUE)
#' }
fs_recursivefeature <- function(data, response_var_index, seed = 123, 
                                control_params = list(method = "cv", number = 5),
                                sizes = NULL, parallel = FALSE, feature_funcs = rfFuncs,
                                handle_categorical = FALSE, return_final_model = FALSE,
                                cross_validate_final_model = TRUE, early_stop = FALSE) {
  
  # Input validation
  response_var_index <- as.integer(response_var_index)  # Ensure it's an integer
  if (!is.integer(response_var_index) || response_var_index > ncol(data) || response_var_index < 1) {
    stop("Invalid response_var_index. It should be an integer between 1 and the number of columns in the dataset.")
  }
  validate_control_params(control_params)
  
  # Split the data
  data_split <- split_data(data, response_var_index, seed)
  trainData <- data_split$trainData
  testData <- data_split$testData
  
  # Handle categorical variables (after splitting)
  if (handle_categorical) {
    trainData <- handle_categorical_variables(trainData)
    testData <- handle_categorical_variables(testData)
  }
  
  # Default sizes if not provided
  if (is.null(sizes)) {
    sizes <- c(1:(ncol(trainData) - 1))
  }
  
  # Set up parallel processing if required
  if (parallel) {
    cl <- makeCluster(detectCores() - 1)
    registerDoParallel(cl)
    on.exit(stopCluster(cl))
  }
  
  # Perform RFE
  rfeProfile <- perform_rfe(trainData, response_var_index, sizes, control_params, feature_funcs, parallel, early_stop)
  
  # Extracting results
  optimal_num_vars <- rfeProfile$optSize
  optimal_vars <- rfeProfile$optVariables
  var_importance <- rfeProfile$variables
  
  final_model <- NULL
  if (return_final_model) {
    # Use the entire dataset for final model training
    final_model <- train_final_model(rbind(trainData, testData), optimal_vars, response_var_index, control_params)
  }
  
  # Return the results
  list(
    OptimalNumberOfVariables = optimal_num_vars,
    OptimalVariables = optimal_vars,
    VariableImportance = var_importance,
    ResamplingResults = rfeProfile$results,
    FinalModel = final_model
  )
}

#' Print and Store Test Result
#'
#' Helper function to print and store the results of a test.
#'
#' @param test_name A character string representing the name of the test.
#' @param passed A logical value indicating whether the test passed.
#' @param message An optional character string providing additional information about the test result.
#'
#' @return None. This function prints the test result and stores it in a global data frame.
#'
#' @examples
#' \dontrun{
#' print_and_store_result("Example Test", TRUE, "Test passed successfully.")
#' }
print_and_store_result <- function(test_name, passed, message = NULL) {
  result <- if(passed) "PASS" else "FAIL"
  cat(sprintf("%-40s [%s]\n", test_name, result))
  if (!is.null(message)) cat("  ", message, "\n")
  test_results <<- rbind(test_results, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE))
}

#' Test Validate Control Parameters Function
#'
#' Tests the validate_control_params function with valid and invalid inputs.
#'
#' @return None. This function prints and stores the results of the tests.
#'
#' @examples
#' \dontrun{
#' test_validate_control_params()
#' }
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
  
  print_and_store_result("validate_control_params: Valid input", valid_input)
  print_and_store_result("validate_control_params: Not a list", invalid_input_not_list)
  print_and_store_result("validate_control_params: Missing elements", invalid_input_missing_elements)
}

#' Test Handle Categorical Variables Function
#'
#' Tests the handle_categorical_variables function to ensure it converts categorical variables to numeric.
#'
#' @return None. This function prints and stores the results of the tests.
#'
#' @examples
#' \dontrun{
#' test_handle_categorical_variables()
#' }
test_handle_categorical_variables <- function() {
  data <- data.frame(
    cat1 = as.factor(c("A", "B", "A")),
    cat2 = as.factor(c("C", "C", "D")),
    num = c(1, 2, 3)
  )
  
  result <- handle_categorical_variables(data)
  
  cat_converted <- all(sapply(result, is.numeric))
  print_and_store_result("handle_categorical_variables: Categorical to numeric conversion", cat_converted)
}

#' Test Split Data Function
#'
#' Tests the split_data function to ensure it correctly splits the data into training and testing sets.
#'
#' @return None. This function prints and stores the results of the tests.
#'
#' @examples
#' \dontrun{
#' test_split_data()
#' }
test_split_data <- function() {
  set.seed(123)
  data <- data.frame(
    response = rnorm(100),
    predictor = rnorm(100)
  )
  
  data_split <- split_data(data, 1, 123)
  train_data <- data_split$trainData
  correct_split <- nrow(train_data) == 80
  print_and_store_result("split_data: Correct data split", correct_split)
}

#' Test Perform RFE Function
#'
#' Tests the perform_rfe function to ensure it performs recursive feature elimination correctly.
#'
#' @return None. This function prints and stores the results of the tests.
#'
#' @examples
#' \dontrun{
#' test_perform_rfe()
#' }
test_perform_rfe <- function() {
  set.seed(123)
  data <- data.frame(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  
  control_params <- list(method = "cv", number = 5)
  data_split <- split_data(data, 1, 123)
  train_data <- data_split$trainData
  
  result <- tryCatch({
    perform_rfe(train_data, 1, c(1, 2), control_params, rfFuncs, FALSE, FALSE)
    TRUE
  }, error = function(e) FALSE)
  
  print_and_store_result("perform_rfe: Basic functionality", result)
}

#' Test Train Final Model Function
#'
#' Tests the train_final_model function to ensure it trains the final model correctly.
#'
#' @return None. This function prints and stores the results of the tests.
#'
#' @examples
#' \dontrun{
#' test_train_final_model()
#' }
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
    model <- train_final_model(data, optimal_vars, 1, control_params)
    !is.null(model) && inherits(model, "train")
  }, error = function(e) FALSE)
  
  print_and_store_result("train_final_model: Model training", final_model)
}

#' Test Recursive Feature Selection Wrapper Function
#'
#' Tests the fs_recursivefeature function to ensure it performs the entire feature selection and model training process correctly.
#'
#' @return None. This function prints and stores the results of the tests.
#'
#' @examples
#' \dontrun{
#' test_fs_recursivefeature()
#' }
test_fs_recursivefeature <- function() {
  set.seed(123)
  data <- data.frame(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  
  result <- tryCatch({
    fs_recursivefeature(data, 1, seed = 123, control_params = list(method = "cv", number = 5), sizes = c(1, 2), parallel = FALSE, return_final_model = TRUE)
  }, error = function(e) list(error = TRUE, message = e$message))
  
  if (!is.null(result$error) && result$error) {
    print_and_store_result("fs_recursivefeature: Basic functionality", FALSE, result$message)
  } else {
    correct_result <- !is.null(result) && all(c("OptimalNumberOfVariables", "OptimalVariables", "VariableImportance", "ResamplingResults", "FinalModel") %in% names(result))
    print_and_store_result("fs_recursivefeature: Basic functionality", correct_result)
  }
}

#' Run All Tests
#'
#' Runs all unit tests and prints a summary of the results.
#'
#' @return None. This function prints the results of all the tests.
#'
#' @examples
#' \dontrun{
#' run_all_tests()
#' }
run_all_tests <- function() {
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

# Execute all tests
run_all_tests()
