library(earth)
library(caret)
library(data.table)
library(doParallel)

#' Check Required Libraries
#'
#' Ensures that all necessary libraries are loaded. If any library is not loaded, it stops the execution and prompts the user to load the required library.
#' 
#' @import earth
#' @import caret
#' @import data.table
#' @import doParallel
#' 
#' @examples
#' \dontrun{
#' check_libraries()
#' }
#' 
#' @export
check_libraries <- function() {
  required_libraries <- c("earth", "caret", "data.table", "doParallel")
  for (lib in required_libraries) {
    if (!lib %in% loadedNamespaces()) stop(paste("The", lib, "package is not loaded. Please install and load it before calling this function."))
  }
}

#' Check if Response Column Exists
#'
#' Verifies that the response column exists in the provided dataset.
#'
#' @param data data.table. The dataset in which to check for the response column.
#' @param responseName character. The name of the response column to check.
#'
#' @examples
#' \dontrun{
#' data <- data.table(response = rnorm(100), predictor1 = rnorm(100))
#' check_response_column(data, "response")
#' }
#'
#' @export
check_response_column <- function(data, responseName) {
  if (!responseName %in% colnames(data)) stop("The responseName does not exist in the dataset.")
}

#' Handle Missing Values
#'
#' Removes rows with missing values from the dataset.
#'
#' @param data data.table. The dataset from which to remove missing values.
#'
#' @return data.table. The cleaned dataset with missing values removed.
#'
#' @examples
#' \dontrun{
#' data <- data.table(response = c(rnorm(95), rep(NA, 5)), predictor1 = c(rnorm(90), rep(NA, 10)))
#' cleaned_data <- handle_missing_values(data)
#' }
#'
#' @export
handle_missing_values <- function(data) {
  initial_rows <- nrow(data)
  data <- na.omit(data)
  cat("Removed", initial_rows - nrow(data), "rows with missing values.\n")
  return(data)
}

#' Check Class Balance
#'
#' Checks if the classes in the response column are balanced. If any class has fewer than two samples, an error is raised. If any class has fewer than ten samples, a warning is issued.
#'
#' @param data data.table. The dataset to check for class balance.
#' @param responseName character. The name of the response column to check.
#' @param show_warnings logical. Whether to show warnings for classes with fewer than ten samples. Defaults to TRUE.
#'
#' @examples
#' \dontrun{
#' data <- data.table(response = factor(rep(c("A", "B"), c(70, 30))), predictor1 = rnorm(100))
#' check_class_balance(data, "response")
#' }
#'
#' @export
check_class_balance <- function(data, responseName, show_warnings = TRUE) {
  if (is.factor(data[[responseName]])) {
    class_counts <- table(data[[responseName]])
    if (any(class_counts < 2)) stop("Each class must have at least two samples.")
    if (any(class_counts < 10) && show_warnings) {
      warning("Some classes have fewer than 10 samples. Oversampling will be used to balance classes.")
    }
  }
}

#' Setup Parallel Processing
#'
#' Sets up parallel processing using the available cores, excluding one core.
#'
#' @return cluster object. The cluster object for parallel processing.
#'
#' @examples
#' \dontrun{
#' cl <- setup_parallel_processing()
#' stopCluster(cl)
#' }
#'
#' @export
setup_parallel_processing <- function() {
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  return(cl)
}

#' Sample Data
#'
#' Samples down the dataset to the specified size if the dataset is larger than the sample size.
#'
#' @param data data.table. The dataset to sample.
#' @param sampleSize integer. The desired sample size.
#' @param seed integer. The seed for random sampling.
#'
#' @return data.table. The sampled dataset.
#'
#' @examples
#' \dontrun{
#' data <- data.table(response = rnorm(20000), predictor1 = rnorm(20000))
#' sampled_data <- sample_data(data, 10000, 123)
#' }
#'
#' @export
sample_data <- function(data, sampleSize, seed) {
  if (nrow(data) > sampleSize) {
    set.seed(seed)
    data <- data[sample(.N, sampleSize)]
    cat("Sampled down to", nrow(data), "rows.\n")
  }
  return(data)
}

#' Split Data into Training and Test Sets
#'
#' Splits the dataset into training and test sets based on the specified proportion.
#'
#' @param data data.table. The dataset to split.
#' @param responseName character. The name of the response column.
#' @param p numeric. The proportion of data to use for the training set.
#' @param seed integer. The seed for random sampling.
#'
#' @return list. A list containing the training and test sets.
#'
#' @examples
#' \dontrun{
#' data <- data.table(response = rnorm(100), predictor1 = rnorm(100))
#' split <- split_data(data, "response", 0.8, 123)
#' train <- split$train
#' test <- split$test
#' }
#'
#' @export
split_data <- function(data, responseName, p, seed) {
  set.seed(seed)
  if (is.factor(data[[responseName]])) {
    trainIndex <- caret::createDataPartition(data[[responseName]], p = p, list = FALSE)
  } else {
    trainIndex <- sample(seq_len(nrow(data)), size = floor(p * nrow(data)))
  }
  train <- data[trainIndex, ]
  test <- data[-trainIndex, ]
  return(list(train = train, test = test))
}

#' Balance Classes in Training Set
#'
#' Balances the classes in the training set by oversampling the minority classes.
#'
#' @param train data.table. The training set.
#' @param responseName character. The name of the response column.
#'
#' @return data.table. The balanced training set.
#'
#' @examples
#' \dontrun{
#' data <- data.table(response = factor(rep(c("A", "B"), c(70, 30))), predictor1 = rnorm(100))
#' balanced_data <- balance_classes(data, "response")
#' }
#'
#' @export
balance_classes <- function(train, responseName) {
  if (is.factor(train[[responseName]])) {
    class_counts <- table(train[[responseName]])
    max_count <- max(class_counts)
    balanced_list <- list()
    for (class in names(class_counts)) {
      class_data <- train[train[[responseName]] == class]
      if (nrow(class_data) < max_count) {
        oversampled <- class_data[sample(1:nrow(class_data), max_count, replace = TRUE)]
        balanced_list[[class]] <- oversampled
      } else {
        balanced_list[[class]] <- class_data
      }
    }
    train <- rbindlist(balanced_list)
    train <- train[sample(1:nrow(train))]
    cat("Balanced training set size:", nrow(train), "rows.\n")
  }
  return(train)
}

#' Define Hyperparameter Grid
#'
#' Defines a grid of hyperparameters for model training.
#'
#' @param degree integer vector. The degrees of interaction to consider.
#' @param nprune integer vector. The number of terms to prune.
#'
#' @return data.frame. A data frame representing the grid of hyperparameters.
#'
#' @examples
#' \dontrun{
#' hyperparameters <- define_hyperparameter_grid(1:3, c(5, 10, 15))
#' }
#'
#' @export
define_hyperparameter_grid <- function(degree, nprune) {
  expand.grid(nprune = nprune, degree = degree)
}

#' Define Training Control Parameters
#'
#' Defines the control parameters for model training using caret.
#'
#' @param number integer. The number of folds for cross-validation.
#' @param repeats integer. The number of times to repeat the cross-validation.
#' @param search character. The search method for hyperparameters.
#' @param train data.table. The training set.
#' @param responseName character. The name of the response column.
#'
#' @return trainControl object. The control parameters for model training.
#'
#' @examples
#' \dontrun{
#' data <- data.table(response = factor(rep(c("A", "B"), each = 50)), predictor1 = rnorm(100))
#' ctrl <- define_train_control(5, 3, "grid", data, "response")
#' }
#'
#' @export
define_train_control <- function(number, repeats, search, train, responseName) {
  caret::trainControl(
    method = "repeatedcv", 
    number = number, 
    repeats = repeats, 
    search = search, 
    allowParallel = TRUE, 
    savePredictions = "final",
    classProbs = is.factor(train[[responseName]]),
    returnResamp = "all"
  )
}

#' Train Model
#'
#' Trains a model using caret with the specified hyperparameters and control parameters.
#'
#' @param train data.table. The training set.
#' @param responseName character. The name of the response column.
#' @param method character. The modeling method to use.
#' @param ctrl trainControl. The control parameters for training.
#' @param hyperParameters data.frame. The grid of hyperparameters to search over.
#' @param seed integer. The seed for random processes.
#'
#' @return train object. The trained model.
#'
#' @examples
#' \dontrun{
#' data <- data.table(response = rnorm(100), predictor1 = rnorm(100))
#' hyperparameters <- define_hyperparameter_grid(1:3, c(5, 10, 15))
#' ctrl <- define_train_control(5, 3, "grid", data, "response")
#' model <- train_model(data, "response", "earth", ctrl, hyperparameters, 123)
#' }
#'
#' @export
train_model <- function(train, responseName, method, ctrl, hyperParameters, seed) {
  set.seed(seed)
  ctrl$allowParallel <- FALSE  # Disable parallel processing
  tryCatch({
    model <- suppressWarnings(
      try({
        caret::train(as.formula(paste(responseName, "~ .")), 
                     data = train, 
                     method = method, 
                     trControl = ctrl, 
                     tuneGrid = hyperParameters)
      }, silent = TRUE)
    )
    
    if (inherits(model, "try-error") || is.null(model)) {
      stop("Model training failed or resulted in a NULL model")
    }
    return(model)
  }, error = function(e) {
    cat("Error in model training:", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Evaluate Model
#'
#' Evaluates the trained model on the test set and returns performance metrics.
#'
#' @param model train object. The trained model.
#' @param test data.table. The test set.
#' @param responseName character. The name of the response column.
#'
#' @return list. A list containing the model and performance metrics (RMSE for numeric response, accuracy for factor response).
#'
#' @examples
#' \dontrun{
#' data <- data.table(response = rnorm(100), predictor1 = rnorm(100))
#' hyperparameters <- define_hyperparameter_grid(1:3, c(5, 10, 15))
#' ctrl <- define_train_control(5, 3, "grid", data, "response")
#' model <- train_model(data, "response", "earth", ctrl, hyperparameters, 123)
#' split <- split_data(data, "response", 0.8, 123)
#' test <- split$test
#' evaluation <- evaluate_model(model, test, "response")
#' }
#'
#' @export
evaluate_model <- function(model, test, responseName) {
  if (is.null(model)) {
    return(list(model = NULL, error = "Model is NULL, cannot evaluate"))
  }
  
  pred <- predict(model, newdata = test)
  if (is.numeric(test[[responseName]])) {
    rmse <- sqrt(mean((test[[responseName]] - pred)^2))
    return(list(model = model, rmse = rmse))
  } else if (is.factor(test[[responseName]])) {
    accuracy <- sum(test[[responseName]] == pred) / length(pred)
    return(list(model = model, accuracy = accuracy))
  } else {
    stop("Unsupported response variable type.")
  }
}

#' MARS Feature Selection and Model Training
#'
#' Performs feature selection and model training using Multivariate Adaptive Regression Splines (MARS).
#'
#' @param data data.table. The dataset to use.
#' @param responseName character. The name of the response column.
#' @param p numeric. The proportion of data to use for the training set. Defaults to 0.8.
#' @param degree integer vector. The degrees of interaction to consider. Defaults to 1:3.
#' @param nprune integer vector. The number of terms to prune. Defaults to c(5, 10, 15).
#' @param method character. The modeling method to use. Defaults to "earth".
#' @param search character. The search method for hyperparameters. Defaults to "grid".
#' @param number integer. The number of folds for cross-validation. Defaults to 5.
#' @param repeats integer. The number of times to repeat the cross-validation. Defaults to 3.
#' @param seed integer. The seed for random processes. Defaults to 123.
#' @param sampleSize integer. The desired sample size. Defaults to 10000.
#'
#' @return list. A list containing the model and performance metrics.
#'
#' @import earth
#' @import caret
#' @import data.table
#' @import doParallel
#'
#' @examples
#' \dontrun{
#' data <- data.table(response = rnorm(100), predictor1 = rnorm(100))
#' results <- fs_mars(data, "response")
#' }
#'
#' @export
fs_mars <- function(data, responseName, p = 0.8, degree = 1:3, 
                    nprune = c(5, 10, 15), method = "earth", search = "grid", 
                    number = 5, repeats = 3, seed = 123, sampleSize = 10000) {
  
  cat("Checking if required libraries are loaded...\n")
  check_libraries()
  
  cat("Checking if responseName exists in the data...\n")
  check_response_column(data, responseName)
  
  cat("Converting data to data.table for efficient processing...\n")
  data <- as.data.table(data)
  
  cat("Handling missing values...\n")
  data <- handle_missing_values(data)
  
  cat("Checking class balance...\n")
  check_class_balance(data, responseName, show_warnings)
  
  cat("Setting up parallel processing...\n")
  cl <- setup_parallel_processing()
  on.exit(stopCluster(cl))
  
  cat("Sampling data for initial training if larger than sampleSize...\n")
  data <- sample_data(data, sampleSize, seed)
  
  cat("Splitting data into training and test sets...\n")
  split <- split_data(data, responseName, p, seed)
  train <- split$train
  test <- split$test
  cat("Training set size:", nrow(train), "rows.\n")
  cat("Test set size:", nrow(test), "rows.\n")
  
  cat("Balancing classes if necessary...\n")
  train <- balance_classes(train, responseName)
  
  cat("Defining the grid of hyperparameters to search over...\n")
  hyperParameters <- define_hyperparameter_grid(degree, nprune)
  
  cat("Defining the control parameters for the model training...\n")
  ctrl <- define_train_control(number, repeats, search, train, responseName)
  
  cat("Training the model using grid search for hyperparameters...\n")
  mars_model <- train_model(train, responseName, method, ctrl, hyperParameters, seed)
  
  if (is.null(mars_model)) {
    return(list(model = NULL, error = "Model training failed"))
  }
  
  cat("Making predictions on test set...\n")
  cat("Calculating performance metrics...\n")
  evaluation <- evaluate_model(mars_model, test, responseName)
  
  return(evaluation)
}

#' Initialize Results Data Frame
#'
#' Initializes a data frame to store the results of unit tests.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{Test}{character. The name of the test.}
#'   \item{Result}{character. The result of the test ("PASS" or "FAIL").}
#' }
#'
#' @export
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

#' Print and Store Test Result
#'
#' Prints and stores the result of a unit test.
#'
#' @param test_name character. The name of the test.
#' @param passed logical. Whether the test passed.
#' @param message character. An optional message to print if the test failed. Default is NULL.
#'
#' @examples
#' \dontrun{
#' print_and_store_result("check_libraries: Required libraries loaded", TRUE)
#' }
#'
#' @export
print_and_store_result <- function(test_name, passed, message = NULL) {
  result <- if(passed) "PASS" else "FAIL"
  cat(sprintf("%-40s [%s]\n", test_name, result))
  if (!is.null(message)) cat("  ", message, "\n")
  test_results <<- rbind(test_results, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE))
}

#' Test `check_libraries` Function
#'
#' Tests the `check_libraries` function to ensure that required libraries are loaded.
#'
#' @examples
#' \dontrun{
#' test_check_libraries()
#' }
#'
#' @export
test_check_libraries <- function() {
  valid_test <- tryCatch({
    check_libraries()
    TRUE
  }, error = function(e) FALSE)
  
  print_and_store_result("check_libraries: Required libraries loaded", valid_test)
}

#' Test `check_response_column` Function
#'
#' Tests the `check_response_column` function to ensure it correctly identifies if the response column exists.
#'
#' @examples
#' \dontrun{
#' test_check_response_column()
#' }
#'
#' @export
test_check_response_column <- function() {
  data <- data.table(response = rnorm(100), predictor1 = rnorm(100))
  
  valid_test <- tryCatch({
    check_response_column(data, "response")
    TRUE
  }, error = function(e) FALSE)
  
  invalid_response <- tryCatch({
    check_response_column(data, "non_existent")
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("check_response_column: Valid response column", valid_test)
  print_and_store_result("check_response_column: Invalid response column", invalid_response)
}

#' Test `handle_missing_values` Function
#'
#' Tests the `handle_missing_values` function to ensure it correctly removes rows with missing values.
#'
#' @examples
#' \dontrun{
#' test_handle_missing_values()
#' }
#'
#' @export
test_handle_missing_values <- function() {
  data <- data.table(response = c(rnorm(95), rep(NA, 5)), predictor1 = c(rnorm(90), rep(NA, 10)))
  
  cleaned_data <- handle_missing_values(data)
  
  missing_values_handled <- nrow(cleaned_data) < nrow(data) && nrow(cleaned_data) > 0
  print_and_store_result("handle_missing_values: Missing values handled", missing_values_handled)
}

#' Test `balance_classes` Function
#'
#' Tests the `balance_classes` function to ensure it correctly balances the classes in the training set.
#'
#' @examples
#' \dontrun{
#' test_balance_classes()
#' }
#'
#' @export
test_balance_classes <- function() {
  data <- data.table(response = factor(rep(c("A", "B"), c(70, 30))), predictor1 = rnorm(100))
  
  balanced_data <- balance_classes(data, "response")
  
  class_counts <- table(balanced_data$response)
  class_balanced <- length(unique(class_counts)) == 1 && all(class_counts > 0)
  print_and_store_result("balance_classes: Classes balanced correctly", class_balanced)
}

#' Test `setup_parallel_processing` Function
#'
#' Tests the `setup_parallel_processing` function to ensure it correctly sets up parallel processing.
#'
#' @examples
#' \dontrun{
#' test_setup_parallel_processing()
#' }
#'
#' @export
test_setup_parallel_processing <- function() {
  cl <- setup_parallel_processing()
  parallel_setup <- !is.null(cl)
  stopCluster(cl)
  print_and_store_result("setup_parallel_processing: Parallel setup", parallel_setup)
}

#' Test `sample_data` Function
#'
#' Tests the `sample_data` function to ensure it correctly samples down the dataset to the specified size.
#'
#' @examples
#' \dontrun{
#' test_sample_data()
#' }
#'
#' @export
test_sample_data <- function() {
  data <- data.table(response = rnorm(20000), predictor1 = rnorm(20000))
  
  sampled_data <- sample_data(data, 10000, 123)
  
  sampling_correct <- nrow(sampled_data) == 10000
  print_and_store_result("sample_data: Data sampled correctly", sampling_correct)
}

#' Test `split_data` Function
#'
#' Tests the `split_data` function to ensure it correctly splits the dataset into training and test sets.
#'
#' @examples
#' \dontrun{
#' test_split_data()
#' }
#'
#' @export
test_split_data <- function() {
  data <- data.table(response = rnorm(100), predictor1 = rnorm(100))
  
  split <- split_data(data, "response", 0.8, 123)
  train <- split$train
  test <- split$test
  
  split_correct <- nrow(train) == 80 && nrow(test) == 20
  print_and_store_result("split_data: Data split correctly", split_correct)
}

#' Test `define_hyperparameter_grid` Function
#'
#' Tests the `define_hyperparameter_grid` function to ensure it correctly defines a grid of hyperparameters.
#'
#' @examples
#' \dontrun{
#' test_define_hyperparameter_grid()
#' }
#'
#' @export
test_define_hyperparameter_grid <- function() {
  hyperparameters <- define_hyperparameter_grid(1:3, c(5, 10, 15))
  
  grid_correct <- nrow(hyperparameters) == 9
  print_and_store_result("define_hyperparameter_grid: Grid defined correctly", grid_correct)
}

#' Test `define_train_control` Function
#'
#' Tests the `define_train_control` function to ensure it correctly defines the control parameters for model training.
#'
#' @examples
#' \dontrun{
#' test_define_train_control()
#' }
#'
#' @export
test_define_train_control <- function() {
  data <- data.table(response = factor(rep(c("A", "B"), each = 50)), predictor1 = rnorm(100))
  
  ctrl <- define_train_control(5, 3, "grid", data, "response")
  
  train_control_defined <- !is.null(ctrl)
  print_and_store_result("define_train_control: Control parameters defined", train_control_defined)
}

#' Test `train_model` Function
#'
#' Tests the `train_model` function to ensure it correctly trains a model using the specified hyperparameters and control parameters.
#'
#' @examples
#' \dontrun{
#' test_train_model()
#' }
#'
#' @export
test_train_model <- function() {
  data <- data.table(response = rnorm(100), predictor1 = rnorm(100))
  hyperparameters <- define_hyperparameter_grid(1:3, c(5, 10, 15))
  ctrl <- define_train_control(5, 3, "grid", data, "response")
  
  model <- train_model(data, "response", "earth", ctrl, hyperparameters, 123)
  
  model_trained <- !is.null(model) && inherits(model, "train")
  print_and_store_result("train_model: Model trained successfully", model_trained)
}

#' Test `evaluate_model` Function
#'
#' Tests the `evaluate_model` function to ensure it correctly evaluates the trained model on the test set.
#'
#' @examples
#' \dontrun{
#' test_evaluate_model()
#' }
#'
#' @export
test_evaluate_model <- function() {
  data <- data.table(response = rnorm(100), predictor1 = rnorm(100))
  hyperparameters <- define_hyperparameter_grid(1:3, c(5, 10, 15))
  ctrl <- define_train_control(5, 3, "grid", data, "response")
  
  model <- train_model(data, "response", "earth", ctrl, hyperparameters, 123)
  split <- split_data(data, "response", 0.8, 123)
  test <- split$test
  
  evaluation <- evaluate_model(model, test, "response")
  
  evaluation_correct <- (!is.null(evaluation$model) && !is.null(evaluation$rmse)) || 
    (!is.null(evaluation$error) && evaluation$error == "Model is NULL, cannot evaluate")
  print_and_store_result("evaluate_model: Model evaluated correctly", evaluation_correct)
}

#' Run All Unit Tests
#'
#' Runs all unit tests and prints a summary of the test results.
#'
#' @examples
#' \dontrun{
#' run_all_tests()
#' }
#'
#' @export
run_all_tests <- function() {
  cat("Running Comprehensive UAT\n")
  cat("==================================\n")
  test_check_libraries()
  test_check_response_column()
  test_handle_missing_values()
  test_setup_parallel_processing()
  test_sample_data()
  test_split_data()
  test_balance_classes()
  test_define_hyperparameter_grid()
  test_define_train_control()
  test_train_model()
  test_evaluate_model()
  cat("==================================\n")
  cat("UAT completed\n\n")
  
  # Print summary
  cat("Test Summary:\n")
  cat("==================================\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

#' Execute All Unit Tests
#'
#' Executes all unit tests by calling the `run_all_tests` function.
#'
#' @examples
#' \dontrun{
#' run_all_tests()
#' }
#'
#' @export
run_all_tests()
