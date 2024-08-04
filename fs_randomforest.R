# Load necessary libraries for testing and functionality
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("doParallel", quietly = TRUE)) install.packages("doParallel")
if (!requireNamespace("foreach", quietly = TRUE)) install.packages("foreach")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
if (!requireNamespace("testthat", quietly = TRUE)) install.packages("testthat")
if (!requireNamespace("bigmemory", quietly = TRUE)) install.packages("bigmemory")

library(randomForest)
library(doParallel)
library(foreach)
library(dplyr)
library(caret)
library(data.table)
library(testthat)
library(bigmemory)

#' Random forest prediction function for big data
#'
#' This function applies the random forest algorithm for either a classification or regression task. 
#' Users can specify the number of trees, whether to compute feature importance, apply custom feature 
#' selection, preprocess the data, and use parallel computing.
#'
#' @param data A data frame or data table with columns for features and a column for the target variable.
#' @param target A character string indicating the name of the column in \code{data} that represents the target variable.
#' @param type A character string indicating the type of prediction task. Should be either "classification" or "regression".
#' @param ntree An integer indicating the number of trees to grow in the random forest. Default is 500.
#' @param importance A logical value indicating whether to compute the importance of features. Default is TRUE.
#' @param sample_size An optional integer indicating the number of observations to randomly sample from \code{data}. Default is NULL, meaning no sampling.
#' @param feature_select An optional function that takes a data frame as input and returns a data frame with selected features.
#' @param preprocess An optional function that takes a data frame as input and returns a processed data frame.
#' @param n_cores An integer indicating the number of cores to use for parallel computing. Default is 1 (no parallelization).
#' @param split_ratio A numeric value between 0 and 1 indicating the proportion of data to include in the training set. Default is 0.75.
#' @param seed An optional integer for reproducibility. Default is NULL.
#'
#' @return A list with the following components:
#' \itemize{
#' \item \code{model}: The trained random forest model.
#' \item \code{predictions}: A vector of predictions for the target variable in the test set.
#' \item \code{accuracy}: If type is "classification", this returns the model's classification accuracy. If type is "regression", this returns the Root Mean Squared Error (RMSE).
#' }
#'
#' @examples
#' \dontrun{
#' data(iris)
#' iris_result <- fs_randomforest(data = iris, target = "Species", type = "classification")
#' print(iris_result$accuracy)
#' print(head(iris_result$predictions))
#'
#' data(mtcars)
#' mtcars_result <- fs_randomforest(data = mtcars, target = "mpg", type = "regression")
#' print(mtcars_result$RMSE)
#' print(head(mtcars_result$predictions))
#' 
#' # Example with preprocessing function
#' preprocess_function <- function(data) {
#'   data$A <- data$A / max(data$A)
#'   return(data)
#' }
#' df <- data.frame(
#'   A = sample(1:100, 1000, replace = TRUE),
#'   B = sample(1:50, 1000, replace = TRUE),
#'   target = rnorm(1000)
#' )
#' result <- fs_randomforest(df, "target", type = "regression", preprocess = preprocess_function)
#' print(result$RMSE)
#' }
#'
#' @export
fs_randomforest <- function(data, target, type, ntree = 500, importance = TRUE, 
                            sample_size = NULL, feature_select = NULL, 
                            preprocess = NULL, n_cores = 1, split_ratio = 0.75, seed = NULL) {
  
  # Set seed for reproducibility
  if(!is.null(seed)) set.seed(seed)
  
  # Early validations
  if (!target %in% names(data)) stop("The target variable does not exist in the dataset.")
  if(!(type %in% c("classification", "regression"))) stop("Invalid type parameter. Choose 'classification' or 'regression'.")
  if(!is.null(preprocess) && !is.function(preprocess)) stop("preprocess must be a function.")
  if(!is.null(feature_select) && !is.function(feature_select)) stop("feature_select must be a function.")
  if(n_cores > parallel::detectCores(logical = FALSE)) stop("Specified number of cores exceeds available cores.")
  
  target_idx <- match(target, names(data))
  
  if(type == "classification") {
    data[[target]] <- as.factor(data[[target]])
  }
  
  # Convert to data.table for efficient memory usage
  data <- as.data.table(data)
  
  # Sample data if specified
  if(!is.null(sample_size)) {
    if(type == "classification") {
      data <- data[, .SD[sample(.N, sample_size)], by = target]
    } else {
      data <- data[sample(.N, sample_size)]
    }
  }
  
  # Stratified split for classification tasks
  inTrain <- createDataPartition(y = data[[target]], p = split_ratio, list = FALSE)
  training <- data[inTrain]
  testing <- data[-inTrain]
  
  # Apply preprocessing
  if(!is.null(preprocess)) {
    training <- preprocess(training)
    testing <- preprocess(testing)
  }
  
  # Apply feature selection
  if(!is.null(feature_select)) {
    training <- feature_select(training)
    testing <- testing[, names(training), with = FALSE]
  }
  
  # Parallel computing setup
  if(n_cores > 1) {
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    on.exit(stopCluster(cl))
    rf <- parLapply(cl, rep(floor(ntree/n_cores), n_cores), function(ntree_part) {
      randomForest(x = training[, -target_idx, with = FALSE], y = training[[target]], ntree = ntree_part, importance = importance)
    })
    rf <- do.call(randomForest::combine, rf)
  } else {
    rf <- randomForest(x = training[, -target_idx, with = FALSE], y = training[[target]], ntree = ntree, importance = importance)
  }
  
  # Predictions and accuracy/RMSE
  if(type == "classification") {
    predictions <- predict(rf, newdata = testing, type = "class")
    accuracy <- sum(predictions == testing[[target]]) / length(predictions)
    return(list(model = rf, predictions = predictions, accuracy = accuracy))
  } else {
    predictions <- predict(rf, newdata = testing)
    RMSE <- sqrt(mean((predictions - testing[[target]])^2))
    return(list(model = rf, predictions = predictions, RMSE = RMSE))
  }
}

# Define UAT for fs_randomforest function
test_fs_randomforest <- function() {
  cat("Running UAT for fs_randomforest...\n")
  
  # Test 1: Simple numeric data frame (Regression)
  df1 <- data.frame(
    A = sample(1:100, 1000, replace = TRUE),
    B = sample(1:50, 1000, replace = TRUE),
    target = rnorm(1000)
  )
  result1 <- fs_randomforest(df1, "target", type = "regression")
  print(result1)
  expect_type(result1$model, "list")
  expect_type(result1$predictions, "double")
  expect_type(result1$RMSE, "double")
  
  # Test 2: Simple numeric data frame (Classification)
  df2 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = sample(1:5, 1000, replace = TRUE),
    target = sample(1:2, 1000, replace = TRUE)
  )
  result2 <- fs_randomforest(df2, "target", type = "classification")
  print(result2)
  expect_type(result2$model, "list")
  expect_type(result2$predictions, "integer")
  expect_type(result2$accuracy, "double")
  
  # Test 3: Data frame with categorical variables (Classification)
  df3 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = sample(c("yes", "no"), 1000, replace = TRUE),
    target = sample(1:2, 1000, replace = TRUE)
  )
  result3 <- fs_randomforest(df3, "target", type = "classification")
  print(result3)
  expect_type(result3$model, "list")
  expect_type(result3$predictions, "integer")
  expect_type(result3$accuracy, "double")
  
  # Test 4: Data frame with date variables (Classification)
  df4 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = sample(c("yes", "no"), 1000, replace = TRUE),
    C = seq(as.Date("2001/1/1"), by = "day", length.out = 1000),
    target = sample(1:2, 1000, replace = TRUE)
  )
  result4 <- fs_randomforest(df4, "target", type = "classification")
  print(result4)
  expect_type(result4$model, "list")
  expect_type(result4$predictions, "integer")
  expect_type(result4$accuracy, "double")
  
  # Test 5: Error handling when target is missing
  df5 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = sample(c("yes", "no"), 1000, replace = TRUE)
  )
  expect_error(fs_randomforest(df5, "target", type = "classification"), "The target variable does not exist in the dataset.")
  
  # Test 6: Error handling for invalid type
  df6 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = sample(1:5, 1000, replace = TRUE),
    target = sample(1:2, 1000, replace = TRUE)
  )
  expect_error(fs_randomforest(df6, "target", type = "invalid_type"), "Invalid type parameter. Choose 'classification' or 'regression'.")
  
  # Test 7: Simple numeric data frame with preprocessing (Regression)
  preprocess_function <- function(data) {
    data$A <- data$A / max(data$A)
    return(data)
  }
  df7 <- data.frame(
    A = sample(1:100, 1000, replace = TRUE),
    B = sample(1:50, 1000, replace = TRUE),
    target = rnorm(1000)
  )
  result7 <- fs_randomforest(df7, "target", type = "regression", preprocess = preprocess_function)
  print(result7)
  expect_type(result7$model, "list")
  expect_type(result7$predictions, "double")
  expect_type(result7$RMSE, "double")
  
  cat("UAT for fs_randomforest completed.\n")
}

# Run the UAT function
test_fs_randomforest()
