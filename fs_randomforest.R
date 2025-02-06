# ------------------------------------------------------------------------------
# Required Libraries and Installation Checks
# ------------------------------------------------------------------------------
required_packages <- c("randomForest", "doParallel", "foreach", 
                       "dplyr", "caret", "data.table", "testthat")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(randomForest)
library(doParallel)
library(foreach)
library(dplyr)
library(caret)
library(data.table)
library(testthat)

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

#' Prepare data for modeling
#'
#' This function performs several preprocessing steps including data.table conversion,
#' custom preprocessing, feature selection, date variable conversion, target conversion,
#' and optional sampling.
#'
#' @param data A data.frame or data.table containing the data.
#' @param target The name of the target variable.
#' @param type Either "classification" or "regression".
#' @param preprocess An optional function to preprocess the data.
#' @param feature_select An optional function to perform feature selection.
#' @param sample_size An optional integer to sample a subset of the data.
#'
#' @return A data.table with the prepared data.
prepare_data <- function(data, target, type, preprocess = NULL, 
                         feature_select = NULL, sample_size = NULL) {
  # Convert to data.table for efficient processing
  dt <- as.data.table(data)
  
  # Apply user-defined preprocessing function if provided
  if (!is.null(preprocess)) {
    if (!is.function(preprocess)) stop("preprocess must be a function.")
    dt <- preprocess(dt)
  }
  
  # Apply user-defined feature selection function if provided
  if (!is.null(feature_select)) {
    if (!is.function(feature_select)) stop("feature_select must be a function.")
    dt <- feature_select(dt)
    if (!target %in% names(dt)) {
      stop("The target variable was removed during feature selection.")
    }
  }
  
  # Convert Date columns to numeric
  date_cols <- sapply(dt, inherits, what = "Date")
  if (any(date_cols)) {
    dt[, (which(date_cols)) := lapply(.SD, as.numeric), .SDcols = which(date_cols)]
  }
  
  # Ensure target variable is of the appropriate type
  if (type == "classification") {
    dt[[target]] <- as.factor(dt[[target]])
  } else {
    dt[[target]] <- as.numeric(dt[[target]])
  }
  
  # Sample data if sample_size is specified and smaller than available rows
  if (!is.null(sample_size) && sample_size < nrow(dt)) {
    if (type == "classification") {
      # Stratified sampling by target
      dt <- dt[, .SD[sample(.N, size = max(1, floor((.N / nrow(dt)) * sample_size)))], by = target]
    } else {
      dt <- dt[sample(.N, sample_size)]
    }
  }
  
  return(dt)
}

#' Train Random Forest Model
#'
#' This function trains a random forest model with support for parallel processing.
#'
#' @param training A data.table containing the training data.
#' @param target The name of the target variable.
#' @param type Either "classification" or "regression".
#' @param ntree Number of trees to grow.
#' @param importance Logical, whether to compute variable importance.
#' @param n_cores Number of cores to use for parallel processing.
#' @param seed Optional seed for reproducibility.
#'
#' @return A trained random forest model.
train_rf_model <- function(training, target, type, ntree, importance, n_cores, seed = NULL) {
  # Identify target column index
  target_idx <- match(target, names(training))
  x_train <- training[, -target_idx, with = FALSE]
  y_train <- training[[target]]
  
  if (n_cores > 1) {
    # Set up parallel cluster
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    on.exit({
      stopCluster(cl)
      registerDoSEQ()
    })
    if (!is.null(seed)) {
      clusterSetRNGStream(cl, seed)
    }
    
    # Divide trees among cores
    ntree_list <- rep(floor(ntree / n_cores), n_cores)
    ntree_remainder <- ntree %% n_cores
    if (ntree_remainder > 0) {
      ntree_list[1:ntree_remainder] <- ntree_list[1:ntree_remainder] + 1
    }
    
    rf_model <- foreach(ntree_part = ntree_list, .combine = randomForest::combine, 
                        .packages = "randomForest") %dopar% {
                          randomForest(x = x_train, y = y_train, ntree = ntree_part, importance = importance)
                        }
  } else {
    rf_model <- randomForest(x = x_train, y = y_train, ntree = ntree, importance = importance)
  }
  
  return(rf_model)
}

#' Evaluate Predictions
#'
#' This function computes accuracy for classification or RMSE for regression.
#'
#' @param model A trained random forest model.
#' @param testing A data.table containing the test data.
#' @param target The name of the target variable.
#' @param type Either "classification" or "regression".
#'
#' @return A list containing predictions and the evaluation metric.
evaluate_model <- function(model, testing, target, type) {
  target_idx <- match(target, names(testing))
  x_test <- testing[, -target_idx, with = FALSE]
  y_test <- testing[[target]]
  
  if (type == "classification") {
    preds <- predict(model, newdata = x_test, type = "class")
    accuracy <- sum(preds == y_test) / length(preds)
    return(list(predictions = preds, accuracy = accuracy))
  } else {
    preds <- predict(model, newdata = x_test)
    rmse <- sqrt(mean((preds - y_test) ^ 2))
    return(list(predictions = preds, RMSE = rmse))
  }
}

# ------------------------------------------------------------------------------
# Main Function: fs_randomforest
# ------------------------------------------------------------------------------

#' Random Forest Prediction Function for Big Data
#'
#' This function applies the random forest algorithm for either classification or regression.
#' It includes options for preprocessing, feature selection, random sampling, and parallel computing.
#'
#' @param data A data.frame or data.table with features and a target variable.
#' @param target A character string indicating the target variable's name.
#' @param type A character string: either "classification" or "regression".
#' @param ntree Number of trees to grow in the random forest (default 500).
#' @param importance Logical, whether to compute feature importance (default TRUE).
#' @param sample_size Optional integer for randomly sampling observations (default NULL).
#' @param feature_select Optional function for custom feature selection.
#' @param preprocess Optional function for custom data preprocessing.
#' @param n_cores Number of cores for parallel computing (default 1).
#' @param split_ratio Proportion of data for training (default 0.75).
#' @param seed Optional integer for reproducibility.
#'
#' @return A list containing:
#' \item{model}{The trained random forest model.}
#' \item{predictions}{Predictions on the test set.}
#' \item{accuracy}{Classification accuracy (if type is "classification").}
#' \item{RMSE}{Root Mean Squared Error (if type is "regression").}
#'
#' @examples
#' \dontrun{
#' # Classification example with iris data
#' iris_result <- fs_randomforest(data = iris, target = "Species", type = "classification")
#' print(iris_result$accuracy)
#' print(head(iris_result$predictions))
#'
#' # Regression example with mtcars data
#' mtcars_result <- fs_randomforest(data = mtcars, target = "mpg", type = "regression")
#' print(mtcars_result$RMSE)
#' print(head(mtcars_result$predictions))
#'
#' # Using a custom preprocessing function
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
  # -------------------------------
  # Input Validations and Seed Setup
  # -------------------------------
  if (!target %in% names(data)) {
    stop("The target variable does not exist in the dataset.")
  }
  if (!(type %in% c("classification", "regression"))) {
    stop("Invalid type parameter. Choose 'classification' or 'regression'.")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Adjust number of cores if needed
  max_cores <- parallel::detectCores(logical = TRUE)
  if (n_cores > max_cores) {
    warning("Specified number of cores exceeds available cores. Using maximum available cores.")
    n_cores <- max_cores
  }
  
  # -------------------------------
  # Data Preparation
  # -------------------------------
  dt <- prepare_data(data, target, type, preprocess, feature_select, sample_size)
  
  # Split the data into training and testing sets
  partition <- createDataPartition(y = dt[[target]], p = split_ratio, list = FALSE)
  training <- dt[partition]
  testing <- dt[-partition]
  
  if (!target %in% names(training)) {
    stop("The target variable is missing in the training data after preprocessing or feature selection.")
  }
  
  # -------------------------------
  # Model Training
  # -------------------------------
  rf_model <- train_rf_model(training, target, type, ntree, importance, n_cores, seed)
  
  # -------------------------------
  # Model Evaluation
  # -------------------------------
  eval_results <- evaluate_model(rf_model, testing, target, type)
  
  # Return the results
  return(c(list(model = rf_model), eval_results))
}

# ------------------------------------------------------------------------------
# Unit Tests for fs_randomforest
# ------------------------------------------------------------------------------

test_fs_randomforest <- function() {
  cat("Running unit tests for fs_randomforest...\n")
  
  # Test 1: Regression on a simple numeric data frame
  df1 <- data.frame(
    A = sample(1:100, 1000, replace = TRUE),
    B = sample(1:50, 1000, replace = TRUE),
    target = rnorm(1000)
  )
  result1 <- fs_randomforest(df1, "target", type = "regression")
  cat("Test 1 RMSE:", result1$RMSE, "\n")
  expect_s3_class(result1$model, "randomForest")
  expect_type(result1$predictions, "double")
  expect_type(result1$RMSE, "double")
  
  # Test 2: Classification on a simple numeric data frame
  df2 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = sample(1:5, 1000, replace = TRUE),
    target = factor(sample(1:2, 1000, replace = TRUE))
  )
  result2 <- fs_randomforest(df2, "target", type = "classification")
  cat("Test 2 Accuracy:", result2$accuracy, "\n")
  expect_s3_class(result2$model, "randomForest")
  expect_true(is.factor(result2$predictions))
  expect_type(result2$accuracy, "double")
  
  # Test 3: Classification with a categorical predictor
  df3 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = factor(sample(c("yes", "no"), 1000, replace = TRUE)),
    target = factor(sample(1:2, 1000, replace = TRUE))
  )
  result3 <- fs_randomforest(df3, "target", type = "classification")
  cat("Test 3 Accuracy:", result3$accuracy, "\n")
  expect_s3_class(result3$model, "randomForest")
  expect_true(is.factor(result3$predictions))
  expect_type(result3$accuracy, "double")
  
  # Test 4: Classification with date variables
  df4 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = factor(sample(c("yes", "no"), 1000, replace = TRUE)),
    C = seq(as.Date("2001-01-01"), by = "day", length.out = 1000),
    target = factor(sample(1:2, 1000, replace = TRUE))
  )
  result4 <- fs_randomforest(df4, "target", type = "classification")
  cat("Test 4 Accuracy:", result4$accuracy, "\n")
  expect_s3_class(result4$model, "randomForest")
  expect_true(is.factor(result4$predictions))
  expect_type(result4$accuracy, "double")
  
  # Test 5: Error when target is missing
  df5 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = factor(sample(c("yes", "no"), 1000, replace = TRUE))
  )
  expect_error(fs_randomforest(df5, "target", type = "classification"),
               "The target variable does not exist in the dataset.")
  
  # Test 6: Error for invalid type parameter
  df6 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = sample(1:5, 1000, replace = TRUE),
    target = sample(1:2, 1000, replace = TRUE)
  )
  expect_error(fs_randomforest(df6, "target", type = "invalid_type"),
               "Invalid type parameter. Choose 'classification' or 'regression'.")
  
  # Test 7: Regression with a preprocessing function
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
  cat("Test 7 RMSE:", result7$RMSE, "\n")
  expect_s3_class(result7$model, "randomForest")
  expect_type(result7$predictions, "double")
  expect_type(result7$RMSE, "double")
  
  cat("All unit tests for fs_randomforest completed.\n")
}

# Run tests
# test_fs_randomforest()
