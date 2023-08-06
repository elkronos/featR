library(e1071)
library(caret)

#' SVM Model with Cross-Validation and Grid Search
#'
#' This function trains an SVM model using cross-validation and grid search. It can be used for both classification and regression tasks. For classification, the function uses the svmLinear method and for regression, it uses the svmRadial method.
#'
#' @param data A data frame containing the data to be used for training and testing the model.
#' @param target The name of the target variable in the data.
#' @param task The type of task to be performed. "classification" for classification task and "regression" for regression task.
#' @param nfolds The number of folds to be used for cross-validation (default is 5).
#' @param tune_grid A data frame containing the tuning grid for the SVM model. If NULL, default tuning parameters will be used.
#' @param seed Seed for reproducibility. If NULL, random seeding will be used (default is NULL).
#'
#' @return A list containing:
#' \itemize{
#'   \item model: The trained SVM model.
#'   \item test_set: The data set used for testing.
#'   \item predictions: Predictions made on the test set.
#'   \item performance: For classification, a confusion matrix. For regression, R-squared.
#' }
#'
#' @examples
#' \dontrun{
#' # Classification example using iris data
#' data(iris)
#' output <- fs_svm(iris, "Species", "classification", seed = 123)
#' print(output$performance)
#'
#' # Regression example using mtcars data
#' data(mtcars)
#' output <- fs_svm(mtcars, "mpg", "regression", seed = 123)
#' print(output$r_squared)
#' }
#'
#' @import e1071 caret
#' @export
fs_svm <- function(data, target, task, nfolds=5, tune_grid=NULL, seed=NULL) {
  
  # Parameter checks
  stopifnot(is.data.frame(data))
  stopifnot(target %in% names(data))
  stopifnot(task %in% c("classification", "regression"))
  
  # If tune_grid is NULL, provide default grids
  if (is.null(tune_grid)) {
    if (task == "classification") {
      tune_grid <- expand.grid(C = seq(0.1, 1, by = 0.1))
    } else {
      tune_grid <- expand.grid(sigma = seq(0.01, 1, by = 0.1), C = seq(1, 10, by = 1))
    }
  }
  
  # Create formula for dependent and independent variables
  all_vars <- names(data)
  independent_vars <- all_vars[!all_vars %in% target]
  formula <- as.formula(paste(target, "~", paste(independent_vars, collapse = "+")))
  
  # Split the data into a training set and a test set
  if (!is.null(seed)) {
    set.seed(seed)
  }
  train_index <- createDataPartition(data[,target], p = 0.7, list = FALSE)
  train_set <- data[train_index,]
  test_set <- data[-train_index,]
  
  # Determine method and preProc
  if (task == "classification") {
    method <- "svmLinear"
    preProc <- NULL
  } else {
    method <- "svmRadial"
    preProc <- c("center", "scale")
  }
  
  # Train SVM
  svm_fit <- train(formula, data = train_set, method = method, 
                   trControl = trainControl(method = "cv", number = nfolds), 
                   preProc = preProc, 
                   tuneGrid = tune_grid)
  
  # Make predictions on the test set
  predictions <- predict(svm_fit, newdata = test_set)
  
  # Calculate performance measures
  if (task == "classification") {
    confusion <- confusionMatrix(predictions, test_set[, target])
    output <- list(model = svm_fit, test_set = test_set, predictions = predictions, performance = confusion)
  } else {
    rsq <- 1 - sum((predictions - test_set[, target])^2) / sum((test_set[, target] - mean(test_set[, target]))^2)
    output <- list(model = svm_fit, test_set = test_set, predictions = predictions, r_squared = rsq)
  }
  
  return(output)
}