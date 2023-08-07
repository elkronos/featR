# Load packages
library(randomForest)
library(doParallel)
library(foreach)
library(dplyr)
library(caret)

#' Random forest prediction function
#'
#' This function applies the random forest algorithm for either a classification or regression task. 
#' Users can specify the number of trees, whether to compute feature importance, apply custom feature 
#' selection, preprocess the data, and use parallel computing.
#'
#' @param data A data frame with columns for features and a column for the target variable.
#' @param target A character string indicating the name of the column in \code{data} that represents the target variable.
#' @param type A character string indicating the type of prediction task. Should be either "classification" or "regression".
#' @param ntree An integer indicating the number of trees to grow in the random forest. Default is 500.
#' @param importance A logical value indicating whether to compute the importance of features. Default is TRUE.
#' @param sample_size An optional integer indicating the number of observations to randomly sample from \code{data}.
#' @param feature_select An optional function that takes a data frame as input and returns a data frame with selected features.
#' @param preprocess An optional function that takes a data frame as input and returns a processed data frame.
#' @param n_cores An integer indicating the number of cores to use for parallel computing. Default is 1 (no parallelization).
#' @param split_ratio A numeric value between 0 and 1 indicating the proportion of data to include in the training set. Default is 0.75.
#' @param seed An optional integer for reproducibility.
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
#' }
#'
#' @export
fs_randomforest <- function(data, target, type, ntree = 500, importance = TRUE, 
                            sample_size = NULL, feature_select = NULL, 
                            preprocess = NULL, n_cores = 1, split_ratio = 0.75, seed = NULL) {
  
  # Set seed for reproducibility
  if(!is.null(seed)) set.seed(seed)
  
  # Early validations
  if (!target %in% names(data)) stop("The target variable does not exist in the dataset")
  if(!(type %in% c("classification", "regression"))) stop("Invalid type parameter. Choose 'classification' or 'regression'")
  if(!is.null(preprocess) && !is.function(preprocess)) stop("preprocess must be a function")
  if(!is.null(feature_select) && !is.function(feature_select)) stop("feature_select must be a function")
  if(n_cores > detectCores(logical = FALSE)) stop("Specified number of cores exceeds available cores.")
  
  target_idx <- match(target, names(data))
  
  if(type == "classification") {
    data[[target]] <- as.factor(data[[target]])
  }
  
  # Apply preprocessing
  if(!is.null(preprocess)) {
    data <- preprocess(data)
  }
  
  # Apply feature selection
  if(!is.null(feature_select)) {
    data <- feature_select(data)
  }
  
  # Sample data
  if(!is.null(sample_size)) {
    if(type == "classification") {
      data <- data %>% group_by(!!sym(target)) %>% sample_n(sample_size/nlevels(data[[target]]))
    } else {
      data <- data %>% sample_n(sample_size)
    }
  }
  
  # Stratified split for classification tasks
  inTrain <- createDataPartition(y = data[[target]], p = split_ratio, list = FALSE)
  training <- data[inTrain,]
  testing <- data[-inTrain,]
  
  # Parallel computing
  if(n_cores > 1) {
    registerDoParallel(cores = n_cores)
    on.exit(stopImplicitCluster())
    rf <- foreach(ntree = rep(floor(ntree/n_cores), n_cores), 
                  .combine = randomForest::combine, 
                  .packages = c("randomForest")) %dopar% {
                    
                    library(randomForest)  # Load within the child process
                    
                    randomForest(x = training[, -target_idx],
                                 y = training[[target]],
                                 ntree = ntree, importance = importance)
                  }
  } else {
    rf <- randomForest(x = training[, -target_idx],
                       y = training[[target]],
                       ntree = ntree, importance = importance)
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