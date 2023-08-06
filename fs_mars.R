library(earth)
library(caret)

#' Train and evaluate a MARS model on a dataset
#'
#' This function takes a dataset, splits it into training and test sets,
#' performs grid search over a predefined set of hyperparameters, and trains
#' a MARS model on the training set using cross-validation. The best model is
#' then used to make predictions on the test set, and depending on the type of
#' response variable, either the root mean squared error (RMSE) for numerical 
#' response or accuracy for categorical response is calculated and returned.
#'
#' NOTE: Ensure that the `earth` and `caret` libraries are loaded before using this function.
#'
#' @param data The dataset to use for training and testing the model.
#' @param responseName The name of the response column. Can be any column name present in the data.
#'        For regression tasks, it should be numeric, and for classification tasks, it should be a factor.
#'        Default is "response".
#' @param p The proportion of the data to use for training. Default is 0.8.
#' @param degree A vector of integers specifying the polynomial degree of the
#'        MARS model to fit. Default is 1:3.
#' @param nprune A vector of integers specifying the number of basis functions
#'        to prune from the MARS model. Default is c(5, 10, 15).
#' @param method The method to use for model fitting. Default is "earth".
#' @param search The method to use for searching the hyperparameter space.
#'        Default is "grid".
#' @param number The number of folds to use for cross-validation. Default is 5.
#' @param seed The seed value for reproducibility. Default is 123.
#' @return A list containing the trained model and either the RMSE or accuracy, 
#'         depending on the response variable type.
#' @examples
#' # Load required libraries and iris dataset
#' library(earth)
#' library(caret)
#' data(iris)
#' 
#' # Test the function for regression
#' result_reg <- fs_mars(iris, responseName = "Sepal.Length", p = 0.7)
#' print(result_reg$model)
#' print(paste("RMSE:", result_reg$rmse))
#'
#' # Test the function for classification
#' result_class <- fs_mars(iris, responseName = "Species", p = 0.7)
#' print(result_class$model)
#' print(paste("Accuracy:", result_class$accuracy))
#' 
#' @export
fs_mars <- function(data, responseName = "response", p = 0.8, degree = 1:3, 
                    nprune = c(5, 10, 15), method = "earth", search = "grid", 
                    number = 5, seed = 123) {
  
  # Check if required libraries are loaded
  if(!"earth" %in% loadedNamespaces()) stop("The 'earth' package is not loaded. Please install and load it before calling this function.")
  if(!"caret" %in% loadedNamespaces()) stop("The 'caret' package is not loaded. Please install and load it before calling this function.")
  
  # Split data into training and test sets
  set.seed(seed)
  trainIndex <- caret::createDataPartition(data[[responseName]], p = p, list = FALSE)
  train <- data[trainIndex, ]
  test <- data[-trainIndex, ]
  
  # Define the grid of hyperparameters to search over
  hyperParameters <- expand.grid(nprune = nprune, degree = degree)
  
  # Define the control parameters for the model training
  ctrl <- caret::trainControl(method = "repeatedcv", number = number, search = search)
  
  # Train the model using grid search for hyperparameters
  set.seed(seed)
  if(method == "earth") {
    mars_model <- caret::train(as.formula(paste(responseName, "~ .")), data = train, method = method, trControl = ctrl, 
                               tuneGrid = hyperParameters)
  } else {
    mars_model <- caret::train(as.formula(paste(responseName, "~ .")), data = train, method = method, trControl = ctrl)
  }
  
  # Make predictions on test set
  pred <- predict(mars_model, newdata = test)
  
  # Calculate the RMSE if the response variable is numeric
  if(is.numeric(test[[responseName]])){
    rmse <- sqrt(mean((test[[responseName]] - pred)^2))
    return(list(model = mars_model, rmse = rmse))
  }
  # Calculate accuracy if the response variable is categorical (factor)
  else if(is.factor(test[[responseName]])){
    accuracy <- sum(test[[responseName]] == pred) / length(pred)
    return(list(model = mars_model, accuracy = accuracy))
  }
  else{
    stop("Unsupported response variable type.")
  }
}