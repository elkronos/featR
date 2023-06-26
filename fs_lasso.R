library(glmnet)

#' Train and test a Lasso regression model on a dataset
#'
#' This function fits a Lasso regression model to the training data using cross-validation, and extracts variable importance scores.
#'
#' @param x The predictor variables in the training dataset.
#' @param y The response variable in the training dataset.
#' @param alpha The Lasso penalty parameter.
#' @param nfolds The number of folds to use for cross-validation.
#'
#' @importFrom glmnet cv.glmnet coef
#'
#' @return A data.frame containing the variable importance scores sorted by importance.
#' The data.frame has two columns: 'Variable' for the variable names and 'Importance' for the corresponding importance scores.
#'
#' @examples
#' # Create a fake dataset
#' set.seed(123)
#' n <- 100
#' p <- 5
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' x3 <- rnorm(n)
#' x4 <- rnorm(n)
#' x5 <- rnorm(n)
#' y <- 2*x1 + 3*x2 + 0.5*x3 - 1.5*x4 + rnorm(n)
#' fakeData <- data.frame(y, x1, x2, x3, x4, x5)
#'
#' # Train a Lasso regression model and get variable importance scores
#' lassoImp <- fs_lasso(x = fakeData[, -1], y = fakeData[, 1])
#' print(lassoImp)
#'
#' @export
#'
#' @importFrom glmnet cv.glmnet coef
#'
#' @importFrom base nrow length stop
#' @importFrom stats is.numeric
#' @importFrom utils order
fs_lasso <- function(x, y, alpha = 1, nfolds = 5) {
  
  # Basic error handling
  if (nrow(x) != length(y)) {
    stop("Error: x and y must have the same number of rows.")
  }
  
  if (!(is.numeric(alpha) && alpha >= 0)) {
    stop("Error: alpha must be a non-negative numeric value.")
  }
  
  if (!(is.numeric(nfolds) && nfolds > 1)) {
    stop("Error: nfolds must be a numeric value greater than 1.")
  }
  
  # Fit a Lasso regression model
  lassoModel <- cv.glmnet(as.matrix(x), y, alpha = alpha, nfolds = nfolds)
  
  # Extract variable importance scores
  lassoImp <- coef(lassoModel, s = "lambda.min")
  lassoImp <- abs(lassoImp)[-1] #removing the intercept
  
  # Save variable names
  varNames <- colnames(x)
  
  # Create a data frame of variable names and their corresponding importance
  lassoImp <- data.frame(Variable = varNames, Importance = lassoImp)
  
  # Sort by importance
  lassoImp <- lassoImp[order(-lassoImp$Importance), ]
  
  # Return the Lasso importance scores
  return(lassoImp)
}