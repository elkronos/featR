library(glmnet)
library(parallel)
library(Matrix)

#' Validate input parameters for fs_lasso
#'
#' This function checks the input parameters for the fs_lasso function to ensure they meet the required criteria.
#'
#' @param x A data frame or matrix of predictor variables.
#' @param y A numeric vector of response variables.
#' @param alpha A non-negative numeric value representing the Lasso penalty parameter.
#' @param nfolds A numeric value greater than 1 representing the number of folds for cross-validation.
#' @param standardize A logical value indicating whether to standardize the predictors.
#' @param parallel A logical value indicating whether to use parallel processing.
#' @param verbose A logical value indicating whether to print progress messages.
#' @param seed A single numeric value to set the random seed for reproducibility.
#' @param custom_folds A vector representing custom cross-validation folds.
#' @param return_model A logical value indicating whether to return the trained model object.
#' @importFrom stats is.numeric
validate_parameters <- function(x, y, alpha, nfolds, standardize, parallel, verbose, seed, custom_folds, return_model) {
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop("Error: 'x' should be a data frame or matrix.")
  }
  
  if (nrow(x) != length(y)) {
    stop("Error: 'x' and 'y' must have the same number of rows.")
  }
  
  if (!is.numeric(y)) {
    stop("Error: 'y' should be numeric.")
  }
  
  if (!(is.numeric(alpha) && alpha >= 0)) {
    stop("Error: 'alpha' must be a non-negative numeric value.")
  }
  
  if (!(is.numeric(nfolds) && nfolds > 1)) {
    stop("Error: 'nfolds' must be a numeric value greater than 1.")
  }
  
  if (!is.logical(standardize)) {
    stop("Error: 'standardize' must be a logical value.")
  }
  
  if (!is.logical(parallel)) {
    stop("Error: 'parallel' must be a logical value.")
  }
  
  if (!is.logical(verbose)) {
    stop("Error: 'verbose' must be a logical value.")
  }
  
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1)) {
    stop("Error: 'seed' must be a single numeric value.")
  }
  
  if (!is.null(custom_folds) && (!is.vector(custom_folds) || length(custom_folds) != length(y))) {
    stop("Error: 'custom_folds' must be a vector of the same length as 'y'.")
  }
  
  if (!is.logical(return_model)) {
    stop("Error: 'return_model' must be a logical value.")
  }
}

#' Handle missing values by imputing with the mean
#'
#' This function imputes missing values in the dataset by replacing them with the mean of the corresponding column.
#'
#' @param x A data frame or matrix of predictor variables.
#' @param y A numeric vector of response variables.
#' @param nfolds A numeric value greater than 1 representing the number of folds for cross-validation.
#' @param custom_folds A vector representing custom cross-validation folds.
#' @importFrom stats colMeans
#' @return The data frame or matrix with missing values imputed.
handle_missing_values <- function(x, y, nfolds, custom_folds) {
  if (is.null(custom_folds)) {
    folds <- sample(1:nfolds, nrow(x), replace = TRUE)
  } else {
    folds <- custom_folds
  }
  
  for (fold in unique(folds)) {
    train_idx <- which(folds != fold)
    test_idx <- which(folds == fold)
    
    train_x <- x[train_idx, ]
    train_y <- y[train_idx]
    
    if (any(is.na(train_x))) {
      col_means <- colMeans(train_x, na.rm = TRUE)
      for (col in seq_along(col_means)) {
        train_x[is.na(train_x[, col]), col] <- col_means[col]
      }
    }
    
    if (any(is.na(x[test_idx, ]))) {
      for (col in seq_along(col_means)) {
        x[test_idx, col][is.na(x[test_idx, col])] <- col_means[col]
      }
    }
  }
  
  return(x)
}

#' Convert x to a sparse matrix for scalability
#'
#' This function converts a data frame or matrix to a sparse matrix format for better scalability.
#'
#' @param x A data frame or matrix of predictor variables.
#' @importFrom Matrix as
#' @return A sparse matrix representation of the input data.
convert_to_sparse <- function(x) {
  return(as(as.matrix(x), "CsparseMatrix"))
}

#' Fit a Lasso regression model
#'
#' This function fits a Lasso regression model to the provided data using cross-validation.
#'
#' @param x A sparse matrix of predictor variables.
#' @param y A numeric vector of response variables.
#' @param alpha A non-negative numeric value representing the Lasso penalty parameter.
#' @param nfolds A numeric value greater than 1 representing the number of folds for cross-validation.
#' @param standardize A logical value indicating whether to standardize the predictors.
#' @param parallel A logical value indicating whether to use parallel processing.
#' @param custom_folds A vector representing custom cross-validation folds.
#' @param seed A single numeric value to set the random seed for reproducibility.
#' @param verbose A logical value indicating whether to print progress messages.
#' @importFrom glmnet cv.glmnet
#' @importFrom parallel makeCluster stopCluster detectCores clusterEvalQ
#' @importFrom stats set.seed
#' @return A fitted Lasso model.
fit_lasso_model <- function(x, y, alpha, nfolds, standardize, parallel, custom_folds, seed, verbose) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  if (parallel) {
    cl <- makeCluster(detectCores() - 1)
    clusterEvalQ(cl, library(glmnet))
    if (verbose) message("Parallel processing enabled.")
  }
  
  if (is.null(custom_folds)) {
    lassoModel <- cv.glmnet(x, y, alpha = alpha, nfolds = nfolds, standardize = standardize, parallel = parallel, keep = TRUE)
  } else {
    lassoModel <- cv.glmnet(x, y, alpha = alpha, foldid = custom_folds, standardize = standardize, parallel = parallel, keep = TRUE)
  }
  
  if (parallel) {
    stopCluster(cl)
  }
  
  return(lassoModel)
}

#' Extract variable importance scores
#'
#' This function extracts the variable importance scores from a fitted Lasso model.
#'
#' @param lassoModel A fitted Lasso model.
#' @param x A sparse matrix of predictor variables.
#' @importFrom glmnet coef
#' @return A data frame containing variable names and their corresponding importance scores.
extract_importance <- function(lassoModel, x) {
  lassoImp <- as.vector(coef(lassoModel, s = "lambda.min"))[-1] # Removing the intercept
  
  lassoImp <- data.frame(
    Variable = colnames(x),
    Importance = lassoImp
  )
  
  lassoImp <- lassoImp[order(-lassoImp$Importance), ]
  return(lassoImp)
}

#' Fit and evaluate a Lasso regression model
#'
#' This function fits a Lasso regression model to the provided data using cross-validation, extracts variable importance scores, and optionally returns the fitted model object.
#'
#' @param x A data frame or matrix of predictor variables.
#' @param y A numeric vector of response variables.
#' @param alpha A non-negative numeric value representing the Lasso penalty parameter. Default is 1.
#' @param nfolds A numeric value greater than 1 representing the number of folds for cross-validation. Default is 5.
#' @param standardize A logical value indicating whether to standardize the predictors. Default is TRUE.
#' @param parallel A logical value indicating whether to use parallel processing. Default is TRUE.
#' @param verbose A logical value indicating whether to print progress messages. Default is FALSE.
#' @param seed A single numeric value to set the random seed for reproducibility. Default is NULL.
#' @param return_model A logical value indicating whether to return the trained model object. Default is FALSE.
#' @param custom_folds A vector representing custom cross-validation folds. Default is NULL.
#' @return A list containing:
#'   \itemize{
#'     \item \code{importance}: A data frame with variable importance scores sorted by importance.
#'     \item \code{model}: The trained Lasso model (if \code{return_model} is TRUE).
#'   }
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   n <- 100
#'   X <- matrix(rnorm(n * 5), ncol = 5)
#'   y <- 2 * X[,1] - 3 * X[,2] + 1.5 * X[,3] + rnorm(n)
#'   colnames(X) <- paste0("X", 1:5)
#'   fakeData <- data.frame(y = y, X)
#'   result <- fs_lasso(x = fakeData[, -1], y = fakeData[, 1], verbose = TRUE, seed = 123)
#'   print(result$importance)
#' }
#' @importFrom glmnet cv.glmnet coef
#' @importFrom parallel makeCluster stopCluster detectCores clusterEvalQ
#' @importFrom stats colMeans is.numeric set.seed
#' @importFrom Matrix as
#' @export
fs_lasso <- function(x, y, alpha = 1, nfolds = 5, standardize = TRUE, parallel = TRUE, verbose = FALSE, seed = NULL, return_model = FALSE, custom_folds = NULL) {
  
  # Validate parameters
  validate_parameters(x, y, alpha, nfolds, standardize, parallel, verbose, seed, custom_folds, return_model)
  
  # Handle missing values
  x <- handle_missing_values(x, y, nfolds, custom_folds)
  
  # Convert to sparse matrix
  x <- convert_to_sparse(x)
  
  # Fit Lasso model
  lassoModel <- fit_lasso_model(x, y, alpha, nfolds, standardize, parallel, custom_folds, seed, verbose)
  
  # Extract importance scores
  lassoImp <- extract_importance(lassoModel, x)
  
  # Return results
  result <- list(importance = lassoImp)
  if (return_model) {
    result$model <- lassoModel
  }
  
  return(result)
}

#' Test fs_lasso function
#'
#' This function runs a series of tests to validate the functionality of the fs_lasso function.
#'
#' @examples
#' \dontrun{
#'   test_fs_lasso()
#' }
#' @export
test_fs_lasso <- function() {
  cat("Running UAT for fs_lasso...\n")
  
  # Test 1: Basic Functionality with Default Parameters
  cat("Test 1: Basic Functionality with Default Parameters\n")
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  y <- 2 * X[,1] - 3 * X[,2] + 1.5 * X[,3] + rnorm(n)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(y = y, X)
  result <- fs_lasso(x = fakeData[, -1], y = fakeData[, 1])
  print(result$importance)
  
  # Test 2: Verbose Option
  cat("Test 2: Verbose Option\n")
  result_verbose <- fs_lasso(x = fakeData[, -1], y = fakeData[, 1], verbose = TRUE)
  print(result_verbose$importance)
  
  # Test 3: Parallel Processing
  cat("Test 3: Parallel Processing\n")
  result_parallel <- fs_lasso(x = fakeData[, -1], y = fakeData[, 1], parallel = TRUE, verbose = TRUE)
  print(result_parallel$importance)
  
  # Test 4: Custom Seed for Reproducibility
  cat("Test 4: Custom Seed for Reproducibility\n")
  result_seed1 <- fs_lasso(x = fakeData[, -1], y = fakeData[, 1], seed = 123)
  result_seed2 <- fs_lasso(x = fakeData[, -1], y = fakeData[, 1], seed = 123)
  identical_results <- all(result_seed1$importance == result_seed2$importance)
  print(result_seed1$importance)
  print(paste("Results are identical:", identical_results))
  
  # Test 5: Return Model Object
  cat("Test 5: Return Model Object\n")
  result_model <- fs_lasso(x = fakeData[, -1], y = fakeData[, 1], return_model = TRUE)
  print(result_model$importance)
  print(class(result_model$model))
  
  # Test 6: Handling Missing Values
  cat("Test 6: Handling Missing Values\n")
  fakeData_with_na <- fakeData
  fakeData_with_na[1:10, 2] <- NA
  result_na <- fs_lasso(x = fakeData_with_na[, -1], y = fakeData_with_na[, 1])
  print(result_na$importance)
  
  # Test 7: Scalability with Sparse Matrix
  cat("Test 7: Scalability with Sparse Matrix\n")
  set.seed(123)
  large_n <- 10000
  large_p <- 100
  largeData <- data.frame(matrix(rnorm(large_n * large_p), ncol = large_p))
  largeY <- 2 * largeData[,1] - 3 * largeData[,2] + 1.5 * largeData[,3] + rnorm(large_n)
  result_sparse <- fs_lasso(x = largeData, y = largeY, verbose = TRUE)
  print(head(result_sparse$importance))
  
  # Test 8: Custom Cross-Validation Folds
  cat("Test 8: Custom Cross-Validation Folds\n")
  custom_folds <- sample(1:5, n, replace = TRUE)
  result_custom_folds <- fs_lasso(x = fakeData[, -1], y = fakeData[, 1], custom_folds = custom_folds)
  print(result_custom_folds$importance)
  
  # Test 9: Invalid Inputs
  cat("Test 9: Invalid Inputs\n")
  tryCatch({
    fs_lasso("not a data frame", y)
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  tryCatch({
    fs_lasso(x = fakeData[, -1], y = fakeData[, 1], alpha = -1)
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  tryCatch({
    fs_lasso(x = fakeData[, -1], y = fakeData[, 1], nfolds = 1)
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  tryCatch({
    fs_lasso(x = fakeData[, -1], y = "not numeric")
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  cat("UAT for fs_lasso completed.\n")
}
