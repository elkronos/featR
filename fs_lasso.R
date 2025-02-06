#' Validate Input Parameters
#'
#' Checks the user-provided parameters for consistency and correctness.
#'
#' @param x A data frame or matrix of predictor variables.
#' @param y A numeric vector of response variables.
#' @param alpha A numeric value between 0 (exclusive) and 1 (inclusive) for the lasso penalty.
#' @param nfolds An integer greater than 1 specifying the number of folds for cross-validation.
#' @param standardize Logical; whether to standardize predictors.
#' @param parallel Logical; whether to use parallel processing.
#' @param verbose Logical; whether to print progress messages.
#' @param seed Either NULL or a single integer value for reproducibility.
#' @param custom_folds Optional integer vector of custom fold IDs (must be same length as y).
#' @param return_model Logical; whether to return the fitted model.
#'
#' @return Invisibly returns TRUE if all parameters are valid; otherwise, stops with an error.
#' @keywords internal
validate_parameters <- function(x, y, alpha, nfolds, standardize,
                                parallel, verbose, seed, custom_folds,
                                return_model) {
  # x must be a data frame or matrix
  if (!inherits(x, c("data.frame", "matrix"))) {
    stop("Error: 'x' should be a data frame or matrix.")
  }
  
  # Ensure x and y have the same number of observations
  if (nrow(x) != length(y)) {
    stop("Error: 'x' and 'y' must have the same number of rows.")
  }
  
  # y must be numeric
  if (!is.numeric(y)) {
    stop("Error: 'y' should be a numeric vector.")
  }
  
  # alpha must be a single numeric in (0, 1]
  if (!(is.numeric(alpha) && length(alpha) == 1 && alpha > 0 && alpha <= 1)) {
    stop("Error: 'alpha' must be a numeric value between 0 (exclusive) and 1 (inclusive).")
  }
  
  # nfolds must be an integer > 1
  if (!(is.numeric(nfolds) && nfolds > 1 && nfolds == as.integer(nfolds))) {
    stop("Error: 'nfolds' must be an integer greater than 1.")
  }
  
  # standardize, parallel, verbose, and return_model must be logical
  for (param in list(standardize = standardize,
                     parallel = parallel,
                     verbose = verbose,
                     return_model = return_model)) {
    if (!is.logical(param)) {
      stop("Error: 'standardize', 'parallel', 'verbose', and 'return_model' must be logical values.")
    }
  }
  
  # Seed should be either NULL or a single integer
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || seed != as.integer(seed))) {
    stop("Error: 'seed' must be a single integer value or NULL.")
  }
  
  # Validate custom_folds if provided
  if (!is.null(custom_folds)) {
    if (!(is.integer(custom_folds) || all(custom_folds == as.integer(custom_folds)))) {
      stop("Error: 'custom_folds' must be an integer vector.")
    }
    if (length(custom_folds) != length(y)) {
      stop("Error: 'custom_folds' must be the same length as 'y'.")
    }
    if (any(custom_folds < 1 | custom_folds > nfolds)) {
      stop("Error: 'custom_folds' contains invalid fold IDs (must be between 1 and nfolds).")
    }
  }
  
  return(invisible(TRUE))
}

#' Handle Missing Values in Predictor Data
#'
#' Imputes missing values in a numeric matrix or data frame using column means.
#'
#' @param x A numeric matrix or data frame.
#'
#' @return The data with missing values imputed.
#' @keywords internal
handle_missing_values <- function(x) {
  if (anyNA(x)) {
    # Use colMeans (ignoring NA) and then replace
    col_means <- colMeans(x, na.rm = TRUE)
    for (j in seq_along(col_means)) {
      missing_idx <- which(is.na(x[, j]))
      if (length(missing_idx)) {
        x[missing_idx, j] <- col_means[j]
      }
    }
  }
  return(x)
}

#' Convert Predictors to a Sparse Matrix
#'
#' Converts a matrix or data frame of predictors to a sparse matrix format.
#'
#' @param x A data frame or matrix of predictor variables.
#'
#' @return A sparse matrix (of class "CsparseMatrix").
#' @keywords internal
convert_to_sparse <- function(x) {
  # Ensure the input is a matrix then convert to a sparse format.
  as(as.matrix(x), "CsparseMatrix")
}

#' Manage Parallel Cluster Setup and Teardown
#'
#' Sets up a parallel processing cluster if enabled and registers the backend.
#'
#' @param enable_parallel Logical; whether to enable parallel processing.
#' @param verbose Logical; whether to print status messages.
#'
#' @return A list containing the cluster object (if any) and the number of cores used.
#' @keywords internal
manage_parallel_cluster <- function(enable_parallel, verbose) {
  cluster_info <- list(cluster = NULL, n_cores = 1)
  
  if (enable_parallel) {
    n_cores <- max(1, parallel::detectCores() - 1)
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    if (verbose) message("Parallel processing enabled using ", n_cores, " cores.")
    cluster_info$cluster <- cl
    cluster_info$n_cores <- n_cores
  } else {
    doParallel::registerDoSEQ()  # Ensures sequential processing
  }
  
  return(cluster_info)
}

#' Fit a Lasso Regression Model with Cross-Validation
#'
#' Fits a lasso model using the \code{cv.glmnet} function, optionally using custom folds.
#'
#' @param x A sparse matrix of predictor variables.
#' @param y A numeric vector of response variables.
#' @param alpha A numeric value between 0 (exclusive) and 1 (inclusive) for the lasso penalty.
#' @param nfolds An integer specifying the number of cross-validation folds.
#' @param standardize Logical; whether to standardize predictors.
#' @param parallel Logical; whether to use parallel processing.
#' @param custom_folds Optional integer vector of custom fold IDs.
#' @param seed Optional integer for setting the random seed.
#' @param verbose Logical; whether to print status messages.
#'
#' @return The fitted lasso model object from \code{cv.glmnet}.
#' @import glmnet
#' @keywords internal
fit_lasso_model <- function(x, y, alpha, nfolds, standardize,
                            parallel, custom_folds, seed, verbose) {
  if (!is.null(seed)) set.seed(seed)
  
  # Setup parallel cluster if needed
  cluster_info <- manage_parallel_cluster(parallel, verbose)
  
  # Fit the model using custom folds if provided
  if (is.null(custom_folds)) {
    lasso_model <- glmnet::cv.glmnet(x, y,
                                     alpha = alpha,
                                     nfolds = nfolds,
                                     standardize = standardize,
                                     parallel = parallel,
                                     keep = TRUE)
  } else {
    lasso_model <- glmnet::cv.glmnet(x, y,
                                     alpha = alpha,
                                     foldid = custom_folds,
                                     standardize = standardize,
                                     parallel = parallel,
                                     keep = TRUE)
  }
  
  # Stop the cluster if it was started
  if (parallel && !is.null(cluster_info$cluster)) {
    parallel::stopCluster(cluster_info$cluster)
    doParallel::registerDoSEQ()
  }
  
  return(lasso_model)
}

#' Extract Variable Importance from a Fitted Lasso Model
#'
#' Retrieves and orders the coefficients (excluding the intercept) as variable importance.
#'
#' @param lasso_model A fitted lasso model object from \code{cv.glmnet}.
#' @param x A sparse matrix of predictor variables (used for extracting predictor names).
#'
#' @return A data frame with variables and their corresponding importance scores.
#' @keywords internal
extract_importance <- function(lasso_model, x) {
  # Extract coefficients at the lambda that minimizes CV error.
  coefs <- as.vector(stats::coef(lasso_model, s = "lambda.min"))
  # Remove the intercept (first coefficient)
  var_coef <- coefs[-1]
  
  # Ensure the predictor names exist; if not, assign default names.
  predictor_names <- colnames(x)
  if (is.null(predictor_names)) {
    predictor_names <- paste0("V", seq_along(var_coef))
  }
  
  importance_df <- data.frame(
    Variable = predictor_names,
    Importance = var_coef,
    stringsAsFactors = FALSE
  )
  
  # Order by descending absolute importance
  importance_df <- importance_df[order(-abs(importance_df$Importance)), ]
  rownames(importance_df) <- NULL
  return(importance_df)
}

###############################################################################
# Main Function: fs_lasso
###############################################################################

#' Fit and Evaluate a Lasso Regression Model with Feature Selection
#'
#' This function preprocesses the input data, fits a lasso regression model
#' using cross-validation, extracts variable importance scores, and optionally
#' returns the fitted model.
#'
#' @param x A data frame or matrix of predictor variables.
#' @param y A numeric vector of response variables.
#' @param alpha A numeric value between 0 (exclusive) and 1 (inclusive) for the lasso penalty. Default is 1.
#' @param nfolds Number of folds for cross-validation. Must be an integer > 1. Default is 5.
#' @param standardize Logical indicating whether to standardize predictors. Default is TRUE.
#' @param parallel Logical indicating whether to use parallel processing. Default is TRUE.
#' @param verbose Logical indicating whether to print progress messages. Default is FALSE.
#' @param seed An optional integer seed for reproducibility. Default is NULL.
#' @param return_model Logical indicating whether to return the fitted model. Default is FALSE.
#' @param custom_folds Optional integer vector of custom fold IDs (same length as y). Default is NULL.
#'
#' @return A list with the following components:
#'   \item{importance}{A data frame of variable importance scores.}
#'   \item{model}{(Optional) The fitted lasso model (included if return_model is TRUE).}
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   n <- 100
#'   X <- matrix(rnorm(n * 5), ncol = 5)
#'   y <- 2 * X[,1] - 3 * X[,2] + 1.5 * X[,3] + rnorm(n)
#'   colnames(X) <- paste0("X", 1:5)
#'   result <- fs_lasso(x = X, y = y, verbose = TRUE, seed = 123)
#'   print(result$importance)
#' }
#' @export
fs_lasso <- function(x, y, alpha = 1, nfolds = 5, standardize = TRUE,
                     parallel = TRUE, verbose = FALSE, seed = NULL,
                     return_model = FALSE, custom_folds = NULL) {
  
  # Validate the input parameters
  validate_parameters(x, y, alpha, nfolds, standardize,
                      parallel, verbose, seed, custom_folds, return_model)
  
  # Handle missing data
  x <- handle_missing_values(x)
  
  # Ensure predictor names exist
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("V", seq_len(ncol(x)))
  }
  
  # Convert predictors to a sparse matrix for efficiency
  x_sparse <- convert_to_sparse(x)
  
  # Fit the lasso model using cross-validation
  lasso_model <- fit_lasso_model(x_sparse, y, alpha, nfolds, standardize,
                                 parallel, custom_folds, seed, verbose)
  
  # Extract variable importance scores
  importance_df <- extract_importance(lasso_model, x_sparse)
  
  # Return the results
  result <- list(importance = importance_df)
  if (return_model) {
    result$model <- lasso_model
  }
  
  return(result)
}

###############################################################################
# Test Function for fs_lasso
###############################################################################

#' Run Unit Tests for the fs_lasso Function
#'
#' This function runs several tests to ensure the functionality and robustness of \code{fs_lasso}.
#'
#' @examples
#' \dontrun{
#'   test_fs_lasso()
#' }
#' @export
test_fs_lasso <- function() {
  cat("Running unit tests for fs_lasso...\n")
  
  # Prepare sample data
  set.seed(123)
  n <- 100
  p <- 5
  X <- matrix(rnorm(n * p), ncol = p)
  y <- 2 * X[, 1] - 3 * X[, 2] + 1.5 * X[, 3] + rnorm(n)
  colnames(X) <- paste0("X", 1:p)
  data_df <- data.frame(X)
  
  # Test 1: Basic Functionality
  cat("Test 1: Basic functionality...\n")
  result1 <- fs_lasso(x = X, y = y)
  print(result1$importance)
  
  # Test 2: Verbose Option
  cat("Test 2: Verbose option...\n")
  result2 <- fs_lasso(x = X, y = y, verbose = TRUE)
  print(result2$importance)
  
  # Test 3: Parallel Processing Option
  cat("Test 3: Parallel processing...\n")
  result3 <- fs_lasso(x = X, y = y, parallel = TRUE, verbose = TRUE)
  print(result3$importance)
  
  # Test 4: Reproducibility with Seed
  cat("Test 4: Reproducibility with seed...\n")
  res_seed1 <- fs_lasso(x = X, y = y, seed = 123)
  res_seed2 <- fs_lasso(x = X, y = y, seed = 123)
  cat("Are results identical? ", identical(res_seed1$importance, res_seed2$importance), "\n")
  
  # Test 5: Return the Fitted Model Object
  cat("Test 5: Return model object...\n")
  result5 <- fs_lasso(x = X, y = y, return_model = TRUE)
  print(class(result5$model))
  
  # Test 6: Handling Missing Values
  cat("Test 6: Handling missing values...\n")
  X_na <- X
  X_na[1:10, 2] <- NA
  result6 <- fs_lasso(x = X_na, y = y)
  print(result6$importance)
  
  # Test 7: Custom Cross-Validation Folds
  cat("Test 7: Custom cross-validation folds...\n")
  custom_folds <- as.integer(sample(1:5, n, replace = TRUE))
  result7 <- fs_lasso(x = X, y = y, custom_folds = custom_folds)
  print(result7$importance)
  
  # Test 8: Invalid Input Handling
  cat("Test 8: Invalid inputs...\n")
  tryCatch({
    fs_lasso("invalid", y)
  }, error = function(e) {
    cat("Caught expected error: ", e$message, "\n")
  })
  
  tryCatch({
    fs_lasso(x = X, y = y, alpha = -0.5)
  }, error = function(e) {
    cat("Caught expected error: ", e$message, "\n")
  })
  
  tryCatch({
    fs_lasso(x = X, y = y, nfolds = 1)
  }, error = function(e) {
    cat("Caught expected error: ", e$message, "\n")
  })
  
  tryCatch({
    fs_lasso(x = X, y = "non-numeric")
  }, error = function(e) {
    cat("Caught expected error: ", e$message, "\n")
  })
  
  cat("All tests completed.\n")
}

# Example usage (uncomment to run):
# result <- fs_lasso(x = my_data[ , -1], y = my_data[ , 1], verbose = TRUE)
# print(result$importance)
