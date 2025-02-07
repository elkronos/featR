###############################################################################
# Testing Infrastructure - Lasso
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
