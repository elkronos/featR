###############################################################################
# Testing Infrastructure - SVD
###############################################################################

#' Unit Acceptance Testing for fs_svd
#'
#' Runs a series of tests to ensure the fs_svd function works as expected.
#'
#' @examples
#' test_fs_svd()
test_fs_svd <- function() {
  cat("Running Unit Acceptance Tests (UAT) for fs_svd...\n")
  
  run_test <- function(test_name, expr) {
    cat("=== ", test_name, " ===\n")
    result <- tryCatch(expr, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      return(NULL)
    })
    if (!is.null(result)) {
      print(result)
      cat(test_name, "completed successfully.\n\n")
    }
  }
  
  set.seed(123)
  # Test 1: Default SVD with centering and scaling.
  run_test("Test 1: Default SVD", {
    matrix1 <- matrix(rnorm(20), nrow = 4)
    fs_svd(matrix1, verbose = TRUE)
  })
  
  # Test 2: Centering only with truncation.
  run_test("Test 2: Centering Only", {
    matrix2 <- matrix(rnorm(20), nrow = 4)
    fs_svd(matrix2, scale_input = "center", n_singular_values = 2, verbose = TRUE)
  })
  
  # Test 3: Scaling only with truncation.
  run_test("Test 3: Scaling Only", {
    matrix3 <- matrix(rnorm(20), nrow = 4)
    fs_svd(matrix3, scale_input = "scale", n_singular_values = 2, verbose = TRUE)
  })
  
  # Test 4: No scaling.
  run_test("Test 4: No Scaling", {
    matrix4 <- matrix(rnorm(20), nrow = 4)
    fs_svd(matrix4, scale_input = FALSE, n_singular_values = 2, verbose = TRUE)
  })
  
  # Test 5: Approximate SVD on a large matrix.
  run_test("Test 5: Approximate SVD", {
    large_matrix <- matrix(rnorm(150 * 120), nrow = 150)
    fs_svd(large_matrix, n_singular_values = 5, svd_method = "approx", verbose = TRUE)
  })
  
  # Test 6: Automatic method selection based on threshold.
  run_test("Test 6: Auto SVD with custom threshold", {
    matrix6 <- matrix(rnorm(150 * 120), nrow = 150)
    fs_svd(matrix6, n_singular_values = 5, svd_threshold = 50, verbose = TRUE)
  })
  
  # Test 7: Passing additional arguments to RSpectra::svds.
  run_test("Test 7: Approx SVD with approx_args", {
    matrix7 <- matrix(rnorm(150 * 120), nrow = 150)
    fs_svd(matrix7, n_singular_values = 5, svd_method = "approx", 
           approx_args = list(tol = 1e-3), verbose = TRUE)
  })
  
  # Test 8: Non-matrix input.
  run_test("Test 8: Non-matrix Input", {
    non_matrix <- data.frame(x = 1:5, y = 6:10)
    fs_svd(non_matrix, verbose = TRUE)
  })
  
  # Test 9: Matrix with missing values.
  run_test("Test 9: Matrix with NA", {
    matrix_na <- matrix(c(1, 2, NA, 4), nrow = 2)
    fs_svd(matrix_na, verbose = TRUE)
  })
  
  # Test 10: Invalid scale_input value.
  run_test("Test 10: Invalid scale_input", {
    matrix10 <- matrix(rnorm(20), nrow = 4)
    fs_svd(matrix10, scale_input = "invalid", verbose = TRUE)
  })
  
  # Test 11: Invalid n_singular_values.
  run_test("Test 11: Invalid n_singular_values", {
    matrix11 <- matrix(rnorm(20), nrow = 4)
    fs_svd(matrix11, n_singular_values = 10, verbose = TRUE)
  })
  
  cat("All UAT for fs_svd completed.\n")
}

## Uncomment the next line to run the unit tests.
# test_fs_svd()
