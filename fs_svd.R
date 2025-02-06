# Install RSpectra if needed:
# install.packages("RSpectra")

library(memoise)
library(RSpectra)

#' Validate the Input Matrix
#'
#' Ensures the input is a valid matrix without missing values.
#'
#' @param mat The input matrix.
#' @return Returns TRUE if valid; otherwise, stops with an error.
validate_matrix <- function(mat) {
  if (!is.matrix(mat)) {
    stop("Input must be a matrix.")
  }
  if (any(is.na(mat))) {
    stop("Input matrix contains missing values.")
  }
  return(TRUE)
}

#' Scale the Matrix
#'
#' Scales the input matrix based on the specified option.
#'
#' @param mat The input matrix.
#' @param scale_input Specifies the type of scaling:
#'   \itemize{
#'     \item \code{TRUE}: Centering and scaling.
#'     \item \code{"center"}: Centering only.
#'     \item \code{"scale"}: Scaling only.
#'     \item \code{FALSE}: No scaling.
#'   }
#' @param verbose Logical; if TRUE, prints a message about the scaling.
#' @return The scaled matrix.
scale_matrix <- function(mat, scale_input = TRUE, verbose = FALSE) {
  if (isTRUE(scale_input)) {
    if (verbose) cat("Applying centering and scaling...\n")
    mat <- scale(mat, center = TRUE, scale = TRUE)
  } else if (is.character(scale_input)) {
    if (scale_input == "center") {
      if (verbose) cat("Applying centering only...\n")
      mat <- scale(mat, center = TRUE, scale = FALSE)
    } else if (scale_input == "scale") {
      if (verbose) cat("Applying scaling only...\n")
      mat <- scale(mat, center = FALSE, scale = TRUE)
    } else {
      stop("Invalid scale_input value. Valid options are TRUE, FALSE, 'center', or 'scale'.")
    }
  } else if (isFALSE(scale_input)) {
    if (verbose) cat("No scaling applied.\n")
    ## Do nothing
  } else {
    stop("Invalid scale_input value. Valid options are TRUE, FALSE, 'center', or 'scale'.")
  }
  return(mat)
}

#' Truncate SVD Results
#'
#' Truncates the SVD output to the desired number of singular values/vectors.
#'
#' @param svd_result A list containing components \code{u}, \code{d}, and \code{v}.
#' @param n_keep The number of singular values/vectors to keep.
#' @return A list with truncated \code{singular_values}, \code{left_singular_vectors}, and \code{right_singular_vectors}.
truncate_svd <- function(svd_result, n_keep) {
  if (!is.numeric(n_keep) || n_keep <= 0 || n_keep %% 1 != 0) {
    stop("n_singular_values must be a positive integer.")
  }
  if (n_keep > length(svd_result$d)) {
    stop("n_singular_values exceeds the available singular values.")
  }
  
  return(list(
    singular_values = svd_result$d[1:n_keep],
    left_singular_vectors = svd_result$u[, 1:n_keep, drop = FALSE],
    right_singular_vectors = svd_result$v[, 1:n_keep, drop = FALSE]
  ))
}

#' Perform Singular Value Decomposition (SVD) on a Matrix Internally
#'
#' This internal function computes the SVD of a matrix with options for scaling,
#' truncating the number of components, and using an approximate SVD
#' algorithm for large matrices.
#'
#' @param matrix_data The input matrix.
#' @param scale_input Specifies the type of scaling; valid options are:
#'   \itemize{
#'     \item \code{TRUE} (default): Centering and scaling.
#'     \item \code{"center"}: Centering only.
#'     \item \code{"scale"}: Scaling only.
#'     \item \code{FALSE}: No scaling.
#'   }
#' @param n_singular_values An integer indicating the number of singular values to keep.
#'   Default is \code{min(dim(matrix_data))}.
#' @param svd_method SVD computation method. Options:
#'   \itemize{
#'     \item \code{"auto"} (default): Automatically choose between exact and approximate SVD.
#'     \item \code{"exact"}: Use base R's \code{svd} function.
#'     \item \code{"approx"}: Use \code{RSpectra::svds} for approximate SVD.
#'   }
#' @param svd_threshold A numeric value; if the smallest dimension of the matrix exceeds this threshold,
#'   approximate SVD will be considered when \code{svd_method} is "auto". Default is 100.
#' @param approx_args A list of additional arguments to pass to \code{RSpectra::svds} when using approximate SVD.
#' @param verbose Logical; if TRUE, prints informative messages during computation.
#' @return A named list containing:
#'   \itemize{
#'     \item \code{singular_values} - The singular values.
#'     \item \code{left_singular_vectors} - The left singular vectors.
#'     \item \code{right_singular_vectors} - The right singular vectors.
#'   }
perform_svd_internal <- function(matrix_data,
                                 scale_input = TRUE,
                                 n_singular_values = min(dim(matrix_data)),
                                 svd_method = c("auto", "exact", "approx"),
                                 svd_threshold = 100,
                                 approx_args = list(),
                                 verbose = FALSE) {
  # Validate the matrix input.
  validate_matrix(matrix_data)
  
  # Apply scaling.
  matrix_scaled <- scale_matrix(matrix_data, scale_input, verbose = verbose)
  
  # Decide on SVD method.
  svd_method <- match.arg(svd_method)
  n_keep <- n_singular_values
  
  if (svd_method == "auto") {
    if (min(dim(matrix_scaled)) > svd_threshold) {
      if (verbose) cat("Matrix dimensions exceed threshold; switching to approximate SVD.\n")
      svd_method <- "approx"
    } else {
      svd_method <- "exact"
    }
  }
  
  # Compute SVD based on chosen method.
  if (svd_method == "exact") {
    if (verbose) cat("Computing exact SVD using base::svd...\n")
    svd_result <- svd(matrix_scaled)
  } else if (svd_method == "approx") {
    if (n_keep >= min(dim(matrix_scaled))) {
      if (verbose) cat("Requested n_singular_values is too high for approximate SVD; falling back to exact SVD.\n")
      svd_result <- svd(matrix_scaled)
    } else {
      if (verbose) cat("Computing approximate SVD using RSpectra::svds...\n")
      # RSpectra::svds expects the matrix to be provided as 'A'
      args_list <- c(list(A = matrix_scaled, k = n_keep), approx_args)
      svd_result <- do.call(RSpectra::svds, args_list)
      
      # Ensure the singular values and vectors are sorted in decreasing order.
      ord <- order(svd_result$d, decreasing = TRUE)
      svd_result$u <- svd_result$u[, ord, drop = FALSE]
      svd_result$d <- svd_result$d[ord]
      svd_result$v <- svd_result$v[, ord, drop = FALSE]
      
      # Return the approximate result immediately.
      return(list(
        singular_values = svd_result$d,
        left_singular_vectors = svd_result$u,
        right_singular_vectors = svd_result$v
      ))
    }
  } else {
    stop("Unknown SVD method specified.")
  }
  
  # Truncate the SVD output if needed.
  return(truncate_svd(svd_result, n_keep))
}

#' Main Function: fs_svd
#'
#' This is the main SVD function that computes the singular value decomposition
#' of a matrix with options for scaling, truncation, and approximate computations.
#'
#' @param matrix_data The input matrix.
#' @param scale_input Specifies the type of scaling; valid options are:
#'   \itemize{
#'     \item \code{TRUE} (default): Centering and scaling.
#'     \item \code{"center"}: Centering only.
#'     \item \code{"scale"}: Scaling only.
#'     \item \code{FALSE}: No scaling.
#'   }
#' @param n_singular_values An integer indicating the number of singular values to keep.
#'   Default is \code{min(dim(matrix_data))}.
#' @param svd_method SVD computation method. Options:
#'   \itemize{
#'     \item \code{"auto"} (default): Automatically choose between exact and approximate SVD.
#'     \item \code{"exact"}: Use base R's \code{svd} function.
#'     \item \code{"approx"}: Use \code{RSpectra::svds} for approximate SVD.
#'   }
#' @param svd_threshold A numeric value; if the smallest dimension of the matrix exceeds this threshold,
#'   approximate SVD will be considered when \code{svd_method} is "auto". Default is 100.
#' @param approx_args A list of additional arguments to pass to \code{RSpectra::svds} when using approximate SVD.
#' @param verbose Logical; if TRUE, prints informative messages during computation.
#' @param memoise_result Logical; if TRUE (default), the result is memoised.
#' @return A named list containing:
#'   \itemize{
#'     \item \code{singular_values} - The singular values.
#'     \item \code{left_singular_vectors} - The left singular vectors.
#'     \item \code{right_singular_vectors} - The right singular vectors.
#'   }
#' @examples
#' set.seed(123)
#' matrix_data <- matrix(rnorm(9), nrow = 3)
#' result_list <- fs_svd(matrix_data, scale_input = TRUE, n_singular_values = 2)
#' cat("Singular values: ", result_list$singular_values, "\n")
#' print(result_list$left_singular_vectors)
#' print(result_list$right_singular_vectors)
fs_svd <- function(matrix_data,
                   scale_input = TRUE,
                   n_singular_values = min(dim(matrix_data)),
                   svd_method = c("auto", "exact", "approx"),
                   svd_threshold = 100,
                   approx_args = list(),
                   verbose = FALSE,
                   memoise_result = TRUE) {
  
  # Create a unique key based on the function arguments if needed.
  internal_fun <- perform_svd_internal
  if (memoise_result) {
    internal_fun <- memoise(perform_svd_internal)
  }
  
  # Call the (possibly memoised) internal function.
  internal_fun(matrix_data = matrix_data,
               scale_input = scale_input,
               n_singular_values = n_singular_values,
               svd_method = svd_method,
               svd_threshold = svd_threshold,
               approx_args = approx_args,
               verbose = verbose)
}

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
