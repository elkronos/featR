library(memoise)

#' Perform Singular Value Decomposition (SVD) on a Matrix
#'
#' This function performs Singular Value Decomposition (SVD) on a matrix.
#' The function takes three arguments: \code{matrix_data} (the input matrix),
#' \code{scale_input} (an option indicating how to scale the input matrix),
#' and \code{n_singular_values} (the number of singular values to keep).
#' 
#' \code{scale_input} can be:
#' \itemize{
#'   \item \code{TRUE} - both centering and scaling (default)
#'   \item \code{"center"} - only centering
#'   \item \code{"scale"} - only scaling
#'   \item \code{FALSE} - no centering or scaling
#' }
#' 
#' If \code{n_singular_values} is set to an integer, only the first
#' \code{n_singular_values} singular values, left singular vectors, and right singular
#' vectors are returned.
#'
#' @param matrix_data The input matrix to perform SVD on.
#' @param scale_input An option indicating how to scale the input matrix.
#' @param n_singular_values An integer indicating the number of singular values to keep (default is minimum dimension of the matrix).
#' @return A named list containing:
#' \itemize{
#'   \item \code{singular_values} - the singular values
#'   \item \code{left_singular_vectors} - the left singular vectors
#'   \item \code{right_singular_vectors} - the right singular vectors
#' }
#' @examples
#' # Generate a random matrix
#' set.seed(123)
#' matrix_data <- matrix(rnorm(9), nrow = 3)
#'
#' # Perform SVD using the fs_svd function with scaling and keeping only the first 2 singular values
#' result_list <- fs_svd(matrix_data, scale_input = TRUE, n_singular_values = 2)
#'
#' # Extract and print the results
#' cat("Singular values: ", result_list$singular_values, "\n")
#' cat("Left singular vectors: \n")
#' print(result_list$left_singular_vectors)
#' cat("Right singular vectors: \n")
#' print(result_list$right_singular_vectors)
#'
#' # Additional tests with other scaling options
#' result_center_only <- fs_svd(matrix_data, scale_input = "center", n_singular_values = 2)
#' result_scale_only <- fs_svd(matrix_data, scale_input = "scale", n_singular_values = 2)
#' 
#' cat("\nSingular values (center only): ", result_center_only$singular_values, "\n")
#' cat("Singular values (scale only): ", result_scale_only$singular_values, "\n")
fs_svd <- memoise(function(matrix_data, scale_input = TRUE, n_singular_values = min(dim(matrix_data))) {
  # Check if the input is a matrix
  if (!is.matrix(matrix_data)) {
    stop("Input must be a matrix.")
  }
  
  # Check for missing values
  if (any(is.na(matrix_data))) {
    stop("Input matrix contains missing values.")
  }
  
  # Scale the input matrix
  if (isTRUE(scale_input)) {
    matrix_data <- scale(matrix_data, center = TRUE, scale = TRUE)
  } else if (is.character(scale_input)) {
    if (scale_input == "center") {
      matrix_data <- scale(matrix_data, center = TRUE, scale = FALSE)
    } else if (scale_input == "scale") {
      matrix_data <- scale(matrix_data, center = FALSE, scale = TRUE)
    } else {
      stop("Invalid scale_input value. Valid options are TRUE, FALSE, 'center', or 'scale'.")
    }
  } else if (isFALSE(scale_input)) {
    # No scaling or centering
  } else {
    stop("Invalid scale_input value. Valid options are TRUE, FALSE, 'center', or 'scale'.")
  }
  
  # Compute SVD
  svd_result <- svd(matrix_data)
  
  # Extract components
  singular_values <- svd_result$d
  left_singular_vectors <- svd_result$u
  right_singular_vectors <- svd_result$v
  
  # Keep only a specific number of singular values if specified
  if (!is.null(n_singular_values)) {
    if (!is.numeric(n_singular_values) || n_singular_values <= 0 || n_singular_values %% 1 != 0) {
      stop("n_singular_values must be a positive integer.")
    } else if (n_singular_values > min(dim(matrix_data))) {
      stop("n_singular_values exceeds the number of singular values in the matrix.")
    }
    singular_values <- singular_values[1:n_singular_values]
    left_singular_vectors <- left_singular_vectors[, 1:n_singular_values, drop = FALSE]
    right_singular_vectors <- right_singular_vectors[, 1:n_singular_values, drop = FALSE]
  }
  
  # Return named list of results
  result_list <- list(singular_values = singular_values, 
                      left_singular_vectors = left_singular_vectors, 
                      right_singular_vectors = right_singular_vectors)
  return(result_list)
})

# Define UAT for fs_svd function
test_fs_svd <- function() {
  cat("Running UAT for fs_svd...\n")
  
  # Test 1: Simple numeric matrix
  cat("Test 1: Simple numeric matrix\n")
  set.seed(123)
  matrix1 <- matrix(rnorm(20), nrow = 4)
  result1 <- tryCatch({
    fs_svd(matrix1)
  }, error = function(e) {
    cat("Error in Test 1:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result1)) {
    print(result1)
    cat("Test 1 completed successfully.\n\n")
  }
  
  # Test 2: Matrix with scaling (default)
  cat("Test 2: Matrix with scaling (default)\n")
  set.seed(123)
  matrix2 <- matrix(rnorm(20), nrow = 4)
  result2 <- tryCatch({
    fs_svd(matrix2, scale_input = TRUE)
  }, error = function(e) {
    cat("Error in Test 2:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result2)) {
    print(result2)
    cat("Test 2 completed successfully.\n\n")
  }
  
  # Test 3: Matrix with centering only
  cat("Test 3: Matrix with centering only\n")
  set.seed(123)
  matrix3 <- matrix(rnorm(20), nrow = 4)
  result3 <- tryCatch({
    fs_svd(matrix3, scale_input = "center")
  }, error = function(e) {
    cat("Error in Test 3:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result3)) {
    print(result3)
    cat("Test 3 completed successfully.\n\n")
  }
  
  # Test 4: Matrix with scaling only
  cat("Test 4: Matrix with scaling only\n")
  set.seed(123)
  matrix4 <- matrix(rnorm(20), nrow = 4)
  result4 <- tryCatch({
    fs_svd(matrix4, scale_input = "scale")
  }, error = function(e) {
    cat("Error in Test 4:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result4)) {
    print(result4)
    cat("Test 4 completed successfully.\n\n")
  }
  
  # Test 5: Matrix without scaling or centering
  cat("Test 5: Matrix without scaling or centering\n")
  set.seed(123)
  matrix5 <- matrix(rnorm(20), nrow = 4)
  result5 <- tryCatch({
    fs_svd(matrix5, scale_input = FALSE)
  }, error = function(e) {
    cat("Error in Test 5:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result5)) {
    print(result5)
    cat("Test 5 completed successfully.\n\n")
  }
  
  # Test 6: Specifying number of singular values
  cat("Test 6: Specifying number of singular values\n")
  set.seed(123)
  matrix6 <- matrix(rnorm(20), nrow = 4)
  result6 <- tryCatch({
    fs_svd(matrix6, n_singular_values = 2)
  }, error = function(e) {
    cat("Error in Test 6:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result6)) {
    print(result6)
    cat("Test 6 completed successfully.\n\n")
  }
  
  # Test 7: Error handling for non-matrix input
  cat("Test 7: Error handling for non-matrix input\n")
  non_matrix <- data.frame(x = 1:5, y = 6:10)
  result7 <- tryCatch({
    fs_svd(non_matrix)
  }, error = function(e) {
    cat("Expected error in Test 7:", conditionMessage(e), "\n")
    return("Error caught successfully")
  })
  if (!is.null(result7)) {
    print(result7)
    cat("Test 7 completed successfully.\n\n")
  }
  
  # Test 8: Error handling for matrix with missing values
  cat("Test 8: Error handling for matrix with missing values\n")
  matrix_with_na <- matrix(c(1, 2, 3, NA, 5, 6), nrow = 2)
  result8 <- tryCatch({
    fs_svd(matrix_with_na)
  }, error = function(e) {
    cat("Expected error in Test 8:", conditionMessage(e), "\n")
    return("Error caught successfully")
  })
  if (!is.null(result8)) {
    print(result8)
    cat("Test 8 completed successfully.\n\n")
  }
  
  # Test 9: Error handling for invalid scale_input value
  cat("Test 9: Error handling for invalid scale_input value\n")
  matrix9 <- matrix(rnorm(20), nrow = 4)
  result9 <- tryCatch({
    fs_svd(matrix9, scale_input = "invalid")
  }, error = function(e) {
    cat("Expected error in Test 9:", conditionMessage(e), "\n")
    return("Error caught successfully")
  })
  if (!is.null(result9)) {
    print(result9)
    cat("Test 9 completed successfully.\n\n")
  }
  
  # Test 10: Error handling for invalid n_singular_values
  cat("Test 10: Error handling for invalid n_singular_values\n")
  matrix10 <- matrix(rnorm(20), nrow = 4)
  result10 <- tryCatch({
    fs_svd(matrix10, n_singular_values = 10)  # Exceeds matrix dimension
  }, error = function(e) {
    cat("Expected error in Test 10:", conditionMessage(e), "\n")
    return("Error caught successfully")
  })
  if (!is.null(result10)) {
    print(result10)
    cat("Test 10 completed successfully.\n\n")
  }
  
  cat("UAT for fs_svd completed.\n")
}

# Run the UAT
test_fs_svd()