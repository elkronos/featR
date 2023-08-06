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
      stop("Invalid scale_input value.")
    }
  }
  # Compute SVD
  svd_result <- svd(matrix_data)
  
  # Extract components
  singular_values <- svd_result$d
  left_singular_vectors <- svd_result$u
  right_singular_vectors <- svd_result$v
  
  # Keep only a specific number of singular values if specified
  if (!is.null(n_singular_values)) {
    if (!is.numeric(n_singular_values) || n_singular_values <= 0) {
      stop("n_singular_values must be a positive integer.")
    } else if (n_singular_values > min(dim(matrix_data))) {
      stop("n_singular_values exceeds the number of singular values in the matrix.")
    }
    singular_values <- singular_values[1:n_singular_values]
    left_singular_vectors <- left_singular_vectors[, 1:n_singular_values]
    right_singular_vectors <- right_singular_vectors[, 1:n_singular_values]
  }
  # Return named list of results
  result_list <- list(singular_values = singular_values, 
                      left_singular_vectors = left_singular_vectors, 
                      right_singular_vectors = right_singular_vectors)
  return(result_list)
})