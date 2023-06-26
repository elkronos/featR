library(memoise)

#' Perform Singular Value Decomposition (SVD) on a Matrix
#'
#' This function performs Singular Value Decomposition (SVD) on a matrix.
#' The function takes three arguments: \code{matrix_data} (the input matrix),
#' \code{scale_input} (a logical indicating whether or not to scale the input matrix),
#' and \code{n_singular_values} (the number of singular values to keep).
#' If \code{scale_input} is set to \code{TRUE}, the input matrix is scaled.
#' If \code{n_singular_values} is set to an integer, only the first
#' \code{n_singular_values} singular values, left singular vectors, and right singular
#' vectors are returned.
#' @param matrix_data The input matrix to perform SVD on.
#' @param scale_input A logical indicating whether or not to scale the input matrix.
#' @param n_singular_values An integer indicating the number of singular values to keep.
#' @return A list containing the singular values, left singular vectors, and right
#' singular vectors of the input matrix.
#' @examples
#' # Generate a random matrix
#' set.seed(123)
#' matrix_data <- matrix(rnorm(9), nrow = 3)
#'
#' # Perform SVD using the fs_svd function with scaling and keeping only the first 2 singular values
#' result_list <- fs_svd(matrix_data, scale_input = TRUE, n_singular_values = 2)
#'
#' # Extract the singular values, left singular vectors, and right singular vectors from the result list
#' singular_values <- result_list[[1]]
#' left_singular_vectors <- result_list[[2]]
#' right_singular_vectors <- result_list[[3]]
#'
#' # Print the results
#' cat("Singular values: ", singular_values, "\n")
#' cat("Left singular vectors: \n")
#' print(left_singular_vectors)
#' cat("Right singular vectors: \n")
#' print(right_singular_vectors)
fs_svd <- memoise(function(matrix_data, scale_input = TRUE, n_singular_values = NULL) {
  #' Check if the input is a matrix, throw an error if it's not
  if (!is.matrix(matrix_data)) {
    stop("Input must be a matrix.")
  }
  #' Check if the input contains missing values, throw an error if it does
  if (any(is.na(matrix_data))) {
    stop("Input matrix contains missing values.")
  }
  #' Scale the input matrix if the `scale_input` argument is set to TRUE
  if (scale_input) {
    matrix_data <- scale(matrix_data, center = TRUE, scale = TRUE)
  }
  #' Compute the SVD of the input matrix
  svd_result <- svd(matrix_data)
  #' Extract the singular values, left singular vectors, and right singular vectors
  singular_values <- svd_result$d
  left_singular_vectors <- svd_result$u
  right_singular_vectors <- svd_result$v
  #' If the `n_singular_values` argument is not NULL, only keep the first n_singular_values
  if (!is.null(n_singular_values)) {
    if (!is.numeric(n_singular_values) || n_singular_values <= 0 || n_singular_values > min(dim(matrix_data))) {
      stop("n_singular_values must be a positive integer not exceeding the number of singular values in the matrix.")
    }
    singular_values <- singular_values[1:n_singular_values]
    left_singular_vectors <- left_singular_vectors[, 1:n_singular_values]
    right_singular_vectors <- right_singular_vectors[, 1:n_singular_values]
  }
  #' Create a list of the results and return it
  result_list <- list(singular_values, left_singular_vectors, right_singular_vectors)
  return(result_list)
})