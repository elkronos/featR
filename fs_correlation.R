#' Correlation-based feature selection
#'
#' This function selects features from a data set based on their correlation with
#' other features. It returns a list containing the correlation matrix and the
#' names of the selected variables.
#'
#' @param data A data frame or matrix containing the data. The function will stop 
#'   if the 'data' is not a data frame or a matrix.
#' @param threshold The threshold above which to select features based on their
#'   correlation. Must be a value between 0 and 1. The function will stop if the 
#'   'threshold' is not in this range.
#' @param method The correlation method to use, either "pearson" or "spearman". 
#'   The function will stop if any other method is provided.
#'
#' @return A list containing two elements: 
#'   \itemize{
#'     \item \strong{corr_matrix}: The correlation matrix (as a matrix). 
#'     \item \strong{selected_vars}: Character vector containing the names of the selected variables. If no variables meet the correlation threshold, this will be NULL.
#'   }
#'
#' @examples
#' # Load the mtcars data set
#' data(mtcars)
#'
#' # Default is Pearson
#' corr_vars <- fs_correlation(mtcars, 0.7)
#' print(corr_vars$corr_matrix)
#' print(corr_vars$selected_vars)
#'
#' # Specify spearman
#' corr_vars <- fs_correlation(mtcars, 0.7, method = "spearman")
#' print(corr_vars$corr_matrix)
#' print(corr_vars$selected_vars)
#'
#' @export
fs_correlation <- function(data, threshold, method = "pearson") {
  
  # Check if data is a data frame or matrix
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("The 'data' argument must be a data frame or a matrix.")
  }
  
  # Check if threshold is between 0 and 1
  if (!(threshold >= 0 && threshold <= 1)) {
    stop("The 'threshold' argument must be a value between 0 and 1.")
  }
  
  # Validate method
  if (!(method %in% c("pearson", "spearman"))) {
    stop("Invalid correlation method. Please specify 'pearson' or 'spearman'.")
  }
  
  # Calculate the correlation matrix
  corr_matrix <- cor(data, method = method)
  
  # Set the diagonal of the matrix to 0
  diag(corr_matrix) <- 0
  
  # Find the absolute correlation values above the threshold using the upper triangle
  high_corr_vars <- which(abs(corr_matrix) > threshold & upper.tri(corr_matrix), arr.ind = TRUE)
  
  # Check if there are any selected variables
  if (nrow(high_corr_vars) == 0) {
    message("No variables meet the correlation threshold.")
    return(list(corr_matrix = corr_matrix, selected_vars = NULL))
  }
  
  # Extract the selected variables from the data frame
  selected_vars <- unique(c(high_corr_vars[,1], high_corr_vars[,2]))
  selected_names <- colnames(data)[selected_vars]
  
  # Save the correlation matrix and selected variables in a list
  result <- list(corr_matrix = corr_matrix, selected_vars = selected_names)
  return(result)
}