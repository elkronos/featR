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
#' @return A list containing two elements: the correlation matrix and the
#'   names of the selected variables. If no variables meet the correlation threshold, the
#'   selected variables will be NULL.
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
  
  # Calculate the correlation matrix
  if (method == "pearson") {
    corr_matrix <- cor(data, method = "pearson")
  } else if (method == "spearman") {
    corr_matrix <- cor(data, method = "spearman")
  } else {
    stop("Invalid correlation method. Please specify 'pearson' or 'spearman'.")
  }
  
  # Find the absolute correlation values above the threshold
  # Include both lower and upper triangle of the matrix, but exclude diagonal
  high_corr_vars <- which(abs(corr_matrix) > threshold & row(corr_matrix) != col(corr_matrix), arr.ind = TRUE)
  
  # Remove duplicate correlations
  high_corr_vars <- high_corr_vars[high_corr_vars[,1] <= high_corr_vars[,2],]
  
  # Check if there are any selected variables
  if (nrow(high_corr_vars) == 0) {
    message("No variables meet the correlation threshold.")
    return(list(corr_matrix = as.data.frame(corr_matrix), selected_vars = NULL))
  }
  
  # Extract the selected variables from the data frame
  selected_vars <- unique(c(high_corr_vars[,1], high_corr_vars[,2]))
  selected_names <- colnames(data)[selected_vars]
  
  # Save the correlation matrix and selected variables as data frames in a list
  result <- list(corr_matrix = as.data.frame(corr_matrix), selected_vars = selected_names)
  return(result)
}