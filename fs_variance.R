#' Apply variance thresholding to a numeric dataset
#'
#' This function applies variance thresholding to a numeric dataset, removing features
#' whose variances are not above a certain threshold. The function fails with an appropriate
#' error message if the inputs are not as expected, and converts data frames to matrices
#' if necessary for calculations.
#'
#' @param data A numeric matrix or data frame containing the input data. All columns of
#'             data frames should be numeric.
#' @param threshold A non-negative numeric value specifying the variance threshold. Features
#'                  with variances above this threshold are kept in the dataset.
#'
#' @return A numeric matrix containing the thresholded data. If the data was originally a
#'         data frame, it will be returned as a matrix.
#'
#' @examples
#' # Create a sample dataset with 5 features and 10 observations
#' data <- matrix(rnorm(50), ncol = 5)
#' colnames(data) <- paste0("Feature", 1:5)
#'
#' # Apply variance thresholding with a threshold of 0.5
#' thresholded_data <- fs_variance(data, 0.5)
#'
#' # Check the dimensions of the thresholded dataset
#' dim(thresholded_data)
#'
#' @export
fs_variance <- function(data, threshold) {
  # Check inputs
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0) {
    stop("Threshold should be a non-negative numeric value.")
  }
  
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data should be a matrix or a data frame.")
  }
  
  if (is.data.frame(data)) {
    if (!all(sapply(data, is.numeric))) {
      stop("All columns of data frame should be numeric.")
    }
    data <- as.matrix(data)  # Convert to matrix for calculations
  }
  
  # Get the number of rows in the input data
  n <- nrow(data)
  
  # Calculate the variances for each feature using matrix multiplication
  variances <- apply(data, 2, var)
  
  # Remove features with variances below the threshold
  data <- data[, variances > threshold, drop = FALSE]
  
  return(data)
}