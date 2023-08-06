library(caret)

#' Recursive Feature Elimination with Cross-Validation
#'
#' This function performs Recursive Feature Elimination (RFE) on the provided dataset using the Random Forest algorithm with cross-validation. It returns the optimal number of features, their names, variable importance scores, and cross-validation results.
#'
#' @param data A dataframe containing the dataset to be used.
#' @param response_var_index The index of the response variable in the dataset.
#' @param seed An integer for reproducibility. Default is 123.
#' @param control_params A list containing control parameters for rfeControl. Available parameters include method (cross-validation method) and number (number of folds). Default is list(method = "cv", number = 5).
#'
#' @return A list containing:
#' \itemize{
#'  \item OptimalNumberOfVariables: An integer representing the optimal number of features.
#'  \item OptimalVariables: A character vector containing the names of the optimal features.
#'  \item VariableImportance: A dataframe containing the variable importance scores.
#'  \item ResamplingResults: A dataframe containing the cross-validation resampling results.
#' }
#'
#' @examples
#' \dontrun{
#' # Load the caret package
#' library(caret)
#'
#' # Generate a synthetic dataset with 100 samples and 10 linear variables
#' data <- caret::twoClassSim(100, linearVars = 10)
#'
#' # Apply the fs_recursivefeature function to the synthetic dataset
#' result <- fs_recursivefeature(data, ncol(data))
#'
#' # View the optimal number of variables
#' print(result$OptimalNumberOfVariables)
#'
#' # View the names of the optimal variables
#' print(result$OptimalVariables)
#'
#' # View the variable importance scores
#' print(result$VariableImportance)
#'
#' # View the cross-validation results
#' print(result$ResamplingResults)
#' }
#'
#' @importFrom caret rfeControl rfe rfFuncs createDataPartition
#' @export
fs_recursivefeature <- function(data, response_var_index, seed = 123, 
                                control_params = list(method = "cv", number = 5)) {
  # Check if the response_var_index is valid and an integer
  if (!is.integer(response_var_index) || response_var_index > ncol(data) || response_var_index < 1) {
    stop("Invalid response_var_index. It should be an integer between 1 and the number of columns in the dataset.")
  }
  
  # Split the data into training and testing sets
  set.seed(seed)
  trainIndex <- createDataPartition(data[, response_var_index], p = .8, list = FALSE)
  trainData <- data[trainIndex, ]
  
  # Recursive Feature Elimination (RFE) with cross-validation
  ctrl <- rfeControl(functions = rfFuncs, method = control_params$method, number = control_params$number)
  rfeProfile <- rfe(x = trainData[, -response_var_index], y = trainData[, response_var_index], 
                    sizes = c(1:(ncol(trainData)-1)), rfeControl = ctrl)
  
  # Extracting results
  optimal_num_vars <- rfeProfile$optSize
  optimal_vars <- rfeProfile$optVariables
  var_importance <- rfeProfile$variables
  
  # Return the results
  list(
    OptimalNumberOfVariables = optimal_num_vars,
    OptimalVariables = optimal_vars,
    VariableImportance = var_importance,
    ResamplingResults = rfeProfile$results
  )
}