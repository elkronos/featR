library(caret)

#' Recursive Feature Elimination
#' 
#' This function loads the specified dataset, splits it into training and testing sets, and performs Recursive Feature Elimination (RFE) on the training set using the Random Forest algorithm. The function returns the variable importance scores computed from the RFE analysis.
#' 
#' @importFrom caret createDataPartition rfeControl rfe rfFuncs
#' 
#' @param data_name A character string specifying the name of the dataset to be used. The dataset must be available in the R environment.
#' @param response_var_index The index of the response variable in the dataset. It must be an integer between 1 and the number of columns in the dataset.
#'
#' @return A data frame containing the variable importance scores computed from the RFE analysis.
#' 
#' @examples
#' # Generate a synthetic dataset
#' data <- caret::twoClassSim(100, linearVars = 10)
#' assign("synth_data", data, envir = .GlobalEnv)
#'
#' # Apply the function to the synthetic dataset
#' synth_imp <- fs_recrusivefeature("synth_data", ncol(synth_data))
#'
#' @export
fs_recrusivefeature <- function(data_name, response_var_index) {
  # Check if the data_name is available in R's environment
  if (!exists(data_name)){
    stop(paste0("Dataset ", data_name, " does not exist in R's environment"))
  }
  
  # Load a dataset
  data <- get(data_name)
  
  # Check if the response_var_index is valid
  if (response_var_index > ncol(data) | response_var_index < 1){
    stop("Invalid response_var_index. It should be an integer between 1 and the number of columns in the dataset.")
  }
  
  # Split the data into training and testing sets
  set.seed(123)
  trainIndex <- createDataPartition(data[,response_var_index], p = .8, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  
  # Recursive Feature Elimination (RFE)
  ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
  rfeProfile <- rfe(x = trainData[, -response_var_index], y = trainData[, response_var_index], sizes = c(1:(ncol(trainData)-1)),
                    rfeControl = ctrl)
  
  # Get the variable importance scores
  rfeImp <- rfeProfile$variables
  
  # Return the variable importance scores
  return(rfeImp)
}