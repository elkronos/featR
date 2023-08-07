# Load packages
library(Boruta)
library(caret)
library(data.table)
library(doParallel)

#' Feature Selection Using Boruta Algorithm
#'
#' Performs feature selection from a given dataset using the Boruta algorithm.
#' Optionally, you can limit the number of features and remove highly correlated variables.
#'
#' @param data A data frame or data table containing the dataset.
#' @param target_var The name of the target variable as a string.
#' @param seed An optional integer for random seed initialization.
#' @param doTrace Verbosity level of the Boruta algorithm (0 = silent, 1 = confirmed attributes, 2 = tentative attributes).
#' @param maxRuns Maximum number of iterations for the Boruta algorithm.
#' @param num_cores Optional integer to specify the number of cores for parallel processing.
#' @param cutoff_features Optional integer to limit the number of selected features.
#' @param cutoff_cor Numeric threshold for correlation coefficient. Features with higher correlation than this are removed.
#'
#' @return A list with two components: selected feature indices and the Boruta result object.
#'
#' @examples
#' # Load iris dataset
#' data(iris)
#'
#' # Apply the fs_boruta function
#' result <- fs_boruta(iris, "Species", doTrace = 0, num_cores = 2, cutoff_features = 10, cutoff_cor = 0.7)
#' 
#' # Display the selected features
#' print(result$selected_features)
#'
#' @importFrom Boruta Boruta getSelectedAttributes
#' @importFrom caret findCorrelation
#' @importFrom data.table as.data.table
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @export
fs_boruta <- function(data, target_var, seed = NULL, doTrace = 1, maxRuns = 250, num_cores = NULL, cutoff_features = NULL, cutoff_cor = 0.7) {
  
  # convert input data to data.table object
  dt <- as.data.table(data)
  
  # create a vector of the dependent variable
  y <- dt[[target_var]]
  
  # remove the target variable from the data.table
  dt <- dt[, -..target_var]
  
  # run the Boruta algorithm
  if (!is.null(num_cores)) {
    registerDoParallel(num_cores)
    on.exit(stopImplicitCluster())
  }
  boruta_obj <- Boruta(dt, y, doTrace = doTrace, maxRuns = maxRuns, seed = seed)
  
  # get the selected features
  selected_features <- getSelectedAttributes(boruta_obj, withTentative = FALSE)
  
  # get the correlations
  correlation_matrix <- cor(dt[, selected_features, with = FALSE])
  
  # get the indices of high correlations
  high_correlation_indices <- which(abs(correlation_matrix) > cutoff_cor, arr.ind = TRUE)
  
  # drop self-correlations
  high_correlation_indices <- high_correlation_indices[high_correlation_indices[,1] != high_correlation_indices[,2], ]
  
  # get the variables to drop
  to_drop <- findCorrelation(correlation_matrix, cutoff = cutoff_cor)
  
  # check pairs and decide which one to drop
  for(i in 1:nrow(high_correlation_indices)) {
    v1 <- high_correlation_indices[i, 1]
    v2 <- high_correlation_indices[i, 2]
    
    if(v1 %in% to_drop && v2 %in% to_drop) {
      imp <- getImp(boruta_obj, type = "importance")
      if(mean(imp[selected_features[v1], , drop = FALSE]) > mean(imp[selected_features[v2], , drop = FALSE])) {
        to_drop <- setdiff(to_drop, v1)
      } else {
        to_drop <- setdiff(to_drop, v2)
      }
    }
  }
  
  # drop the variables
  selected_features <- selected_features[-to_drop]
  
  return(list(selected_features = selected_features, boruta_obj = boruta_obj))
}