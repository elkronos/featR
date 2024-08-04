# Load necessary packages
library(data.table)
library(Boruta)
library(caret)
library(doParallel)
library(testthat)

#' @title Perform Feature Selection Using Boruta Algorithm
#'
#' @description
#' This function performs feature selection from a given dataset using the Boruta algorithm. Optionally, you can limit 
#' the number of features and remove highly correlated variables. The function handles parallel processing to speed up 
#' computation when multiple cores are available.
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
#' @return A list containing:
#' * `selected_features`: A character vector of the selected feature names.
#' * `boruta_obj`: The Boruta result object.
#'
#' @examples
#' \dontrun{
#' # Load iris dataset
#' data(iris)
#'
#' # Apply the fs_boruta function
#' result <- fs_boruta(iris, "Species", doTrace = 0, num_cores = 2, cutoff_features = 10, cutoff_cor = 0.7)
#'
#' # Display the selected features
#' print(result$selected_features)
#' }
#' @seealso 
#' \code{\link[Boruta]{Boruta}} for details on the Boruta algorithm.
#'
#' @importFrom Boruta Boruta getSelectedAttributes
#' @importFrom caret findCorrelation
#' @importFrom data.table as.data.table
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @export
fs_boruta <- function(data, target_var, seed = NULL, doTrace = 1, maxRuns = 250, num_cores = NULL, cutoff_features = NULL, cutoff_cor = 0.7) {
  # Check if the target variable exists in the data
  if (!(target_var %in% names(data))) {
    stop("The target variable is not found in the provided data frame.")
  }
  
  # convert input data to data.table object
  dt <- as.data.table(data)
  
  # create a vector of the dependent variable
  y <- dt[[target_var]]
  
  # remove the target variable from the data.table
  dt <- dt[, !target_var, with = FALSE]
  
  # run the Boruta algorithm
  if (!is.null(num_cores)) {
    registerDoParallel(num_cores)
    on.exit(stopImplicitCluster())
  }
  
  boruta_obj <- Boruta(x = as.matrix(dt), y = y, doTrace = doTrace, maxRuns = maxRuns, seed = seed)
  
  # get the selected features
  selected_features <- getSelectedAttributes(boruta_obj, withTentative = FALSE)
  
  if (length(selected_features) == 0) {
    return(list(selected_features = character(0), boruta_obj = boruta_obj))
  }
  
  # If only one feature is selected, skip correlation step
  if (length(selected_features) > 1) {
    # get the correlations among selected features
    selected_dt <- dt[, selected_features, with = FALSE]
    correlation_matrix <- cor(selected_dt)
    
    # get the indices of high correlations
    to_drop <- findCorrelation(correlation_matrix, cutoff = cutoff_cor)
    
    # drop the variables
    if (length(to_drop) > 0) {
      selected_features <- selected_features[-to_drop]
    }
  }
  
  # Optionally limit the number of selected features
  if (!is.null(cutoff_features) && length(selected_features) > cutoff_features) {
    selected_features <- head(selected_features, cutoff_features)
  }
  
  return(list(selected_features = selected_features, boruta_obj = boruta_obj))
}

# Define UAT for fs_boruta function
test_fs_boruta <- function() {
  cat("Running UAT for fs_boruta...\n")
  
  # Test 1: Simple numeric data frame
  df1 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(1:5, 100, replace = TRUE),
    target = sample(1:2, 100, replace = TRUE)
  )
  result1 <- fs_boruta(df1, "target")
  print(result1)
  expect_type(result1$selected_features, "character")
  expect_true(is.list(result1$boruta_obj))
  expect_true("finalDecision" %in% names(result1$boruta_obj))
  
  # Test 2: Data frame with categorical variables
  df2 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(c("yes", "no"), 100, replace = TRUE),
    target = sample(1:2, 100, replace = TRUE)
  )
  result2 <- fs_boruta(df2, "target")
  print(result2)
  expect_type(result2$selected_features, "character")
  expect_true(is.list(result2$boruta_obj))
  expect_true("finalDecision" %in% names(result2$boruta_obj))
  
  # Test 3: Data frame with date variables
  df3 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(c("yes", "no"), 100, replace = TRUE),
    C = seq(as.Date("2001/1/1"), by = "month", length.out = 100),
    target = sample(1:2, 100, replace = TRUE)
  )
  result3 <- fs_boruta(df3, "target")
  print(result3)
  expect_type(result3$selected_features, "character")
  expect_true(is.list(result3$boruta_obj))
  expect_true("finalDecision" %in% names(result3$boruta_obj))
  
  # Test 4: Error handling when target is missing
  df4 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(c("yes", "no"), 100, replace = TRUE)
  )
  expect_error(fs_boruta(df4, "target"), "The target variable is not found in the provided data frame.")
  
  cat("UAT for fs_boruta completed.\n")
}

# Run the UAT functions
test_fs_boruta()
