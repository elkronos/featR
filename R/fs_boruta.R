#' @title Preprocess Predictors for Feature Selection
#'
#' @description
#' This helper function prepares the predictor variables for feature selection by removing the target variable,
#' converting character variables to factors, converting date variables to numeric values, and validating that
#' only supported variable types (numeric, factor, character, and Date) are present.
#'
#' @param data A data frame containing the dataset.
#' @param target_var A string with the name of the target variable to be excluded from predictors.
#'
#' @return A data frame containing only the processed predictor variables.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     A = 1:10,
#'     B = letters[1:10],
#'     C = seq(as.Date("2000-01-01"), by = "day", length.out = 10),
#'     target = rnorm(10)
#'   )
#'   predictors <- preprocess_predictors(df, "target")
#' }
preprocess_predictors <- function(data, target_var) {
  # Exclude the target variable from predictors
  predictors <- data[, setdiff(names(data), target_var), drop = FALSE]
  
  # Validate supported variable types: allow numeric, factor, character, and Date
  unsupported_vars <- names(predictors)[sapply(predictors, function(x) {
    is.matrix(x) || is.array(x) ||
      (!is.numeric(x) && !is.factor(x) && !is.character(x) && !inherits(x, "Date"))
  })]
  if (length(unsupported_vars) > 0) {
    stop(paste("Unsupported variable types found in the following columns:",
               paste(unsupported_vars, collapse = ", ")))
  }
  
  # Convert character variables to factors
  char_vars <- names(predictors)[sapply(predictors, is.character)]
  if (length(char_vars) > 0) {
    predictors[char_vars] <- lapply(predictors[char_vars], as.factor)
  }
  
  # Convert Date variables to numeric representation
  date_vars <- names(predictors)[sapply(predictors, inherits, "Date")]
  if (length(date_vars) > 0) {
    predictors[date_vars] <- lapply(predictors[date_vars], as.numeric)
  }
  
  # Final check for any remaining unsupported variable types
  remaining_unsupported <- names(predictors)[sapply(predictors, function(x) {
    !is.numeric(x) && !is.factor(x)
  })]
  if (length(remaining_unsupported) > 0) {
    stop(paste("After processing, unsupported variable types found in the following columns:",
               paste(remaining_unsupported, collapse = ", ")))
  }
  
  return(predictors)
}

#' @title Remove Highly Correlated Features
#'
#' @description
#' This helper function removes features from a set of selected predictors if they are highly correlated beyond a
#' specified cutoff. Only numeric variables are considered for correlation calculations.
#'
#' @param predictors A data frame of predictor variables.
#' @param selected_features A character vector of feature names selected by the feature selection algorithm.
#' @param cutoff_cor A numeric threshold for the correlation coefficient. Features with correlations higher than this
#'   threshold are removed.
#'
#' @return A character vector of feature names after removing highly correlated variables.
#'
#' @examples
#' \dontrun{
#'   # Suppose predictors contains features A, B, C, and D
#'   selected_features <- c("A", "B", "C", "D")
#'   final_features <- remove_highly_correlated(predictors, selected_features, cutoff_cor = 0.8)
#' }
remove_highly_correlated <- function(predictors, selected_features, cutoff_cor) {
  # If only one feature is selected, nothing to remove
  if (length(selected_features) <= 1) {
    return(selected_features)
  }
  
  selected_data <- predictors[, selected_features, drop = FALSE]
  # Filter numeric variables only
  numeric_vars <- names(selected_data)[sapply(selected_data, is.numeric)]
  if (length(numeric_vars) <= 1) {
    return(selected_features)
  }
  
  numeric_data <- selected_data[, numeric_vars, drop = FALSE]
  correlation_matrix <- cor(numeric_data)
  # Find indices of features to drop based on the cutoff
  drop_idx <- caret::findCorrelation(correlation_matrix, cutoff = cutoff_cor)
  if (length(drop_idx) > 0) {
    drop_features <- numeric_vars[drop_idx]
    selected_features <- setdiff(selected_features, drop_features)
  }
  return(selected_features)
}

#' @title Perform Feature Selection Using Boruta Algorithm
#'
#' @description
#' This function performs feature selection on a given dataset using the Boruta algorithm. It preprocesses the data,
#' optionally runs in parallel, and can remove highly correlated features. The function returns the selected features
#' and the full Boruta object for further inspection.
#'
#' @param data A data frame containing the dataset.
#' @param target_var A string with the name of the target variable.
#' @param seed Optional integer for random seed initialization.
#' @param doTrace Integer value indicating the verbosity level for the Boruta algorithm (0 = silent, 1 = default,
#'   2 = more detailed).
#' @param maxRuns Maximum number of iterations for the Boruta algorithm.
#' @param num_cores Optional integer specifying the number of cores for parallel processing. If set to a number greater
#'   than 1, parallel processing will be enabled.
#' @param cutoff_features Optional integer to limit the number of selected features.
#' @param cutoff_cor Numeric threshold for the correlation coefficient. Features with correlation higher than this value
#'   will be removed.
#'
#' @return A list containing:
#' \item{selected_features}{A character vector of the selected feature names.}
#' \item{boruta_obj}{The Boruta result object containing details of the feature selection process.}
#'
#' @examples
#' \dontrun{
#'   data(iris)
#'   result <- fs_boruta(iris, "Species", doTrace = 0, num_cores = 2, cutoff_features = 10, cutoff_cor = 0.7)
#'   print(result$selected_features)
#' }
#'
#' @export
fs_boruta <- function(data, target_var, seed = NULL, doTrace = 1, maxRuns = 250,
                      num_cores = NULL, cutoff_features = NULL, cutoff_cor = 0.7) {
  # Validate that the target variable exists in the data
  if (!(target_var %in% names(data))) {
    stop("The target variable is not found in the provided data frame.")
  }
  
  # Prepare the dependent variable and predictors
  y <- data[[target_var]]
  predictors <- preprocess_predictors(data, target_var)
  
  # Setup parallel processing if requested
  if (!is.null(num_cores) && is.numeric(num_cores) && num_cores > 1) {
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      parallel::stopCluster(cl)
      doParallel::registerDoSEQ()
    }, add = TRUE)
  }
  
  # Run the Boruta feature selection algorithm using explicit namespace calls
  boruta_obj <- Boruta::Boruta(x = predictors, y = y, doTrace = doTrace, maxRuns = maxRuns, seed = seed)
  selected_features <- Boruta::getSelectedAttributes(boruta_obj, withTentative = FALSE)
  
  # Return early if no features were selected
  if (length(selected_features) == 0) {
    return(list(selected_features = character(0), boruta_obj = boruta_obj))
  }
  
  # Remove highly correlated features among the selected ones
  selected_features <- remove_highly_correlated(predictors, selected_features, cutoff_cor)
  
  # Optionally limit the number of features returned
  if (!is.null(cutoff_features) && length(selected_features) > cutoff_features) {
    selected_features <- head(selected_features, cutoff_features)
  }
  
  return(list(selected_features = selected_features, boruta_obj = boruta_obj))
}
