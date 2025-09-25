#' @title Preprocess Predictors for Feature Selection
#'
#' @description
#' Prepare predictors by excluding the target, converting character to factor,
#' converting Date to numeric, and validating supported types.
#'
#' @param data A data frame containing the dataset.
#' @param target_var A string with the name of the target variable to be excluded.
#'
#' @return A data frame containing only processed predictor variables.
preprocess_predictors <- function(data, target_var) {
  # Exclude the target variable from predictors
  predictors <- data[, setdiff(names(data), target_var), drop = FALSE]
  
  # Validate supported variable types: numeric, factor, character, Date
  unsupported_vars <- names(predictors)[vapply(predictors, function(x) {
    is.matrix(x) || is.array(x) ||
      (!is.numeric(x) && !is.factor(x) && !is.character(x) && !inherits(x, "Date"))
  }, logical(1))]
  
  if (length(unsupported_vars) > 0) {
    stop(paste(
      "Unsupported variable types found in columns:",
      paste(unsupported_vars, collapse = ", ")
    ))
  }
  
  # Convert character variables to factors
  char_vars <- names(predictors)[vapply(predictors, is.character, logical(1))]
  if (length(char_vars) > 0) {
    predictors[char_vars] <- lapply(predictors[char_vars], as.factor)
  }
  
  # Convert Date variables to numeric (days since origin)
  date_vars <- names(predictors)[vapply(predictors, inherits, logical(1), what = "Date")]
  if (length(date_vars) > 0) {
    predictors[date_vars] <- lapply(predictors[date_vars], as.numeric)
  }
  
  # Final check: after processing we should have only numeric or factor
  remaining_unsupported <- names(predictors)[vapply(predictors, function(x) {
    !is.numeric(x) && !is.factor(x)
  }, logical(1))]
  if (length(remaining_unsupported) > 0) {
    stop(paste(
      "After processing, unsupported types remain in columns:",
      paste(remaining_unsupported, collapse = ", ")
    ))
  }
  
  predictors
}

#' @title Remove Highly Correlated Features
#'
#' @description
#' Remove features among `selected_features` if they are highly correlated
#' (absolute Pearson correlation) above `cutoff_cor`. Only numeric variables
#' are considered for correlation calculations.
#'
#' @param predictors A data frame of predictor variables.
#' @param selected_features Character vector of feature names to evaluate.
#' @param cutoff_cor Numeric threshold for absolute correlation (e.g., 0.7).
#'
#' @return A character vector of feature names after dropping highly correlated ones.
remove_highly_correlated <- function(predictors, selected_features, cutoff_cor = 0.7) {
  # If only one feature is selected, nothing to do
  if (length(selected_features) <= 1) return(selected_features)
  
  selected_data <- predictors[, selected_features, drop = FALSE]
  
  # Keep only numeric variables for correlation
  numeric_vars <- names(selected_data)[vapply(selected_data, is.numeric, logical(1))]
  if (length(numeric_vars) <= 1) return(selected_features)
  
  numeric_data <- selected_data[, numeric_vars, drop = FALSE]
  
  # Compute correlation matrix safely (pairwise complete obs)
  correlation_matrix <- stats::cor(numeric_data, use = "pairwise.complete.obs")
  
  # Identify indices to drop using caret's heuristic
  drop_idx <- caret::findCorrelation(correlation_matrix, cutoff = cutoff_cor, names = FALSE)
  
  if (length(drop_idx) > 0) {
    drop_features <- numeric_vars[drop_idx]
    selected_features <- setdiff(selected_features, drop_features)
  }
  
  selected_features
}

#' @title Perform Feature Selection Using Boruta
#'
#' @description
#' Run Boruta on a dataset. Preprocesses predictors, (optionally) sets RNG seed,
#' (optionally) resolves tentative features, and (optionally) removes highly
#' correlated features among the confirmed set. Returns selected feature names
#' and the Boruta object.
#'
#' @param data A data frame.
#' @param target_var Name of the target variable.
#' @param seed Optional integer for reproducibility (uses base \code{set.seed}).
#' @param doTrace Integer verbosity for Boruta (0 silent, 1 default, 2 more).
#' @param maxRuns Maximum number of Boruta iterations.
#' @param cutoff_features Optional integer to cap the number of returned features.
#' @param cutoff_cor Numeric correlation cutoff to drop redundant features (set \code{NULL} to skip).
#' @param resolve_tentative Logical; if TRUE, apply \code{Boruta::TentativeRoughFix}.
#'
#' @return A list with:
#' \item{selected_features}{Character vector of selected feature names.}
#' \item{boruta_obj}{The Boruta result object.}
#'
#' @export
fs_boruta <- function(data,
                      target_var,
                      seed = NULL,
                      doTrace = 1,
                      maxRuns = 250,
                      cutoff_features = NULL,
                      cutoff_cor = 0.7,
                      resolve_tentative = TRUE) {
  # Validate target presence
  if (!(target_var %in% names(data))) {
    stop("The target variable is not found in the provided data frame.")
  }
  
  # Prepare y and predictors
  y <- data[[target_var]]
  predictors <- preprocess_predictors(data, target_var)
  
  # Optional seed for reproducibility
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) stop("`seed` must be a single numeric value.")
    set.seed(as.integer(seed))
  }
  
  # Run Boruta (no `seed` arg supported)
  boruta_obj <- Boruta::Boruta(
    x = predictors,
    y = y,
    doTrace = doTrace,
    maxRuns = maxRuns
  )
  
  # Optionally resolve tentative features to confirmed/rejected
  if (isTRUE(resolve_tentative) && any(boruta_obj$finalDecision == "Tentative")) {
    boruta_obj <- Boruta::TentativeRoughFix(boruta_obj)
  }
  
  # Get confirmed selected attributes
  selected_features <- Boruta::getSelectedAttributes(boruta_obj, withTentative = FALSE)
  
  # Early return if none
  if (length(selected_features) == 0) {
    return(list(selected_features = character(0), boruta_obj = boruta_obj))
  }
  
  # Optionally remove highly correlated among the selected
  if (!is.null(cutoff_cor)) {
    selected_features <- remove_highly_correlated(predictors, selected_features, cutoff_cor = cutoff_cor)
  }
  
  # Optionally cap the number of features (keep first N in current order)
  if (!is.null(cutoff_features) && length(selected_features) > cutoff_features) {
    selected_features <- head(selected_features, cutoff_features)
  }
  
  list(selected_features = selected_features, boruta_obj = boruta_obj)
}
