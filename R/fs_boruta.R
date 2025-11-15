#' @title Preprocess Predictors for Feature Selection
#'
#' @description
#' Prepare predictors by excluding the target, coercing to a base data.frame,
#' converting character and logical variables to factor, converting Date/POSIXt
#' to numeric, and validating supported types. After processing, only numeric
#' and factor predictors are returned.
#'
#' @param data A data frame (or data-frame-like object) containing the dataset.
#' @param target_var A string with the name of the target variable to be excluded.
#'
#' @return A data frame containing only processed predictor variables.
preprocess_predictors <- function(data, target_var) {
  # Coerce to base data.frame to avoid surprises with tibbles / data.table
  data <- as.data.frame(data)
  
  # Basic validation of target_var (do not require it to be present here)
  if (!is.character(target_var) || length(target_var) != 1L) {
    stop("`target_var` must be a single string.")
  }
  
  # Exclude the target variable from predictors (if present)
  predictors <- data[, setdiff(names(data), target_var), drop = FALSE]
  
  # Validate supported variable types before transformation:
  # - numeric
  # - factor
  # - character
  # - logical
  # - Date
  # - POSIXt (e.g. POSIXct/POSIXlt)
  unsupported_vars <- names(predictors)[vapply(
    predictors,
    function(x) {
      is.matrix(x) || is.array(x) ||
        (!is.numeric(x) &&
           !is.factor(x) &&
           !is.character(x) &&
           !is.logical(x) &&
           !inherits(x, "Date") &&
           !inherits(x, "POSIXt"))
    },
    logical(1L)
  )]
  
  if (length(unsupported_vars) > 0) {
    stop(
      "Unsupported variable types found in columns: ",
      paste(unsupported_vars, collapse = ", ")
    )
  }
  
  # Convert logical variables to factors (TRUE/FALSE as levels)
  logical_vars <- names(predictors)[vapply(predictors, is.logical, logical(1L))]
  if (length(logical_vars) > 0) {
    predictors[logical_vars] <- lapply(predictors[logical_vars], as.factor)
  }
  
  # Convert character variables to factors
  char_vars <- names(predictors)[vapply(predictors, is.character, logical(1L))]
  if (length(char_vars) > 0) {
    predictors[char_vars] <- lapply(predictors[char_vars], as.factor)
  }
  
  # Convert Date variables to numeric (days since origin)
  date_vars <- names(predictors)[vapply(predictors, inherits, logical(1L), what = "Date")]
  if (length(date_vars) > 0) {
    predictors[date_vars] <- lapply(predictors[date_vars], as.numeric)
  }
  
  # Convert POSIXt variables (e.g. POSIXct/POSIXlt) to numeric (seconds since origin)
  posix_vars <- names(predictors)[vapply(predictors, inherits, logical(1L), what = "POSIXt")]
  if (length(posix_vars) > 0) {
    predictors[posix_vars] <- lapply(predictors[posix_vars], as.numeric)
  }
  
  # Final check: after processing we should have only numeric or factor
  remaining_unsupported <- names(predictors)[vapply(
    predictors,
    function(x) !is.numeric(x) && !is.factor(x),
    logical(1L)
  )]
  
  if (length(remaining_unsupported) > 0) {
    stop(
      "After processing, unsupported types remain in columns: ",
      paste(remaining_unsupported, collapse = ", ")
    )
  }
  
  predictors
}

#' @title Remove Highly Correlated Features
#'
#' @description
#' Remove features among \code{selected_features} if they are highly correlated
#' (absolute Pearson correlation) above \code{cutoff_cor}. Only numeric variables
#' are considered for correlation calculations.
#'
#' @param predictors A data frame of predictor variables.
#' @param selected_features Character vector of feature names to evaluate.
#' @param cutoff_cor Numeric threshold for absolute correlation in [0, 1]
#'   (e.g., 0.7).
#'
#' @return A character vector of feature names after dropping highly correlated ones.
remove_highly_correlated <- function(predictors,
                                     selected_features,
                                     cutoff_cor = 0.7) {
  # Coerce to base data.frame
  predictors <- as.data.frame(predictors)
  
  # Validate cutoff_cor
  if (!is.numeric(cutoff_cor) ||
      length(cutoff_cor) != 1L ||
      !is.finite(cutoff_cor) ||
      cutoff_cor < 0 || cutoff_cor > 1) {
    stop("`cutoff_cor` must be a single finite numeric value in [0, 1].")
  }
  
  # If only one feature is selected, nothing to do
  if (length(selected_features) <= 1L) {
    return(selected_features)
  }
  
  # Validate that selected_features exist in predictors
  missing <- setdiff(selected_features, names(predictors))
  if (length(missing) > 0L) {
    stop(
      "`selected_features` not found in `predictors`: ",
      paste(missing, collapse = ", ")
    )
  }
  
  selected_data <- predictors[, selected_features, drop = FALSE]
  
  # Keep only numeric variables for correlation
  numeric_vars <- names(selected_data)[vapply(selected_data, is.numeric, logical(1L))]
  if (length(numeric_vars) <= 1L) {
    return(selected_features)
  }
  
  numeric_data <- selected_data[, numeric_vars, drop = FALSE]
  
  # Compute correlation matrix safely (pairwise complete obs)
  correlation_matrix <- stats::cor(numeric_data, use = "pairwise.complete.obs")
  
  # Handle potential NA correlations (e.g. due to constant or all-NA columns)
  if (anyNA(correlation_matrix)) {
    correlation_matrix[is.na(correlation_matrix)] <- 0
  }
  
  # Identify indices to drop using caret's heuristic
  drop_idx <- caret::findCorrelation(correlation_matrix,
                                     cutoff = cutoff_cor,
                                     names = FALSE)
  
  if (length(drop_idx) > 0L) {
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
#' correlated features among the selected set. Returns selected feature names
#' and the Boruta object.
#'
#' @param data A data frame (or data-frame-like object).
#' @param target_var Name of the target variable.
#' @param seed Optional integer for reproducibility (uses base \code{set.seed}).
#' @param doTrace Integer verbosity for Boruta (0 silent, 1 default, 2 more).
#' @param maxRuns Maximum number of Boruta iterations.
#' @param cutoff_features Optional integer to cap the number of returned features.
#'   When specified, the top features by Boruta importance are retained.
#' @param cutoff_cor Numeric correlation cutoff in [0, 1] to drop redundant
#'   features (set \code{NULL} to skip).
#' @param resolve_tentative Logical; if \code{TRUE}, apply
#'   \code{Boruta::TentativeRoughFix} and return only confirmed attributes.
#'   If \code{FALSE}, tentative attributes are included in the selected set.
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
  # Coerce to base data.frame
  data <- as.data.frame(data)
  
  # Validate target_var
  if (!is.character(target_var) || length(target_var) != 1L) {
    stop("`target_var` must be a single string naming the target column.")
  }
  if (!(target_var %in% names(data))) {
    stop("The target variable is not found in the provided data frame.")
  }
  
  # Validate and normalize cutoff_features (if provided)
  if (!is.null(cutoff_features)) {
    if (!is.numeric(cutoff_features) ||
        length(cutoff_features) != 1L ||
        !is.finite(cutoff_features) ||
        cutoff_features <= 0) {
      stop("`cutoff_features` must be a single positive integer.")
    }
    cutoff_features <- as.integer(cutoff_features)
  }
  
  # Validate cutoff_cor (if not NULL)
  if (!is.null(cutoff_cor)) {
    if (!is.numeric(cutoff_cor) ||
        length(cutoff_cor) != 1L ||
        !is.finite(cutoff_cor) ||
        cutoff_cor < 0 || cutoff_cor > 1) {
      stop("`cutoff_cor` must be a single finite numeric value in [0, 1].")
    }
  }
  
  # Optional seed for reproducibility (with optional restoration of old seed)
  if (!is.null(seed)) {
    if (!is.numeric(seed) ||
        length(seed) != 1L ||
        !is.finite(seed)) {
      stop("`seed` must be a single finite numeric value.")
    }
    
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    }
    
    set.seed(as.integer(seed))
  }
  
  # Prepare y (target)
  y <- data[[target_var]]
  
  # Validate target type
  if (!is.factor(y) && !is.numeric(y)) {
    stop("`target_var` must be numeric (regression) or factor (classification).")
  }
  if (anyNA(y)) {
    stop("`target_var` contains missing values; please impute or remove them before calling `fs_boruta()`.")
  }
  
  # Prepare predictors
  predictors <- preprocess_predictors(data, target_var)
  
  # Enforce NA-free predictors for Boruta/randomForest
  if (anyNA(predictors)) {
    stop("Predictors contain missing values; please impute or remove them before calling `fs_boruta()`.")
  }
  
  # Run Boruta (no `seed` argument supported by Boruta itself)
  boruta_obj <- Boruta::Boruta(
    x = predictors,
    y = y,
    doTrace = doTrace,
    maxRuns = maxRuns
  )
  
  # Optionally resolve tentative features to confirmed/rejected
  if (isTRUE(resolve_tentative) &&
      any(boruta_obj$finalDecision == "Tentative")) {
    boruta_obj <- Boruta::TentativeRoughFix(boruta_obj)
  }
  
  # Get selected attributes:
  # - if resolve_tentative = TRUE: only confirmed (withTentative = FALSE)
  # - if resolve_tentative = FALSE: confirmed + tentative (withTentative = TRUE)
  selected_features <- Boruta::getSelectedAttributes(
    boruta_obj,
    withTentative = !isTRUE(resolve_tentative)
  )
  
  # Early return if none
  if (length(selected_features) == 0L) {
    return(list(selected_features = character(0L), boruta_obj = boruta_obj))
  }
  
  # Optionally remove highly correlated among the selected
  if (!is.null(cutoff_cor)) {
    selected_features <- remove_highly_correlated(
      predictors = predictors,
      selected_features = selected_features,
      cutoff_cor = cutoff_cor
    )
  }
  
  # Optionally cap the number of features (keep top N by Boruta importance)
  if (!is.null(cutoff_features) &&
      length(selected_features) > cutoff_features) {
    stats <- Boruta::attStats(boruta_obj)
    # Keep only rows corresponding to currently selected features
    stats <- stats[rownames(stats) %in% selected_features, , drop = FALSE]
    
    if (!"meanImp" %in% colnames(stats)) {
      stop("`Boruta::attStats` does not provide `meanImp`; cannot rank features by importance.")
    }
    
    # Order by mean importance (descending)
    stats <- stats[order(stats[["meanImp"]], decreasing = TRUE), , drop = FALSE]
    selected_features <- rownames(stats)[seq_len(cutoff_features)]
  }
  
  list(selected_features = selected_features, boruta_obj = boruta_obj)
}

