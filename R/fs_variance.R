#' Ensure a Package is Installed and Loaded
#'
#' Checks if a package is installed; if not, installs the package, then loads it.
#'
#' @param pkg A character string specifying the package name.
#' @return Invisibly returns TRUE if the package was loaded successfully.
ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Package '%s' not found. Installing...", pkg))
    install.packages(pkg, dependencies = TRUE)
  }
  # Try loading the package and check if successful
  if (!suppressPackageStartupMessages(require(pkg, character.only = TRUE, quietly = TRUE))) {
    stop(sprintf("Package '%s' failed to load.", pkg))
  }
  invisible(TRUE)
}

#' Convert Input Data to data.table
#'
#' Converts a numeric matrix, data frame, or data.table into a data.table for efficient processing.
#'
#' @param data A numeric matrix, data frame, or data.table.
#' @param log_progress Logical indicating whether progress messages should be printed.
#' @return A data.table version of the input data.
convert_to_datatable <- function(data, log_progress = FALSE) {
  ensure_package("data.table")
  
  # If already a data.table, just check for numeric columns and return
  if (data.table::is.data.table(data)) {
    if (!all(vapply(data, is.numeric, logical(1)))) {
      stop("All columns of the data.table must be numeric.")
    }
    if (log_progress) message("Input is already a data.table.")
    return(data)
  }
  
  # Handle data.frame (but not data.table, since that's already handled above)
  if (is.data.frame(data)) {
    if (!all(vapply(data, is.numeric, logical(1)))) {
      stop("All columns of the data frame must be numeric.")
    }
    if (log_progress) message("Converting data frame to data.table...")
    dt <- data.table::as.data.table(data)
  } else if (is.matrix(data)) {
    if (!is.numeric(data)) {
      stop("Matrix data must be numeric.")
    }
    if (log_progress) message("Converting matrix to data.table...")
    # Converting a matrix directly to data.table can lead to unexpected behavior;
    # hence converting via data.frame to preserve column structure.
    dt <- data.table::as.data.table(as.data.frame(data))
  } else {
    stop("Data must be a numeric matrix, data frame, or data.table.")
  }
  
  dt
}

#' Compute Feature Variances
#'
#' Efficiently computes the variance for each feature (column) in a data.table.
#'
#' @param dt A data.table containing numeric columns.
#' @param log_progress Logical indicating whether progress messages should be printed.
#' @return A named numeric vector of variances.
compute_feature_variances <- function(dt, log_progress = FALSE) {
  if (log_progress) message("Calculating feature variances...")
  # Compute variance for each column; na.rm = TRUE ensures missing values are handled.
  variance_list <- dt[, lapply(.SD, stats::var, na.rm = TRUE)]
  unlist(variance_list)
}

#' Variance Thresholding for Feature Selection
#'
#' Applies variance thresholding to a numeric dataset with additional flexibility:
#' you can specify whether to keep or remove features with variances either above or below
#' a specified threshold.
#'
#' @param data A numeric matrix, data frame, or data.table containing the input data.
#'             For data frames and data.tables, all columns must be numeric.
#' @param threshold A single, non-negative, finite numeric value. Depending on the
#'                  `threshold_direction` and `action` parameters, features with variances
#'                  either above or below this threshold will be kept or removed.
#'                  Default is 0.5.
#' @param log_progress Logical indicating whether progress messages should be printed.
#'                     Default is FALSE.
#' @param action A string specifying the action to take on features meeting the threshold
#'               condition. Valid options are "keep" (to retain features that meet the condition)
#'               or "remove" (to drop features that meet the condition). Default is "keep".
#' @param threshold_direction A string specifying whether the threshold applies to features
#'                            with variance "above" or "below" the threshold.
#'                            Default is "above".
#' @return A numeric matrix containing the filtered data. If no features meet the criteria,
#'         returns NULL.
#' @examples
#' set.seed(123)
#' data <- matrix(rnorm(1000), ncol = 10)
#'
#' # Default: keep features with variance > 0.5
#' thresholded_data <- fs_variance(data, threshold = 0.5, log_progress = TRUE)
#'
#' # Alternatively, remove features with variance > 0.5 (i.e. keep features with variance <= 0.5)
#' filtered_data <- fs_variance(data, threshold = 0.5, action = "remove", threshold_direction = "above")
fs_variance <- function(data, threshold = 0.5, log_progress = FALSE,
                        action = "keep", threshold_direction = "above") {
  # Validate threshold input
  if (!is.numeric(threshold) || length(threshold) != 1 ||
      threshold < 0 || !is.finite(threshold)) {
    stop("Threshold must be a single non-negative, finite numeric value.")
  }
  
  # Validate action and threshold_direction arguments
  if (!action %in% c("keep", "remove")) {
    stop("Argument 'action' must be either 'keep' or 'remove'.")
  }
  
  if (!threshold_direction %in% c("above", "below")) {
    stop("Argument 'threshold_direction' must be either 'above' or 'below'.")
  }
  
  # Convert data to a data.table for efficient processing
  dt <- convert_to_datatable(data, log_progress = log_progress)
  
  # Compute variances for each feature
  variances <- compute_feature_variances(dt, log_progress = log_progress)
  
  # Determine which features meet the condition based on the specified parameters
  if (threshold_direction == "above") {
    if (action == "keep") {
      selected <- variances > threshold
    } else {  # action == "remove"
      selected <- variances > threshold
    }
  } else {  # threshold_direction == "below"
    if (action == "keep") {
      selected <- variances < threshold
    } else {  # action == "remove"
      selected <- variances < threshold
    }
  }
  
  # For "keep" action, we select features meeting the condition;
  # for "remove", we select features NOT meeting the condition.
  if (action == "keep") {
    keep_features <- names(variances)[selected]
  } else {
    keep_features <- names(variances)[!selected]
  }
  
  # Warn and return NULL if no features meet the criteria
  if (length(keep_features) == 0) {
    warning("No features meet the specified variance criteria.")
    return(NULL)
  }
  
  if (log_progress) {
    condition <- if (threshold_direction == "above") ">" else "<"
    verb <- if (action == "keep") "Retaining" else "Removing"
    message(sprintf("%s features with variance %s %s", verb, condition, threshold))
  }
  
  # Subset the data.table to include only the appropriate features
  filtered_dt <- dt[, ..keep_features]
  
  if (log_progress) message("Variance thresholding completed.")
  
  # Return the result as a numeric matrix
  as.matrix(filtered_dt)
}

