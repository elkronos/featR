# Load necessary packages
library(data.table)
library(lubridate)
library(testthat)

########################################
# Helper Functions
########################################

#' Compute Entropy
#'
#' Computes the entropy of a vector, ignoring NA values.
#'
#' @param x A numeric or factor vector.
#' @return A numeric value representing the entropy.
#' @examples
#' compute_entropy(c(1, 1, 2, 2, 2, 3))
compute_entropy <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(0)
  freq <- as.numeric(table(x))
  prob <- freq / n
  -sum(prob * log2(prob))
}

#' Calculate Number of Histogram Bins
#'
#' Uses the Freedman-Diaconis rule with a fallback to Sturges' rule.
#'
#' @param x A numeric vector.
#' @return An integer number of bins.
#' @examples
#' calculate_bins(c(1, 2, 3, 4, 5, 6))
calculate_bins <- function(x) {
  n <- sum(!is.na(x))
  if (n == 0) return(1)
  
  iqr_x <- IQR(x, na.rm = TRUE)
  if (iqr_x == 0) return(1)
  
  fd_bin_width <- 2 * iqr_x / (n^(1/3))
  range_x <- diff(range(x, na.rm = TRUE))
  
  if (fd_bin_width == 0) {
    bins <- 1
  } else {
    bins <- ceiling(range_x / fd_bin_width)
    if (bins < ceiling(log2(n) + 1)) {
      bins <- ceiling(log2(n) + 1)
    }
  }
  max(bins, 1)
}

#' Discretize a Numeric Column
#'
#' Converts a numeric vector into a factor by binning.
#'
#' If the column has only one unique value, it returns a factor with one level.
#'
#' @param x A numeric vector.
#' @param bins Number of bins to use; if NULL, uses calculate_bins().
#' @return A factor indicating the bin membership.
#' @examples
#' discretize_numeric(c(1,2,3,4,5))
discretize_numeric <- function(x, bins = NULL) {
  # If there is only one unique value, return a factor with one level.
  if(length(unique(x)) == 1) {
    return(factor(rep(1, length(x))))
  }
  if (is.null(bins)) {
    bins <- calculate_bins(x)
  }
  # Ensure there are at least 2 breaks for cut()
  if(bins < 2) bins <- 2
  cut(x, breaks = bins, include.lowest = TRUE, labels = FALSE)
}

#' Expand Date Columns
#'
#' For every date column in the data frame, creates additional columns for
#' year, month, and day.
#'
#' @param df A data.frame or data.table.
#' @return A data.table with expanded date columns.
#' @examples
#' df <- data.frame(date = as.Date('2020-01-01') + 0:4, x = rnorm(5))
#' expand_date_columns(df)
expand_date_columns <- function(df) {
  dt <- as.data.table(df)
  date_cols <- names(dt)[sapply(dt, lubridate::is.Date)]
  if (length(date_cols) > 0) {
    for (col in date_cols) {
      dt[, paste0(col, "_year") := year(get(col))]
      dt[, paste0(col, "_month") := month(get(col))]
      dt[, paste0(col, "_day") := day(get(col))]
    }
    dt[, (date_cols) := NULL]
  }
  dt
}

########################################
# Core Functions (Internal)
########################################

#' Feature Selection via Information Gain (Single Data Frame)
#'
#' Internal function that calculates the information gain of each predictor variable relative to the target.
#' Numeric predictors are discretized using a binning method (default is Freedman-Diaconis with a Sturges fallback).
#' Date columns are expanded to year, month, and day.
#'
#' @param df A data.frame containing predictors and a target.
#' @param target A string specifying the name of the target variable.
#' @param numeric_bins An optional integer to override automatic bin calculation for numeric predictors.
#'        If NULL, bins are calculated automatically.
#' @param remove_na Logical. If TRUE, rows with any missing values are removed before calculation.
#' @return A data.frame with columns: Variable and InfoGain.
#' @examples
#' df <- data.frame(
#'   A = sample(1:10, 100, replace = TRUE),
#'   B = sample(c("yes", "no"), 100, replace = TRUE),
#'   target = sample(1:2, 100, replace = TRUE)
#' )
#' fs_infogain_single(df, "target")
fs_infogain_single <- function(df, target, numeric_bins = NULL, remove_na = TRUE) {
  if (!target %in% names(df)) {
    stop("The target variable is not found in the provided data frame.")
  }
  
  dt <- as.data.table(copy(df))
  dt <- expand_date_columns(dt)
  
  if (remove_na) {
    dt <- dt[complete.cases(dt), ]
  }
  
  if (nrow(dt) == 0) {
    stop("No complete cases available after NA removal.")
  }
  
  predictors <- setdiff(names(dt), target)
  entropy_target <- compute_entropy(dt[[target]])
  total_rows <- nrow(dt)
  info_gain <- numeric(length(predictors))
  
  for (i in seq_along(predictors)) {
    col <- predictors[i]
    current_col <- dt[[col]]
    
    if (is.numeric(current_col)) {
      current_col <- discretize_numeric(current_col, bins = numeric_bins)
    }
    
    unique_levels <- unique(current_col)
    entropy_col <- 0
    for (lev in unique_levels) {
      idx <- which(current_col == lev)
      weight <- length(idx) / total_rows
      entropy_col <- entropy_col + weight * compute_entropy(dt[[target]][idx])
    }
    info_gain[i] <- entropy_target - entropy_col
  }
  
  data.frame(Variable = predictors, InfoGain = info_gain, stringsAsFactors = FALSE)
}

#' Calculate Information Gain Across Multiple Data Frames
#'
#' Internal function that applies fs_infogain_single to a list of data frames.
#' Each data frame must contain the target column.
#'
#' @param dfs_list A named or unnamed list of data.frames.
#' @param numeric_bins Optional integer for numeric discretization; passed to fs_infogain_single.
#' @param remove_na Logical. Passed to fs_infogain_single.
#' @return A data.table with columns: Variable, InfoGain, and Origin (the source data frame).
#' @examples
#' df1 <- data.frame(
#'   A = sample(1:10, 100, replace = TRUE),
#'   target = sample(1:2, 100, replace = TRUE)
#' )
#' df2 <- data.frame(
#'   B = sample(c("yes", "no"), 100, replace = TRUE),
#'   target = sample(1:3, 100, replace = TRUE)
#' )
#' dfs_list <- list(df1 = df1, df2 = df2)
#' calculate_information_gain_multiple(dfs_list)
calculate_information_gain_multiple <- function(dfs_list, numeric_bins = NULL, remove_na = TRUE) {
  results_list <- list()
  
  for (i in seq_along(dfs_list)) {
    df <- dfs_list[[i]]
    if (!("target" %in% names(df))) {
      stop(paste0("The target variable is not found in data frame at position ", i, "."))
    }
    result <- fs_infogain_single(df, "target", numeric_bins = numeric_bins, remove_na = remove_na)
    origin_name <- if (!is.null(names(dfs_list)) && names(dfs_list)[i] != "") {
      names(dfs_list)[i]
    } else {
      paste0("Data_Frame_", i)
    }
    result$Origin <- origin_name
    results_list[[i]] <- result
  }
  
  rbindlist(results_list)
}

########################################
# Final Wrapper Function: fs_infogain
########################################

#' Feature Selection via Information Gain
#'
#' This convenience wrapper accepts either a single data.frame or a list of data.frames.
#' It calculates the information gain of predictors relative to a target variable.
#'
#' For a single data.frame, numeric predictors are discretized (using Freedman-Diaconis with a Sturges fallback),
#' date columns are expanded, and rows with missing values can be removed.
#'
#' For a list of data.frames, each data frame is processed similarly and an additional column 'Origin' indicates
#' the source of each result.
#'
#' @param data A data.frame or a list of data.frames.
#' @param target A string specifying the target variable name.
#' @param numeric_bins Optional integer for numeric discretization; passed to underlying functions.
#' @param remove_na Logical. If TRUE, rows with missing values are removed; passed to underlying functions.
#' @return If a data.frame is provided, returns a data.frame with columns: Variable and InfoGain.
#'         If a list is provided, returns a data.table with columns: Variable, InfoGain, and Origin.
#' @examples
#' # Single data.frame example:
#' df <- data.frame(
#'   A = sample(1:10, 100, replace = TRUE),
#'   B = sample(c("yes", "no"), 100, replace = TRUE),
#'   target = sample(1:2, 100, replace = TRUE)
#' )
#' fs_infogain(df, "target")
#'
#' # List of data.frames example:
#' df1 <- data.frame(
#'   A = sample(1:10, 100, replace = TRUE),
#'   target = sample(1:2, 100, replace = TRUE)
#' )
#' df2 <- data.frame(
#'   B = sample(c("yes", "no"), 100, replace = TRUE),
#'   target = sample(1:3, 100, replace = TRUE)
#' )
#' fs_infogain(list(df1 = df1, df2 = df2), "target")
#' @export
fs_infogain <- function(data, target, numeric_bins = NULL, remove_na = TRUE) {
  if (inherits(data, "data.frame")) {
    return(fs_infogain_single(data, target, numeric_bins = numeric_bins, remove_na = remove_na))
  } else if (is.list(data)) {
    # Ensure all elements in the list are data.frames.
    if (!all(sapply(data, function(x) inherits(x, "data.frame")))) {
      stop("All elements in the list must be data.frames.")
    }
    return(calculate_information_gain_multiple(data, numeric_bins = numeric_bins, remove_na = remove_na))
  } else {
    stop("Input data must be either a data.frame or a list of data.frames.")
  }
}

