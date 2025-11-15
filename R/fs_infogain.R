# Load necessary packages
library(data.table)
library(lubridate)

########################################
# Helpers
########################################

#' Compute Entropy
#'
#' Computes the Shannon entropy of a vector, ignoring NA values.
#' Works for numeric, character, or factor vectors (treated categorically).
#'
#' @param x A vector.
#' @return A numeric value (entropy in bits).
compute_entropy <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(0)
  freq <- as.numeric(table(x))
  if (!length(freq)) return(0)
  prob <- freq / n
  # Guard against rounding noise
  prob <- prob[prob > 0]
  if (!length(prob)) return(0)
  val <- -sum(prob * log2(prob))
  if (!is.finite(val)) 0 else val
}

#' Sturges' Rule (bins)
sturges_bins <- function(n) {
  max(2L, ceiling(log2(n) + 1))
}

#' Calculate Number of Histogram Bins (Freedman–Diaconis with Sturges fallback)
#'
#' @param x Numeric vector.
#' @return Integer (# of bins, >= 2).
calculate_bins <- function(x) {
  n <- sum(!is.na(x))
  if (n <= 1) return(2L)
  
  iqr_x   <- IQR(x, na.rm = TRUE)
  range_x <- diff(range(x, na.rm = TRUE))
  
  # If no spread, use 2 bins
  if (iqr_x == 0 || range_x == 0) {
    return(2L)
  }
  
  fd_bin_width <- 2 * iqr_x / (n^(1/3))
  if (!is.finite(fd_bin_width) || fd_bin_width <= 0) {
    return(sturges_bins(n))
  }
  
  fd_bins <- ceiling(range_x / fd_bin_width)
  bins <- max(fd_bins, sturges_bins(n))
  max(2L, as.integer(bins))
}

#' Discretize a Numeric Vector
#'
#' Converts numeric vector into an ordered factor via binning.
#' If the vector has one unique non-NA value, returns a single-level factor.
#'
#' @param x Numeric vector.
#' @param bins Optional integer; if NULL, uses calculate_bins().
#' @return Ordered factor of bin membership.
discretize_numeric <- function(x, bins = NULL) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) <= 1) {
    return(factor(ifelse(is.na(x), NA, ux[1]), ordered = TRUE))
  }
  if (is.null(bins)) bins <- calculate_bins(x)
  bins <- max(2L, as.integer(bins))
  cut(x, breaks = bins, include.lowest = TRUE, ordered_result = TRUE)
}

#' Determine if a column is Date/POSIXt
.is_date_like <- function(x) {
  inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")
}

# Safe as.data.table that doesn't mutate caller's object
.as_dt <- function(df) {
  as.data.table(data.table::copy(df))
}

#' Expand Date Columns (year, month, day)
#'
#' Creates *_year, *_month, *_day columns for date-like predictors.
#' Optionally removes the original date columns. Target column is never touched.
#'
#' @param df data.frame/data.table
#' @param exclude character vector of column names to skip (e.g., the target)
#' @param remove_original logical; if TRUE, remove the original date columns
#' @return data.table with expanded columns
expand_date_columns <- function(df, exclude = character(), remove_original = TRUE) {
  dt <- .as_dt(df)
  cand <- setdiff(names(dt), exclude)
  
  # identify date-like columns among candidates
  date_cols <- cand[vapply(dt[, ..cand], .is_date_like, logical(1))]
  
  if (length(date_cols)) {
    for (col in date_cols) {
      # Use lubridate accessors on POSIXt or Date
      dt[, paste0(col, "_year")  := year(get(col))]
      dt[, paste0(col, "_month") := month(get(col))]
      dt[, paste0(col, "_day")   := day(get(col))]
    }
    if (remove_original) {
      dt[, (date_cols) := NULL]
    }
  }
  dt
}

#' Coerce Target to Categorical if Needed
#'
#' Information gain assumes a categorical target. If target is numeric,
#' discretize it into bins (Freedman–Diaconis with Sturges fallback).
#' Date-like targets are treated as categorical via their date representation.
#'
#' @param y target vector
#' @param numeric_bins optional integer override for numeric target discretization
#' @return factor/ordered factor target
as_categorical_target <- function(y, numeric_bins = NULL) {
  # Date-like targets: treat as categorical via YYYY-MM-DD string
  if (.is_date_like(y)) {
    return(factor(as.Date(y)))
  }
  if (is.numeric(y)) {
    return(discretize_numeric(y, bins = numeric_bins))
  }
  if (is.logical(y)) {
    return(factor(y))
  }
  if (is.character(y)) {
    return(factor(y))
  }
  if (is.factor(y)) {
    return(y)
  }
  # Fallback: coerce to factor
  factor(y)
}

########################################
# Core: Information Gain
########################################

#' Information Gain for One Predictor vs Target
#'
#' @param x predictor vector (numeric/character/factor/date-like)
#' @param y target vector (will be coerced to categorical if not already)
#' @param numeric_bins optional integer for predictor discretization
#' @return numeric information gain value
info_gain_one <- function(x, y, numeric_bins = NULL) {
  # Use complete cases only for this pair (don’t drop rows for other predictors)
  ok <- complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]
  
  if (!length(x) || !length(y)) return(NA_real_)
  
  # Ensure categorical target
  y <- as_categorical_target(y, numeric_bins = numeric_bins)
  
  # Discretize/normalize predictor to factor
  if (.is_date_like(x)) {
    x <- factor(as.Date(x))
  } else if (is.numeric(x)) {
    x <- discretize_numeric(x, bins = numeric_bins)
  } else if (is.logical(x)) {
    x <- factor(x)
  } else if (is.character(x)) {
    x <- factor(x)
  }
  # factors are left as-is
  
  # If x has one unique value, IG == 0
  if (length(unique(x)) <= 1) return(0)
  
  H_y <- compute_entropy(y)
  total <- length(y)
  
  # Conditional entropy H(Y|X)
  H_y_given_x <- 0
  ux <- unique(x)
  for (lev in ux) {
    idx <- which(x == lev)
    # idx is guaranteed non-empty because lev came from unique(x)
    w <- length(idx) / total
    H_y_given_x <- H_y_given_x + w * compute_entropy(y[idx])
  }
  
  ig <- H_y - H_y_given_x
  
  # Robustness clamp against tiny negatives / non-finites
  if (!is.finite(ig)) ig <- 0
  if (ig < 0) ig <- 0
  
  as.numeric(ig)
}

########################################
# Single-Frame Driver
########################################

#' Feature Selection via Information Gain (Single Data Frame)
#'
#' @param df data.frame/data.table with predictors and target
#' @param target character: target column name
#' @param numeric_bins optional integer to override bin calculation for numeric cols
#' @param remove_na if TRUE, removes rows with NA in the TARGET ONLY
#' @return data.frame with columns Variable and InfoGain
fs_infogain_single <- function(df, target, numeric_bins = NULL, remove_na = TRUE) {
  if (!target %in% names(df)) {
    stop("The specified target '", target, "' is not found in the data.")
  }
  
  # Expand date-like predictors, but NEVER touch the target column
  dt <- expand_date_columns(df, exclude = target, remove_original = TRUE)
  
  # Optionally drop rows with NA in the target only (precompute index to avoid data.table scoping issues)
  if (remove_na) {
    ok <- !is.na(dt[[target]])
    dt <- dt[ok]
  }
  if (!nrow(dt)) stop("No rows available after filtering on the target.")
  
  predictors <- setdiff(names(dt), target)
  if (!length(predictors)) {
    return(data.frame(Variable = character(), InfoGain = numeric(), stringsAsFactors = FALSE))
  }
  
  y <- dt[[target]]
  ig <- vapply(
    predictors,
    function(col) info_gain_one(dt[[col]], y, numeric_bins = numeric_bins),
    numeric(1)
  )
  
  data.frame(Variable = predictors, InfoGain = ig, stringsAsFactors = FALSE)
}

########################################
# Multi-Frame Driver
########################################

#' Information Gain Across Multiple Data Frames
#'
#' @param dfs_list named or unnamed list of data.frames
#' @param target character: target column present in each data.frame
#' @param numeric_bins optional integer for discretization (passed through)
#' @param remove_na logical; drop rows with NA in target (per data.frame)
#' @return data.table with columns: Variable, InfoGain, Origin
calculate_information_gain_multiple <- function(dfs_list, target, numeric_bins = NULL, remove_na = TRUE) {
  if (!length(dfs_list)) {
    stop("dfs_list must be a non-empty list of data.frames.")
  }
  
  results_list <- vector("list", length(dfs_list))
  nm <- names(dfs_list)
  
  for (i in seq_along(dfs_list)) {
    df <- dfs_list[[i]]
    if (!inherits(df, "data.frame")) {
      stop("Element ", i, " is not a data.frame.")
    }
    if (!(target %in% names(df))) {
      stop("Target '", target, "' not found in data.frame at position ", i, ".")
    }
    
    res <- fs_infogain_single(df, target, numeric_bins = numeric_bins, remove_na = remove_na)
    
    origin_name <- if (!is.null(nm)) {
      this_name <- nm[i]
      if (!is.na(this_name) && nzchar(this_name)) this_name else paste0("Data_Frame_", i)
    } else {
      paste0("Data_Frame_", i)
    }
    
    if (nrow(res)) res$Origin <- origin_name
    results_list[[i]] <- res
  }
  
  rbindlist(results_list, use.names = TRUE, fill = TRUE)
}

########################################
# Public Wrapper
########################################

#' Feature Selection via Information Gain
#'
#' Accepts either a single data.frame or a list of data.frames and computes
#' the information gain of each predictor relative to the specified target.
#'
#' Details:
#' - Numeric predictors are discretized (Freedman–Diaconis with Sturges' fallback).
#' - Date-like predictors are expanded (Y/M/D) or coerced to categorical as appropriate.
#' - Target is coerced to categorical (numeric targets are discretized).
#' - NAs are handled per predictor (rows are not globally dropped); optionally
#'   rows with NA in the TARGET can be removed.
#'
#' @param data data.frame or list of data.frames
#' @param target character: target column name
#' @param numeric_bins optional integer override for discretization
#' @param remove_na logical: if TRUE, removes rows with NA in the target
#' @return data.frame (single input) or data.table (list input)
#' @examples
#' # Single data.frame:
#' set.seed(1)
#' df <- data.frame(
#'   A = sample(1:10, 100, replace = TRUE),
#'   B = sample(c("yes", "no"), 100, replace = TRUE),
#'   when = as.Date("2020-01-01") + sample(0:30, 100, TRUE),
#'   target = sample(1:2, 100, replace = TRUE)
#' )
#' fs_infogain(df, "target")
#'
#' # List of data.frames:
#' df1 <- data.frame(
#'   A = sample(1:10, 100, replace = TRUE),
#'   target = sample(1:2, 100, replace = TRUE)
#' )
#' df2 <- data.frame(
#'   B = sample(c("yes", "no"), 100, replace = TRUE),
#'   target = sample(letters[1:3], 100, replace = TRUE)
#' )
#' fs_infogain(list(df1 = df1, df2 = df2), "target")
#' @export
fs_infogain <- function(data, target, numeric_bins = NULL, remove_na = TRUE) {
  if (inherits(data, "data.frame")) {
    fs_infogain_single(
      df = data,
      target = target,
      numeric_bins = numeric_bins,
      remove_na = remove_na
    )
  } else if (is.list(data)) {
    # Ensure all elements are data.frames
    if (!all(vapply(data, function(x) inherits(x, "data.frame"), logical(1)))) {
      stop("All elements in 'data' must be data.frames.")
    }
    calculate_information_gain_multiple(
      dfs_list = data,
      target = target,
      numeric_bins = numeric_bins,
      remove_na = remove_na
    )
  } else {
    stop("Input 'data' must be a data.frame or a list of data.frames.")
  }
}
