# Load packages
library(data.table)
library(lubridate)

#' Calculate entropy.
#'
#' @param x A numeric vector.
#' @return Entropy value.
#' @examples
#' entropy(c(1, 1, 2, 2, 2, 3))
#' @export
entropy <- function(x) {
  x_sorted <- sort(x)
  freq <- rle(x_sorted)$lengths
  prob <- freq / length(x)
  -sum(prob * log2(prob))
}

#' Calculate the number of histogram bins.
#'
#' @param x A numeric vector.
#' @return Number of bins calculated using Freedman-Diaconis rule or Sturges' rule as a fallback.
#' @examples
#' calculate_bins(c(1, 2, 3, 4, 5, 6))
#' @export
calculate_bins <- function(x) {
  n <- length(x)
  iqr_x <- IQR(x)
  iqr_x <- ifelse(iqr_x == 0, 1, iqr_x)  # Ensure non-zero IQR
  fd_bin_width <- 2 * iqr_x / (n^(1/3))  # Freedman-Diaconis rule
  sturges_bins <- ceiling(log2(n) + 1)    # Sturges' rule
  
  bins <- ifelse((max(x) - min(x)) / fd_bin_width > 1, ceiling((max(x) - min(x)) / fd_bin_width), sturges_bins)
  return(bins)
}

#' Calculate information gain for each variable in a data frame.
#'
#' @param df A data frame.
#' @param target The name of the target variable.
#' @return A data frame with variable names and their corresponding information gains.
#' @examples
#' df <- data.frame(A = sample(1:10, 100, replace = TRUE), 
#'                  B = sample(c("yes", "no"), 100, replace = TRUE), 
#'                  C = seq(as.Date("2001/1/1"), by = "month", length.out = 100),
#'                  target = sample(1:2, 100, replace = TRUE))
#' fs_infogain(df, "target")
#' @export
fs_infogain <- function(df, target) {
  if (!target %in% names(df)) {
    stop("The target variable is not found in the provided data frame.")
  }
  
  df <- data.table(df)
  
  date_cols <- which(vapply(df, lubridate::is.Date, logical(1)))
  for(col in names(df)[date_cols]) {
    df[, paste0(col, "_year") := lubridate::year(df[[col]])]
    df[, paste0(col, "_month") := lubridate::month(df[[col]])]
    df[, paste0(col, "_day") := lubridate::day(df[[col]])]
  }
  
  info_gain <- vector()
  column_names <- names(df)
  entropy_target <- entropy(df[[target]])
  total_rows <- nrow(df)
  
  for(col in column_names) {
    if(col != target){
      if(is.numeric(df[[col]])) {
        n_bins <- calculate_bins(df[[col]])
        df[[col]] <- cut(df[[col]], breaks = n_bins, include.lowest = TRUE, labels = FALSE)
      }
      
      levels <- unique(df[[col]])
      entropy_attribute <- 0
      
      for(level in levels) {
        subset <- df[df[[col]] == level, ]
        weight <- nrow(subset) / total_rows
        entropy_attribute <- entropy_attribute + weight * entropy(subset[[target]])
      }
      
      gain <- entropy_target - entropy_attribute
      info_gain <- c(info_gain, gain)
    }
  }
  
  result <- data.frame(Variable = column_names[column_names != target], InfoGain = info_gain)
  return(result)
}

#' Calculate information gain for each variable across multiple data frames.
#'
#' @param dfs_list A list of data frames.
#' @return A data frame with variable names, their corresponding information gains, and originating data frame.
#' @examples
#' df1 <- data.frame(A = sample(1:10, 100, replace = TRUE), 
#'                  B = sample(c("yes", "no"), 100, replace = TRUE), 
#'                  C = seq(as.Date("2001/1/1"), by = "month", length.out = 100),
#'                  target = sample(1:2, 100, replace = TRUE))
#' df2 <- data.frame(A = sample(1:5, 50, replace = TRUE), 
#'                  D = sample(c("up", "down"), 50, replace = TRUE), 
#'                  E = seq(as.Date("2015/1/1"), by = "day", length.out = 50),
#'                  target = sample(1:3, 50, replace = TRUE))
#' dfs_list <- list(df1, df2)
#' calculate_information_gain_multiple(dfs_list)
#' @export
calculate_information_gain_multiple <- function(dfs_list) {
  results_list <- list()
  
  for(i in seq_along(dfs_list)) {
    df <- dfs_list[[i]]
    if (!"target" %in% names(df)) {
      stop(paste0("The target variable is not found in data frame at position ", i, "."))
    }
    result <- fs_infogain(df, "target")
    result$Origin <- paste0("Data_Frame_", i)
    results_list[[i]] <- result
  }
  
  all_results <- rbindlist(results_list)
  return(all_results)
}