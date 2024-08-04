# Load packages
library(data.table)
library(lubridate)

#' Calculate entropy.
#'
#' @param x A numeric vector.
#' @return Entropy value.
#' @examples
#' \dontrun{
#' entropy(c(1, 1, 2, 2, 2, 3))
#' }
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
#' \dontrun{
#' calculate_bins(c(1, 2, 3, 4, 5, 6))
#' }
#' @export
calculate_bins <- function(x) {
  n <- length(x)
  iqr_x <- IQR(x)
  iqr_x <- ifelse(iqr_x == 0, 1, iqr_x)  # Ensure non-zero IQR
  fd_bin_width <- 2 * iqr_x / (n^(1/3))  # Freedman-Diaconis rule
  sturges_bins <- ceiling(log2(n) + 1)    # Sturges' rule
  
  bins <- ifelse((max(x) - min(x)) / fd_bin_width > 1, ceiling((max(x) - min(x)) / fd_bin_width), sturges_bins)
  bins <- max(bins, 1)  # Ensure at least one bin
  return(bins)
}

#' Calculate information gain for each variable in a data frame.
#'
#' @param df A data frame.
#' @param target The name of the target variable.
#' @return A data frame with variable names and their corresponding information gains.
#' @examples
#' \dontrun{
#' df <- data.frame(A = sample(1:10, 100, replace = TRUE), 
#'                  B = sample(c("yes", "no"), 100, replace = TRUE), 
#'                  C = seq(as.Date("2001/1/1"), by = "month", length.out = 100),
#'                  target = sample(1:2, 100, replace = TRUE))
#' fs_infogain(df, "target")
#' }
#' @export
fs_infogain <- function(df, target) {
  if (!target %in% names(df)) {
    stop("The target variable is not found in the provided data frame.")
  }
  
  df <- as.data.table(df)
  
  # Convert date columns to year, month, day columns
  date_cols <- names(df)[sapply(df, lubridate::is.Date)]
  for (col in date_cols) {
    df[, (paste0(col, "_year")) := year(get(col))]
    df[, (paste0(col, "_month")) := month(get(col))]
    df[, (paste0(col, "_day")) := day(get(col))]
  }
  
  column_names <- setdiff(names(df), target)
  entropy_target <- entropy(df[[target]])
  total_rows <- nrow(df)
  
  info_gain <- numeric(length(column_names))
  
  for (i in seq_along(column_names)) {
    col <- column_names[i]
    
    if (is.numeric(df[[col]])) {
      n_bins <- calculate_bins(df[[col]])
      df[, (col) := cut(get(col), breaks = n_bins, include.lowest = TRUE, labels = FALSE)]
    }
    
    levels <- unique(df[[col]])
    entropy_attribute <- 0
    
    for (level in levels) {
      subset <- df[get(col) == level, , drop = FALSE]
      weight <- nrow(subset) / total_rows
      entropy_attribute <- entropy_attribute + weight * entropy(subset[[target]])
    }
    
    info_gain[i] <- entropy_target - entropy_attribute
  }
  
  result <- data.frame(Variable = column_names, InfoGain = info_gain)
  return(result)
}

#' Calculate information gain for each variable across multiple data frames.
#'
#' @param dfs_list A list of data frames.
#' @return A data frame with variable names, their corresponding information gains, and originating data frame.
#' @examples
#' \dontrun{
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
#' }
#' @export
calculate_information_gain_multiple <- function(dfs_list) {
  results_list <- vector("list", length(dfs_list))
  
  for (i in seq_along(dfs_list)) {
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

# Load necessary packages
library(data.table)
library(lubridate)
library(testthat)

# Define a helper function to check if two data frames are equal
expect_equal_data_frames <- function(df1, df2) {
  expect_equal(dim(df1), dim(df2))
  expect_equal(sort(names(df1)), sort(names(df2)))
  for (col in names(df1)) {
    expect_equal(df1[[col]], df2[[col]])
  }
}

# Define UAT for fs_infogain function
test_fs_infogain <- function() {
  cat("Running UAT for fs_infogain...\n")
  
  # Test 1: Simple numeric data frame
  df1 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(1:5, 100, replace = TRUE),
    target = sample(1:2, 100, replace = TRUE)
  )
  result1 <- fs_infogain(df1, "target")
  print(result1)
  
  # Test 2: Data frame with categorical variables
  df2 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(c("yes", "no"), 100, replace = TRUE),
    target = sample(1:2, 100, replace = TRUE)
  )
  result2 <- fs_infogain(df2, "target")
  print(result2)
  
  # Test 3: Data frame with date variables
  df3 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(c("yes", "no"), 100, replace = TRUE),
    C = seq(as.Date("2001/1/1"), by = "month", length.out = 100),
    target = sample(1:2, 100, replace = TRUE)
  )
  result3 <- fs_infogain(df3, "target")
  print(result3)
  
  # Test 4: Error handling when target is missing
  df4 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(c("yes", "no"), 100, replace = TRUE)
  )
  expect_error(fs_infogain(df4, "target"), "The target variable is not found in the provided data frame.")
  
  cat("UAT for fs_infogain completed.\n")
}

# Define UAT for calculate_information_gain_multiple function
test_calculate_information_gain_multiple <- function() {
  cat("Running UAT for calculate_information_gain_multiple...\n")
  
  # Test 1: Multiple data frames with numeric and categorical variables
  df1 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(c("yes", "no"), 100, replace = TRUE),
    C = seq(as.Date("2001/1/1"), by = "month", length.out = 100),
    target = sample(1:2, 100, replace = TRUE)
  )
  df2 <- data.frame(
    A = sample(1:5, 50, replace = TRUE),
    D = sample(c("up", "down"), 50, replace = TRUE),
    E = seq(as.Date("2015/1/1"), by = "day", length.out = 50),
    target = sample(1:3, 50, replace = TRUE)
  )
  dfs_list <- list(df1, df2)
  result <- calculate_information_gain_multiple(dfs_list)
  print(result)
  
  # Test 2: Error handling when target is missing in any data frame
  df3 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(c("yes", "no"), 100, replace = TRUE)
  )
  dfs_list_with_error <- list(df1, df3)
  expect_error(calculate_information_gain_multiple(dfs_list_with_error), "The target variable is not found in data frame at position 2.")
  
  cat("UAT for calculate_information_gain_multiple completed.\n")
}

# Run the UAT functions
test_fs_infogain()
test_calculate_information_gain_multiple()
