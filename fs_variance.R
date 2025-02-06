#' Ensure a Package is Installed and Loaded
#'
#' Checks if a package is installed; if not, it installs the package, then loads it.
#'
#' @param pkg A character string specifying the package name.
#' @return Invisibly returns TRUE if the package was loaded successfully.
ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  if (!suppressPackageStartupMessages(require(pkg, character.only = TRUE, quietly = TRUE))) {
    stop(sprintf("Package '%s' failed to load.", pkg))
  }
  invisible(TRUE)
}

#' Convert Input Data to data.table
#'
#' Converts a numeric data frame or matrix into a data.table for efficient processing.
#'
#' @param data A numeric matrix or data frame.
#' @param log_progress Logical indicating whether progress messages should be printed.
#' @return A data.table version of the input data.
convert_to_datatable <- function(data, log_progress = FALSE) {
  ensure_package("data.table")
  
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
    dt <- data.table::as.data.table(as.data.frame(data))
  } else {
    stop("Data must be either a numeric matrix or a data frame.")
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
  variance_list <- dt[, lapply(.SD, stats::var, na.rm = TRUE)]
  unlist(variance_list)
}

#' Variance Thresholding for Feature Selection
#'
#' Applies variance thresholding to a numeric dataset by removing features whose
#' variances do not exceed the specified threshold.
#'
#' @param data A numeric matrix or data frame containing the input data.
#'             For data frames, all columns must be numeric.
#' @param threshold A single, non-negative, finite numeric value. Only features with
#'                  variance strictly greater than this threshold will be retained.
#'                  Default is 0.5.
#' @param log_progress Logical indicating whether progress messages should be printed.
#'                     Default is FALSE.
#' @return A numeric matrix containing the filtered data. If no features have variance
#'         above the threshold, returns NULL.
#' @examples
#' set.seed(123)
#' data <- matrix(rnorm(1000), ncol = 10)
#' thresholded_data <- fs_variance(data, threshold = 0.5, log_progress = TRUE)
#' if (!is.null(thresholded_data)) {
#'   dim(thresholded_data)
#' }
fs_variance <- function(data, threshold = 0.5, log_progress = FALSE) {
  # Validate threshold input
  if (!is.numeric(threshold) || length(threshold) != 1 ||
      threshold < 0 || !is.finite(threshold)) {
    stop("Threshold must be a single non-negative, finite numeric value.")
  }
  
  # Convert data to a data.table for efficient processing
  dt <- convert_to_datatable(data, log_progress = log_progress)
  
  # Compute variances for each feature
  variances <- compute_feature_variances(dt, log_progress = log_progress)
  
  # Identify features with variance above the threshold
  keep_features <- names(variances)[variances > threshold]
  
  # Warn and return NULL if no features meet the threshold criteria
  if (length(keep_features) == 0) {
    warning("No features have variance above the specified threshold.")
    return(NULL)
  }
  
  if (log_progress) message("Retaining features with variance > ", threshold)
  # Subset the data.table to retain only the selected features
  filtered_dt <- dt[, ..keep_features]
  
  if (log_progress) message("Variance thresholding completed.")
  # Return the result as a numeric matrix
  as.matrix(filtered_dt)
}

#' Unit Tests for fs_variance Function
#'
#' Runs a series of tests to verify that fs_variance behaves as expected.
#'
#' @examples
#' \dontrun{
#' test_fs_variance()
#' }
test_fs_variance <- function() {
  ensure_package("testthat")
  suppressPackageStartupMessages(library(testthat))
  
  cat("Running unit tests for fs_variance...\n")
  
  test_that("Numeric matrix input works", {
    set.seed(123)
    data_mat <- matrix(rnorm(1000), ncol = 10)
    result <- fs_variance(data_mat, threshold = 0.5)
    expect_true(is.matrix(result))
    expect_true(ncol(result) <= ncol(data_mat))
  })
  
  test_that("Numeric data frame input works", {
    set.seed(123)
    df <- data.frame(matrix(rnorm(1000), ncol = 10))
    result <- fs_variance(df, threshold = 0.5)
    expect_true(is.matrix(result))
    expect_true(ncol(result) <= ncol(df))
  })
  
  test_that("Returns NULL when no feature exceeds threshold", {
    set.seed(123)
    df_low_var <- data.frame(matrix(rnorm(1000, mean = 0, sd = 0.1), ncol = 10))
    result <- fs_variance(df_low_var, threshold = 0.5)
    expect_null(result)
  })
  
  test_that("Works with mixed variances", {
    set.seed(123)
    df_mixed <- data.frame(matrix(0, ncol = 10, nrow = 100))
    for (i in 1:10) {
      df_mixed[, i] <- if (i %% 2 == 0) {
        rnorm(100, mean = 0, sd = 0.1)
      } else {
        rnorm(100, mean = 0, sd = 1)
      }
    }
    result <- fs_variance(df_mixed, threshold = 0.5)
    expect_true(is.matrix(result))
    expect_true(ncol(result) < ncol(df_mixed))
  })
  
  test_that("Fails for non-numeric columns", {
    set.seed(123)
    df_non_numeric <- data.frame(
      x1 = rnorm(100),
      x2 = rnorm(100),
      x3 = sample(letters, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    expect_error(fs_variance(df_non_numeric, threshold = 0.5),
                 "All columns of the data frame must be numeric.")
  })
  
  test_that("Handles missing values gracefully", {
    set.seed(123)
    # Create a column with near-zero variance by using a constant value and some NAs.
    df_missing <- data.frame(
      x1 = rnorm(100),
      x2 = rnorm(100),
      x3 = c(rep(0, 95), rep(NA, 5))
    )
    result <- fs_variance(df_missing, threshold = 0.5)
    expect_true(is.matrix(result))
    expect_true(ncol(result) < ncol(df_missing))
  })
  
  test_that("Matrix with low variance returns NULL", {
    set.seed(123)
    data_low_var <- matrix(rnorm(1000, mean = 0, sd = 0.1), ncol = 10)
    result <- fs_variance(data_low_var, threshold = 1)
    expect_null(result)
  })
  
  test_that("Invalid threshold type triggers error", {
    df <- data.frame(matrix(rnorm(1000), ncol = 10))
    expect_error(fs_variance(df, threshold = "invalid"),
                 "Threshold must be a single non-negative, finite numeric value.")
  })
  
  test_that("Negative threshold triggers error", {
    df <- data.frame(matrix(rnorm(1000), ncol = 10))
    expect_error(fs_variance(df, threshold = -1),
                 "Threshold must be a single non-negative, finite numeric value.")
  })
  
  cat("All unit tests completed successfully.\n")
}

# Run tests
# test_fs_variance()
