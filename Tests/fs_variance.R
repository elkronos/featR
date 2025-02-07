###############################################################################
# Testing Infrastructure - Variance Thresholding
###############################################################################

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
  ensure_package("data.table")
  
  cat("Running unit tests for fs_variance...\n")
  
  test_that("Numeric matrix input works with default parameters", {
    set.seed(123)
    data_mat <- matrix(rnorm(1000), ncol = 10)
    result <- fs_variance(data_mat, threshold = 0.5)
    expect_true(is.matrix(result))
    expect_true(ncol(result) <= ncol(data_mat))
  })
  
  test_that("Numeric data frame input works with default parameters", {
    set.seed(123)
    df <- data.frame(matrix(rnorm(1000), ncol = 10))
    result <- fs_variance(df, threshold = 0.5)
    expect_true(is.matrix(result))
    expect_true(ncol(result) <= ncol(df))
  })
  
  test_that("Data.table input works with default parameters", {
    set.seed(123)
    dt_input <- data.table::as.data.table(matrix(rnorm(1000), ncol = 10))
    result <- fs_variance(dt_input, threshold = 0.5)
    expect_true(is.matrix(result))
    expect_true(ncol(result) <= ncol(dt_input))
  })
  
  test_that("Returns NULL when no feature meets threshold with default keep/above", {
    set.seed(123)
    df_low_var <- data.frame(matrix(rnorm(1000, mean = 0, sd = 0.1), ncol = 10))
    result <- fs_variance(df_low_var, threshold = 0.5)
    expect_null(result)
  })
  
  test_that("Works with mixed variances using default parameters", {
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
    df_missing <- data.frame(
      x1 = rnorm(100),
      x2 = rnorm(100),
      x3 = c(rep(0, 95), rep(NA, 5))
    )
    result <- fs_variance(df_missing, threshold = 0.5)
    expect_true(is.matrix(result))
    expect_true(ncol(result) < ncol(df_missing))
  })
  
  test_that("Matrix with low variance returns NULL using default parameters", {
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
  
  # New tests for the additional parameters:
  test_that("Works with action = 'remove' and threshold_direction = 'above'", {
    set.seed(123)
    data_mat <- matrix(rnorm(1000), ncol = 10)
    result <- fs_variance(data_mat, threshold = 0.5, action = "remove", threshold_direction = "above")
    original_variances <- apply(data_mat, 2, var)
    # All columns in the result should have variance <= 0.5.
    kept_cols <- colnames(result)
    expect_true(all(original_variances[kept_cols] <= 0.5))
  })
  
  test_that("Works with action = 'keep' and threshold_direction = 'below'", {
    set.seed(123)
    data_mat <- matrix(rnorm(1000, mean = 0, sd = 0.5), ncol = 10)
    result <- fs_variance(data_mat, threshold = 0.1, action = "keep", threshold_direction = "below")
    original_variances <- apply(data_mat, 2, var)
    kept_cols <- colnames(result)
    expect_true(all(original_variances[kept_cols] < 0.1))
  })
  
  cat("All unit tests completed successfully.\n")
}

# Uncomment the line below to run the tests
# test_fs_variance()
