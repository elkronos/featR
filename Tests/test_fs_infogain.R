###############################################################################
# Testing Infrastructure - Infogain (updated for robust implementation)
###############################################################################

#' Run tests for fs_infogain API
#'
#' Assumes the fs_infogain implementation has been sourced in the current session.
#' This wrapper seeds RNG for reproducibility and executes a set of unit tests.
#'
#' @param seed Integer RNG seed for reproducibility (default: 42)
#' @examples
#' # After sourcing the implementation file:
#' # test_fs_infogain_wrapper()
test_fs_infogain_wrapper <- function(seed = 42) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("Package 'testthat' is required to run tests.")
  }
  library(testthat)
  
  set.seed(seed)
  
  test_that("fs_infogain handles a single data.frame with numeric predictors", {
    df1 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(1:5, 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    result1 <- fs_infogain(df1, "target")
    
    expect_true(all(c("Variable", "InfoGain") %in% names(result1)))
    expect_setequal(result1$Variable, c("A", "B"))
    expect_type(result1$InfoGain, "double")
    expect_true(all(!is.na(result1$InfoGain)))
    expect_true(all(is.finite(result1$InfoGain)))
    expect_true(all(result1$InfoGain >= 0))
    
    # IG should be bounded above by H(Y)
    H_y <- {
      y <- factor(df1$target)
      freq <- as.numeric(table(y))
      p <- freq / sum(freq)
      -sum(p * log2(p))
    }
    expect_true(all(result1$InfoGain <= H_y + 1e-12))
  })
  
  test_that("fs_infogain handles a single data.frame with mixed (numeric + categorical) predictors", {
    df2 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    result2 <- fs_infogain(df2, "target")
    expect_true(all(c("Variable", "InfoGain") %in% names(result2)))
    expect_setequal(result2$Variable, c("A", "B"))
    expect_true(all(!is.na(result2$InfoGain)))
    expect_true(all(is.finite(result2$InfoGain)))
    expect_true(all(result2$InfoGain >= 0))
  })
  
  test_that("fs_infogain expands date columns in a single data.frame (predictors only)", {
    df3 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE),
      C = seq(as.Date("2001-01-01"), by = "month", length.out = 100),
      target = sample(1:2, 100, replace = TRUE)
    )
    result3 <- fs_infogain(df3, "target")
    expect_true(all(c("Variable", "InfoGain") %in% names(result3)))
    # Expect A, B, and 3 expanded columns from C (year/month/day)
    expect_true(any(grepl("^C_", result3$Variable)),
                info = "Expected expanded date-derived predictors named like C_year/C_month/C_day.")
    # Ensure original date column 'C' is not present after expansion
    expect_false("C" %in% result3$Variable)
    expect_true(all(!is.na(result3$InfoGain)))
    expect_true(all(is.finite(result3$InfoGain)))
  })
  
  test_that("fs_infogain errors when target is missing in a single data.frame", {
    df4 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE)
    )
    expect_error(
      fs_infogain(df4, "target"),
      regexp = "specified target.*is not found",
      ignore.case = TRUE
    )
  })
  
  test_that("fs_infogain handles a list of data.frames and returns Origin", {
    df1 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    df2 <- data.frame(
      B = sample(c("yes", "no"), 100, replace = TRUE),
      target = sample(1:3, 100, replace = TRUE)
    )
    dfs_list <- list(df1 = df1, df2 = df2)
    result <- fs_infogain(dfs_list, "target")
    expect_true(all(c("Variable", "InfoGain", "Origin") %in% names(result)))
    expect_true(all(result$Origin %in% c("df1", "df2")))
    expect_true(all(!is.na(result$InfoGain)))
    expect_true(all(is.finite(result$InfoGain)))
    expect_true(all(result$InfoGain >= 0))
  })
  
  test_that("fs_infogain errors when non-data.frame is included in list", {
    expect_error(
      fs_infogain(list(1, 2), "target"),
      regexp = "All elements in 'data' must be data\\.frames\\.",
      ignore.case = FALSE
    )
  })
  
  test_that("fs_infogain errors when target is missing in one data.frame of a list", {
    df1 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    df3 <- data.frame(
      A = sample(1:10, 100, replace = TRUE)
    )
    dfs_list_with_error <- list(df1 = df1, df3 = df3)
    expect_error(
      fs_infogain(dfs_list_with_error, "target"),
      regexp = "Target 'target' not found.*position 2",
      ignore.case = TRUE
    )
  })
  
  test_that("numeric_bins override is accepted and used for discretization (no error, finite IG)", {
    df <- data.frame(
      X = rnorm(200),
      Y = runif(200, min = -5, max = 5),
      target = sample(0:1, 200, replace = TRUE)
    )
    res <- fs_infogain(df, "target", numeric_bins = 3L)
    expect_true(all(c("Variable", "InfoGain") %in% names(res)))
    expect_setequal(res$Variable, c("X", "Y"))
    expect_true(all(!is.na(res$InfoGain)))
    expect_true(all(is.finite(res$InfoGain)))
    expect_true(all(res$InfoGain >= 0))
  })
  
  test_that("per-predictor NA handling (rows not globally dropped)", {
    n <- 150
    df <- data.frame(
      A = sample(1:10, n, replace = TRUE),
      B = sample(c("yes", "no", NA), n, replace = TRUE, prob = c(0.45, 0.45, 0.10)),
      target = sample(c(0, 1, NA), n, replace = TRUE, prob = c(0.45, 0.45, 0.10))
    )
    
    # Compare with and without removing NA in target
    res_keep <- fs_infogain(df, "target", remove_na = FALSE)
    res_drop <- fs_infogain(df, "target", remove_na = TRUE)
    
    expect_true(all(c("Variable", "InfoGain") %in% names(res_keep)))
    expect_true(all(c("Variable", "InfoGain") %in% names(res_drop)))
    expect_setequal(res_keep$Variable, c("A", "B"))
    expect_setequal(res_drop$Variable, c("A", "B"))
    expect_true(all(is.finite(res_keep$InfoGain) | is.na(res_keep$InfoGain)))
    expect_true(all(is.finite(res_drop$InfoGain) | is.na(res_drop$InfoGain)))
    # Any non-NA IGs should be non-negative
    expect_true(all(res_keep$InfoGain[!is.na(res_keep$InfoGain)] >= 0))
    expect_true(all(res_drop$InfoGain[!is.na(res_drop$InfoGain)] >= 0))
  })
  
  test_that("numeric target is discretized automatically and returns finite IG", {
    df <- data.frame(
      A = rnorm(250),
      B = sample(letters[1:3], 250, replace = TRUE),
      target = rnorm(250)
    )
    res <- fs_infogain(df, "target")
    expect_setequal(res$Variable, c("A", "B"))
    expect_true(all(!is.na(res$InfoGain)))
    expect_true(all(is.finite(res$InfoGain)))
    expect_true(all(res$InfoGain >= 0))
  })
  
  test_that("date-like target coerced to categorical and handled", {
    dates <- as.Date("2020-01-01") + sample(0:60, 200, TRUE)
    df <- data.frame(
      A = rnorm(200),
      B = sample(c("x", "y"), 200, TRUE),
      target = dates
    )
    res <- fs_infogain(df, "target")
    expect_setequal(res$Variable, c("A", "B"))
    expect_true(all(!is.na(res$InfoGain)))
    expect_true(all(is.finite(res$InfoGain)))
    expect_true(all(res$InfoGain >= 0))
  })
}

# Run the tests (uncomment to execute)
# test_fs_infogain_wrapper()
