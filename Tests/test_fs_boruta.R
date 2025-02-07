###############################################################################
# Testing Infrastructure - Boruta
###############################################################################

#' @title Unit Tests for fs_boruta Function
#'
#' @description
#' Runs a series of tests to verify that the \code{fs_boruta} function behaves as expected under various conditions,
#' including handling of numeric, categorical, and date variables, as well as error handling.
#'
#' @examples
#' \dontrun{
#'   test_fs_boruta()
#' }
test_fs_boruta <- function() {
  cat("Running UAT for fs_boruta...\n")
  
  # Test 1: Simple numeric data frame
  testthat::test_that("fs_boruta works with numeric data", {
    df1 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(1:5, 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    result1 <- fs_boruta(df1, "target")
    testthat::expect_type(result1$selected_features, "character")
    testthat::expect_true(is.list(result1$boruta_obj))
    testthat::expect_true("finalDecision" %in% names(result1$boruta_obj))
  })
  
  # Test 2: Data frame with categorical variables
  testthat::test_that("fs_boruta handles categorical variables", {
    df2 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    result2 <- fs_boruta(df2, "target")
    testthat::expect_type(result2$selected_features, "character")
    testthat::expect_true(is.list(result2$boruta_obj))
    testthat::expect_true("finalDecision" %in% names(result2$boruta_obj))
  })
  
  # Test 3: Data frame with date variables
  testthat::test_that("fs_boruta handles date variables", {
    df3 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE),
      C = seq(as.Date("2001-01-01"), by = "day", length.out = 100),
      target = sample(1:2, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    result3 <- fs_boruta(df3, "target")
    testthat::expect_type(result3$selected_features, "character")
    testthat::expect_true(is.list(result3$boruta_obj))
    testthat::expect_true("finalDecision" %in% names(result3$boruta_obj))
  })
  
  # Test 4: Error handling when target variable is missing
  testthat::test_that("fs_boruta stops when target variable is missing", {
    df4 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE)
    )
    testthat::expect_error(fs_boruta(df4, "target"),
                           "The target variable is not found in the provided data frame.")
  })
  
  # Test 5: Data frame with non-supported variable types
  testthat::test_that("fs_boruta handles non-numeric, non-factor variables", {
    df5 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = I(matrix(sample(1:5, 200, replace = TRUE), ncol = 2)),
      target = sample(1:2, 100, replace = TRUE)
    )
    testthat::expect_error(fs_boruta(df5, "target"), "Unsupported variable types found")
  })
  
  # Test 6: Limiting the number of selected features
  testthat::test_that("fs_boruta limits the number of features", {
    df6 <- data.frame(
      A = rnorm(100),
      B = rnorm(100),
      C = rnorm(100),
      D = rnorm(100),
      target = sample(1:2, 100, replace = TRUE)
    )
    result6 <- fs_boruta(df6, "target", cutoff_features = 2)
    testthat::expect_lte(length(result6$selected_features), 2)
  })
  
  # Test 7: Removing highly correlated features
  testthat::test_that("fs_boruta removes highly correlated features", {
    df7 <- data.frame(
      A = rnorm(100),
      B = rnorm(100),
      C = rnorm(100),
      D = rnorm(100)
    )
    # Introduce a feature highly correlated with A
    df7$E <- df7$A + rnorm(100, sd = 0.01)
    df7$target <- sample(1:2, 100, replace = TRUE)
    result7 <- fs_boruta(df7, "target", cutoff_cor = 0.9)
    # Check that both A and its highly correlated counterpart E are not both selected
    testthat::expect_false(all(c("A", "E") %in% result7$selected_features))
  })
  
  cat("UAT for fs_boruta completed.\n")
}

# Run tests
# test_fs_boruta()
