###############################################################################
# Testing Infrastructure - Boruta
###############################################################################

#' @title Unit Tests for fs_boruta Function
#'
#' @description
#' Runs a series of tests to verify that \code{fs_boruta} behaves as expected
#' on numeric, categorical, and date variables; and validates error handling,
#' feature caps, and correlation-pruning logic.
#'
#' @examples
#' \dontrun{
#'   test_fs_boruta()
#' }
test_fs_boruta <- function() {
  cat("Running UAT for fs_boruta...\n")
  
  # ---------------------------------------------------------------------------
  # Pre-flight: skip if Boruta is missing; some tests also need caret
  # ---------------------------------------------------------------------------
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("Package 'testthat' is required to run these tests.")
  }
  if (!requireNamespace("Boruta",   quietly = TRUE)) {
    testthat::skip("Package 'Boruta' is not installed; skipping fs_boruta UAT.")
    return(invisible(NULL))
  }
  
  have_caret <- requireNamespace("caret", quietly = TRUE)
  
  # A helper to run Boruta quietly and deterministically
  .fs <- function(df, target, ...) {
    # keep outputs reproducible across tests
    fs_boruta(df, target, seed = 42L, doTrace = 0, ...)
  }
  
  # ---------------------------------------------------------------------------
  # Test 1: Simple numeric data frame
  # ---------------------------------------------------------------------------
  testthat::test_that("fs_boruta works with numeric data", {
    set.seed(1)
    df1 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(1:5,  100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    result1 <- .fs(df1, "target")
    testthat::expect_type(result1$selected_features, "character")
    testthat::expect_true(inherits(result1$boruta_obj, "Boruta"))
    testthat::expect_true("finalDecision" %in% names(result1$boruta_obj))
  })
  
  # ---------------------------------------------------------------------------
  # Test 2: Data frame with categorical variables (character -> factor)
  # ---------------------------------------------------------------------------
  testthat::test_that("fs_boruta handles categorical variables", {
    set.seed(2)
    df2 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    result2 <- .fs(df2, "target")
    testthat::expect_type(result2$selected_features, "character")
    testthat::expect_true(inherits(result2$boruta_obj, "Boruta"))
  })
  
  # ---------------------------------------------------------------------------
  # Test 3: Data frame with date variables (Date -> numeric)
  # ---------------------------------------------------------------------------
  testthat::test_that("fs_boruta handles date variables", {
    set.seed(3)
    df3 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE),
      C = seq(as.Date("2001-01-01"), by = "day", length.out = 100),
      target = sample(1:2, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    result3 <- .fs(df3, "target")
    testthat::expect_type(result3$selected_features, "character")
    testthat::expect_true(inherits(result3$boruta_obj, "Boruta"))
  })
  
  # ---------------------------------------------------------------------------
  # Test 4: Error when target variable is missing
  # ---------------------------------------------------------------------------
  testthat::test_that("fs_boruta stops when target variable is missing", {
    df4 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE)
    )
    testthat::expect_error(
      fs_boruta(df4, "target"),
      "The target variable is not found in the provided data frame\\."
    )
  })
  
  # ---------------------------------------------------------------------------
  # Test 5: Error on unsupported variable types (e.g., matrix column)
  # ---------------------------------------------------------------------------
  testthat::test_that("fs_boruta errors on unsupported predictor types", {
    set.seed(5)
    df5 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = I(matrix(sample(1:5, 200, replace = TRUE), ncol = 2)),
      target = sample(1:2, 100, replace = TRUE)
    )
    # Expect the preprocessing error message prefix from preprocess_predictors()
    testthat::expect_error(
      fs_boruta(df5, "target"),
      "Unsupported variable types found in columns:"
    )
  })
  
  # ---------------------------------------------------------------------------
  # Test 6: Limiting the number of selected features
  # ---------------------------------------------------------------------------
  testthat::test_that("fs_boruta respects feature cap (cutoff_features)", {
    set.seed(6)
    df6 <- data.frame(
      A = rnorm(200),
      B = rnorm(200),
      C = rnorm(200),
      D = rnorm(200),
      target = sample(1:2, 200, replace = TRUE)
    )
    result6 <- .fs(df6, "target", cutoff_features = 2)
    testthat::expect_true(length(result6$selected_features) <= 2)
    testthat::expect_type(result6$selected_features, "character")
  })
  
  # ---------------------------------------------------------------------------
  # Test 7: Removing highly correlated features (requires 'caret')
  # ---------------------------------------------------------------------------
  testthat::test_that("fs_boruta removes highly correlated features when caret is available", {
    if (!have_caret) testthat::skip("Package 'caret' not installed; skipping correlation-pruning test.")
    
    set.seed(7)
    df7 <- data.frame(
      A = rnorm(200),
      B = rnorm(200),
      C = rnorm(200),
      D = rnorm(200)
    )
    # E ~ A + noise -> high correlation
    df7$E <- df7$A + rnorm(200, sd = 0.01)
    df7$target <- sample(1:2, 200, replace = TRUE)
    
    result7 <- .fs(df7, "target", cutoff_cor = 0.9)
    # Ensure not both A and its near-duplicate E survive
    testthat::expect_false(all(c("A", "E") %in% result7$selected_features))
  })
  
  # ---------------------------------------------------------------------------
  # Test 8: Skipping correlation-pruning when cutoff_cor = NULL
  # ---------------------------------------------------------------------------
  testthat::test_that("fs_boruta does not prune by correlation when cutoff_cor is NULL", {
    set.seed(8)
    df8 <- data.frame(
      A = rnorm(150),
      B = rnorm(150),
      target = sample(1:2, 150, replace = TRUE)
    )
    res_no_prune <- .fs(df8, "target", cutoff_cor = NULL)
    res_prune    <- .fs(df8, "target", cutoff_cor = 0.7)
    # We can't guarantee specific features, but with pruning enabled,
    # the selected set size should be <= the no-prune size.
    testthat::expect_true(length(res_prune$selected_features) <= length(res_no_prune$selected_features))
  })
  
  cat("UAT for fs_boruta completed.\n")
}

# Run tests
# test_fs_boruta()
