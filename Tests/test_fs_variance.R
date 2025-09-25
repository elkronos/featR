###############################################################################
# Testing Infrastructure - Variance Thresholding (refactored for new API)
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
    expect_equal(nrow(result), nrow(data_mat))
  })
  
  test_that("Numeric data frame input works with default parameters", {
    set.seed(123)
    df <- data.frame(matrix(rnorm(1000), ncol = 10))
    result <- fs_variance(df, threshold = 0.5)
    expect_true(is.matrix(result))
    expect_true(ncol(result) <= ncol(df))
    expect_equal(nrow(result), nrow(df))
  })
  
  test_that("data.table input works with default parameters", {
    set.seed(123)
    dt_input <- data.table::as.data.table(matrix(rnorm(1000), ncol = 10))
    result <- fs_variance(dt_input, threshold = 0.5)
    expect_true(is.matrix(result))
    expect_true(ncol(result) <= ncol(dt_input))
    expect_equal(nrow(result), nrow(dt_input))
  })
  
  test_that("Empty selection returns 0-column matrix (not NULL) with defaults", {
    set.seed(123)
    df_low_var <- data.frame(matrix(rnorm(1000, mean = 0, sd = 0.1), ncol = 10))
    result <- expect_warning(
      fs_variance(df_low_var, threshold = 0.5),
      "No features meet the specified variance criteria\\."
    )
    expect_true(is.matrix(result))
    expect_equal(ncol(result), 0)
    expect_equal(nrow(result), nrow(df_low_var))
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
    expect_error(
      fs_variance(df_non_numeric, threshold = 0.5),
      "All columns of the data frame must be numeric\\."
    )
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
    expect_true(ncol(result) <= ncol(df_missing))
  })
  
  test_that("Low-variance matrix returns 0-column matrix with defaults", {
    set.seed(123)
    data_low_var <- matrix(rnorm(1000, mean = 0, sd = 0.1), ncol = 10)
    result <- expect_warning(
      fs_variance(data_low_var, threshold = 1),
      "No features meet the specified variance criteria\\."
    )
    expect_true(is.matrix(result))
    expect_equal(ncol(result), 0)
    expect_equal(nrow(result), nrow(data_low_var))
  })
  
  test_that("Invalid threshold type triggers error", {
    df <- data.frame(matrix(rnorm(1000), ncol = 10))
    expect_error(
      fs_variance(df, threshold = "invalid"),
      "`threshold` must be a single non-negative, finite numeric value\\."
    )
  })
  
  test_that("Negative threshold triggers error", {
    df <- data.frame(matrix(rnorm(1000), ncol = 10))
    expect_error(
      fs_variance(df, threshold = -1),
      "`threshold` must be a single non-negative, finite numeric value\\."
    )
  })
  
  # ---- New API options ------------------------------------------------------
  
  test_that("action='remove' with direction='above' keeps <= threshold", {
    set.seed(123)
    data_mat <- matrix(rnorm(1000), ncol = 10)
    result <- expect_warning(
      fs_variance(
        data_mat,
        threshold = 0.5,
        action = "remove",
        direction = "above",
        include_equal = TRUE # allow <= 0.5
      ),
      "No features meet the specified variance criteria\\.",
      fixed = FALSE
    )
    original_variances <- apply(data_mat, 2, var)
    kept_cols <- colnames(result)
    if (length(kept_cols) > 0) {
      expect_true(all(original_variances[kept_cols] <= 0.5 + 1e-12))
    } else {
      succeed()
    }
  })
  
  test_that("action='keep' with direction='below' keeps strictly less than threshold", {
    set.seed(123)
    data_mat <- matrix(rnorm(1000, mean = 0, sd = 0.5), ncol = 10)
    result <- expect_warning(
      fs_variance(
        data_mat,
        threshold = 0.1,
        action = "keep",
        direction = "below",
        include_equal = FALSE
      ),
      "No features meet the specified variance criteria\\.",
      fixed = FALSE
    )
    original_variances <- apply(data_mat, 2, var)
    kept_cols <- colnames(result)
    if (length(kept_cols) > 0) {
      expect_true(all(original_variances[kept_cols] < 0.1))
    } else {
      succeed()
    }
  })
  
  test_that("include_equal toggles boundary behavior", {
    set.seed(42)
    # Construct a column with approx variance ~ threshold
    x <- rnorm(100, sd = sqrt(0.5))
    y <- rnorm(100, sd = 1)
    m <- cbind(x, y)
    colnames(m) <- c("near", "high")
    
    res_excl <- fs_variance(m, threshold = 0.5, include_equal = FALSE)
    res_incl <- fs_variance(m, threshold = 0.5, include_equal = TRUE)
    
    expect_true(ncol(res_incl) >= ncol(res_excl)) # inclusive should keep >=
  })
  
  test_that("return='names' / 'indices' / 'mask' / 'dt' / 'data.frame' / 'list' work", {
    set.seed(123)
    X <- matrix(rnorm(1000), ncol = 10)
    out_names  <- fs_variance(X, 0.5, return = "names")
    out_idx    <- fs_variance(X, 0.5, return = "indices")
    out_mask   <- fs_variance(X, 0.5, return = "mask")
    out_dt     <- fs_variance(X, 0.5, return = "dt")
    out_df     <- fs_variance(X, 0.5, return = "data.frame")
    out_list   <- fs_variance(X, 0.5, return = "list")
    
    expect_type(out_names, "character")
    expect_type(out_idx, "integer")
    expect_type(out_mask, "logical")
    expect_true(data.table::is.data.table(out_dt))   # <-- fixed (S3, not S4)
    expect_s3_class(out_df, "data.frame")
    expect_type(out_list, "list")
    expect_true(all(c("filtered","mask","indices","names","variances","meta") %in% names(out_list)))
    expect_true(is.matrix(out_list$filtered))
    expect_true(length(out_list$variances) == ncol(X))
    expect_true(is.list(out_list$meta))
  })
  
  test_that("Columns with all-NA variance are not selected when action='keep'", {
    dt <- data.table::data.table(
      a = rnorm(50),
      b = rep(NA_real_, 50)
    )
    # keep above 0 (b has NA variance -> should not be kept)
    res <- fs_variance(dt, threshold = 0, direction = "above", action = "keep")
    expect_false("b" %in% colnames(res))
  })
  
  cat("All unit tests completed successfully.\n")
}

# Uncomment the line below to run the tests
# test_fs_variance()
