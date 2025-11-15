###############################################################################
# Testing Infrastructure - Correlations (run via test_fs_correlation())
###############################################################################

test_fs_correlation <- function() {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("Package 'testthat' is required to run tests. Please install it.")
  }
  
  cat("Running UAT for fs_correlation...\n")
  
  # ----------------------------------------------------------------------------
  # Test 1: Basic functionality with defaults (pearson)
  # ----------------------------------------------------------------------------
  testthat::test_that("basic functionality with defaults (pearson)", {
    data <- mtcars
    res  <- fs_correlation(data, threshold = 0.7)
    testthat::expect_true(is.matrix(res$corr_matrix) || is.data.frame(res$corr_matrix))
    testthat::expect_type(res$selected_vars, "character")
    testthat::expect_true(length(res$selected_vars) >= 0)
  })
  
  # ----------------------------------------------------------------------------
  # Test 2: Different correlation methods: pearson, spearman, kendall
  # ----------------------------------------------------------------------------
  testthat::test_that("different correlation methods: pearson, spearman, kendall", {
    data <- mtcars
    
    r1 <- fs_correlation(data, 0.6, method = "pearson")
    r2 <- fs_correlation(data, 0.6, method = "spearman")
    r3 <- fs_correlation(data, 0.6, method = "kendall")
    
    for (r in list(r1, r2, r3)) {
      testthat::expect_true(is.matrix(r$corr_matrix) || is.data.frame(r$corr_matrix))
      testthat::expect_type(r$selected_vars, "character")
    }
  })
  
  # ----------------------------------------------------------------------------
  # Test 3: Point-biserial (sequential) on mixed data
  # ----------------------------------------------------------------------------
  testthat::test_that("pointbiserial works on mixed data (sequential)", {
    testthat::skip_if_not_installed("ltm")
    
    set.seed(123)
    x <- rnorm(200)
    y <- ifelse(x > 0, 1, 0)
    df <- data.frame(x = x, y = y)
    
    res <- fs_correlation(df, threshold = 0.05, method = "pointbiserial", na.rm = TRUE)
    testthat::expect_true(is.matrix(res$corr_matrix) || is.data.frame(res$corr_matrix))
    testthat::expect_type(res$selected_vars, "character")
    testthat::expect_true(length(res$selected_vars) >= 0)
  })
  
  # ----------------------------------------------------------------------------
  # Test 4: Point-biserial (parallel) path
  # ----------------------------------------------------------------------------
  testthat::test_that("pointbiserial parallel path (skips if deps unavailable)", {
    testthat::skip_if_not_installed("ltm")
    testthat::skip_if_not_installed("doParallel")
    testthat::skip_if_not_installed("foreach")
    
    set.seed(42)
    x <- rnorm(300)
    y <- ifelse(x + rnorm(300, sd = 0.2) > 0, 1, 0)
    z <- rnorm(300)
    df <- data.frame(x = x, y = y, z = z)
    
    testthat::expect_message(
      res <- fs_correlation(df, threshold = 0.05, method = "pointbiserial",
                            parallel = TRUE, n_cores = 2, na.rm = TRUE, verbose = TRUE),
      regexp = "point-biserial|parallel",
      fixed  = FALSE
    )
    testthat::expect_true(is.matrix(res$corr_matrix) || is.data.frame(res$corr_matrix))
    testthat::expect_type(res$selected_vars, "character")
  })
  
  # ----------------------------------------------------------------------------
  # Test 5: Point-biserial warns & returns NA off-diagonal when no valid pairs
  # ----------------------------------------------------------------------------
  testthat::test_that("pointbiserial: warns and returns NA off-diagonal when no valid pairs", {
    testthat::skip_if_not_installed("ltm")
    # Only continuous variables => no dichotomous
    df <- data.frame(a = rnorm(50), b = rnorm(50))
    testthat::expect_warning(
      res <- fs_correlation(df, threshold = 0.1, method = "pointbiserial"),
      regexp = "No valid continuous–dichotomous pairs",
      fixed  = FALSE
    )
    m <- res$corr_matrix
    off_diag <- m[row(m) != col(m)]
    testthat::expect_true(all(is.na(off_diag)))
    testthat::expect_true(all(diag(m) == 0, na.rm = TRUE))
    # If you prefer "all NA", you can assert this instead by calling with diag_value = NA_real_:
    res2 <- fs_correlation(df, threshold = 0.1, method = "pointbiserial", diag_value = NA_real_)
    testthat::expect_true(all(is.na(res2$corr_matrix)))
  })
  
  # ----------------------------------------------------------------------------
  # Test 6: Polychoric on ordered factors
  # ----------------------------------------------------------------------------
  testthat::test_that("polychoric works on ordered factors", {
    testthat::skip_if_not_installed("polycor")
    
    set.seed(123)
    o1 <- factor(sample(1:5, 120, replace = TRUE), ordered = TRUE)
    o2 <- factor(
      pmin(
        pmax(as.integer(o1) + sample(c(-1, 0, 1), 120, replace = TRUE), 1),
        5
      ),
      levels = 1:5, ordered = TRUE
    )
    df <- data.frame(o1 = o1, o2 = o2)
    
    res <- fs_correlation(df, threshold = 0.1, method = "polychoric")
    testthat::expect_true(is.matrix(res$corr_matrix) || is.data.frame(res$corr_matrix))
    testthat::expect_type(res$selected_vars, "character")
    testthat::expect_true(length(res$selected_vars) >= 0)
  })
  
  # ----------------------------------------------------------------------------
  # Test 7: Handling missing values (pairwise complete obs path)
  # ----------------------------------------------------------------------------
  testthat::test_that("handles missing values when na.rm = TRUE", {
    data_na <- mtcars
    data_na[1:5, 1] <- NA
    res <- fs_correlation(data_na, threshold = 0.7, na.rm = TRUE)
    testthat::expect_true(is.matrix(res$corr_matrix) || is.data.frame(res$corr_matrix))
    testthat::expect_type(res$selected_vars, "character")
  })
  
  # ----------------------------------------------------------------------------
  # Test 8: Data sampling
  # ----------------------------------------------------------------------------
  testthat::test_that("data sampling reduces rows but runs", {
    data <- mtcars
    res  <- fs_correlation(data, threshold = 0.7, sample_frac = 0.5, seed = 42, verbose = TRUE)
    testthat::expect_true(is.matrix(res$corr_matrix) || is.data.frame(res$corr_matrix))
    testthat::expect_type(res$selected_vars, "character")
  })
  
  # ----------------------------------------------------------------------------
  # Test 9: Output format = data.frame
  # ----------------------------------------------------------------------------
  testthat::test_that("returns data.frame when output_format = 'data.frame'", {
    data <- mtcars
    res  <- fs_correlation(data, threshold = 0.7, output_format = "data.frame")
    testthat::expect_true(is.data.frame(res$corr_matrix))
    testthat::expect_named(res$corr_matrix, c("Var1", "Var2", "Correlation"))
    testthat::expect_type(res$selected_vars, "character")
  })
  
  # ----------------------------------------------------------------------------
  # Test 10: Custom diagonal value
  # ----------------------------------------------------------------------------
  testthat::test_that("sets custom diagonal value", {
    data <- mtcars
    res  <- fs_correlation(data, threshold = 0.7, diag_value = NA_real_)
    testthat::expect_true(all(is.na(diag(res$corr_matrix))))
  })
  
  # ----------------------------------------------------------------------------
  # Test 11: Custom 'no variables' message
  # ----------------------------------------------------------------------------
  testthat::test_that("uses custom 'no variables' message", {
    data <- mtcars
    custom_msg <- "Custom message: No variables selected."
    testthat::expect_message(
      fs_correlation(data, threshold = 0.9999, no_vars_message = custom_msg),
      custom_msg
    )
  })
  
  # ----------------------------------------------------------------------------
  # Test 12: Invalid inputs (error messages match refactor)
  # ----------------------------------------------------------------------------
  testthat::test_that("invalid inputs produce clear errors", {
    # Data type
    testthat::expect_error(
      fs_correlation("not a data frame", 0.7),
      regexp = "`data` must be a data frame or matrix\\.",
      fixed  = FALSE
    )
    
    # Threshold out of range
    testthat::expect_error(
      fs_correlation(mtcars, 1.5),
      regexp = "`threshold` must be a single finite numeric value in \\[0, 1\\]\\.",
      fixed  = FALSE
    )
    
    # Invalid method
    testthat::expect_error(
      fs_correlation(mtcars, 0.7, method = "invalid_method"),
      regexp = "Invalid `method`\\.",
      fixed  = FALSE
    )
    
    # Sample fraction out of range
    testthat::expect_error(
      fs_correlation(mtcars, 0.7, sample_frac = 0),
      regexp = "`sample_frac` must be a single finite numeric value in \\(0, 1\\]\\.",
      fixed  = FALSE
    )
    
    # Sample fraction NA
    testthat::expect_error(
      fs_correlation(mtcars, 0.7, sample_frac = NA_real_),
      regexp = "`sample_frac` must be a single finite numeric value in \\(0, 1\\]\\.",
      fixed  = FALSE
    )
    
    # n_cores < 1
    testthat::expect_error(
      fs_correlation(mtcars, 0.7, n_cores = 0),
      regexp = "`n_cores` must be a finite numeric value >= 1\\.",
      fixed  = FALSE
    )
    
    # diag_value invalid type
    testthat::expect_error(
      fs_correlation(mtcars, 0.7, diag_value = "x"),
      regexp = "`diag_value` must be a single numeric value or NA\\.",
      fixed  = FALSE
    )
    
    # Invalid seed type when sampling
    testthat::expect_error(
      fs_correlation(mtcars, 0.7, sample_frac = 0.5, seed = "not_numeric"),
      regexp = "`seed` must be a single numeric value if provided\\.",
      fixed  = FALSE
    )
    
    # Zero-row data
    empty_df <- mtcars[0, ]
    testthat::expect_error(
      fs_correlation(empty_df, 0.7),
      regexp = "`data` must have at least one row\\.",
      fixed  = FALSE
    )
  })
  
  # ----------------------------------------------------------------------------
  # Test 13: Verbose output smoke tests
  # ----------------------------------------------------------------------------
  testthat::test_that("verbose output appears where expected", {
    # Classic method verbose
    testthat::expect_message(
      fs_correlation(mtcars, threshold = 0.3, method = "pearson", na.rm = TRUE, verbose = TRUE),
      regexp = "Calculating pearson correlation matrix",
      fixed  = FALSE
    )
    
    # Point-biserial sequential verbose
    testthat::skip_if_not_installed("ltm")
    set.seed(123)
    x <- rnorm(120); y <- ifelse(x > 0, 1, 0)
    df <- data.frame(x = x, y = y)
    testthat::expect_message(
      fs_correlation(df, threshold = 0.05, method = "pointbiserial", verbose = TRUE),
      regexp = "point-biserial",
      fixed  = FALSE
    )
    
    # Polychoric verbose
    testthat::skip_if_not_installed("polycor")
    set.seed(1)
    o1 <- factor(sample(1:3, 80, replace = TRUE), ordered = TRUE)
    o2 <- factor(sample(1:3, 80, replace = TRUE), ordered = TRUE)
    df2 <- data.frame(o1 = o1, o2 = o2)
    testthat::expect_message(
      fs_correlation(df2, threshold = 0.1, method = "polychoric", verbose = TRUE),
      regexp = "polychoric.*polycor::hetcor",
      fixed  = FALSE
    )
  })
  
  # ----------------------------------------------------------------------------
  # Test 14: Polychoric with missing values and na.rm = TRUE
  # ----------------------------------------------------------------------------
  testthat::test_that("polychoric handles missing values when na.rm = TRUE", {
    testthat::skip_if_not_installed("polycor")
    
    set.seed(2)
    o1 <- factor(sample(1:4, 100, replace = TRUE), ordered = TRUE)
    o2 <- factor(sample(1:4, 100, replace = TRUE), ordered = TRUE)
    df <- data.frame(o1 = o1, o2 = o2)
    df$o1[1:10] <- NA
    df$o2[5:15] <- NA
    
    res <- fs_correlation(df, threshold = 0.1, method = "polychoric", na.rm = TRUE)
    testthat::expect_true(is.matrix(res$corr_matrix) || is.data.frame(res$corr_matrix))
    testthat::expect_type(res$selected_vars, "character")
  })
  
  cat("UAT for fs_correlation completed.\n")
}

# Run tests interactively:
# test_fs_correlation()
