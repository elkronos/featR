###############################################################################
# Testing Infrastructure - Correlations
###############################################################################

#' Test fs_correlation Function
#'
#' This function runs a suite of tests (using the \code{testthat} framework) to verify that
#' the \code{fs_correlation} function behaves as expected with various parameters and datasets.
#'
#' @return No return value. Test results are printed to the console.
#'
#' @examples
#' \dontrun{
#' test_fs_correlation()
#' }
test_fs_correlation <- function() {
  cat("Running UAT for fs_correlation...\n")
  
  # Test 1: Basic Functionality with Default Parameters
  testthat::test_that("fs_correlation works with default parameters", {
    data <- mtcars
    threshold <- 0.7
    result <- fs_correlation(data, threshold)
    testthat::expect_true(is.matrix(result$corr_matrix) || is.data.frame(result$corr_matrix))
    testthat::expect_type(result$selected_vars, "character")
    testthat::expect_true(length(result$selected_vars) > 0)
  })
  
  # Test 2: Different Correlation Methods with Appropriate Data
  testthat::test_that("fs_correlation works with different correlation methods", {
    data <- mtcars
    threshold <- 0.7
    
    # Pearson
    result_pearson <- fs_correlation(data, threshold, method = "pearson")
    testthat::expect_true(is.matrix(result_pearson$corr_matrix) || is.data.frame(result_pearson$corr_matrix))
    testthat::expect_type(result_pearson$selected_vars, "character")
    
    # Spearman
    result_spearman <- fs_correlation(data, threshold, method = "spearman")
    testthat::expect_true(is.matrix(result_spearman$corr_matrix) || is.data.frame(result_spearman$corr_matrix))
    testthat::expect_type(result_spearman$selected_vars, "character")
    
    # Kendall
    result_kendall <- fs_correlation(data, threshold, method = "kendall")
    testthat::expect_true(is.matrix(result_kendall$corr_matrix) || is.data.frame(result_kendall$corr_matrix))
    testthat::expect_type(result_kendall$selected_vars, "character")
  })
  
  # Test 3: Point-Biserial Correlation with Mixed Data
  testthat::test_that("fs_correlation works with point-biserial correlation", {
    set.seed(123)
    continuous_var <- rnorm(100)
    dichotomous_var <- ifelse(continuous_var > 0, 1, 0)
    data_pb <- data.frame(continuous_var = continuous_var,
                          dichotomous_var = dichotomous_var)
    result_pb <- fs_correlation(data_pb, threshold = 0.1, method = "pointbiserial")
    testthat::expect_true(is.matrix(result_pb$corr_matrix) || is.data.frame(result_pb$corr_matrix))
    testthat::expect_type(result_pb$selected_vars, "character")
    testthat::expect_true(length(result_pb$selected_vars) > 0)
  })
  
  # Test 4: Polychoric Correlation with Ordered Factors
  testthat::test_that("fs_correlation works with polychoric correlation", {
    set.seed(123)
    ordinal_values <- sample(1:5, 100, replace = TRUE)
    ordinal_var1 <- factor(ordinal_values, ordered = TRUE)
    ordinal_var2 <- factor(ordinal_values + sample(c(-1, 0, 1), 100, replace = TRUE),
                           levels = 1:5, ordered = TRUE)
    data_poly <- data.frame(ordinal_var1 = ordinal_var1,
                            ordinal_var2 = ordinal_var2)
    result_poly <- fs_correlation(data_poly, threshold = 0.1, method = "polychoric")
    testthat::expect_true(is.matrix(result_poly$corr_matrix) || is.data.frame(result_poly$corr_matrix))
    testthat::expect_type(result_poly$selected_vars, "character")
    testthat::expect_true(length(result_poly$selected_vars) > 0)
  })
  
  # Test 5: Handling Missing Values
  testthat::test_that("fs_correlation handles missing values", {
    data_na <- mtcars
    data_na[1:5, 1] <- NA
    result_na <- fs_correlation(data_na, threshold = 0.7, na.rm = TRUE)
    testthat::expect_true(is.matrix(result_na$corr_matrix) || is.data.frame(result_na$corr_matrix))
    testthat::expect_type(result_na$selected_vars, "character")
  })
  
  # Test 6: Parallel Processing
  testthat::test_that("fs_correlation works with parallel processing", {
    set.seed(123)
    continuous_var <- rnorm(100)
    dichotomous_var <- ifelse(continuous_var > 0, 1, 0)
    data_pb <- data.frame(continuous_var = continuous_var,
                          dichotomous_var = dichotomous_var)
    result_parallel <- fs_correlation(data_pb, threshold = 0.1, method = "pointbiserial",
                                      parallel = TRUE, n_cores = 2, verbose = TRUE)
    testthat::expect_true(is.matrix(result_parallel$corr_matrix) || is.data.frame(result_parallel$corr_matrix))
    testthat::expect_type(result_parallel$selected_vars, "character")
    testthat::expect_true(length(result_parallel$selected_vars) > 0)
  })
  
  # Test 7: Data Sampling
  testthat::test_that("fs_correlation works with data sampling", {
    data <- mtcars
    result_sampling <- fs_correlation(data, threshold = 0.7, sample_frac = 0.5, seed = 42)
    testthat::expect_true(is.matrix(result_sampling$corr_matrix) || is.data.frame(result_sampling$corr_matrix))
    testthat::expect_type(result_sampling$selected_vars, "character")
  })
  
  # Test 8: Output Format as Data Frame
  testthat::test_that("fs_correlation returns data.frame when output_format is 'data.frame'", {
    data <- mtcars
    result_df <- fs_correlation(data, threshold = 0.7, output_format = "data.frame")
    testthat::expect_true(is.data.frame(result_df$corr_matrix))
    testthat::expect_type(result_df$selected_vars, "character")
  })
  
  # Test 9: Custom Diagonal Value
  testthat::test_that("fs_correlation sets custom diagonal value", {
    data <- mtcars
    result_diag <- fs_correlation(data, threshold = 0.7, diag_value = NA)
    testthat::expect_true(all(is.na(diag(result_diag$corr_matrix))))
  })
  
  # Test 10: Custom No Variables Message
  testthat::test_that("fs_correlation uses custom no variables message", {
    data <- mtcars
    custom_msg <- "Custom message: No variables selected."
    testthat::expect_message(fs_correlation(data, 0.95, no_vars_message = custom_msg), custom_msg)
  })
  
  # Test 11: Invalid Inputs
  testthat::test_that("fs_correlation handles invalid inputs", {
    testthat::expect_error(fs_correlation("not a data frame", 0.7),
                           "The 'data' argument must be a data frame or a matrix.")
    testthat::expect_error(fs_correlation(mtcars, 1.5),
                           "The 'threshold' argument must be a single numeric value between 0 and 1.")
    testthat::expect_error(fs_correlation(mtcars, 0.7, method = "invalid_method"),
                           "Invalid correlation method.")
  })
  
  # Test 12: Verbose Output
  testthat::test_that("fs_correlation provides verbose output when verbose = TRUE", {
    set.seed(123)
    continuous_var <- rnorm(100)
    dichotomous_var <- ifelse(continuous_var > 0, 1, 0)
    data_pb <- data.frame(continuous_var = continuous_var,
                          dichotomous_var = dichotomous_var)
    testthat::expect_message(fs_correlation(data_pb, threshold = 0.1, method = "pointbiserial", verbose = TRUE),
                             "Calculating point-biserial correlation between:")
  })
  
  cat("UAT for fs_correlation completed.\n")
}

# Run tests
# test_fs_correlation()
