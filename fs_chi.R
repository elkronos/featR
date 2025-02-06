# Load necessary packages
library(data.table)
library(furrr)
library(future)
library(testthat)

#' Perform feature selection using the chi-square test
#'
#' @description
#' This function evaluates the association between categorical features and a target variable using the chi-square
#' test. It automatically handles low expected frequencies by switching to a simulation-based p-value estimation,
#' and it detects and omits missing values for each test.
#'
#' @param data A data frame containing the features and the target variable.
#' @param target_col A character string specifying the name of the target column.
#' @param sig_level A numeric significance level for the chi-square test (default is 0.05).
#' @param correct A logical value indicating whether to apply the continuity correction for 2x2 tables. If NULL,
#'   the function will automatically decide (default is TRUE for 2x2 tables).
#' @param apply_bonferroni Logical. Should Bonferroni correction be applied to account for multiple testing? (default is TRUE).
#' @param simulation_B An integer specifying the number of replicates to use for simulation-based p-value estimation
#'   when expected counts are low (default is 2000).
#' @param parallel Logical. If TRUE (default), the chi-square tests are computed in parallel using future.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{significant_features}{A character vector with the names of features that are significantly associated with the target.}
#'   \item{p_values}{A named numeric vector of (adjusted) p-values for each tested feature.}
#' }
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   target <- factor(rep(c("Yes", "No"), each = 250))
#'   feature_1 <- factor(c(rep(c("A", "B"), times = c(200, 50)),
#'                           rep(c("A", "B", "C"), times = c(50, 100, 100))))
#'   feature_2 <- factor(sample(c("X", "Y"), 500, replace = TRUE))
#'   feature_3 <- factor(c(rep(c("M", "N"), times = c(150, 100)),
#'                           rep(c("M", "N"), times = c(100, 150))))
#'   data <- data.frame(feature_1 = feature_1, feature_2 = feature_2,
#'                      feature_3 = feature_3, target = target)
#'
#'   # Run the feature selection
#'   results <- fs_chi(data, "target")
#'   print(results$significant_features)
#'   print(results$p_values)
#' }
#'
#' @importFrom data.table as.data.table
#' @importFrom future plan multisession
#' @importFrom furrr future_map_dbl
#' @importFrom stats chisq.test
#' @export
fs_chi <- function(data, target_col, 
                   sig_level = 0.05, 
                   correct = NULL,
                   apply_bonferroni = TRUE,
                   simulation_B = 2000,
                   parallel = TRUE) {
  
  # --- Input validation ---
  if (!is.data.frame(data)) 
    stop("`data` must be a data frame.")
  if (!target_col %in% names(data)) 
    stop("`target_col` must be a column in `data`.")
  
  # Convert to data.table for efficiency
  dt <- as.data.table(data)
  
  # Convert character columns to factors
  char_cols <- names(dt)[sapply(dt, is.character)]
  if (length(char_cols) > 0) {
    dt[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]
  }
  
  # Identify categorical features (factors) excluding the target column
  feature_cols <- setdiff(names(dt), target_col)
  feature_cols <- feature_cols[sapply(dt[, ..feature_cols], is.factor)]
  
  if (length(feature_cols) == 0) {
    message("No categorical features found for testing.")
    return(list(significant_features = character(0), 
                p_values = setNames(numeric(0), character(0))))
  }
  
  # --- Helper function to compute the p-value for a single feature ---
  compute_chi_pvalue <- function(feature) {
    # Handle missing values: omit rows with NA in either the feature or the target
    valid_idx <- !is.na(dt[[feature]]) & !is.na(dt[[target_col]])
    if (any(!valid_idx)) {
      message(sprintf("Missing values detected in feature '%s' or target '%s'. Omitting these rows.", 
                      feature, target_col))
    }
    dt_sub <- dt[valid_idx, ]
    
    # Create contingency table
    cont_table <- table(dt_sub[[feature]], dt_sub[[target_col]])
    
    # First, try the chi-square test without correction to obtain expected counts
    init_test <- suppressWarnings(chisq.test(cont_table, correct = FALSE))
    expected <- init_test$expected
    
    # Determine if simulation is needed (expected count < 5)
    if (any(expected < 5)) {
      message(sprintf("Low expected counts (<5) detected for feature '%s'. Using simulation with B = %d.", 
                      feature, simulation_B))
      test_res <- chisq.test(cont_table, simulate.p.value = TRUE, B = simulation_B)
    } else {
      # Determine if continuity correction should be applied (only applicable to 2x2 tables)
      corr <- if (nrow(cont_table) == 2 && ncol(cont_table) == 2) {
        if (is.null(correct)) TRUE else correct
      } else {
        FALSE
      }
      test_res <- chisq.test(cont_table, correct = corr)
    }
    return(test_res$p.value)
  }
  
  # --- Compute p-values for all features ---
  if (parallel) {
    # Use parallel processing with furrr
    p_values <- future_map_dbl(feature_cols, compute_chi_pvalue, .options = furrr_options(seed = TRUE))
  } else {
    # Use a simple loop (or lapply) for sequential processing
    p_values <- sapply(feature_cols, compute_chi_pvalue)
  }
  names(p_values) <- feature_cols
  
  # --- Adjust p-values if requested ---
  if (apply_bonferroni && length(p_values) > 0) {
    adj_p_values <- pmin(p_values * length(p_values), 1)
  } else {
    adj_p_values <- p_values
  }
  
  # Identify significant features based on the provided significance level
  sig_features <- feature_cols[!is.na(adj_p_values) & (adj_p_values < sig_level)]
  
  return(list(significant_features = sig_features, p_values = adj_p_values))
}

# Set up parallel processing (if desired)
plan(multisession)

## ---------------------------
## Unit Tests using testthat
## ---------------------------
test_fs_chi <- function() {
  cat("Running UAT for fs_chi...\n")
  
  # Test 1: Data frame input and target column validation
  test_that("fs_chi errors if data is not a data frame", {
    expect_error(fs_chi("not a data frame", "target"))
  })
  
  test_that("fs_chi errors if target column is missing", {
    df <- data.frame(A = factor(c("a", "b")), B = factor(c("x", "y")))
    expect_error(fs_chi(df, "target"))
  })
  
  # Test 2: No categorical features available
  test_that("fs_chi returns empty result if no categorical features", {
    df <- data.frame(target = factor(c("Yes", "No")), X = 1:2)
    res <- fs_chi(df, "target")
    expect_equal(length(res$significant_features), 0)
    expect_equal(length(res$p_values), 0)
  })
  
  # Test 3: Feature with low frequency counts (not significant)
  test_that("fs_chi handles low frequency counts without significant association", {
    set.seed(123)
    target <- factor(rep(c("Yes", "No"), each = 250))
    feature_low_freq <- factor(c(rep("A", 495), rep("B", 5)))
    df_low <- data.frame(feature_low_freq = feature_low_freq, target = target)
    
    expect_message(
      res_low <- fs_chi(df_low, "target"),
      regexp = "Low expected counts.*feature 'feature_low_freq'"
    )
    expect_length(res_low$significant_features, 0)
    expect_true("feature_low_freq" %in% names(res_low$p_values))
  })
  
  # Test 4: Feature with low frequency counts (significant association)
  test_that("fs_chi detects significant association with low frequency counts", {
    set.seed(123)
    target <- factor(c(rep("Yes", 495), rep("No", 5)))
    feature_low_freq <- factor(c(rep("A", 495), rep("B", 5)))
    df_low_sig <- data.frame(feature_low_freq = feature_low_freq, target = target)
    
    expect_message(
      res_low_sig <- fs_chi(df_low_sig, "target"),
      regexp = "Low expected counts.*feature 'feature_low_freq'"
    )
    expect_equal(length(res_low_sig$significant_features), 1)
    expect_true("feature_low_freq" %in% res_low_sig$significant_features)
  })
  
  cat("All UAT for fs_chi completed.\n")
}

# Run the unit tests
# test_fs_chi()
