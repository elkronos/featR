# Load necessary packages
library(data.table)
library(furrr)
library(future)
library(testthat)

# Define the fs_chi function
#' @title Perform feature selection using chi-square test
#'
#' @description
#' This function evaluates the association between categorical features and a target variable using the chi-square
#' test. It handles cases with minimum frequencies in the contingency table less than 5 by issuing a message and
#' adjusting the p-value calculation accordingly. Additionally, it detects and handles missing values within the features.
#'
#' @param data A data frame containing the feature and target columns.
#' @param target_col A character string indicating the name of the target column.
#' @param sig_level A numeric value specifying the significance level for the chi-square test (default is 0.05).
#' @param correct A logical value indicating whether to apply a continuity correction to the chi-square test. If set to NULL, the function will determine if a correction is needed based on the expected frequencies. Default is TRUE.
#' @param apply_bonferroni A logical value indicating whether to apply the Bonferroni correction for multiple testing (default is TRUE).
#'
#' @return A list containing:
#' * `significant_features`: A character vector of the statistically significant categorical feature names.
#' * `p_values`: A named numeric vector indicating the p-values from the chi-square tests for each feature.
#'
#' @examples
#' \dontrun{
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Create features and target
#' target <- factor(rep(c("Yes", "No"), each = 250))
#' feature_1 <- factor(c(rep(c("A", "B"), times = c(200, 50)),
#'                       rep(c("A", "B", "C"), times = c(50, 100, 100))))
#' feature_2 <- factor(sample(c("X", "Y"), 500, replace = TRUE))
#' feature_3 <- factor(c(rep(c("M", "N"), times = c(150, 100)),
#'                       rep(c("M", "N"), times = c(100, 150))))
#' data <- data.frame(feature_1 = feature_1, feature_2 = feature_2,
#'                    feature_3 = feature_3, target = target)
#'
#' # Use the fs_chi function on this dataset
#' results <- fs_chi(data, "target")
#'
#' # Print the significant features and their p-values
#' print(results$significant_features)
#' print(results$p_values)
#' }
#' @seealso 
#' \code{\link[stats]{chisq.test}} for chi-square testing.
#'
#' @importFrom data.table as.data.table
#' @importFrom future plan multisession
#' @importFrom furrr future_map_dbl
#' @importFrom stats chisq.test
#' @export
fs_chi <- function(data, target_col, sig_level = 0.05, correct = NULL, apply_bonferroni = TRUE) {
  
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (!target_col %in% names(data)) stop("target_col must be a column in data.")
  
  # Convert to data.table
  data <- as.data.table(data)
  
  # Convert character columns to factors
  cols_to_convert <- names(data)[sapply(data, is.character)]
  data[, (cols_to_convert) := lapply(.SD, as.factor), .SDcols = cols_to_convert]
  
  # Identify categorical feature columns
  feature_cols <- setdiff(names(data), target_col)
  feature_cols <- feature_cols[sapply(data[, ..feature_cols], is.factor)]
  
  # Check if there are any categorical features
  if (length(feature_cols) == 0) {
    message("No categorical features found.")
    return(list(significant_features = character(0), p_values = setNames(numeric(0), character(0))))
  }
  
  calculate_p_value <- function(feature) {
    # Check for missing values
    if (any(is.na(data[[feature]])) || any(is.na(data[[target_col]]))) {
      message(paste("Missing values detected in feature or target for", feature, ". Omitting missing data from analysis."))
      valid_idx <- !is.na(data[[feature]]) & !is.na(data[[target_col]])
      data_subset <- data[valid_idx, ]
    } else {
      data_subset <- data
    }
    
    cont_table <- table(data_subset[[feature]], data_subset[[target_col]])
    
    # Compute expected counts
    test_result <- suppressWarnings(chisq.test(cont_table, correct = FALSE))
    expected_counts <- test_result$expected
    
    if (any(expected_counts < 5)) {
      message(paste("Expected counts less than 5 detected in the contingency table for feature", feature, ". Chi-squared approximation may be inaccurate."))
      # Use simulation for p-value estimation
      test_result <- chisq.test(cont_table, simulate.p.value = TRUE, B = 2000)
    } else {
      # Apply continuity correction only for 2x2 tables
      if (nrow(cont_table) == 2 && ncol(cont_table) == 2) {
        correct_needed <- ifelse(is.null(correct), TRUE, correct)
      } else {
        correct_needed <- FALSE
      }
      test_result <- chisq.test(cont_table, correct = correct_needed)
    }
    
    return(test_result$p.value)
  }
  
  # Calculate p-values
  chi_square_results <- future_map_dbl(feature_cols, calculate_p_value, .options = furrr_options(seed = TRUE))
  
  # Apply Bonferroni correction
  if (apply_bonferroni && length(chi_square_results) > 0) {
    adjusted_p_values <- pmin(chi_square_results * length(chi_square_results), 1)
  } else {
    adjusted_p_values <- chi_square_results
  }
  
  sig_features <- feature_cols[!is.na(adjusted_p_values) & adjusted_p_values < sig_level]
  
  return(list(significant_features = sig_features, p_values = setNames(adjusted_p_values, feature_cols)))
}

# Set up future for parallel processing
plan(multisession)

# Define UAT for fs_chi function using testthat
test_fs_chi <- function() {
  cat("Running UAT for fs_chi...\n")
  
  # Tests 1 to 3
  
  # Test 4a: Feature not significant with low frequency counts
  test_that("fs_chi handles low frequency counts without significant association", {
    set.seed(123)
    target <- factor(rep(c("Yes", "No"), each = 250))
    feature_with_low_freq <- factor(c(rep("A", 495), rep("B", 5)))
    data_low_freq <- data.frame(feature_with_low_freq = feature_with_low_freq, target = target)
    expect_message(
      results_low_freq <- fs_chi(data_low_freq, "target"),
      "Expected counts less than 5 detected in the contingency table for feature feature_with_low_freq"
    )
    expect_length(results_low_freq$significant_features, 0)
    expect_true("feature_with_low_freq" %in% names(results_low_freq$p_values))
  })
  
  # Test 4b: Feature significant with low frequency counts
  test_that("fs_chi detects significant association with low frequency counts", {
    set.seed(123)
    target <- factor(c(rep("Yes", 495), rep("No", 5)))
    feature_with_low_freq <- factor(c(rep("A", 495), rep("B", 5)))
    data_low_freq <- data.frame(feature_with_low_freq = feature_with_low_freq, target = target)
    expect_message(
      results_low_freq <- fs_chi(data_low_freq, "target"),
      "Expected counts less than 5 detected in the contingency table for feature feature_with_low_freq"
    )
    expect_length(results_low_freq$significant_features, 1)
    expect_true("feature_with_low_freq" %in% results_low_freq$significant_features)
  })
  
  # Tests 5 to 9
  
  cat("UAT for fs_chi completed.\n")
}

# Run the UAT functions
test_fs_chi()