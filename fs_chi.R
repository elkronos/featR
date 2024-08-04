# Load necessary packages
library(data.table)
library(furrr)
library(future)
library(testthat)

# Define a helper function to check if two lists are equal
expect_equal_lists <- function(list1, list2) {
  expect_equal(length(list1), length(list2))
  expect_equal(sort(names(list1)), sort(names(list2)))
  for (name in names(list1)) {
    expect_equal(list1[[name]], list2[[name]])
  }
}

#' @title Perform feature selection using chi-square test
#'
#' @description
#' This function evaluates the association between categorical features and a target variable using the chi-square
#' test. It handles cases with minimum frequencies in the contingency table less than 5 by issuing a warning and
#' setting the p-value as `NA`. Additionally, it detects and handles missing values within the features.
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
#' # Create a vector with Yes and No repeated 250 times each
#' target <- factor(rep(c("Yes", "No"), each = 250))
#'
#' # Create a feature that is directly associated with the target variable
#' feature_1 <- factor(c(rep(c("A", "B"), times = c(200, 50)), rep(c("A", "B", "C"), times = c(50, 100, 100))))
#'
#' # Create a feature that is not associated with the target variable
#' feature_2 <- factor(sample(c("X", "Y"), 500, replace = TRUE))
#'
#' # Create a feature that is weakly associated with the target variable
#' feature_3 <- factor(c(rep(c("M", "N"), times = c(150, 100)), rep(c("M", "N"), times = c(100, 150))))
#'
#' # Combine all the features and target into a data frame
#' data <- data.frame(feature_1 = feature_1, feature_2 = feature_2, feature_3 = feature_3, target = target)
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
fs_chi <- function(data, target_col, sig_level = 0.05, correct = TRUE, apply_bonferroni = TRUE) {
  
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (!target_col %in% names(data)) stop("target_col must be a column in data.")
  
  # Convert to data.table
  data <- as.data.table(data)
  
  # Convert non-numeric columns to factors
  cols_to_convert <- names(data)[sapply(data, is.character)]
  data[, (cols_to_convert) := lapply(.SD, as.factor), .SDcols = cols_to_convert]
  
  # Identify the categorical feature columns in the data frame
  feature_cols <- setdiff(names(data), target_col)
  feature_cols <- feature_cols[sapply(data[, ..feature_cols], is.factor)]
  
  calculate_p_value <- function(feature) {
    cont_table <- table(data[[feature]], data[[target_col]])
    
    if (any(is.na(cont_table))) {
      warning(paste("Missing values detected in feature", feature, ". Setting p-value as NA."))
      return(NA)
    }
    
    if (min(cont_table) < 5) {
      warning(paste("Frequency less than 5 detected in the contingency table for feature", feature, ". Setting p-value as NA."))
      return(NA)
    }
    
    if (is.null(correct)) {
      correct_needed <- min(chisq.test(cont_table)$expected) < 5
    } else {
      correct_needed <- correct
    }
    
    res <- chisq.test(cont_table, correct = correct_needed)
    return(res$p.value)
  }
  
  # Use furrr for parallel processing to calculate p-values for each feature
  chi_square_results <- future_map_dbl(feature_cols, calculate_p_value)
  
  if (apply_bonferroni) {
    adj_sig_level <- sig_level / length(feature_cols)
  } else {
    adj_sig_level <- sig_level
  }
  
  sig_features <- feature_cols[!is.na(chi_square_results) & chi_square_results < adj_sig_level]
  
  return(list(significant_features = sig_features, p_values = setNames(chi_square_results, feature_cols)))
}

# Define UAT for fs_chi function
test_fs_chi <- function() {
  cat("Running UAT for fs_chi...\n")
  
  # Scenario 1: Normal functionality with a mix of associated and non-associated features
  cat("Scenario 1: Normal functionality\n")
  set.seed(123)
  target <- factor(rep(c("Yes", "No"), each = 250))
  feature_1 <- factor(c(rep(c("A", "B"), times = c(200, 50)), rep(c("A", "B", "C"), times = c(50, 100, 100))))
  feature_2 <- factor(sample(c("X", "Y"), 500, replace = TRUE))
  feature_3 <- factor(c(rep(c("M", "N"), times = c(150, 100)), rep(c("M", "N"), times = c(100, 150))))
  data <- data.frame(feature_1 = feature_1, feature_2 = feature_2, feature_3 = feature_3, target = target)
  results <- fs_chi(data, "target")
  print(results$significant_features)
  print(results$p_values)
  
  # Scenario 2: Data frame with no categorical features
  cat("Scenario 2: No categorical features\n")
  data_no_cat <- data.frame(numeric_feature = rnorm(500), target = target)
  results_no_cat <- tryCatch(fs_chi(data_no_cat, "target"), error = function(e) e)
  print(results_no_cat)
  
  # Scenario 3: Data frame with missing values in features
  cat("Scenario 3: Missing values in features\n")
  feature_1_with_na <- feature_1
  feature_1_with_na[c(10, 20)] <- NA
  data_with_na <- data.frame(feature_1 = feature_1_with_na, feature_2 = feature_2, feature_3 = feature_3, target = target)
  results_with_na <- fs_chi(data_with_na, "target")
  print(results_with_na$significant_features)
  print(results_with_na$p_values)
  
  # Scenario 4: Frequency less than 5 in contingency table
  cat("Scenario 4: Frequency less than 5 in contingency table\n")
  feature_with_low_freq <- factor(c(rep("A", 495), rep("B", 5)))
  data_low_freq <- data.frame(feature_1 = feature_1, feature_with_low_freq = feature_with_low_freq, target = target)
  results_low_freq <- fs_chi(data_low_freq, "target")
  print(results_low_freq$significant_features)
  print(results_low_freq$p_values)
  
  # Scenario 5: Apply Bonferroni correction
  cat("Scenario 5: Apply Bonferroni correction\n")
  results_bonferroni <- fs_chi(data, "target", apply_bonferroni = TRUE)
  print(results_bonferroni$significant_features)
  print(results_bonferroni$p_values)
  
  # Scenario 6: Do not apply Bonferroni correction
  cat("Scenario 6: Do not apply Bonferroni correction\n")
  results_no_bonferroni <- fs_chi(data, "target", apply_bonferroni = FALSE)
  print(results_no_bonferroni$significant_features)
  print(results_no_bonferroni$p_values)
  
  # Scenario 7: Custom significance level
  cat("Scenario 7: Custom significance level\n")
  results_custom_sig <- fs_chi(data, "target", sig_level = 0.01)
  print(results_custom_sig$significant_features)
  print(results_custom_sig$p_values)
  
  # Scenario 8: Check function with target column not present
  cat("Scenario 8: Target column not present\n")
  results_no_target <- tryCatch(fs_chi(data, "non_existent_column"), error = function(e) e)
  print(results_no_target)
  
  # Scenario 9: Check function with non-data frame input
  cat("Scenario 9: Non-data frame input\n")
  results_non_df <- tryCatch(fs_chi(matrix(1:10, ncol = 2), "V1"), error = function(e) e)
  print(results_non_df)
  
  cat("UAT for fs_chi completed.\n")
}

# Setup future for parallel processing
plan(multisession)

# Run the UAT functions
test_fs_chi()
