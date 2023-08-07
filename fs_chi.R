# Load packages
library(dplyr)
library(purrr)

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
#' * `p_values`: A numeric vector indicating the p-values from the chi-square tests for each feature.
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
#' @importFrom dplyr mutate_if
#' @importFrom purrr map_dbl set_names
#' @import stats
#' @export
fs_chi <- function(data, target_col, sig_level = 0.05, correct = TRUE, apply_bonferroni = TRUE) {
  
  if(!is.data.frame(data)) stop("data must be a data frame.")
  if(!target_col %in% names(data)) stop("target_col must be a column in data.")
  
  # Convert non-numeric columns to factors
  data <- data %>% mutate_if(is.character, as.factor)
  
  # Identify the categorical feature columns in the data frame
  feature_cols <- setdiff(names(data), target_col)
  categorical_feature_cols <- sapply(data[feature_cols], is.factor)
  feature_cols <- feature_cols[categorical_feature_cols]
  
  chi_square_results <- map_dbl(feature_cols, ~{
    cont_table <- table(data[[.x]], data[[target_col]])
    
    if(any(is.na(cont_table))) {
      warning(paste("Missing values detected in feature", .x, ". Setting p-value as NA."))
      return(NA)
    }
    
    if (min(cont_table) < 5) {
      warning(paste("Frequency less than 5 detected in the contingency table for feature", .x, ". Setting p-value as NA."))
      return(NA)
    }
    
    if(is.null(correct)) {
      expected <- sum(cont_table) * prod(margin.table(cont_table, 1)) * prod(margin.table(cont_table, 2)) / prod(margin.table(cont_table))
      correct = min(expected) < 5
    }
    
    res <- chisq.test(cont_table, correct = correct)
    return(res$p.value)
  })
  
  if(apply_bonferroni) {
    adj_sig_level <- sig_level / length(feature_cols)
  } else {
    adj_sig_level <- sig_level
  }
  
  sig_features <- feature_cols[!is.na(chi_square_results) & chi_square_results < adj_sig_level]
  
  return(list(significant_features = sig_features, p_values = setNames(chi_square_results, feature_cols)))
}