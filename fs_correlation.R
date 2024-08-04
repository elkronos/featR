#' Correlation-based feature selection
#'
#' This function selects features from a data set based on their correlation with
#' other features. It returns a list containing the correlation matrix and the
#' names of the selected variables.
#'
#' @param data A data frame or matrix containing the data. The function will stop 
#'   if the 'data' is not a data frame or a matrix.
#' @param threshold The threshold above which to select features based on their
#'   correlation. Must be a value between 0 and 1, or a vector of such values for different pairs.
#' @param method The correlation method to use, either "pearson" (default), "spearman", "kendall", "polychoric", or "pointbiserial". 
#'   The function will stop if any other method is provided.
#' @param na.rm Logical indicating whether to remove missing values before calculating correlations. Default is FALSE.
#' @param parallel Logical indicating whether to use parallel processing. Default is FALSE.
#' @param n_cores Number of cores to use for parallel processing if parallel is TRUE. Default is 2.
#' @param sample_frac Fraction of the data to sample if the dataset is too large. Default is 1 (no sampling).
#' @param output_format The format of the correlation matrix output, either "matrix" (default) or "data.frame".
#' @param diag_value The value to set on the diagonal of the correlation matrix. Default is 0.
#' @param no_vars_message Custom message when no variables meet the correlation threshold. Default is "No variables meet the correlation threshold."
#'
#' @return A list containing two elements: 
#'   \itemize{
#'     \item \strong{corr_matrix}: The correlation matrix (as specified by output_format). 
#'     \item \strong{selected_vars}: Character vector containing the names of the selected variables. If no variables meet the correlation threshold, this will be NULL.
#'   }
#'
#' @examples
#' # Load the mtcars data set
#' data(mtcars)
#'
#' # Default is Pearson
#' corr_vars <- fs_correlation(mtcars, 0.7)
#' print(corr_vars$corr_matrix)
#' print(corr_vars$selected_vars)
#'
#' # Specify Spearman
#' corr_vars <- fs_correlation(mtcars, 0.7, method = "spearman")
#' print(corr_vars$corr_matrix)
#' print(corr_vars$selected_vars)
#'
#' @export
fs_correlation <- function(data, threshold, method = "pearson", na.rm = FALSE, parallel = FALSE, n_cores = 2, sample_frac = 1, output_format = "matrix", diag_value = 0, no_vars_message = "No variables meet the correlation threshold.") {
  # Validate inputs
  validate_inputs(data, threshold, method, output_format)
  
  # Sample data if required
  data <- sample_data(data, sample_frac)
  
  # Parallel processing
  if (parallel) {
    library(parallel)
    cl <- makeCluster(n_cores)
    on.exit(stopCluster(cl))
    clusterExport(cl, varlist = c("data", "method", "na.rm", "calculate_correlation", 
                                  "validate_inputs", "sample_data", "find_high_correlation", 
                                  "calculate_pearson_correlation", "calculate_spearman_correlation", 
                                  "calculate_kendall_correlation", "calculate_polychoric_correlation", 
                                  "calculate_pointbiserial_correlation"), envir = environment())
    corr_matrix <- parLapply(cl, list(data), calculate_correlation, method = method, na.rm = na.rm)[[1]]
  } else {
    corr_matrix <- calculate_correlation(data, method, na.rm)
  }
  
  # Set the diagonal of the matrix
  diag(corr_matrix) <- diag_value
  
  # Find the absolute correlation values above the threshold
  high_corr_vars <- find_high_correlation(corr_matrix, threshold)
  
  # Check if there are any selected variables
  if (nrow(high_corr_vars) == 0) {
    message(no_vars_message)
    return(list(corr_matrix = corr_matrix, selected_vars = NULL))
  }
  
  # Extract the selected variables from the data frame
  selected_vars <- unique(c(rownames(corr_matrix)[high_corr_vars[, 1]], colnames(corr_matrix)[high_corr_vars[, 2]]))
  
  # Convert correlation matrix to specified output format
  if (output_format == "data.frame") {
    corr_matrix <- as.data.frame(as.table(corr_matrix))
  }
  
  # Save the correlation matrix and selected variables in a list
  result <- list(corr_matrix = corr_matrix, selected_vars = selected_vars)
  return(result)
}

#' Validate Inputs for fs_correlation Function
#'
#' This function validates the inputs for the fs_correlation function.
#'
#' @param data A data frame or matrix containing the data.
#' @param threshold The threshold above which to select features based on their correlation.
#' @param method The correlation method to use.
#' @param output_format The format of the correlation matrix output.
#'
#' @return NULL if all inputs are valid, otherwise an error is thrown.
validate_inputs <- function(data, threshold, method, output_format) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("The 'data' argument must be a data frame or a matrix.")
  }
  if (!all(sapply(data, is.numeric))) {
    stop("All columns in 'data' must be numeric.")
  }
  if (!(is.numeric(threshold) && all(threshold >= 0) && all(threshold <= 1))) {
    stop("The 'threshold' argument must be a single value or a vector with values between 0 and 1.")
  }
  if (!(method %in% c("pearson", "spearman", "kendall", "polychoric", "pointbiserial"))) {
    stop("Invalid correlation method. Please specify 'pearson', 'spearman', 'kendall', 'polychoric', or 'pointbiserial'.")
  }
  if (!(output_format %in% c("matrix", "data.frame"))) {
    stop("Invalid output format. Please specify 'matrix' or 'data.frame'.")
  }
}

#' Sample Data for fs_correlation Function
#'
#' This function samples the data if the sample fraction is less than 1.
#'
#' @param data A data frame or matrix containing the data.
#' @param sample_frac The fraction of the data to sample.
#'
#' @return The sampled data.
sample_data <- function(data, sample_frac) {
  if (sample_frac < 1) {
    set.seed(123)
    data <- data[sample(1:nrow(data), size = sample_frac * nrow(data)), ]
  }
  return(data)
}

#' Calculate Correlation for fs_correlation Function
#'
#' This function calculates the correlation matrix for the data using the specified method.
#'
#' @param data A data frame or matrix containing the data.
#' @param method The correlation method to use.
#' @param na.rm Logical indicating whether to remove missing values before calculating correlations.
#'
#' @return The correlation matrix.
calculate_correlation <- function(data, method, na.rm) {
  if (method == "pointbiserial") {
    return(calculate_pointbiserial_correlation(data, na.rm))
  } else if (method == "polychoric") {
    return(calculate_polychoric_correlation(data))
  } else {
    return(cor(data, method = method, use = if (na.rm) "complete.obs" else "everything"))
  }
}

#' Calculate Point Biserial Correlation
#'
#' This function calculates the point biserial correlation matrix for the data.
#'
#' @param data A data frame or matrix containing the data.
#' @param na.rm Logical indicating whether to remove missing values before calculating correlations.
#'
#' @return The point biserial correlation matrix.
calculate_pointbiserial_correlation <- function(data, na.rm) {
  n <- ncol(data)
  correlation_matrix <- matrix(NA, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        correlation_matrix[i, j] <- 1
      } else if (is.dichotomous(data[, j]) && is.continuous(data[, i])) {
        print(paste("Calculating biserial correlation for columns:", i, j))
        correlation_matrix[i, j] <- tryCatch(psych::biserial(data[, i], data[, j], use = if (na.rm) "complete.obs" else "everything"), 
                                             error = function(e) NA)
      } else {
        correlation_matrix[i, j] <- NA
      }
    }
  }
  
  return(correlation_matrix)
}

#' Check if a Variable is Dichotomous
#'
#' This function checks if a variable is dichotomous.
#'
#' @param x A vector.
#'
#' @return TRUE if the variable is dichotomous, otherwise FALSE.
is.dichotomous <- function(x) {
  length(unique(x[!is.na(x)])) == 2
}

#' Check if a Variable is Continuous
#'
#' This function checks if a variable is continuous.
#'
#' @param x A vector.
#'
#' @return TRUE if the variable is continuous, otherwise FALSE.
is.continuous <- function(x) {
  length(unique(x[!is.na(x)])) > 2
}

#' Calculate Polychoric Correlation
#'
#' This function calculates the polychoric correlation matrix for the data.
#'
#' @param data A data frame or matrix containing the data.
#'
#' @return The polychoric correlation matrix.
calculate_polychoric_correlation <- function(data) {
  library(polycor)
  return(polycor::hetcor(data)$correlations)
}

#' Find High Correlation Pairs
#'
#' This function finds the pairs of variables with absolute correlation values above the specified threshold.
#'
#' @param corr_matrix The correlation matrix.
#' @param threshold The threshold above which to select features based on their correlation.
#'
#' @return A matrix with the row and column indices of the high correlation pairs.
find_high_correlation <- function(corr_matrix, threshold) {
  if (length(threshold) == 1) {
    high_corr_vars <- which(abs(corr_matrix) > threshold, arr.ind = TRUE)
  } else {
    if (length(threshold) != choose(ncol(corr_matrix), 2)) {
      stop("Length of threshold vector must match the number of unique variable pairs.")
    }
    upper_tri_indices <- which(upper.tri(corr_matrix), arr.ind = TRUE)
    high_corr_vars <- upper_tri_indices[abs(corr_matrix[upper_tri_indices]) > threshold, , drop = FALSE]
  }
  return(high_corr_vars)
}

#' User Acceptance Testing for fs_correlation function
#'
#' This function runs a series of tests on the fs_correlation function to ensure it works
#' as expected. It includes tests for various correlation methods, handling missing values,
#' parallel processing, data sampling, output formats, and more.
#'
#' @examples
#' ## Not run:
#' test_fs_correlation()
#' ## End(Not run)
test_fs_correlation <- function() {
  cat("Running UAT for fs_correlation...\n")
  
  # Test 1: Basic Functionality with Default Parameters
  cat("Test 1: Basic Functionality with Default Parameters\n")
  data <- mtcars
  threshold <- 0.7
  result <- fs_correlation(data, threshold)
  print(result$corr_matrix)
  print(result$selected_vars)
  
  # Test 2: Different Correlation Methods
  cat("Test 2: Different Correlation Methods\n")
  result_pearson <- fs_correlation(data, threshold, method = "pearson")
  result_spearman <- fs_correlation(data, threshold, method = "spearman")
  result_kendall <- fs_correlation(data, threshold, method = "kendall")
  result_pointbiserial <- fs_correlation(data, threshold, method = "pointbiserial")
  print(result_pearson$corr_matrix)
  print(result_spearman$corr_matrix)
  print(result_kendall$corr_matrix)
  print(result_pointbiserial$corr_matrix)
  
  # Test 3: Handling Missing Values
  cat("Test 3: Handling Missing Values\n")
  data_with_na <- mtcars
  data_with_na[1:5, 1] <- NA
  result_na <- fs_correlation(data_with_na, threshold, na.rm = TRUE)
  print(result_na$corr_matrix)
  print(result_na$selected_vars)
  
  # Test 4: Parallel Processing
  cat("Test 4: Parallel Processing\n")
  result_parallel <- fs_correlation(data, threshold, parallel = TRUE, n_cores = 2)
  print(result_parallel$corr_matrix)
  print(result_parallel$selected_vars)
  
  # Test 5: Data Sampling
  cat("Test 5: Data Sampling\n")
  result_sampling <- fs_correlation(data, threshold, sample_frac = 0.5)
  print(result_sampling$corr_matrix)
  print(result_sampling$selected_vars)
  
  # Test 6: Output Format
  cat("Test 6: Output Format\n")
  result_df <- fs_correlation(data, threshold, output_format = "data.frame")
  print(result_df$corr_matrix)
  print(result_df$selected_vars)
  
  # Test 7: Custom Diagonal Value
  cat("Test 7: Custom Diagonal Value\n")
  result_diag <- fs_correlation(data, threshold, diag_value = NA)
  print(result_diag$corr_matrix)
  print(result_diag$selected_vars)
  
  # Test 8: Custom No Variables Message
  cat("Test 8: Custom No Variables Message\n")
  custom_message <- "Custom message: No variables selected."
  result_custom_message <- fs_correlation(data, 0.95, no_vars_message = custom_message)
  print(result_custom_message$corr_matrix)
  print(result_custom_message$selected_vars)
  
  # Test 9: Invalid Inputs
  cat("Test 9: Invalid Inputs\n")
  tryCatch({
    fs_correlation("not a data frame", threshold)
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  tryCatch({
    fs_correlation(data, 1.5)
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  tryCatch({
    fs_correlation(data, threshold, method = "invalid_method")
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  # Test 10: Threshold as Vector
  cat("Test 10: Threshold as Vector\n")
  threshold_vector <- rep(0.7, choose(ncol(data), 2))
  result_threshold_vector <- fs_correlation(data, threshold_vector)
  print(result_threshold_vector$corr_matrix)
  print(result_threshold_vector$selected_vars)
  
  cat("UAT for fs_correlation completed.\n")
}

# Run the UAT function
test_fs_correlation()
