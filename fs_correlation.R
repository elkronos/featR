# Load necessary packages
library(Boruta)
library(caret)
library(doParallel)
library(testthat)
library(foreach)
library(iterators)
library(ltm)
library(polycor)

#' Correlation-based feature selection
#'
#' This function selects features from a data set based on their correlation with
#' other features. It returns a list containing the correlation matrix and the
#' names of the selected variables.
#'
#' @param data A data frame or matrix containing the data. For methods "pearson",
#'   "spearman", and "kendall", all columns must be numeric. For "polychoric",
#'   all columns must be ordered factors. For "pointbiserial", data can include
#'   numeric and dichotomous variables.
#' @param threshold A single numeric value between 0 and 1. Features with absolute
#'   correlation above this threshold will be selected.
#' @param method The correlation method to use:
#'   \describe{
#'     \item{"pearson"}{Standard Pearson correlation (numeric data).}
#'     \item{"spearman"}{Spearman rank correlation (numeric data).}
#'     \item{"kendall"}{Kendall's tau correlation (numeric data).}
#'     \item{"polychoric"}{Polychoric correlation (ordered categorical data).}
#'     \item{"pointbiserial"}{Point-biserial correlation (mix of numeric and dichotomous data).}
#'   }
#' @param na.rm Logical indicating whether to remove missing values before calculating correlations. Default is FALSE.
#' @param parallel Logical indicating whether to use parallel processing. Default is FALSE.
#' @param n_cores Number of cores to use for parallel processing if parallel is TRUE. Default is 2.
#' @param sample_frac Fraction of the data to sample if the dataset is too large. Default is 1 (no sampling).
#' @param output_format The format of the correlation matrix output, either "matrix" (default) or "data.frame".
#' @param diag_value The value to set on the diagonal of the correlation matrix. Default is 0.
#' @param no_vars_message Custom message when no variables meet the correlation threshold. Default is "No variables meet the correlation threshold."
#' @param seed Optional seed for random number generation when sampling data. Default is NULL (no seed set).
#' @param verbose Logical indicating whether to print detailed messages during execution. Default is FALSE.
#'
#' @return A list containing two elements:
#'   \describe{
#'     \item{\code{corr_matrix}}{The correlation matrix (format specified by \code{output_format}).}
#'     \item{\code{selected_vars}}{Character vector containing the names of the selected variables. If no variables meet the correlation threshold, this will be an empty character vector.}
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
#' @importFrom stats cor cor.test
#' @importFrom utils combn
#'
#' @export
fs_correlation <- function(data, threshold, method = "pearson", na.rm = FALSE,
                           parallel = FALSE, n_cores = 2, sample_frac = 1,
                           output_format = "matrix", diag_value = 0, no_vars_message = "No variables meet the correlation threshold.",
                           seed = NULL, verbose = FALSE) {
  # Validate inputs
  validate_inputs(data, threshold, method, output_format)
  
  # Sample data if required
  data <- sample_data(data, sample_frac, seed)
  
  # Load required packages
  load_required_packages(method)
  
  # Calculate correlation matrix
  corr_matrix <- calculate_correlation(data, method, na.rm, parallel, n_cores, verbose)
  
  # Set the diagonal of the matrix
  diag(corr_matrix) <- diag_value
  
  # Find the absolute correlation values above the threshold
  high_corr_vars <- find_high_correlation(corr_matrix, threshold)
  
  # Check if there are any selected variables
  if (nrow(high_corr_vars) == 0) {
    message(no_vars_message)
    selected_vars <- character(0)  # Return empty character vector instead of NULL
  } else {
    # Extract the selected variables from the data frame
    selected_vars <- unique(c(rownames(corr_matrix)[high_corr_vars[, 1]], colnames(corr_matrix)[high_corr_vars[, 2]]))
  }
  
  # Convert correlation matrix to specified output format
  if (output_format == "data.frame") {
    corr_matrix <- as.data.frame(as.table(corr_matrix))
    names(corr_matrix) <- c("Var1", "Var2", "Correlation")
  }
  
  # Save the correlation matrix and selected variables in a list
  result <- list(corr_matrix = corr_matrix, selected_vars = selected_vars)
  return(result)
}

# Helper Functions

validate_inputs <- function(data, threshold, method, output_format) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("The 'data' argument must be a data frame or a matrix.")
  }
  valid_methods <- c("pearson", "spearman", "kendall", "polychoric", "pointbiserial")
  if (!(method %in% valid_methods)) {
    stop("Invalid correlation method. Please specify one of: ", paste(valid_methods, collapse = ", "), ".")
  }
  if (!(is.numeric(threshold) && length(threshold) == 1 && threshold >= 0 && threshold <= 1)) {
    stop("The 'threshold' argument must be a single numeric value between 0 and 1.")
  }
  if (!(output_format %in% c("matrix", "data.frame"))) {
    stop("Invalid output format. Please specify 'matrix' or 'data.frame'.")
  }
  # Check data types based on method
  if (method %in% c("pearson", "spearman", "kendall")) {
    if (!all(sapply(data, is.numeric))) {
      stop("All columns in 'data' must be numeric for method '", method, "'.")
    }
  } else if (method == "pointbiserial") {
    if (!all(sapply(data, function(x) is.numeric(x) || is_dichotomous(x)))) {
      stop("All columns in 'data' must be numeric or dichotomous (factor with two levels) for method 'pointbiserial'.")
    }
  } else if (method == "polychoric") {
    if (!all(sapply(data, is.ordered))) {
      stop("All columns in 'data' must be ordered factors for method 'polychoric'.")
    }
  }
}

load_required_packages <- function(method) {
  if (method == "polychoric") {
    if (!requireNamespace("polycor", quietly = TRUE)) {
      stop("Package 'polycor' is required for polychoric correlation. Please install it.")
    }
  } else if (method == "pointbiserial") {
    if (!requireNamespace("ltm", quietly = TRUE)) {
      stop("Package 'ltm' is required for point-biserial correlation. Please install it.")
    }
  }
}

sample_data <- function(data, sample_frac, seed = NULL) {
  if (sample_frac < 1) {
    if (!is.null(seed)) set.seed(seed)
    data <- data[sample(1:nrow(data), size = floor(sample_frac * nrow(data))), ]
  }
  return(data)
}

calculate_correlation <- function(data, method, na.rm, parallel, n_cores, verbose) {
  if (method == "pointbiserial") {
    return(calculate_pointbiserial_correlation(data, na.rm, parallel, n_cores, verbose))
  } else if (method == "polychoric") {
    return(calculate_polychoric_correlation(data, verbose))
  } else {
    use_option <- if (na.rm) "complete.obs" else "everything"
    return(cor(data, method = method, use = use_option))
  }
}

calculate_pointbiserial_correlation <- function(data, na.rm, parallel, n_cores, verbose) {
  n <- ncol(data)
  correlation_matrix <- matrix(NA, n, n)
  colnames(correlation_matrix) <- colnames(data)
  rownames(correlation_matrix) <- colnames(data)
  
  continuous_vars <- which(sapply(data, is_continuous))
  dichotomous_vars <- which(sapply(data, is_dichotomous))
  
  if (parallel) {
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("Package 'doParallel' is required for parallel processing. Please install it.")
    }
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    on.exit({
      stopCluster(cl)
      registerDoSEQ()
    })
    # Create combinations of continuous and dichotomous variables
    var_pairs <- expand.grid(continuous_vars, dichotomous_vars)
    # Remove pairs where variables are the same
    var_pairs <- var_pairs[var_pairs[,1] != var_pairs[,2], ]
    results <- foreach(k = 1:nrow(var_pairs), .combine = rbind, .packages = c("ltm")) %dopar% {
      i <- var_pairs[k, 1]
      j <- var_pairs[k, 2]
      if (verbose) message("Calculating point-biserial correlation for columns: ", colnames(data)[i], " and ", colnames(data)[j])
      corr_value <- tryCatch(
        ltm::biserial.cor(data[[i]], as.numeric(data[[j]]), use = if (na.rm) "complete.obs" else "all.obs"),
        error = function(e) NA
      )
      data.frame(i = i, j = j, corr = corr_value)
    }
    # Fill in the correlation matrix
    for (row in 1:nrow(results)) {
      i <- results$i[row]
      j <- results$j[row]
      corr_value <- results$corr[row]
      correlation_matrix[i, j] <- corr_value
      correlation_matrix[j, i] <- corr_value  # Make it symmetric
    }
  } else {
    for (i in continuous_vars) {
      for (j in dichotomous_vars) {
        if (i == j) {
          correlation_matrix[i, j] <- 1
        } else {
          if (verbose) message("Calculating point-biserial correlation for columns: ", colnames(data)[i], " and ", colnames(data)[j])
          corr_value <- tryCatch(
            ltm::biserial.cor(data[[i]], as.numeric(data[[j]]), use = if (na.rm) "complete.obs" else "all.obs"),
            error = function(e) NA
          )
          correlation_matrix[i, j] <- corr_value
          correlation_matrix[j, i] <- corr_value  # Make it symmetric
        }
      }
    }
  }
  
  return(correlation_matrix)
}

is_dichotomous <- function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) == 2
}

is_continuous <- function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) > 2 && is.numeric(x)
}

calculate_polychoric_correlation <- function(data, verbose) {
  if (verbose) message("Calculating polychoric correlation matrix.")
  return(polycor::hetcor(data)$correlations)
}

find_high_correlation <- function(corr_matrix, threshold) {
  high_corr_vars <- which(abs(corr_matrix) > threshold & !is.na(corr_matrix), arr.ind = TRUE)
  # Remove duplicates and lower triangle entries
  high_corr_vars <- high_corr_vars[high_corr_vars[, 1] < high_corr_vars[, 2], , drop = FALSE]
  return(high_corr_vars)
}

# User Acceptance Testing (UAT)

test_fs_correlation <- function() {
  cat("Running UAT for fs_correlation...\n")
  
  # Test 1: Basic Functionality with Default Parameters
  test_that("fs_correlation works with default parameters", {
    data <- mtcars
    threshold <- 0.7
    result <- fs_correlation(data, threshold)
    expect_true(is.matrix(result$corr_matrix) || is.data.frame(result$corr_matrix))
    expect_type(result$selected_vars, "character")
    expect_true(length(result$selected_vars) > 0)
  })
  
  # Test 2: Different Correlation Methods with Appropriate Data
  test_that("fs_correlation works with different correlation methods", {
    data <- mtcars
    threshold <- 0.7
    
    # Pearson correlation with numeric data
    result_pearson <- fs_correlation(data, threshold, method = "pearson")
    expect_true(is.matrix(result_pearson$corr_matrix) || is.data.frame(result_pearson$corr_matrix))
    expect_type(result_pearson$selected_vars, "character")
    
    # Spearman correlation with numeric data
    result_spearman <- fs_correlation(data, threshold, method = "spearman")
    expect_true(is.matrix(result_spearman$corr_matrix) || is.data.frame(result_spearman$corr_matrix))
    expect_type(result_spearman$selected_vars, "character")
    
    # Kendall correlation with numeric data
    result_kendall <- fs_correlation(data, threshold, method = "kendall")
    expect_true(is.matrix(result_kendall$corr_matrix) || is.data.frame(result_kendall$corr_matrix))
    expect_type(result_kendall$selected_vars, "character")
  })
  
  # Test 3: Point-Biserial Correlation with Mixed Data
  test_that("fs_correlation works with point-biserial correlation", {
    # Generate data with known correlation
    set.seed(123)
    continuous_var <- rnorm(100)
    dichotomous_var <- ifelse(continuous_var > 0, 1, 0)
    data_pb <- data.frame(
      continuous_var = continuous_var,
      dichotomous_var = dichotomous_var
    )
    result_pointbiserial <- fs_correlation(data_pb, threshold = 0.1, method = "pointbiserial")
    expect_true(is.matrix(result_pointbiserial$corr_matrix) || is.data.frame(result_pointbiserial$corr_matrix))
    expect_type(result_pointbiserial$selected_vars, "character")
    expect_true(length(result_pointbiserial$selected_vars) > 0)
  })
  
  # Test 4: Polychoric Correlation with Ordered Factors
  test_that("fs_correlation works with polychoric correlation", {
    # Generate data with known correlation
    set.seed(123)
    ordinal_values <- sample(1:5, 100, replace = TRUE)
    ordinal_var1 <- factor(ordinal_values, ordered = TRUE)
    ordinal_var2 <- factor(ordinal_values + sample(c(-1, 0, 1), 100, replace = TRUE), levels = 1:5, ordered = TRUE)
    data_poly <- data.frame(
      ordinal_var1 = ordinal_var1,
      ordinal_var2 = ordinal_var2
    )
    result_polychoric <- fs_correlation(data_poly, threshold = 0.1, method = "polychoric")
    expect_true(is.matrix(result_polychoric$corr_matrix) || is.data.frame(result_polychoric$corr_matrix))
    expect_type(result_polychoric$selected_vars, "character")
    expect_true(length(result_polychoric$selected_vars) > 0)
  })
  
  # Test 5: Handling Missing Values
  test_that("fs_correlation handles missing values", {
    data_with_na <- mtcars
    data_with_na[1:5, 1] <- NA
    result_na <- fs_correlation(data_with_na, threshold = 0.7, na.rm = TRUE)
    expect_true(is.matrix(result_na$corr_matrix) || is.data.frame(result_na$corr_matrix))
    expect_type(result_na$selected_vars, "character")
  })
  
  # Test 6: Parallel Processing
  test_that("fs_correlation works with parallel processing", {
    # Generate data with known correlation
    set.seed(123)
    continuous_var <- rnorm(100)
    dichotomous_var <- ifelse(continuous_var > 0, 1, 0)
    data_pb <- data.frame(
      continuous_var = continuous_var,
      dichotomous_var = dichotomous_var
    )
    result_parallel <- fs_correlation(data_pb, threshold = 0.1, method = "pointbiserial", parallel = TRUE, n_cores = 2, verbose = TRUE)
    expect_true(is.matrix(result_parallel$corr_matrix) || is.data.frame(result_parallel$corr_matrix))
    expect_type(result_parallel$selected_vars, "character")
    expect_true(length(result_parallel$selected_vars) > 0)
  })
  
  # Test 7: Data Sampling
  test_that("fs_correlation works with data sampling", {
    data <- mtcars
    result_sampling <- fs_correlation(data, threshold = 0.7, sample_frac = 0.5, seed = 42)
    expect_true(is.matrix(result_sampling$corr_matrix) || is.data.frame(result_sampling$corr_matrix))
    expect_type(result_sampling$selected_vars, "character")
  })
  
  # Test 8: Output Format
  test_that("fs_correlation returns data.frame when output_format is 'data.frame'", {
    data <- mtcars
    result_df <- fs_correlation(data, threshold = 0.7, output_format = "data.frame")
    expect_true(is.data.frame(result_df$corr_matrix))
    expect_type(result_df$selected_vars, "character")
  })
  
  # Test 9: Custom Diagonal Value
  test_that("fs_correlation sets custom diagonal value", {
    data <- mtcars
    result_diag <- fs_correlation(data, threshold = 0.7, diag_value = NA)
    expect_true(all(is.na(diag(result_diag$corr_matrix))))
  })
  
  # Test 10: Custom No Variables Message
  test_that("fs_correlation uses custom no variables message", {
    data <- mtcars
    custom_message <- "Custom message: No variables selected."
    expect_message(fs_correlation(data, 0.95, no_vars_message = custom_message), custom_message)
  })
  
  # Test 11: Invalid Inputs
  test_that("fs_correlation handles invalid inputs", {
    expect_error(fs_correlation("not a data frame", 0.7), "The 'data' argument must be a data frame or a matrix.")
    expect_error(fs_correlation(mtcars, 1.5), "The 'threshold' argument must be a single numeric value between 0 and 1.")
    expect_error(fs_correlation(mtcars, 0.7, method = "invalid_method"), "Invalid correlation method.")
  })
  
  # Test 12: Verbose Output
  test_that("fs_correlation provides verbose output when verbose = TRUE", {
    # Generate data with known correlation
    set.seed(123)
    continuous_var <- rnorm(100)
    dichotomous_var <- ifelse(continuous_var > 0, 1, 0)
    data_pb <- data.frame(
      continuous_var = continuous_var,
      dichotomous_var = dichotomous_var
    )
    expect_message(
      fs_correlation(data_pb, threshold = 0.1, method = "pointbiserial", verbose = TRUE),
      "Calculating point-biserial correlation for columns"
    )
  })
  
  cat("UAT for fs_correlation completed.\n")
}

# Run the UAT function
test_fs_correlation()
