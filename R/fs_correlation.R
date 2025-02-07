#' Correlation-based Feature Selection
#'
#' This function selects features from a dataset based on the correlation between variables.
#' It computes a correlation matrix using a specified method and returns the matrix along with
#' the names of the variables whose pairwise absolute correlations exceed a given threshold.
#'
#' @param data A data frame or matrix containing the dataset. For methods \code{"pearson"},
#'   \code{"spearman"}, and \code{"kendall"}, all columns must be numeric. For \code{"polychoric"},
#'   all columns must be ordered factors. For \code{"pointbiserial"}, columns can be numeric or
#'   dichotomous.
#' @param threshold A numeric value between 0 and 1 indicating the correlation threshold.
#'   Variable pairs with absolute correlation above this threshold will be selected.
#' @param method Character string specifying the correlation method to use.
#'   Options are: \code{"pearson"} (default), \code{"spearman"}, \code{"kendall"},
#'   \code{"polychoric"}, and \code{"pointbiserial"}.
#' @param na.rm Logical value (default \code{FALSE}) indicating whether to remove missing values
#'   before computing the correlations.
#' @param parallel Logical value (default \code{FALSE}) indicating whether to perform point-biserial
#'   correlation computations in parallel.
#' @param n_cores Numeric value (default \code{2}) specifying the number of cores to use for parallel
#'   processing (when \code{parallel} is \code{TRUE}).
#' @param sample_frac Numeric value between 0 and 1 (default \code{1}) specifying the fraction of data
#'   to use (useful for large datasets). A value of 1 means no sampling.
#' @param output_format Character string indicating the desired output format for the correlation matrix.
#'   Either \code{"matrix"} (default) or \code{"data.frame"}.
#' @param diag_value The value (default \code{0}) to assign to the diagonal of the correlation matrix.
#' @param no_vars_message Custom message (default \code{"No variables meet the correlation threshold."})
#'   to be printed if no variable pairs exceed the threshold.
#' @param seed Optional numeric seed (default \code{NULL}) for reproducible sampling.
#' @param verbose Logical value (default \code{FALSE}) indicating whether to print detailed progress messages.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{corr_matrix}{The correlation matrix (either as a matrix or data frame,
#'      depending on \code{output_format}).}
#'   \item{selected_vars}{A character vector containing the names of the selected variables.
#'      If no variables meet the threshold, an empty character vector is returned.}
#' }
#'
#' @importFrom stats cor
#' @importFrom utils combn
#'
#' @examples
#' \dontrun{
#' # Using the mtcars dataset with default Pearson correlation
#' result <- fs_correlation(mtcars, threshold = 0.7)
#' print(result$corr_matrix)
#' print(result$selected_vars)
#'
#' # Using Spearman correlation
#' result <- fs_correlation(mtcars, threshold = 0.7, method = "spearman")
#' print(result$corr_matrix)
#' print(result$selected_vars)
#' }
#'
#' @export
fs_correlation <- function(data, threshold, method = "pearson", na.rm = FALSE,
                           parallel = FALSE, n_cores = 2, sample_frac = 1,
                           output_format = "matrix", diag_value = 0,
                           no_vars_message = "No variables meet the correlation threshold.",
                           seed = NULL, verbose = FALSE) {
  
  # Validate inputs
  validate_inputs(data, threshold, method, output_format, sample_frac, n_cores)
  
  # Sample the data if required
  data <- sample_data(data, sample_frac, seed, verbose)
  
  # Load any method-specific packages
  load_required_packages(method)
  
  # Calculate the correlation matrix
  corr_matrix <- calculate_correlation(data, method, na.rm, parallel, n_cores, verbose)
  
  # Set the diagonal values as specified
  diag(corr_matrix) <- diag_value
  
  # Identify variable pairs with absolute correlation above the threshold
  high_corr_vars <- find_high_correlation(corr_matrix, threshold)
  
  # Determine the selected variables from the correlation matrix indices
  if (nrow(high_corr_vars) == 0) {
    message(no_vars_message)
    selected_vars <- character(0)
  } else {
    selected_vars <- unique(c(rownames(corr_matrix)[high_corr_vars[, 1]],
                              colnames(corr_matrix)[high_corr_vars[, 2]]))
  }
  
  # Convert correlation matrix to desired output format if needed
  if (output_format == "data.frame") {
    corr_matrix <- as.data.frame(as.table(corr_matrix))
    names(corr_matrix) <- c("Var1", "Var2", "Correlation")
  }
  
  return(list(corr_matrix = corr_matrix, selected_vars = selected_vars))
}

#' Validate Function Inputs
#'
#' This function checks that the inputs provided to \code{fs_correlation} are valid.
#'
#' @param data A data frame or matrix containing the dataset.
#' @param threshold A numeric value between 0 and 1 indicating the correlation threshold.
#' @param method Character string specifying the correlation method.
#'   Options: \code{"pearson"}, \code{"spearman"}, \code{"kendall"},
#'   \code{"polychoric"}, \code{"pointbiserial"}.
#' @param output_format Character string specifying the desired output format for the correlation matrix.
#'   Either \code{"matrix"} or \code{"data.frame"}.
#' @param sample_frac Numeric value between 0 and 1 specifying the fraction of data to use.
#' @param n_cores Numeric value specifying the number of cores for parallel processing.
#'
#' @return This function does not return a value. It stops execution with an error message if an input is invalid.
#'
#' @examples
#' \dontrun{
#' validate_inputs(mtcars, 0.7, "pearson", "matrix", sample_frac = 1, n_cores = 2)
#' }
validate_inputs <- function(data, threshold, method, output_format, sample_frac, n_cores) {
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
  
  if (!(is.numeric(sample_frac) && length(sample_frac) == 1 && sample_frac > 0 && sample_frac <= 1)) {
    stop("The 'sample_frac' argument must be a single numeric value between 0 (exclusive) and 1 (inclusive).")
  }
  
  if (!(is.numeric(n_cores) && length(n_cores) == 1 && n_cores >= 1)) {
    stop("The 'n_cores' argument must be a numeric value greater than or equal to 1.")
  }
  
  # Data type check based on the chosen method
  if (method %in% c("pearson", "spearman", "kendall")) {
    if (!all(sapply(data, is.numeric))) {
      stop("All columns in 'data' must be numeric for method '", method, "'.")
    }
  } else if (method == "pointbiserial") {
    if (!all(sapply(data, function(x) is.numeric(x) || is_dichotomous(x)))) {
      stop("For 'pointbiserial' method, all columns in 'data' must be numeric or dichotomous (with exactly 2 unique values).")
    }
  } else if (method == "polychoric") {
    if (!all(sapply(data, is.ordered))) {
      stop("All columns in 'data' must be ordered factors for method 'polychoric'.")
    }
  }
}

#' Load Required Packages for Correlation Methods
#'
#' This function checks for and loads any additional packages required by the
#' specified correlation method.
#'
#' @param method Character string specifying the correlation method.
#'
#' @return No return value. An error is thrown if a required package is not installed.
#'
#' @examples
#' \dontrun{
#' load_required_packages("polychoric")
#' }
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

#' Sample Data
#'
#' This function samples the dataset based on the specified fraction. It is useful
#' for reducing computation time on very large datasets.
#'
#' @param data A data frame or matrix containing the dataset.
#' @param sample_frac Numeric value between 0 and 1 specifying the fraction of data to use.
#'   A value of 1 means no sampling (default).
#' @param seed Optional numeric seed (default \code{NULL}) for reproducible sampling.
#' @param verbose Logical value (default \code{FALSE}) indicating whether to print a message about sampling.
#'
#' @return A data frame or matrix containing the sampled data.
#'
#' @examples
#' \dontrun{
#' sampled_data <- sample_data(mtcars, sample_frac = 0.5, seed = 123)
#' }
sample_data <- function(data, sample_frac, seed = NULL, verbose = FALSE) {
  if (sample_frac < 1) {
    if (!is.null(seed)) set.seed(seed)
    if (verbose) message("Sampling data: ", sample_frac * 100, "% of the original dataset will be used.")
    n_rows <- nrow(data)
    sampled_indices <- sample(seq_len(n_rows), size = floor(sample_frac * n_rows))
    data <- data[sampled_indices, , drop = FALSE]
  }
  return(data)
}

#' Calculate Correlation Matrix
#'
#' This function calculates the correlation matrix for the dataset using the specified method.
#' For methods requiring special handling (\code{"polychoric"} and \code{"pointbiserial"}), the
#' computation is delegated to specialized functions.
#'
#' @param data A data frame or matrix containing the dataset.
#' @param method Character string specifying the correlation method.
#' @param na.rm Logical value indicating whether to remove missing values before computing correlations.
#' @param parallel Logical value indicating whether to use parallel processing (only applicable for
#'   \code{"pointbiserial"}).
#' @param n_cores Numeric value specifying the number of cores to use if parallel processing is enabled.
#' @param verbose Logical value indicating whether to print progress messages.
#'
#' @return A correlation matrix.
#'
#' @examples
#' \dontrun{
#' corr_mat <- calculate_correlation(mtcars, "pearson", na.rm = TRUE, parallel = FALSE, n_cores = 2, verbose = FALSE)
#' }
calculate_correlation <- function(data, method, na.rm, parallel, n_cores, verbose) {
  if (method == "pointbiserial") {
    return(calculate_pointbiserial_correlation(data, na.rm, parallel, n_cores, verbose))
  } else if (method == "polychoric") {
    return(calculate_polychoric_correlation(data, verbose))
  } else {
    use_option <- if (na.rm) "complete.obs" else "everything"
    if (verbose) message("Calculating ", method, " correlation matrix.")
    return(cor(data, method = method, use = use_option))
  }
}

#' Calculate Point-Biserial Correlation Matrix
#'
#' This function computes the point-biserial correlation matrix for datasets containing
#' both numeric and dichotomous variables. Optionally, parallel processing can be used to
#' speed up the computation.
#'
#' @param data A data frame or matrix containing the dataset.
#' @param na.rm Logical value indicating whether to remove missing values before computation.
#' @param parallel Logical value indicating whether to use parallel processing.
#' @param n_cores Numeric value specifying the number of cores to use for parallel processing.
#' @param verbose Logical value indicating whether to print detailed progress messages.
#'
#' @return A symmetric correlation matrix with point-biserial correlation coefficients.
#'
#' @importFrom foreach foreach %dopar%
#'
#' @examples
#' \dontrun{
#' result <- calculate_pointbiserial_correlation(data, na.rm = TRUE, parallel = TRUE, n_cores = 2, verbose = FALSE)
#' }
calculate_pointbiserial_correlation <- function(data, na.rm, parallel, n_cores, verbose) {
  n_vars <- ncol(data)
  correlation_matrix <- matrix(NA, n_vars, n_vars)
  colnames(correlation_matrix) <- colnames(data)
  rownames(correlation_matrix) <- colnames(data)
  
  continuous_vars <- which(sapply(data, is_continuous))
  dichotomous_vars <- which(sapply(data, is_dichotomous))
  
  if (parallel) {
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("Package 'doParallel' is required for parallel processing. Please install it.")
    }
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      parallel::stopCluster(cl)
      foreach::registerDoSEQ()  # Reset to sequential execution
    })
    
    # Bind the %dopar% operator explicitly from foreach:
    `%dopar%` <- foreach::`%dopar%`
    
    # Create all combinations of continuous and dichotomous variable indices
    var_pairs <- expand.grid(continuous_vars, dichotomous_vars)
    var_pairs <- var_pairs[var_pairs[, 1] != var_pairs[, 2], , drop = FALSE]
    
    results <- foreach::foreach(k = seq_len(nrow(var_pairs)), .combine = rbind, .packages = "ltm") %dopar% {
      i <- var_pairs[k, 1]
      j <- var_pairs[k, 2]
      if (verbose) message("Calculating point-biserial correlation between: ", 
                           colnames(data)[i], " and ", colnames(data)[j])
      corr_value <- tryCatch(
        ltm::biserial.cor(data[[i]], as.numeric(data[[j]]),
                          use = if (na.rm) "complete.obs" else "all.obs"),
        error = function(e) NA
      )
      data.frame(i = i, j = j, corr = corr_value)
    }
    
    # Populate the symmetric correlation matrix
    for (row in seq_len(nrow(results))) {
      i <- results$i[row]
      j <- results$j[row]
      correlation_matrix[i, j] <- results$corr[row]
      correlation_matrix[j, i] <- results$corr[row]
    }
    
  } else {
    for (i in continuous_vars) {
      for (j in dichotomous_vars) {
        if (i == j) {
          correlation_matrix[i, j] <- 1
        } else {
          if (verbose) message("Calculating point-biserial correlation between: ", 
                               colnames(data)[i], " and ", colnames(data)[j])
          corr_value <- tryCatch(
            ltm::biserial.cor(data[[i]], as.numeric(data[[j]]),
                              use = if (na.rm) "complete.obs" else "all.obs"),
            error = function(e) NA
          )
          correlation_matrix[i, j] <- corr_value
          correlation_matrix[j, i] <- corr_value
        }
      }
    }
  }
  
  return(correlation_matrix)
}

#' Calculate Polychoric Correlation Matrix
#'
#' This function computes the polychoric correlation matrix for datasets with ordered factor
#' variables. It is used when the data consists of ordinal measures.
#'
#' @param data A data frame containing ordered factor variables.
#' @param verbose Logical value indicating whether to print progress messages.
#'
#' @return A polychoric correlation matrix.
#'
#' @importFrom polycor hetcor
#'
#' @examples
#' \dontrun{
#' corr_mat <- calculate_polychoric_correlation(ordered_data, verbose = TRUE)
#' }
calculate_polychoric_correlation <- function(data, verbose) {
  if (verbose) message("Calculating polychoric correlation matrix.")
  # polycor::hetcor returns a list with element 'correlations'
  return(polycor::hetcor(data)$correlations)
}

#' Find High Correlation Pairs
#'
#' This function identifies pairs of variables in the correlation matrix that have an
#' absolute correlation above a specified threshold. Only the upper triangle is considered
#' to avoid duplicate pairs.
#'
#' @param corr_matrix A correlation matrix.
#' @param threshold Numeric value specifying the correlation threshold.
#'
#' @return A matrix of indices (with columns for row and column) for variable pairs that exceed the threshold.
#'
#' @examples
#' \dontrun{
#' high_corr <- find_high_correlation(corr_matrix, threshold = 0.7)
#' }
find_high_correlation <- function(corr_matrix, threshold) {
  indices <- which(abs(corr_matrix) > threshold & !is.na(corr_matrix), arr.ind = TRUE)
  # Remove duplicate pairs (only consider the upper triangle)
  indices <- indices[indices[, 1] < indices[, 2], , drop = FALSE]
  return(indices)
}

#' Check if Variable is Dichotomous
#'
#' This function checks whether a variable is dichotomous (i.e., has exactly two unique non-missing values).
#'
#' @param x A vector representing a variable.
#'
#' @return \code{TRUE} if the variable is dichotomous, otherwise \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' is_dichotomous(c(0, 1, 1, 0))
#' }
is_dichotomous <- function(x) {
  unique_vals <- unique(x[!is.na(x)])
  length(unique_vals) == 2
}

#' Check if Variable is Continuous
#'
#' This function determines if a variable is continuous. A variable is considered continuous if
#' it is numeric and has more than 2 unique non-missing values.
#'
#' @param x A vector representing a variable.
#'
#' @return \code{TRUE} if the variable is continuous, otherwise \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' is_continuous(rnorm(100))
#' }
is_continuous <- function(x) {
  x <- x[!is.na(x)]
  is.numeric(x) && (length(unique(x)) > 2)
}

