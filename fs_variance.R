#' Ensure a package is installed and loaded
#'
#' This helper function checks if a package is installed and loaded. If not,
#' it installs the package and loads it.
#'
#' @param pkg A character string specifying the name of the package.
#' @return None
#' @examples
#' ## Not run: 
#' ensure_package("dplyr")
#' ## End(Not run)
ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  if (!require(pkg, character.only = TRUE)) {
    stop(paste("Package", pkg, "failed to load."))
  }
}

#' Apply variance thresholding to a numeric dataset
#'
#' This function applies variance thresholding to a numeric dataset, removing features
#' whose variances are not above a certain threshold. The function returns a warning 
#' if no features have variance above the threshold and returns NULL. It will also
#' fail with an appropriate error message if the inputs are not as expected, and 
#' converts data frames to matrices if necessary for calculations.
#'
#' @param data A numeric matrix or data frame containing the input data. All columns of
#'             data frames should be numeric.
#' @param threshold A non-negative numeric value specifying the variance threshold. Features
#'                  with variances above this threshold are kept in the dataset. Default is 0.5.
#' @param log_progress A logical value indicating whether to log progress messages. Default is FALSE.
#' @return A numeric matrix containing the thresholded data. If the data was originally a
#'         data frame, it will be returned as a matrix. If no features have variance above 
#'         the threshold, the function returns NULL.
#' @examples
#' ## Not run: 
#' # Create a sample dataset with 10 features and 100 observations
#' set.seed(123)
#' n_features <- 10
#' n_observations <- 100
#' data <- matrix(0, ncol = n_features, nrow = n_observations)
#' for (i in 1:n_features) {
#'   if (i %% 2 == 0) {
#'     data[, i] <- rnorm(n_observations, mean = 0, sd = 0.1)
#'   } else {
#'     data[, i] <- rnorm(n_observations, mean = 0, sd = 1)
#'   }
#' }
#' colnames(data) <- paste0("Feature", 1:n_features)
#'
#' # Apply variance thresholding with a threshold of 0.5
#' thresholded_data <- fs_variance(data, 0.5)
#'
#' # Check the dimensions of the thresholded dataset
#' dim(thresholded_data)
#' ## End(Not run)
fs_variance <- function(data, threshold = 0.5, log_progress = FALSE) {
  ensure_package("parallel")  # Ensure the parallel package is installed and loaded
  ensure_package("data.table")  # Ensure the data.table package is installed and loaded
  
  # Validate threshold input
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || !is.finite(threshold)) {
    stop("Threshold must be a single non-negative, finite numeric value.")
  }
  
  # Validate data input
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or a data frame.")
  }
  
  if (is.data.frame(data)) {
    if (!all(sapply(data, is.numeric))) {
      stop("All columns of the data frame must be numeric.")
    }
    if (log_progress) message("Converting data frame to data.table for efficient processing...")
    data <- data.table::as.data.table(data)  # Convert data frame to data.table for efficiency
  } else {
    if (log_progress) message("Converting matrix to data.table for efficient processing...")
    data <- data.table::as.data.table(data)
  }
  
  # Set up parallel backend
  num_cores <- parallel::detectCores() - 1  # Use one less than the total number of cores
  cl <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl)
  
  # Calculate variances for each feature using foreach and %dopar%
  if (log_progress) message("Calculating variances using parallel processing...")
  variances <- foreach(i = 1:ncol(data), .combine = c) %dopar% {
    var(data[[i]])
  }
  
  # Stop the cluster
  parallel::stopCluster(cl)
  
  # Identify features with variance above the threshold
  selected_features <- variances > threshold
  
  # Check if any features meet the threshold
  if (!any(selected_features)) {
    warning("No features have variance above the specified threshold.")
    return(NULL)
  }
  
  # Subset data to retain features with variance above the threshold
  filtered_data <- data[, which(selected_features), with = FALSE]
  
  if (log_progress) message("Variance thresholding completed.")
  return(as.matrix(filtered_data))
}

#' User Acceptance Testing for fs_variance function
#'
#' This function runs a series of tests on the fs_variance function to ensure it works
#' as expected. It includes tests for simple numeric matrices, data frames with mixed variances,
#' data frames with non-numeric columns, and various edge cases.
#'
#' @examples
#' ## Not run:
#' test_fs_variance()
#' ## End(Not run)
test_fs_variance <- function() {
  cat("Running UAT for fs_variance...\n")
  
  # Test 1: Simple numeric matrix
  set.seed(123)
  data1 <- matrix(rnorm(1000), ncol = 10)
  result1 <- fs_variance(data1, 0.5)
  print(result1)
  expect_type(result1, "double")
  expect_true(ncol(result1) <= ncol(data1))
  
  # Test 2: Simple numeric data frame
  set.seed(123)
  df2 <- data.frame(matrix(rnorm(1000), ncol = 10))
  result2 <- fs_variance(df2, 0.5)
  print(result2)
  expect_type(result2, "double")
  expect_true(ncol(result2) <= ncol(df2))
  
  # Test 3: Data frame with all variances below threshold
  set.seed(123)
  df3 <- data.frame(matrix(rnorm(1000, mean = 0, sd = 0.1), ncol = 10))
  result3 <- fs_variance(df3, 0.5)
  print(result3)
  expect_null(result3)
  
  # Test 4: Data frame with mixed variances
  set.seed(123)
  df4 <- data.frame(matrix(0, ncol = 10, nrow = 100))
  for (i in 1:10) {
    if (i %% 2 == 0) {
      df4[, i] <- rnorm(100, mean = 0, sd = 0.1)
    } else {
      df4[, i] <- rnorm(100, mean = 0, sd = 1)
    }
  }
  result4 <- fs_variance(df4, 0.5)
  print(result4)
  expect_type(result4, "double")
  expect_true(ncol(result4) < ncol(df4))
  
  # Test 5: Data frame with non-numeric columns
  set.seed(123)
  df5 <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = sample(letters, 100, replace = TRUE)
  )
  expect_error(fs_variance(df5, 0.5), "All columns of the data frame must be numeric.")
  
  # Test 6: Data frame with missing values
  set.seed(123)
  df6 <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = c(rnorm(95), rep(NA, 5))
  )
  result6 <- fs_variance(df6, 0.5)
  print(result6)
  expect_type(result6, "double")
  expect_true(ncol(result6) < ncol(df6))
  
  # Test 7: Matrix with no variances above threshold
  set.seed(123)
  data7 <- matrix(rnorm(1000, mean = 0, sd = 0.1), ncol = 10)
  result7 <- fs_variance(data7, 1)
  print(result7)
  expect_null(result7)
  
  # Test 8: Error handling for non-numeric threshold
  set.seed(123)
  df8 <- data.frame(matrix(rnorm(1000), ncol = 10))
  expect_error(fs_variance(df8, "a"), "Threshold must be a single non-negative, finite numeric value.")
  
  # Test 9: Error handling for negative threshold
  set.seed(123)
  df9 <- data.frame(matrix(rnorm(1000), ncol = 10))
  expect_error(fs_variance(df9, -1), "Threshold must be a single non-negative, finite numeric value.")
  
  cat("UAT for fs_variance completed.\n")
}

# Run the UAT function
test_fs_variance()
