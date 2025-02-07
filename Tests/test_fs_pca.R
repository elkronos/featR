###############################################################################
# Testing Infrastructure - PCA
###############################################################################

#' Unit Test Suite for fs_pca Functionality
#'
#' Runs a series of tests to verify that \code{fs_pca} works correctly, including:
#' basic functionality, handling invalid data, missing values, custom PC numbers, and plotting.
#'
#' @examples
#' \dontrun{
#' test_fs_pca()
#' }
test_fs_pca <- function() {
  cat("Starting unit tests for fs_pca...\n")
  
  # Test 1: Basic functionality with default parameters
  cat("Test 1: Basic Functionality\n")
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  result <- fs_pca(fakeData)
  print(result$var_explained)
  
  # Test 2: Data validation (NULL and empty data)
  cat("Test 2: Data Validation for Invalid Inputs\n")
  tryCatch({
    fs_pca(NULL)
  }, error = function(e) {
    cat("Caught expected error (NULL data):", e$message, "\n")
  })
  
  tryCatch({
    fs_pca(data.frame())
  }, error = function(e) {
    cat("Caught expected error (empty data):", e$message, "\n")
  })
  
  # Test 3: Data with only non-numeric columns should fail
  cat("Test 3: Data Without Numeric Columns\n")
  tryCatch({
    fs_pca(data.frame(Char1 = c("a", "b"), Char2 = c("c", "d")))
  }, error = function(e) {
    cat("Caught expected error (no numeric columns):", e$message, "\n")
  })
  
  # Test 4: Handling Missing Values
  cat("Test 4: Handling Missing Values\n")
  fakeData_na <- fakeData
  fakeData_na[1:10, 2] <- NA
  result_na <- fs_pca(fakeData_na)
  print(result_na$var_explained)
  
  # Test 5: Custom Number of Principal Components
  cat("Test 5: Custom Number of Principal Components\n")
  result_3pc <- fs_pca(fakeData, num_pc = 3)
  print(result_3pc$var_explained)
  
  # Test 6: Requesting too many PCs should error out
  cat("Test 6: Requesting Too Many Principal Components\n")
  tryCatch({
    fs_pca(fakeData, num_pc = 10)
  }, error = function(e) {
    cat("Caught expected error (too many PCs):", e$message, "\n")
  })
  
  # Test 7: Label Column Identification and Plotting
  cat("Test 7: Label Column Plotting\n")
  fakeData_labels <- fakeData
  fakeData_labels$group <- sample(c("A", "B", "C"), n, replace = TRUE)
  result_labels <- fs_pca(fakeData_labels, label_col = "group")
  print(result_labels$var_explained)
  
  # Test 8: Plotting with an invalid label column name
  cat("Test 8: Plotting with Invalid Label Column\n")
  tryCatch({
    plot_pca_results(result_labels, label_col = "invalid")
  }, error = function(e) {
    cat("Caught expected error (invalid label for plot):", e$message, "\n")
  })
  
  # Test 9: Many Unique Labels in Plotting
  cat("Test 9: Plotting with Many Unique Labels\n")
  fakeData_many_labels <- fakeData_labels
  fakeData_many_labels$group <- as.character(1:n)
  result_many_labels <- fs_pca(fakeData_many_labels, num_pc = 2, label_col = "group")
  plot_pca_results(result_many_labels, label_col = "group")
  
  cat("All unit tests completed.\n")
}

# Uncomment the line below to run the unit tests:
# test_fs_pca()
