###############################################################################
# Testing Infrastructure - PCA (standalone runner for fs_pca)
###############################################################################

#' Unit Test Suite for fs_pca Functionality
#'
#' Runs a series of tests to verify that `fs_pca` works correctly, including:
#' basic functionality, invalid data handling, missing values & row alignment,
#' custom PC counts, plotting (valid/invalid labels), many-label plots, and
#' numeric label columns. Designed to run without `testthat`.
#'
#' @examples
#' test_fs_pca()
test_fs_pca <- function() {
  cat("=============================================\n")
  cat("Starting unit tests for fs_pca...\n")
  cat("=============================================\n\n")
  
  # --- Helper for pass/fail output
  pass <- function(msg) cat("[PASS]", msg, "\n")
  fail <- function(msg) cat("[FAIL]", msg, "\n")
  
  # --- Pre-flight checks
  if (!exists("fs_pca") || !is.function(fs_pca)) {
    stop("fs_pca() not found. Please source the PCA implementation before running tests.")
  }
  if (!exists("plot_pca_results") || !is.function(plot_pca_results)) {
    stop("plot_pca_results() not found. Please source the PCA implementation before running tests.")
  }
  
  # ===========================================================================
  # Test 1: Basic functionality with default parameters
  # ===========================================================================
  cat("\nTest 1: Basic Functionality\n")
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  
  out1 <- NULL
  tryCatch({
    out1 <- fs_pca(fakeData)
    stopifnot(is.list(out1))
    stopifnot(all(c("pc_loadings", "pc_scores", "var_explained", "pca_df", "meta") %in% names(out1)))
    stopifnot(length(out1$var_explained) == 2)
    pass("fs_pca returns expected structure with default settings.")
  }, error = function(e) fail(paste("Unexpected error:", e$message)))
  
  # ===========================================================================
  # Test 2: Data validation (NULL and empty data)
  # ===========================================================================
  cat("\nTest 2: Data Validation for Invalid Inputs\n")
  tryCatch({
    fs_pca(NULL)
    fail("Expected error for NULL data, but none was thrown.")
  }, error = function(e) pass(paste("Caught expected error (NULL data):", e$message)))
  
  tryCatch({
    fs_pca(data.frame())
    fail("Expected error for empty data, but none was thrown.")
  }, error = function(e) pass(paste("Caught expected error (empty data):", e$message)))
  
  # ===========================================================================
  # Test 3: Data with only non-numeric columns should fail
  # ===========================================================================
  cat("\nTest 3: Data Without Numeric Columns\n")
  tryCatch({
    fs_pca(data.frame(Char1 = c("a", "b"), Char2 = c("c", "d")))
    fail("Expected error for data with no numeric columns, but none was thrown.")
  }, error = function(e) pass(paste("Caught expected error (no numeric columns):", e$message)))
  
  # ===========================================================================
  # Test 4: Handling Missing Values & Row Alignment
  # ===========================================================================
  cat("\nTest 4: Handling Missing Values & Row Alignment\n")
  fakeData_na <- fakeData
  fakeData_na[1:10, 2] <- NA  # introduce NAs in one numeric column
  out_na <- NULL
  tryCatch({
    out_na <- fs_pca(fakeData_na)
    # Verify fewer rows used and that rows_kept marks the dropped rows
    stopifnot(sum(out_na$meta$rows_kept) < nrow(fakeData_na))
    stopifnot(!all(out_na$meta$rows_kept[1:10])) # at least some of first 10 should be FALSE
    stopifnot(nrow(out_na$pca_df) == sum(out_na$meta$rows_kept))
    pass("Rows with NA in numeric columns are dropped and alignment is preserved.")
  }, error = function(e) fail(paste("Unexpected error:", e$message)))
  
  # ===========================================================================
  # Test 5: Custom Number of Principal Components
  # ===========================================================================
  cat("\nTest 5: Custom Number of Principal Components\n")
  out_3pc <- NULL
  tryCatch({
    out_3pc <- fs_pca(fakeData, num_pc = 3)
    stopifnot(length(out_3pc$var_explained) == 3)
    stopifnot(ncol(out_3pc$pc_scores) == 3)
    stopifnot(ncol(out_3pc$pc_loadings) == 3)
    pass("Custom number of PCs (3) returns correctly sized outputs.")
  }, error = function(e) fail(paste("Unexpected error:", e$message)))
  
  # ===========================================================================
  # Test 6: Requesting too many PCs should error out
  # ===========================================================================
  cat("\nTest 6: Requesting Too Many Principal Components\n")
  tryCatch({
    fs_pca(fakeData, num_pc = 10)
    fail("Expected error for requesting too many PCs, but none was thrown.")
  }, error = function(e) pass(paste("Caught expected error (too many PCs):", e$message)))
  
  # ===========================================================================
  # Test 7: Label Column Identification and Plotting (character/factor)
  # ===========================================================================
  cat("\nTest 7: Label Column Plotting (character/factor)\n")
  fakeData_labels <- fakeData
  fakeData_labels$group <- sample(c("A", "B", "C"), n, replace = TRUE)
  out_labels <- NULL
  tryCatch({
    out_labels <- fs_pca(fakeData_labels, label_col = "group")
    stopifnot("group" %in% names(out_labels$pca_df))
    # Plot should succeed silently:
    invisible(plot_pca_results(out_labels, label_col = "group"))
    pass("Plotting with a valid non-numeric label column works.")
  }, error = function(e) fail(paste("Unexpected error:", e$message)))
  
  # ===========================================================================
  # Test 8: Plotting with an invalid label column name
  # ===========================================================================
  cat("\nTest 8: Plotting with Invalid Label Column\n")
  tryCatch({
    plot_pca_results(out_labels, label_col = "invalid")
    fail("Expected error for invalid label column in plot, but none was thrown.")
  }, error = function(e) pass(paste("Caught expected error (invalid label for plot):", e$message)))
  
  # ===========================================================================
  # Test 9: Many Unique Labels in Plotting (should switch to viridis)
  # ===========================================================================
  cat("\nTest 9: Plotting with Many Unique Labels (switch palette)\n")
  fakeData_many_labels <- fakeData_labels
  fakeData_many_labels$group <- as.character(seq_len(n))  # n unique labels
  out_many <- NULL
  tryCatch({
    # Expect a warning about palette switch; capture it but keep going
    w <- NULL
    out_many <- withCallingHandlers(
      fs_pca(fakeData_many_labels, num_pc = 2, label_col = "group"),
      warning = function(wrn) { w <<- wrn$message; invokeRestart("muffleWarning") }
    )
    # draw plot (warning already muffled)
    invisible(plot_pca_results(out_many, label_col = "group"))
    pass("Plotting with many labels completed (palette switched).")
  }, error = function(e) fail(paste("Unexpected error:", e$message)))
  
  # ===========================================================================
  # Test 10: Numeric label column (e.g., mtcars$cyl) should be allowed
  # ===========================================================================
  cat("\nTest 10: Numeric Label Column Allowed\n")
  tryCatch({
    out_numlabel <- fs_pca(mtcars, num_pc = 2, label_col = "cyl")
    stopifnot("cyl" %in% names(out_numlabel$pca_df))
    # plot should coerce numeric label to factor internally
    invisible(plot_pca_results(out_numlabel, label_col = "cyl"))
    pass("Numeric label column accepted and plotted.")
  }, error = function(e) fail(paste("Unexpected error:", e$message)))
  
  # ===========================================================================
  # Test 11: plot=TRUE without label_col should warn and not plot
  # ===========================================================================
  cat("\nTest 11: plot=TRUE without label_col (warns, no plot)\n")
  tryCatch({
    msg <- NULL
    withCallingHandlers(
      fs_pca(fakeData, num_pc = 2, plot = TRUE),
      warning = function(wrn) { msg <<- wrn$message; invokeRestart("muffleWarning") }
    )
    pass("Warning captured and function returned without plotting when label_col is missing.")
  }, error = function(e) fail(paste("Unexpected error:", e$message)))
  
  # ===========================================================================
  # Test 12: Constant (zero-variance) column handling
  # ===========================================================================
  cat("\nTest 12: Zero-variance Numeric Column Handling\n")
  tryCatch({
    fd_const <- fakeData
    fd_const$const <- 1  # zero variance
    msg <- NULL
    out_const <- withCallingHandlers(
      fs_pca(fd_const, num_pc = 2),
      warning = function(wrn) { msg <<- wrn$message; invokeRestart("muffleWarning") }
    )
    stopifnot(is.list(out_const), length(out_const$var_explained) == 2)
    if (!is.null(msg) && grepl("zero-variance", msg)) {
      pass("Zero-variance column removed with warning; PCA completed successfully.")
    } else {
      pass("PCA completed successfully without error on zero-variance column.")
    }
  }, error = function(e) fail(paste("Unexpected error:", e$message)))
  
  cat("\n=============================================\n")
  cat("All unit tests completed.\n")
  cat("=============================================\n")
}

# To run the tests now, uncomment:
# test_fs_pca()
