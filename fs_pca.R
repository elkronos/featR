# Required Libraries
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(bigstatsr)
  library(future)
  library(future.apply)
  library(Matrix)
  library(RColorBrewer)
  library(ggplot2)
  library(viridis)
})

#############################
# 1. Data Validation Module #
#############################

#' Check Data Validity for PCA
#'
#' Validates the input data ensuring it is non-null, has more than one row,
#' and includes at least one numeric column.
#'
#' @param data A data frame or data table.
#' @return Invisibly returns TRUE if data is valid; otherwise, stops execution with an error.
check_data_validity <- function(data) {
  data <- as.data.table(data)
  
  if (is.null(data) || nrow(data) <= 1) {
    stop("Invalid data for PCA: Data must be non-null and have more than one row.")
  }
  
  if (ncol(Filter(is.numeric, data)) == 0) {
    stop("Invalid data for PCA: No numeric columns found.")
  }
  
  invisible(TRUE)
}

#' Identify Label Columns
#'
#' Identifies columns that contain non-numeric data (character or factor) to be used as labels.
#'
#' @param data A data frame or data table.
#' @return A character vector with names of the label columns.
identify_label_cols <- function(data) {
  dt <- as.data.table(data)
  label_cols <- names(dt)[sapply(dt, function(col) is.character(col) || is.factor(col))]
  return(label_cols)
}

###############################
# 2. PCA Computation Module   #
###############################

#' Perform PCA on Numeric Columns
#'
#' Computes principal components on the numeric portion of the data.
#' For small datasets, the function uses the built-in \code{prcomp} function.
#' For large datasets (i.e. when \code{nrows * ncols >= 1e7}), it uses \code{bigstatsr}.
#'
#' @param data A data frame or data table.
#' @param label_cols Character vector of columns to treat as labels (ignored during PCA).
#' @param num_pc Number of principal components to compute.
#' @param scale_data Logical indicating whether to scale the data (default TRUE).
#' @param center_data Logical indicating whether to center the data (default TRUE).
#' @param ncores Number of cores to use for computation (default uses all available cores).
#' @return A list with components: \code{u} (scores), \code{d} (singular values), and \code{v} (loadings).
perform_pca <- function(data, label_cols, num_pc, scale_data = TRUE, center_data = TRUE, 
                        ncores = future::availableCores()) {
  dt <- as.data.table(data)
  # Exclude label columns
  numeric_cols <- setdiff(names(dt), label_cols)
  
  # Remove rows with missing values in numeric columns
  dt <- dt[complete.cases(dt[, ..numeric_cols]), ]
  
  n_rows <- nrow(dt)
  n_cols <- length(numeric_cols)
  
  # Use built-in prcomp for small datasets
  if (n_rows * n_cols < 1e7) {
    message("Using prcomp for PCA computation on a small dataset.")
    pca_obj <- prcomp(dt[, ..numeric_cols], center = center_data, scale. = scale_data)
    # Return a list similar to SVD output
    svd <- list(u = pca_obj$x, d = pca_obj$sdev, v = pca_obj$rotation)
  } else {
    message("Using bigstatsr for PCA computation on a large dataset.")
    # Create Filebacked Big Matrix (FBM)
    big_mat <- FBM(n_rows, n_cols, backingfile = tempfile())
    big_mat[,] <- as.matrix(dt[, ..numeric_cols])
    
    # Ensure the number of PCs is valid
    max_possible <- min(n_rows - 1, n_cols)
    k <- min(num_pc, max_possible)
    if (k <= 0) stop("Invalid number of principal components computed from data dimensions.")
    
    svd <- big_SVD(big_mat, 
                   fun.scaling = big_scale(center = center_data, scale = scale_data),
                   k = k,
                   ncores = ncores)
  }
  
  return(svd)
}

#' Create PCA Results Structure
#'
#' Constructs a list containing PCA scores, loadings, variance explained, and
#' a combined data table with optional label columns.
#'
#' @param svd List object returned from \code{perform_pca}.
#' @param num_pc Number of principal components to include.
#' @param data Original data frame or data table.
#' @param label_cols Character vector of label column names.
#' @return A list with PCA loadings, scores, variance explained, and a data table (\code{pca_df}).
create_pca_results <- function(svd, num_pc, data, label_cols) {
  # Validate the number of computed components
  if (num_pc > ncol(svd$u)) {
    stop(sprintf("Requested %d PCs, but only %d available.", num_pc, ncol(svd$u)))
  }
  
  # Calculate variance explained by each principal component
  var_explained <- (svd$d[1:num_pc]^2) / sum(svd$d^2)
  
  # Extract loadings and scores
  pc_loadings <- svd$v[, 1:num_pc, drop = FALSE]
  pc_scores <- svd$u[, 1:num_pc, drop = FALSE]
  colnames(pc_loadings) <- paste0("PC", 1:num_pc)
  colnames(pc_scores) <- paste0("PC", 1:num_pc)
  
  # Attach label columns if provided
  dt <- as.data.table(data)
  label_data <- if (length(label_cols) > 0) dt[, ..label_cols] else data.table()
  
  pca_df <- cbind(data.table(pc_scores), label_data)
  
  return(list(pc_loadings  = pc_loadings,
              pc_scores    = pc_scores,
              var_explained = var_explained,
              pca_df       = pca_df))
}

#########################
# 3. Visualization Code #
#########################

#' Plot PCA Results
#'
#' Generates a scatter plot of the first two principal components. Points can be colored
#' based on a provided label column. For many unique labels, the function switches to the
#' 'viridis' palette.
#'
#' @param pca_result A list returned from \code{create_pca_results}.
#' @param label_col Name of the column in \code{pca_df} to be used for coloring points.
#' @return None; the function prints the plot.
plot_pca_results <- function(pca_result, label_col) {
  dt <- copy(pca_result$pca_df)
  
  if (!label_col %in% names(dt)) {
    stop(sprintf("Label column '%s' not found in PCA results.", label_col))
  }
  
  # Determine number of unique labels
  unique_labels <- unique(dt[[label_col]])
  num_labels <- length(unique_labels)
  
  # Construct the plot
  p <- ggplot(dt, aes(x = PC1, y = PC2, color = .data[[label_col]])) +
    geom_point(alpha = 0.8, size = 2) +
    ggtitle("PCA Results") +
    xlab(sprintf("PC1 (%.2f%% Variance)", pca_result$var_explained[1] * 100)) +
    ylab(sprintf("PC2 (%.2f%% Variance)", pca_result$var_explained[2] * 100))
  
  # Choose an appropriate color scale
  if (num_labels > 9) {
    warning("More than 9 unique labels; using viridis color scale.")
    p <- p + scale_color_viridis_d()
  } else {
    p <- p + scale_color_brewer(palette = "Set1")
  }
  
  print(p)
  message("PCA plot generated successfully.")
}

###############################
# 4. Main Function: fs_pca   #
###############################

#' Full PCA Analysis Wrapper (fs_pca)
#'
#' Runs the full PCA analysis workflow: validates data, performs PCA, creates results,
#' and optionally plots the PCA if a label column is provided.
#'
#' @param data A data frame or data table.
#' @param num_pc Number of principal components to retain (default 2).
#' @param scale_data Logical to scale data (default TRUE).
#' @param center_data Logical to center data (default TRUE).
#' @param label_col Optional column name for labeling in the plot (default NULL).
#' @param ncores Number of cores for parallel computation (default all available cores).
#' @return A list with PCA loadings, scores, variance explained, and a data table.
#' @examples
#' \dontrun{
#' result <- fs_pca(mtcars, num_pc = 2, label_col = "cyl")
#' }
fs_pca <- function(data, num_pc = 2, scale_data = TRUE, center_data = TRUE,
                   label_col = NULL, ncores = future::availableCores()) {
  dt <- as.data.table(data)
  
  # Validate data
  check_data_validity(dt)
  
  # Identify label columns (all non-numeric columns)
  label_cols <- identify_label_cols(dt)
  
  # Compute PCA using appropriate method
  svd <- perform_pca(dt, label_cols, num_pc, scale_data, center_data, ncores)
  
  # Create structured PCA results
  pca_results <- create_pca_results(svd, num_pc, dt, label_cols)
  
  # Plot if a valid label column is provided
  if (!is.null(label_col)) {
    plot_pca_results(pca_results, label_col)
  }
  
  return(pca_results)
}

###############################
# 5. Unit Testing for fs_pca  #
###############################

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
