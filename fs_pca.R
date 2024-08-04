# Required Libraries
library(data.table)
library(tidyverse)
library(bigstatsr)
library(future)
library(future.apply)
library(Matrix)
library(RColorBrewer)
library(ggplot2)
library(viridis)  # Load viridis for the new color palette

#' Check if the data is valid for PCA
#'
#' This function checks if the data is valid for PCA analysis.
#'
#' @param data A data frame or data table to be checked.
#' @importFrom data.table is.data.table
#' @return None. Stops execution if the data is not valid.
#' @examples
#' \dontrun{
#' check_data_validity(mtcars)
#' }
check_data_validity <- function(data) {
  if (is.null(data) || nrow(data) <= 1) {
    stop("Invalid data for PCA: Ensure it has more than one row and is not NULL.")
  }
  if (ncol(Filter(is.numeric, data)) == 0) {
    stop("No numeric columns found for PCA.")
  }
}

#' Identify label columns in the data
#'
#' This function identifies label columns in the data which are either character or factor type.
#'
#' @param data A data frame or data table containing the data.
#' @importFrom data.table is.data.table
#' @return A character vector of column names that are labels.
#' @examples
#' \dontrun{
#' identify_label_cols(mtcars)
#' }
identify_label_cols <- function(data) {
  label_cols <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]
  return(label_cols)
}

#' Perform PCA on the numeric columns of the data
#'
#' This function performs PCA on the numeric columns of the data after handling missing values.
#'
#' @param data A data frame or data table containing the data.
#' @param label_cols A character vector of label column names.
#' @param scale_data Logical, indicating whether to scale the data. Default is TRUE.
#' @param center_data Logical, indicating whether to center the data. Default is TRUE.
#' @importFrom bigstatsr FBM big_SVD big_scale
#' @importFrom future availableCores
#' @return An object of class 'big_SVD' containing the PCA results.
#' @examples
#' \dontrun{
#' svd <- perform_pca(mtcars, label_cols = c("carb"))
#' }
perform_pca <- function(data, label_cols, scale_data = TRUE, center_data = TRUE) {
  numeric_cols <- setdiff(names(data), label_cols)
  
  # Handle missing values
  data <- data[complete.cases(data[, numeric_cols]), ]
  
  # Convert data to a Filebacked Big Matrix (FBM)
  big_data <- FBM(nrow(data), length(numeric_cols), backingfile = tempfile())
  big_data[,] <- as.matrix(data[, numeric_cols])
  
  # Determine the number of components to compute
  k <- min(nrow(big_data), ncol(big_data)) - 1
  if (k <= 0) stop("The data matrix must have more than one row and one column for PCA.")
  
  # Perform SVD which can be used to derive PCA results
  options(bigstatsr.ncores = future::availableCores())
  svd <- big_SVD(big_data, fun.scaling = big_scale(center = center_data, scale = scale_data), k = k)
  
  return(svd)
}

#' Create PCA results including loadings, scores, and variance explained
#'
#' This function creates PCA results including loadings, scores, and variance explained.
#'
#' @param svd An object of class 'big_SVD' containing the PCA results.
#' @param num_pc Number of principal components to retain.
#' @param data A data frame or data table containing the data.
#' @param label_cols A character vector of label column names.
#' @importFrom data.table data.table as.data.table
#' @return A list containing PCA loadings, scores, variance explained, and a data table of PCA results.
#' @examples
#' \dontrun{
#' svd <- perform_pca(mtcars, label_cols = c("carb"))
#' pca_results <- create_pca_results(svd, num_pc = 2, data = mtcars, label_cols = c("carb"))
#' }
create_pca_results <- function(svd, num_pc, data, label_cols) {
  print(paste("Number of principal components requested:", num_pc))
  print("Structure of SVD result:")
  print(str(svd))
  print(paste("Data columns:", names(data)))
  print(paste("Label columns:", label_cols))
  
  if (num_pc > ncol(svd$u)) {
    stop("Number of principal components exceeds the available number.")
  }
  
  var_explained <- round(svd$d[1:num_pc]^2 / sum(svd$d^2), 2)
  
  pc_loadings <- round(svd$v[, 1:num_pc], 2)
  pc_scores <- round(svd$u[, 1:num_pc], 2)
  colnames(pc_loadings) <- colnames(pc_scores) <- paste0("PC", 1:num_pc)
  
  # Select the label columns correctly
  if (length(label_cols) > 0) {
    label_data <- data[, label_cols, drop = FALSE]
  } else {
    label_data <- data.frame()
  }
  
  pca_df <- data.table(as.data.table(pc_scores), label_data)
  
  return(list(pc_loadings = pc_loadings,
              pc_scores = pc_scores,
              var_explained = var_explained,
              pca_df = pca_df))
}

#' Plot PCA results
#'
#' This function plots PCA results, coloring the points based on a specified label column.
#'
#' @param pca_result A list containing PCA loadings, scores, variance explained, and a data table of PCA results.
#' @param label_col A character string specifying the column name to use for coloring the points.
#' @importFrom ggplot2 ggplot geom_point scale_color_brewer scale_color_viridis_d ggtitle xlab ylab
#' @return None. Generates and displays a plot.
#' @examples
#' \dontrun{
#' svd <- perform_pca(mtcars, label_cols = c("carb"))
#' pca_results <- create_pca_results(svd, num_pc = 2, data = mtcars, label_cols = c("carb"))
#' plot_pca_results(pca_results, label_col = "carb")
#' }
plot_pca_results <- function(pca_result, label_col) {
  pca_df <- pca_result$pca_df
  
  if (!label_col %in% colnames(pca_df)) {
    stop("Label column does not exist in PCA result data.")
  }
  
  num_colors <- length(unique(pca_df[[label_col]]))
  if (num_colors > 9) {
    warning("Number of unique labels exceeds the available colors in the 'Set1' palette. Using 'viridis' palette.")
    pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = !!sym(label_col))) + 
      geom_point() +
      scale_color_viridis_d() +
      ggtitle("PCA Plot") +
      xlab(paste0("PC1 (", pca_result$var_explained[1] * 100, "%)")) +
      ylab(paste0("PC2 (", pca_result$var_explained[2] * 100, "%)"))
  } else {
    pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = !!sym(label_col))) + 
      geom_point() +
      scale_color_brewer(palette = "Set1") +
      ggtitle("PCA Plot") +
      xlab(paste0("PC1 (", pca_result$var_explained[1] * 100, "%)")) +
      ylab(paste0("PC2 (", pca_result$var_explained[2] * 100, "%)"))
  }
  
  print(pca_plot)
  
  message("PCA plot generated successfully.")
}

#' Wrapper function to perform PCA and plot results
#'
#' This function performs PCA on the data and optionally plots the results if a label column is specified.
#'
#' @param data A data frame or data table containing the data.
#' @param num_pc Number of principal components to retain. Default is 2.
#' @param scale_data Logical, indicating whether to scale the data. Default is TRUE.
#' @param center_data Logical, indicating whether to center the data. Default is TRUE.
#' @param label_col A character string specifying the column name to use for coloring the points. Default is NULL.
#' @return A list containing PCA loadings, scores, variance explained, and a data table of PCA results.
#' @examples
#' \dontrun{
#' result <- fs_pca(mtcars, num_pc = 2, label_col = "carb")
#' }
fs_pca <- function(data, num_pc = 2, scale_data = TRUE, center_data = TRUE, label_col = NULL) {
  check_data_validity(data)
  label_cols <- identify_label_cols(data)
  svd <- perform_pca(data, label_cols, scale_data, center_data)
  pca_results <- create_pca_results(svd, num_pc, data, label_cols)
  if (!is.null(label_col)) {
    plot_pca_results(pca_results, label_col)
  }
  return(pca_results)
}

#' Unit Test Function for fs_pca
#'
#' This function performs a series of unit tests on the fs_pca function to validate its functionality.
#'
#' @examples
#' \dontrun{
#' test_fs_pca()
#' }
test_fs_pca <- function() {
  cat("Running UAT for fs_pca...\n")
  
  # Test 1: Basic Functionality with Default Parameters
  cat("Test 1: Basic Functionality with Default Parameters\n")
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  result <- fs_pca(fakeData)
  print(result$var_explained)
  
  # Test 2: Check Data Validity - Invalid Data
  cat("Test 2: Check Data Validity - Invalid Data\n")
  tryCatch({
    fs_pca(NULL)
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  tryCatch({
    fs_pca(data.frame())
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  tryCatch({
    fs_pca(data.frame(X1 = c("a", "b"), X2 = c("c", "d")))
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  # Test 3: Handling Missing Values
  cat("Test 3: Handling Missing Values\n")
  fakeData_with_na <- fakeData
  fakeData_with_na[1:10, 2] <- NA
  result_na <- fs_pca(fakeData_with_na)
  print(result_na$var_explained)
  
  # Test 4: Custom Number of Principal Components
  cat("Test 4: Custom Number of Principal Components\n")
  result_custom_pc <- fs_pca(fakeData, num_pc = 3)
  print(result_custom_pc$var_explained)
  
  # Test 5: Invalid Number of Principal Components
  cat("Test 5: Invalid Number of Principal Components\n")
  tryCatch({
    fs_pca(fakeData, num_pc = 10)
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  # Test 6: Label Columns Identification
  cat("Test 6: Label Columns Identification\n")
  fakeData_with_labels <- fakeData
  fakeData_with_labels$label <- sample(c("A", "B", "C"), n, replace = TRUE)
  result_labels <- fs_pca(fakeData_with_labels, label_col = "label")
  print(result_labels$var_explained)
  
  # Test 7: Plot PCA Results with Valid Label Column
  cat("Test 7: Plot PCA Results with Valid Label Column\n")
  plot_pca_results(result_labels, label_col = "label")
  
  # Test 8: Plot PCA Results with Invalid Label Column
  cat("Test 8: Plot PCA Results with Invalid Label Column\n")
  tryCatch({
    plot_pca_results(result_labels, label_col = "invalid_label")
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  # Test 9: Too Many Unique Labels for Plotting
  cat("Test 9: Too Many Unique Labels for Plotting\n")
  fakeData_with_many_labels <- fakeData_with_labels
  fakeData_with_many_labels$label <- as.character(1:n)
  result_many_labels <- fs_pca(fakeData_with_many_labels, label_col = "label")
  tryCatch({
    plot_pca_results(result_many_labels, label_col = "label")
  }, error = function(e) {
    cat("Caught expected error:", e$message, "\n")
  })
  
  cat("UAT for fs_pca completed.\n")
}

# Run the test
test_fs_pca()
