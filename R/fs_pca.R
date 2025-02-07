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

