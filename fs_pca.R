library(data.table)
library(tidyverse)
library(stats)

#' Perform PCA on Data
#'
#' This function performs principal component analysis (PCA) on a given dataset and returns the PCA results,
#' including the principal component loadings, scores, the proportion of variance explained by each principal component,
#' and a data table with the principal component scores and the labels.
#'
#' @param data A data frame containing the data to be analyzed. This data frame must contain at least one numeric column and one factor or character column for labels.
#' @param num_pc An integer indicating the number of principal components to return. Default is 2.
#' @param scale_data A logical indicating whether to scale the data before performing PCA. Default is TRUE.
#'
#' @return A list containing the following components:
#' \itemize{
#' \item pc_loadings: a matrix of principal component loadings.
#' \item pc_scores: a matrix of principal component scores.
#' \item var_explained: a vector of the proportion of variance explained by each principal component.
#' \item pca_df: a data table of principal component scores and labels.
#' }
#'
#' @importFrom data.table as.data.table
#' @importFrom dplyr select where is.character is.factor names all_of bind_cols
#' @importFrom stats prcomp summary Filter
#'
#' @examples
#' data(iris)
#' fs_pca(iris, num_pc = 2, scale_data = TRUE)
#'
#' @export

fs_pca <- function(data, num_pc = 2, scale_data = TRUE) {
  
  # Check if data is valid for PCA
  if (is.null(data) | nrow(data) <= 1 | ncol(Filter(is.numeric, data)) == 0) {
    stop("The input data is not valid for PCA. Ensure it has more than one row, contains numeric columns, and is not NULL.")
  }
  
  # Identify label columns
  label_cols <- data %>% select(where(is.character) | where(is.factor)) %>% names()
  
  if(length(label_cols) == 0) {
    stop("The data does not have any categorical or factor columns to use as labels.")
  }
  
  # Perform PCA on the numeric columns
  pca <- prcomp(select(data, -all_of(label_cols)), scale. = scale_data)
  
  # Check if num_pc exceeds available PCs
  if (num_pc > ncol(pca$x)) {
    stop("The specified number of principal components exceeds the available number.")
  }
  
  # Get the proportion of variance explained by each principal component
  var_explained <- round(summary(pca)$importance[2, 1:num_pc], 2)
  
  # Get the PC loadings
  pc_loadings <- round(pca$rotation[,1:num_pc], 2)
  colnames(pc_loadings) <- paste0("PC", 1:num_pc)
  
  # Get the PC scores
  pc_scores <- round(pca$x[,1:num_pc], 2)
  colnames(pc_scores) <- paste0("PC", 1:num_pc)
  
  # Create a data frame with the PC scores and the labels
  pca_df <- bind_cols(as.data.frame(pc_scores), select(data, all_of(label_cols)))
  
  # Return the PCA results
  return(list(pc_loadings = pc_loadings,
              pc_scores = pc_scores,
              var_explained = var_explained,
              pca_df = pca_df))
}

#' Plot PCA Results
#'
#' This function takes the result of a PCA analysis and plots the scores of the first two principal components,
#' colored by a specified label.
#'
#' @param pca_result A list containing the results of a PCA analysis. Expected to be the output of the fs_pca function.
#' @param pca_df A data frame containing principal component scores and labels. This is expected to be the pca_df element from the pca_result list.
#' @param label_col A character string specifying the name of the column in pca_df to use for coloring the points.
#'
#' @return This function does not return anything. It generates a plot of the PCA results.
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_color_brewer ggtitle xlab ylab
#'
#' @examples
#' \dontrun{
#' data(iris)
#' pca_result <- fs_pca(iris)
#' plot_pca(pca_result, pca_result$pca_df, "Species")
#' }
#' @export
plot_pca <- function(pca_result, label_col) {
  
  var_explained <- pca_result$var_explained
  pca_df <- pca_result$pca_df
  
  # Ensure the palette can accommodate the number of unique labels
  num_colors <- length(unique(pca_df[[label_col]]))
  if (num_colors > 8) { # Set1 has 8 distinct colors
    stop("The number of unique labels exceeds the number of available colors in the 'Set1' palette.")
  }
  
  # Plot the PC scores, colored by the labels using aes_string
  pca_plot <- ggplot(pca_df, aes_string(x = "PC1", y = "PC2", color = label_col)) + 
    geom_point() +
    scale_color_brewer(palette = "Set1") +
    ggtitle("PCA Plot") +
    xlab(paste0("PC1 (", var_explained[1]*100, "%)")) +
    ylab(paste0("PC2 (", var_explained[2]*100, "%)"))
  
  # Display the plot
  print(pca_plot)
}