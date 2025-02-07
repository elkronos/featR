#' Load Necessary Libraries
#'
#' Loads required libraries for model training, parallel processing,
#' and PCA on sparse matrices.
#'
#' @import caret glmnet Matrix doParallel foreach irlba
#' @noRd
suppressPackageStartupMessages({
  library(caret)
  library(glmnet)
  library(Matrix)
  library(doParallel)
  library(foreach)
  library(irlba)  # For efficient PCA on sparse matrices
})

##########################################
# Helper: Verbose Logger
##########################################
#' Print Verbose Message
#'
#' Prints a message if verbose mode is enabled.
#'
#' @param msg Character. The message to print.
#' @param verbose Logical. If \code{TRUE}, the message is printed.
#' @return None.
verbose_message <- function(msg, verbose) {
  if (verbose) cat(msg, "\n")
}

##########################################
# Helper: Parallel Setup
##########################################
#' Execute Expression with Parallel Processing
#'
#' Sets up a parallel backend, executes an expression, and then
#' stops the cluster.
#'
#' @param expr An R expression to evaluate.
#' @param cores Integer. Number of cores to use (default: max available cores minus one).
#' @return The result of the evaluated expression.
with_parallel <- function(expr, cores = max(1, parallel::detectCores() - 1)) {
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  on.exit({
    stopCluster(cl)
    registerDoSEQ()
  })
  force(expr)
}

##########################################
# Custom Summary Function for caret
##########################################
#' Safe Summary Function for caret
#'
#' A custom summary function for \code{caret} that replaces any NA
#' in resampled performance measures with \code{Inf}.
#'
#' @param data A data frame of observed and predicted values.
#' @param lev Optional factor levels.
#' @param model Optional model name.
#' @return A named vector of performance metrics.
safe_summary <- function(data, lev = NULL, model = NULL) {
  out <- defaultSummary(data, lev, model)
  out[is.na(out)] <- Inf  
  out
}

##########################################
# Extract Response and Predictor Variables
##########################################
#' Extract Response and Predictor Variables
#'
#' Extracts the response and predictor variables from a data frame using a formula.
#'
#' @param data A data frame.
#' @param formula A formula specifying the model.
#' @return A list with elements \code{y} (response) and \code{x} (predictors).
extract_variables <- function(data, formula) {
  model_data <- model.frame(formula, data)
  y <- model.response(model_data)
  # Remove intercept column (if present) from the design matrix
  x <- model.matrix(formula, model_data)[, -1, drop = FALSE]
  list(y = y, x = x)
}

##########################################
# Handle Missing Values
##########################################
#' Handle Missing Values
#'
#' Removes rows with missing values from predictors and response.
#'
#' @param x A matrix or sparse matrix of predictors.
#' @param y A numeric vector of responses.
#' @return A list with cleaned \code{x} and \code{y}.
handle_missing_values <- function(x, y) {
  complete_cases <- !is.na(y) & (rowSums(is.na(x)) == 0)
  list(x = x[complete_cases, , drop = FALSE],
       y = y[complete_cases])
}

##########################################
# Perform Principal Component Analysis (PCA)
##########################################
#' Perform PCA on Predictors
#'
#' Optionally performs PCA on predictors using an efficient SVD algorithm.
#' Switches to standard SVD if a large number of components is requested.
#'
#' @param x A sparse matrix of predictors.
#' @param use_pca Logical. Whether to perform PCA.
#' @param nPCs Integer. Number of principal components to retain. If \code{NULL}, all are retained.
#' @return Transformed predictor matrix.
perform_pca <- function(x, use_pca = FALSE, nPCs = NULL) {
  if (use_pca) {
    total_cols <- ncol(x)
    # If nPCs is NULL or requested too high (e.g., >50% of predictors), use standard SVD via prcomp()
    if (is.null(nPCs) || nPCs > total_cols * 0.5) {
      pca <- prcomp(x, scale. = TRUE)
      if (is.null(nPCs)) {
        return(pca$x)
      } else {
        return(pca$x[, 1:nPCs, drop = FALSE])
      }
    } else {
      # Use efficient method for a small number of PCs
      pca <- prcomp_irlba(x, n = nPCs, scale. = TRUE)
      return(pca$x)
    }
  }
  x
}

##########################################
# Train Elastic Net Model via Caret
##########################################
#' Train Elastic Net Models
#'
#' Trains elastic net models using \code{caret::train} with parallel processing.
#'
#' @param x A predictor matrix (can be sparse).
#' @param y A numeric response vector.
#' @param tuneGrid Data frame with hyperparameter grid (\code{alpha} and \code{lambda}).
#' @param trControl A \code{trainControl} object for cross-validation.
#' @param cores Integer. Number of cores to use for parallel processing.
#' @return The training results from \code{caret::train}.
train_models <- function(x, y, tuneGrid, trControl, cores = max(1, parallel::detectCores() - 1)) {
  with_parallel({
    fit <- train(x, y,
                 method = "glmnet",
                 tuneGrid = tuneGrid,
                 trControl = trControl)
    fit
  }, cores = cores)
}

##########################################
# Select the Best Model Based on RMSE
##########################################
#' Select the Best Model
#'
#' Identifies the model with the minimum RMSE from cross-validation results.
#'
#' @param cv_results A data frame containing cross-validation metrics and model objects.
#' @return A list with the best model and its parameters.
select_best_model <- function(cv_results) {
  best_index <- which.min(cv_results$RMSE)
  best_row <- cv_results[best_index, ]
  list(model = best_row$model[[1]],
       alpha = best_row$alpha,
       lambda = best_row$lambda,
       RMSE = best_row$RMSE)
}

##########################################
# Elastic Net Feature Selection and Training
##########################################
#' Elastic Net Feature Selection and Model Training
#'
#' Performs feature selection and model training using elastic net regularization.
#'
#' @param data A data frame containing predictors and response.
#' @param formula A formula specifying the model.
#' @param alpha_seq Numeric vector of alpha values for tuning. Default is \code{seq(0, 1, by = 0.1)}.
#' @param lambda_seq Numeric vector of lambda values for tuning. Default is \code{10^seq(-3, 3, length = 100)}.
#' @param trControl A \code{trainControl} object. Default is 5-fold cross-validation with a safe summary function.
#' @param use_pca Logical. Whether to perform PCA.
#' @param nPCs Integer. Number of principal components to retain if \code{use_pca} is \code{TRUE}.
#' @param cores Integer. Number of cores for parallel processing.
#' @param verbose Logical. If \code{TRUE}, prints progress messages.
#' @return A list containing model coefficients, best hyperparameters, and RMSE.
fs_elastic <- function(data, 
                       formula,
                       alpha_seq = seq(0, 1, by = 0.1), 
                       lambda_seq = 10^seq(-3, 3, length = 100),
                       trControl = trainControl(method = "cv", 
                                                number = 5, 
                                                summaryFunction = safe_summary),
                       use_pca = FALSE,
                       nPCs = NULL,
                       cores = max(1, parallel::detectCores() - 1),
                       verbose = TRUE) {
  verbose_message("Extracting response and predictor variables...", verbose)
  vars <- extract_variables(data, formula)
  y <- vars$y
  x <- vars$x
  
  verbose_message("Converting predictors to sparse matrix...", verbose)
  x <- as(x, "CsparseMatrix")
  
  verbose_message("Handling missing values...", verbose)
  cleaned <- handle_missing_values(x, y)
  x <- cleaned$x
  y <- cleaned$y
  
  verbose_message("Performing PCA if specified...", verbose)
  x <- perform_pca(x, use_pca, nPCs)
  
  # Create tuning grid
  tuneGrid <- expand.grid(alpha = alpha_seq, lambda = lambda_seq)
  
  verbose_message("Training models (using parallel processing)...", verbose)
  fit <- train_models(x, y, tuneGrid, trControl, cores = cores)
  
  # Select best model based on RMSE
  best_model_info <- select_best_model(fit$results)
  verbose_message("Best model selected.", verbose)
  
  # Extract coefficients using the best lambda
  coefficients <- coef(best_model_info$model$finalModel, s = best_model_info$lambda)
  
  list(coef = coefficients,
       best_alpha = best_model_info$alpha,
       best_lambda = best_model_info$lambda,
       RMSE = best_model_info$RMSE,
       full_model = best_model_info$model)
}
