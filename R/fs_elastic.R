#' Load Necessary Libraries
#'
#' Loads required libraries for model training, parallel processing,
#' and PCA on sparse matrices.
#'
#' @import caret glmnet Matrix doParallel foreach irlba methods
#' @noRd
suppressPackageStartupMessages({
  library(caret)
  library(glmnet)
  library(Matrix)
  library(doParallel)
  library(foreach)
  library(irlba)  # Efficient PCA/SVD for large/sparse matrices
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
  if (isTRUE(verbose)) cat(msg, "\n")
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
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  on.exit({
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  }, add = TRUE)
  eval.parent(substitute(expr))
}

##########################################
# Custom Summary Function for caret
##########################################
#' Safe Summary Function for caret
#'
#' A custom summary function for \code{caret} that replaces NA values
#' with metric-appropriate infinities so resamples don't crash selection.
#'
#' @param data A data frame of observed and predicted values.
#' @param lev Optional factor levels.
#' @param model Optional model name.
#' @return A named vector of performance metrics.
safe_summary <- function(data, lev = NULL, model = NULL) {
  out <- caret::defaultSummary(data, lev, model)
  if ("RMSE" %in% names(out))     out[["RMSE"]][is.na(out[["RMSE"]])]         <- Inf
  if ("MAE" %in% names(out))      out[["MAE"]][is.na(out[["MAE"]])]           <- Inf
  if ("Rsquared" %in% names(out)) out[["Rsquared"]][is.na(out[["Rsquared"]])] <- -Inf
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
  model_data <- model.frame(formula, data, na.action = na.pass)
  y <- model.response(model_data)
  # Remove intercept column (if present) from the design matrix
  x <- model.matrix(formula, model_data)[, -1, drop = FALSE]
  list(y = y, x = x)
}

##########################################
# Handle Missing Values (Sparse-Safe)
##########################################
#' Handle Missing Values
#'
#' Removes rows with missing values from predictors and response.
#' Works efficiently for sparse matrices without unintended densification.
#'
#' @param x A matrix or sparse matrix of predictors.
#' @param y A numeric vector of responses.
#' @return A list with cleaned \code{x} and \code{y}.
handle_missing_values <- function(x, y) {
  ny <- is.na(y)
  
  if (inherits(x, "sparseMatrix")) {
    has_na_vals <- length(x@x) > 0 && anyNA(x@x)
    if (has_na_vals) {
      rows_with_na <- unique(x@i[is.na(x@x)]) + 1L
      keep <- !(seq_len(nrow(x)) %in% rows_with_na) & !ny
    } else {
      keep <- !ny
    }
  } else {
    keep <- !ny & rowSums(is.na(x)) == 0
  }
  
  list(x = x[keep, , drop = FALSE], y = y[keep])
}

##########################################
# Perform Principal Component Analysis (PCA)
##########################################
#' Perform PCA on Predictors
#'
#' Performs PCA using an efficient truncated SVD algorithm. Requires \code{nPCs}.
#' Avoids densifying large sparse matrices.
#'
#' @param x A (possibly sparse) predictor matrix.
#' @param use_pca Logical. Whether to perform PCA.
#' @param nPCs Integer. Number of principal components to retain.
#' @return Transformed predictor matrix (scores).
perform_pca <- function(x, use_pca = FALSE, nPCs = NULL) {
  if (!use_pca) return(x)
  if (is.null(nPCs) || nPCs < 1L) stop("Please set a positive nPCs when use_pca = TRUE.")
  
  # Ensure sparse input remains sparse; always prefer truncated SVD
  if (!inherits(x, "sparseMatrix")) {
    # Convert dense to sparse for consistency if large
    if (prod(dim(x)) > 5e5) x <- Matrix::Matrix(x, sparse = TRUE)
  } else {
    x <- methods::as(x, "dgCMatrix")
  }
  
  pca <- irlba::prcomp_irlba(x, n = nPCs, scale. = TRUE)
  pca$x
}

##########################################
# Train Elastic Net Model via caret
##########################################
#' Train Elastic Net Models
#'
#' Trains elastic net models using \code{caret::train} with parallel processing.
#'
#' @param x A predictor matrix (dense or sparse).
#' @param y A numeric response vector.
#' @param tuneGrid Data frame with hyperparameter grid (\code{alpha} and \code{lambda}).
#' @param trControl A \code{trainControl} object for resampling.
#' @param cores Integer. Number of cores to use for parallel processing.
#' @return A \code{train} object.
train_models <- function(x, y, tuneGrid, trControl, cores = max(1, parallel::detectCores() - 1)) {
  with_parallel({
    caret::train(
      x = x,
      y = y,
      method = "glmnet",
      tuneGrid = tuneGrid,
      trControl = trControl,
      metric = "RMSE"
    )
  }, cores = cores)
}

##########################################
# Select the Best Model Based on RMSE
##########################################
#' Select the Best Model
#'
#' Extracts the best model and its parameters from a caret \code{train} object.
#'
#' @param fit A \code{train} object as returned by \code{caret::train}.
#' @return A list with the best model and its parameters.
select_best_model <- function(fit) {
  list(
    model  = fit$finalModel,
    alpha  = fit$bestTune$alpha,
    lambda = fit$bestTune$lambda,
    RMSE   = min(fit$results$RMSE, na.rm = TRUE)
  )
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
#' @param alpha_seq Numeric vector of alpha values for tuning. Default \code{seq(0, 1, by = 0.1)}.
#' @param lambda_seq Numeric vector of lambda values for tuning. Default \code{10^seq(-3, 3, length = 100)}.
#' @param trControl A \code{trainControl} object. Default = 5-fold CV using \code{safe_summary()}.
#' @param use_pca Logical. Whether to perform PCA.
#' @param nPCs Integer. Number of PCs to retain when \code{use_pca = TRUE}.
#' @param cores Integer. Number of cores for parallel processing.
#' @param verbose Logical. If \code{TRUE}, prints progress messages.
#' @return A list containing model coefficients, best hyperparameters, RMSE, and the full \code{train} object.
fs_elastic <- function(data,
                       formula,
                       alpha_seq  = seq(0, 1, by = 0.1),
                       lambda_seq = 10^seq(-3, 3, length = 100),
                       trControl  = caret::trainControl(
                         method = "cv",
                         number = 5,
                         summaryFunction = safe_summary
                       ),
                       use_pca = FALSE,
                       nPCs    = NULL,
                       cores   = max(1, parallel::detectCores() - 1),
                       verbose = TRUE) {
  
  verbose_message("Extracting response and predictor variables...", verbose)
  vars <- extract_variables(data, formula)
  y <- vars$y
  x <- vars$x
  
  verbose_message("Ensuring predictors are in sparse format (if large)...", verbose)
  if (!inherits(x, "sparseMatrix")) {
    # Promote to sparse if moderately large to improve memory behavior
    if (prod(dim(x)) > 5e5) x <- Matrix::Matrix(x, sparse = TRUE)
  }
  if (inherits(x, "sparseMatrix")) x <- methods::as(x, "dgCMatrix")
  
  verbose_message("Handling missing values...", verbose)
  cleaned <- handle_missing_values(x, y)
  x <- cleaned$x
  y <- cleaned$y
  
  verbose_message("Performing PCA if specified...", verbose)
  x <- perform_pca(x, use_pca = use_pca, nPCs = nPCs)
  
  verbose_message("Creating tuning grid...", verbose)
  tuneGrid <- expand.grid(alpha = alpha_seq, lambda = lambda_seq)
  
  verbose_message("Training models (using parallel processing)...", verbose)
  fit <- train_models(x, y, tuneGrid = tuneGrid, trControl = trControl, cores = cores)
  
  verbose_message("Selecting best model...", verbose)
  best <- select_best_model(fit)
  
  verbose_message("Extracting coefficients at best lambda...", verbose)
  coefficients <- coef(best$model, s = best$lambda)
  
  list(
    coef        = coefficients,          # dgCMatrix from glmnet::coef
    best_alpha  = best$alpha,
    best_lambda = best$lambda,
    RMSE        = best$RMSE,
    full_model  = fit                    # caret::train object
  )
}
