#' Load Necessary Libraries
#'
#' Loads required libraries for model training, parallel processing,
#' and PCA on sparse matrices.
#'
#' @import caret glmnet Matrix doParallel foreach irlba methods parallel
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
  if (isTRUE(verbose)) message(msg)
}

##########################################
# Helper: Safe Core Detection
##########################################
#' Safely Detect Number of Cores
#'
#' Returns a conservative estimate of cores for parallel processing.
#' If detection fails, falls back to a default.
#'
#' @param default Integer. Fallback number of cores (default: 1).
#' @return Integer. Number of cores to use.
detect_cores_safe <- function(default = 1L) {
  cores <- parallel::detectCores()
  if (is.na(cores) || cores <= 1L) {
    default
  } else {
    cores - 1L
  }
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
with_parallel <- function(expr, cores = detect_cores_safe()) {
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
#' with metric-appropriate infinities so resamples do not crash selection.
#' Works for both regression and classification metrics produced by
#' \code{caret::defaultSummary()}.
#'
#' @param data A data frame of observed and predicted values.
#' @param lev Optional factor levels.
#' @param model Optional model name.
#' @return A named vector of performance metrics.
safe_summary <- function(data, lev = NULL, model = NULL) {
  out <- caret::defaultSummary(data, lev, model)
  
  # Metrics to minimize
  for (nm in intersect(c("RMSE", "MAE", "logLoss"), names(out))) {
    out[[nm]][is.na(out[[nm]])] <- Inf
  }
  
  # Metrics to maximize
  for (nm in intersect(c("Rsquared", "Accuracy", "Kappa"), names(out))) {
    out[[nm]][is.na(out[[nm]])] <- -Inf
  }
  
  out
}

##########################################
# Extract Response and Predictor Variables
##########################################
#' Extract Response and Predictor Variables
#'
#' Extracts the response and predictor variables from a data frame using a formula.
#' Safely removes intercept columns (if present) based on column name
#' instead of position.
#'
#' @param data A data frame.
#' @param formula A formula specifying the model.
#' @return A list with elements \code{y} (response) and \code{x} (predictors).
extract_variables <- function(data, formula) {
  model_data <- model.frame(formula, data, na.action = na.pass)
  y <- model.response(model_data)
  mm <- model.matrix(formula, model_data)
  
  # Remove intercept column (if present) by name, not by position
  intercept_col <- which(colnames(mm) == "(Intercept)")
  if (length(intercept_col) > 0L) {
    mm <- mm[, -intercept_col, drop = FALSE]
  }
  
  list(y = y, x = mm)
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
#' @param y A response vector (numeric, factor, or character).
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
  
  if (!any(keep)) {
    stop("All rows were removed due to missing values in predictors and/or response.")
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
#' @return A list with elements \code{x} (PC scores) and \code{pca} (PCA model or \code{NULL}).
perform_pca <- function(x, use_pca = FALSE, nPCs = NULL) {
  if (!use_pca) {
    return(list(x = x, pca = NULL))
  }
  
  if (is.null(nPCs) || nPCs < 1L) {
    stop("Please set a positive nPCs when use_pca = TRUE.")
  }
  
  # Ensure sparse input remains sparse; always prefer truncated SVD
  if (!inherits(x, "sparseMatrix")) {
    # Convert dense to sparse for consistency if large
    if (prod(dim(x)) > 5e5) {
      x <- Matrix::Matrix(x, sparse = TRUE)
    }
  } else {
    x <- methods::as(x, "dgCMatrix")
  }
  
  if (nPCs > min(dim(x))) {
    stop("nPCs must be <= min(nrow(x), ncol(x)).")
  }
  
  pca <- irlba::prcomp_irlba(x, n = nPCs, scale. = TRUE)
  
  list(x = pca$x, pca = pca)
}

##########################################
# Determine Task Type and Coerce Response
##########################################
#' Infer Task Type and Coerce Response
#'
#' Determines whether the task is regression or classification based on
#' the response vector and returns a coerced response, task label,
#' and a default metric name.
#'
#' @param y A response vector.
#' @return A list with elements \code{y}, \code{task}, and \code{metric}.
infer_task_and_coerce_response <- function(y) {
  # Regression: numeric
  if (is.numeric(y)) {
    return(list(y = y, task = "regression", metric = "RMSE"))
  }
  
  # Classification: factor or character
  if (is.factor(y)) {
    y <- droplevels(y)
    if (nlevels(y) < 2L) {
      stop("Classification outcome must have at least 2 levels.")
    }
    return(list(y = y, task = "classification", metric = "Accuracy"))
  }
  
  if (is.character(y)) {
    y_factor <- factor(y)
    if (nlevels(y_factor) < 2L) {
      stop("Classification outcome must have at least 2 levels.")
    }
    return(list(y = y_factor, task = "classification", metric = "Accuracy"))
  }
  
  stop("Unsupported response type: response must be numeric, factor, or character.")
}

##########################################
# Train Elastic Net Model via caret
##########################################
#' Train Elastic Net Models
#'
#' Trains elastic net models using \code{caret::train} with parallel processing.
#' Supports both regression and classification, depending on the type of \code{y}.
#'
#' @param x A predictor matrix (dense or sparse).
#' @param y A response vector (numeric or factor).
#' @param tuneGrid Data frame with hyperparameter grid (\code{alpha} and \code{lambda}).
#' @param trControl A \code{trainControl} object for resampling.
#' @param metric Character. Name of the performance metric used by \code{caret::train}.
#' @param cores Integer. Number of cores to use for parallel processing.
#' @return A \code{train} object.
train_models <- function(x,
                         y,
                         tuneGrid,
                         trControl,
                         metric,
                         cores = detect_cores_safe()) {
  with_parallel({
    caret::train(
      x         = x,
      y         = y,
      method    = "glmnet",
      tuneGrid  = tuneGrid,
      trControl = trControl,
      metric    = metric
    )
  }, cores = cores)
}

##########################################
# Select the Best Model Based on Metric
##########################################
#' Select the Best Model
#'
#' Extracts the best model and its parameters from a caret \code{train} object.
#' Works for any metric present in \code{fit$results}.
#'
#' @param fit A \code{train} object as returned by \code{caret::train}.
#' @param metric Character. Name of the performance metric used for model selection.
#' @return A list with the best model, its parameters, and metric value.
select_best_model <- function(fit, metric) {
  bt <- fit$bestTune
  res <- fit$results
  
  # Identify row corresponding to bestTune
  idx <- rep(TRUE, nrow(res))
  for (nm in names(bt)) {
    if (nm %in% names(res)) {
      idx <- idx & res[[nm]] == bt[[nm]]
    }
  }
  if (!any(idx)) {
    metric_value <- NA_real_
  } else if (metric %in% names(res)) {
    metric_value <- res[[metric]][idx][1]
  } else {
    metric_value <- NA_real_
  }
  
  list(
    model        = fit$finalModel,
    alpha        = bt$alpha,
    lambda       = bt$lambda,
    metric_name  = metric,
    metric_value = metric_value
  )
}

##########################################
# Elastic Net Feature Selection and Training
##########################################
#' Elastic Net Feature Selection and Model Training
#'
#' Performs feature selection and model training using elastic net regularization.
#' Supports both regression (numeric outcomes) and classification (factor/character outcomes).
#'
#' @param data A data frame containing predictors and response.
#' @param formula A formula specifying the model.
#' @param alpha_seq Numeric vector of alpha values for tuning. Default \code{seq(0, 1, by = 0.1)}.
#' @param lambda_seq Numeric vector of lambda values for tuning. Default \code{10^seq(-3, 3, length = 100)}.
#' @param trControl Optional \code{trainControl} object. If \code{NULL} (default),
#'   a sensible default is used (5-fold CV with \code{safe_summary()}).
#' @param metric Optional character. Name of the performance metric to optimize.
#'   If \code{NULL} (default), \code{"RMSE"} is used for regression and
#'   \code{"Accuracy"} for classification.
#' @param use_pca Logical. Whether to perform PCA.
#' @param nPCs Integer. Number of PCs to retain when \code{use_pca = TRUE}.
#' @param cores Integer. Number of cores for parallel processing.
#' @param verbose Logical. If \code{TRUE}, prints progress messages.
#' @return A list containing:
#'   \item{coef}{Model coefficients at the best \code{lambda} (matrix or list, depending on glmnet family).}
#'   \item{best_alpha}{Best alpha value.}
#'   \item{best_lambda}{Best lambda value.}
#'   \item{metric_name}{Name of the performance metric used.}
#'   \item{metric_value}{Value of the performance metric at the best hyperparameters.}
#'   \item{task}{Character: \code{"regression"} or \code{"classification"}.}
#'   \item{full_model}{The \code{caret::train} object.}
#'   \item{pca_model}{The PCA model (if \code{use_pca = TRUE}), else \code{NULL}.}
#'   \item{use_pca}{Logical, whether PCA was used.}
#'   \item{formula}{The model formula.}
fs_elastic <- function(data,
                       formula,
                       alpha_seq  = seq(0, 1, by = 0.1),
                       lambda_seq = 10^seq(-3, 3, length = 100),
                       trControl  = NULL,
                       metric     = NULL,
                       use_pca    = FALSE,
                       nPCs       = NULL,
                       cores      = detect_cores_safe(),
                       verbose    = TRUE) {
  
  verbose_message("Extracting response and predictor variables...", verbose)
  vars <- extract_variables(data, formula)
  y <- vars$y
  x <- vars$x
  
  verbose_message("Inferring task type (regression vs classification)...", verbose)
  task_info <- infer_task_and_coerce_response(y)
  y     <- task_info$y
  task  <- task_info$task
  default_metric <- task_info$metric
  
  if (is.null(metric)) {
    metric <- default_metric
  }
  
  verbose_message("Ensuring predictors are in sparse format (if large)...", verbose)
  if (!inherits(x, "sparseMatrix")) {
    # Promote to sparse if moderately large to improve memory behavior
    if (prod(dim(x)) > 5e5) {
      x <- Matrix::Matrix(x, sparse = TRUE)
    }
  }
  if (inherits(x, "sparseMatrix")) {
    x <- methods::as(x, "dgCMatrix")
  }
  
  verbose_message("Handling missing values...", verbose)
  cleaned <- handle_missing_values(x, y)
  x <- cleaned$x
  y <- cleaned$y
  
  verbose_message("Performing PCA if specified...", verbose)
  pca_res <- perform_pca(x, use_pca = use_pca, nPCs = nPCs)
  x <- pca_res$x
  pca_model <- pca_res$pca
  
  verbose_message("Creating tuning grid...", verbose)
  tuneGrid <- expand.grid(alpha = alpha_seq, lambda = lambda_seq)
  
  if (is.null(trControl)) {
    verbose_message("Creating default trainControl...", verbose)
    trControl <- caret::trainControl(
      method          = "cv",
      number          = 5,
      summaryFunction = safe_summary
    )
  }
  
  verbose_message("Training models (using parallel processing)...", verbose)
  fit <- train_models(
    x         = x,
    y         = y,
    tuneGrid  = tuneGrid,
    trControl = trControl,
    metric    = metric,
    cores     = cores
  )
  
  verbose_message("Selecting best model...", verbose)
  best <- select_best_model(fit, metric = metric)
  
  verbose_message("Extracting coefficients at best lambda...", verbose)
  coefficients <- coef(best$model, s = best$lambda)
  
  list(
    coef         = coefficients,      # dgCMatrix or list (for multinomial)
    best_alpha   = best$alpha,
    best_lambda  = best$lambda,
    metric_name  = best$metric_name,
    metric_value = best$metric_value,
    task         = task,
    full_model   = fit,              # caret::train object
    pca_model    = pca_model,        # PCA model (if use_pca = TRUE), else NULL
    use_pca      = use_pca,
    formula      = formula
  )
}
