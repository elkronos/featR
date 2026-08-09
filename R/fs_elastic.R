# Elastic net feature selection for featR.
# Suggests: caret, glmnet, Matrix (always); irlba (only when use_pca = TRUE);
# foreach + doParallel (only when cores > 1).

#' Print a progress message when verbose
#' @noRd
elastic_message <- function(msg, verbose) {
  if (isTRUE(verbose)) message(msg)
}

#' Safe summary function for caret
#'
#' Wraps `caret::defaultSummary()` and replaces NA metric values with
#' metric-appropriate infinities so resamples do not crash model selection.
#'
#' @param data A data frame of observed and predicted values.
#' @param lev Optional factor levels.
#' @param model Optional model name.
#' @return A named vector of performance metrics.
#' @noRd
elastic_safe_summary <- function(data, lev = NULL, model = NULL) {
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

#' Extract response and predictor variables from a formula
#'
#' Removes the intercept column (if present) by name, not by position.
#'
#' @param data A data frame.
#' @param formula A model formula.
#' @return A list with elements `y` (response) and `x` (predictor matrix).
#' @noRd
elastic_extract_variables <- function(data, formula) {
  model_data <- stats::model.frame(formula, data, na.action = stats::na.pass)
  y <- stats::model.response(model_data)
  mm <- stats::model.matrix(formula, model_data)

  intercept_col <- which(colnames(mm) == "(Intercept)")
  if (length(intercept_col) > 0L) {
    mm <- mm[, -intercept_col, drop = FALSE]
  }

  list(y = y, x = mm)
}

#' Drop rows with missing values (sparse-safe)
#'
#' Uses `@i`/`@x` internals only for `dgCMatrix` input; other sparse classes
#' go through a generic `is.na()` path and dense matrices through `rowSums()`.
#'
#' @param x A matrix or sparse matrix of predictors.
#' @param y A response vector.
#' @return A list with cleaned `x` and `y`.
#' @noRd
elastic_drop_missing <- function(x, y) {
  ny <- is.na(y)

  if (inherits(x, "dgCMatrix")) {
    has_na_vals <- length(x@x) > 0L && anyNA(x@x)
    if (has_na_vals) {
      rows_with_na <- unique(x@i[is.na(x@x)]) + 1L
      keep <- !(seq_len(nrow(x)) %in% rows_with_na) & !ny
    } else {
      keep <- !ny
    }
  } else if (inherits(x, "sparseMatrix")) {
    keep <- as.vector(Matrix::rowSums(is.na(x))) == 0 & !ny
  } else {
    keep <- !ny & rowSums(is.na(x)) == 0
  }

  if (!any(keep)) {
    stop("All rows were removed due to missing values in predictors and/or response.")
  }

  list(x = x[keep, , drop = FALSE], y = y[keep])
}

#' Names of zero-variance (constant) columns
#'
#' Computed without densifying sparse input.
#'
#' @param x A dense or sparse numeric matrix.
#' @return Character vector of offending column names (possibly empty).
#' @noRd
elastic_zero_sd_cols <- function(x) {
  if (nrow(x) < 2L) {
    return(character(0L))
  }
  if (inherits(x, "sparseMatrix")) {
    n <- nrow(x)
    mu <- as.vector(Matrix::colMeans(x))
    mu2 <- as.vector(Matrix::colMeans(x * x))
    v <- (mu2 - mu * mu) * n / (n - 1)
    zero <- !is.na(v) & v <= sqrt(.Machine$double.eps) * pmax(mu2, 1)
  } else {
    sds <- apply(x, 2L, stats::sd)
    zero <- !is.na(sds) & sds == 0
  }
  nms <- colnames(x)
  if (is.null(nms)) {
    nms <- paste0("V", seq_len(ncol(x)))
  }
  nms[zero]
}

#' Stop when constant columns would break scaling or model selection
#' @noRd
elastic_check_variance <- function(x, context) {
  zv <- elastic_zero_sd_cols(x)
  if (length(zv) > 0L) {
    shown <- utils::head(zv, 5L)
    extra <- length(zv) - length(shown)
    stop(sprintf(
      "Zero-variance predictor column%s detected before %s: %s%s. Remove constant columns and retry.",
      if (length(zv) > 1L) "s" else "",
      context,
      paste0("'", shown, "'", collapse = ", "),
      if (extra > 0L) sprintf(" (and %d more)", extra) else ""
    ))
  }
  invisible(NULL)
}

#' Perform PCA on predictors via truncated SVD
#'
#' `irlba::prcomp_irlba()` requires `nPCs` strictly less than `min(dim(x))`.
#'
#' @param x A (possibly sparse) predictor matrix.
#' @param use_pca Logical. Whether to perform PCA.
#' @param nPCs Integer. Number of principal components to retain.
#' @return A list with elements `x` (PC scores) and `pca` (model or `NULL`).
#' @noRd
elastic_pca <- function(x, use_pca = FALSE, nPCs = NULL) {
  if (!use_pca) {
    return(list(x = x, pca = NULL))
  }

  if (is.null(nPCs)) {
    stop("Please set a positive 'nPCs' when 'use_pca' is TRUE.")
  }
  nPCs <- assert_count(nPCs, "nPCs", lower = 1L)

  # Keep sparse input sparse; promote large dense input for memory behavior
  if (!inherits(x, "sparseMatrix") && prod(dim(x)) > 5e5) {
    x <- Matrix::Matrix(x, sparse = TRUE)
  }

  if (nPCs >= min(dim(x))) {
    stop("'nPCs' must be strictly less than min(nrow(x), ncol(x)) for irlba::prcomp_irlba().")
  }

  pca <- irlba::prcomp_irlba(x, n = nPCs, scale. = TRUE)

  list(x = pca$x, pca = pca)
}

#' Infer task type and coerce the response
#'
#' Numeric responses are treated as regression (with a warning when they have
#' exactly two unique values); logical responses are converted to factors with
#' a message; factors and characters are classification.
#'
#' @param y A response vector.
#' @return A list with elements `y`, `task`, and `metric`.
#' @noRd
elastic_infer_task <- function(y) {
  if (is.logical(y)) {
    message("Logical response converted to a two-level factor for classification.")
    y <- factor(y)
  }

  if (is.numeric(y)) {
    if (length(unique(y[!is.na(y)])) == 2L) {
      warning("Numeric response has exactly 2 unique values; treated as regression; convert to factor for classification.")
    }
    return(list(y = y, task = "regression", metric = "RMSE"))
  }

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

  stop("Unsupported response type: response must be numeric, logical, factor, or character.")
}

#' Train elastic net models via caret
#'
#' Sequential when `cores == 1` (no cluster is ever created); otherwise a
#' cluster is registered and guaranteed to stop on exit, even on error.
#'
#' @param x A predictor matrix (dense or sparse).
#' @param y A response vector (numeric or factor).
#' @param tuneGrid Data frame with `alpha` and `lambda` combinations.
#' @param trControl A `caret::trainControl()` object.
#' @param metric Character. Performance metric used by `caret::train()`.
#' @param cores Integer >= 1, already resolved via `resolve_cores()`.
#' @return A `caret::train` object.
#' @noRd
elastic_train_models <- function(x, y, tuneGrid, trControl, metric, cores = 1L) {
  if (cores > 1L) {
    cl <- parallel::makeCluster(cores)
    on.exit({
      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
    }, add = TRUE)
    doParallel::registerDoParallel(cl)
  }

  caret::train(
    x         = x,
    y         = y,
    method    = "glmnet",
    tuneGrid  = tuneGrid,
    trControl = trControl,
    metric    = metric
  )
}

#' Select the best model from a caret fit
#'
#' @param fit A `caret::train` object.
#' @param metric Character. Metric used for model selection.
#' @return A list with the best model, its parameters, and metric value.
#' @noRd
elastic_select_best <- function(fit, metric) {
  bt <- fit$bestTune
  res <- fit$results

  idx <- rep(TRUE, nrow(res))
  for (nm in names(bt)) {
    if (nm %in% names(res)) {
      idx <- idx & res[[nm]] == bt[[nm]]
    }
  }
  if (!any(idx)) {
    metric_value <- NA_real_
  } else if (metric %in% names(res)) {
    metric_value <- res[[metric]][idx][1L]
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

#' Elastic Net Feature Selection and Model Training
#'
#' Performs feature selection and model training using elastic net
#' regularization via `caret::train(method = "glmnet")`. Supports regression
#' (numeric outcomes) and classification (factor/character outcomes).
#'
#' @details
#' When `use_pca = TRUE`, the PCA is fit on the **full** data set before
#' cross-validation, so the CV metrics are optimistic (the component loadings
#' have seen the held-out folds). Proper per-fold PCA is deferred; if you need
#' it today, pass a custom `trControl` and use `preProcess = "pca"` in
#' `caret::train()` instead of `use_pca`.
#'
#' @param data A data frame containing predictors and response.
#' @param formula A formula specifying the model.
#' @param alpha_seq Numeric vector of alpha values to tune over.
#'   Default `seq(0, 1, by = 0.1)`.
#' @param lambda_seq Numeric vector of lambda values to tune over.
#'   Default `10^seq(-3, 3, length.out = 100)`.
#' @param trControl Optional `caret::trainControl()` object. If `NULL`
#'   (default), 5-fold CV with an NA-safe summary function is used.
#' @param metric Optional character. Performance metric to optimize. If `NULL`
#'   (default), `"RMSE"` is used for regression and `"Accuracy"` for
#'   classification.
#' @param use_pca Logical. Whether to project predictors onto principal
#'   components before training. Default `FALSE`. See Details for the
#'   cross-validation caveat.
#' @param nPCs Integer. Number of principal components to retain when
#'   `use_pca = TRUE`. Must be strictly less than `min(nrow, ncol)` of the
#'   predictor matrix.
#' @param cores Integer >= 1. Number of workers for parallel training.
#'   Default `1` (sequential; no cluster is created). Values above the
#'   detected core count are capped.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#' @param seed Optional integer seed applied locally (and restored on exit)
#'   before resampling and tuning. Default `NULL` (never seeds by default).
#' @return A list containing:
#'   \item{coef}{Coefficients at the best `lambda` (matrix, or list for multinomial models).}
#'   \item{best_alpha}{Best alpha value.}
#'   \item{best_lambda}{Best lambda value.}
#'   \item{metric_name}{Name of the performance metric used.}
#'   \item{metric_value}{Metric value at the best hyperparameters.}
#'   \item{task}{Character: `"regression"` or `"classification"`.}
#'   \item{full_model}{The `caret::train` object.}
#'   \item{pca_model}{The PCA model when `use_pca = TRUE`, else `NULL`.}
#'   \item{use_pca}{Logical, whether PCA was used.}
#'   \item{formula}{The model formula.}
#' @examples
#' \donttest{
#' if (requireNamespace("caret", quietly = TRUE) &&
#'     requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("Matrix", quietly = TRUE)) {
#'   df <- data.frame(
#'     y  = rnorm(60),
#'     x1 = rnorm(60),
#'     x2 = rnorm(60),
#'     x3 = rnorm(60)
#'   )
#'   fit <- fs_elastic(
#'     df, y ~ .,
#'     lambda_seq = 10^seq(-2, 1, length.out = 10),
#'     verbose = FALSE, seed = 1
#'   )
#'   fit$best_alpha
#' }
#' }
#' @export
fs_elastic <- function(data,
                       formula,
                       alpha_seq  = seq(0, 1, by = 0.1),
                       lambda_seq = 10^seq(-3, 3, length.out = 100),
                       trControl  = NULL,
                       metric     = NULL,
                       use_pca    = FALSE,
                       nPCs       = NULL,
                       cores      = 1L,
                       verbose    = TRUE,
                       seed       = NULL) {
  assert_data_frame(data)
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula.", call. = FALSE)
  }
  if (!is.numeric(alpha_seq) || length(alpha_seq) == 0L || anyNA(alpha_seq) ||
      any(alpha_seq < 0) || any(alpha_seq > 1)) {
    stop("'alpha_seq' must be a numeric vector with values in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(lambda_seq) || length(lambda_seq) == 0L || anyNA(lambda_seq) ||
      any(lambda_seq < 0)) {
    stop("'lambda_seq' must be a numeric vector of non-negative values.", call. = FALSE)
  }
  if (!is.null(metric)) {
    assert_string(metric, "metric")
  }
  assert_flag(use_pca, "use_pca")
  assert_flag(verbose, "verbose")
  if (!is.null(seed)) {
    assert_number(seed, "seed")
    if (seed != round(seed) || abs(seed) > .Machine$integer.max) {
      stop("'seed' must be a single whole number (integer-sized) or NULL.", call. = FALSE)
    }
  }
  cores <- resolve_cores(cores, "cores")

  fs_require(c("caret", "glmnet", "Matrix"), "elastic net feature selection")
  if (use_pca) {
    fs_require("irlba", "PCA via truncated SVD")
  }
  if (cores > 1L) {
    fs_require(c("foreach", "doParallel"), "parallel model training")
  }

  elastic_message("Extracting response and predictor variables...", verbose)
  vars <- elastic_extract_variables(data, formula)
  y <- vars$y
  x <- vars$x

  elastic_message("Inferring task type (regression vs classification)...", verbose)
  task_info <- elastic_infer_task(y)
  y <- task_info$y
  task <- task_info$task
  if (is.null(metric)) {
    metric <- task_info$metric
  }

  elastic_message("Ensuring predictors are in sparse format (if large)...", verbose)
  if (!inherits(x, "sparseMatrix") && prod(dim(x)) > 5e5) {
    x <- Matrix::Matrix(x, sparse = TRUE)
  }

  elastic_message("Handling missing values...", verbose)
  cleaned <- elastic_drop_missing(x, y)
  x <- cleaned$x
  y <- cleaned$y

  elastic_check_variance(x, if (use_pca) "PCA with scaling" else "model training")

  elastic_message("Performing PCA if specified...", verbose)
  pca_res <- elastic_pca(x, use_pca = use_pca, nPCs = nPCs)
  x <- pca_res$x
  pca_model <- pca_res$pca

  elastic_message("Creating tuning grid...", verbose)
  tuneGrid <- expand.grid(alpha = alpha_seq, lambda = lambda_seq)

  local_seed(seed)

  if (is.null(trControl)) {
    elastic_message("Creating default trainControl...", verbose)
    trControl <- caret::trainControl(
      method          = "cv",
      number          = 5,
      summaryFunction = elastic_safe_summary
    )
  }

  elastic_message(
    if (cores > 1L) "Training models (parallel)..." else "Training models...",
    verbose
  )
  fit <- elastic_train_models(
    x         = x,
    y         = y,
    tuneGrid  = tuneGrid,
    trControl = trControl,
    metric    = metric,
    cores     = cores
  )

  elastic_message("Selecting best model...", verbose)
  best <- elastic_select_best(fit, metric = metric)

  elastic_message("Extracting coefficients at best lambda...", verbose)
  coefficients <- stats::coef(best$model, s = best$lambda)

  list(
    coef         = coefficients,
    best_alpha   = best$alpha,
    best_lambda  = best$lambda,
    metric_name  = best$metric_name,
    metric_value = best$metric_value,
    task         = task,
    full_model   = fit,
    pca_model    = pca_model,
    use_pca      = use_pca,
    formula      = formula
  )
}
