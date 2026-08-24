# Elastic net feature selection for featR.
# Suggests: caret, glmnet, Matrix (always); foreach + doParallel (only when
# n_cores > 1). When use_pca = TRUE the components are fitted by caret's
# preProcess inside every resample, never once on the full data.

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

#' Build the model formula for a data + target pair
#'
#' Non-syntactic column names are backticked so they survive parsing.
#'
#' @param target Name of the outcome column.
#' @param predictors Character vector of predictor column names.
#' @return A formula.
#' @noRd
elastic_formula <- function(target, predictors) {
  stats::as.formula(paste(
    backtick(target), "~", paste(backtick(predictors), collapse = " + ")
  ))
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
#'
#' @param x A dense or sparse numeric matrix of predictors.
#' @param context Short string naming the step the check guards; it is
#'   interpolated into the error message, after "detected before".
#' @return Invisibly `NULL`; errors when any column is constant.
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

#' Validate the component count requested for the PCA pre-processing step
#'
#' The bound is checked against the full predictor matrix; note that caret
#' refits the PCA inside every resample, so `nPCs` must also be smaller than
#' the number of rows each resample trains on.
#'
#' @param x The predictor matrix.
#' @param nPCs Requested number of principal components.
#' @return `nPCs` as an integer.
#' @noRd
elastic_check_npcs <- function(x, nPCs) {
  if (is.null(nPCs)) {
    stop("Please set a positive 'nPCs' when 'use_pca' is TRUE.", call. = FALSE)
  }
  nPCs <- assert_count(nPCs, "nPCs", lower = 1L)
  if (nPCs >= min(dim(x))) {
    stop("'nPCs' must be strictly less than min(nrow(x), ncol(x)).", call. = FALSE)
  }
  nPCs
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

#' Tuning grid built from glmnet's own lambda path, one path per alpha
#'
#' `caret::train()` rejects a glmnet tuning grid that has no `lambda` column,
#' so "let glmnet choose the path" means asking `glmnet::glmnet()` which
#' lambdas it would use at each alpha and tuning over exactly those, instead
#' of an arbitrary fixed sequence. Only the candidate values are taken from
#' the full data (this is what `caret`'s own default grid does); which lambda
#' wins is still decided by resampling.
#'
#' @param x The predictor matrix handed to `caret::train()`.
#' @param y The response vector.
#' @param alpha_seq Numeric vector of alpha values.
#' @param task Either "regression" or "classification".
#' @param nlambda Number of lambda values requested per alpha.
#' @return A data.frame with columns `alpha` and `lambda`.
#' @noRd
elastic_lambda_grid <- function(x, y, alpha_seq, task, nlambda = 50L) {
  family <- if (identical(task, "classification")) {
    if (nlevels(y) > 2L) "multinomial" else "binomial"
  } else {
    "gaussian"
  }

  xm <- if (inherits(x, "sparseMatrix")) x else as.matrix(x)

  grids <- lapply(alpha_seq, function(a) {
    fit <- glmnet::glmnet(xm, y, family = family, alpha = a, nlambda = nlambda)
    data.frame(alpha = a, lambda = unique(as.numeric(fit$lambda)))
  })

  do.call(rbind, grids)
}

#' Train elastic net models via caret
#'
#' Sequential when `n_cores == 1` (no cluster is ever created); otherwise a
#' cluster is registered and guaranteed to stop on exit, even on error.
#'
#' @param x A predictor matrix (dense or sparse).
#' @param y A response vector (numeric or factor).
#' @param tuneGrid Data frame with `alpha` and `lambda` combinations.
#' @param trControl A `caret::trainControl()` object.
#' @param metric Character. Performance metric used by `caret::train()`.
#' @param preProcess Optional character vector of `caret::preProcess()` steps
#'   applied inside every resample; NULL for none.
#' @param n_cores Integer >= 1, already resolved via `resolve_cores()`.
#' @return A `caret::train` object.
#' @noRd
elastic_train_models <- function(x, y, tuneGrid, trControl, metric,
                                 preProcess = NULL, n_cores = 1L) {
  if (n_cores > 1L) {
    cl <- parallel::makeCluster(n_cores)
    on.exit({
      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
    }, add = TRUE)
    doParallel::registerDoParallel(cl)
  }

  caret::train(
    x          = x,
    y          = y,
    method     = "glmnet",
    tuneGrid   = tuneGrid,
    trControl  = trControl,
    metric     = metric,
    preProcess = preProcess
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
      # A tuning column can carry NA (caret records NA for a metric a fold
      # could not compute), and `NA & TRUE` would propagate into the if()
      # below as "missing value where TRUE/FALSE needed".
      matched <- res[[nm]] == bt[[nm]]
      idx <- idx & !is.na(matched) & matched
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

#' Names of predictors with a non-zero coefficient
#'
#' Multinomial fits give one coefficient matrix per class, so the union across
#' classes is taken.
#'
#' @param coefs A coefficient matrix, or a list of them (multinomial).
#' @return Character vector of predictor names, intercept excluded.
#' @noRd
elastic_nonzero_names <- function(coefs) {
  if (is.list(coefs)) {
    return(
      unique(unlist(lapply(coefs, elastic_nonzero_names), use.names = FALSE)) %||%
        character(0L)
    )
  }

  nms <- rownames(coefs)
  if (is.null(nms)) {
    return(character(0L))
  }
  v <- as.vector(coefs)
  keep <- !is.na(v) & v != 0 & nms != "(Intercept)"
  nms[keep]
}

#' Absolute coefficients as a named numeric score vector
#'
#' NULL for multinomial fits, where a predictor has one coefficient per class
#' and there is no single comparable score.
#'
#' @param coefs A coefficient matrix, or a list of them (multinomial).
#' @return A named numeric vector, or NULL.
#' @noRd
elastic_scores <- function(coefs) {
  if (is.list(coefs)) {
    return(NULL)
  }
  nms <- rownames(coefs)
  if (is.null(nms)) {
    return(NULL)
  }
  v <- as.vector(coefs)
  keep <- nms != "(Intercept)"
  stats::setNames(abs(v[keep]), nms[keep])
}

#' Elastic Net Feature Selection and Model Training
#'
#' Performs feature selection and model training using elastic net
#' regularization via `caret::train(method = "glmnet")`. Supports regression
#' (numeric outcomes) and classification (factor/character outcomes).
#'
#' @details
#' Use this when the question is "which predictors survive a jointly tuned
#' L1/L2 penalty?". Both alpha and lambda are chosen by resampling, and every
#' predictor with a non-zero coefficient at the winning pair is reported. When
#' the winning alpha is below 1 the ridge component spreads weight across
#' correlated predictors, so a group of collinear columns tends to survive
#' together instead of being reduced to a single representative.
#'
#' The model formula is built internally from `data` and `target`: every other
#' column of `data` is a candidate predictor, and non-syntactic names are
#' backticked. Predictors then go through `stats::model.matrix()` and the
#' intercept column is removed, so a k-level factor or character column
#' contributes k - 1 dummy columns and `selected`, `scores` and `details$coef`
#' name design-matrix columns rather than the original columns.
#'
#' Rows with a missing response, or a missing value in any predictor, are
#' dropped before fitting (an error if that leaves nothing), and a constant
#' predictor column is a hard error rather than a silently degenerate fit. A
#' logical response is converted to a two-level factor with a message; a
#' numeric response with only two distinct values is still treated as
#' regression, with a warning telling you to convert it to a factor if you
#' meant classification.
#'
#' `scores` are absolute coefficients on the scale of the columns the model
#' saw, not standardized ones, so they rank predictors fairly only when those
#' columns are on comparable scales -- unlike `fs_lasso()`, this function does
#' not rescale them for you.
#'
#' When `use_pca = TRUE` the PCA is **not** fitted up front. `caret` is asked
#' for `preProcess = c("center", "scale", "pca")` with `pcaComp = nPCs` in
#' `trControl$preProcOptions`, so centering, scaling and the component
#' loadings are refit on the training part of every resample and the held-out
#' fold never contributes to them. The model is then fitted on components, so
#' `selected`, `scores` and `details$coef` are named `PC1`, `PC2`, ... rather
#' than after the original columns. `nPCs` must be smaller than the number of
#' rows each resample trains on as well as smaller than the number of
#' predictors.
#'
#' `lambda_seq = NULL` (the default) tunes over the lambda path
#' `glmnet::glmnet()` itself proposes at each alpha (up to 50 values per
#' alpha), which is scaled to the data, instead of a fixed sequence that spends
#' most of its fits on irrelevant lambdas. Only the candidate values come from
#' the full data, exactly as in caret's own default glmnet grid; which pair
#' wins is still decided by resampling. With `use_pca = TRUE` that path is
#' computed on the original predictors, so it is only an approximation of the
#' scale the components live on; pass `lambda_seq` explicitly if you need to
#' control it.
#'
#' @param data A data frame (or data.table) containing the target and the
#'   candidate predictors.
#' @param target Single string naming the outcome column in `data`.
#' @param alpha_seq Numeric vector of alpha values to tune over, each in
#'   `[0, 1]`. Default `seq(0, 1, by = 0.1)`.
#' @param lambda_seq Numeric vector of non-negative lambda values to tune
#'   over, or `NULL` (default) to use glmnet's own path per alpha.
#' @param trControl Optional `caret::trainControl()` object. If `NULL`
#'   (default), 5-fold CV with an NA-safe summary function is used. When
#'   `use_pca = TRUE`, `pcaComp = nPCs` is injected into its `preProcOptions`.
#' @param metric Optional character. Performance metric to optimize. If `NULL`
#'   (default), `"RMSE"` is used for regression and `"Accuracy"` for
#'   classification.
#' @param use_pca Logical. Whether to project predictors onto principal
#'   components inside each resample. Default `FALSE`.
#' @param nPCs Integer >= 1. Number of principal components to retain when
#'   `use_pca = TRUE`. Must be strictly less than `min(nrow, ncol)` of the
#'   predictor matrix. Default `NULL`, which is an error when
#'   `use_pca = TRUE` and ignored otherwise.
#' @param seed Optional integer seed applied locally (and restored on exit)
#'   before resampling and tuning. Default `NULL` (never seeds by default).
#' @param verbose Logical. Print progress messages. Default `FALSE`.
#' @param n_cores Integer >= 1. Number of workers for parallel training.
#'   Default `1` (sequential; no cluster is created). Values above the
#'   detected core count are capped.
#' @return An `fs_result` object with:
#'   \item{selected}{Predictors (or components, when `use_pca = TRUE`) whose
#'     coefficient at the chosen alpha/lambda is non-zero; for multinomial
#'     fits, the union across classes.}
#'   \item{scores}{Named numeric vector of absolute coefficients at the chosen
#'     alpha/lambda, one entry per column the model saw (predictors shrunk to
#'     zero are kept, with a score of 0); `NULL` for multinomial fits, where a
#'     predictor has one coefficient per class and no single score exists.}
#'   \item{method}{`"elastic_net"`.}
#'   \item{task}{`"regression"` or `"classification"`.}
#'   \item{model}{The `caret::train` object.}
#'   \item{details}{List of `coef` (coefficients at the best lambda, intercept
#'     included: a sparse matrix, or a list of them for multinomial fits),
#'     `best_alpha` and `best_lambda` (the winning tuning pair), `metric_name`
#'     (the metric optimized), `metric_value` (its resampled value for that
#'     pair, `NA` when the metric is absent from caret's results table),
#'     `use_pca`, and `n_features` (number of columns the model saw, i.e.
#'     `nPCs` when `use_pca = TRUE`).}
#'   \item{call}{The matched call.}
#' @examples
#' \donttest{
#' if (requireNamespace("caret", quietly = TRUE) &&
#'     requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("Matrix", quietly = TRUE)) {
#'   # x1 and x2 drive y; x3 is noise
#'   df <- data.frame(
#'     x1 = seq(-2, 2, length.out = 60),
#'     x2 = rep(c(-1, 0, 1), 20),
#'     x3 = cos(seq_len(60))
#'   )
#'   df$y <- 2 * df$x1 - df$x2 + 0.1 * cos(seq_len(60) * 3)
#'   res <- fs_elastic(df, "y", alpha_seq = c(0.5, 1), seed = 1)
#'   selected(res)
#'   res$scores
#'   res$details$best_alpha
#' }
#' }
#' @export
fs_elastic <- function(data,
                       target,
                       alpha_seq  = seq(0, 1, by = 0.1),
                       lambda_seq = NULL,
                       trControl  = NULL,
                       metric     = NULL,
                       use_pca    = FALSE,
                       nPCs       = NULL,
                       seed       = NULL,
                       verbose    = FALSE,
                       n_cores    = 1L) {
  mc <- match.call()

  assert_data_frame(data)
  assert_target(data, target)
  if (!is.numeric(alpha_seq) || length(alpha_seq) == 0L || anyNA(alpha_seq) ||
      any(alpha_seq < 0) || any(alpha_seq > 1)) {
    stop("'alpha_seq' must be a numeric vector with values in [0, 1].", call. = FALSE)
  }
  if (!is.null(lambda_seq)) {
    if (!is.numeric(lambda_seq) || length(lambda_seq) == 0L ||
        anyNA(lambda_seq) || any(lambda_seq < 0)) {
      stop("'lambda_seq' must be a numeric vector of non-negative values, or NULL.",
           call. = FALSE)
    }
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
  n_cores <- resolve_cores(n_cores, "n_cores")

  # as.data.frame() keeps data.table input away from data.table's NSE and
  # copies, so the caller's object is never touched.
  data <- as.data.frame(data)
  predictor_names <- setdiff(names(data), target)
  if (length(predictor_names) == 0L) {
    stop(sprintf("'data' must contain at least one predictor column besides '%s'.",
                 target), call. = FALSE)
  }

  fs_require(c("caret", "glmnet", "Matrix"), "elastic net feature selection")
  if (n_cores > 1L) {
    fs_require(c("foreach", "doParallel"), "parallel model training")
  }

  local_seed(seed)

  elastic_message("Extracting response and predictor variables...", verbose)
  form <- elastic_formula(target, predictor_names)
  vars <- elastic_extract_variables(data, form)
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
  # caret's preProcess cannot handle sparse input, so the PCA path stays dense.
  if (!use_pca && !inherits(x, "sparseMatrix") && prod(dim(x)) > 5e5) {
    x <- Matrix::Matrix(x, sparse = TRUE)
  }

  elastic_message("Handling missing values...", verbose)
  cleaned <- elastic_drop_missing(x, y)
  x <- cleaned$x
  y <- cleaned$y

  elastic_check_variance(x, if (use_pca) "PCA with scaling" else "model training")

  if (use_pca) {
    nPCs <- elastic_check_npcs(x, nPCs)
  }

  elastic_message("Creating tuning grid...", verbose)
  tuneGrid <- if (is.null(lambda_seq)) {
    elastic_lambda_grid(x, y, alpha_seq, task)
  } else {
    expand.grid(alpha = alpha_seq, lambda = lambda_seq)
  }

  if (is.null(trControl)) {
    elastic_message("Creating default trainControl...", verbose)
    trControl <- caret::trainControl(
      method          = "cv",
      number          = 5,
      summaryFunction = elastic_safe_summary
    )
  }
  if (use_pca) {
    # Refit PCA inside every resample rather than once on the full data.
    trControl$preProcOptions <- utils::modifyList(
      trControl$preProcOptions %||% list(),
      list(pcaComp = nPCs)
    )
  }
  # caret defaults allowParallel to TRUE, which would dispatch resamples to
  # whatever foreach backend the caller happens to have registered even when
  # featR created no cluster. Tie it to what featR actually set up.
  trControl$allowParallel <- n_cores > 1L

  elastic_message(
    if (n_cores > 1L) "Training models (parallel)..." else "Training models...",
    verbose
  )
  fit <- elastic_train_models(
    x          = x,
    y          = y,
    tuneGrid   = tuneGrid,
    trControl  = trControl,
    metric     = metric,
    preProcess = if (use_pca) c("center", "scale", "pca") else NULL,
    n_cores    = n_cores
  )

  elastic_message("Selecting best model...", verbose)
  best <- elastic_select_best(fit, metric = metric)

  elastic_message("Extracting coefficients at best lambda...", verbose)
  coefficients <- stats::coef(best$model, s = best$lambda)

  selected_features <- elastic_nonzero_names(coefficients)
  scores <- elastic_scores(coefficients)

  new_fs_result(
    selected = selected_features,
    scores   = scores,
    method   = "elastic_net",
    task     = task,
    model    = fit,
    details  = list(
      coef         = coefficients,
      best_alpha   = best$alpha,
      best_lambda  = best$lambda,
      metric_name  = best$metric_name,
      metric_value = best$metric_value,
      use_pca      = use_pca,
      n_features   = if (use_pca) nPCs else ncol(x)
    ),
    call = mc
  )
}
