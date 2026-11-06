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
  # Report the caller's column names, not model.matrix()'s backticked labels.
  colnames(mm) <- unbacktick_names(colnames(mm))

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
#' A column counts as constant when its range is negligible relative to its
#' magnitude (`is_numerically_constant()` with tolerance `64 * eps`), so a
#' column that differs only by floating-point round-off (e.g. `0.1 + 0.2`
#' versus `0.3`) is caught, while a genuinely varying column on a tiny scale
#' (sd 1e-5) or with a large offset (a timestamp near 1.7e9) is not. Dense and
#' sparse input use the same rule; sparse columns are scanned through their
#' stored values (plus an implicit 0 when the column is not full) without
#' densifying. Non-finite values are ignored.
#'
#' @param x A dense or sparse numeric matrix.
#' @return Character vector of offending column names (possibly empty).
#' @noRd
elastic_zero_sd_cols <- function(x) {
  if (nrow(x) < 2L) {
    return(character(0L))
  }
  tol <- 64 * .Machine$double.eps
  if (inherits(x, "sparseMatrix")) {
    n <- nrow(x)
    xc <- methods::as(x, "CsparseMatrix")
    vals <- if (methods::.hasSlot(xc, "x")) xc@x else rep(1, length(xc@i))
    p <- xc@p
    zero <- vapply(seq_len(ncol(xc)), function(j) {
      nnz <- p[j + 1L] - p[j]
      v <- if (nnz > 0L) vals[(p[j] + 1L):p[j + 1L]] else numeric(0L)
      if (nnz < n) v <- c(v, 0)
      is_numerically_constant(v, tol = tol)
    }, logical(1L))
  } else {
    zero <- vapply(seq_len(ncol(x)), function(j) {
      is_numerically_constant(x[, j], tol = tol)
    }, logical(1L))
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
#' The bound is checked against the full predictor matrix. caret also
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
    local_parallel_cluster(n_cores)
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

#' Coefficients of a glmnet fit at exactly the tuned lambda
#'
#' `coef(fit, s = lambda)` linearly interpolates between the two knots of the
#' fit's own lambda path that bracket `lambda`. caret's final glmnet model
#' uses glmnet's default 100-knot path while the tuning grid holds other
#' values (a 50-value path, or `lambda_seq`), so the tuned lambda almost never
#' sits on a knot, and interpolation gives every predictor that enters only at
#' the next-smaller knot a small non-zero coefficient: it would be reported as
#' selected although its coefficient at the chosen lambda is exactly 0. A tuned
#' value within rounding of a knot (it can come back one ulp below the path's
#' first knot, the null model) is snapped to that knot; otherwise the model is
#' refitted at `s` (`exact = TRUE`) from the data recorded in its call, falling
#' back to interpolation only if that refit fails.
#'
#' @param model A glmnet fit (caret's `finalModel`).
#' @param s The tuned lambda.
#' @return What `stats::coef()` returns for the fit at `s`.
#' @noRd
elastic_coef_at <- function(model, s) {
  lam <- model$lambda
  if (is.numeric(lam) && length(lam) > 0L && length(s) == 1L &&
      is.finite(s)) {
    j <- which.min(abs(lam - s))
    if (abs(lam[j] - s) <= sqrt(.Machine$double.eps) * max(abs(s), abs(lam[j]))) {
      return(stats::coef(model, s = lam[j]))
    }
    # caret hands glmnet the data itself (do.call), so the call carries the
    # values; evaluating in emptyenv() refuses anything that is not a value.
    cl <- model$call
    if (!is.null(cl$x) && !is.null(cl$y)) {
      exact <- tryCatch({
        xv <- eval(cl$x, envir = emptyenv())
        yv <- eval(cl$y, envir = emptyenv())
        if (length(dim(xv)) != 2L || ncol(xv) != model$dim[1L]) {
          stop("design matrix not recoverable")
        }
        stats::coef(model, s = s, exact = TRUE, x = xv, y = yv)
      }, error = function(e) NULL)
      if (!is.null(exact)) {
        return(exact)
      }
    }
  }
  stats::coef(model, s = s)
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

#' Validate a user-supplied trControl
#'
#' Must be NULL or a full `caret::trainControl()` list: a string or a partial
#' list such as `list(method = "cv")` otherwise fails deep inside caret with
#' "argument is of length zero".
#'
#' @param trControl The `trControl` argument.
#' @return Invisibly `NULL`.
#' @noRd
elastic_check_trcontrol <- function(trControl) {
  if (is.null(trControl)) {
    return(invisible(NULL))
  }
  core <- c("method", "number", "repeats", "summaryFunction",
            "selectionFunction", "classProbs", "returnResamp")
  missing_fields <- if (is.list(trControl)) {
    core[!core %in% names(trControl) |
           vapply(core, function(f) is.null(trControl[[f]]), logical(1L))]
  } else {
    core
  }
  if (length(missing_fields) > 0L) {
    stop(sprintf(paste0(
      "'trControl' must be NULL or a list built by caret::trainControl(); ",
      "missing field(s): %s."), paste(missing_fields, collapse = ", ")),
      call. = FALSE)
  }
  invisible(NULL)
}

#' Stop on categorical predictors with fewer than two distinct values
#'
#' `stats::model.matrix()` otherwise fails with the unhelpful "contrasts can
#' be applied only to factors with 2 or more levels", without naming the
#' column.
#'
#' @param data A data.frame.
#' @param predictors Predictor column names.
#' @return Invisibly `NULL`.
#' @noRd
elastic_check_categorical_levels <- function(data, predictors) {
  bad <- predictors[vapply(predictors, function(nm) {
    v <- data[[nm]]
    (is.factor(v) || is.character(v)) &&
      length(unique(as.character(v[!is.na(v)]))) < 2L
  }, logical(1L))]
  if (length(bad) > 0L) {
    stop(sprintf(paste0(
      "Categorical predictor(s) with fewer than 2 distinct values: %s%s. ",
      "Remove them before calling fs_elastic()."),
      paste0("'", utils::head(bad, 5L), "'", collapse = ", "),
      if (length(bad) > 5L) ", ..." else ""), call. = FALSE)
  }
  invisible(NULL)
}

#' Elastic-net feature selection with tuned alpha and lambda
#'
#' @description
#' Tunes an elastic-net model over a grid of `alpha` and `lambda` values with
#' `caret::train(method = "glmnet")` and selects the predictors whose
#' coefficient is non-zero at the winning pair. Handles regression (numeric
#' outcome) and classification (factor, character or logical outcome).
#'
#' @details
#' The elastic net penalizes coefficients with a mix of an L1 (lasso) and an
#' L2 (ridge) penalty: `alpha` sets the mix and `lambda` the overall
#' strength. Both are chosen by resampling, and every column with a non-zero
#' coefficient at the winning pair is reported. When the winning `alpha` is
#' below 1, the ridge part spreads weight across correlated predictors, so a
#' group of collinear columns tends to survive together instead of being
#' reduced to one representative as `fs_lasso()` tends to do.
#'
#' `alpha = 0` is pure ridge, which never sets a coefficient to exactly zero
#' and so cannot select. The default `alpha_seq` starts at 0.1 for that
#' reason. If you include 0 and it wins, every column is selected and a
#' warning says so.
#'
#' The winning pair is the one with the best resampled metric (caret's
#' default `selectionFunction = "best"`). With hundreds of alpha/lambda
#' candidates that choice is noisy and leans towards too little penalty: on
#' pure-noise data (100 rows, 50 predictors) it selected 8.5 predictors on
#' average and at least one in every run. A `trControl` with
#' `selectionFunction = "oneSE"` picks the most penalized pair within one
#' standard error of the best and was much sparser in the same simulation
#' (1.8 predictors on average).
#'
#' For the same reason `details$metric_value`, the best value over the whole
#' grid, is an optimistic estimate of performance. `fs_elastic()` holds out
#' no test rows and reports no honest performance estimate; that needs an
#' outer resampling loop around the whole call.
#'
#' @section How the design matrix is built:
#' Every column of `data` except `target` is a candidate predictor. The
#' formula is built for you (non-syntactic names are backticked), and the
#' predictors go through `stats::model.matrix()` with the intercept column
#' removed, so a k-level factor or character column contributes k - 1 dummy
#' columns. `selected`, `scores` and `details$coef` therefore name
#' design-matrix columns rather than the original columns. Non-syntactic
#' names are reported as you wrote them, without the backticks
#' `model.matrix()` adds. Above 500,000 cells the matrix is converted to a
#' sparse matrix unless `use_pca = TRUE`.
#'
#' These inputs are errors:
#' * fewer than two design-matrix columns (`glmnet` cannot fit a
#'   single-column `x`), so a single numeric predictor fails;
#' * a factor or character predictor with fewer than two distinct values;
#' * a dummy name that collides with another column (factor `g` with level
#'   `b` next to a column called `gb`);
#' * Inf or -Inf in the target or any predictor;
#' * a predictor column that is constant, or constant up to floating-point
#'   round-off (range at most `64 * .Machine$double.eps` times its largest
#'   absolute value), checked the same way for dense and sparse input.
#'
#' @section Missing values and the outcome type:
#' Rows with a missing response or a missing value in any predictor are
#' dropped before fitting; if no rows remain, that is an error. The task is
#' then inferred from the remaining rows. A class that occurred only in
#' dropped rows is removed with a message, and a class with fewer than two
#' complete rows is an error. When rows are dropped, a user-supplied
#' `trControl` carrying `index`, `indexOut` or `indexFinal` is an error,
#' because those row numbers no longer match.
#'
#' A logical response is converted to a two-level factor with a message. A
#' numeric response with only two distinct values is still treated as
#' regression, with a warning telling you to convert it to a factor if you
#' meant classification.
#'
#' @section Scores:
#' `scores` are absolute coefficients on the scale of the columns the model
#' saw, not standardized ones. They rank predictors fairly only when those
#' columns are on comparable scales; unlike `fs_lasso()`, this function does
#' not rescale them for you. Coefficients are taken at exactly the winning
#' lambda (the path is refitted if that lambda is not one of the final
#' model's knots), not interpolated between knots.
#'
#' @section Principal components inside resampling:
#' With `use_pca = TRUE`, caret is asked for
#' `preProcess = c("center", "scale", "pca")` with `pcaComp = nPCs` in
#' `trControl$preProcOptions`. Centering, scaling and the component loadings
#' are therefore refitted on the training part of every resample, and the
#' held-out fold never contributes to them. The model is fitted on
#' components, so `selected`, `scores` and `details$coef` are named `PC1`,
#' `PC2`, and so on, rather than after the original columns. `nPCs` must be
#' smaller than the number of predictors and than the number of rows each
#' resample trains on.
#'
#' @section Tuning grid and metrics:
#' `lambda_seq = NULL` (the default) tunes over the lambda path that
#' `glmnet::glmnet()` itself proposes at each alpha (up to 50 values per
#' alpha), which is scaled to the data, instead of a fixed sequence that
#' spends most of its fits on irrelevant lambdas. Only the candidate values
#' come from the full data, as in caret's own default glmnet grid; which pair
#' wins is still decided by resampling. With `use_pca = TRUE` the path is
#' computed on the original predictors, so it only approximates the scale of
#' the components; pass `lambda_seq` if you need control over it.
#'
#' The resampled metrics in `model$results` and `details$metric_value` are
#' caret's. For regression they are `RMSE`, `Rsquared` and `MAE`. `Rsquared`
#' is the squared Pearson correlation between held-out predictions and
#' observations (`caret::postResample()`), not `1 - SSE/SST`, so it is not
#' comparable with an `R2` defined as `1 - SSE/SST` (for example
#' `fs_randomforest()`'s). It lies in \[0, 1\], except that with the default
#' `trControl = NULL` featR's summary function records a fold where it is
#' undefined (constant predictions, typical at the large-lambda end of the
#' path) as `-Inf`, so such grid rows show `-Inf` in `model$results`. For
#' classification the metrics are `Accuracy` and Cohen's `Kappa`, which need
#' the suggested package 'e1071'. featR checks for it before fitting when
#' the default summary function is used; a custom `summaryFunction` in
#' `trControl` is responsible for its own dependencies.
#'
#' @param data A data frame (or data.table) containing the target and the
#'   candidate predictors, with unique, non-empty column names.
#' @param target Single string naming the outcome column in `data`.
#' @param alpha_seq Numeric vector of alpha values to tune over, each in
#'   `[0, 1]`. Default `seq(0.1, 1, by = 0.1)`, which leaves out 0 (ridge)
#'   because ridge cannot zero a coefficient.
#' @param lambda_seq Numeric vector of non-negative lambda values to tune
#'   over, or `NULL` (default) to use glmnet's own path at each alpha (see
#'   "Tuning grid and metrics").
#' @param trControl Optional `caret::trainControl()` object; a partial list or
#'   a string is an error. If `NULL` (default), 5-fold CV with an NA-safe
#'   summary function is used. When `use_pca = TRUE`, `pcaComp = nPCs` is
#'   added to its `preProcOptions`. Its `allowParallel` is always overwritten
#'   with `n_cores > 1`, so a registered foreach backend is used only when
#'   `n_cores > 1`.
#' @param metric Optional character. Performance metric to optimize. If
#'   `NULL` (default), `"RMSE"` for regression and `"Accuracy"` for
#'   classification. If caret does not find the name in the summary output,
#'   it warns and falls back to its default, and `details$metric_name`
#'   reports the metric actually used.
#' @param use_pca Logical; default `FALSE`. Whether to project the predictors
#'   onto principal components inside each resample.
#' @param nPCs Integer >= 1. Number of principal components to retain when
#'   `use_pca = TRUE`; must be strictly less than `min(nrow, ncol)` of the
#'   predictor matrix. Default `NULL`, which is an error when
#'   `use_pca = TRUE` and ignored otherwise.
#' @param seed Optional whole number; default `NULL`, which leaves the RNG
#'   alone. When given, the seed is set for this call only and the previous
#'   RNG state is restored on exit. The resampling folds are drawn in the
#'   main R process and the glmnet fits are deterministic, so a seeded
#'   `n_cores > 1` run gives the same result as a seeded sequential one. When
#'   `trControl$seeds` is unset (`NA`, the `trainControl()` default), caret
#'   draws one seed per resample from this seed in the main process and sets
#'   it on each worker, so seeded runs that use worker-side randomness (for
#'   example `sampling = "up"`) are reproducible with any `n_cores`. A
#'   user-supplied `trControl$seeds` takes precedence.
#' @param verbose Logical; default `FALSE`. Print progress messages.
#' @param n_cores Integer >= 1; default 1 (sequential, and no cluster is
#'   created). Number of workers for parallel training; values above the
#'   detected core count are capped. Values above 1 need the 'foreach' and
#'   'doParallel' packages.
#' @return An `fs_result` object with:
#'   \item{selected}{Columns (or components, when `use_pca = TRUE`) whose
#'     coefficient at the chosen alpha/lambda is non-zero; for multinomial
#'     fits, the union across classes.}
#'   \item{scores}{Named numeric vector of absolute coefficients at the chosen
#'     alpha/lambda, one entry per column the model saw; columns shrunk to
#'     zero are kept with a score of 0. `NULL` for multinomial fits, where a
#'     predictor has one coefficient per class and no single score exists.}
#'   \item{method}{`"elastic_net"`.}
#'   \item{task}{`"regression"` or `"classification"`.}
#'   \item{model}{The `caret::train` object.}
#'   \item{details}{List of `coef` (coefficients at the winning lambda,
#'     intercept included: a sparse matrix, or a list of them for
#'     multinomial fits), `best_alpha` and `best_lambda` (the winning tuning
#'     pair), `metric_name` (the metric caret optimized, which differs from
#'     `metric` when caret fell back to its default), `metric_value` (its
#'     resampled value for the winning pair, `NA` when the metric is absent
#'     from caret's results table; optimistic, see Details), `use_pca`, and
#'     `n_features` (number of columns the model saw, which is `nPCs` when
#'     `use_pca = TRUE`).}
#'   \item{call}{The matched call.}
#' @examples
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
#'   print(selected(res))
#'   print(res$scores)
#'   print(res$details$best_alpha)
#' }
#' @seealso The [Regularization and embedded importance](https://elkronos.github.io/featR/articles/regularization.html)
#'   guide, which runs this function on a worked example.
#' @export
fs_elastic <- function(data,
                       target,
                       alpha_seq  = seq(0.1, 1, by = 0.1),
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
  # A duplicated name kept only one copy in the model frame, so the other
  # silently vanished from the scores.
  assert_column_names(data, "fs_elastic")
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
  elastic_check_trcontrol(trControl)
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

  # anyNA() does not catch Inf: an infinite predictor was silently zeroed out
  # by glmnet and an infinite target failed with an opaque glmnet error.
  assert_finite_columns(data, names(data), "fs_elastic")
  elastic_check_categorical_levels(data, predictor_names)

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
  if (ncol(x) < 2L) {
    stop("fs_elastic() needs at least two design-matrix columns (glmnet ",
         "cannot fit a single-column x).", call. = FALSE)
  }

  # A dummy named <factor><level> can equal another column's name; the
  # selection would then report two indistinguishable entries.
  assert_unique_encoded_names(colnames(x), NULL, "fs_elastic")

  # Incomplete rows are removed BEFORE the task is inferred, so droplevels()
  # and the class checks see only the rows that are actually fitted (a class
  # whose rows all had a missing predictor used to survive as an empty level
  # and crash glmnet).
  elastic_message("Handling missing values...", verbose)
  n_rows_in <- nrow(x)
  y_levels_in <- if (is.factor(y)) {
    levels(droplevels(y))
  } else if (is.character(y) || is.logical(y)) {
    sort(unique(as.character(y[!is.na(y)])))
  } else {
    NULL
  }
  cleaned <- elastic_drop_missing(x, y)
  x <- cleaned$x
  y <- cleaned$y
  # A user-supplied trControl$index / indexOut / indexFinal numbers the rows
  # of `data`. Once incomplete rows are dropped those numbers point at
  # different observations (every row after a dropped one shifts up), so the
  # resamples (or the final fit) would silently use the wrong rows.
  if (nrow(x) < n_rows_in && !is.null(trControl) &&
      (!is.null(trControl$index) || !is.null(trControl$indexOut) ||
       !is.null(trControl$indexFinal))) {
    stop(sprintf(paste0(
      "%d row(s) with missing values were dropped, so the row numbers in ",
      "'trControl$index'/'trControl$indexOut'/'trControl$indexFinal' no ",
      "longer refer to the rows they were built for. Remove incomplete rows ",
      "from 'data' before building the resampling indices."),
      n_rows_in - nrow(x)),
      call. = FALSE)
  }
  if (!is.null(y_levels_in)) {
    lost <- setdiff(y_levels_in, unique(as.character(y)))
    if (length(lost) > 0L) {
      message(sprintf(
        "Class(es) %s of '%s' occur only in rows with missing predictor values and were dropped.",
        paste0("'", lost, "'", collapse = ", "), target))
    }
  }

  elastic_message("Inferring task type (regression vs classification)...", verbose)
  task_info <- elastic_infer_task(y)
  y <- task_info$y
  task <- task_info$task
  if (is.null(metric)) {
    metric <- task_info$metric
  }
  if (identical(task, "classification")) {
    # caret's classification summary (defaultSummary -> postResample, used by
    # the default elastic_safe_summary) needs e1071 inside every resample;
    # fail here with a featR message instead of inside caret.
    summary_fun <- if (is.null(trControl)) NULL else trControl$summaryFunction
    if (is.null(summary_fun) ||
        identical(summary_fun, elastic_safe_summary) ||
        identical(summary_fun, caret::defaultSummary) ||
        identical(summary_fun, caret::multiClassSummary)) {
      fs_require("e1071", "classification metrics (caret::postResample)")
    }
    counts <- table(y)
    small <- names(counts)[counts < 2L]
    if (length(small) > 0L) {
      stop(sprintf(paste0(
        "Class(es) %s of '%s' have fewer than 2 complete rows; glmnet needs ",
        "at least 2 observations per class. Drop or merge those classes."),
        paste0("'", small, "'", collapse = ", "), target), call. = FALSE)
    }
  }

  elastic_message("Ensuring predictors are in sparse format (if large)...", verbose)
  # caret's preProcess cannot handle sparse input, so the PCA path stays dense.
  if (!use_pca && !inherits(x, "sparseMatrix") && prod(dim(x)) > 5e5) {
    x <- Matrix::Matrix(x, sparse = TRUE)
  }

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
  # caret falls back (with a warning) to RMSE/Accuracy when `metric` is not
  # in the summary output; report the metric that was actually optimized.
  best <- elastic_select_best(fit, metric = fit$metric %||% metric)
  if (isTRUE(best$alpha == 0)) {
    warning("The winning alpha is 0 (pure ridge), which never zeroes a ",
            "coefficient, so every column is reported as selected. Drop 0 ",
            "from 'alpha_seq' to make the result a selection.", call. = FALSE)
  }

  elastic_message("Extracting coefficients at best lambda...", verbose)
  # Exactly at the tuned lambda, not interpolated between path knots (which
  # reported predictors that only enter at the next knot as selected).
  coefficients <- elastic_coef_at(best$model, best$lambda)

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
