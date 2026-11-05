# Recursive feature elimination (caret-based) for featR.

# -----------------------------
# Validation and small utilities
# -----------------------------

#' Print a progress message when verbose
#' @noRd
rfe_message <- function(msg, verbose) {
  if (isTRUE(verbose)) message(msg)
}

#' Infer task type from a target vector
#'
#' Factors, characters, and logicals imply classification; everything else is
#' treated as regression.
#' @noRd
rfe_task_type <- function(y) {
  if (is.factor(y) || is.character(y) || is.logical(y)) {
    return("classification")
  }
  "regression"
}

#' Guard a random-forest RFE function set against constant resample columns
#'
#' `randomForest()` never returns on a classification fit in which no
#' predictor can be split. featR removes predictors that are constant on the
#' whole training split, but a rare indicator can still be constant inside one
#' cross-validation resample, and caret's size-1 fit on it then hangs. The
#' wrapped `fit` drops columns with fewer than two distinct values in that
#' resample (falling back to a majority-class / mean model when none is
#' left), `pred` handles that fallback, and `rank` appends the dropped
#' columns at the bottom with importance 0 so every candidate is still ranked.
#'
#' Applied only when `funcs$fit` is caret's `rfFuncs$fit`. The wrappers use
#' base R only and a minimal enclosing environment, because caret serialises
#' them to parallel workers that may not have featR loaded.
#' @noRd
rfe_guard_rf_funcs <- function(funcs) {
  if (!identical(funcs$fit, caret::rfFuncs$fit)) return(funcs)
  env <- new.env(parent = baseenv())
  env$fit0 <- funcs$fit
  env$pred0 <- funcs$pred
  env$rank0 <- funcs$rank
  env$varies <- function(x) {
    if (is.matrix(x)) {
      apply(x, 2L, function(v) length(unique(v[!is.na(v)])) > 1L)
    } else {
      vapply(x, function(v) length(unique(v[!is.na(v)])) > 1L, logical(1L))
    }
  }
  fit <- function(x, y, first, last, ...) {
    keep <- varies(x)
    if (!any(keep)) {
      return(structure(list(y = y, vars = colnames(x)),
                       class = "featr_constant_fit"))
    }
    obj <- fit0(x[, keep, drop = FALSE], y, first, last, ...)
    obj$featr_vars <- colnames(x)[keep]
    obj
  }
  pred <- function(object, x) {
    if (inherits(object, "featr_constant_fit")) {
      y <- object$y
      n <- nrow(x)
      if (is.factor(y)) {
        p <- as.numeric(table(y)) / length(y)
        names(p) <- levels(y)
        cls <- factor(rep(levels(y)[which.max(p)], n), levels = levels(y))
        probs <- as.data.frame(matrix(rep(p, each = n), nrow = n,
                                      dimnames = list(NULL, levels(y))))
        return(cbind(data.frame(pred = cls), probs))
      }
      return(rep(mean(y), n))
    }
    if (!is.null(object$featr_vars)) {
      x <- x[, object$featr_vars, drop = FALSE]
    }
    pred0(object, x)
  }
  rank <- function(object, x, y) {
    all_vars <- colnames(x)
    if (inherits(object, "featr_constant_fit")) {
      return(data.frame(Overall = rep(0, length(all_vars)), var = all_vars,
                        row.names = all_vars, stringsAsFactors = FALSE))
    }
    used <- if (is.null(object$featr_vars)) all_vars else object$featr_vars
    r <- rank0(object, x[, used, drop = FALSE], y)
    miss <- setdiff(all_vars, r$var)
    if (length(miss) > 0L) {
      add <- r[rep(1L, length(miss)), , drop = FALSE]
      num <- vapply(add, is.numeric, logical(1L))
      add[num] <- 0
      add$var <- miss
      rownames(add) <- miss
      r <- rbind(r, add)
    }
    r
  }
  environment(fit) <- env
  environment(pred) <- env
  environment(rank) <- env
  funcs$fit <- fit
  funcs$pred <- pred
  funcs$rank <- rank
  funcs
}

#' Validate RFE resampling control parameters
#'
#' `caret::rfeControl()` returns a plain list, so only lists are accepted.
#' @return Invisibly, `control_params` with `method` normalised to caret's
#'   canonical spelling.
#' @noRd
rfe_validate_rfe_control <- function(control_params) {
  if (!is.list(control_params)) {
    stop("'rfe_control' must be a list.", call. = FALSE)
  }
  if (!("method" %in% names(control_params))) {
    stop("'rfe_control' must contain 'method'.", call. = FALSE)
  }
  if (!("number" %in% names(control_params))) {
    stop("'rfe_control' must contain 'number'.", call. = FALSE)
  }
  assert_string(control_params$method, "rfe_control$method")
  # The resampling schemes caret::rfe() knows how to build; anything else
  # leaves its resampling index NULL and fails deep inside caret
  # ("attempt to set an attribute on NULL").
  rfe_methods <- c("cv", "repeatedcv", "LOOCV", "boot", "boot632", "LGOCV",
                   "test")
  # caret matches the method case-insensitively; normalise to its canonical
  # spelling so the checks below (and caret's own `== "LOOCV"`) see it.
  m_idx <- match(tolower(control_params$method), tolower(rfe_methods))
  if (is.na(m_idx)) {
    stop(sprintf("'rfe_control$method' must be one of %s; got '%s'.",
                 paste0("'", rfe_methods, "'", collapse = ", "),
                 control_params$method), call. = FALSE)
  }
  control_params$method <- rfe_methods[m_idx]
  assert_count(control_params$number, "rfe_control$number", lower = 1L)
  # A one-fold CV leaves an empty training set in every resample.
  if (control_params$method %in% c("cv", "repeatedcv") &&
      control_params$number < 2) {
    stop(sprintf("'rfe_control$number' must be at least 2 for method = '%s'.",
                 control_params$method), call. = FALSE)
  }
  rfe_reject_row_indices(control_params, "rfe_control")

  rfe_parts <- c("summary", "fit", "pred", "rank", "selectSize", "selectVar")
  if (!is.null(control_params$functions)) {
    fns <- control_params$functions
    ok <- is.list(fns) && all(rfe_parts %in% names(fns)) &&
      all(vapply(fns[intersect(rfe_parts, names(fns))], is.function,
                 logical(1L)))
    if (!ok) {
      missing_parts <- if (is.list(fns)) {
        rfe_parts[!rfe_parts %in% names(fns) |
                    !vapply(rfe_parts, function(nm) is.function(fns[[nm]]),
                            logical(1L))]
      } else {
        rfe_parts
      }
      stop(sprintf(paste0(
        "'rfe_control$functions' must be a caret RFE function set (a list ",
        "of functions %s), for example caret::rfFuncs or caret::lmFuncs; ",
        "missing or not a function: %s."),
        paste(rfe_parts, collapse = ", "),
        paste(missing_parts, collapse = ", ")), call. = FALSE)
    }
  }

  if (identical(control_params$method, "repeatedcv")) {
    if (!("repeats" %in% names(control_params))) {
      warning("'rfe_control$repeats' not provided for method = 'repeatedcv'; caret's default of 1 will be used.",
              call. = FALSE)
    } else {
      assert_count(control_params$repeats, "rfe_control$repeats", lower = 1L)
    }
  }
  invisible(control_params)
}

#' Refuse user-supplied resampling row indices
#'
#' Resampling runs on the training split, whose rows are drawn inside
#' `fs_recursivefeature()`. Row numbers built for `data` would therefore point
#' at different observations of that split, so the resamples would silently
#' train and test on the wrong rows.
#' @noRd
rfe_reject_row_indices <- function(control_params, arg) {
  given <- intersect(c("index", "indexOut", "indexFinal"),
                     names(control_params))
  if (length(given) > 0L) {
    stop(sprintf(paste0(
      "'%s$%s' is not supported: resampling runs on the training split ",
      "chosen inside fs_recursivefeature(), so row indices built for 'data' ",
      "would refer to the wrong rows. Use 'method'/'number' instead."),
      arg, given[1L]), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate training control parameters
#'
#' `caret::trainControl()` returns a plain (unclassed) list, so only lists are
#' accepted and validated.
#' @noRd
rfe_validate_train_control <- function(control_params) {
  if (!is.list(control_params)) {
    stop("'train_control' must be a list.", call. = FALSE)
  }
  if (!("method" %in% names(control_params))) {
    stop("'train_control' must contain at least 'method'.", call. = FALSE)
  }
  assert_string(control_params$method, "train_control$method")
  rfe_reject_row_indices(control_params, "train_control")

  if (!identical(control_params$method, "none")) {
    if (!("number" %in% names(control_params))) {
      stop("When 'train_control$method' is not 'none', provide 'train_control$number'.",
           call. = FALSE)
    }
    assert_count(control_params$number, "train_control$number", lower = 1L)
    if (control_params$method %in% c("cv", "repeatedcv") &&
        control_params$number < 2) {
      stop(sprintf("'train_control$number' must be at least 2 for method = '%s'.",
                   control_params$method), call. = FALSE)
    }
  }

  if (identical(control_params$method, "repeatedcv") &&
      "repeats" %in% names(control_params)) {
    assert_count(control_params$repeats, "train_control$repeats", lower = 1L)
  }
  invisible(TRUE)
}

#' Named importance vector from caret::varImp() on an rfe object
#'
#' `caret::varImp.rfe()` returns a data.frame whose row names are the variable
#' names and whose "Overall" column holds the resample-averaged importance at
#' the optimal subset size.
#'
#' @param var_imp The data.frame returned by `caret::varImp()`.
#' @return A named numeric vector, or NULL when nothing usable is present.
#' @noRd
rfe_importance_scores <- function(var_imp) {
  if (!is.data.frame(var_imp) || nrow(var_imp) == 0L) {
    return(NULL)
  }
  nms <- rownames(var_imp)
  if (is.null(nms)) {
    return(NULL)
  }
  if ("Overall" %in% names(var_imp) && is.numeric(var_imp[["Overall"]])) {
    return(stats::setNames(as.numeric(var_imp[["Overall"]]), nms))
  }
  num <- vapply(var_imp, is.numeric, logical(1L))
  if (!any(num)) {
    return(NULL)
  }
  stats::setNames(
    as.numeric(rowMeans(as.matrix(var_imp[, num, drop = FALSE]), na.rm = TRUE)),
    nms
  )
}

#' Full-size, all-resample importance from an rfe object
#'
#' `caret::varImp.rfe()` averages each variable only over the resamples in
#' which it survived to the optimal subset size, so a marginal variable that
#' made the cut in one fold is scored by that fold alone and a variable that
#' never made it gets no score. Every resample ranks all candidates at the
#' full size, so averaging those rows scores every candidate on the same
#' footing.
#'
#' @param rfe_obj A caret `rfe` object.
#' @param features Character vector of candidate (internal) predictor names.
#' @return A numeric vector named by `features` (`NA` where nothing was
#'   recorded), or NULL when `rfe_obj$variables` carries no usable importance.
#' @noRd
rfe_fullsize_scores <- function(rfe_obj, features) {
  vars <- rfe_obj$variables
  if (!is.data.frame(vars) || nrow(vars) == 0L ||
      !all(c("Overall", "var") %in% names(vars)) ||
      !is.numeric(vars$Overall)) {
    return(NULL)
  }
  if ("Variables" %in% names(vars)) {
    vars <- vars[vars$Variables == max(vars$Variables), , drop = FALSE]
  }
  agg <- tapply(vars$Overall, as.character(vars$var), mean, na.rm = TRUE)
  out <- stats::setNames(rep(NA_real_, length(features)), features)
  common <- intersect(features, names(agg))
  out[common] <- as.numeric(agg[common])
  out
}

#' Names of predictors that are constant on the given rows
#'
#' Numeric columns use `is_numerically_constant()`; factors count the levels
#' actually present.
#' @noRd
rfe_constant_columns <- function(df, cols) {
  cols[vapply(cols, function(nm) {
    x <- df[[nm]]
    if (is.factor(x)) {
      nlevels(droplevels(x[!is.na(x)])) < 2L
    } else if (is.numeric(x)) {
      is_numerically_constant(x)
    } else {
      length(unique(x[!is.na(x)])) < 2L
    }
  }, logical(1L))]
}

# -----------------------------
# Encoding (train-fitted, applied to others)
# -----------------------------

#' Fit a one-hot encoder on training predictors
#'
#' Fits a `caret::dummyVars()` transformer on the training predictors only
#' (the target is excluded). Uses `fullRank = TRUE` so downstream models do
#' not receive a rank-deficient all-levels design.
#'
#' Categorical predictors must already be factors: `dummyVars()` stores levels
#' only for columns where `is.factor()` is TRUE, and anything else is re-levelled
#' from the data passed to `predict()`, which would defeat the point of fitting
#' the encoder on the training rows.
#'
#' @param train_df Training data.frame, character/logical predictors already
#'   converted to factors carrying the training levels.
#' @param target Target column name.
#' @return A fitted `dummyVars` object.
#' @noRd
rfe_fit_encoder <- function(train_df, target) {
  predictors <- train_df[, setdiff(colnames(train_df), target), drop = FALSE]
  caret::dummyVars(~ ., data = predictors, fullRank = TRUE)
}

#' Apply a fitted one-hot encoder and reattach the target
#'
#' @param data_df data.frame to transform.
#' @param target Target column name.
#' @param dv Fitted `dummyVars` object.
#' @return A data.frame with the target first, then encoded predictors.
#' @noRd
rfe_apply_encoder <- function(data_df, target, dv) {
  predictors <- data_df[, setdiff(colnames(data_df), target), drop = FALSE]
  X <- as.data.frame(stats::predict(dv, newdata = predictors))
  # predict.dummyVars() deparses non-syntactic names with backticks
  # ("`my var`", "`my grp`b"); report the user's names instead.
  colnames(X) <- unbacktick_names(colnames(X))
  # A dummy '<var><level>' can coincide with another column (factor 'g' with
  # level '1' next to a numeric 'g1'); left alone, caret would resolve the
  # name to the first copy and silently lose the other predictor.
  assert_unique_encoded_names(colnames(X), target, "fs_recursivefeature")
  # Defensive only: predictor levels are aligned before the encoder is fitted
  # and predict.dummyVars() passes NAs through, so the row count should always
  # match. Report what was actually detected rather than guessing a cause.
  if (nrow(X) != nrow(data_df)) {
    stop(sprintf(paste(
      "One-hot encoding returned %d row(s) for %d input row(s), so the",
      "encoded predictors can no longer be matched to the target. This should",
      "not happen; please report it with a reproducible example."
    ), nrow(X), nrow(data_df)), call. = FALSE)
  }
  data.frame(
    stats::setNames(list(data_df[[target]]), target),
    X,
    check.names = FALSE
  )
}

# -----------------------------
# Parallel backend helpers
# -----------------------------

#' Map internal (syntactic) predictor names back to the user's names
#'
#' @param x Character vector of internal names (or NULL).
#' @param lookup Named character vector: values are the user's names, names
#'   are the internal ones.
#' @return `x` with every known internal name replaced.
#' @noRd
rfe_unmap_names <- function(x, lookup) {
  if (is.null(x) || length(lookup) == 0L) {
    return(x)
  }
  x <- as.character(x)
  hit <- x %in% names(lookup)
  x[hit] <- unname(lookup[x[hit]])
  x
}

# -----------------------------
# RFE core
# -----------------------------

#' Run recursive feature elimination on the training data
#'
#' @param train_df Training data.frame containing the target.
#' @param target Target column name.
#' @param sizes Numeric vector of subset sizes, or NULL for `1:ncol(X)`.
#' @param rfe_control_params List for `caret::rfeControl()` (method/number at
#'   least), already checked with `rfe_validate_rfe_control()`. `functions` selects the caret RFE function set and defaults to
#'   `caret::rfFuncs`; an `allowParallel` entry is dropped with a warning.
#' @param parallel Logical; passed to `rfeControl(allowParallel = )`.
#' @param verbose Logical; used as `rfeControl(verbose = )` unless the caller
#'   set it in `rfe_control`.
#' @return A caret `rfe` object.
#' @noRd
rfe_perform <- function(train_df, target, sizes, rfe_control_params,
                        parallel = FALSE, verbose = FALSE) {
  # rfe_control_params is validated by fs_recursivefeature() up front (once,
  # so the repeatedcv warning is not emitted twice).

  train_df <- as.data.frame(train_df)
  if (!target %in% colnames(train_df)) {
    stop("Target column not found in the training data.", call. = FALSE)
  }

  feature_funcs <- rfe_control_params$functions
  if (is.null(feature_funcs)) {
    feature_funcs <- caret::rfFuncs
  }
  # A predictor can be constant inside one resample even when it varies on
  # the training split; randomForest would then hang on that fit.
  feature_funcs <- rfe_guard_rf_funcs(feature_funcs)
  rfe_control_params$functions <- NULL

  # rfeControl() cannot receive allowParallel twice; the 'parallel' argument
  # of fs_recursivefeature() owns it.
  if ("allowParallel" %in% names(rfe_control_params)) {
    warning("'rfe_control$allowParallel' is ignored; use the 'parallel' argument of fs_recursivefeature() instead.",
            call. = FALSE)
    rfe_control_params$allowParallel <- NULL
  }

  ctrl_args <- c(
    list(functions = feature_funcs, allowParallel = isTRUE(parallel)),
    rfe_control_params
  )
  if (!("verbose" %in% names(ctrl_args))) {
    ctrl_args$verbose <- isTRUE(verbose)
  }
  ctrl <- do.call(caret::rfeControl, ctrl_args)

  X <- train_df[, setdiff(colnames(train_df), target), drop = FALSE]

  # Defensive: fs_recursivefeature() already turns character/logical predictors
  # into factors before calling this helper, so in the normal path there is
  # nothing left to convert. Repeat it here so the helper is safe on its own.
  for (nm in colnames(X)) {
    if (is.character(X[[nm]]) || is.logical(X[[nm]])) {
      X[[nm]] <- factor(X[[nm]])
    }
  }

  if (anyNA(X)) {
    stop("Predictors contain missing values; impute or drop incomplete rows before running fs_recursivefeature().",
         call. = FALSE)
  }
  assert_finite_columns(X, colnames(X), "fs_recursivefeature")

  y_vec <- train_df[[target]]
  if (rfe_task_type(y_vec) == "classification" && !is.factor(y_vec)) {
    y_vec <- as.factor(y_vec)
  }

  if (is.null(sizes)) {
    sizes <- seq_len(ncol(X))
  } else {
    kept <- sizes[sizes >= 1 & sizes <= ncol(X)]
    if (length(kept) < length(sizes)) {
      warning(sprintf("Removed %d value(s) of 'sizes' outside [1, %d].",
                      length(sizes) - length(kept), ncol(X)), call. = FALSE)
    }
    sizes <- kept
    if (length(sizes) == 0L) {
      stop("No valid 'sizes' remain within the range of available predictors.",
           call. = FALSE)
    }
  }

  caret::rfe(x = X, y = y_vec, sizes = sizes, rfeControl = ctrl)
}

# -----------------------------
# Final model training
# -----------------------------

#' Train the final caret model on the selected predictors
#'
#' @param data_df data.frame with the target and predictors (training rows).
#' @param target Target column name.
#' @param optimal_vars Character vector of selected predictor names.
#' @param train_control_params List of `caret::trainControl()` arguments.
#' @param model_method caret model key (e.g. "rf", "lm").
#' @param parallel Logical; the default for `trainControl(allowParallel = )`
#'   when `train_control_params` does not set it, so a caller's registered
#'   foreach backend is not used unless `fs_recursivefeature()` was asked to
#'   run in parallel.
#' @return A `caret::train` object; the predictors actually used (after NZV
#'   and linear-combination filtering) are stored in
#'   `attr(model, "predictors_used")`.
#' @noRd
rfe_train_final <- function(data_df, target, optimal_vars,
                            train_control_params = list(method = "cv",
                                                        number = 5),
                            model_method = "rf", parallel = FALSE) {
  rfe_validate_train_control(train_control_params)

  missing_vars <- setdiff(optimal_vars, colnames(data_df))
  if (length(missing_vars) > 0L) {
    stop("The following selected predictors are missing from the data: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  df <- data_df[, c(target, optimal_vars), drop = FALSE]

  # Coerce the target appropriately.
  task <- rfe_task_type(df[[target]])
  if (task == "classification") {
    df[[target]] <- as.factor(df[[target]])
  } else {
    df[[target]] <- as.numeric(df[[target]])
  }

  # Reasonable predictor types: logical -> integer, character -> factor.
  # fs_recursivefeature() has already turned both into factors by the time it
  # calls this, so neither branch fires on the normal path; they only matter
  # when the helper is handed data from somewhere else.
  pred_names <- setdiff(colnames(df), target)
  for (nm in pred_names) {
    if (is.logical(df[[nm]])) {
      df[[nm]] <- as.integer(df[[nm]])
    } else if (is.character(df[[nm]])) {
      df[[nm]] <- factor(df[[nm]])
    }
  }

  # Remove near-zero variance predictors.
  if (length(pred_names) > 0L) {
    nzv_idx <- caret::nearZeroVar(df[, pred_names, drop = FALSE],
                                  saveMetrics = FALSE)
    if (length(nzv_idx) > 0L) {
      removed <- pred_names[nzv_idx]
      warning("Removing near-zero variance predictor(s) before final training: ",
              paste(removed, collapse = ", "), call. = FALSE)
      pred_names <- setdiff(pred_names, removed)
      df <- df[, c(target, pred_names), drop = FALSE]
    }
  }

  # Remove linear combinations among numeric predictors.
  if (length(pred_names) > 1L) {
    X <- df[, pred_names, drop = FALSE]
    num_cols <- vapply(X, is.numeric, logical(1L))
    if (sum(num_cols) > 1L) {
      lc <- caret::findLinearCombos(as.matrix(X[, num_cols, drop = FALSE]))
      if (!is.null(lc$remove) && length(lc$remove) > 0L) {
        drop_lc <- colnames(X[, num_cols, drop = FALSE])[lc$remove]
        warning("Removing linearly dependent predictor(s) before final training: ",
                paste(drop_lc, collapse = ", "), call. = FALSE)
        pred_names <- setdiff(pred_names, drop_lc)
        df <- df[, c(target, pred_names), drop = FALSE]
      }
    }
  }

  if (length(pred_names) == 0L) {
    stop("No predictors remain after preprocessing for the final model.",
         call. = FALSE)
  }

  if (is.null(train_control_params$allowParallel)) {
    train_control_params$allowParallel <- isTRUE(parallel)
  }
  tr_ctrl <- do.call(caret::trainControl, train_control_params)
  form <- stats::as.formula(paste(backtick(target), "~ ."))

  model_obj <- caret::train(form, data = df, method = model_method,
                            trControl = tr_ctrl)
  attr(model_obj, "predictors_used") <- pred_names
  model_obj
}

# -----------------------------
# Main wrapper
# -----------------------------

#' Recursive feature elimination with held-out evaluation
#'
#' Answers "how few predictors can I keep before resampled performance starts
#' to fall off?" Splits the data into stratified train/test partitions,
#' optionally one-hot encodes the predictors (encoder fitted on the training
#' rows only), runs `caret::rfe()` on the training set, evaluates the fitted
#' RFE model on the held-out test rows, and optionally trains a final caret
#' model on the training rows.
#'
#' @details
#' RFE is a wrapper method: it refits the underlying model once per candidate
#' subset size per resample, so it is by far the most expensive method here,
#' and its answer is specific to the model family in `rfe_control$functions`
#' rather than being a general statement about the features. In exchange, the
#' subset it reports is tuned to the model you actually intend to use, and the
#' subset size is chosen by resampling instead of by a threshold you invent.
#'
#' Requires the suggested package 'caret'. The RFE function set comes from
#' `rfe_control$functions` and defaults to `caret::rfFuncs`, which fits random
#' forests, so the suggested package 'randomForest' must also be installed
#' unless you supply a different set (for example
#' `rfe_control = list(method = "cv", number = 5, functions = caret::lmFuncs)`);
#' it is also needed for the default final model (`model_method = "rf"`) when
#' `return_final_model = TRUE`. Both are checked before any fitting starts.
#' Parallel execution additionally requires 'foreach' and 'doParallel';
#' classification metrics use `caret::postResample()`, which needs 'e1071'
#' (also checked before any fitting starts).
#'
#' Everything that could leak is fitted on the training rows: the one-hot
#' encoder, the factor levels the test columns are aligned to, the elimination
#' itself, and the final model. `details$test_metrics` is therefore a genuine
#' held-out estimate. It applies `caret::postResample()` to the predictions of
#' the fitted `rfe` object (caret's own refit on all the training rows,
#' restricted to the optimal subset) on test rows that took no part in choosing
#' either the features or the subset size.
#'
#' The metrics are caret's: for regression `RMSE`, `Rsquared` and `MAE`, where
#' `Rsquared` is the \emph{squared Pearson correlation} between predictions
#' and observations, not `1 - SSE/SST`. It is never negative and ignores bias
#' and scale errors, so it is not comparable with an `R2` defined as
#' `1 - SSE/SST` (for example `fs_randomforest()`'s `details$metrics$R2`).
#' For classification they are `Accuracy` (proportion of held-out rows
#' classified correctly) and `Kappa` (Cohen's kappa, accuracy corrected for
#' chance agreement).
#'
#' Two other summaries on the result are \emph{not} held-out estimates, by
#' construction. `details$resampling_results` summarizes resampling performed
#' inside the training rows across candidate subset sizes, and, when
#' `return_final_model = TRUE`, `model$results` reports `train_control`
#' resampling inside those same training rows on features that were already
#' selected. Only `details$test_metrics` is computed on data the search never
#' saw.
#'
#' Predictor names that are not syntactic R names (for example `"a b"`) are
#' replaced by their `make.names()` versions inside the `rfe` object and the
#' final model, because caret's function sets cannot handle them; `selected`,
#' `scores`, `details$variable_importance` and `details$final_model_variables`
#' report the original names.
#'
#' Infinite values (`Inf`/`-Inf`) anywhere in `data` are an error.
#' Predictors that are constant on the training rows (a single value, or a
#' factor with a single observed level) are removed before RFE with a
#' warning; they carry no information, and a random forest fitted on a single
#' constant predictor never returns. Factor predictors keep only the levels
#' observed in the training rows; a held-out value with a level the training
#' rows never saw is treated as missing, with a warning.
#'
#' Missing values in the \emph{training} predictors are rejected with an error;
#' impute or drop incomplete rows before calling. NAs in the held-out rows are
#' not imputed: depending on the function set, `predict()` either stops (random
#' forests) or returns `NA` for those rows, which `caret::postResample()` then
#' leaves out of `details$test_metrics`; a warning reports how many rows were
#' left out. With `handle_categorical = TRUE` the encoded test
#' rows are row-count checked against their input, so an encoding that quietly
#' loses rows becomes an error rather than a silently misaligned metric.
#'
#' @param data A data.frame or data.table with at least one row and one column,
#'   holding the target and the candidate predictors (every other column), with
#'   unique, non-empty column names and at least two candidate predictors
#'   (after any one-hot encoding). It is converted to a plain data.frame on
#'   entry.
#' @param target Single string naming the target column of `data`; a column
#'   index is not accepted. A factor, character, or logical target means
#'   classification (unused factor levels are dropped), anything else
#'   regression. The target may not contain missing or infinite values.
#' @param sizes Vector of whole-number feature-subset sizes to evaluate
#'   (fractional sizes are an error). Default
#'   `NULL`, which uses `1:p`, where `p` is the predictor count after any
#'   one-hot encoding. Values outside `[1, p]` are dropped with a warning; if
#'   that leaves nothing, the call is an error rather than a silent empty run.
#' @param train_ratio Numeric, strictly between 0 and 1: the training
#'   proportion of the stratified split. Default 0.8.
#' @param rfe_control List of arguments for `caret::rfeControl()`; must contain
#'   at least `method` and `number`. Default `list(method = "cv", number = 5)`.
#'   `method` must be one of `"cv"`, `"repeatedcv"`, `"LOOCV"`, `"boot"`,
#'   `"boot632"`, `"LGOCV"` or `"test"`, and `number` must be at least 2 for
#'   `"cv"`/`"repeatedcv"`. `index`, `indexOut` and `indexFinal` are not
#'   accepted (an error): resampling runs on the internal training split, so
#'   row numbers built for `data` would point at the wrong rows.
#'   `functions` selects the caret RFE function set and defaults to
#'   `caret::rfFuncs`; a supplied set must contain the functions `summary`,
#'   `fit`, `pred`, `rank`, `selectSize` and `selectVar`. An `allowParallel` entry is dropped with a warning (use
#'   the `parallel` argument instead), while a `verbose` entry, if present,
#'   overrides the `verbose` argument for `caret::rfe()`. With
#'   `method = "repeatedcv"` and no `repeats`, caret's default of 1 is used and
#'   a warning says so.
#' @param train_control List of arguments for `caret::trainControl()`, used
#'   only when `return_final_model = TRUE`, and then validated (together with
#'   `model_method`) before the search starts. Must contain `method`, and also
#'   `number` unless `method = "none"` (at least 2 for `"cv"`/`"repeatedcv"`).
#'   `index`, `indexOut` and `indexFinal` are not accepted (an error), for the
#'   same reason as in `rfe_control`. Unless it is set here, `allowParallel`
#'   follows the `parallel` argument, so a foreach backend you registered
#'   yourself is not used by a sequential call. Default
#'   `list(method = "cv", number = 5)`.
#' @param model_method Single string; the caret model key used for the final
#'   model, for example `"rf"` or `"lm"`. Used only when
#'   `return_final_model = TRUE`. Default `"rf"`.
#' @param handle_categorical Logical; one-hot encode predictors with full-rank
#'   dummies (fitted on the training rows, applied to the test rows). A dummy
#'   name (`<variable><level>`) that collides with another column is an
#'   error. Function sets that work on a model matrix (for example
#'   `caret::lmFuncs`) rank dummy columns rather than factors, so they need
#'   `handle_categorical = TRUE` when factor predictors are present; without
#'   it the call stops with an error saying so. Default `FALSE`.
#' @param return_final_model Logical; train a final caret model on the training
#'   rows using the selected features and return it as `model`, with the `rfe`
#'   object still available in `details$rfe`. Near-zero-variance and linearly
#'   dependent predictors are dropped from the selected set first, each with a
#'   warning, and what survives is recorded in `details$final_model_variables`.
#'   Default `FALSE`.
#' @param seed Optional single whole number within the range of an R integer;
#'   fractional or out-of-range values are an error rather than being
#'   truncated. It covers the split and the RFE resampling, is applied for
#'   the duration of the call only (the previous RNG state is restored on
#'   exit), and defaults to `NULL`, which never seeds. Under
#'   `parallel = TRUE` a seed also sets `rfe_control$seeds = NULL` (unless
#'   you supplied `seeds`), so caret draws one seed per resample from the
#'   locally seeded RNG; that is what makes two seeded parallel runs agree
#'   with each other (the workers additionally get L'Ecuyer-CMRG streams, but
#'   those alone would not, because foreach hands resamples to whichever
#'   worker is free). A seeded parallel run need not equal a seeded
#'   sequential one, because the sequential run keeps caret's default
#'   `seeds = NA`.
#' @param verbose Logical; print progress messages and let `caret::rfe()`
#'   report its own progress, unless `rfe_control$verbose` overrides the
#'   latter. Default `FALSE`.
#' @param parallel Logical. If `TRUE`, registers a two-worker PSOCK cluster
#'   (capped at the detected core count) for the duration of the call, then
#'   stops it and restores whichever foreach backend was registered before the
#'   call; requires the suggested packages 'foreach' and 'doParallel'.
#'   The RFE resampling and the final model's resampling both follow this
#'   setting; a custom function set such as `caret::caretFuncs` builds its own
#'   inner `caret::train()` control and is not covered. Default `FALSE`.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector of the variables RFE kept
#'         (`optVariables` at the optimal subset size).}
#'   \item{scores}{Named numeric vector with one entry per candidate
#'         predictor: its importance in the full-size fit (all candidates
#'         present), averaged over every resample. Unlike `caret::varImp()`
#'         on the `rfe` object, this does not average a variable only over
#'         the resamples where it survived to the optimal size, so marginal
#'         variables are not inflated and no candidate is missing. When the
#'         function set records no per-resample importance, the
#'         `caret::varImp()` values are used instead; `NULL` when there is
#'         nothing usable.}
#'   \item{method}{"rfe".}
#'   \item{task}{"classification" or "regression", inferred from the target.}
#'   \item{model}{The final `caret::train` model when
#'         `return_final_model = TRUE`, otherwise the `rfe` object.}
#'   \item{details}{A list, in snake_case, with `rfe` (the caret `rfe` object,
#'         always present even when `model` holds the final model),
#'         `optimal_size` (the subset size RFE chose), `test_metrics`
#'         (`caret::postResample()` on the held-out rows -- the only held-out
#'         estimate on the object), `resampling_results` (the RFE resampling
#'         summary over candidate sizes, computed inside the training rows),
#'         `variable_importance` (the `caret::varImp()` data.frame, which
#'         averages each variable only over the resamples where it survived
#'         to the optimal size; `selected` follows caret's `optVariables`
#'         rule, which is weighted the same way),
#'         `preprocessor` (the `dummyVars` encoder, or `NULL` when
#'         `handle_categorical = FALSE`), `train_index` and `test_index` (row
#'         indices into `data` for the two partitions), `final_model_variables`
#'         (predictors the final model actually used after
#'         NZV/linear-combination filtering, or `NULL` when no final model was
#'         requested) and `n_features` (candidate predictors offered to RFE,
#'         counted after one-hot encoding when `handle_categorical = TRUE` and
#'         after constant predictors were removed).}
#'   \item{call}{The matched call.}
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("caret", quietly = TRUE) &&
#'     requireNamespace("randomForest", quietly = TRUE) &&
#'     requireNamespace("e1071", quietly = TRUE)) {
#'   res <- fs_recursivefeature(
#'     iris,
#'     target = "Species",
#'     sizes = 1:4,
#'     rfe_control = list(method = "cv", number = 3),
#'     seed = 42
#'   )
#'   print(res$selected)
#'   print(res$details$optimal_size)
#'   # the only held-out estimate on the object
#'   print(res$details$test_metrics)
#' }
#' }
#' @export
fs_recursivefeature <- function(data,
                                target,
                                sizes = NULL,
                                train_ratio = 0.8,
                                rfe_control = list(method = "cv", number = 5),
                                train_control = list(method = "cv",
                                                     number = 5),
                                model_method = "rf",
                                handle_categorical = FALSE,
                                return_final_model = FALSE,
                                seed = NULL,
                                verbose = FALSE,
                                parallel = FALSE) {
  cl_call <- match.call()

  assert_data_frame(data, "data")
  # Plain data.frame semantics for all subsetting below (a data.table input
  # would otherwise dispatch to NSE-based [.data.table).
  data <- as.data.frame(data)
  assert_target(data, target, "target")
  assert_column_names(data, "fs_recursivefeature")
  # Inf in a predictor or the target otherwise fails inside caret's foreach
  # loop ('task 1 failed - "NA/NaN/Inf in foreign function call"').
  assert_finite_columns(data, colnames(data), "fs_recursivefeature")
  if (!is.null(sizes) &&
      (!is.numeric(sizes) || length(sizes) == 0L || anyNA(sizes) ||
       any(!is.finite(sizes)) || any(sizes != trunc(sizes)))) {
    stop("'sizes' must be a vector of whole numbers, or NULL.", call. = FALSE)
  }
  assert_number(train_ratio, "train_ratio")
  if (train_ratio <= 0 || train_ratio >= 1) {
    stop("'train_ratio' must be strictly between 0 and 1.", call. = FALSE)
  }
  assert_string(model_method, "model_method")
  assert_flag(handle_categorical, "handle_categorical")
  assert_flag(return_final_model, "return_final_model")
  assert_flag(verbose, "verbose")
  assert_flag(parallel, "parallel")

  fs_require("caret", "recursive feature elimination")
  rfe_control <- rfe_validate_rfe_control(rfe_control)
  if (return_final_model) {
    # Checked before the (expensive) search, not only once it has finished.
    rfe_validate_train_control(train_control)
    if (is.null(caret::getModelInfo(model_method, regex = FALSE)[[model_method]])) {
      stop(sprintf("'model_method' = '%s' is not in caret's built-in model library.",
                   model_method), call. = FALSE)
    }
  }
  # The default function set (caret::rfFuncs) and the default final model
  # ("rf") both fit random forests; fail up front, not inside a resample.
  rfe_funcs <- rfe_control$functions
  if (is.null(rfe_funcs) || identical(rfe_funcs, caret::rfFuncs) ||
      (return_final_model && identical(model_method, "rf"))) {
    fs_require("randomForest", "random-forest based RFE / final model")
  }

  y_raw <- data[[target]]
  task <- rfe_task_type(y_raw)
  if (task == "classification") {
    # caret's classification summary (postResample) needs e1071 inside every
    # resample, so check before any work starts.
    fs_require("e1071", "classification metrics (caret::postResample)")
  }
  if (anyNA(y_raw)) {
    stop("The target contains missing values; remove those rows before calling fs_recursivefeature().",
         call. = FALSE)
  }
  if (task == "classification") {
    # One factor, with only the classes present, for the split, the RFE fit
    # and the held-out metrics alike. An unused level made randomForest stop
    # ("Can't have empty classes in y"), and re-deriving the test levels from
    # the test rows alone let caret::postResample() drop every prediction of
    # a class absent from the test rows, inflating the held-out accuracy.
    data[[target]] <- droplevels(as.factor(y_raw))
    y_raw <- data[[target]]
    # Checked here, not left to caret, whose errors arrive wrapped by foreach
    # ('task 1 failed - "Need at least two classes ..."', 'column name ""
    # cannot match any column').
    if (nlevels(y_raw) < 2L) {
      stop("A classification target must have at least two observed classes.",
           call. = FALSE)
    }
    assert_class_labels(y_raw, target)
  }

  # Seed applies to the split and to RFE resampling; restored on exit.
  local_seed(seed)

  y_part <- if (task == "classification") as.factor(y_raw) else as.numeric(y_raw)
  train_idx <- fs_split_index(y_part, p = train_ratio)
  test_idx <- setdiff(seq_len(nrow(data)), train_idx)
  train_raw <- data[train_idx, , drop = FALSE]
  test_raw <- data[test_idx, , drop = FALSE]
  if (length(test_idx) == 0L) {
    stop("Test partition is empty; decrease 'train_ratio' or supply more data.",
         call. = FALSE)
  }
  rfe_message(sprintf("Training on %d rows; holding out %d rows.",
                      length(train_idx), length(test_idx)), verbose)

  # Coerce character/logical predictors to factor (levels from the training
  # rows) and align the test columns to those levels, so RFE, prediction, and
  # final training all see consistent types.
  #
  # This must happen BEFORE the one-hot encoder is fitted:
  # `caret::dummyVars()` records levels (its `lvls` field) only for columns
  # that are already factors -- it tests `is.factor()` on the raw data -- and
  # `predict.dummyVars()` passes exactly those levels to `model.frame(xlev =)`.
  # A character or logical column handed over un-coerced therefore has its
  # levels re-derived from whatever `newdata` happens to contain, so the "fitted
  # on the training rows only" encoder would silently be refitted on the test
  # rows: different dummy columns, a different base level, or a different width,
  # all with an unchanged row count.
  #
  # Factor columns keep only the levels observed in the training rows, the
  # same as character columns: unused levels, or levels seen only in held-out
  # rows, would otherwise become all-zero candidate dummies. A held-out value
  # whose level the training rows never saw becomes NA (reported below).
  n_unseen <- 0L
  for (nm in setdiff(colnames(train_raw), target)) {
    if (is.character(train_raw[[nm]]) || is.logical(train_raw[[nm]])) {
      train_raw[[nm]] <- factor(train_raw[[nm]])
    } else if (is.factor(train_raw[[nm]])) {
      train_raw[[nm]] <- droplevels(train_raw[[nm]])
    }
    if (is.factor(train_raw[[nm]])) {
      before_na <- is.na(test_raw[[nm]])
      test_raw[[nm]] <- factor(as.character(test_raw[[nm]]),
                               levels = levels(train_raw[[nm]]))
      n_unseen <- n_unseen + sum(is.na(test_raw[[nm]]) & !before_na)
    }
  }
  if (n_unseen > 0L) {
    warning(sprintf(paste(
      "%d held-out value(s) have a factor level not seen in the training",
      "rows and are treated as missing."
    ), n_unseen), call. = FALSE)
  }

  # Constant predictors (including a factor with a single level on the
  # training rows) are removed before encoding: caret::dummyVars() cannot
  # encode a one-level factor and failed with "contrasts can be applied only
  # to factors with 2 or more levels".
  const_raw <- rfe_constant_columns(train_raw,
                                    setdiff(colnames(train_raw), target))
  if (length(const_raw) > 0L) {
    warning("Removing predictor(s) that are constant on the training rows ",
            "before RFE: ", paste(const_raw, collapse = ", "), call. = FALSE)
    train_raw <- train_raw[, setdiff(colnames(train_raw), const_raw),
                           drop = FALSE]
    test_raw <- test_raw[, setdiff(colnames(test_raw), const_raw),
                         drop = FALSE]
  }

  preproc <- NULL
  if (handle_categorical) {
    preproc <- rfe_fit_encoder(train_raw, target = target)
    train_df <- rfe_apply_encoder(train_raw, target = target, dv = preproc)
    test_df <- rfe_apply_encoder(test_raw, target = target, dv = preproc)
  } else {
    pred_names <- setdiff(colnames(train_raw), target)
    train_df <- train_raw[, c(target, pred_names), drop = FALSE]
    test_df <- test_raw[, c(target, pred_names), drop = FALSE]
  }

  # Constant predictors carry no information, and a constant column is a
  # precondition violation for randomForest: a classification forest on a
  # single constant predictor (subset size 1) never returns.
  const_cols <- rfe_constant_columns(train_df,
                                     setdiff(colnames(train_df), target))
  if (length(const_cols) > 0L) {
    warning("Removing predictor(s) that are constant on the training rows ",
            "before RFE: ", paste(const_cols, collapse = ", "), call. = FALSE)
    train_df <- train_df[, setdiff(colnames(train_df), const_cols),
                         drop = FALSE]
    test_df <- test_df[, setdiff(colnames(test_df), const_cols), drop = FALSE]
  }

  n_features <- length(setdiff(colnames(train_df), target))
  # Elimination needs something to eliminate between; with a single predictor
  # caret's rfFuncs also fails mid-resample on a classification target
  # ('task 1 failed - "argument 1 is not a vector"').
  if (n_features < 2L) {
    stop("fs_recursivefeature() needs at least two candidate predictors ",
         "(after any encoding); 'data' has ", n_features, ".", call. = FALSE)
  }

  # caret's function sets (lmFuncs, caretFuncs, ...) rebuild data frames with
  # check.names = TRUE, so non-syntactic predictor names such as "a b" break
  # them mid-resample. Work on make.names() versions internally and map the
  # reported names back afterwards.
  user_names <- setdiff(colnames(train_df), target)
  safe_names <- make.names(c(target, user_names), unique = TRUE)[-1L]
  name_lookup <- stats::setNames(user_names, safe_names)
  name_lookup <- name_lookup[names(name_lookup) != name_lookup]
  if (length(name_lookup) > 0L) {
    pred_pos <- match(user_names, colnames(train_df))
    colnames(train_df)[pred_pos] <- safe_names
    colnames(test_df)[match(user_names, colnames(test_df))] <- safe_names
  }

  if (parallel) {
    # Teardown is scheduled before registration and restores the caller's
    # previous foreach backend on exit.
    local_parallel_cluster(resolve_cores(2L), seed = seed)
    # Worker RNG streams alone do not make a seeded run reproducible: foreach
    # hands resamples to whichever worker is free, so which stream fits which
    # resample varies from run to run. rfeControl()'s default seeds = NA
    # disables caret's per-resample seeding; seeds = NULL makes caret draw
    # them from the (locally seeded) master RNG, after the resampling index.
    if (!is.null(seed) && !"seeds" %in% names(rfe_control)) {
      rfe_control["seeds"] <- list(NULL)
    }
  }

  rfe_message("Running recursive feature elimination on the training rows...",
              verbose)
  rfe_obj <- tryCatch(
    rfe_perform(
      train_df = train_df,
      target = target,
      sizes = sizes,
      rfe_control_params = rfe_control,
      parallel = parallel,
      verbose = verbose
    ),
    error = function(e) {
      has_factor <- any(vapply(train_df[setdiff(colnames(train_df), target)],
                               is.factor, logical(1L)))
      if (!handle_categorical && has_factor &&
          grepl("undefined columns selected", conditionMessage(e),
                fixed = TRUE)) {
        stop(paste(
          "caret::rfe() failed with 'undefined columns selected'. The",
          "function set in 'rfe_control$functions' ranks model coefficients",
          "(dummy columns such as 'gb'), not the factor predictors themselves;",
          "set handle_categorical = TRUE to one-hot encode factor predictors",
          "before RFE."
        ), call. = FALSE)
      }
      stop(e)
    }
  )

  # ---- Evaluate on the held-out test rows ------------------------------------
  test_x <- test_df[, setdiff(colnames(test_df), target), drop = FALSE]
  preds <- stats::predict(rfe_obj, test_x)
  if (is.data.frame(preds)) {
    preds <- if ("pred" %in% colnames(preds)) preds[["pred"]] else preds[[1L]]
  }

  test_y <- test_df[[target]]
  if (task == "classification") {
    # Same levels as the training target (the whole-data factor), never
    # levels re-derived from the test rows.
    test_y <- factor(test_y, levels = levels(data[[target]]))
  } else {
    test_y <- as.numeric(test_y)
  }
  # caret::postResample() silently drops NA predictions (e.g. lm on a held-out
  # row with a missing predictor), so say how many rows the metrics exclude.
  n_na_pred <- sum(is.na(preds))
  if (n_na_pred > 0L) {
    warning(sprintf(paste(
      "%d of %d held-out prediction(s) are NA (missing values in the test",
      "predictors) and are excluded from 'details$test_metrics'."
    ), n_na_pred, length(preds)), call. = FALSE)
  }
  test_metrics <- caret::postResample(preds, test_y)

  optimal_size <- rfe_obj$optsize
  optimal_vars <- rfe_obj$optVariables
  var_imp <- caret::varImp(rfe_obj)
  resamp <- rfe_obj$results
  var_imp_user <- var_imp
  if (is.data.frame(var_imp_user) && !is.null(rownames(var_imp_user))) {
    rownames(var_imp_user) <- rfe_unmap_names(rownames(var_imp_user),
                                              name_lookup)
  }

  # Scores: every candidate's importance at the full size, averaged over all
  # resamples (see rfe_fullsize_scores()); caret's own varImp() table, which
  # averages only over the resamples where a variable survived, stays in
  # details$variable_importance. Fall back to it for a custom function set
  # that records no per-resample importance.
  scores <- rfe_fullsize_scores(rfe_obj, setdiff(colnames(train_df), target))
  if (is.null(scores)) {
    scores <- rfe_importance_scores(var_imp_user)
  } else {
    names(scores) <- rfe_unmap_names(names(scores), name_lookup)
  }

  final_model <- NULL
  final_model_vars <- NULL

  if (return_final_model) {
    # Train on the TRAINING rows only, so the held-out rows stay unseen by
    # every model this function returns.
    rfe_message("Training the final model on the training rows only...",
                verbose)
    final_model <- rfe_train_final(
      data_df = train_df,
      target = target,
      optimal_vars = optimal_vars,
      train_control_params = train_control,
      model_method = model_method,
      parallel = parallel
    )
    final_model_vars <- rfe_unmap_names(attr(final_model, "predictors_used"),
                                        name_lookup)
  }

  new_fs_result(
    selected = rfe_unmap_names(as.character(optimal_vars), name_lookup),
    scores   = scores,
    method   = "rfe",
    task     = task,
    model    = if (return_final_model) final_model else rfe_obj,
    details  = list(
      rfe                   = rfe_obj,
      optimal_size          = optimal_size,
      test_metrics          = test_metrics,
      resampling_results    = resamp,
      variable_importance   = var_imp_user,
      preprocessor          = preproc,
      train_index           = train_idx,
      test_index            = test_idx,
      final_model_variables = final_model_vars,
      n_features            = n_features
    ),
    call = cl_call
  )
}
