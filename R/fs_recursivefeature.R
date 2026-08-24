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

#' Validate RFE resampling control parameters
#'
#' `caret::rfeControl()` returns a plain list, so only lists are accepted.
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
  assert_count(control_params$number, "rfe_control$number", lower = 1L)

  if (!is.null(control_params$functions) &&
      !is.list(control_params$functions)) {
    stop("'rfe_control$functions' must be a caret RFE function set (a list), for example caret::rfFuncs or caret::lmFuncs.",
         call. = FALSE)
  }

  if (identical(control_params$method, "repeatedcv")) {
    if (!("repeats" %in% names(control_params))) {
      warning("'rfe_control$repeats' not provided for method = 'repeatedcv'; caret's default of 1 will be used.",
              call. = FALSE)
    } else {
      assert_count(control_params$repeats, "rfe_control$repeats", lower = 1L)
    }
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

  if (!identical(control_params$method, "none")) {
    if (!("number" %in% names(control_params))) {
      stop("When 'train_control$method' is not 'none', provide 'train_control$number'.",
           call. = FALSE)
    }
    assert_count(control_params$number, "train_control$number", lower = 1L)
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

# -----------------------------
# Encoding (train-fitted, applied to others)
# -----------------------------

#' Fit a one-hot encoder on training predictors
#'
#' Fits a `caret::dummyVars()` transformer on the training predictors only
#' (the target is excluded). Uses `fullRank = TRUE` so downstream models do
#' not receive a rank-deficient all-levels design.
#'
#' @param train_df Training data.frame.
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
  if (nrow(X) != nrow(data_df)) {
    stop(sprintf(paste(
      "One-hot encoding returned %d row(s) for %d input row(s).",
      "This usually means missing values or factor levels unseen in training;",
      "clean or impute the data before calling fs_recursivefeature()."
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

#' Start a doParallel backend for RFE
#'
#' Registers a two-worker PSOCK cluster (capped at the detected core count via
#' `resolve_cores()`). featR never grabs all available cores.
#'
#' @param enable Logical; return NULL without side effects when FALSE.
#' @return The cluster object, or NULL.
#' @noRd
rfe_start_parallel <- function(enable) {
  if (!isTRUE(enable)) {
    return(NULL)
  }
  fs_require(c("foreach", "doParallel"), "parallel RFE")
  cores <- resolve_cores(2L)
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  cl
}

#' Stop a doParallel backend and restore sequential execution
#'
#' @param cl Cluster object or NULL.
#' @return Invisibly NULL.
#' @noRd
rfe_stop_parallel <- function(cl) {
  if (is.null(cl)) {
    return(invisible(NULL))
  }
  try(parallel::stopCluster(cl), silent = TRUE)
  if (requireNamespace("foreach", quietly = TRUE)) {
    try(foreach::registerDoSEQ(), silent = TRUE)
  }
  invisible(NULL)
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
#'   least). `functions` selects the caret RFE function set and defaults to
#'   `caret::rfFuncs`; an `allowParallel` entry is dropped with a warning.
#' @param parallel Logical; passed to `rfeControl(allowParallel = )`.
#' @param verbose Logical; used as `rfeControl(verbose = )` unless the caller
#'   set it in `rfe_control`.
#' @return A caret `rfe` object.
#' @noRd
rfe_perform <- function(train_df, target, sizes, rfe_control_params,
                        parallel = FALSE, verbose = FALSE) {
  rfe_validate_rfe_control(rfe_control_params)

  train_df <- as.data.frame(train_df)
  if (!target %in% colnames(train_df)) {
    stop("Target column not found in the training data.", call. = FALSE)
  }

  feature_funcs <- rfe_control_params$functions
  if (is.null(feature_funcs)) {
    feature_funcs <- caret::rfFuncs
  }
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

  # Mirror the final-model coercions so RFE sees the same predictor types.
  for (nm in colnames(X)) {
    if (is.character(X[[nm]]) || is.logical(X[[nm]])) {
      X[[nm]] <- factor(X[[nm]])
    }
  }

  if (anyNA(X)) {
    stop("Predictors contain missing values; impute or drop incomplete rows before running fs_recursivefeature().",
         call. = FALSE)
  }

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
#' @return A `caret::train` object; the predictors actually used (after NZV
#'   and linear-combination filtering) are stored in
#'   `attr(model, "predictors_used")`.
#' @noRd
rfe_train_final <- function(data_df, target, optimal_vars,
                            train_control_params = list(method = "cv",
                                                        number = 5),
                            model_method = "rf") {
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
#' Splits the data into stratified train/test partitions, optionally one-hot
#' encodes the predictors (encoder fitted on the training rows only), runs
#' `caret::rfe()` on the training set, evaluates the selected feature set on
#' the held-out test rows, and optionally trains a final caret model on the
#' training rows.
#'
#' @details
#' Requires the suggested package 'caret'. The RFE function set comes from
#' `rfe_control$functions` and defaults to `caret::rfFuncs`, which fits random
#' forests, so the suggested package 'randomForest' must also be installed
#' unless you supply a different set (for example
#' `rfe_control = list(method = "cv", number = 5, functions = caret::lmFuncs)`).
#' Parallel execution additionally requires 'foreach' and 'doParallel';
#' classification metrics use `caret::postResample()`, which needs 'e1071'.
#'
#' `details$test_metrics` is computed by predicting on the held-out test rows
#' with the fitted RFE model and summarizing with `caret::postResample()`.
#' When `return_final_model = TRUE`, the final model is trained on the
#' \emph{training rows only} (not the full data), so those metrics remain an
#' honest estimate. Predictors containing missing values are rejected; impute
#' before calling.
#'
#' @param data A data.frame (or data.table) with the target and predictors.
#' @param target Character. Name of the target column in `data`.
#' @param sizes Numeric vector of feature-subset sizes to evaluate; `NULL`
#'   uses `1:p`. Out-of-range values are dropped with a warning.
#' @param train_ratio Numeric in (0, 1). Training proportion of the stratified
#'   split (default 0.8).
#' @param rfe_control List of arguments for `caret::rfeControl()`; must contain
#'   at least `method` and `number`. `functions` selects the caret RFE function
#'   set (default `caret::rfFuncs`). Any `allowParallel` entry is dropped with
#'   a warning; use the `parallel` argument instead.
#' @param train_control List of arguments for `caret::trainControl()` used when
#'   `return_final_model = TRUE`.
#' @param model_method caret model key for the final model (e.g. `"rf"`,
#'   `"lm"`).
#' @param handle_categorical Logical; one-hot encode predictors with full-rank
#'   dummies (fitted on the training rows, applied to the test rows).
#' @param return_final_model Logical; train a final caret model on the training
#'   rows using the selected features, and return it as `model`.
#' @param seed Optional whole number seed. Applied for the duration of the call
#'   only (previous RNG state is restored on exit); default `NULL` never seeds.
#' @param verbose Logical; print progress messages and let `caret::rfe()`
#'   report its own progress. Default `FALSE`.
#' @param parallel Logical. If `TRUE`, registers a two-worker PSOCK cluster
#'   (capped at the available cores) for the duration of the call; requires the
#'   suggested packages 'foreach' and 'doParallel'.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector of the variables RFE kept
#'         (`optVariables` at the optimal subset size).}
#'   \item{scores}{Named numeric vector of resample-averaged importance from
#'         `caret::varImp()` on the `rfe` object (its "Overall" column), or
#'         `NULL` when caret reports none.}
#'   \item{method}{"rfe".}
#'   \item{task}{"classification" or "regression".}
#'   \item{model}{The final `caret::train` model when
#'         `return_final_model = TRUE`, otherwise the `rfe` object.}
#'   \item{details}{A list, in snake_case, with `rfe` (the caret `rfe` object,
#'         always present even when `model` holds the final model),
#'         `optimal_size` (the subset size RFE chose), `test_metrics`
#'         (`caret::postResample()` on the held-out rows), `resampling_results`
#'         (the RFE resampling summary), `variable_importance` (the
#'         `caret::varImp()` data.frame), `preprocessor` (the `dummyVars`
#'         encoder, or `NULL`), `train_index` and `test_index` (row indices of
#'         the two partitions), `final_model_variables` (predictors the final
#'         model actually used after NZV/linear-combination filtering, or
#'         `NULL`) and `n_features` (candidate predictors offered to RFE).}
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
#'   res$selected
#'   res$details$test_metrics
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
  if (!is.null(sizes) && !is.numeric(sizes)) {
    stop("'sizes' must be a numeric vector or NULL.", call. = FALSE)
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

  y_raw <- data[[target]]
  task <- rfe_task_type(y_raw)

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

  # Coerce character/logical predictors to factor (levels from the training
  # rows) and align the test columns to those levels, so RFE, prediction, and
  # final training all see consistent types.
  for (nm in setdiff(colnames(train_df), target)) {
    if (is.character(train_df[[nm]]) || is.logical(train_df[[nm]])) {
      train_df[[nm]] <- factor(train_df[[nm]])
    }
    if (is.factor(train_df[[nm]])) {
      test_df[[nm]] <- factor(test_df[[nm]], levels = levels(train_df[[nm]]))
    }
  }

  n_features <- length(setdiff(colnames(train_df), target))

  cl <- rfe_start_parallel(parallel)
  on.exit(rfe_stop_parallel(cl), add = TRUE)

  rfe_message("Running recursive feature elimination on the training rows...",
              verbose)
  rfe_obj <- rfe_perform(
    train_df = train_df,
    target = target,
    sizes = sizes,
    rfe_control_params = rfe_control,
    parallel = parallel,
    verbose = verbose
  )

  # ---- Evaluate on the held-out test rows ------------------------------------
  test_x <- test_df[, setdiff(colnames(test_df), target), drop = FALSE]
  preds <- stats::predict(rfe_obj, test_x)
  if (is.data.frame(preds)) {
    preds <- if ("pred" %in% colnames(preds)) preds[["pred"]] else preds[[1L]]
  }

  test_y <- test_df[[target]]
  if (task == "classification") {
    fs_require("e1071", "classification test metrics (caret::postResample)")
    test_y <- as.factor(test_y)
  } else {
    test_y <- as.numeric(test_y)
  }
  test_metrics <- caret::postResample(preds, test_y)

  optimal_size <- rfe_obj$optsize
  optimal_vars <- rfe_obj$optVariables
  var_imp <- caret::varImp(rfe_obj)
  resamp <- rfe_obj$results

  final_model <- NULL
  final_model_vars <- NULL

  if (return_final_model) {
    # Train on the TRAINING rows only, so the test metrics stay honest.
    rfe_message("Training the final model on the training rows only...",
                verbose)
    final_model <- rfe_train_final(
      data_df = train_df,
      target = target,
      optimal_vars = optimal_vars,
      train_control_params = train_control,
      model_method = model_method
    )
    final_model_vars <- attr(final_model, "predictors_used")
  }

  new_fs_result(
    selected = as.character(optimal_vars),
    scores   = rfe_importance_scores(var_imp),
    method   = "rfe",
    task     = task,
    model    = if (return_final_model) final_model else rfe_obj,
    details  = list(
      rfe                   = rfe_obj,
      optimal_size          = optimal_size,
      test_metrics          = test_metrics,
      resampling_results    = resamp,
      variable_importance   = var_imp,
      preprocessor          = preproc,
      train_index           = train_idx,
      test_index            = test_idx,
      final_model_variables = final_model_vars,
      n_features            = n_features
    ),
    call = cl_call
  )
}
