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

#' Start a doParallel backend for RFE
#'
#' Registers a two-worker PSOCK cluster (capped at the detected core count via
#' `resolve_cores()`). featR never grabs all available cores.
#'
#' When `seed` is supplied, the workers are given reproducible L'Ecuyer-CMRG
#' streams, so two seeded parallel runs agree with each other.
#'
#' @param enable Logical; return NULL without side effects when FALSE.
#' @param seed Optional seed used to set the workers' RNG streams.
#' @return The cluster object, or NULL.
#' @noRd
rfe_start_parallel <- function(enable, seed = NULL) {
  if (!isTRUE(enable)) {
    return(NULL)
  }
  fs_require(c("foreach", "doParallel"), "parallel RFE")
  cores <- resolve_cores(2L)
  cl <- parallel::makeCluster(cores)
  # Stop the cluster ourselves if registration fails: it has not reached the
  # caller's on.exit() yet, so it would otherwise leak connections.
  ok <- FALSE
  on.exit(if (!ok) try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  if (!is.null(seed)) {
    parallel::clusterSetRNGStream(cl, iseed = as.integer(seed))
  }
  doParallel::registerDoParallel(cl)
  ok <- TRUE
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
#' `rfe_control = list(method = "cv", number = 5, functions = caret::lmFuncs)`).
#' Parallel execution additionally requires 'foreach' and 'doParallel';
#' classification metrics use `caret::postResample()`, which needs 'e1071'.
#'
#' Everything that could leak is fitted on the training rows: the one-hot
#' encoder, the factor levels the test columns are aligned to, the elimination
#' itself, and the final model. `details$test_metrics` is therefore a genuine
#' held-out estimate. It applies `caret::postResample()` to the predictions of
#' the fitted `rfe` object (caret's own refit on all the training rows,
#' restricted to the optimal subset) on test rows that took no part in choosing
#' either the features or the subset size.
#'
#' Two other summaries on the result are \emph{not} held-out estimates, by
#' construction. `details$resampling_results` summarizes resampling performed
#' inside the training rows across candidate subset sizes, and, when
#' `return_final_model = TRUE`, `model$results` reports `train_control`
#' resampling inside those same training rows on features that were already
#' selected. Only `details$test_metrics` is computed on data the search never
#' saw.
#'
#' Missing values in the \emph{training} predictors are rejected with an error;
#' impute or drop incomplete rows before calling. NAs in the held-out rows are
#' not checked here and will propagate through `predict()` into
#' `details$test_metrics`. With `handle_categorical = TRUE` the encoded test
#' rows are row-count checked against their input, so an encoding that quietly
#' loses rows becomes an error rather than a silently misaligned metric.
#'
#' @param data A data.frame or data.table with at least one row and one column,
#'   holding the target and the candidate predictors (every other column). It
#'   is converted to a plain data.frame on entry.
#' @param target Single string naming the target column of `data`; a column
#'   index is not accepted. A factor, character, or logical target means
#'   classification, anything else regression.
#' @param sizes Numeric vector of feature-subset sizes to evaluate. Default
#'   `NULL`, which uses `1:p`, where `p` is the predictor count after any
#'   one-hot encoding. Values outside `[1, p]` are dropped with a warning; if
#'   that leaves nothing, the call is an error rather than a silent empty run.
#' @param train_ratio Numeric, strictly between 0 and 1: the training
#'   proportion of the stratified split. Default 0.8.
#' @param rfe_control List of arguments for `caret::rfeControl()`; must contain
#'   at least `method` and `number`. Default `list(method = "cv", number = 5)`.
#'   `functions` selects the caret RFE function set and defaults to
#'   `caret::rfFuncs`. An `allowParallel` entry is dropped with a warning (use
#'   the `parallel` argument instead), while a `verbose` entry, if present,
#'   overrides the `verbose` argument for `caret::rfe()`. With
#'   `method = "repeatedcv"` and no `repeats`, caret's default of 1 is used and
#'   a warning says so.
#' @param train_control List of arguments for `caret::trainControl()`, used
#'   only when `return_final_model = TRUE`. Must contain `method`, and also
#'   `number` unless `method = "none"`. Default
#'   `list(method = "cv", number = 5)`.
#' @param model_method Single string; the caret model key used for the final
#'   model, for example `"rf"` or `"lm"`. Used only when
#'   `return_final_model = TRUE`. Default `"rf"`.
#' @param handle_categorical Logical; one-hot encode predictors with full-rank
#'   dummies (fitted on the training rows, applied to the test rows).
#'   Default `FALSE`.
#' @param return_final_model Logical; train a final caret model on the training
#'   rows using the selected features and return it as `model`, with the `rfe`
#'   object still available in `details$rfe`. Near-zero-variance and linearly
#'   dependent predictors are dropped from the selected set first, each with a
#'   warning, and what survives is recorded in `details$final_model_variables`.
#'   Default `FALSE`.
#' @param seed Optional single finite number, truncated to an integer with
#'   `as.integer()`. It covers the split and the RFE resampling, is applied for
#'   the duration of the call only (the previous RNG state is restored on
#'   exit), and defaults to `NULL`, which never seeds. Under
#'   `parallel = TRUE` the seed is also used to set reproducible
#'   L'Ecuyer-CMRG streams on the workers, so two seeded parallel runs agree
#'   with each other; because the workers draw from their own streams, a
#'   seeded parallel run need not equal a seeded sequential one.
#' @param verbose Logical; print progress messages and let `caret::rfe()`
#'   report its own progress, unless `rfe_control$verbose` overrides the
#'   latter. Default `FALSE`.
#' @param parallel Logical. If `TRUE`, registers a two-worker PSOCK cluster
#'   (capped at the detected core count) for the duration of the call and stops
#'   it again when the call returns; requires the suggested packages 'foreach'
#'   and 'doParallel'. Default `FALSE`.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector of the variables RFE kept
#'         (`optVariables` at the optimal subset size).}
#'   \item{scores}{Named numeric vector of resample-averaged importance from
#'         `caret::varImp()` on the `rfe` object: its "Overall" column when
#'         caret supplies one, otherwise the row means of whatever numeric
#'         columns it did supply. `NULL` when there is nothing usable.}
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
#'         `variable_importance` (the `caret::varImp()` data.frame),
#'         `preprocessor` (the `dummyVars` encoder, or `NULL` when
#'         `handle_categorical = FALSE`), `train_index` and `test_index` (row
#'         indices into `data` for the two partitions), `final_model_variables`
#'         (predictors the final model actually used after
#'         NZV/linear-combination filtering, or `NULL` when no final model was
#'         requested) and `n_features` (candidate predictors offered to RFE,
#'         counted after one-hot encoding when `handle_categorical = TRUE`).}
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
  for (nm in setdiff(colnames(train_raw), target)) {
    if (is.character(train_raw[[nm]]) || is.logical(train_raw[[nm]])) {
      train_raw[[nm]] <- factor(train_raw[[nm]])
    }
    if (is.factor(train_raw[[nm]])) {
      test_raw[[nm]] <- factor(test_raw[[nm]],
                               levels = levels(train_raw[[nm]]))
    }
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

  n_features <- length(setdiff(colnames(train_df), target))

  cl <- rfe_start_parallel(parallel, seed = seed)
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
    # Train on the TRAINING rows only, so the held-out rows stay unseen by
    # every model this function returns.
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
