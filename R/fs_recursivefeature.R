# Recursive feature elimination (caret-based) for featR.

# -----------------------------
# Validation and small utilities
# -----------------------------

#' Resolve the response column name
#'
#' Converts a response specification (column name or single column index) to a
#' column name present in `data`.
#'
#' @param data A data.frame.
#' @param response_var A single column name (character) or column index.
#' @return A single character string with the resolved column name.
#' @noRd
rfe_response_name <- function(data, response_var) {
  if (is.numeric(response_var)) {
    if (length(response_var) != 1L || !is.finite(response_var) ||
        response_var != as.integer(response_var)) {
      stop("'response_var' must be a single finite integer index or a column name.",
           call. = FALSE)
    }
    response_var <- as.integer(response_var)
    if (response_var < 1L || response_var > ncol(data)) {
      stop("'response_var' index is out of bounds.", call. = FALSE)
    }
    return(colnames(data)[response_var])
  }
  if (is.character(response_var) && length(response_var) == 1L &&
      !is.na(response_var)) {
    if (!response_var %in% colnames(data)) {
      stop("'response_var' name not found in 'data'.", call. = FALSE)
    }
    return(response_var)
  }
  stop("'response_var' must be a single column name (character) or a single column index (integer).",
       call. = FALSE)
}

#' Infer task type from a response vector
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

# -----------------------------
# Encoding (train-fitted, applied to others)
# -----------------------------

#' Fit a one-hot encoder on training predictors
#'
#' Fits a `caret::dummyVars()` transformer on the training predictors only
#' (the response is excluded). Uses `fullRank = TRUE` so downstream models do
#' not receive a rank-deficient all-levels design.
#'
#' @param train_df Training data.frame.
#' @param response_name Response column name.
#' @return A fitted `dummyVars` object.
#' @noRd
rfe_fit_encoder <- function(train_df, response_name) {
  predictors <- train_df[, setdiff(colnames(train_df), response_name),
                         drop = FALSE]
  caret::dummyVars(~ ., data = predictors, fullRank = TRUE)
}

#' Apply a fitted one-hot encoder and reattach the response
#'
#' @param data_df data.frame to transform.
#' @param response_name Response column name.
#' @param dv Fitted `dummyVars` object.
#' @return A data.frame with the response first, then encoded predictors.
#' @noRd
rfe_apply_encoder <- function(data_df, response_name, dv) {
  predictors <- data_df[, setdiff(colnames(data_df), response_name),
                        drop = FALSE]
  X <- as.data.frame(stats::predict(dv, newdata = predictors))
  if (nrow(X) != nrow(data_df)) {
    stop(sprintf(paste(
      "One-hot encoding returned %d row(s) for %d input row(s).",
      "This usually means missing values or factor levels unseen in training;",
      "clean or impute the data before calling fs_recursivefeature()."
    ), nrow(X), nrow(data_df)), call. = FALSE)
  }
  data.frame(
    stats::setNames(list(data_df[[response_name]]), response_name),
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
#' @param train_df Training data.frame containing the response.
#' @param response_name Response column name.
#' @param sizes Numeric vector of subset sizes, or NULL for `1:ncol(X)`.
#' @param rfe_control_params List for `caret::rfeControl()` (method/number at
#'   least). `functions`/`allowParallel` entries are dropped with a warning.
#' @param feature_funcs caret RFE function set; default `caret::rfFuncs`.
#' @param parallel Logical; passed to `rfeControl(allowParallel = )`.
#' @return A caret `rfe` object.
#' @noRd
rfe_perform <- function(train_df, response_name, sizes, rfe_control_params,
                        feature_funcs = NULL, parallel = FALSE) {
  rfe_validate_rfe_control(rfe_control_params)

  train_df <- as.data.frame(train_df)
  if (!response_name %in% colnames(train_df)) {
    stop("Response column not found in the training data.", call. = FALSE)
  }

  if (is.null(feature_funcs)) {
    feature_funcs <- caret::rfFuncs
  }

  # rfeControl() cannot receive these twice; drop user-supplied duplicates.
  for (nm in c("functions", "allowParallel")) {
    if (nm %in% names(rfe_control_params)) {
      warning(sprintf("'rfe_control$%s' is ignored; use the '%s' argument of fs_recursivefeature() instead.",
                      nm, if (nm == "functions") "feature_funcs" else "parallel"),
              call. = FALSE)
      rfe_control_params[[nm]] <- NULL
    }
  }

  ctrl <- do.call(
    caret::rfeControl,
    c(
      list(functions = feature_funcs, allowParallel = parallel),
      rfe_control_params
    )
  )

  X <- train_df[, setdiff(colnames(train_df), response_name), drop = FALSE]

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

  y_vec <- train_df[[response_name]]
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
#' @param data_df data.frame with the response and predictors (training rows).
#' @param response_name Response column name.
#' @param optimal_vars Character vector of selected predictor names.
#' @param train_control_params List of `caret::trainControl()` arguments.
#' @param model_method caret model key (e.g. "rf", "lm").
#' @return A `caret::train` object; the predictors actually used (after NZV
#'   and linear-combination filtering) are stored in
#'   `attr(model, "predictors_used")`.
#' @noRd
rfe_train_final <- function(data_df, response_name, optimal_vars,
                            train_control_params = list(method = "cv",
                                                        number = 5),
                            model_method = "rf") {
  rfe_validate_train_control(train_control_params)

  missing_vars <- setdiff(optimal_vars, colnames(data_df))
  if (length(missing_vars) > 0L) {
    stop("The following selected predictors are missing from the data: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  df <- data_df[, c(response_name, optimal_vars), drop = FALSE]

  # Coerce the response appropriately.
  task <- rfe_task_type(df[[response_name]])
  if (task == "classification") {
    df[[response_name]] <- as.factor(df[[response_name]])
  } else {
    df[[response_name]] <- as.numeric(df[[response_name]])
  }

  # Reasonable predictor types: logical -> integer, character -> factor.
  pred_names <- setdiff(colnames(df), response_name)
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
      df <- df[, c(response_name, pred_names), drop = FALSE]
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
        df <- df[, c(response_name, pred_names), drop = FALSE]
      }
    }
  }

  if (length(pred_names) == 0L) {
    stop("No predictors remain after preprocessing for the final model.",
         call. = FALSE)
  }

  tr_ctrl <- do.call(caret::trainControl, train_control_params)
  form <- stats::as.formula(paste(backtick(response_name), "~ ."))

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
#' Splits the data into stratified train/test partitions (80/20), optionally
#' one-hot encodes the predictors (encoder fitted on the training rows only),
#' runs `caret::rfe()` on the training set, evaluates the selected feature set
#' on the held-out test rows, and optionally trains a final caret model on the
#' training rows.
#'
#' @details
#' Requires the suggested package 'caret'. The default `feature_funcs`
#' (`caret::rfFuncs`) fits random forests, so the suggested package
#' 'randomForest' must also be installed unless you supply a different
#' function set. Parallel execution additionally requires 'foreach' and
#' 'doParallel'; classification metrics use `caret::postResample()`, which
#' needs 'e1071'.
#'
#' `TestMetrics` is computed by predicting on the held-out test rows with the
#' fitted RFE model and summarizing with `caret::postResample()`. When
#' `return_final_model = TRUE`, the final model is trained on the
#' \emph{training rows only} (not the full data), so `TestMetrics` remains an
#' honest estimate. Predictors containing missing values are rejected; impute
#' before calling.
#'
#' @param data A data.frame with the response and predictors.
#' @param response_var Response column name (character) or a single column
#'   index (numeric).
#' @param seed Optional integer seed. Applied for the duration of the call
#'   only (previous RNG state is restored on exit); default `NULL` never
#'   seeds.
#' @param rfe_control List of arguments for `caret::rfeControl()`; must
#'   contain at least `method` and `number`. Any `functions` or
#'   `allowParallel` entries are dropped with a warning (use `feature_funcs`
#'   and `parallel` instead).
#' @param train_control List of arguments for `caret::trainControl()` used
#'   when `return_final_model = TRUE`.
#' @param sizes Numeric vector of feature-subset sizes to evaluate; `NULL`
#'   uses `1:p`. Out-of-range values are dropped with a warning.
#' @param parallel Logical. If `TRUE`, registers a two-worker PSOCK cluster
#'   (capped at the available cores) for the duration of the call; requires
#'   the suggested packages 'foreach' and 'doParallel'.
#' @param feature_funcs A caret RFE function set (e.g. `caret::rfFuncs`,
#'   `caret::lmFuncs`). `NULL` uses `caret::rfFuncs`.
#' @param handle_categorical Logical; one-hot encode predictors with
#'   full-rank dummies (fitted on the training rows, applied to the test
#'   rows).
#' @param return_final_model Logical; train a final caret model on the
#'   training rows using the selected features.
#' @param model_method caret model key for the final model (e.g. `"rf"`,
#'   `"lm"`).
#'
#' @return A list with components:
#' \describe{
#'   \item{ResponseName}{Resolved response column name.}
#'   \item{TaskType}{`"classification"` or `"regression"`.}
#'   \item{TrainIndex}{Row indices of the training partition.}
#'   \item{TestIndex}{Row indices of the testing partition.}
#'   \item{Preprocessor}{`dummyVars` encoder used, if any; else `NULL`.}
#'   \item{RFE}{The `rfe` object returned by caret.}
#'   \item{OptimalNumberOfVariables}{Optimal subset size selected by RFE.}
#'   \item{OptimalVariables}{Names of the selected variables.}
#'   \item{VariableImportance}{Aggregated variable importance from
#'     `caret::varImp()` on the RFE object (one row per variable), not the
#'     raw per-resample table.}
#'   \item{ResamplingResults}{Resampling performance summary from RFE.}
#'   \item{TestMetrics}{`caret::postResample()` metrics of the RFE model on
#'     the held-out test rows.}
#'   \item{FinalModel}{`caret::train` object trained on the training rows if
#'     `return_final_model = TRUE`; else `NULL`.}
#'   \item{FinalModelVariables}{Predictors actually used by the final model
#'     (after NZV/linear-combination filtering), or `NULL`.}
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("caret", quietly = TRUE) &&
#'     requireNamespace("randomForest", quietly = TRUE) &&
#'     requireNamespace("e1071", quietly = TRUE)) {
#'   res <- fs_recursivefeature(
#'     iris,
#'     response_var = "Species",
#'     seed = 42,
#'     rfe_control = list(method = "cv", number = 3),
#'     sizes = 1:4
#'   )
#'   res$OptimalVariables
#'   res$TestMetrics
#' }
#' }
#' @export
fs_recursivefeature <- function(data, response_var,
                                seed = NULL,
                                rfe_control = list(method = "cv", number = 5),
                                train_control = list(method = "cv",
                                                     number = 5),
                                sizes = NULL,
                                parallel = FALSE,
                                feature_funcs = NULL,
                                handle_categorical = FALSE,
                                return_final_model = FALSE,
                                model_method = "rf") {
  fs_require("caret", "recursive feature elimination")

  assert_data_frame(data, "data")
  # Plain data.frame semantics for all subsetting below (a data.table input
  # would otherwise dispatch to NSE-based [.data.table).
  data <- as.data.frame(data)
  assert_flag(parallel, "parallel")
  assert_flag(handle_categorical, "handle_categorical")
  assert_flag(return_final_model, "return_final_model")
  assert_string(model_method, "model_method")
  if (!is.null(sizes) && !is.numeric(sizes)) {
    stop("'sizes' must be a numeric vector or NULL.", call. = FALSE)
  }

  y_name <- rfe_response_name(data, response_var)
  y_raw <- data[[y_name]]
  task <- rfe_task_type(y_raw)

  # Seed applies to the split and to RFE resampling; restored on exit.
  local_seed(seed)

  y_part <- if (task == "classification") as.factor(y_raw) else as.numeric(y_raw)
  train_idx <- fs_split_index(y_part, p = 0.8)
  test_idx <- setdiff(seq_len(nrow(data)), train_idx)
  train_raw <- data[train_idx, , drop = FALSE]
  test_raw <- data[test_idx, , drop = FALSE]

  preproc <- NULL
  if (handle_categorical) {
    preproc <- rfe_fit_encoder(train_raw, response_name = y_name)
    train_df <- rfe_apply_encoder(train_raw, response_name = y_name,
                                  dv = preproc)
    test_df <- rfe_apply_encoder(test_raw, response_name = y_name,
                                 dv = preproc)
  } else {
    pred_names <- setdiff(colnames(train_raw), y_name)
    train_df <- train_raw[, c(y_name, pred_names), drop = FALSE]
    test_df <- test_raw[, c(y_name, pred_names), drop = FALSE]
  }

  # Coerce character/logical predictors to factor (levels from the training
  # rows) and align the test columns to those levels, so RFE, prediction, and
  # final training all see consistent types.
  for (nm in setdiff(colnames(train_df), y_name)) {
    if (is.character(train_df[[nm]]) || is.logical(train_df[[nm]])) {
      train_df[[nm]] <- factor(train_df[[nm]])
    }
    if (is.factor(train_df[[nm]])) {
      test_df[[nm]] <- factor(test_df[[nm]], levels = levels(train_df[[nm]]))
    }
  }

  cl <- rfe_start_parallel(parallel)
  on.exit(rfe_stop_parallel(cl), add = TRUE)

  rfe_obj <- rfe_perform(
    train_df = train_df,
    response_name = y_name,
    sizes = sizes,
    rfe_control_params = rfe_control,
    feature_funcs = feature_funcs,
    parallel = parallel
  )

  # ---- Evaluate on the held-out test rows ------------------------------------
  test_x <- test_df[, setdiff(colnames(test_df), y_name), drop = FALSE]
  preds <- stats::predict(rfe_obj, test_x)
  if (is.data.frame(preds)) {
    preds <- if ("pred" %in% colnames(preds)) preds[["pred"]] else preds[[1L]]
  }

  test_y <- test_df[[y_name]]
  if (task == "classification") {
    fs_require("e1071", "classification test metrics (caret::postResample)")
    test_y <- as.factor(test_y)
  } else {
    test_y <- as.numeric(test_y)
  }
  test_metrics <- caret::postResample(preds, test_y)

  optimal_num <- rfe_obj$optsize
  optimal_vars <- rfe_obj$optVariables
  var_imp <- caret::varImp(rfe_obj)
  resamp <- rfe_obj$results

  final_model <- NULL
  final_model_vars <- NULL

  if (return_final_model) {
    # Train on the TRAINING rows only, so TestMetrics stays honest.
    final_model <- rfe_train_final(
      data_df = train_df,
      response_name = y_name,
      optimal_vars = optimal_vars,
      train_control_params = train_control,
      model_method = model_method
    )
    final_model_vars <- attr(final_model, "predictors_used")
  }

  list(
    ResponseName = y_name,
    TaskType = task,
    TrainIndex = train_idx,
    TestIndex = test_idx,
    Preprocessor = preproc,
    RFE = rfe_obj,
    OptimalNumberOfVariables = optimal_num,
    OptimalVariables = optimal_vars,
    VariableImportance = var_imp,
    ResamplingResults = resamp,
    TestMetrics = test_metrics,
    FinalModel = final_model,
    FinalModelVariables = final_model_vars
  )
}
