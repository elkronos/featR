# ============================================================
# Feature Selection & Modeling Utilities (caret-based)
# ============================================================
# Depends on: caret, doParallel, parallel, foreach (via caret)
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(caret)
  library(doParallel)
  library(parallel)
})

# -----------------------------
# Validation and small utilities
# -----------------------------

#' Resolve the Response Column Name
#'
#' Converts a response specification (column name or column index) to a valid
#' column name present in `data`.
#'
#' @param data A `data.frame` containing the dataset.
#' @param response_var A single column name (`character`) or column index (`numeric` of length 1)
#'   indicating the response variable.
#'
#' @return A single `character` string with the resolved response column name.
#'
#' @keywords internal
.resolve_response_name <- function(data, response_var) {
  if (is.numeric(response_var)) {
    if (length(response_var) != 1L) {
      stop("response_var must be a single column index (integer).")
    }
    if (!is.finite(response_var) || response_var != as.integer(response_var)) {
      stop("response_var must be a finite integer index.")
    }
    response_var <- as.integer(response_var)
    if (response_var < 1L || response_var > ncol(data)) {
      stop("response_var index is out of bounds.")
    }
    return(colnames(data)[response_var])
  } else if (is.character(response_var) && length(response_var) == 1L) {
    if (!response_var %in% colnames(data)) {
      stop("response_var name not found in data.")
    }
    return(response_var)
  } else {
    stop("response_var must be a single column name (character) or a single column index (integer).")
  }
}

#' Infer Task Type from Response
#'
#' Determines whether a supervised learning task is classification or regression
#' based on the response vector.
#'
#' @param y A response vector.
#'   Factors, character vectors, and logicals imply classification;
#'   all other types imply regression.
#'
#' @return A `character` scalar: either `"classification"` or `"regression"`.
#'
#' @keywords internal
.task_type <- function(y) {
  if (is.factor(y) || is.character(y) || is.logical(y)) {
    return("classification")
  }
  "regression"
}

# -----------------------------
# Control parameter validation
# -----------------------------

#' Validate RFE Control Parameters
#'
#' Validates the resampling controls provided for recursive feature elimination (RFE).
#'
#' @param control_params A `list` with at least elements
#'   \itemize{
#'     \item `method` - resampling method (character scalar).
#'     \item `number` - positive integer (e.g., number of folds or resamples).
#'   }
#'   Additional fields (e.g., `repeats`) are allowed and passed through.
#'
#' @return Invisibly returns `TRUE` on success; otherwise throws an error.
#'
#' @export
validate_rfe_control <- function(control_params) {
  if (!is.list(control_params)) {
    stop("rfe control_params should be a list.")
  }
  if (!("method" %in% names(control_params))) {
    stop("rfe control_params must contain 'method'.")
  }
  if (!("number" %in% names(control_params))) {
    stop("rfe control_params must contain 'number'.")
  }
  
  method <- control_params$method
  number <- control_params$number
  
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("rfe control_params$method must be a non-NA character scalar.")
  }
  
  if (!is.numeric(number) || length(number) != 1L ||
      !is.finite(number) || number <= 0L || number != as.integer(number)) {
    stop("rfe control_params$number must be a positive integer.")
  }
  
  # Optional check for repeats when using repeated CV
  if (identical(method, "repeatedcv")) {
    if (!("repeats" %in% names(control_params))) {
      warning("rfe control_params$repeats is not provided for method='repeatedcv'; default of 1 will be used by caret.")
    } else {
      repeats <- control_params$repeats
      if (!is.numeric(repeats) || length(repeats) != 1L ||
          !is.finite(repeats) || repeats <= 0L || repeats != as.integer(repeats)) {
        stop("rfe control_params$repeats must be a positive integer when method='repeatedcv'.")
      }
    }
  }
  
  invisible(TRUE)
}

#' Validate Training Control Parameters
#'
#' Validates the resampling controls provided for final model training.
#'
#' This function accepts either a `list` of arguments for `caret::trainControl`
#' or an already constructed `trainControl` object.
#'
#' @param control_params Either:
#'   \itemize{
#'     \item A `trainControl` object, in which case it is accepted as-is, or
#'     \item A `list` with at least `method` and, when `method != "none"`,
#'       a positive integer `number`. Additional trainControl arguments are allowed
#'       and passed through unchanged.
#'   }
#'
#' @return Invisibly returns `TRUE` on success; otherwise throws an error.
#'
#' @export
validate_train_control <- function(control_params) {
  # If the user passes a trainControl object, accept it directly
  if (inherits(control_params, "trainControl")) {
    return(invisible(TRUE))
  }
  
  if (!is.list(control_params)) {
    stop("train control_params should be a list or a trainControl object.")
  }
  
  if (!("method" %in% names(control_params))) {
    stop("train control_params must contain at least 'method'.")
  }
  
  method <- control_params$method
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("train control_params$method must be a non-NA character scalar.")
  }
  
  if (!identical(method, "none")) {
    if (!("number" %in% names(control_params))) {
      stop("When training control method is not 'none', provide 'number'.")
    }
    number <- control_params$number
    if (!is.numeric(number) || length(number) != 1L ||
        !is.finite(number) || number <= 0L || number != as.integer(number)) {
      stop("train control_params$number must be a positive integer.")
    }
  }
  
  # Optional: if repeatedcv, check repeats
  if (identical(method, "repeatedcv") && "repeats" %in% names(control_params)) {
    repeats <- control_params$repeats
    if (!is.numeric(repeats) || length(repeats) != 1L ||
        !is.finite(repeats) || repeats <= 0L || repeats != as.integer(repeats)) {
      stop("train control_params$repeats must be a positive integer when method='repeatedcv'.")
    }
  }
  
  invisible(TRUE)
}

# -----------------------------
# Data splitting
# -----------------------------

#' Train/Test Split (80/20 by default)
#'
#' Splits a dataset into training and testing partitions. For classification tasks,
#' the split is stratified by the response using `caret::createDataPartition`.
#'
#' @param data A `data.frame` containing predictors and response.
#' @param response_var A response column name (`character`) or index (`numeric` of length 1).
#' @param seed An `integer` seed for reproducibility. Default: `123`.
#' @param p Proportion for the training set (`0 < p < 1`). Default: `0.8`.
#'
#' @return A `list` with elements:
#' \describe{
#'   \item{train}{Training `data.frame`.}
#'   \item{test}{Testing `data.frame`.}
#'   \item{response_name}{The resolved response column name.}
#'   \item{TrainIndex}{Row indices (1-based) used for the training partition.}
#'   \item{TestIndex}{Row indices (1-based) used for the testing partition.}
#' }
#'
#' @export
split_data <- function(data, response_var, seed = 123, p = 0.8) {
  if (!is.numeric(p) || length(p) != 1L || !is.finite(p) || p <= 0 || p >= 1) {
    stop("p must be a numeric scalar with 0 < p < 1.")
  }
  if (!is.data.frame(data)) {
    stop("split_data: 'data' must be a data.frame.")
  }
  
  set.seed(seed)
  y_name <- .resolve_response_name(data, response_var)
  y <- data[[y_name]]
  task <- .task_type(y)
  
  message(sprintf(
    "DEBUG[split_data]: n=%d | p=%.3f | response=%s | task=%s",
    nrow(data), p, y_name, task
  ))
  
  # Prepare y for createDataPartition
  if (task == "classification") {
    y_part <- as.factor(y)
  } else {
    y_part <- as.numeric(y)
  }
  
  idx <- caret::createDataPartition(y_part, p = p, list = FALSE)
  idx <- as.vector(idx)
  
  all_rows <- seq_len(nrow(data))
  test_idx <- setdiff(all_rows, idx)
  
  train_df <- data[idx, , drop = FALSE]
  test_df  <- data[test_idx, , drop = FALSE]
  
  message(sprintf(
    "DEBUG[split_data]: train_n=%d | test_n=%d | min(idx)=%d | max(idx)=%d",
    nrow(train_df), nrow(test_df), min(idx), max(idx)
  ))
  message(sprintf(
    "DEBUG[split_data]: train_cols={%s} | test_cols={%s}",
    paste(colnames(train_df), collapse = ", "),
    paste(colnames(test_df),  collapse = ", ")
  ))
  
  # Self-check for debugging (note: for stratified classification, counts may differ by 1)
  expected_train <- floor(p * nrow(data))
  expected_test  <- nrow(data) - expected_train
  sizes_ok <- (nrow(train_df) == expected_train) && (nrow(test_df) == expected_test)
  align_ok <- identical(data[idx, , drop = FALSE], train_df) &&
    identical(data[test_idx, , drop = FALSE], test_df)
  message(sprintf(
    "DEBUG[split_data:selfcheck]: sizes_ok=%s | align_ok=%s | expected_train=%d | actual_train=%d | expected_test=%d | actual_test=%d",
    sizes_ok, align_ok, expected_train, nrow(train_df), expected_test, nrow(test_df)
  ))
  
  list(
    train         = train_df,
    test          = test_df,
    response_name = y_name,
    TrainIndex    = idx,
    TestIndex     = test_idx
  )
}

# -----------------------------
# Encoding (train-fitted, applied to others)
# -----------------------------

#' Fit One-Hot Encoder on Training Predictors
#'
#' Fits a `caret::dummyVars` transformer on the training predictors only (response is excluded).
#'
#' @param train_df Training `data.frame`.
#' @param response_name A single `character` column name of the response.
#'
#' @return A `dummyVars` object fitted on training predictors.
#'
#' @export
fit_one_hot_encoder <- function(train_df, response_name) {
  if (!is.data.frame(train_df)) {
    stop("fit_one_hot_encoder: train_df must be a data.frame.")
  }
  if (!response_name %in% colnames(train_df)) {
    stop("fit_one_hot_encoder: response_name not found in train_df.")
  }
  predictors <- train_df[, setdiff(colnames(train_df), response_name), drop = FALSE]
  caret::dummyVars(~ ., data = predictors)
}

#' Apply One-Hot Encoder to a Dataset
#'
#' Applies a previously fitted `dummyVars` encoder to predictors of a dataset and reattaches the response as the first column.
#'
#' @param data_df A `data.frame` to transform.
#' @param response_name A single `character` column name of the response.
#' @param dv A fitted `caret::dummyVars` object.
#'
#' @return A `data.frame` with the response as the first column, followed by encoded predictors.
#'
#' @export
apply_one_hot_encoder <- function(data_df, response_name, dv) {
  if (!is.data.frame(data_df)) {
    stop("apply_one_hot_encoder: data_df must be a data.frame.")
  }
  if (!response_name %in% colnames(data_df)) {
    stop("apply_one_hot_encoder: response_name not found in data_df.")
  }
  predictors <- data_df[, setdiff(colnames(data_df), response_name), drop = FALSE]
  X <- as.data.frame(predict(dv, newdata = predictors))
  data.frame(
    setNames(list(data_df[[response_name]]), response_name),
    X,
    check.names = FALSE
  )
}

# -----------------------------
# Parallel backend helpers
# -----------------------------

#' Start Parallel Backend
#'
#' Creates and registers a PSOCK cluster using `doParallel` if `enable = TRUE`.
#'
#' @param enable `logical`. If `TRUE`, start a cluster; otherwise return `NULL`.
#'
#' @return A cluster object (from `parallel::makeCluster`) if started; otherwise `NULL`.
#'
#' @keywords internal
.start_parallel <- function(enable) {
  if (!enable) return(NULL)
  cores <- parallel::detectCores()
  if (!is.numeric(cores) || length(cores) != 1L || !is.finite(cores)) {
    cores <- 2L
  }
  cores <- max(1L, cores - 1L)
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  message(sprintf("DEBUG[parallel]: started PSOCK cluster with %d workers", cores))
  cl
}

#' Stop Parallel Backend
#'
#' Stops a previously started PSOCK cluster if not `NULL` and resets to sequential execution.
#'
#' @param cl A cluster object or `NULL`.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
.stop_parallel <- function(cl) {
  if (!is.null(cl)) {
    message("DEBUG[parallel]: stopping PSOCK cluster and returning to sequential execution")
    parallel::stopCluster(cl)
  } else {
    message("DEBUG[parallel]: no cluster to stop, ensuring sequential backend")
  }
  # Ensure we return to sequential backend using foreach's registerDoSEQ
  if (requireNamespace("foreach", quietly = TRUE)) {
    foreach::registerDoSEQ()
  } else {
    message("DEBUG[parallel]: 'foreach' namespace not available; cannot register sequential backend explicitly")
  }
  invisible(NULL)
}

# -----------------------------
# RFE (Recursive Feature Elimination)
# -----------------------------

#' Perform Recursive Feature Elimination
#'
#' Runs recursive feature elimination (RFE) on a preprocessed training dataset.
#'
#' @param train_df A `data.frame` where the first column is the response and remaining columns
#'   are predictors (or ensure `response_name` is provided to select the response).
#' @param response_name A single `character` name of the response column in `train_df`.
#' @param sizes A numeric vector of feature subset sizes to evaluate. If `NULL`, uses `1:ncol(X)`.
#' @param rfe_control_params A `list` with at least `method` and `number` for resampling control.
#' @param feature_funcs A caret RFE function set (e.g., `caret::rfFuncs`). If `NULL`, defaults to `caret::rfFuncs`.
#' @param parallel `logical`. Allow parallel evaluation.
#' @param early_stop `logical`. If `TRUE`, sets `returnResamp = "final"` and `saveDetails = TRUE`.
#'
#' @return An object of class `rfe` from `caret`, containing RFE results.
#'
#' @export
perform_rfe <- function(train_df, response_name, sizes, rfe_control_params,
                        feature_funcs = NULL, parallel = FALSE, early_stop = FALSE) {
  validate_rfe_control(rfe_control_params)
  
  # Defensive: ensure data.frame
  if (!is.data.frame(train_df)) {
    message(sprintf(
      "DEBUG[perform_rfe]: coercing train_df to data.frame; class was: %s",
      paste(class(train_df), collapse = "/")
    ))
    train_df <- as.data.frame(train_df)
  }
  
  if (!response_name %in% colnames(train_df)) {
    message("DEBUG[perform_rfe]: response_name not found in train_df.")
    message(sprintf("DEBUG[perform_rfe]: colnames(train_df) = %s",
                    paste(colnames(train_df), collapse = ", ")))
    stop("perform_rfe: response_name not found in train_df.")
  }
  
  y <- train_df[[response_name]]
  task <- .task_type(y)
  
  if (is.null(feature_funcs)) {
    feature_funcs <- caret::rfFuncs
  }
  
  # Build rfeControl, starting from user params
  ctrl <- do.call(
    caret::rfeControl,
    c(
      list(
        functions = feature_funcs,
        allowParallel = parallel
      ),
      rfe_control_params
    )
  )
  
  if (early_stop) {
    ctrl$returnResamp <- "final"
    ctrl$saveDetails  <- TRUE
  }
  
  X <- train_df[, setdiff(colnames(train_df), response_name), drop = FALSE]
  y_vec <- train_df[[response_name]]
  if (task == "classification" && !is.factor(y_vec)) {
    y_vec <- as.factor(y_vec)
  }
  
  if (is.null(sizes)) {
    sizes <- seq_len(ncol(X))
  } else {
    sizes <- sizes[sizes >= 1 & sizes <= ncol(X)]
    if (length(sizes) == 0L) {
      stop("No valid 'sizes' remain within the range of available predictors.")
    }
  }
  
  message(sprintf(
    "DEBUG[perform_rfe]: n=%d | p=%d | response=%s | predictors={%s} | sizes={%s}",
    nrow(X), ncol(X), response_name,
    paste(colnames(X), collapse = ", "),
    paste(sizes, collapse = ", ")
  ))
  
  rfe_profile <- tryCatch(
    {
      caret::rfe(x = X, y = y_vec, sizes = sizes, rfeControl = ctrl)
    },
    error = function(e) {
      stop("Error during RFE: ", e$message)
    }
  )
  
  rfe_profile
}

# -----------------------------
# Final model training
# -----------------------------

#' Train the Final Model
#'
#' Trains a model on a preprocessed dataset using a specified set of selected predictors.
#'
#' @param data_df A `data.frame` containing the response column and predictors.
#' @param response_name A single `character` name of the response column.
#' @param optimal_vars A `character` vector of selected predictor names.
#' @param train_control_params A `list` specifying training control or a `trainControl` object.
#' @param model_method A `character` string specifying the caret model key (e.g., `"rf"`, `"lm"`, `"xgbTree"`).
#'
#' @return A trained `caret::train` model object. The set of predictors actually used
#'   (after NZV and linear-combination filtering) is stored as
#'   `attr(model, "predictors_used")`.
#'
#' @export
train_final_model <- function(data_df, response_name, optimal_vars,
                              train_control_params = list(method = "cv", number = 5),
                              model_method = "rf") {
  validate_train_control(train_control_params)
  
  if (!is.data.frame(data_df)) {
    stop("train_final_model: data_df must be a data.frame.")
  }
  if (!response_name %in% colnames(data_df)) {
    stop("train_final_model: response_name not found in data_df.")
  }
  
  # Check requested predictors exist
  missing_vars <- setdiff(optimal_vars, colnames(data_df))
  if (length(missing_vars) > 0L) {
    stop("train_final_model: The following predictors are missing from data_df: ",
         paste(missing_vars, collapse = ", "))
  }
  
  # Build working data.frame
  df <- data_df[, c(response_name, optimal_vars), drop = FALSE]
  
  # Coerce response appropriately
  task <- .task_type(df[[response_name]])
  if (task == "classification") {
    df[[response_name]] <- as.factor(df[[response_name]])
  } else {
    df[[response_name]] <- as.numeric(df[[response_name]])
  }
  
  # Ensure predictors are in reasonable types:
  # - logical -> integer
  # - character -> factor
  pred_names <- setdiff(colnames(df), response_name)
  for (nm in pred_names) {
    if (is.logical(df[[nm]])) {
      df[[nm]] <- as.integer(df[[nm]])
    } else if (is.character(df[[nm]])) {
      df[[nm]] <- factor(df[[nm]])
    }
  }
  
  # Remove NZV predictors (handles numeric and factor/character)
  if (length(pred_names) > 0L) {
    nzv_idx <- caret::nearZeroVar(df[, pred_names, drop = FALSE], saveMetrics = FALSE)
    if (length(nzv_idx) > 0L) {
      removed <- pred_names[nzv_idx]
      message("DEBUG[nzv]: Removing near-zero variance predictors: ",
              paste(removed, collapse = ", "))
      pred_names <- setdiff(pred_names, removed)
      df <- df[, c(response_name, pred_names), drop = FALSE]
    }
  }
  
  # Remove linear combinations on numeric predictors only
  if (length(pred_names) > 1L) {
    X <- df[, pred_names, drop = FALSE]
    num_cols <- vapply(X, is.numeric, logical(1L))
    if (sum(num_cols) > 1L) {
      lc <- caret::findLinearCombos(as.matrix(X[, num_cols, drop = FALSE]))
      if (!is.null(lc$remove) && length(lc$remove) > 0L) {
        drop_lc <- colnames(X[, num_cols, drop = FALSE])[lc$remove]
        message("DEBUG[lincomb]: Removing linear-combination predictors: ",
                paste(drop_lc, collapse = ", "))
        pred_names <- setdiff(pred_names, drop_lc)
        df <- df[, c(response_name, pred_names), drop = FALSE]
      }
    }
  }
  
  if (length(pred_names) == 0L) {
    stop("train_final_model: No predictors remain after preprocessing.")
  }
  
  # TrainControl setup
  make_ctrl <- function(ctrl) {
    # Allow user-supplied trainControl object
    if (inherits(ctrl, "trainControl")) {
      return(ctrl)
    }
    # Otherwise, treat as argument list to trainControl
    do.call(caret::trainControl, ctrl)
  }
  
  tr_ctrl <- make_ctrl(train_control_params)
  
  # Formula interface
  form <- stats::as.formula(paste(response_name, "~ ."))
  
  # Function to attempt training (with full debug on error)
  attempt_train <- function(ctrl_used, tag = "primary") {
    message(sprintf(
      "DEBUG[train:%s]: method=%s | resampling=%s | n=%d | p=%d | response=%s | predictors=%s",
      tag, model_method, ctrl_used$method, nrow(df), length(pred_names), response_name,
      paste(pred_names, collapse = ", ")
    ))
    tryCatch(
      {
        model_obj <- caret::train(form, data = df, method = model_method, trControl = ctrl_used)
        attr(model_obj, "predictors_used") <- pred_names
        model_obj
      },
      error = function(e) {
        message("DEBUG[error:", tag, "]: caret::train failed")
        message("DEBUG[message:", tag, "]: ", conditionMessage(e))
        message("DEBUG[formula:", tag, "]: ", deparse(form))
        message("DEBUG[dims:", tag, "]: rows=", nrow(df), " cols=", length(pred_names))
        message("DEBUG[predictors:", tag, "]: ", paste(pred_names, collapse = ", "))
        message("DEBUG[class(y):", tag, "]: ", paste(class(df[[response_name]]), collapse = "/"))
        message("DEBUG[head(df):", tag, "]:")
        dbg <- utils::capture.output(print(utils::head(df, 5)))
        message(paste(dbg, collapse = "\n"))
        stop(e)
      }
    )
  }
  
  # First attempt: as requested
  res <- tryCatch(
    attempt_train(tr_ctrl, tag = "cv"),
    error = function(e) {
      # Automatic fallback for 'lm' if resampling is requested and user gave a list
      if (identical(model_method, "lm") &&
          (!inherits(train_control_params, "trainControl")) &&
          !identical(train_control_params$method, "none")) {
        message("DEBUG[retry]: Retrying with trainControl(method='none') for model_method='lm'")
        ctrl_none <- caret::trainControl(method = "none")
        return(attempt_train(ctrl_none, tag = "none"))
      }
      stop(e)
    }
  )
  
  res
}

# -----------------------------
# Main wrapper
# -----------------------------

#' Recursive Feature Selection and Optional Final Training
#'
#' Orchestrates train/test split, optional one-hot encoding, RFE on the training set,
#' and optional final model training on all rows.
#'
#' @param data A `data.frame` with the response and predictors.
#' @param response_var A response column name (`character`) or index (`numeric` of length 1).
#' @param seed An `integer` seed. Default: `123`.
#' @param rfe_control A `list` of arguments for `caret::rfeControl` (at least `method` and `number`).
#' @param train_control A `list` of arguments for `caret::trainControl` or a `trainControl` object.
#' @param sizes A numeric vector of subset sizes to evaluate during RFE. If `NULL`, uses `1:ncol(X)`
#'   where `X` are predictors after preprocessing.
#' @param parallel `logical`. Enable parallel processing for RFE/training.
#' @param feature_funcs A caret RFE function set (e.g., `caret::rfFuncs`). If `NULL`, defaults to `caret::rfFuncs`.
#' @param handle_categorical `logical`. Apply one-hot encoding to predictors (fit on train, apply to test and full).
#' @param return_final_model `logical`. If `TRUE`, trains a final model on the full dataset using selected features.
#' @param model_method A `character` specifying the caret model key for final training.
#'
#' @return A `list` with components:
#' \describe{
#'   \item{ResponseName}{Resolved response column name.}
#'   \item{TaskType}{`"classification"` or `"regression"`.}
#'   \item{TrainIndex}{Row indices used for the training partition.}
#'   \item{TestIndex}{Row indices used for the testing partition.}
#'   \item{Preprocessor}{`dummyVars` object used for encoding, if any; else `NULL`.}
#'   \item{RFE}{The `rfe` object returned by `caret`.}
#'   \item{OptimalNumberOfVariables}{Optimal subset size selected by RFE.}
#'   \item{OptimalVariables}{Character vector of selected variable names (from RFE).}
#'   \item{VariableImportance}{Variable importance data.frame from RFE.}
#'   \item{ResamplingResults}{Resampling summary from the RFE process.}
#'   \item{FinalModel}{Trained `caret::train` object if `return_final_model = TRUE`; else `NULL`.}
#'   \item{FinalModelVariables}{Character vector of predictors actually used in the final model (after NZV and
#'         linear-combination filtering), or `NULL` if no final model was fit.}
#' }
#'
#' @export
fs_recursivefeature <- function(data, response_var,
                                seed = 123,
                                rfe_control = list(method = "cv", number = 5),
                                train_control = list(method = "cv", number = 5),
                                sizes = NULL,
                                parallel = FALSE,
                                feature_funcs = NULL,
                                handle_categorical = FALSE,
                                return_final_model = FALSE,
                                model_method = "rf") {
  if (!is.data.frame(data)) {
    stop("fs_recursivefeature: 'data' must be a data.frame.")
  }
  
  y_name <- .resolve_response_name(data, response_var)
  
  split <- split_data(data, response_var = y_name, seed = seed, p = 0.8)
  train_raw <- split$train
  test_raw  <- split$test
  train_idx <- split$TrainIndex
  test_idx  <- split$TestIndex
  
  message(sprintf(
    "DEBUG[fs_recursivefeature]: train_n=%d | test_n=%d | response=%s",
    nrow(train_raw), nrow(test_raw), y_name
  ))
  
  preproc <- NULL
  if (handle_categorical) {
    preproc <- fit_one_hot_encoder(train_raw, response_name = y_name)
    train_df <- apply_one_hot_encoder(train_raw, response_name = y_name, dv = preproc)
    test_df  <- apply_one_hot_encoder(test_raw,  response_name = y_name, dv = preproc)
  } else {
    pred_names <- setdiff(colnames(train_raw), y_name)
    train_df <- train_raw[, c(y_name, pred_names), drop = FALSE]
    test_df  <- test_raw[,  c(y_name, pred_names), drop = FALSE]
  }
  
  cl <- .start_parallel(parallel)
  on.exit(.stop_parallel(cl), add = TRUE)
  
  rfe_obj <- perform_rfe(
    train_df           = train_df,
    response_name      = y_name,
    sizes              = sizes,
    rfe_control_params = rfe_control,
    feature_funcs      = feature_funcs,
    parallel           = parallel,
    early_stop         = FALSE
  )
  
  optimal_num  <- rfe_obj$optsize
  optimal_vars <- rfe_obj$optVariables
  var_imp      <- rfe_obj$variables
  resamp       <- rfe_obj$results
  
  final_model <- NULL
  final_model_vars <- NULL
  
  if (return_final_model) {
    if (handle_categorical) {
      full_df <- apply_one_hot_encoder(data, response_name = y_name, dv = preproc)
    } else {
      pred_names_full <- setdiff(colnames(data), y_name)
      full_df <- data[, c(y_name, pred_names_full), drop = FALSE]
    }
    final_model <- train_final_model(
      data_df = full_df,
      response_name = y_name,
      optimal_vars = optimal_vars,
      train_control_params = train_control,
      model_method = model_method
    )
    final_model_vars <- attr(final_model, "predictors_used")
  }
  
  list(
    ResponseName             = y_name,
    TaskType                 = .task_type(data[[y_name]]),
    TrainIndex               = train_idx,
    TestIndex                = test_idx,
    Preprocessor             = preproc,
    RFE                      = rfe_obj,
    OptimalNumberOfVariables = optimal_num,
    OptimalVariables         = optimal_vars,
    VariableImportance       = var_imp,
    ResamplingResults        = resamp,
    FinalModel               = final_model,
    FinalModelVariables      = final_model_vars
  )
}
