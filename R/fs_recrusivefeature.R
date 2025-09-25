# ============================================================
# Feature Selection & Modeling Utilities (caret-based)
# ============================================================
# Depends on: caret, doParallel, parallel
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
#' Converts a response specification (column name or column index) to a valid column name present in `data`.
#'
#' @param data A `data.frame` containing the dataset.
#' @param response_var A single column name (`character`) or column index (`integer`) indicating the response variable.
#'
#' @return A single `character` string with the resolved response column name.
#'
#' @examples
#' df <- data.frame(y = 1:3, x = 4:6)
#' .resolve_response_name(df, "y")
#' .resolve_response_name(df, 1L)
#'
#' @keywords internal
.resolve_response_name <- function(data, response_var) {
  if (is.numeric(response_var)) {
    response_var <- as.integer(response_var)
    if (response_var < 1 || response_var > ncol(data)) {
      stop("response_var index is out of bounds.")
    }
    return(colnames(data)[response_var])
  } else if (is.character(response_var) && length(response_var) == 1) {
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
#' Determines whether a supervised learning task is classification or regression based on the response vector.
#'
#' @param y A response vector (factor/character implies classification; otherwise regression).
#'
#' @return A `character` scalar: either `"classification"` or `"regression"`.
#'
#' @examples
#' .task_type(factor(c("A","B")))
#' .task_type(rnorm(10))
#'
#' @keywords internal
.task_type <- function(y) {
  if (is.factor(y) || is.character(y)) return("classification")
  "regression"
}

# -----------------------------
# Control parameter validation
# -----------------------------

#' Validate RFE Control Parameters
#'
#' Validates the resampling controls provided for recursive feature elimination (RFE).
#'
#' @param control_params A `list` with elements `method` and `number`. `method` must be one of `"cv"`, `"LOOCV"`, `"repeatedcv"`, or `"boot"`. `number` must be a positive integer.
#'
#' @return Invisibly returns `TRUE` on success; otherwise throws an error.
#'
#' @examples
#' validate_rfe_control(list(method = "cv", number = 5))
#'
#' @export
validate_rfe_control <- function(control_params) {
  if (!is.list(control_params)) stop("rfe control_params should be a list.")
  required <- c("method", "number")
  if (!all(required %in% names(control_params))) {
    stop("rfe control_params must contain 'method' and 'number'.")
  }
  valid_methods <- c("cv", "LOOCV", "repeatedcv", "boot")
  if (!(control_params$method %in% valid_methods)) {
    stop(paste0(
      "Invalid RFE resampling method. Must be one of: ",
      paste(valid_methods, collapse = ", "), "."
    ))
  }
  if (!is.numeric(control_params$number) || control_params$number <= 0) {
    stop("rfe control_params$number must be a positive integer.")
  }
  invisible(TRUE)
}

#' Validate Training Control Parameters
#'
#' Validates the resampling controls provided for final model training.
#'
#' @param control_params A `list` with at least `method`. `method` must be one of `"cv"`, `"LOOCV"`, `"repeatedcv"`, `"boot"`, or `"none"`. When `method != "none"`, provide a positive integer `number`.
#'
#' @return Invisibly returns `TRUE` on success; otherwise throws an error.
#'
#' @examples
#' validate_train_control(list(method = "cv", number = 5))
#' validate_train_control(list(method = "none"))
#'
#' @export
validate_train_control <- function(control_params) {
  if (!is.list(control_params)) stop("train control_params should be a list.")
  required <- c("method")
  if (!all(required %in% names(control_params))) {
    stop("train control_params must contain at least 'method'.")
  }
  valid_methods <- c("cv", "LOOCV", "repeatedcv", "boot", "none")
  if (!(control_params$method %in% valid_methods)) {
    stop(paste0(
      "Invalid training resampling method. Must be one of: ",
      paste(valid_methods, collapse = ", "), "."
    ))
  }
  if (control_params$method != "none") {
    if (!("number" %in% names(control_params))) {
      stop("When training control method is not 'none', provide 'number'.")
    }
    if (!is.numeric(control_params$number) || control_params$number <= 0) {
      stop("train control_params$number must be a positive integer.")
    }
  }
  invisible(TRUE)
}

# -----------------------------
# Data splitting
# -----------------------------

#' Train/Test Split (80/20)
#'
#' Splits a dataset into training (80%) and testing (20%) partitions. Stratifies by response for classification tasks.
#'
#' @param data A `data.frame` containing predictors and response.
#' @param response_var A response column name (`character`) or index (`integer`).
#' @param seed An `integer` seed for reproducibility. Default: `123`.
#' @param p Proportion for the training set (`0 < p < 1`). Default: `0.8`.
#'
#' @return A `list` with elements:
#' \describe{
#'   \item{train}{Training `data.frame`.}
#'   \item{test}{Testing `data.frame`.}
#'   \item{response_name}{The resolved response column name.}
#' }
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(y = factor(sample(c("A","B"), 50, TRUE)),
#'                  x1 = rnorm(50), x2 = rnorm(50))
#' split <- split_data(df, response_var = "y", seed = 99)
#' str(split)
#'
#' @export
split_data <- function(data, response_var, seed = 123, p = 0.8) {
  set.seed(seed)
  y_name <- .resolve_response_name(data, response_var)
  y <- data[[y_name]]
  if (is.factor(y) || is.character(y)) {
    y <- as.factor(y)
  } else {
    y <- as.numeric(y)
  }
  idx <- createDataPartition(y, p = p, list = FALSE)
  list(
    train = data[idx, , drop = FALSE],
    test  = data[-idx, , drop = FALSE],
    response_name = y_name
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
#' @examples
#' tr <- data.frame(y = 1:3, a = factor(c("A","B","A")), b = c(0,1,0))
#' dv <- fit_one_hot_encoder(tr, response_name = "y")
#' dv
#'
#' @export
fit_one_hot_encoder <- function(train_df, response_name) {
  predictors <- train_df[ , setdiff(colnames(train_df), response_name), drop = FALSE]
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
#' @examples
#' tr <- data.frame(y = 1:3, a = factor(c("A","B","A")), b = c(0,1,0))
#' dv <- fit_one_hot_encoder(tr, response_name = "y")
#' apply_one_hot_encoder(tr, response_name = "y", dv = dv)
#'
#' @export
apply_one_hot_encoder <- function(data_df, response_name, dv) {
  predictors <- data_df[ , setdiff(colnames(data_df), response_name), drop = FALSE]
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
#' @examples
#' cl <- .start_parallel(FALSE)  # returns NULL
#'
#' @keywords internal
.start_parallel <- function(enable) {
  if (!enable) return(NULL)
  cores <- max(1L, parallel::detectCores() - 1L)
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  cl
}

#' Stop Parallel Backend
#'
#' Stops a previously started PSOCK cluster if not `NULL`.
#'
#' @param cl A cluster object or `NULL`.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' .stop_parallel(NULL)
#'
#' @keywords internal
.stop_parallel <- function(cl) {
  if (!is.null(cl)) parallel::stopCluster(cl)
  invisible(NULL)
}

# -----------------------------
# RFE (Recursive Feature Elimination)
# -----------------------------

#' Perform Recursive Feature Elimination
#'
#' Runs recursive feature elimination (RFE) on a preprocessed training dataset.
#'
#' @param train_df A `data.frame` where the first column is the response and remaining columns are predictors (or ensure `response_name` is provided to select the response).
#' @param response_name A single `character` name of the response column in `train_df`.
#' @param sizes A numeric vector of feature subset sizes to evaluate. If `NULL`, uses `1:ncol(X)`.
#' @param rfe_control_params A `list` with elements `method` and `number` for resampling control; see [\code{validate_rfe_control}].
#' @param feature_funcs A caret RFE function set (e.g., `caret::rfFuncs`). If `NULL`, defaults to `caret::rfFuncs`.
#' @param parallel `logical`. Allow parallel evaluation.
#' @param early_stop `logical`. If `TRUE`, sets `returnResamp = "final"` and `saveDetails = TRUE`.
#'
#' @return An object of class `rfe` from `caret`, containing RFE results.
#'
#' @examples
#' set.seed(1)
#' tr <- data.frame(y = rnorm(40), x1 = rnorm(40), x2 = rnorm(40))
#' rfe_obj <- perform_rfe(
#'   train_df = tr,
#'   response_name = "y",
#'   sizes = c(1,2),
#'   rfe_control_params = list(method = "cv", number = 3),
#'   feature_funcs = caret::rfFuncs,
#'   parallel = FALSE,
#'   early_stop = FALSE
#' )
#' rfe_obj$optsize
#'
#' @export
perform_rfe <- function(train_df, response_name, sizes, rfe_control_params,
                        feature_funcs = NULL, parallel = FALSE, early_stop = FALSE) {
  validate_rfe_control(rfe_control_params)
  
  y <- train_df[[response_name]]
  task <- .task_type(y)
  if (is.null(feature_funcs)) {
    feature_funcs <- caret::rfFuncs
  }
  
  ctrl <- caret::rfeControl(
    functions = feature_funcs,
    method    = rfe_control_params$method,
    number    = rfe_control_params$number,
    verbose   = TRUE,
    allowParallel = parallel
  )
  
  if (early_stop) {
    ctrl$returnResamp <- "final"
    ctrl$saveDetails  <- TRUE
  }
  
  X <- train_df[ , setdiff(colnames(train_df), response_name), drop = FALSE]
  y_vec <- train_df[[response_name]]
  if (task == "classification" && !is.factor(y_vec)) y_vec <- as.factor(y_vec)
  
  if (is.null(sizes)) {
    sizes <- seq_len(ncol(X))
  } else {
    sizes <- sizes[sizes >= 1 & sizes <= ncol(X)]
    if (length(sizes) == 0) stop("No valid 'sizes' remain within the range of available predictors.")
  }
  
  rfe_profile <- tryCatch(
    {
      caret::rfe(x = X, y = y_vec, sizes = sizes, rfeControl = ctrl)
    },
    error = function(e) stop("Error during RFE: ", e$message)
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
#' @param data_df A `data.frame` containing the response column and encoded predictors.
#' @param response_name A single `character` name of the response column.
#' @param optimal_vars A `character` vector of selected predictor names.
#' @param train_control_params A `list` specifying training control; see [\code{validate_train_control}].
#' @param model_method A `character` string specifying the caret model key (e.g., `"rf"`, `"lm"`, `"xgbTree"`). Default: `"rf"`.
#'
#' @return A trained `caret::train` model object.
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(y = rnorm(30), a = rnorm(30), b = rnorm(30))
#' model <- train_final_model(
#'   data_df = dat,
#'   response_name = "y",
#'   optimal_vars = c("a","b"),
#'   train_control_params = list(method = "cv", number = 3),
#'   model_method = "lm"
#' )
#' model
#'
#' @export
train_final_model <- function(data_df, response_name, optimal_vars,
                              train_control_params = list(method = "cv", number = 5),
                              model_method = "rf") {
  validate_train_control(train_control_params)
  
  # Check requested predictors exist
  missing_vars <- setdiff(optimal_vars, colnames(data_df))
  if (length(missing_vars) > 0) {
    stop("train_final_model: The following predictors are missing from data_df: ",
         paste(missing_vars, collapse = ", "))
  }
  
  # Build working data.frame
  df <- data_df[, c(response_name, optimal_vars), drop = FALSE]
  
  # Coerce response appropriately
  if (is.factor(df[[response_name]]) || is.character(df[[response_name]])) {
    df[[response_name]] <- as.factor(df[[response_name]])
  } else {
    df[[response_name]] <- as.numeric(df[[response_name]])
  }
  
  # Ensure predictors are numeric (defensive; dummyVars usually guarantees this)
  pred_names <- setdiff(colnames(df), response_name)
  for (nm in pred_names) {
    if (is.logical(df[[nm]])) df[[nm]] <- as.integer(df[[nm]])
    if (is.factor(df[[nm]]) || is.character(df[[nm]])) {
      df[[nm]] <- as.numeric(as.factor(df[[nm]]))
    }
  }
  
  # Remove NZV predictors
  nzv_idx <- caret::nearZeroVar(df[, pred_names, drop = FALSE], saveMetrics = FALSE)
  if (length(nzv_idx) > 0) {
    removed <- pred_names[nzv_idx]
    message("DEBUG[nzv]: Removing near-zero variance predictors: ",
            paste(removed, collapse = ", "))
    pred_names <- setdiff(pred_names, removed)
    df <- df[, c(response_name, pred_names), drop = FALSE]
  }
  
  # Remove linear combinations
  if (length(pred_names) > 1) {
    lc <- caret::findLinearCombos(as.matrix(df[, pred_names, drop = FALSE]))
    if (!is.null(lc$remove) && length(lc$remove) > 0) {
      drop_lc <- pred_names[lc$remove]
      message("DEBUG[lincomb]: Removing linear-combination predictors: ",
              paste(drop_lc, collapse = ", "))
      pred_names <- setdiff(pred_names, drop_lc)
      df <- df[, c(response_name, pred_names), drop = FALSE]
    }
  }
  
  if (length(pred_names) == 0) {
    stop("train_final_model: No predictors remain after preprocessing.")
  }
  
  # TrainControl setup
  make_ctrl <- function(ctrl) {
    if (identical(ctrl$method, "none")) {
      caret::trainControl(method = "none")
    } else {
      caret::trainControl(method = ctrl$method, number = ctrl$number)
    }
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
      caret::train(form, data = df, method = model_method, trControl = ctrl_used),
      error = function(e) {
        message("DEBUG[error:", tag, "]: caret::train failed")
        message("DEBUG[message:", tag, "]: ", conditionMessage(e))
        message("DEBUG[formula:", tag, "]: ", deparse(form))
        message("DEBUG[dims:", tag, "]: rows=", nrow(df), " cols=", length(pred_names))
        message("DEBUG[predictors:", tag, "]: ", paste(pred_names, collapse = ", "))
        message("DEBUG[class(y):", tag, "]: ", paste(class(df[[response_name]]), collapse = "/"))
        message("DEBUG[head(df):", tag, "]:")
        utils::capture.output(print(utils::head(df, 5))) |> paste(collapse = "\n") |> message()
        stop(e)
      }
    )
  }
  
  # First attempt: as requested
  res <- tryCatch(
    attempt_train(tr_ctrl, tag = "cv"),
    error = function(e) {
      # Automatic fallback for 'lm' if resampling is requested
      if (identical(model_method, "lm") && !identical(train_control_params$method, "none")) {
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
#' Orchestrates train/test split, optional one-hot encoding, RFE on the training set, and optional final model training on all rows.
#'
#' @param data A `data.frame` with the response and predictors.
#' @param response_var A response column name (`character`) or index (`integer`).
#' @param seed An `integer` seed. Default: `123`.
#' @param rfe_control A `list(method, number)` for RFE resampling control. Methods: `"cv"`, `"LOOCV"`, `"repeatedcv"`, `"boot"`. Default: `list(method = "cv", number = 5)`.
#' @param train_control A `list(method, number)` for final training control. Methods: `"cv"`, `"LOOCV"`, `"repeatedcv"`, `"boot"`, `"none"`. Default: `list(method = "cv", number = 5)`.
#' @param sizes A numeric vector of subset sizes to evaluate during RFE. If `NULL`, uses `1:ncol(X)` where `X` are predictors after preprocessing.
#' @param parallel `logical`. Enable parallel processing for RFE/training. Default: `FALSE`.
#' @param feature_funcs A caret RFE function set (e.g., `caret::rfFuncs`). If `NULL`, defaults to `caret::rfFuncs`.
#' @param handle_categorical `logical`. Apply one-hot encoding to predictors (fit on train, apply to test and full). Default: `FALSE`.
#' @param return_final_model `logical`. If `TRUE`, trains a final model on the full dataset using selected features. Default: `FALSE`.
#' @param model_method A `character` specifying the caret model key for final training. Default: `"rf"`.
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
#'   \item{OptimalVariables}{Character vector of selected variable names.}
#'   \item{VariableImportance}{Variable importance data.frame from RFE.}
#'   \item{ResamplingResults}{Resampling summary from the RFE process.}
#'   \item{FinalModel}{Trained `caret::train` object if `return_final_model = TRUE`; else `NULL`.}
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' n <- 100
#' dat <- data.frame(
#'   y = factor(sample(c("A","B"), n, TRUE)),
#'   x1 = rnorm(n),
#'   x2 = rnorm(n),
#'   cat = sample(LETTERS[1:3], n, TRUE)
#' )
#'
#' res <- fs_recursivefeature(
#'   data = dat,
#'   response_var = "y",
#'   seed = 42,
#'   rfe_control = list(method = "cv", number = 5),
#'   train_control = list(method = "cv", number = 5),
#'   sizes = c(1,2,3,4),
#'   parallel = FALSE,
#'   feature_funcs = caret::rfFuncs,
#'   handle_categorical = TRUE,
#'   return_final_model = TRUE,
#'   model_method = "rf"
#' )
#'
#' res$OptimalNumberOfVariables
#' res$OptimalVariables
#' res$FinalModel
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
  y_name <- .resolve_response_name(data, response_var)
  
  split <- split_data(data, response_var = y_name, seed = seed, p = 0.8)
  train_raw <- split$train
  test_raw  <- split$test
  train_idx <- as.integer(rownames(train_raw))
  test_idx  <- as.integer(rownames(test_raw))
  
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
    train_df        = train_df,
    response_name   = y_name,
    sizes           = sizes,
    rfe_control_params = rfe_control,
    feature_funcs   = feature_funcs,
    parallel        = parallel,
    early_stop      = FALSE
  )
  
  optimal_num  <- rfe_obj$optsize
  optimal_vars <- rfe_obj$optVariables
  var_imp      <- rfe_obj$variables
  resamp       <- rfe_obj$results
  
  final_model <- NULL
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
  }
  
  list(
    ResponseName = y_name,
    TaskType = .task_type(data[[y_name]]),
    TrainIndex = train_idx,
    TestIndex = test_idx,
    Preprocessor = preproc,
    RFE = rfe_obj,
    OptimalNumberOfVariables = optimal_num,
    OptimalVariables = optimal_vars,
    VariableImportance = var_imp,
    ResamplingResults = resamp,
    FinalModel = final_model
  )
}
