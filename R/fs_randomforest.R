# ==============================================================================
# Random Forest Wrapper (Classification/Regression)
# ==============================================================================
# Dependencies: randomForest, doParallel, foreach, caret, data.table, stats
# Optional: pROC (binary ROC/AUC if installed)
# ==============================================================================

# ---- Package checks -----------------------------------------------------------
.require_or_install <- function(pkgs, auto_install = FALSE) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (isTRUE(auto_install)) {
        install.packages(pkg)
      } else {
        stop(sprintf("Package '%s' is required. Install it first.", pkg), call. = FALSE)
      }
    }
  }
}

#' Fit and evaluate a Random Forest model
#'
#' @param data A data.frame or data.table containing predictors and target.
#' @param target Character scalar; name of the target column.
#' @param type One of \code{"classification"} or \code{"regression"}.
#' @param control List of options (see Details).
#' @param auto_install Logical; if TRUE, installs missing packages.
#'
#' @details
#' \strong{control list (defaults)}:
#' \itemize{
#'   \item \code{seed = NULL}
#'   \item \code{split_ratio = 0.75}
#'   \item \code{sample_size = NULL}
#'   \item \code{ntree = 500}
#'   \item \code{importance = TRUE}
#'   \item \code{mtry = NULL}
#'   \item \code{nodesize = NULL}
#'   \item \code{maxnodes = NULL}
#'   \item \code{sampsize = NULL}
#'   \item \code{classwt = NULL}
#'   \item \code{strata = NULL}
#'   \item \code{replace = TRUE}
#'   \item \code{n_cores = 1}
#'   \item \code{preprocess = NULL} (function \code{dt -> dt})
#'   \item \code{feature_select = NULL} (function \code{dt -> dt}; must retain target)
#'   \item \code{impute = TRUE} (median/mode)
#'   \item \code{drop_zerovar = TRUE}
#'   \item \code{oob = TRUE}
#'   \item \code{return_test_data = FALSE}
#' }
#'
#' @return An object of class \code{fs_rf_result} containing:
#' \itemize{
#'   \item \code{model} : the fitted \code{randomForest} object
#'   \item \code{metrics} : named list of evaluation metrics
#'   \item \code{predictions} : predictions on the test set
#'   \item \code{probabilities} : class probabilities (classification)
#'   \item \code{importance} : variable importance data.frame (if requested)
#'   \item \code{confusion} : confusion matrix (classification)
#'   \item \code{oob} : out-of-bag metrics (if available)
#'   \item \code{target}, \code{type}, \code{feature_names}, \code{train_index}, \code{control}
#'   \item \code{test_data} : held-out test set (if \code{return_test_data = TRUE})
#' }
#' @importFrom stats median predict
#' @importFrom utils head
#' @export
fs_randomforest <- function(data,
                            target,
                            type = c("classification", "regression"),
                            control = list(),
                            auto_install = FALSE) {
  
  .require_or_install(c("randomForest", "doParallel", "foreach", "caret", "data.table"), auto_install)
  
  type <- match.arg(type)
  if (!is.data.frame(data)) stop("`data` must be a data.frame or data.table.", call. = FALSE)
  if (!is.character(target) || length(target) != 1 || !nzchar(target)) {
    stop("`target` must be a non-empty single column name.", call. = FALSE)
  }
  if (!target %in% names(data)) stop("Target column not found in `data`.", call. = FALSE)
  
  # ---- Merge control with defaults -------------------------------------------
  ctrl <- modifyList(list(
    seed = NULL,
    split_ratio = 0.75,
    sample_size = NULL,
    ntree = 500,
    importance = TRUE,
    mtry = NULL,
    nodesize = NULL,
    maxnodes = NULL,
    sampsize = NULL,
    classwt = NULL,
    strata = NULL,
    replace = TRUE,
    n_cores = 1,
    preprocess = NULL,
    feature_select = NULL,
    impute = TRUE,
    drop_zerovar = TRUE,
    oob = TRUE,
    return_test_data = FALSE
  ), control, keep.null = TRUE)
  
  # ---- Seed -------------------------------------------------------------------
  if (!is.null(ctrl$seed)) set.seed(as.integer(ctrl$seed))
  
  # ---- Convert to data.table --------------------------------------------------
  dt <- data.table::as.data.table(data)
  
  # ---- Custom preprocess ------------------------------------------------------
  if (!is.null(ctrl$preprocess)) {
    if (!is.function(ctrl$preprocess)) stop("`control$preprocess` must be a function(dt) -> dt.", call. = FALSE)
    dt <- ctrl$preprocess(dt)
    if (!is.data.frame(dt)) stop("`preprocess` must return a data.frame/data.table.", call. = FALSE)
  }
  
  # ---- Custom feature selection ----------------------------------------------
  if (!is.null(ctrl$feature_select)) {
    if (!is.function(ctrl$feature_select)) stop("`control$feature_select` must be a function(dt) -> dt.", call. = FALSE)
    dt <- ctrl$feature_select(dt)
    if (!target %in% names(dt)) stop("Feature selection removed the target column.", call. = FALSE)
  }
  
  # ---- Date -> numeric --------------------------------------------------------
  date_cols <- vapply(dt, inherits, logical(1), what = "Date")
  if (any(date_cols)) {
    idx <- which(date_cols)
    dt[, (idx) := lapply(.SD, as.numeric), .SDcols = idx]
  }
  
  # ---- Target type ------------------------------------------------------------
  if (type == "classification") {
    dt[[target]] <- as.factor(dt[[target]])
    if (nlevels(dt[[target]]) < 2) stop("Classification target must have at least 2 classes.", call. = FALSE)
  } else {
    dt[[target]] <- suppressWarnings(as.numeric(dt[[target]]))
    if (anyNA(dt[[target]])) stop("Regression target must be numeric (coercion introduced NAs).", call. = FALSE)
  }
  
  # ---- Optional downsampling BEFORE split ------------------------------------
  if (!is.null(ctrl$sample_size) && is.finite(ctrl$sample_size) && ctrl$sample_size > 0) {
    if (type == "classification") {
      dt <- dt[, .SD[sample(.N, size = max(1L, floor((.N / nrow(dt)) * ctrl$sample_size)))], by = target]
    } else {
      dt <- dt[sample(.N, min(ctrl$sample_size, .N))]
    }
  }
  
  # ---- Zero-variance removal --------------------------------------------------
  if (isTRUE(ctrl$drop_zerovar)) {
    nzv <- caret::nearZeroVar(dt[, !target, with = FALSE], saveMetrics = TRUE)
    if (any(nzv$zeroVar | nzv$nzv)) {
      drop_cols <- rownames(nzv)[nzv$zeroVar | nzv$nzv]
      if (length(drop_cols)) dt[, (drop_cols) := NULL]
    }
  }
  
  # ---- Imputation -------------------------------------------------------------
  if (isTRUE(ctrl$impute)) {
    impute_col <- function(x) {
      if (!anyNA(x)) return(x)
      if (is.numeric(x)) {
        med <- stats::median(x, na.rm = TRUE); x[is.na(x)] <- med
      } else if (is.factor(x)) {
        tab <- table(x, useNA = "no"); if (length(tab)) x[is.na(x)] <- names(which.max(tab))
      } else if (is.character(x)) {
        tab <- table(x, useNA = "no"); if (length(tab)) x[is.na(x)] <- names(which.max(tab))
      } else {
        x[is.na(x)] <- NA
      }
      x
    }
    feat_cols <- setdiff(names(dt), target)
    dt[, (feat_cols) := lapply(.SD, impute_col), .SDcols = feat_cols]
  }
  
  # ---- Train/test split -------------------------------------------------------
  if (ctrl$split_ratio <= 0 || ctrl$split_ratio >= 1) stop("`split_ratio` must be in (0,1).", call. = FALSE)
  index <- caret::createDataPartition(y = dt[[target]], p = ctrl$split_ratio, list = FALSE)
  train_dt <- dt[index]
  test_dt  <- dt[-index]
  if (nrow(test_dt) == 0L) stop("Test split is empty; adjust `split_ratio`.", call. = FALSE)
  
  # ---- Align factor levels ----------------------------------------------------
  align_levels <- function(train, test) {
    for (nm in intersect(names(train), names(test))) {
      if (is.factor(train[[nm]])) test[[nm]] <- factor(test[[nm]], levels = levels(train[[nm]]))
    }
    list(train = train, test = test)
  }
  aligned <- align_levels(train_dt, test_dt)
  train_dt <- aligned$train
  test_dt  <- aligned$test
  
  # ---- Prepare X/Y ------------------------------------------------------------
  target_idx <- match(target, names(train_dt))
  x_train <- train_dt[, -target_idx, with = FALSE]
  y_train <- train_dt[[target]]
  
  # Ensure data.frame for randomForest; validate predictors
  x_train <- as.data.frame(x_train)
  p <- ncol(x_train)
  if (is.na(p) || p < 1L) {
    stop("No predictor columns left after preprocessing/feature selection/NZV removal.", call. = FALSE)
  }
  
  # ---- Compute mtry safely ----------------------------------------------------
  mtry_eff <- ctrl$mtry
  if (is.null(mtry_eff) || is.na(mtry_eff) || !is.finite(mtry_eff)) {
    mtry_eff <- if (type == "classification") max(1L, floor(sqrt(p))) else max(1L, floor(p / 3))
  }
  mtry_eff <- as.integer(mtry_eff)
  mtry_eff <- min(max(1L, mtry_eff), p)
  
  # ---- Compute sampsize safely ------------------------------------------------
  sampsize_eff <- ctrl$sampsize
  if (is.null(sampsize_eff)) {
    # leave NULL so randomForest applies its internal default
  } else {
    if (type == "regression" && length(sampsize_eff) > 1L) {
      stop("`sampsize` must be a single scalar for regression.", call. = FALSE)
    }
    if (anyNA(sampsize_eff)) {
      sampsize_eff <- if (isTRUE(ctrl$replace)) nrow(x_train) else ceiling(0.632 * nrow(x_train))
    }
    sampsize_eff <- as.integer(sampsize_eff)
    if (!is.finite(sampsize_eff) || sampsize_eff < 1L) sampsize_eff <- 1L
    sampsize_eff <- min(sampsize_eff, nrow(x_train))
  }
  
  # ---- Parallel setup ---------------------------------------------------------
  max_cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 1L)
  n_cores <- max(1L, min(as.integer(ctrl$n_cores), max_cores))
  if (is.na(n_cores)) n_cores <- 1L
  
  # ---- ntree distribution -----------------------------------------------------
  ntree <- as.integer(ctrl$ntree)
  if (!is.finite(ntree) || ntree < 1L) stop("`ntree` must be a positive integer.", call. = FALSE)
  
  ntree_list <- if (n_cores > 1L) {
    base <- ntree %/% n_cores
    remainder <- ntree %% n_cores
    out <- rep(base, n_cores)
    if (remainder > 0) out[seq_len(remainder)] <- out[seq_len(remainder)] + 1L
    out
  } else ntree
  
  # ---- randomForest args ------------------------------------------------------
  rf_args <- list(
    x = x_train,
    y = y_train,
    ntree = NULL, # set per-worker or serially
    importance = isTRUE(ctrl$importance),
    mtry = mtry_eff,
    nodesize = ctrl$nodesize,
    maxnodes = ctrl$maxnodes,
    sampsize = sampsize_eff,
    strata = ctrl$strata,
    classwt = ctrl$classwt,
    replace = isTRUE(ctrl$replace),
    do.trace = FALSE,
    keep.forest = TRUE
  )
  rf_args <- Filter(function(z) !is.null(z), rf_args) # drop NULLs
  
  # ---- Train model ------------------------------------------------------------
  if (n_cores > 1L) {
    .require_or_install("foreach", auto_install)
    .require_or_install("doParallel", auto_install)
    
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      try(doParallel::registerDoSEQ(), silent = TRUE)
      try(parallel::stopCluster(cl), silent = TRUE)
    }, add = TRUE)
    
    if (!is.null(ctrl$seed)) {
      parallel::clusterSetRNGStream(cl, as.integer(ctrl$seed))
    }
    
    # call %dopar% as a function to avoid parsing issues
    dopar_op <- get("%dopar%", asNamespace("foreach"))
    rf_model <- dopar_op(
      foreach::foreach(
        ntree_part = ntree_list,
        .combine = randomForest::combine,
        .packages = "randomForest"
      ),
      {
        args_i <- rf_args
        args_i$ntree <- ntree_part
        do.call(randomForest::randomForest, args_i)
      }
    )
    
  } else {
    args_serial <- rf_args
    args_serial$ntree <- ntree
    rf_model <- do.call(randomForest::randomForest, args_serial)
  }
  
  # ---- Evaluate on test set ---------------------------------------------------
  target_idx_test <- match(target, names(test_dt))
  x_test <- test_dt[, -target_idx_test, with = FALSE]
  y_test <- test_dt[[target]]
  x_test <- as.data.frame(x_test)
  
  metrics <- list()
  preds <- NULL
  probs <- NULL
  confusion <- NULL
  
  if (type == "classification") {
    preds <- stats::predict(rf_model, newdata = x_test, type = "class")
    suppressWarnings({
      probs <- try(stats::predict(rf_model, newdata = x_test, type = "prob"), silent = TRUE)
      if (inherits(probs, "try-error")) probs <- NULL
    })
    
    acc <- mean(preds == y_test)
    kappa <- try(caret::confusionMatrix(preds, y_test)$overall[["Kappa"]], silent = TRUE)
    if (inherits(kappa, "try-error") || is.null(kappa)) kappa <- NA_real_
    
    confusion <- table(Observed = y_test, Predicted = preds)
    
    auc <- NA_real_
    if (nlevels(y_test) == 2L && !is.null(probs) && requireNamespace("pROC", quietly = TRUE)) {
      positive <- levels(y_test)[2L]
      roc_obj <- pROC::roc(response = y_test, predictor = probs[, positive], quiet = TRUE)
      auc <- as.numeric(pROC::auc(roc_obj))
    }
    
    metrics <- list(accuracy = acc, kappa = unname(kappa), auc = auc)
    
  } else {
    preds <- as.numeric(stats::predict(rf_model, newdata = x_test))
    err <- preds - y_test
    rmse <- sqrt(mean(err^2))
    mae  <- mean(abs(err))
    r2   <- 1 - sum(err^2) / sum((y_test - mean(y_test))^2)
    metrics <- list(RMSE = rmse, MAE = mae, R2 = r2)
  }
  
  # ---- Variable importance ----------------------------------------------------
  importance_df <- NULL
  if (isTRUE(ctrl$importance)) {
    imp <- randomForest::importance(rf_model, type = 1)
    importance_df <- data.frame(
      feature = rownames(imp),
      importance = imp[, 1],
      row.names = NULL,
      check.names = FALSE
    )
    importance_df <- importance_df[order(-importance_df$importance), , drop = FALSE]
  }
  
  # ---- OOB metrics ------------------------------------------------------------
  oob <- NULL
  if (type == "classification" && length(rf_model$confusion)) {
    oob_acc <- 1 - rf_model$err.rate[rf_model$ntree, "OOB"]
    oob <- list(accuracy = oob_acc)
  } else if (type == "regression" && !is.null(rf_model$mse)) {
    oob <- list(RMSE = sqrt(tail(rf_model$mse, 1)))
  }
  
  # ---- Assemble result --------------------------------------------------------
  result <- list(
    model = rf_model,
    metrics = metrics,
    predictions = preds,
    probabilities = probs,
    importance = importance_df,
    confusion = confusion,
    oob = oob,
    target = target,
    type = type,
    feature_names = setdiff(names(train_dt), target),
    train_index = as.integer(index),
    control = ctrl
  )
  if (isTRUE(ctrl$return_test_data)) result$test_data <- test_dt
  
  class(result) <- c("fs_rf_result", class(result))
  return(result)
}


# ---- Helpers -----------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b
