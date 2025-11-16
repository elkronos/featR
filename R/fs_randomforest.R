# ==============================================================================
# Random Forest Wrapper (Classification/Regression)
# ==============================================================================
# Dependencies: randomForest, caret, data.table
# Optional: doParallel, foreach, pROC (binary ROC/AUC)
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
#'   \item \code{seed = NULL} (integer; used for reproducibility)
#'   \item \code{split_ratio = 0.75}
#'   \item \code{sample_size = NULL} (optional downsampling size before split)
#'   \item \code{ntree = 500}
#'   \item \code{importance = TRUE}
#'   \item \code{mtry = NULL} (defaults: \eqn{\sqrt{p}} for classification, \eqn{p/3} for regression)
#'   \item \code{nodesize = NULL}
#'   \item \code{maxnodes = NULL}
#'   \item \code{sampsize = NULL}
#'   \item \code{classwt = NULL}
#'   \item \code{strata = NULL}
#'   \item \code{replace = TRUE}
#'   \item \code{n_cores = 1} (capped at \code{ntree} and available cores)
#'   \item \code{preprocess = NULL} (function \code{dt -> dt}, applied before split)
#'   \item \code{feature_select = NULL} (function \code{dt -> dt}; must retain target, applied before split)
#'   \item \code{impute = TRUE} (median/mode; learned on training data and applied to test)
#'   \item \code{drop_zerovar = TRUE} (near-zero variance removal using training data only)
#'   \item \code{oob = TRUE} (include OOB metrics in the result object)
#'   \item \code{return_test_data = FALSE}
#'   \item \code{positive_class = NULL} (optional level name for binary AUC; defaults to second factor level)
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
#'   \item \code{oob} : out-of-bag metrics (if \code{control$oob = TRUE})
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
  
  # Core dependencies (parallel and pROC are optional)
  .require_or_install(c("randomForest", "caret", "data.table"), auto_install)
  
  type <- match.arg(type)
  if (!is.data.frame(data)) stop("`data` must be a data.frame or data.table.", call. = FALSE)
  if (!is.character(target) || length(target) != 1L || !nzchar(target)) {
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
    return_test_data = FALSE,
    positive_class = NULL
  ), control, keep.null = TRUE)
  
  # ---- Seed -------------------------------------------------------------------
  seed_int <- NULL
  if (!is.null(ctrl$seed)) {
    seed_int <- suppressWarnings(as.integer(ctrl$seed))
    if (is.na(seed_int)) stop("`control$seed` must be coercible to an integer scalar.", call. = FALSE)
    set.seed(seed_int)
  }
  
  # ---- Convert to data.table --------------------------------------------------
  dt <- data.table::as.data.table(data)
  
  # ---- Custom preprocess ------------------------------------------------------
  if (!is.null(ctrl$preprocess)) {
    if (!is.function(ctrl$preprocess)) stop("`control$preprocess` must be a function(dt) -> dt.", call. = FALSE)
    dt <- ctrl$preprocess(dt)
    if (!is.data.frame(dt)) stop("`preprocess` must return a data.frame/data.table.", call. = FALSE)
    dt <- data.table::as.data.table(dt)
  }
  
  # ---- Custom feature selection ----------------------------------------------
  if (!is.null(ctrl$feature_select)) {
    if (!is.function(ctrl$feature_select)) stop("`control$feature_select` must be a function(dt) -> dt.", call. = FALSE)
    dt <- ctrl$feature_select(dt)
    if (!is.data.frame(dt)) stop("`feature_select` must return a data.frame/data.table.", call. = FALSE)
    if (!target %in% names(dt)) stop("Feature selection removed the target column.", call. = FALSE)
    dt <- data.table::as.data.table(dt)
  }
  
  # ---- Date -> numeric --------------------------------------------------------
  date_cols <- vapply(dt, inherits, logical(1), what = "Date")
  if (any(date_cols)) {
    idx <- which(date_cols)
    dt[, (idx) := lapply(.SD, as.numeric), .SDcols = idx]
  }
  
  # ---- Target type and NA handling -------------------------------------------
  if (type == "classification") {
    dt[[target]] <- as.factor(dt[[target]])
    
    if (anyNA(dt[[target]])) {
      warning("Rows with NA in the classification target were removed before training.")
      keep_rows <- !is.na(dt[[target]])
      if (length(keep_rows) != nrow(dt)) {
        stop("Internal error: NA-filter logical index length mismatch (classification).", call. = FALSE)
      }
      dt <- dt[keep_rows]
    }
    
    dt[[target]] <- droplevels(dt[[target]])
    if (nlevels(dt[[target]]) < 2L) {
      stop("Classification target must have at least 2 classes after preprocessing and NA removal.", call. = FALSE)
    }
    
  } else {
    dt[[target]] <- suppressWarnings(as.numeric(dt[[target]]))
    
    if (anyNA(dt[[target]])) {
      warning("Rows with NA in the regression target (including from coercion) were removed before training.")
      keep_rows <- !is.na(dt[[target]])
      if (length(keep_rows) != nrow(dt)) {
        stop("Internal error: NA-filter logical index length mismatch (regression).", call. = FALSE)
      }
      dt <- dt[keep_rows]
    }
  }
  
  if (nrow(dt) == 0L) {
    stop("No rows left after cleaning the target variable.", call. = FALSE)
  }
  
  # ---- Optional downsampling BEFORE split ------------------------------------
  if (!is.null(ctrl$sample_size) && is.finite(ctrl$sample_size) && ctrl$sample_size > 0) {
    total_n <- nrow(dt)
    desired_total <- min(as.integer(ctrl$sample_size), total_n)
    
    if (type == "classification") {
      dt <- dt[
        , {
          desired_class <- floor((.N / total_n) * desired_total)
          size_class    <- max(1L, min(.N, desired_class))
          .SD[sample(.N, size = size_class)]
        },
        by = target
      ]
    } else {
      dt <- dt[sample(.N, size = desired_total)]
    }
  }
  
  # ---- Train/test split -------------------------------------------------------
  if (ctrl$split_ratio <= 0 || ctrl$split_ratio >= 1) {
    stop("`split_ratio` must be in (0,1).", call. = FALSE)
  }
  
  index <- caret::createDataPartition(y = dt[[target]], p = ctrl$split_ratio, list = FALSE)
  train_df <- as.data.frame(dt[index])
  test_df  <- as.data.frame(dt[-index])
  
  if (nrow(test_df) == 0L) {
    stop("Test split is empty; adjust `split_ratio`.", call. = FALSE)
  }
  
  # ---- Align factor levels between train and test -----------------------------
  align_levels <- function(train, test) {
    common <- intersect(names(train), names(test))
    for (nm in common) {
      if (is.factor(train[[nm]])) {
        test[[nm]] <- factor(test[[nm]], levels = levels(train[[nm]]))
      }
    }
    list(train = train, test = test)
  }
  
  aligned <- align_levels(train_df, test_df)
  train_df <- aligned$train
  test_df  <- aligned$test
  
  # ---- Zero-variance removal (train-only stats) ------------------------------
  if (isTRUE(ctrl$drop_zerovar)) {
    predictor_cols_train <- setdiff(names(train_df), target)
    if (length(predictor_cols_train) > 0L) {
      nzv <- caret::nearZeroVar(
        train_df[, predictor_cols_train, drop = FALSE],
        saveMetrics = TRUE
      )
      drop_cols <- rownames(nzv)[nzv$zeroVar | nzv$nzv]
      if (length(drop_cols) > 0L) {
        for (dc in drop_cols) {
          train_df[[dc]] <- NULL
          if (dc %in% names(test_df)) test_df[[dc]] <- NULL
        }
      }
    }
  }
  
  # ---- Imputation (train-only stats, applied to both) ------------------------
  if (isTRUE(ctrl$impute)) {
    feat_cols <- setdiff(names(train_df), target)
    if (length(feat_cols) > 0L) {
      # Compute imputation values on training data
      impute_values <- vector("list", length(feat_cols))
      names(impute_values) <- feat_cols
      
      for (i in seq_along(feat_cols)) {
        nm <- feat_cols[i]
        x  <- train_df[[nm]]
        if (!anyNA(x)) {
          impute_values[[i]] <- NA
        } else if (is.numeric(x)) {
          impute_values[[i]] <- stats::median(x, na.rm = TRUE)
        } else if (is.factor(x) || is.character(x)) {
          tab <- table(x, useNA = "no")
          impute_values[[i]] <- if (length(tab)) names(which.max(tab)) else NA
        } else {
          impute_values[[i]] <- NA
        }
      }
      
      # Apply to train and test
      for (i in seq_along(feat_cols)) {
        nm <- feat_cols[i]
        val <- impute_values[[i]]
        if (length(val) == 1L && is.na(val)) next
        
        # Train
        x_tr <- train_df[[nm]]
        idx_na_tr <- is.na(x_tr)
        if (any(idx_na_tr)) {
          x_tr[idx_na_tr] <- val
          train_df[[nm]] <- x_tr
        }
        
        # Test
        if (nm %in% names(test_df)) {
          x_te <- test_df[[nm]]
          idx_na_te <- is.na(x_te)
          if (any(idx_na_te)) {
            x_te[idx_na_te] <- val
            test_df[[nm]] <- x_te
          }
        }
      }
    }
  }
  
  # ---- Prepare X/Y for training ----------------------------------------------
  if (!target %in% names(train_df)) {
    stop("Target column missing from training data after preprocessing.", call. = FALSE)
  }
  
  target_idx <- match(target, names(train_df))
  x_train <- train_df[, -target_idx, drop = FALSE]
  y_train <- train_df[[target]]
  
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
      n_obs <- nrow(x_train)
      sampsize_eff <- if (isTRUE(ctrl$replace)) n_obs else ceiling(0.632 * n_obs)
    }
    sampsize_eff <- as.integer(sampsize_eff)
    if (any(!is.finite(sampsize_eff)) || any(sampsize_eff < 1L)) {
      sampsize_eff <- 1L
    }
    if (length(sampsize_eff) == 1L) {
      sampsize_eff <- min(sampsize_eff, nrow(x_train))
    } else {
      sampsize_eff <- pmin(sampsize_eff, nrow(x_train))
    }
  }
  
  # ---- ntree and parallel setup ----------------------------------------------
  ntree <- as.integer(ctrl$ntree)
  if (!is.finite(ntree) || ntree < 1L) stop("`ntree` must be a positive integer.", call. = FALSE)
  
  max_cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 1L)
  requested_cores <- suppressWarnings(as.integer(ctrl$n_cores))
  if (is.na(requested_cores) || requested_cores < 1L) requested_cores <- 1L
  n_cores <- min(requested_cores, max_cores, ntree)
  if (is.na(n_cores) || n_cores < 1L) n_cores <- 1L
  
  ntree_list <- if (n_cores > 1L) {
    base <- ntree %/% n_cores
    remainder <- ntree %% n_cores
    out <- rep(base, n_cores)
    if (remainder > 0) out[seq_len(remainder)] <- out[seq_len(remainder)] + 1L
    out[out > 0L]
  } else {
    ntree
  }
  
  if (n_cores > 1L && length(ntree_list) < 1L) {
    # Fallback in unexpected edge cases
    n_cores <- 1L
    ntree_list <- ntree
  }
  
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
    .require_or_install(c("foreach", "doParallel"), auto_install)
    
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      try(doParallel::registerDoSEQ(), silent = TRUE)
      try(parallel::stopCluster(cl), silent = TRUE)
    }, add = TRUE)
    
    if (!is.null(seed_int)) {
      parallel::clusterSetRNGStream(cl, seed_int)
    }
    
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
  target_idx_test <- match(target, names(test_df))
  x_test <- test_df[, -target_idx_test, drop = FALSE]
  y_test <- test_df[[target]]
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
      levels_y <- levels(y_test)
      
      # Determine positive class and levels ordering for pROC
      if (!is.null(ctrl$positive_class) && ctrl$positive_class %in% levels_y) {
        positive <- ctrl$positive_class
        negative <- setdiff(levels_y, positive)
        if (length(negative) != 1L) {
          negative <- setdiff(levels_y, positive)[1L]
        }
        roc_levels <- c(negative, positive)
      } else {
        positive <- levels_y[2L]
        roc_levels <- levels_y
      }
      
      # Ensure we have a matching probability column
      if (!positive %in% colnames(probs)) {
        # Fallback to second column if naming mismatch (very unlikely for randomForest)
        positive <- colnames(probs)[min(2L, ncol(probs))]
      }
      
      roc_obj <- pROC::roc(
        response  = y_test,
        predictor = probs[, positive],
        quiet     = TRUE,
        levels    = roc_levels
      )
      auc <- as.numeric(pROC::auc(roc_obj))
    }
    
    metrics <- list(accuracy = acc, kappa = unname(kappa), auc = auc)
    
  } else {
    preds <- as.numeric(stats::predict(rf_model, newdata = x_test))
    err <- preds - y_test
    rmse <- sqrt(mean(err^2))
    mae  <- mean(abs(err))
    sst  <- sum((y_test - mean(y_test))^2)
    r2   <- if (sst > 0) 1 - sum(err^2) / sst else NA_real_
    metrics <- list(RMSE = rmse, MAE = mae, R2 = r2)
  }
  
  # ---- Variable importance ----------------------------------------------------
  importance_df <- NULL
  if (isTRUE(ctrl$importance)) {
    imp <- randomForest::importance(rf_model, type = 1)
    if (!is.null(imp)) {
      importance_scores <- imp[, ncol(imp)]
      importance_df <- data.frame(
        feature = rownames(imp),
        importance = importance_scores,
        row.names = NULL,
        check.names = FALSE
      )
      importance_df <- importance_df[order(-importance_df$importance), , drop = FALSE]
    }
  }
  
  # ---- OOB metrics ------------------------------------------------------------
  oob <- NULL
  if (isTRUE(ctrl$oob)) {
    if (type == "classification" && !is.null(rf_model$err.rate)) {
      oob_acc <- 1 - rf_model$err.rate[rf_model$ntree, "OOB"]
      oob <- list(accuracy = oob_acc)
    } else if (type == "regression" && !is.null(rf_model$mse)) {
      oob <- list(RMSE = sqrt(tail(rf_model$mse, 1)))
    }
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
    feature_names = setdiff(names(train_df), target),
    train_index = as.integer(index),
    control = ctrl
  )
  if (isTRUE(ctrl$return_test_data)) result$test_data <- test_df
  
  class(result) <- c("fs_rf_result", class(result))
  return(result)
}
