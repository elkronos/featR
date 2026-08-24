# Random forest fitting and evaluation for featR.

#' Align factor levels between train and test predictors
#'
#' For every training column that is a factor, the corresponding test column
#' is re-leveled to the training levels. Test values unseen in training become
#' NA (and are later imputed when `control$impute = TRUE`).
#'
#' @param train,test data.frames.
#' @return A list with the aligned `train` and `test` data.frames.
#' @noRd
rf_align_levels <- function(train, test) {
  common <- intersect(names(train), names(test))
  for (nm in common) {
    if (is.factor(train[[nm]])) {
      test[[nm]] <- factor(test[[nm]], levels = levels(train[[nm]]))
    }
  }
  list(train = train, test = test)
}

#' Fit and evaluate a random forest model
#'
#' Splits `data` into train and test sets, optionally preprocesses, imputes,
#' and downsamples, trains a `randomForest` model (optionally across several
#' workers), and evaluates it on the held-out test set.
#'
#' @param data A data.frame or data.table containing predictors and target.
#' @param target Character scalar; name of the target column.
#' @param type One of `"classification"` or `"regression"`.
#' @param control List of options (see Details).
#'
#' @details
#' \strong{control list (defaults)}:
#' \itemize{
#'   \item \code{seed = NULL} (optional; applied for the duration of the call
#'     only, and the previous RNG state is restored on exit)
#'   \item \code{split_ratio = 0.75}
#'   \item \code{sample_size = NULL} (optional downsampling size before split)
#'   \item \code{ntree = 500}
#'   \item \code{importance = TRUE}
#'   \item \code{scale_importance = TRUE} (passed as \code{scale} to
#'     \code{randomForest::importance()}; set to \code{FALSE} for raw,
#'     unscaled permutation importance)
#'   \item \code{mtry = NULL} (defaults: \eqn{\sqrt{p}} for classification,
#'     \eqn{p/3} for regression)
#'   \item \code{nodesize = NULL}
#'   \item \code{maxnodes = NULL}
#'   \item \code{sampsize = NULL}
#'   \item \code{classwt = NULL}
#'   \item \code{strata = NULL}
#'   \item \code{replace = TRUE}
#'   \item \code{n_cores = 1} (capped at the available cores and at
#'     \code{ntree}; values above 1 require the suggested packages 'foreach'
#'     and 'doParallel')
#'   \item \code{preprocess = NULL} (function \code{dt -> dt}, applied before
#'     the split)
#'   \item \code{feature_select = NULL} (function \code{dt -> dt}; must retain
#'     the target; see the warning below)
#'   \item \code{impute = TRUE} (median/mode values learned on the training
#'     data for every predictor and applied to NAs in train and test)
#'   \item \code{drop_zerovar = TRUE} (near-zero variance removal using
#'     training data only)
#'   \item \code{oob = TRUE} (include OOB metrics in the result object; see
#'     the note below about parallel training)
#'   \item \code{return_test_data = FALSE}
#'   \item \code{positive_class = NULL} (optional level name for binary AUC;
#'     defaults to the second factor level)
#' }
#'
#' \strong{Warning -- potential data leakage}: \code{control$feature_select}
#' runs on the \emph{full} dataset \emph{before} the train/test split. Any
#' selection rule that looks at the target therefore sees the test rows, and
#' the reported test metrics can be optimistically biased. Interpret them
#' accordingly; a redesign is deferred to a future release.
#'
#' \strong{OOB metrics and parallel training}: when \code{n_cores > 1} the
#' forest is assembled with \code{randomForest::combine()}, which drops the
#' out-of-bag error structures (\code{err.rate}, \code{mse},
#' \code{confusion}). In that case \code{oob} is returned as \code{NULL} with
#' a warning if \code{control$oob = TRUE}.
#'
#' Character predictors are converted to factors (levels learned on the
#' training split; test values unseen in training become NA and are imputed
#' when \code{control$impute = TRUE}). AUC for binary classification requires
#' the suggested package 'pROC'; when it is not installed a message is emitted
#' and AUC is \code{NA}.
#'
#' @return An object of class \code{fs_rf_result} containing:
#' \itemize{
#'   \item \code{model} : the fitted \code{randomForest} object
#'   \item \code{metrics} : named list of evaluation metrics
#'   \item \code{predictions} : predictions on the test set
#'   \item \code{probabilities} : class probabilities (classification)
#'   \item \code{importance} : variable importance data.frame (if requested)
#'   \item \code{confusion} : confusion matrix (classification)
#'   \item \code{oob} : out-of-bag metrics (if \code{control$oob = TRUE};
#'     \code{NULL} with a warning when trained in parallel)
#'   \item \code{target}, \code{type}, \code{feature_names},
#'     \code{train_index}, \code{control}
#'   \item \code{test_data} : held-out test set (if
#'     \code{return_test_data = TRUE})
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForest", quietly = TRUE) &&
#'     requireNamespace("caret", quietly = TRUE)) {
#'   res <- fs_randomforest(
#'     iris,
#'     target = "Species",
#'     type = "classification",
#'     control = list(ntree = 100, seed = 42)
#'   )
#'   res$metrics
#'   head(res$importance)
#' }
#' }
#' @export
fs_randomforest <- function(data,
                            target,
                            type = c("classification", "regression"),
                            control = list()) {
  # caret is used for splitting, near-zero variance filtering, and Kappa.
  fs_require(c("randomForest", "caret"), "random forest modeling")

  type <- match.arg(type)
  assert_data_frame(data, "data")
  assert_target(data, target, arg = "target")
  if (!is.list(control)) {
    stop("'control' must be a list.", call. = FALSE)
  }

  # ---- Merge control with defaults -------------------------------------------
  ctrl <- utils::modifyList(list(
    seed = NULL,
    split_ratio = 0.75,
    sample_size = NULL,
    ntree = 500,
    importance = TRUE,
    scale_importance = TRUE,
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

  # ---- Validate control values ------------------------------------------------
  assert_number(ctrl$split_ratio, "control$split_ratio")
  if (ctrl$split_ratio <= 0 || ctrl$split_ratio >= 1) {
    stop("'control$split_ratio' must be strictly between 0 and 1.",
         call. = FALSE)
  }
  ntree <- assert_count(ctrl$ntree, "control$ntree", lower = 1L)
  if (!is.null(ctrl$sample_size)) {
    ctrl$sample_size <- assert_count(ctrl$sample_size, "control$sample_size",
                                     lower = 1L)
  }
  if (!is.null(ctrl$mtry)) {
    ctrl$mtry <- assert_count(ctrl$mtry, "control$mtry", lower = 1L)
  }
  if (!is.null(ctrl$nodesize)) {
    ctrl$nodesize <- assert_count(ctrl$nodesize, "control$nodesize",
                                  lower = 1L)
  }
  if (!is.null(ctrl$maxnodes)) {
    ctrl$maxnodes <- assert_count(ctrl$maxnodes, "control$maxnodes",
                                  lower = 2L)
  }
  assert_flag(ctrl$importance, "control$importance")
  assert_flag(ctrl$scale_importance, "control$scale_importance")
  assert_flag(ctrl$replace, "control$replace")
  assert_flag(ctrl$impute, "control$impute")
  assert_flag(ctrl$drop_zerovar, "control$drop_zerovar")
  assert_flag(ctrl$oob, "control$oob")
  assert_flag(ctrl$return_test_data, "control$return_test_data")
  if (!is.null(ctrl$positive_class)) {
    assert_string(ctrl$positive_class, "control$positive_class")
  }
  if (!is.null(ctrl$seed)) {
    assert_number(ctrl$seed, "control$seed")
  }

  # ---- Seed (restored when this function exits) ------------------------------
  local_seed(ctrl$seed)

  # ---- Convert to data.table (always a copy) ---------------------------------
  dt <- as_dt(data)

  # ---- Custom preprocess ------------------------------------------------------
  if (!is.null(ctrl$preprocess)) {
    if (!is.function(ctrl$preprocess)) {
      stop("'control$preprocess' must be a function(dt) -> dt.", call. = FALSE)
    }
    dt <- ctrl$preprocess(dt)
    if (!is.data.frame(dt)) {
      stop("'control$preprocess' must return a data.frame/data.table.",
           call. = FALSE)
    }
    dt <- as_dt(dt)
  }

  # ---- Custom feature selection (see leakage warning in Details) -------------
  if (!is.null(ctrl$feature_select)) {
    if (!is.function(ctrl$feature_select)) {
      stop("'control$feature_select' must be a function(dt) -> dt.",
           call. = FALSE)
    }
    dt <- ctrl$feature_select(dt)
    if (!is.data.frame(dt)) {
      stop("'control$feature_select' must return a data.frame/data.table.",
           call. = FALSE)
    }
    if (!target %in% names(dt)) {
      stop("Feature selection removed the target column.", call. = FALSE)
    }
    dt <- as_dt(dt)
  }

  # ---- Date -> numeric --------------------------------------------------------
  date_cols <- vapply(dt, inherits, logical(1L), what = "Date")
  if (any(date_cols)) {
    idx <- which(date_cols)
    dt[, (idx) := lapply(.SD, as.numeric), .SDcols = idx]
  }

  # ---- Target type and NA handling -------------------------------------------
  if (type == "classification") {
    dt[[target]] <- as.factor(dt[[target]])

    if (anyNA(dt[[target]])) {
      warning("Rows with NA in the classification target were removed before training.",
              call. = FALSE)
      # Single-symbol i is evaluated in calling scope (no column shadowing).
      keep_rows <- !is.na(dt[[target]])
      dt <- dt[keep_rows]
    }

    dt[[target]] <- droplevels(dt[[target]])
    if (nlevels(dt[[target]]) < 2L) {
      stop("Classification target must have at least 2 classes after preprocessing and NA removal.",
           call. = FALSE)
    }
  } else {
    dt[[target]] <- suppressWarnings(as.numeric(dt[[target]]))

    if (anyNA(dt[[target]])) {
      warning("Rows with NA in the regression target (including from coercion) were removed before training.",
              call. = FALSE)
      # Single-symbol i is evaluated in calling scope (no column shadowing).
      keep_rows <- !is.na(dt[[target]])
      dt <- dt[keep_rows]
    }
  }

  if (nrow(dt) == 0L) {
    stop("No rows left after cleaning the target variable.", call. = FALSE)
  }

  # ---- Optional downsampling BEFORE split ------------------------------------
  if (!is.null(ctrl$sample_size)) {
    total_n <- nrow(dt)
    desired_total <- min(ctrl$sample_size, total_n)

    if (type == "classification") {
      # by = c(target) forces evaluation of `target` to the user's column
      # name; a bare `by = target` would grab a column literally named
      # "target" whenever one exists.
      dt <- dt[
        , {
          desired_class <- floor((.N / total_n) * desired_total)
          size_class <- max(1L, min(.N, desired_class))
          .SD[sample(.N, size = size_class)]
        },
        by = c(target)
      ]
    } else {
      dt <- dt[sample(.N, size = desired_total)]
    }
  }

  # ---- Train/test split (plain integer index) --------------------------------
  index <- fs_split_index(dt[[target]], p = ctrl$split_ratio)
  # Subset with data.frame semantics so no data.table NSE lookup is involved.
  df_all <- as.data.frame(dt)
  train_df <- df_all[index, , drop = FALSE]
  test_df <- df_all[-index, , drop = FALSE]

  if (nrow(test_df) == 0L) {
    stop("Test split is empty; adjust 'control$split_ratio'.", call. = FALSE)
  }

  # ---- Character predictors -> factors (randomForest rejects characters) -----
  predictor_cols <- setdiff(names(train_df), target)
  for (nm in predictor_cols) {
    if (is.character(train_df[[nm]])) {
      train_df[[nm]] <- factor(train_df[[nm]])
    }
  }

  # ---- Align factor levels between train and test -----------------------------
  aligned <- rf_align_levels(train_df, test_df)
  train_df <- aligned$train
  test_df <- aligned$test

  # ---- Zero-variance removal (train-only stats) ------------------------------
  if (isTRUE(ctrl$drop_zerovar)) {
    predictor_cols_train <- setdiff(names(train_df), target)
    if (length(predictor_cols_train) > 0L) {
      nzv <- caret::nearZeroVar(
        train_df[, predictor_cols_train, drop = FALSE],
        saveMetrics = TRUE
      )
      drop_cols <- rownames(nzv)[nzv$zeroVar | nzv$nzv]
      for (dc in drop_cols) {
        train_df[[dc]] <- NULL
        if (dc %in% names(test_df)) {
          test_df[[dc]] <- NULL
        }
      }
    }
  }

  # ---- Imputation (train-only stats, applied to both) ------------------------
  if (isTRUE(ctrl$impute)) {
    feat_cols <- setdiff(names(train_df), target)
    if (length(feat_cols) > 0L) {
      # Learn imputation values for EVERY predictor from the training data,
      # not only for columns with NAs in train: the test set can contain NAs
      # of its own, including NAs introduced by unseen factor levels during
      # level alignment.
      impute_values <- vector("list", length(feat_cols))
      names(impute_values) <- feat_cols

      for (i in seq_along(feat_cols)) {
        nm <- feat_cols[i]
        x <- train_df[[nm]]
        if (is.numeric(x)) {
          impute_values[[i]] <- stats::median(x, na.rm = TRUE)
        } else if (is.factor(x) || is.character(x)) {
          tab <- table(x, useNA = "no")
          impute_values[[i]] <- if (length(tab) > 0L) names(which.max(tab)) else NA
        } else {
          impute_values[[i]] <- NA
        }
      }

      # Apply to any NA in train or test.
      for (i in seq_along(feat_cols)) {
        nm <- feat_cols[i]
        val <- impute_values[[i]]
        if (length(val) != 1L || is.na(val)) next

        x_tr <- train_df[[nm]]
        idx_na_tr <- is.na(x_tr)
        if (any(idx_na_tr)) {
          x_tr[idx_na_tr] <- val
          train_df[[nm]] <- x_tr
        }

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
    stop("Target column missing from training data after preprocessing.",
         call. = FALSE)
  }

  target_idx <- match(target, names(train_df))
  x_train <- train_df[, -target_idx, drop = FALSE]
  y_train <- train_df[[target]]

  p <- ncol(x_train)
  if (is.na(p) || p < 1L) {
    stop("No predictor columns left after preprocessing/feature selection/NZV removal.",
         call. = FALSE)
  }

  # ---- Compute mtry safely ----------------------------------------------------
  mtry_eff <- ctrl$mtry
  if (is.null(mtry_eff)) {
    mtry_eff <- if (type == "classification") {
      max(1L, floor(sqrt(p)))
    } else {
      max(1L, floor(p / 3))
    }
  }
  mtry_eff <- min(max(1L, as.integer(mtry_eff)), p)

  # ---- Compute sampsize safely ------------------------------------------------
  sampsize_eff <- ctrl$sampsize
  if (!is.null(sampsize_eff)) {
    if (type == "regression" && length(sampsize_eff) > 1L) {
      stop("'control$sampsize' must be a single scalar for regression.",
           call. = FALSE)
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

  # ---- Worker count (sequential by default; capped at cores and ntree) -------
  n_cores <- resolve_cores(ctrl$n_cores, arg = "control$n_cores")
  n_cores <- min(n_cores, ntree)

  ntree_list <- if (n_cores > 1L) {
    base <- ntree %/% n_cores
    remainder <- ntree %% n_cores
    out <- rep(base, n_cores)
    if (remainder > 0L) {
      out[seq_len(remainder)] <- out[seq_len(remainder)] + 1L
    }
    out[out > 0L]
  } else {
    ntree
  }

  if (n_cores > 1L && length(ntree_list) < 1L) {
    # Fallback in unexpected edge cases.
    n_cores <- 1L
    ntree_list <- ntree
  }

  # ---- randomForest args ------------------------------------------------------
  rf_args <- list(
    x = x_train,
    y = y_train,
    ntree = NULL, # set per-worker or serially below
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
    fs_require(c("foreach", "doParallel"), "parallel random forest training")

    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      try(foreach::registerDoSEQ(), silent = TRUE)
      try(parallel::stopCluster(cl), silent = TRUE)
    }, add = TRUE)

    if (!is.null(ctrl$seed)) {
      parallel::clusterSetRNGStream(cl, as.integer(ctrl$seed))
    }

    ntree_part <- NULL # foreach iteration variable; quiets R CMD check
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
  x_test <- as.data.frame(test_df[, -target_idx_test, drop = FALSE])
  y_test <- test_df[[target]]

  metrics <- list()
  preds <- NULL
  probs <- NULL
  confusion <- NULL

  if (type == "classification") {
    preds <- stats::predict(rf_model, newdata = x_test, type = "class")

    suppressWarnings({
      probs <- try(stats::predict(rf_model, newdata = x_test, type = "prob"),
                   silent = TRUE)
      if (inherits(probs, "try-error")) probs <- NULL
    })

    acc <- mean(preds == y_test)

    kappa <- try(caret::confusionMatrix(preds, y_test)$overall[["Kappa"]],
                 silent = TRUE)
    if (inherits(kappa, "try-error") || is.null(kappa)) kappa <- NA_real_

    confusion <- table(Observed = y_test, Predicted = preds)

    auc <- NA_real_
    if (nlevels(y_test) == 2L && !is.null(probs)) {
      if (requireNamespace("pROC", quietly = TRUE)) {
        levels_y <- levels(y_test)

        # Determine positive class and level ordering for pROC.
        if (!is.null(ctrl$positive_class) &&
            ctrl$positive_class %in% levels_y) {
          positive <- ctrl$positive_class
          negative <- setdiff(levels_y, positive)
          roc_levels <- c(negative, positive)
        } else {
          positive <- levels_y[2L]
          roc_levels <- levels_y
        }

        # Ensure we have a matching probability column.
        if (!positive %in% colnames(probs)) {
          positive <- colnames(probs)[min(2L, ncol(probs))]
        }

        roc_obj <- pROC::roc(
          response = y_test,
          predictor = probs[, positive],
          quiet = TRUE,
          levels = roc_levels,
          direction = "<"
        )
        auc <- as.numeric(pROC::auc(roc_obj))
      } else {
        message("Package 'pROC' is not installed; AUC was not computed.")
      }
    }

    metrics <- list(accuracy = acc, kappa = unname(kappa), auc = auc)
  } else {
    preds <- as.numeric(stats::predict(rf_model, newdata = x_test))
    err <- preds - y_test
    rmse <- sqrt(mean(err^2))
    mae <- mean(abs(err))
    sst <- sum((y_test - mean(y_test))^2)
    r2 <- if (sst > 0) 1 - sum(err^2) / sst else NA_real_
    metrics <- list(RMSE = rmse, MAE = mae, R2 = r2)
  }

  # ---- Variable importance ----------------------------------------------------
  importance_df <- NULL
  if (isTRUE(ctrl$importance)) {
    imp <- randomForest::importance(rf_model, type = 1,
                                    scale = isTRUE(ctrl$scale_importance))
    if (!is.null(imp)) {
      importance_scores <- imp[, ncol(imp)]
      importance_df <- data.frame(
        feature = rownames(imp),
        importance = importance_scores,
        row.names = NULL,
        check.names = FALSE
      )
      importance_df <- importance_df[order(-importance_df$importance), ,
                                     drop = FALSE]
    }
  }

  # ---- OOB metrics ------------------------------------------------------------
  oob <- NULL
  if (isTRUE(ctrl$oob)) {
    if (n_cores > 1L) {
      warning(paste(
        "OOB metrics are unavailable when the forest is trained in parallel:",
        "randomForest::combine() drops err.rate/mse/confusion.",
        "Returning NULL 'oob'; use n_cores = 1 to obtain OOB metrics."
      ), call. = FALSE)
    } else if (type == "classification" && !is.null(rf_model$err.rate)) {
      oob <- list(accuracy = 1 - rf_model$err.rate[rf_model$ntree, "OOB"])
    } else if (type == "regression" && !is.null(rf_model$mse)) {
      oob <- list(RMSE = sqrt(utils::tail(rf_model$mse, 1L)))
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
  if (isTRUE(ctrl$return_test_data)) {
    result$test_data <- test_df
  }

  class(result) <- c("fs_rf_result", class(result))
  result
}
