# ============================
# MARS Utilities (revised)
# ============================

# NOTE:
# - Required packages are validated via check_libraries()
# - This file uses fully-qualified package references where appropriate
# - For classification, the FIRST factor level is treated as the "positive" class
#   for ROC/PR AUC calculations and for caret::twoClassSummary.

# ---------------------------
# Utility Functions
# ---------------------------

#' Check Required Libraries
#'
#' Ensures required and optional packages are installed.
#'
#' @return invisible(NULL)
#' @export
check_libraries <- function() {
  required_libs <- c("earth", "caret", "data.table")
  opt_libs <- c("doParallel", "pROC", "PRROC")
  for (lib in required_libs) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      stop(sprintf("Package '%s' is not installed. Please install it before proceeding.", lib))
    }
  }
  for (lib in opt_libs) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      message(sprintf("Note: optional package '%s' not found. Some features may be disabled.", lib))
    }
  }
  invisible(NULL)
}

#' Check Response Column
#' @export
check_response_column <- function(data, responseName) {
  if (!(responseName %in% colnames(data))) {
    stop("The specified response column does not exist in the dataset.")
  }
  invisible(NULL)
}

#' Coerce Response
#'
#' Converts character response to factor; ensures supported types.
#' Optionally sanitizes factor levels with make.names().
#'
#' @param data data.frame or data.table.
#' @param responseName character. Name of response column.
#' @param make_factor_names logical. If TRUE, factor levels are made syntactically valid.
#'
#' @export
coerce_response <- function(data, responseName, make_factor_names = TRUE) {
  y <- data[[responseName]]
  if (is.character(y)) {
    message(sprintf("Coercing character response '%s' to factor.", responseName))
    data[[responseName]] <- factor(y)
  }
  if (is.factor(data[[responseName]]) && make_factor_names) {
    lev <- levels(data[[responseName]])
    levels(data[[responseName]]) <- make.names(lev)
  }
  if (!is.factor(data[[responseName]]) && !is.numeric(data[[responseName]])) {
    stop("Response must be numeric (regression) or factor (classification).")
  }
  data
}

#' Handle Missing Values
#' @export
handle_missing_values <- function(data) {
  initial_rows <- nrow(data)
  data_clean <- stats::na.omit(data)
  message(sprintf("Removed %d rows with missing values.", initial_rows - nrow(data_clean)))
  return(data_clean)
}

#' Check Class Balance
#' @export
check_class_balance <- function(data, responseName, show_warnings = TRUE) {
  if (is.factor(data[[responseName]])) {
    counts <- table(data[[responseName]])
    if (any(counts < 2)) {
      stop("Each class must have at least two samples.")
    }
    if (any(counts < 10) && show_warnings) {
      warning("Some classes have fewer than 10 samples. Oversampling will be used to balance classes.")
    }
  }
  invisible(NULL)
}

#' Sample Data
#'
#' Down-samples to at most sampleSize rows (randomly).
#'
#' @export
sample_data <- function(data, sampleSize, seed) {
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  if (nrow(data) > sampleSize) {
    set.seed(seed)
    data <- data[sample(.N, sampleSize)]
    message(sprintf("Data sampled down to %d rows.", sampleSize))
  }
  data
}

#' Split Data (stratified for both regression & classification)
#' @export
split_data <- function(data, responseName, train_prop, seed) {
  set.seed(seed)
  idx <- caret::createDataPartition(y = data[[responseName]], p = train_prop, list = FALSE)
  list(train = data[idx, ], test = data[-idx, ])
}

#' Balance Classes (simple upsampling)
#' @export
balance_classes <- function(train, responseName) {
  if (!data.table::is.data.table(train)) {
    train <- data.table::as.data.table(train)
  }
  if (is.factor(train[[responseName]])) {
    counts <- table(train[[responseName]])
    max_count <- max(counts)
    balanced_list <- lapply(names(counts), function(cl) {
      subset_data <- train[train[[responseName]] == cl]
      if (nrow(subset_data) < max_count) {
        subset_data[sample(1:.N, max_count, replace = TRUE)]
      } else {
        subset_data
      }
    })
    train_balanced <- data.table::rbindlist(balanced_list)
    train_balanced <- train_balanced[sample(.N)]
    message(sprintf("Balanced training set size: %d rows.", nrow(train_balanced)))
    return(train_balanced)
  }
  train
}

#' Define Hyperparameter Grid
#' @export
define_hyperparameter_grid <- function(degree, nprune) {
  expand.grid(
    nprune = unique(sort(nprune)),
    degree = unique(sort(degree))
  )
}

#' Preprocessing: remove NZV and high-correlation predictors
#'
#' @param train data.table. Training data.
#' @param test data.table. Test data.
#' @param responseName character. Response column name.
#' @param corr_cut numeric correlation cutoff (0 disables).
#' @param remove_nzv logical. Remove near-zero-variance predictors.
#'
#' @export
preprocess_predictors <- function(train, test, responseName,
                                  corr_cut = 0.95,
                                  remove_nzv = TRUE) {
  if (!data.table::is.data.table(train)) {
    train <- data.table::as.data.table(train)
  }
  if (!data.table::is.data.table(test)) {
    test <- data.table::as.data.table(test)
  }
  
  pred_cols <- setdiff(colnames(train), responseName)
  nzv_removed <- character()
  corr_removed <- character()
  
  if (remove_nzv && length(pred_cols) > 0) {
    nzv <- caret::nearZeroVar(train[, ..pred_cols], saveMetrics = TRUE)
    rm_idx <- which(nzv$nzv | nzv$zeroVar)
    if (length(rm_idx)) {
      nzv_removed <- rownames(nzv)[rm_idx]
      keep <- setdiff(pred_cols, nzv_removed)
      cols <- c(keep, responseName)
      train <- train[, ..cols]
      test  <- test[, ..cols]
      pred_cols <- keep
      message(sprintf("Removed %d near/zero-variance predictors.", length(nzv_removed)))
    }
  }
  
  if (is.numeric(corr_cut) && corr_cut > 0 && length(pred_cols) > 1) {
    num_cols <- pred_cols[vapply(train[, ..pred_cols], is.numeric, logical(1))]
    if (length(num_cols) > 1) {
      cmat <- stats::cor(train[, ..num_cols], use = "pairwise.complete.obs")
      high <- caret::findCorrelation(cmat, cutoff = corr_cut, verbose = FALSE)
      if (length(high)) {
        corr_removed <- num_cols[high]
        keep <- setdiff(pred_cols, corr_removed)
        cols <- c(keep, responseName)
        train <- train[, ..cols]
        test  <- test[, ..cols]
        message(sprintf(
          "Removed %d highly correlated numeric predictors (cutoff=%.2f).",
          length(corr_removed), corr_cut
        ))
      }
    }
  }
  
  list(
    train = train,
    test  = test,
    removed = list(nzv = nzv_removed, corr = corr_removed)
  )
}

#' Define Training Control
#'
#' @param number integer. CV folds.
#' @param repeats integer. CV repeats.
#' @param search character. "grid" or "random". (Note: with an explicit tuneGrid,
#'   caret will effectively do grid search even if search = "random".)
#' @param train data.table. Training data.
#' @param responseName character. Response column.
#' @param seed integer. RNG seed.
#' @param tune_grid_n integer or NULL. Number of tuning parameter combinations;
#'   if NULL, seeds are not set.
#' @param verbose_iter logical. If TRUE, caret prints fold progress.
#'
#' @export
define_train_control <- function(number, repeats, search, train, responseName, seed,
                                 tune_grid_n = NULL, verbose_iter = FALSE) {
  y <- train[[responseName]]
  is_class  <- is.factor(y)
  is_binary <- is_class && length(levels(y)) == 2
  
  # Select summary function
  if (is_binary && requireNamespace("pROC", quietly = TRUE)) {
    summary_fun <- caret::twoClassSummary
  } else if (is_class &&
             !is_binary &&
             "multiClassSummary" %in% getNamespaceExports("caret")) {
    summary_fun <- caret::multiClassSummary
  } else {
    summary_fun <- caret::defaultSummary
  }
  
  # Seeds only if we know grid size (to avoid caret seed mismatch warnings)
  seeds <- NULL
  if (!is.null(tune_grid_n) && is.finite(tune_grid_n) && tune_grid_n > 0) {
    total_resamples <- number * repeats
    set.seed(seed)
    seeds <- vector(mode = "list", length = total_resamples + 1)
    for (i in seq_len(total_resamples)) {
      seeds[[i]] <- sample.int(1e6, tune_grid_n)
    }
    seeds[[total_resamples + 1]] <- sample.int(1e6, 1)
  }
  
  ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = number,
    repeats = repeats,
    search = search,
    allowParallel = TRUE,
    savePredictions = "final",
    classProbs = is_class,
    returnResamp = "all",
    summaryFunction = summary_fun,
    verboseIter = verbose_iter,
    seeds = seeds
  )
  
  # Defensive: ensure correct class (caret already does this, but safe)
  if (!inherits(ctrl, "trainControl")) {
    class(ctrl) <- c("trainControl", class(ctrl))
  }
  
  ctrl
}

#' Train Model
#'
#' @export
train_model <- function(train, responseName, method, ctrl, hyperParameters,
                        seed, metric = NULL) {
  set.seed(seed)
  
  # Determine metric if not specified
  if (is.null(metric)) {
    if (is.factor(train[[responseName]]) &&
        length(levels(train[[responseName]])) == 2 &&
        identical(ctrl$summaryFunction, caret::twoClassSummary)) {
      metric <- "ROC"
    } else if (is.factor(train[[responseName]])) {
      metric <- "Accuracy"
    } else {
      metric <- "RMSE"
    }
  }
  
  fmla <- stats::as.formula(sprintf("`%s` ~ .", responseName))
  
  model <- tryCatch({
    caret::train(
      fmla,
      data      = train,
      method    = method,
      trControl = ctrl,
      tuneGrid  = hyperParameters,
      metric    = metric,
      preProcess = c("center", "scale")
    )
  }, error = function(e) {
    stop(sprintf("Error during model training: %s", conditionMessage(e)))
  })
  model
}

#' Evaluate Model
#'
#' Computes predictions and metrics; for binary classification, if the model
#' was trained with twoClassSummary, the FIRST factor level is treated as the
#' positive class for ROC and PR AUC.
#'
#' @export
evaluate_model <- function(model, test, responseName) {
  pred <- stats::predict(model, newdata = test)
  out  <- list(model = model, predictions = pred)
  
  # Regression
  if (is.numeric(test[[responseName]])) {
    obs      <- test[[responseName]]
    rmse_val <- sqrt(mean((obs - pred)^2))
    mae_val  <- mean(abs(obs - pred))
    r2_val   <- caret::R2(pred, obs)
    out$metrics <- list(RMSE = rmse_val, MAE = mae_val, R2 = r2_val)
    
    # Classification
  } else if (is.factor(test[[responseName]])) {
    pred <- factor(pred, levels = levels(test[[responseName]]))
    cm <- caret::confusionMatrix(pred, test[[responseName]])
    out$metrics <- list(
      Accuracy = unname(cm$overall["Accuracy"]),
      Kappa    = unname(cm$overall["Kappa"])
    )
    out$confusion_matrix <- cm$table
    
    # Binary classification: ROC/PR AUC if twoClassSummary was used
    if (length(levels(test[[responseName]])) == 2 &&
        isTRUE(identical(model$control$summaryFunction, caret::twoClassSummary))) {
      
      # Probability predictions (required)
      probs <- tryCatch(
        stats::predict(model, newdata = test, type = "prob"),
        error = function(e) NULL
      )
      
      if (!is.null(probs)) {
        # Positive class = FIRST factor level (consistent with twoClassSummary)
        pos_level <- levels(test[[responseName]])[1]
        
        if (pos_level %in% colnames(probs)) {
          # ROC AUC via pROC, if available
          if (requireNamespace("pROC", quietly = TRUE)) {
            roc_obj <- pROC::roc(
              response  = test[[responseName]],
              predictor = probs[[pos_level]],
              quiet     = TRUE
            )
            out$metrics$ROC_AUC <- as.numeric(pROC::auc(roc_obj))
          }
          
          # PR AUC via PRROC, if available
          if (requireNamespace("PRROC", quietly = TRUE)) {
            labs_pos <- test[[responseName]] == pos_level
            scores_pos <- probs[[pos_level]][labs_pos]
            scores_neg <- probs[[pos_level]][!labs_pos]
            
            # Only compute if both classes are present in test
            if (length(scores_pos) > 0L && length(scores_neg) > 0L) {
              pr <- PRROC::pr.curve(
                scores.class0 = scores_pos,
                scores.class1 = scores_neg,
                curve = FALSE
              )
              out$metrics$PR_AUC <- unname(pr$auc.integral)
            }
          }
        }
      }
    }
    
  } else {
    stop("Unsupported response variable type.")
  }
  
  # Variable importance (silently ignore if unsupported)
  vi_ok <- tryCatch({
    vi <- caret::varImp(model)
    out$varimp <- vi
    TRUE
  }, error = function(e) FALSE)
  
  out
}

#' Start Parallel Backend (optional)
#'
#' @export
maybe_register_parallel <- function(verbose = TRUE) {
  if (requireNamespace("doParallel", quietly = TRUE)) {
    n_cores <- parallel::detectCores()
    if (is.na(n_cores) || n_cores < 2L) {
      cores <- 1L
    } else {
      cores <- n_cores - 1L
    }
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    if (verbose) {
      message(sprintf("Parallel backend registered with %d worker(s).", cores))
    }
    return(cl)
  } else {
    if (verbose) message("doParallel not installed; running single-threaded.")
    return(NULL)
  }
}

#' Stop Parallel Backend
#'
#' @export
stop_parallel <- function(cl) {
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  }
  invisible(NULL)
}

#' MARS Feature Selection and Model Training
#'
#' @param data data.frame or data.table.
#' @param responseName character. Response column name.
#' @param p numeric. Train proportion (default 0.8).
#' @param degree integer vector. Interaction degrees (default 1:3).
#' @param nprune integer vector. Terms to prune (default c(5,10,15)).
#' @param method character. Model method (default "earth").
#' @param search character. Hyperparameter search method (default "grid").
#' @param number integer. CV folds (default 5).
#' @param repeats integer. CV repeats (default 3).
#' @param seed integer. Random seed (default 123).
#' @param sampleSize integer. Max samples (default 10000).
#' @param show_warnings logical. Warn on imbalance (default TRUE).
#' @param verbose logical. Messages (default TRUE).
#' @param corr_cut numeric. Correlation cutoff (default 0.95; 0 disables).
#' @param remove_nzv logical. Remove near-zero-variance predictors (default TRUE).
#' @param verbose_iter logical. If TRUE, caret prints fold progress.
#'
#' @return list with model, metrics, predictions, varimp, and preprocessing report.
#' @export
fs_mars <- function(data, responseName,
                    p = 0.8,
                    degree = 1:3,
                    nprune = c(5, 10, 15),
                    method = "earth",
                    search = "grid",
                    number = 5,
                    repeats = 3,
                    seed = 123,
                    sampleSize = 10000,
                    show_warnings = TRUE,
                    verbose = TRUE,
                    corr_cut = 0.95,
                    remove_nzv = TRUE,
                    verbose_iter = FALSE) {
  
  if (verbose) message("Verifying required libraries...")
  check_libraries()
  
  # Ensure data.table
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
    if (verbose) message("Converted input data to data.table.")
  }
  
  if (verbose) message("Checking response column existence...")
  check_response_column(data, responseName)
  
  # Coerce response and sanitize factor levels if needed
  data <- coerce_response(data, responseName, make_factor_names = TRUE)
  
  # Remove rows with missing values
  data <- handle_missing_values(data)
  
  # Optional down-sampling BEFORE class-balance checks and splitting
  data <- sample_data(data, sampleSize, seed)
  
  # Check global class balance after sampling
  check_class_balance(data, responseName, show_warnings)
  
  if (verbose) message("Splitting data into training and test sets...")
  splits <- split_data(data, responseName, p, seed)
  train  <- splits$train
  test   <- splits$test
  if (verbose) {
    message(sprintf("Training set: %d rows; Test set: %d rows.", nrow(train), nrow(test)))
  }
  
  # Check class balance in training set (for classification only)
  check_class_balance(train, responseName, show_warnings)
  
  # Simple upsampling for class imbalance
  train <- balance_classes(train, responseName)
  
  # Predictor preprocessing (NZV, correlation)
  pp <- preprocess_predictors(train, test, responseName,
                              corr_cut = corr_cut,
                              remove_nzv = remove_nzv)
  train <- pp$train
  test  <- pp$test
  
  # Align factor levels between train and test for classification
  if (is.factor(train[[responseName]])) {
    test[[responseName]] <- factor(test[[responseName]],
                                   levels = levels(train[[responseName]]))
  }
  
  # Hyperparameter grid and trainControl
  hyperParameters <- define_hyperparameter_grid(degree, nprune)
  ctrl <- define_train_control(
    number       = number,
    repeats      = repeats,
    search       = search,
    train        = train,
    responseName = responseName,
    seed         = seed,
    tune_grid_n  = nrow(hyperParameters),
    verbose_iter = verbose_iter
  )
  
  # Parallel backend (optional)
  cl <- maybe_register_parallel(verbose)
  
  if (verbose) message("Training the model...")
  t_train <- system.time({
    model <- train_model(
      train           = train,
      responseName    = responseName,
      method          = method,
      ctrl            = ctrl,
      hyperParameters = hyperParameters,
      seed            = seed,
      metric          = NULL
    )
  })
  
  if (!is.null(cl)) stop_parallel(cl)
  
  if (verbose) {
    message(sprintf("Training completed in %.2f seconds.", t_train[["elapsed"]]))
  }
  
  if (verbose) message("Evaluating model performance...")
  eval_metrics <- evaluate_model(model, test, responseName)
  eval_metrics$preprocessing <- list(removed_predictors = pp$removed)
  
  if (verbose) message("Model training and evaluation complete.")
  return(eval_metrics)
}
