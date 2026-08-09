# MARS (earth) feature selection for featR.
# Suggests: caret, earth (always); pROC and PRROC are optional extras
# (soft-checked via requireNamespace); doParallel + foreach only when
# n_cores > 1. For classification, the FIRST factor level is treated as the
# "positive" class for ROC/PR AUC and caret::twoClassSummary.

#' Print a progress message when verbose
#' @noRd
mars_message <- function(msg, verbose) {
  if (isTRUE(verbose)) message(msg)
}

#' Coerce the response column in place
#'
#' Converts character responses to factor and (optionally) sanitizes factor
#' levels with `make.names(unique = TRUE)` so distinct classes such as
#' "class 1" and "class.1" can never be merged.
#'
#' @param data data.table (already a private copy).
#' @param responseName Character. Response column name.
#' @param make_factor_names Logical. Sanitize factor levels.
#' @param verbose Logical. Gate progress messages.
#' @return The data.table with a coerced response column.
#' @noRd
mars_coerce_response <- function(data, responseName, make_factor_names = TRUE,
                                 verbose = TRUE) {
  y <- data[[responseName]]
  if (is.character(y)) {
    mars_message(sprintf("Coercing character response '%s' to factor.", responseName), verbose)
    y <- factor(y)
    data.table::set(data, j = responseName, value = y)
  }
  if (is.factor(y) && make_factor_names) {
    lev <- levels(y)
    levels(y) <- make.names(lev, unique = TRUE)
    data.table::set(data, j = responseName, value = y)
  }
  if (!is.factor(data[[responseName]]) && !is.numeric(data[[responseName]])) {
    stop("Response must be numeric (regression) or factor (classification).")
  }
  data
}

#' Drop rows with missing values
#' @noRd
mars_drop_missing <- function(data, verbose = TRUE) {
  initial_rows <- nrow(data)
  data_clean <- stats::na.omit(data)
  mars_message(sprintf("Removed %d rows with missing values.", initial_rows - nrow(data_clean)), verbose)
  data_clean
}

#' Check class balance for classification responses
#' @noRd
mars_check_class_balance <- function(data, responseName, show_warnings = TRUE) {
  if (is.factor(data[[responseName]])) {
    counts <- table(data[[responseName]])
    if (any(counts < 2)) {
      stop("Each class must have at least two samples.")
    }
    if (any(counts < 10) && show_warnings) {
      warning("Some classes have fewer than 10 samples. Per-fold upsampling will be applied during resampling.")
    }
  }
  invisible(NULL)
}

#' Randomly down-sample to at most sampleSize rows
#' @noRd
mars_sample_rows <- function(data, sampleSize, seed, verbose = TRUE) {
  if (nrow(data) > sampleSize) {
    local_seed(seed)
    .mars_row_idx <- sample(nrow(data), sampleSize)
    data <- data[.mars_row_idx]
    mars_message(sprintf("Data sampled down to %d rows.", sampleSize), verbose)
  }
  data
}

#' Hyperparameter grid for earth
#' @noRd
mars_hyper_grid <- function(degree, nprune) {
  expand.grid(
    nprune = unique(sort(nprune)),
    degree = unique(sort(degree))
  )
}

#' Remove near-zero-variance and highly correlated predictors
#'
#' @param train data.table. Training data.
#' @param test data.table. Test data.
#' @param responseName Character. Response column name.
#' @param corr_cut Numeric correlation cutoff (0 disables).
#' @param remove_nzv Logical. Remove near-zero-variance predictors.
#' @param verbose Logical. Gate progress messages.
#' @return A list with train, test, and a report of removed predictors.
#' @noRd
mars_preprocess <- function(train, test, responseName,
                            corr_cut = 0.95,
                            remove_nzv = TRUE,
                            verbose = TRUE) {
  pred_cols <- setdiff(colnames(train), responseName)
  nzv_removed <- character()
  corr_removed <- character()

  if (remove_nzv && length(pred_cols) > 0L) {
    nzv <- caret::nearZeroVar(train[, pred_cols, with = FALSE], saveMetrics = TRUE)
    rm_idx <- which(nzv$nzv | nzv$zeroVar)
    if (length(rm_idx)) {
      nzv_removed <- rownames(nzv)[rm_idx]
      keep <- setdiff(pred_cols, nzv_removed)
      cols <- c(keep, responseName)
      train <- train[, cols, with = FALSE]
      test <- test[, cols, with = FALSE]
      pred_cols <- keep
      mars_message(sprintf("Removed %d near/zero-variance predictors.", length(nzv_removed)), verbose)
    }
  }

  if (is.numeric(corr_cut) && corr_cut > 0 && length(pred_cols) > 1L) {
    num_cols <- pred_cols[vapply(train[, pred_cols, with = FALSE], is.numeric, logical(1L))]
    if (length(num_cols) > 1L) {
      cmat <- stats::cor(train[, num_cols, with = FALSE], use = "pairwise.complete.obs")
      high <- caret::findCorrelation(cmat, cutoff = corr_cut, verbose = FALSE)
      if (length(high)) {
        corr_removed <- num_cols[high]
        keep <- setdiff(pred_cols, corr_removed)
        cols <- c(keep, responseName)
        train <- train[, cols, with = FALSE]
        test <- test[, cols, with = FALSE]
        mars_message(sprintf(
          "Removed %d highly correlated numeric predictors (cutoff=%.2f).",
          length(corr_removed), corr_cut
        ), verbose)
      }
    }
  }

  list(
    train = train,
    test = test,
    removed = list(nzv = nzv_removed, corr = corr_removed)
  )
}

#' Build the caret::trainControl object
#'
#' For classification, `sampling = "up"` is set so that any class imbalance
#' is handled by upsampling INSIDE each resample (no pre-CV upsampling, which
#' would leak duplicated rows across folds). Deterministic per-resample seed
#' lists are only constructed when `seed` is not NULL.
#'
#' @param number Integer. CV folds.
#' @param repeats Integer. CV repeats.
#' @param search Character. "grid" or "random".
#' @param train data.table. Training data.
#' @param responseName Character. Response column.
#' @param seed Optional whole number; NULL means no seeding.
#' @param tune_n Integer or NULL. Number of tuning combinations evaluated per
#'   resample (grid rows for "grid", tuneLength for "random"); when NULL,
#'   seeds are not set.
#' @param verbose_iter Logical. If TRUE, caret prints fold progress.
#' @param verbose Logical. Gate progress messages.
#' @return A list as returned by `caret::trainControl()`.
#' @noRd
mars_train_control <- function(number, repeats, search, train, responseName,
                               seed, tune_n = NULL, verbose_iter = FALSE,
                               verbose = TRUE) {
  y <- train[[responseName]]
  is_class <- is.factor(y)
  is_binary <- is_class && length(levels(y)) == 2L

  # Select summary function
  if (is_binary && requireNamespace("pROC", quietly = TRUE)) {
    summary_fun <- caret::twoClassSummary
  } else if (is_class &&
             !is_binary &&
             "multiClassSummary" %in% getNamespaceExports("caret")) {
    summary_fun <- caret::multiClassSummary
  } else {
    if (is_binary) {
      mars_message("Optional package 'pROC' not installed; using accuracy-based resampling summary instead of ROC.", verbose)
    }
    summary_fun <- caret::defaultSummary
  }

  # Deterministic resampling seeds only when the user asked for a seed
  seeds <- NULL
  if (!is.null(seed) && !is.null(tune_n) && is.finite(tune_n) && tune_n > 0) {
    local_seed(seed)
    total_resamples <- number * repeats
    seeds <- vector(mode = "list", length = total_resamples + 1L)
    for (i in seq_len(total_resamples)) {
      seeds[[i]] <- sample.int(1e6, tune_n)
    }
    seeds[[total_resamples + 1L]] <- sample.int(1e6, 1L)
  }

  caret::trainControl(
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
    sampling = if (is_class) "up" else NULL,
    seeds = seeds
  )
}

#' Train the model via caret
#'
#' When `search = "random"`, `tuneLength` controls the number of random
#' hyperparameter combinations and no fixed grid is passed; when
#' `search = "grid"`, the explicit grid is used.
#'
#' @noRd
mars_train_model <- function(train, responseName, method, ctrl,
                             hyperParameters, tune_length, search,
                             seed, metric = NULL) {
  local_seed(seed)

  # Determine metric if not specified
  if (is.null(metric)) {
    if (is.factor(train[[responseName]]) &&
        length(levels(train[[responseName]])) == 2L &&
        identical(ctrl$summaryFunction, caret::twoClassSummary)) {
      metric <- "ROC"
    } else if (is.factor(train[[responseName]])) {
      metric <- "Accuracy"
    } else {
      metric <- "RMSE"
    }
  }

  fmla <- stats::as.formula(sprintf("`%s` ~ .", responseName))

  train_args <- list(
    fmla,
    data = train,
    method = method,
    trControl = ctrl,
    metric = metric,
    preProcess = c("center", "scale")
  )
  if (identical(search, "random")) {
    train_args$tuneLength <- tune_length
  } else {
    train_args$tuneGrid <- hyperParameters
  }

  tryCatch(
    do.call(caret::train, train_args),
    error = function(e) {
      stop(sprintf("Model training failed: %s", conditionMessage(e)))
    }
  )
}

#' Evaluate a fitted model on the test set
#'
#' For binary classification trained with `caret::twoClassSummary`, the FIRST
#' factor level is the positive class. ROC AUC is computed with explicit
#' `levels = c(negative, positive)` and `direction = "<"` so worse-than-chance
#' models honestly score below 0.5 instead of being silently flipped.
#'
#' @noRd
mars_evaluate <- function(model, test, responseName, verbose = TRUE) {
  pred <- stats::predict(model, newdata = test)
  out <- list(model = model, predictions = pred)

  # Regression
  if (is.numeric(test[[responseName]])) {
    obs <- test[[responseName]]
    rmse_val <- sqrt(mean((obs - pred)^2))
    mae_val <- mean(abs(obs - pred))
    # Note: caret::R2 is the squared correlation between predicted and
    # observed, not 1 - SSE/SST.
    r2_val <- caret::R2(pred, obs)
    out$metrics <- list(RMSE = rmse_val, MAE = mae_val, R2 = r2_val)

    # Classification
  } else if (is.factor(test[[responseName]])) {
    pred <- factor(pred, levels = levels(test[[responseName]]))
    cm <- caret::confusionMatrix(pred, test[[responseName]])
    out$metrics <- list(
      Accuracy = unname(cm$overall["Accuracy"]),
      Kappa = unname(cm$overall["Kappa"])
    )
    out$confusion_matrix <- cm$table

    # Binary classification: ROC/PR AUC if twoClassSummary was used
    if (length(levels(test[[responseName]])) == 2L &&
        isTRUE(identical(model$control$summaryFunction, caret::twoClassSummary))) {

      probs <- tryCatch(
        stats::predict(model, newdata = test, type = "prob"),
        error = function(e) NULL
      )

      if (!is.null(probs)) {
        # Positive class = FIRST factor level (as with twoClassSummary)
        pos_level <- levels(test[[responseName]])[1L]
        neg_level <- levels(test[[responseName]])[2L]

        if (pos_level %in% colnames(probs)) {
          if (requireNamespace("pROC", quietly = TRUE)) {
            roc_obj <- pROC::roc(
              response = test[[responseName]],
              predictor = probs[[pos_level]],
              levels = c(neg_level, pos_level),
              direction = "<",
              quiet = TRUE
            )
            out$metrics$ROC_AUC <- as.numeric(pROC::auc(roc_obj))
          } else {
            mars_message("Optional package 'pROC' not installed; skipping test-set ROC AUC.", verbose)
          }

          if (requireNamespace("PRROC", quietly = TRUE)) {
            labs_pos <- test[[responseName]] == pos_level
            scores_pos <- probs[[pos_level]][labs_pos]
            scores_neg <- probs[[pos_level]][!labs_pos]

            # Only compute if both classes are present in the test set
            if (length(scores_pos) > 0L && length(scores_neg) > 0L) {
              pr <- PRROC::pr.curve(
                scores.class0 = scores_pos,
                scores.class1 = scores_neg,
                curve = FALSE
              )
              out$metrics$PR_AUC <- unname(pr$auc.integral)
            }
          } else {
            mars_message("Optional package 'PRROC' not installed; skipping test-set PR AUC.", verbose)
          }
        }
      }
    }

  } else {
    stop("Unsupported response variable type.")
  }

  # Variable importance (silently skipped if unsupported)
  vi <- tryCatch(caret::varImp(model), error = function(e) NULL)
  if (!is.null(vi)) {
    out$varimp <- vi
  }

  out
}

#' MARS Feature Selection and Model Training
#'
#' Trains a Multivariate Adaptive Regression Splines model (via
#' `caret::train(method = "earth")` by default) with repeated
#' cross-validation, evaluates it on a held-out test set, and returns
#' variable importance together with test-set metrics.
#'
#' @details
#' For classification, class imbalance is handled by `sampling = "up"` inside
#' `caret::trainControl()`, i.e. upsampling happens within each resample; the
#' data are never upsampled before cross-validation (which would leak
#' duplicated rows across folds). The FIRST factor level is treated as the
#' positive class for ROC/PR AUC. Factor levels are sanitized with
#' `make.names(unique = TRUE)`, so distinct labels can never be merged.
#'
#' For regression, the reported `R2` comes from `caret::R2()`, which is the
#' squared correlation between predictions and observations -- not
#' `1 - SSE/SST` -- and can be high even for a biased model.
#'
#' `search = "grid"` tunes over `expand.grid(nprune, degree)`;
#' `search = "random"` ignores that grid and evaluates `tuneLength` random
#' hyperparameter combinations instead.
#'
#' @param data data.frame or data.table with predictors and the response.
#' @param responseName Character. Response column name.
#' @param p Numeric in (0, 1). Training proportion (default 0.8).
#' @param degree Integer vector. Interaction degrees to tune (default 1:3).
#'   Used when `search = "grid"`.
#' @param nprune Integer vector. Numbers of retained terms to tune
#'   (default c(5, 10, 15)). Used when `search = "grid"`.
#' @param method Character. caret model method (default "earth").
#' @param search Character. "grid" (default) or "random"; see Details.
#' @param number Integer. CV folds (default 5).
#' @param repeats Integer. CV repeats (default 3).
#' @param seed Optional whole number for reproducibility, applied locally and
#'   restored on exit. Default NULL (never seeds by default). When NULL, no
#'   deterministic resampling seed lists are constructed either.
#' @param sampleSize Integer. Maximum number of rows used; larger data sets
#'   are randomly down-sampled first (default 10000).
#' @param show_warnings Logical. Warn on class imbalance (default TRUE).
#' @param verbose Logical. Print progress messages (default TRUE).
#' @param corr_cut Numeric in [0, 1]. Correlation cutoff for dropping highly
#'   correlated numeric predictors (default 0.95; 0 disables).
#' @param remove_nzv Logical. Remove near-zero-variance predictors
#'   (default TRUE).
#' @param verbose_iter Logical. If TRUE, caret prints fold progress.
#' @param n_cores Integer >= 1. Number of parallel workers for model training
#'   (default 1 = sequential; no cluster is created). Values above the
#'   detected core count are capped; requires 'doParallel' and 'foreach' when
#'   greater than 1.
#' @param tuneLength Integer. Number of random hyperparameter combinations
#'   evaluated when `search = "random"` (default 10; ignored for grid
#'   search).
#' @return A list with the fitted `model`, test-set `predictions`, `metrics`
#'   (RMSE/MAE/R2 for regression; Accuracy/Kappa plus ROC_AUC/PR_AUC when the
#'   optional 'pROC'/'PRROC' packages are installed for binary
#'   classification), `confusion_matrix` (classification only), `varimp`
#'   (when supported), and a `preprocessing` report of removed predictors.
#' @examples
#' \donttest{
#' if (requireNamespace("caret", quietly = TRUE) &&
#'     requireNamespace("earth", quietly = TRUE)) {
#'   df <- data.frame(
#'     y  = rnorm(150),
#'     x1 = rnorm(150),
#'     x2 = rnorm(150),
#'     x3 = rnorm(150)
#'   )
#'   res <- fs_mars(df, "y", degree = 1, nprune = c(5, 10),
#'                  number = 3, repeats = 1, verbose = FALSE, seed = 42)
#'   res$metrics
#' }
#' }
#' @export
fs_mars <- function(data, responseName,
                    p = 0.8,
                    degree = 1:3,
                    nprune = c(5, 10, 15),
                    method = "earth",
                    search = "grid",
                    number = 5,
                    repeats = 3,
                    seed = NULL,
                    sampleSize = 10000,
                    show_warnings = TRUE,
                    verbose = TRUE,
                    corr_cut = 0.95,
                    remove_nzv = TRUE,
                    verbose_iter = FALSE,
                    n_cores = 1L,
                    tuneLength = 10L) {

  # ---- Input validation -----------------------------------------------------
  assert_data_frame(data)
  assert_target(data, responseName, "responseName")
  assert_number(p, "p")
  if (p <= 0 || p >= 1) {
    stop("'p' must be strictly between 0 and 1.")
  }
  if (!is.numeric(degree) || length(degree) == 0L || anyNA(degree) ||
      any(degree < 1) || any(degree != round(degree))) {
    stop("'degree' must be a vector of positive whole numbers.")
  }
  if (!is.numeric(nprune) || length(nprune) == 0L || anyNA(nprune) ||
      any(nprune < 2) || any(nprune != round(nprune))) {
    stop("'nprune' must be a vector of whole numbers >= 2.")
  }
  assert_string(method, "method")
  assert_string(search, "search")
  if (!search %in% c("grid", "random")) {
    stop("'search' must be either \"grid\" or \"random\".")
  }
  assert_count(number, "number", lower = 2L)
  assert_count(repeats, "repeats", lower = 1L)
  if (!is.null(seed)) {
    assert_number(seed, "seed")
    if (seed != round(seed) || abs(seed) > .Machine$integer.max) {
      stop("'seed' must be a single whole number (integer-sized) or NULL.")
    }
  }
  sampleSize <- assert_count(sampleSize, "sampleSize", lower = 1L)
  assert_flag(show_warnings, "show_warnings")
  assert_flag(verbose, "verbose")
  assert_number(corr_cut, "corr_cut", lower = 0, upper = 1)
  assert_flag(remove_nzv, "remove_nzv")
  assert_flag(verbose_iter, "verbose_iter")
  n_cores <- resolve_cores(n_cores, "n_cores")
  tuneLength <- assert_count(tuneLength, "tuneLength", lower = 1L)

  fs_require(c("caret", "earth"), "MARS feature selection")
  if (n_cores > 1L) {
    fs_require(c("doParallel", "foreach"), "parallel model training")
  }

  # ---- Data preparation -----------------------------------------------------
  data <- as_dt(data)

  # Coerce response and sanitize factor levels if needed
  data <- mars_coerce_response(data, responseName, make_factor_names = TRUE,
                               verbose = verbose)

  # Remove rows with missing values
  data <- mars_drop_missing(data, verbose = verbose)
  if (nrow(data) == 0L) {
    stop("No rows remain after removing missing values.")
  }

  # Optional down-sampling BEFORE class-balance checks and splitting
  data <- mars_sample_rows(data, sampleSize, seed, verbose = verbose)

  # Check global class balance after sampling
  mars_check_class_balance(data, responseName, show_warnings)

  mars_message("Splitting data into training and test sets...", verbose)
  .mars_train_idx <- fs_split_index(data[[responseName]], p = p, seed = seed)
  train <- data[.mars_train_idx]
  test <- data[-.mars_train_idx]
  if (nrow(test) == 0L) {
    stop("Test set is empty after splitting; decrease 'p' or supply more data.")
  }
  mars_message(sprintf("Training set: %d rows; Test set: %d rows.",
                       nrow(train), nrow(test)), verbose)

  # Check class balance in the training set (classification only). Class
  # imbalance itself is handled per-fold via sampling = "up" in trainControl.
  mars_check_class_balance(train, responseName, show_warnings)

  # Predictor preprocessing (NZV, correlation)
  pp <- mars_preprocess(train, test, responseName,
                        corr_cut = corr_cut,
                        remove_nzv = remove_nzv,
                        verbose = verbose)
  train <- pp$train
  test <- pp$test

  # Align factor levels between train and test for classification
  if (is.factor(train[[responseName]])) {
    data.table::set(test, j = responseName,
                    value = factor(test[[responseName]],
                                   levels = levels(train[[responseName]])))
  }

  # ---- Tuning setup ---------------------------------------------------------
  hyperParameters <- mars_hyper_grid(degree, nprune)
  tune_n <- if (identical(search, "random")) tuneLength else nrow(hyperParameters)
  ctrl <- mars_train_control(
    number = number,
    repeats = repeats,
    search = search,
    train = train,
    responseName = responseName,
    seed = seed,
    tune_n = tune_n,
    verbose_iter = verbose_iter,
    verbose = verbose
  )

  # ---- Optional parallel backend (opt-in, leak-proof) -----------------------
  if (n_cores > 1L) {
    cl <- parallel::makeCluster(n_cores)
    on.exit({
      try(parallel::stopCluster(cl), silent = TRUE)
      foreach::registerDoSEQ()
    }, add = TRUE)
    doParallel::registerDoParallel(cl)
    mars_message(sprintf("Parallel backend registered with %d worker(s).", n_cores), verbose)
  }

  mars_message("Training the model...", verbose)
  t_train <- system.time({
    model <- mars_train_model(
      train = train,
      responseName = responseName,
      method = method,
      ctrl = ctrl,
      hyperParameters = hyperParameters,
      tune_length = tuneLength,
      search = search,
      seed = seed,
      metric = NULL
    )
  })
  mars_message(sprintf("Training completed in %.2f seconds.", t_train[["elapsed"]]), verbose)

  mars_message("Evaluating model performance...", verbose)
  eval_metrics <- mars_evaluate(model, test, responseName, verbose = verbose)
  eval_metrics$preprocessing <- list(removed_predictors = pp$removed)

  mars_message("Model training and evaluation complete.", verbose)
  eval_metrics
}
