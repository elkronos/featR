# MARS (earth) feature selection for featR.
#
# Suggests: caret and earth are needed at run time; pROC and PRROC are optional
# extras (soft-checked via requireNamespace); doParallel + foreach only when
# n_cores > 1. For classification, the FIRST factor level is treated as the
# "positive" class for ROC/PR AUC and caret::twoClassSummary.

#' Print a progress message when verbose
#' @noRd
mars_message <- function(msg, verbose) {
  if (isTRUE(verbose)) message(msg)
}

#' Coerce the target column in place
#'
#' Converts character targets to factor and (optionally) sanitizes factor
#' levels with `make.names(unique = TRUE)` so distinct classes such as
#' "class 1" and "class.1" can never be merged.
#'
#' @param data data.table (already a private copy).
#' @param target Character. Target column name.
#' @param make_factor_names Logical. Sanitize factor levels.
#' @param verbose Logical. Gate progress messages.
#' @return The data.table with a coerced target column.
#' @noRd
mars_coerce_response <- function(data, target, make_factor_names = TRUE,
                                 verbose = FALSE) {
  y <- data[[target]]
  if (is.character(y)) {
    mars_message(sprintf("Coercing character target '%s' to factor.", target),
                 verbose)
    y <- factor(y)
    data.table::set(data, j = target, value = y)
  }
  if (is.factor(y) && make_factor_names) {
    lev <- levels(y)
    levels(y) <- make.names(lev, unique = TRUE)
    data.table::set(data, j = target, value = y)
  }
  if (!is.factor(data[[target]]) && !is.numeric(data[[target]])) {
    stop("Target must be numeric (regression) or factor (classification).")
  }
  data
}

#' Drop rows with missing values
#' @noRd
mars_drop_missing <- function(data, verbose = FALSE) {
  initial_rows <- nrow(data)
  data_clean <- stats::na.omit(data)
  mars_message(sprintf("Removed %d rows with missing values.",
                       initial_rows - nrow(data_clean)), verbose)
  data_clean
}

#' Check class balance for classification targets
#'
#' Classes with fewer than two rows are a hard error. The "small class"
#' notice is gated on `warn`, which `fs_mars()` wires to `verbose`.
#' @noRd
mars_check_class_balance <- function(data, target, warn = FALSE) {
  if (is.factor(data[[target]])) {
    counts <- table(data[[target]])
    if (any(counts < 2)) {
      stop("Each class must have at least two samples.")
    }
    if (any(counts < 10) && isTRUE(warn)) {
      warning("Some classes have fewer than 10 samples. Per-fold upsampling will be applied during resampling.")
    }
  }
  invisible(NULL)
}

#' Randomly down-sample to at most sample_size rows
#' @noRd
mars_sample_rows <- function(data, sample_size, seed, verbose = FALSE) {
  if (nrow(data) > sample_size) {
    local_seed(seed)
    .mars_row_idx <- sample(nrow(data), sample_size)
    data <- data[.mars_row_idx]
    mars_message(sprintf("Data sampled down to %d rows.", sample_size), verbose)
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
#' @param target Character. Target column name.
#' @param corr_cut Numeric correlation cutoff (0 disables).
#' @param remove_nzv Logical. Remove near-zero-variance predictors.
#' @param verbose Logical. Gate progress messages.
#' @return A list with train, test, and a report of removed predictors.
#' @noRd
mars_preprocess <- function(train, test, target,
                            corr_cut = 0.95,
                            remove_nzv = TRUE,
                            verbose = FALSE) {
  pred_cols <- setdiff(colnames(train), target)
  nzv_removed <- character()
  corr_removed <- character()

  if (remove_nzv && length(pred_cols) > 0L) {
    nzv <- caret::nearZeroVar(train[, pred_cols, with = FALSE],
                              saveMetrics = TRUE)
    rm_idx <- which(nzv$nzv | nzv$zeroVar)
    if (length(rm_idx)) {
      nzv_removed <- rownames(nzv)[rm_idx]
      keep <- setdiff(pred_cols, nzv_removed)
      cols <- c(keep, target)
      train <- train[, cols, with = FALSE]
      test <- test[, cols, with = FALSE]
      pred_cols <- keep
      mars_message(sprintf("Removed %d near/zero-variance predictors.",
                           length(nzv_removed)), verbose)
    }
  }

  if (is.numeric(corr_cut) && corr_cut > 0 && length(pred_cols) > 1L) {
    num_cols <- pred_cols[vapply(train[, pred_cols, with = FALSE], is.numeric,
                                 logical(1L))]
    if (length(num_cols) > 1L) {
      cmat <- stats::cor(train[, num_cols, with = FALSE],
                         use = "pairwise.complete.obs")
      high <- caret::findCorrelation(cmat, cutoff = corr_cut, verbose = FALSE)
      if (length(high)) {
        corr_removed <- num_cols[high]
        keep <- setdiff(pred_cols, corr_removed)
        cols <- c(keep, target)
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
#' lists are only constructed when `seed` is not NULL. `allowParallel` is
#' FALSE unless the caller asked for more than one worker, so a backend
#' registered elsewhere in the session is never hijacked.
#'
#' @param number Integer. CV folds.
#' @param repeats Integer. CV repeats.
#' @param search Character. "grid" or "random".
#' @param train data.table. Training data.
#' @param target Character. Target column.
#' @param seed Optional whole number; NULL means no seeding.
#' @param tune_n Integer or NULL. Number of tuning combinations evaluated per
#'   resample (grid rows for "grid", tuneLength for "random"); when NULL,
#'   seeds are not set.
#' @param allow_parallel Logical. Let caret dispatch resamples to a registered
#'   foreach backend.
#' @param verbose Logical. Gate progress messages, and print caret's per-fold
#'   iteration log.
#' @return A list as returned by `caret::trainControl()`.
#' @noRd
mars_train_control <- function(number, repeats, search, train, target,
                               seed, tune_n = NULL, allow_parallel = FALSE,
                               verbose = FALSE) {
  y <- train[[target]]
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
    allowParallel = isTRUE(allow_parallel),
    savePredictions = "final",
    classProbs = is_class,
    returnResamp = "all",
    summaryFunction = summary_fun,
    verboseIter = isTRUE(verbose),
    sampling = if (is_class) "up" else NULL,
    seeds = seeds
  )
}

#' Train the earth model via caret
#'
#' `fs_mars()` is earth-only (the degree/nprune grid is meaningless for any
#' other engine), so the caret method string is hardcoded to "earth".
#' When `search = "random"`, `tuneLength` controls the number of random
#' hyperparameter combinations and no fixed grid is passed; when
#' `search = "grid"`, the explicit grid is used.
#'
#' @noRd
mars_train_model <- function(train, target, ctrl, hyperParameters,
                             tune_length, search, seed, metric = NULL) {
  local_seed(seed)

  # Determine metric if not specified
  if (is.null(metric)) {
    if (is.factor(train[[target]]) &&
        length(levels(train[[target]])) == 2L &&
        identical(ctrl$summaryFunction, caret::twoClassSummary)) {
      metric <- "ROC"
    } else if (is.factor(train[[target]])) {
      metric <- "Accuracy"
    } else {
      metric <- "RMSE"
    }
  }

  fmla <- stats::as.formula(sprintf("`%s` ~ .", target))

  train_args <- list(
    fmla,
    data = train,
    method = "earth",
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

#' Named importance vector from a caret varImp object
#'
#' Accepts either the `varImp.train` object or its `$importance` data.frame.
#' Uses the "Overall" column when present and otherwise averages the numeric
#' columns (caret reports one column per class for some model types).
#'
#' @return A named numeric vector, or NULL when no importance is available.
#' @noRd
mars_importance_vector <- function(vi) {
  if (is.null(vi)) {
    return(NULL)
  }
  imp <- if (is.data.frame(vi)) vi else vi$importance
  if (!is.data.frame(imp) || nrow(imp) == 0L) {
    return(NULL)
  }
  nms <- rownames(imp)
  if (is.null(nms)) {
    return(NULL)
  }
  num <- vapply(imp, is.numeric, logical(1L))
  if (!any(num)) {
    return(NULL)
  }
  vals <- if ("Overall" %in% names(imp) && is.numeric(imp[["Overall"]])) {
    as.numeric(imp[["Overall"]])
  } else {
    as.numeric(rowMeans(as.matrix(imp[, num, drop = FALSE]), na.rm = TRUE))
  }
  stats::setNames(vals, nms)
}

#' Map caret importance names back onto the candidate predictors
#'
#' caret expands factors into dummy columns before earth sees them, so a
#' varImp row can be named "grpb" for the predictor "grp". Every importance
#' row is attributed to the longest candidate predictor name it starts with,
#' and a predictor keeps the largest importance of its encoded columns.
#' Predictors earth never used keep their 0; scores are floored at 0, so a
#' negative importance also reads as "not retained".
#'
#' @param importance Named numeric vector from `mars_importance_vector()`.
#' @param predictors Character vector of candidate predictor names.
#' @return A named numeric vector, one entry per candidate predictor.
#' @noRd
mars_predictor_scores <- function(importance, predictors) {
  scores <- stats::setNames(rep(0, length(predictors)), predictors)
  if (is.null(importance) || length(importance) == 0L ||
      length(predictors) == 0L) {
    return(scores)
  }
  imp_names <- names(importance)
  for (i in seq_along(importance)) {
    nm <- imp_names[i]
    val <- importance[[i]]
    if (is.null(nm) || is.na(nm) || !is.finite(val)) next
    hit <- if (nm %in% predictors) {
      nm
    } else {
      cand <- predictors[startsWith(nm, predictors)]
      if (length(cand) == 0L) NA_character_ else cand[which.max(nchar(cand))]
    }
    if (is.na(hit)) next
    scores[[hit]] <- max(scores[[hit]], val)
  }
  scores
}

#' Evaluate a fitted model on the test set
#'
#' For binary classification trained with `caret::twoClassSummary`, the FIRST
#' factor level is the positive class. ROC AUC is computed with explicit
#' `levels = c(negative, positive)` and `direction = "<"` so worse-than-chance
#' models honestly score below 0.5 instead of being silently flipped.
#'
#' @return A list with `predictions`, `metrics`, `confusion_matrix`
#'   (classification only, else NULL) and `varimp` (NULL when unsupported).
#' @noRd
mars_evaluate <- function(model, test, target, verbose = FALSE) {
  pred <- stats::predict(model, newdata = test)
  out <- list(predictions = pred, metrics = NULL, confusion_matrix = NULL,
              varimp = NULL)

  # Regression
  if (is.numeric(test[[target]])) {
    obs <- test[[target]]
    rmse_val <- sqrt(mean((obs - pred)^2))
    mae_val <- mean(abs(obs - pred))
    # Note: caret::R2 is the squared correlation between predicted and
    # observed, not 1 - SSE/SST.
    r2_val <- caret::R2(pred, obs)
    out$metrics <- list(RMSE = rmse_val, MAE = mae_val, R2 = r2_val)

    # Classification
  } else if (is.factor(test[[target]])) {
    pred <- factor(pred, levels = levels(test[[target]]))
    out$predictions <- pred
    cm <- caret::confusionMatrix(pred, test[[target]])
    out$metrics <- list(
      Accuracy = unname(cm$overall["Accuracy"]),
      Kappa = unname(cm$overall["Kappa"])
    )
    out$confusion_matrix <- cm$table

    # Binary classification: ROC/PR AUC if twoClassSummary was used
    if (length(levels(test[[target]])) == 2L &&
        isTRUE(identical(model$control$summaryFunction,
                         caret::twoClassSummary))) {

      probs <- tryCatch(
        stats::predict(model, newdata = test, type = "prob"),
        error = function(e) NULL
      )

      if (!is.null(probs)) {
        # Positive class = FIRST factor level (as with twoClassSummary)
        pos_level <- levels(test[[target]])[1L]
        neg_level <- levels(test[[target]])[2L]

        if (pos_level %in% colnames(probs)) {
          if (requireNamespace("pROC", quietly = TRUE)) {
            roc_obj <- pROC::roc(
              response = test[[target]],
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
            labs_pos <- test[[target]] == pos_level
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
    stop("Unsupported target variable type.")
  }

  # Variable importance, unscaled: caret's scale = TRUE shifts the smallest
  # importance to 0, which would hide the weakest predictor earth actually
  # kept. Unused predictors are already reported as 0 by caret.
  out$varimp <- tryCatch(caret::varImp(model, scale = FALSE),
                         error = function(e) NULL)

  out
}

#' MARS (earth) feature selection
#'
#' Trains a Multivariate Adaptive Regression Splines model with
#' `caret::train(method = "earth")` and repeated cross-validation, evaluates it
#' on a held-out test set, and reports the predictors earth retained together
#' with their variable importance.
#'
#' @details
#' Selection is read off the fitted model: `caret::varImp(model, scale = FALSE)`
#' is computed on the caret `train` object and every predictor with a non-zero
#' importance is reported in `selected`. Predictors earth pruned away score 0
#' and are not selected. caret expands factors into dummy columns before
#' fitting, so each importance row is attributed back to the source predictor
#' and a predictor keeps the largest importance of its encoded columns.
#'
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
#' @param data data.frame or data.table with predictors and the target.
#' @param target Character. Name of the target column in `data`. A factor or
#'   character target is treated as classification, a numeric one as
#'   regression.
#' @param train_ratio Numeric in (0, 1). Training proportion (default 0.8).
#' @param degree Integer vector. Interaction degrees to tune (default 1:3).
#'   Used when `search = "grid"`.
#' @param nprune Integer vector. Numbers of retained terms to tune
#'   (default `c(5, 10, 15)`). Used when `search = "grid"`.
#' @param tuneLength Integer. Number of random hyperparameter combinations
#'   evaluated when `search = "random"` (default 10; ignored for grid search).
#' @param search Character. "grid" (default) or "random"; see Details.
#' @param number Integer >= 2. Cross-validation folds (default 5).
#' @param repeats Integer >= 1. Cross-validation repeats (default 3).
#' @param sample_size Integer. Maximum number of rows used; larger data sets
#'   are randomly down-sampled first (default 10000).
#' @param corr_cut Numeric between 0 and 1. Correlation cutoff for dropping
#'   highly correlated numeric predictors (default 0.95; 0 disables).
#' @param remove_nzv Logical. Remove near-zero-variance predictors
#'   (default TRUE).
#' @param seed Optional whole number for reproducibility, applied locally and
#'   restored on exit. Default NULL (never seeds by default). When NULL, no
#'   deterministic resampling seed lists are constructed either.
#' @param verbose Logical. Print progress messages, caret's per-fold iteration
#'   log, and the small-class notice (default FALSE).
#' @param n_cores Integer >= 1. Number of parallel workers for model training
#'   (default 1 = sequential; no cluster is created and caret's
#'   `allowParallel` stays FALSE). Values above the detected core count are
#'   capped; requires 'doParallel' and 'foreach' when greater than 1.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector of the predictors earth retained
#'         (non-zero `caret::varImp()` importance).}
#'   \item{scores}{Named numeric vector of unscaled variable importance, one
#'         entry per candidate predictor that entered training, floored at 0
#'         (predictors earth pruned away score 0).}
#'   \item{method}{"mars".}
#'   \item{task}{"classification" for a factor target, "regression" for a
#'         numeric one.}
#'   \item{model}{The `caret::train` object.}
#'   \item{details}{A list with `predictions` (test-set predictions),
#'         `metrics` (RMSE/MAE/R2 for regression; Accuracy/Kappa plus
#'         ROC_AUC/PR_AUC when the optional 'pROC'/'PRROC' packages are
#'         installed for binary classification), `confusion_matrix`
#'         (classification only, else NULL), `varimp` (the `caret::varImp()`
#'         object, or NULL when unsupported), `removed_predictors` (a list with
#'         `nzv` and `corr`), `train_index` (training rows of the cleaned and
#'         optionally down-sampled data), `test_data` (the held-out rows after
#'         preprocessing) and `n_features` (candidate predictors that entered
#'         training).}
#'   \item{call}{The matched call.}
#' }
#'
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
#'                  number = 3, repeats = 1, seed = 42)
#'   res$selected
#'   res$details$metrics
#' }
#' }
#' @export
fs_mars <- function(data,
                    target,
                    train_ratio = 0.8,
                    degree = 1:3,
                    nprune = c(5, 10, 15),
                    tuneLength = 10L,
                    search = "grid",
                    number = 5,
                    repeats = 3,
                    sample_size = 10000,
                    corr_cut = 0.95,
                    remove_nzv = TRUE,
                    seed = NULL,
                    verbose = FALSE,
                    n_cores = 1L) {
  cl_call <- match.call()

  # ---- Input validation -----------------------------------------------------
  assert_data_frame(data)
  assert_target(data, target, "target")
  assert_number(train_ratio, "train_ratio")
  if (train_ratio <= 0 || train_ratio >= 1) {
    stop("'train_ratio' must be strictly between 0 and 1.")
  }
  if (!is.numeric(degree) || length(degree) == 0L || anyNA(degree) ||
      any(degree < 1) || any(degree != round(degree))) {
    stop("'degree' must be a vector of positive whole numbers.")
  }
  if (!is.numeric(nprune) || length(nprune) == 0L || anyNA(nprune) ||
      any(nprune < 2) || any(nprune != round(nprune))) {
    stop("'nprune' must be a vector of whole numbers >= 2.")
  }
  tuneLength <- assert_count(tuneLength, "tuneLength", lower = 1L)
  assert_string(search, "search")
  if (!search %in% c("grid", "random")) {
    stop("'search' must be either \"grid\" or \"random\".")
  }
  assert_count(number, "number", lower = 2L)
  assert_count(repeats, "repeats", lower = 1L)
  sample_size <- assert_count(sample_size, "sample_size", lower = 1L)
  assert_number(corr_cut, "corr_cut", lower = 0, upper = 1)
  assert_flag(remove_nzv, "remove_nzv")
  if (!is.null(seed)) {
    assert_number(seed, "seed")
    if (seed != round(seed) || abs(seed) > .Machine$integer.max) {
      stop("'seed' must be a single whole number (integer-sized) or NULL.")
    }
  }
  assert_flag(verbose, "verbose")
  n_cores <- resolve_cores(n_cores, "n_cores")

  fs_require(c("caret", "earth"), "MARS feature selection")
  if (n_cores > 1L) {
    fs_require(c("doParallel", "foreach"), "parallel model training")
  }

  # ---- Data preparation -----------------------------------------------------
  data <- as_dt(data)

  # Coerce the target and sanitize factor levels if needed
  data <- mars_coerce_response(data, target, make_factor_names = TRUE,
                               verbose = verbose)

  # Remove rows with missing values
  data <- mars_drop_missing(data, verbose = verbose)
  if (nrow(data) == 0L) {
    stop("No rows remain after removing missing values.")
  }

  # Optional down-sampling BEFORE class-balance checks and splitting
  data <- mars_sample_rows(data, sample_size, seed, verbose = verbose)

  # Check global class balance after sampling
  mars_check_class_balance(data, target, warn = verbose)

  task <- if (is.factor(data[[target]])) "classification" else "regression"

  mars_message("Splitting data into training and test sets...", verbose)
  .mars_train_idx <- fs_split_index(data[[target]], p = train_ratio,
                                    seed = seed)
  train <- data[.mars_train_idx]
  test <- data[-.mars_train_idx]
  if (nrow(test) == 0L) {
    stop("Test set is empty after splitting; decrease 'train_ratio' or supply more data.")
  }
  mars_message(sprintf("Training set: %d rows; Test set: %d rows.",
                       nrow(train), nrow(test)), verbose)

  # Check class balance in the training set (classification only). Class
  # imbalance itself is handled per-fold via sampling = "up" in trainControl.
  mars_check_class_balance(train, target, warn = verbose)

  # Predictor preprocessing (NZV, correlation)
  pp <- mars_preprocess(train, test, target,
                        corr_cut = corr_cut,
                        remove_nzv = remove_nzv,
                        verbose = verbose)
  train <- pp$train
  test <- pp$test

  # Align factor levels between train and test for classification
  if (is.factor(train[[target]])) {
    data.table::set(test, j = target,
                    value = factor(test[[target]],
                                   levels = levels(train[[target]])))
  }

  candidate_predictors <- setdiff(colnames(train), target)

  # ---- Tuning setup ---------------------------------------------------------
  hyperParameters <- mars_hyper_grid(degree, nprune)
  tune_n <- if (identical(search, "random")) tuneLength else nrow(hyperParameters)
  ctrl <- mars_train_control(
    number = number,
    repeats = repeats,
    search = search,
    train = train,
    target = target,
    seed = seed,
    tune_n = tune_n,
    allow_parallel = n_cores > 1L,
    verbose = verbose
  )

  # ---- Optional parallel backend (opt-in, leak-proof) -----------------------
  if (n_cores > 1L) {
    cluster <- parallel::makeCluster(n_cores)
    on.exit({
      try(parallel::stopCluster(cluster), silent = TRUE)
      foreach::registerDoSEQ()
    }, add = TRUE)
    doParallel::registerDoParallel(cluster)
    mars_message(sprintf("Parallel backend registered with %d worker(s).",
                         n_cores), verbose)
  }

  mars_message("Training the model...", verbose)
  t_train <- system.time({
    model <- mars_train_model(
      train = train,
      target = target,
      ctrl = ctrl,
      hyperParameters = hyperParameters,
      tune_length = tuneLength,
      search = search,
      seed = seed,
      metric = NULL
    )
  })
  mars_message(sprintf("Training completed in %.2f seconds.",
                       t_train[["elapsed"]]), verbose)

  mars_message("Evaluating model performance...", verbose)
  ev <- mars_evaluate(model, test, target, verbose = verbose)

  # ---- Selection from the fitted model --------------------------------------
  imp_vec <- mars_importance_vector(ev$varimp)
  if (is.null(imp_vec)) {
    scores <- NULL
    selected_features <- character(0)
    mars_message("Variable importance is unavailable for this model; no features reported as selected.", verbose)
  } else {
    scores <- mars_predictor_scores(imp_vec, candidate_predictors)
    keep <- scores > 0
    selected_features <- names(scores)[keep]
    if (length(selected_features) > 1L) {
      v <- unname(scores[keep])
      selected_features <- selected_features[order(-v, seq_along(v))]
    }
  }

  mars_message("Model training and evaluation complete.", verbose)

  new_fs_result(
    selected = selected_features,
    scores   = scores,
    method   = "mars",
    task     = task,
    model    = model,
    details  = list(
      predictions        = ev$predictions,
      metrics            = ev$metrics,
      confusion_matrix   = ev$confusion_matrix,
      varimp             = ev$varimp,
      removed_predictors = pp$removed,
      train_index        = as.integer(.mars_train_idx),
      test_data          = test,
      n_features         = length(candidate_predictors)
    ),
    call = cl_call
  )
}
