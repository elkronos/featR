# SVM training and evaluation pipeline built on caret, with optional
# dummy encoding, random-forest RFE feature selection, class-imbalance
# handling inside resampling, and cross-validated hyperparameter tuning.

#' Validate inputs for the SVM workflow
#'
#' `task` and `kernel` are validated in fs_svm() before this runs, so a typo
#' fails immediately rather than after expensive work.
#'
#' @param data Data frame with predictors and the target.
#' @param target String naming the target column.
#' @param task "classification" or "regression" (already validated).
#' @param train_ratio Single number strictly between 0 and 1.
#' @param nfolds Single whole number > 1.
#' @return Invisibly TRUE when validation passes.
#' @noRd
svm_validate <- function(data, target, task, train_ratio, nfolds) {
  assert_data_frame(data, arg = "data")
  assert_target(data, target)
  assert_number(train_ratio, "train_ratio")
  if (train_ratio <= 0 || train_ratio >= 1) {
    stop("'train_ratio' must be a single numeric value in (0, 1).",
         call. = FALSE)
  }
  assert_count(nfolds, "nfolds", lower = 2L)

  if (nrow(data) < 2L) {
    stop("'data' must contain at least 2 rows.", call. = FALSE)
  }

  predictor_names <- setdiff(names(data), target)
  if (length(predictor_names) == 0L) {
    stop("'data' must contain at least one predictor column besides the target.",
         call. = FALSE)
  }

  if (anyNA(data[[target]])) {
    stop("'target' contains missing values; please handle them before modeling.",
         call. = FALSE)
  }
  if (anyNA(data[predictor_names])) {
    stop("Predictor columns contain missing values; please handle them before modeling.",
         call. = FALSE)
  }

  if (task == "regression" && !is.numeric(data[[target]])) {
    stop("For regression, the target must be numeric.", call. = FALSE)
  }
  if (task == "classification" && length(unique(data[[target]])) < 2L) {
    stop("For classification, the target must have at least two classes.",
         call. = FALSE)
  }

  invisible(TRUE)
}

#' Coerce the target column to the type required by the task
#'
#' For classification, a numeric target with more than 10 unique values is
#' rejected (it is almost certainly a regression target); with 10 or fewer it
#' is coerced to a factor with a message.
#'
#' @param data Data frame.
#' @param target Target column name.
#' @param task "classification" or "regression".
#' @return The data frame with the target coerced.
#' @noRd
svm_coerce_target <- function(data, target, task) {
  y <- data[[target]]
  if (task == "classification") {
    if (is.numeric(y)) {
      n_unique <- length(unique(y[!is.na(y)]))
      if (n_unique > 10L) {
        stop(
          sprintf(
            paste0(
              "'target' is numeric with %d unique values; ",
              "did you mean task = 'regression'?"
            ),
            n_unique
          ),
          call. = FALSE
        )
      }
      message(sprintf(
        "Coercing numeric target '%s' (%d unique values) to a factor for classification.",
        target, n_unique
      ))
    }
    data[[target]] <- as.factor(y)
  } else {
    data[[target]] <- as.numeric(y)
  }
  data
}

#' Align factor/character predictor levels between train and test sets
#'
#' Each factor or character predictor in the test set is re-leveled to the
#' training levels. Test rows containing levels unseen in training are
#' dropped with a warning; without this, caret's dummyVars prediction fails
#' on the first unseen level.
#'
#' @param train_set,test_set Data frames from the same split.
#' @param target Target column name (excluded from alignment).
#' @return List with the aligned `train_set` and `test_set`.
#' @noRd
svm_align_levels <- function(train_set, test_set, target) {
  unseen <- rep(FALSE, nrow(test_set))

  for (nm in setdiff(names(train_set), target)) {
    col_train <- train_set[[nm]]
    if (!is.factor(col_train) && !is.character(col_train)) {
      next
    }
    train_levels <- if (is.factor(col_train)) {
      levels(col_train)
    } else {
      sort(unique(as.character(col_train)))
    }
    test_chr <- as.character(test_set[[nm]])
    unseen <- unseen | (!is.na(test_chr) & !(test_chr %in% train_levels))
    train_set[[nm]] <- factor(as.character(col_train), levels = train_levels)
    test_set[[nm]] <- factor(test_chr, levels = train_levels)
  }

  n_unseen <- sum(unseen)
  if (n_unseen > 0L) {
    warning(
      sprintf(
        "Dropped %d test row(s) containing predictor factor levels unseen in the training set.",
        n_unseen
      ),
      call. = FALSE
    )
    test_set <- test_set[!unseen, , drop = FALSE]
    if (nrow(test_set) == 0L) {
      stop(
        "All test rows contained predictor factor levels unseen in the training set; adjust 'train_ratio' or check the data.",
        call. = FALSE
      )
    }
  }

  list(train_set = train_set, test_set = test_set)
}

#' Fit a dummy encoder on the predictors
#'
#' @param data Data frame.
#' @param target Target column name.
#' @param full_rank Logical; if TRUE, full-rank parameterization.
#' @return A caret::dummyVars object.
#' @noRd
svm_dummy_encoder <- function(data, target, full_rank = TRUE) {
  predictors <- setdiff(names(data), target)
  if (length(predictors) == 0L) {
    stop("No predictor columns available for dummy encoding.", call. = FALSE)
  }
  caret::dummyVars(
    formula = stats::reformulate(backtick(predictors)),
    data = data,
    fullRank = full_rank
  )
}

#' Transform data with a fitted dummy encoder
#'
#' @param dv A caret::dummyVars object.
#' @param data Data frame to transform.
#' @return Numeric data frame of encoded predictors.
#' @noRd
svm_encode <- function(dv, data) {
  as.data.frame(stats::predict(dv, newdata = data))
}

#' Random-forest recursive feature elimination on encoded predictors
#'
#' Note: this is random-forest RFE (caret::rfFuncs) used as a screening step
#' for the SVM; it is NOT SVM-RFE. When rfe() errors or selects nothing, it
#' falls back with a warning to the top random-forest importance features.
#'
#' @param train_set Training data frame (original predictor columns).
#' @param target Target column name.
#' @param rfe_folds Number of CV folds for RFE.
#' @param min_keep Minimum number of predictors returned by the fallback.
#' @param allow_parallel Logical; let RFE use a registered foreach backend.
#' @return Character vector of selected (post-encoding) feature names.
#' @noRd
svm_feature_selection <- function(train_set,
                                  target,
                                  rfe_folds = 10,
                                  min_keep = 1,
                                  allow_parallel = FALSE) {
  dv <- svm_dummy_encoder(train_set, target, full_rank = TRUE)
  x_enc <- svm_encode(dv, train_set)
  yv <- train_set[[target]]

  if (ncol(x_enc) == 0L) {
    stop("No predictors available for feature selection.", call. = FALSE)
  }

  sizes <- seq_len(ncol(x_enc))
  ctrl <- caret::rfeControl(
    functions = caret::rfFuncs,
    method = "cv",
    number = rfe_folds,
    verbose = FALSE,
    allowParallel = allow_parallel
  )

  rfe_vars <- tryCatch(
    caret::predictors(
      caret::rfe(x = x_enc, y = yv, sizes = sizes, rfeControl = ctrl)
    ),
    error = function(e) e
  )

  if (!inherits(rfe_vars, "condition") && length(rfe_vars) > 0L) {
    return(rfe_vars)
  }

  reason <- if (inherits(rfe_vars, "condition")) {
    conditionMessage(rfe_vars)
  } else {
    "rfe selected no features"
  }

  rf_fit <- randomForest::randomForest(x = x_enc, y = yv, importance = TRUE)
  imp <- as.data.frame(randomForest::importance(rf_fit, type = 2))
  imp$Feature <- rownames(imp)
  score_col <- utils::tail(names(imp)[vapply(imp, is.numeric, logical(1L))], 1L)
  ord <- order(imp[[score_col]], decreasing = TRUE, na.last = NA)

  keep_n <- max(min_keep, 1L)
  selected <- imp$Feature[ord][seq_len(min(keep_n, length(ord)))]
  if (length(selected) == 0L) {
    selected <- colnames(x_enc)[seq_len(min(keep_n, ncol(x_enc)))]
  }

  warning(
    sprintf(
      "feature selection failed (%s); falling back to top random-forest importance features (n = %d)",
      reason, length(selected)
    ),
    call. = FALSE
  )

  selected
}

#' Default hyperparameter grids for the SVM kernels
#'
#' @param kernel One of "linear", "radial", "polynomial".
#' @return Data frame suitable for caret::train(tuneGrid = ...).
#' @noRd
svm_tune_grid <- function(kernel = c("linear", "radial", "polynomial")) {
  kernel <- match.arg(kernel)
  if (kernel == "linear") {
    expand.grid(C = 2^seq(-5, 10, by = 1))
  } else if (kernel == "radial") {
    expand.grid(
      sigma = 2^seq(-15, -5, by = 1),
      C = 2^seq(-1, 10, by = 1)
    )
  } else {
    expand.grid(
      degree = 2:5,
      scale = c(0.1, 0.5, 1),
      C = 2^seq(-3, 8, by = 1)
    )
  }
}

#' Compute performance metrics on the test set
#'
#' Prediction/actual pairs containing NA are dropped with a warning, and all
#' metrics (including MAE) are computed from the same NA-filtered vectors so
#' they are internally consistent.
#'
#' @param predictions Predicted values.
#' @param actuals True target values.
#' @param task "classification" or "regression".
#' @return For classification, a caret::confusionMatrix object; for
#'   regression, a named numeric vector with RMSE, Rsquared, and MAE.
#' @noRd
svm_performance <- function(predictions, actuals, task) {
  valid_idx <- !is.na(predictions) & !is.na(actuals)
  n_dropped <- sum(!valid_idx)
  if (n_dropped > 0L) {
    warning(
      sprintf(
        "%d prediction/actual pair(s) contained NA and were dropped before computing performance metrics.",
        n_dropped
      ),
      call. = FALSE
    )
    predictions <- predictions[valid_idx]
    actuals <- actuals[valid_idx]
  }

  if (task == "classification") {
    predictions <- factor(predictions, levels = levels(actuals))
    caret::confusionMatrix(predictions, actuals)
  } else {
    pr <- caret::postResample(pred = predictions, obs = actuals)
    mae <- mean(abs(predictions - actuals))
    c(
      RMSE = unname(pr["RMSE"]),
      Rsquared = unname(pr["Rsquared"]),
      MAE = mae
    )
  }
}

#' Start a parallel backend with an explicit worker count
#'
#' @param n_cores Integer >= 2 (already resolved via resolve_cores()).
#' @return The cluster object.
#' @noRd
svm_setup_parallel <- function(n_cores) {
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  cl
}

#' Stop a parallel backend started by svm_setup_parallel()
#'
#' @param cl Cluster object, or NULL for a no-op.
#' @return Invisibly NULL.
#' @noRd
svm_stop_parallel <- function(cl) {
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
    doParallel::stopImplicitCluster()
  }
  invisible(NULL)
}

#' Train and Evaluate an SVM with Optional Feature Selection and Class Balancing
#'
#' Trains an SVM classifier or regressor using \pkg{caret} (with the
#' \pkg{kernlab} engines), with options for dummy encoding of predictors,
#' feature selection, class-imbalance handling, and hyperparameter tuning
#' via cross-validation. Optional parallel training uses an explicit worker
#' count.
#'
#' Notes:
#' \itemize{
#'   \item Feature selection (\code{feature_select = TRUE}) is
#'     \emph{random-forest} recursive feature elimination via
#'     \code{caret::rfFuncs}, used as a screening step before the SVM; it is
#'     not SVM-RFE. If RFE fails or selects nothing, the top random-forest
#'     importance features are used instead, with a warning.
#'   \item Class-imbalance handling (\code{class_imbalance = TRUE},
#'     classification only) up-samples \emph{within} each cross-validation
#'     resample via \code{caret::trainControl(sampling = "up")}, so no
#'     resampled rows leak across folds.
#'   \item After the train/test split, factor (and character) predictor
#'     levels in the test set are aligned to the training levels; test rows
#'     containing levels unseen in training are dropped with a warning.
#'   \item A numeric target with \code{task = "classification"} is coerced
#'     to a factor (with a message) only when it has at most 10 unique
#'     values; more than 10 unique values is an error suggesting
#'     \code{task = "regression"}.
#' }
#'
#' Suggested packages required at runtime: \pkg{caret} and \pkg{kernlab}
#' always, \pkg{e1071} for classification metrics, \pkg{randomForest} when
#' \code{feature_select = TRUE}, and \pkg{doParallel}/\pkg{foreach} when
#' \code{n_cores > 1}.
#'
#' @param data A data frame containing predictors and the target.
#' @param target A string naming the target variable.
#' @param task Either \code{"classification"} or \code{"regression"}.
#' @param train_ratio Training set proportion, strictly between 0 and 1
#'   (default \code{0.7}).
#' @param nfolds Number of CV folds, a whole number > 1 (default \code{5}).
#' @param tune_grid Optional tuning grid data frame. If \code{NULL}, a
#'   default grid for the chosen kernel is used.
#' @param seed Optional seed, applied locally for the duration of the call
#'   and restored afterwards; also used to set reproducible RNG streams on
#'   parallel workers when \code{n_cores > 1}. Default \code{NULL} (never
#'   seeds by default).
#' @param feature_select Logical; if \code{TRUE}, performs random-forest RFE
#'   on the dummy-encoded predictors (default \code{FALSE}).
#' @param class_imbalance Logical; if \code{TRUE} and the task is
#'   classification, up-samples classes within CV resampling (default
#'   \code{FALSE}).
#' @param kernel One of \code{"linear"}, \code{"radial"}, or
#'   \code{"polynomial"}.
#' @param n_cores Number of parallel workers (default \code{1}, sequential).
#'   Requests are capped at the detected core count; when greater than 1 a
#'   cluster is created for the duration of the call and stopped on exit.
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{model}: Trained \code{caret} model object.
#'   \item \code{test_set}: Test set (original columns, coerced target,
#'     aligned factor levels).
#'   \item \code{predictions}: Predictions on the test set.
#'   \item \code{performance}: Performance metrics.
#'   \item \code{selected_features}: Names of selected encoded features
#'     (when \code{feature_select = TRUE}), otherwise \code{NULL}.
#' }
#' @examples
#' \donttest{
#' if (requireNamespace("caret", quietly = TRUE) &&
#'     requireNamespace("kernlab", quietly = TRUE) &&
#'     requireNamespace("e1071", quietly = TRUE)) {
#'   res <- fs_svm(
#'     data = iris,
#'     target = "Species",
#'     task = "classification",
#'     nfolds = 3,
#'     seed = 42,
#'     kernel = "linear",
#'     tune_grid = data.frame(C = 1)
#'   )
#'   res$performance
#' }
#' }
#' @export
fs_svm <- function(data,
                   target,
                   task,
                   train_ratio = 0.7,
                   nfolds = 5,
                   tune_grid = NULL,
                   seed = NULL,
                   feature_select = FALSE,
                   class_imbalance = FALSE,
                   kernel = c("linear", "radial", "polynomial"),
                   n_cores = 1L) {
  # Validate the cheap, typo-prone arguments first: a bad kernel or task must
  # fail immediately, not after minutes of feature selection.
  kernel <- match.arg(kernel)
  assert_string(task, "task")
  if (!task %in% c("classification", "regression")) {
    stop("'task' must be either 'classification' or 'regression'.",
         call. = FALSE)
  }

  assert_data_frame(data, arg = "data")
  data <- as.data.frame(data)
  svm_validate(data, target, task, train_ratio, nfolds)
  assert_flag(feature_select, "feature_select")
  assert_flag(class_imbalance, "class_imbalance")
  if (!is.null(tune_grid)) {
    assert_data_frame(tune_grid, arg = "tune_grid")
  }
  n_cores <- resolve_cores(n_cores)
  use_parallel <- n_cores > 1L

  pkgs <- c("caret", "kernlab")
  if (task == "classification") {
    pkgs <- c(pkgs, "e1071")
  }
  if (feature_select) {
    pkgs <- c(pkgs, "randomForest")
  }
  fs_require(pkgs, "SVM training and evaluation")

  # One local seed for the whole call; restored when fs_svm() exits.
  local_seed(seed)

  data <- svm_coerce_target(data, target, task)

  # Stratified split (classification strata; quantile groups for numeric
  # targets). The seed above already governs this draw.
  idx <- fs_split_index(data[[target]], p = train_ratio)
  train_set <- data[idx, , drop = FALSE]
  test_set <- data[-idx, , drop = FALSE]

  if (nrow(train_set) == 0L || nrow(test_set) == 0L) {
    stop("Training or test set is empty; adjust 'train_ratio' or check data size.",
         call. = FALSE)
  }

  if (task == "classification") {
    train_set[[target]] <- droplevels(train_set[[target]])
    if (length(unique(train_set[[target]])) < 2L) {
      stop(
        "Training set contains fewer than 2 classes after splitting; adjust 'train_ratio' or handle class imbalance.",
        call. = FALSE
      )
    }
    test_set[[target]] <- factor(
      test_set[[target]],
      levels = levels(train_set[[target]])
    )
    keep_rows <- !is.na(test_set[[target]])
    if (!all(keep_rows)) {
      warning(
        sprintf(
          "Dropped %d test row(s) whose target class was not present in the training set.",
          sum(!keep_rows)
        ),
        call. = FALSE
      )
      test_set <- test_set[keep_rows, , drop = FALSE]
    }
    if (nrow(test_set) == 0L) {
      stop(
        "All test set rows had classes not present in training; adjust 'train_ratio' or check class distribution.",
        call. = FALSE
      )
    }
  }

  # Align factor predictor levels; drops (with a warning) test rows carrying
  # levels unseen in training, which would otherwise crash dummy encoding.
  aligned <- svm_align_levels(train_set, test_set, target)
  train_set <- aligned$train_set
  test_set <- aligned$test_set

  # Optional parallel backend, created early so feature selection and
  # training can share it, and always cleaned up on exit.
  cl <- NULL
  if (use_parallel) {
    fs_require(c("doParallel", "foreach"), "parallel SVM training")
    cl <- svm_setup_parallel(n_cores)
    on.exit(svm_stop_parallel(cl), add = TRUE)
    if (!is.null(seed)) {
      parallel::clusterSetRNGStream(cl, iseed = as.integer(seed))
    }
  }

  # Dummy-encode predictors.
  dv <- svm_dummy_encoder(train_set, target, full_rank = TRUE)
  train_x <- svm_encode(dv, train_set)
  test_x <- svm_encode(dv, test_set)

  # Optional feature selection (random-forest RFE; see svm_feature_selection).
  selected_features <- NULL
  if (feature_select) {
    selected_features <- svm_feature_selection(
      train_set,
      target,
      allow_parallel = use_parallel
    )
    keep <- intersect(colnames(train_x), selected_features)
    if (length(keep) == 0L) {
      stop("No features selected by feature selection.", call. = FALSE)
    }
    train_x <- train_x[, keep, drop = FALSE]
    test_x <- test_x[, keep, drop = FALSE]
  }

  # Modeling frames: encoded predictors + target.
  train_df <- cbind(train_x, stats::setNames(list(train_set[[target]]), target))
  test_df <- cbind(test_x, stats::setNames(list(test_set[[target]]), target))

  method <- switch(
    kernel,
    linear = "svmLinear",
    radial = "svmRadial",
    polynomial = "svmPoly"
  )

  if (is.null(tune_grid)) {
    tune_grid <- svm_tune_grid(kernel)
  }

  # Class-imbalance handling happens inside each CV resample (no leakage of
  # duplicated rows across folds), via caret's sampling hook.
  sampling <- if (task == "classification" && class_imbalance) "up" else NULL
  tr_ctrl <- caret::trainControl(
    method = "cv",
    number = nfolds,
    allowParallel = use_parallel,
    sampling = sampling
  )

  svm_fit <- caret::train(
    stats::as.formula(paste(backtick(target), "~ .")),
    data = train_df,
    method = method,
    trControl = tr_ctrl,
    preProcess = c("center", "scale"),
    tuneGrid = tune_grid
  )

  preds <- stats::predict(svm_fit, newdata = test_df)
  perf <- svm_performance(preds, test_df[[target]], task)

  list(
    model = svm_fit,
    test_set = test_set,
    predictions = preds,
    performance = perf,
    selected_features = selected_features
  )
}
