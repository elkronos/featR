###############################################################################
# Packages
###############################################################################
suppressPackageStartupMessages({
  library(caret)
  library(parallel)
  library(doParallel)
  library(dplyr)
  library(randomForest)
  library(foreach)
  library(kernlab)
})

###############################################################################
# Input Validation
###############################################################################

#' Validate Inputs for SVM Workflow
#'
#' Ensures inputs conform to expected types and ranges.
#'
#' @param data A data frame containing the dataset.
#' @param target A string naming the target variable present in \code{data}.
#' @param task Either \code{"classification"} or \code{"regression"}.
#' @param train_ratio Numeric in (0, 1) specifying the training proportion.
#' @param nfolds Integer greater than 1 for cross-validation folds.
#'
#' @return Invisibly returns \code{TRUE} if validation passes.
#' @export
validate_inputs <- function(data, target, task, train_ratio, nfolds) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!is.character(target) || length(target) != 1) stop("`target` must be a single string.")
  if (!(target %in% names(data))) stop("`target` not found in `data`.")
  if (!(task %in% c("classification", "regression"))) {
    stop("`task` must be either 'classification' or 'regression'.")
  }
  if (!is.numeric(train_ratio) || train_ratio <= 0 || train_ratio >= 1) {
    stop("`train_ratio` must be numeric in (0, 1).")
  }
  if (!is.numeric(nfolds) || nfolds != as.integer(nfolds) || nfolds <= 1) {
    stop("`nfolds` must be an integer > 1.")
  }
  if (task == "regression" && !is.numeric(data[[target]])) {
    stop("For regression, the target must be numeric.")
  }
  invisible(TRUE)
}

###############################################################################
# Target Type Coercion
###############################################################################

#' Coerce Target Type According to Task
#'
#' @param data Data frame.
#' @param target Target variable name.
#' @param task Either \code{"classification"} or \code{"regression"}.
#'
#' @return Data frame with target coerced to appropriate type.
#' @export
coerce_target_type <- function(data, target, task) {
  data <- as.data.frame(data)
  if (task == "classification") {
    data[[target]] <- as.factor(data[[target]])
  } else {
    data[[target]] <- as.numeric(data[[target]])
  }
  data
}

###############################################################################
# Data Splitting
###############################################################################

#' Split Data into Training and Test Sets
#'
#' Uses stratified sampling for classification and random sampling for regression.
#'
#' @param data Data frame.
#' @param target Target variable name.
#' @param train_ratio Proportion of data used for training.
#' @param seed Optional integer seed for reproducibility.
#' @param task Either \code{"classification"} or \code{"regression"}.
#'
#' @return A list with \code{train_set} and \code{test_set}.
#' @export
split_data <- function(data, target, train_ratio, seed = NULL, task = c("classification", "regression")) {
  task <- match.arg(task)
  if (!is.null(seed)) set.seed(seed)
  if (task == "classification") {
    idx <- createDataPartition(data[[target]], p = train_ratio, list = FALSE)
  } else {
    n <- nrow(data)
    idx <- sample.int(n, size = floor(train_ratio * n))
  }
  list(
    train_set = data[idx, , drop = FALSE],
    test_set  = data[-idx, , drop = FALSE]
  )
}

###############################################################################
# Dummy Encoding Utilities
###############################################################################

#' Fit Dummy Encoder on Predictors
#'
#' @param data Data frame.
#' @param target Target variable name.
#' @param fullRank Logical; if \code{TRUE}, creates full-rank parameterization.
#'
#' @return A \code{dummyVars} object.
#' @export
fit_dummy_encoder <- function(data, target, fullRank = TRUE) {
  predictors <- setdiff(names(data), target)
  caret::dummyVars(
    formula = stats::reformulate(predictors),
    data = data,
    fullRank = fullRank
  )
}

#' Transform Data with Dummy Encoder
#'
#' @param dv A \code{dummyVars} object.
#' @param data Data frame to transform.
#'
#' @return A numeric data frame of encoded predictors.
#' @export
transform_with_encoder <- function(dv, data) {
  as.data.frame(predict(dv, newdata = data))
}

###############################################################################
# Feature Selection (Robust)
###############################################################################

#' Recursive Feature Elimination on Encoded Predictors (Robust)
#'
#' Performs RFE using random forest functions over dummy-encoded predictors.
#' If RFE yields no selected variables or errors, falls back to random forest
#' variable importance to return a non-empty set of predictors.
#'
#' @param train_set Training data frame.
#' @param target Target variable name.
#' @param seed Optional integer seed.
#' @param rfe_folds Number of CV folds for RFE.
#' @param min_keep Minimum number of predictors to return on fallback.
#'
#' @return Character vector of selected feature names (post-encoding).
#' @export
perform_feature_selection <- function(train_set,
                                      target,
                                      seed = NULL,
                                      rfe_folds = 10,
                                      min_keep = 1) {
  if (!is.null(seed)) set.seed(seed)
  
  y <- train_set[[target]]
  if (!is.numeric(y) && !is.factor(y)) y <- as.factor(y)
  if (is.character(y)) y <- as.factor(y)
  
  dv <- fit_dummy_encoder(train_set, target, fullRank = TRUE)
  X  <- transform_with_encoder(dv, train_set)
  yv <- y
  
  if (ncol(X) == 0) stop("No predictors available for feature selection.")
  
  sizes <- seq_len(ncol(X))
  ctrl  <- caret::rfeControl(functions = caret::rfFuncs,
                             method    = "cv",
                             number    = rfe_folds,
                             verbose   = FALSE)
  
  rfe_vars <- tryCatch({
    rf_rfe <- caret::rfe(x = X, y = yv, sizes = sizes, rfeControl = ctrl)
    out <- caret::predictors(rf_rfe)
    if (length(out) > 0) out else character()
  }, error = function(e) character())
  
  if (length(rfe_vars) > 0) return(rfe_vars)
  
  rf_fit <- randomForest::randomForest(x = X, y = yv, importance = TRUE)
  imp    <- randomForest::importance(rf_fit, type = 2)
  imp    <- as.data.frame(imp)
  imp$Feature <- rownames(imp)
  score_col <- tail(names(imp)[vapply(imp, is.numeric, logical(1))], 1)
  ord <- order(imp[[score_col]], decreasing = TRUE, na.last = NA)
  
  keep_n <- max(min_keep, 1)
  selected <- imp$Feature[ord][seq_len(min(keep_n, length(ord)))]
  if (length(selected) == 0) {
    selected <- colnames(X)[seq_len(min(keep_n, ncol(X)))]
  }
  selected
}

###############################################################################
# Class Imbalance Handling
###############################################################################

#' Up-Sample Minority Classes in Training Data
#'
#' @param train_df Training data frame with numeric predictors and target column.
#' @param target Target variable name.
#'
#' @return Balanced training data frame.
#' @export
handle_class_imbalance <- function(train_df, target) {
  caret::upSample(
    x = train_df[, setdiff(names(train_df), target), drop = FALSE],
    y = train_df[[target]],
    yname = target
  )
}

###############################################################################
# Default Tuning Grids
###############################################################################

#' Default Hyperparameter Grids for SVM
#'
#' @param kernel One of \code{"linear"}, \code{"radial"}, or \code{"polynomial"}.
#'
#' @return A data frame suitable for \code{caret::train(tuneGrid = ...)}.
#' @export
default_tune_grid <- function(kernel = c("linear", "radial", "polynomial")) {
  kernel <- match.arg(kernel)
  if (kernel == "linear") {
    expand.grid(C = 2 ^ seq(-5, 10, by = 1))
  } else if (kernel == "radial") {
    expand.grid(sigma = 2 ^ seq(-15, -5, by = 1),
                C     = 2 ^ seq(-1, 10, by = 1))
  } else {
    expand.grid(degree = 2:5,
                scale  = c(0.1, 0.5, 1),
                C      = 2 ^ seq(-3, 8, by = 1))
  }
}

###############################################################################
# Performance Metrics
###############################################################################

#' Compute Performance Metrics
#'
#' For classification, returns a \code{caret} confusion matrix.
#' For regression, returns a named numeric vector with RMSE, Rsquared, and MAE.
#'
#' @param predictions Predicted values.
#' @param actuals True target values.
#' @param task Either \code{"classification"} or \code{"regression"}.
#'
#' @return For classification: a \code{confusionMatrix} object.\cr
#' For regression: a named numeric vector with elements \code{RMSE}, \code{Rsquared}, and \code{MAE}.
#' @export
calculate_performance <- function(predictions, actuals, task) {
  if (task == "classification") {
    predictions <- factor(predictions, levels = levels(actuals))
    caret::confusionMatrix(predictions, actuals)
  } else {
    pr <- caret::postResample(pred = predictions, obs = actuals)
    mae <- mean(abs(predictions - actuals))
    c(RMSE = unname(pr["RMSE"]), Rsquared = unname(pr["Rsquared"]), MAE = mae)
  }
}

###############################################################################
# Parallel Processing
###############################################################################

#' Start Parallel Backend
#'
#' Uses \code{detectCores() - 1} workers (minimum 1).
#'
#' @return A cluster object.
#' @export
setup_parallel_processing <- function() {
  cores <- max(parallel::detectCores() - 1, 1)
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  cl
}

#' Stop Parallel Backend
#'
#' @param cl A cluster object returned by \code{setup_parallel_processing()}.
#' @export
stop_parallel_processing <- function(cl) {
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
    doParallel::stopImplicitCluster()
  }
}

###############################################################################
# Main API
###############################################################################

#' Train and Evaluate an SVM with Optional Feature Selection and Class Balancing
#'
#' Trains an SVM classifier or regressor using \pkg{caret}, with options for
#' dummy-encoding of predictors, recursive feature elimination (RFE),
#' class imbalance handling (up-sampling), and hyperparameter tuning via
#' cross-validation. Parallel training is supported.
#'
#' @param data A data frame containing predictors and the target.
#' @param target A string naming the target variable.
#' @param task Either \code{"classification"} or \code{"regression"}.
#' @param train_ratio Training set proportion (default \code{0.7}).
#' @param nfolds Number of CV folds (default \code{5}).
#' @param tune_grid Optional tuning grid data frame. If \code{NULL}, a default grid is used.
#' @param seed Optional integer seed.
#' @param feature_select Logical; if \code{TRUE}, performs RFE on encoded predictors (default \code{FALSE}).
#' @param class_imbalance Logical; if \code{TRUE} and task is classification, up-samples classes (default \code{FALSE}).
#' @param kernel One of \code{"linear"}, \code{"radial"}, or \code{"polynomial"}.
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{model}: Trained \code{caret} model object.
#'   \item \code{test_set}: Test set (with original columns and coerced target).
#'   \item \code{predictions}: Predictions on the encoded test set.
#'   \item \code{performance}: Performance metrics.
#'   \item \code{selected_features}: Names of selected encoded features (if \code{feature_select = TRUE}).
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
                   kernel = c("linear", "radial", "polynomial")) {
  
  validate_inputs(data, target, task, train_ratio, nfolds)
  data <- coerce_target_type(data, target, task)
  
  splits <- split_data(data, target, train_ratio, seed, task)
  train_set <- splits$train_set
  test_set  <- splits$test_set
  
  if (task == "classification") {
    train_set[[target]] <- droplevels(train_set[[target]])
    test_set[[target]]  <- factor(test_set[[target]], levels = levels(train_set[[target]]))
  }
  
  dv <- fit_dummy_encoder(train_set, target, fullRank = TRUE)
  train_x <- transform_with_encoder(dv, train_set)
  test_x  <- transform_with_encoder(dv, test_set)
  
  selected_features <- NULL
  if (isTRUE(feature_select)) {
    selected_features <- perform_feature_selection(train_set, target, seed = seed)
    keep <- intersect(colnames(train_x), selected_features)
    if (length(keep) == 0) stop("No features selected by feature selection.")
    train_x <- train_x[, keep, drop = FALSE]
    test_x  <- test_x[, keep, drop = FALSE]
  }
  
  train_df <- cbind(train_x, setNames(list(train_set[[target]]), target))
  test_df  <- cbind(test_x,  setNames(list(test_set[[target]]),  target))
  
  if (task == "classification" && isTRUE(class_imbalance)) {
    train_df <- handle_class_imbalance(train_df, target)
  }
  
  kernel <- match.arg(kernel)
  method <- switch(kernel,
                   linear     = "svmLinear",
                   radial     = "svmRadial",
                   polynomial = "svmPoly")
  
  if (is.null(tune_grid)) {
    tune_grid <- default_tune_grid(kernel)
  }
  
  tr_ctrl <- caret::trainControl(
    method = "cv",
    number = nfolds,
    allowParallel = TRUE
  )
  
  cl <- setup_parallel_processing()
  on.exit(stop_parallel_processing(cl), add = TRUE)
  if (!is.null(seed)) set.seed(seed)
  
  pre_proc <- c("center", "scale")
  
  formula_str <- paste(target, "~ .")
  svm_fit <- caret::train(
    as.formula(formula_str),   # positional arg to fill `form` across caret versions
    data = train_df,
    method = method,
    trControl = tr_ctrl,
    preProcess = pre_proc,
    tuneGrid = tune_grid
  )
  
  preds <- predict(svm_fit, newdata = test_df)
  perf <- calculate_performance(preds, test_df[[target]], task)
  
  list(
    model = svm_fit,
    test_set = test_set,
    predictions = preds,
    performance = perf,
    selected_features = selected_features
  )
}
