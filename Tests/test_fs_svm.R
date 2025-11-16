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
  library(kernlab)   # caret's svmLinear/svmRadial backend (ksvm)
})

###############################################################################
# Testing Infrastructure - SVM (fs_svm and helpers)
###############################################################################
# Optional: if fs_svm and helpers are in a separate script/package, load here:
# source("fs_svm.R")
# library(yourpackage)

#' Initialize Test Results Container
#'
#' Creates or resets the global results data frame used to record test outcomes.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
init_test_results <- function() {
  df <- data.frame(
    Test    = character(),
    Result  = factor(levels = c("PASS", "FAIL")),
    Message = character(),
    stringsAsFactors = FALSE
  )
  assign("test_results", df, envir = .GlobalEnv)
  invisible(TRUE)
}

#' Print and Store a Test Result
#'
#' Records a single test outcome and prints a formatted line.
#'
#' @param test_name Character scalar name of the test.
#' @param passed Logical indicating if the test passed.
#' @param message Optional character message for additional context.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
print_and_store_result <- function(test_name, passed, message = NULL) {
  if (!exists("test_results", envir = .GlobalEnv)) init_test_results()
  result <- if (isTRUE(passed)) "PASS" else "FAIL"
  cat(sprintf("%-70s [%s]\n", test_name, result))
  if (!is.null(message) && nzchar(message)) cat("  ", message, "\n", sep = "")
  new_row <- data.frame(
    Test    = test_name,
    Result  = factor(result, levels = c("PASS", "FAIL")),
    Message = ifelse(is.null(message), "", message),
    stringsAsFactors = FALSE
  )
  test_results <<- dplyr::bind_rows(test_results, new_row)
  invisible(TRUE)
}

#' Retrieve Test Results
#'
#' @return A data frame with recorded test outcomes.
#' @export
get_test_results <- function() {
  if (!exists("test_results", envir = .GlobalEnv)) init_test_results()
  test_results
}

###############################################################################
# Existence Tests
###############################################################################

#' Test: Core Function Existence
#'
#' Verifies that all exported core functions exist and are callable.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_fs_svm_existence <- function() {
  funs <- c(
    "validate_inputs",
    "coerce_target_type",
    "split_data",
    "fit_dummy_encoder",
    "transform_with_encoder",
    "perform_feature_selection",
    "handle_class_imbalance",
    "default_tune_grid",
    "calculate_performance",
    "setup_parallel_processing",
    "stop_parallel_processing",
    "fs_svm"
  )
  for (fn in funs) {
    err <- NULL
    passed <- tryCatch({
      exists(fn, mode = "function") && is.function(get(fn))
    }, error = function(e) {
      err <<- e
      FALSE
    })
    msg <- if (!is.null(err)) conditionMessage(err) else NULL
    print_and_store_result(
      sprintf("Existence: %s exists and is callable", fn),
      passed,
      msg
    )
  }
  invisible(TRUE)
}

###############################################################################
# Unit Tests - validate_inputs
###############################################################################

#' Test: validate_inputs
#'
#' Executes validation scenarios for \code{validate_inputs()}.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_validate_inputs <- function() {
  # Valid classification
  valid_class <- tryCatch({
    validate_inputs(iris, "Species", "classification", 0.7, 5)
    TRUE
  }, error = function(e) FALSE)
  
  # Valid regression
  valid_reg <- tryCatch({
    validate_inputs(mtcars, "mpg", "regression", 0.8, 3)
    TRUE
  }, error = function(e) FALSE)
  
  # Invalid: data not a data.frame
  err_data <- NULL
  invalid_data <- tryCatch({
    validate_inputs(matrix(1:10), "Species", "classification", 0.7, 5)
    FALSE
  }, error = function(e) { err_data <<- e; TRUE })
  
  # Invalid: target column missing
  err_target <- NULL
  invalid_target <- tryCatch({
    validate_inputs(iris, "NonExistent", "classification", 0.7, 5)
    FALSE
  }, error = function(e) { err_target <<- e; TRUE })
  
  # Invalid: task value
  err_task <- NULL
  invalid_task <- tryCatch({
    validate_inputs(iris, "Species", "not_a_task", 0.7, 5)
    FALSE
  }, error = function(e) { err_task <<- e; TRUE })
  
  # Invalid: train_ratio out of (0,1)
  err_ratio <- NULL
  invalid_train_ratio <- tryCatch({
    validate_inputs(iris, "Species", "classification", 1.2, 5)
    FALSE
  }, error = function(e) { err_ratio <<- e; TRUE })
  
  # Invalid: nfolds <= 1
  err_nfolds <- NULL
  invalid_nfolds <- tryCatch({
    validate_inputs(iris, "Species", "classification", 0.7, 1)
    FALSE
  }, error = function(e) { err_nfolds <<- e; TRUE })
  
  # Invalid: classification target has < 2 classes
  iris_one_class <- subset(iris, Species == "setosa")
  err_classes <- NULL
  invalid_classes <- tryCatch({
    validate_inputs(iris_one_class, "Species", "classification", 0.7, 5)
    FALSE
  }, error = function(e) { err_classes <<- e; TRUE })
  
  # Invalid: target has NA
  iris_target_na <- iris
  iris_target_na$Species[1] <- NA
  err_target_na <- NULL
  invalid_target_na <- tryCatch({
    validate_inputs(iris_target_na, "Species", "classification", 0.7, 5)
    FALSE
  }, error = function(e) { err_target_na <<- e; TRUE })
  
  # Invalid: predictors have NA
  iris_pred_na <- iris
  iris_pred_na$Sepal.Length[1] <- NA
  err_pred_na <- NULL
  invalid_pred_na <- tryCatch({
    validate_inputs(iris_pred_na, "Species", "classification", 0.7, 5)
    FALSE
  }, error = function(e) { err_pred_na <<- e; TRUE })
  
  # Invalid: no predictors (only target column)
  df_only_target <- iris["Species"]
  err_no_pred <- NULL
  invalid_no_pred <- tryCatch({
    validate_inputs(df_only_target, "Species", "classification", 0.7, 5)
    FALSE
  }, error = function(e) { err_no_pred <<- e; TRUE })
  
  # Invalid: regression with non-numeric target
  df_char_target <- data.frame(
    mpg_chr = as.character(mtcars$mpg),
    mtcars[, setdiff(names(mtcars), "mpg"), drop = FALSE]
  )
  err_reg_target <- NULL
  invalid_reg_target <- tryCatch({
    validate_inputs(df_char_target, "mpg_chr", "regression", 0.7, 5)
    FALSE
  }, error = function(e) { err_reg_target <<- e; TRUE })
  
  print_and_store_result("validate_inputs: valid classification", valid_class)
  print_and_store_result("validate_inputs: valid regression", valid_reg)
  print_and_store_result(
    "validate_inputs: errors if data is not data.frame",
    invalid_data,
    if (!is.null(err_data)) conditionMessage(err_data)
  )
  print_and_store_result(
    "validate_inputs: errors if target missing",
    invalid_target,
    if (!is.null(err_target)) conditionMessage(err_target)
  )
  print_and_store_result(
    "validate_inputs: errors if task is invalid",
    invalid_task,
    if (!is.null(err_task)) conditionMessage(err_task)
  )
  print_and_store_result(
    "validate_inputs: errors if train_ratio not in (0,1)",
    invalid_train_ratio,
    if (!is.null(err_ratio)) conditionMessage(err_ratio)
  )
  print_and_store_result(
    "validate_inputs: errors if nfolds <= 1",
    invalid_nfolds,
    if (!is.null(err_nfolds)) conditionMessage(err_nfolds)
  )
  print_and_store_result(
    "validate_inputs: errors if classification target has <2 classes",
    invalid_classes,
    if (!is.null(err_classes)) conditionMessage(err_classes)
  )
  print_and_store_result(
    "validate_inputs: errors if target contains NA",
    invalid_target_na,
    if (!is.null(err_target_na)) conditionMessage(err_target_na)
  )
  print_and_store_result(
    "validate_inputs: errors if predictors contain NA",
    invalid_pred_na,
    if (!is.null(err_pred_na)) conditionMessage(err_pred_na)
  )
  print_and_store_result(
    "validate_inputs: errors if there are no predictors",
    invalid_no_pred,
    if (!is.null(err_no_pred)) conditionMessage(err_no_pred)
  )
  print_and_store_result(
    "validate_inputs: errors if regression target is non-numeric",
    invalid_reg_target,
    if (!is.null(err_reg_target)) conditionMessage(err_reg_target)
  )
  
  invisible(TRUE)
}

###############################################################################
# Unit Tests - coerce_target_type
###############################################################################

#' Test: coerce_target_type
#'
#' Verifies target coercion behavior for classification and regression.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_coerce_target_type <- function() {
  df_class <- iris
  df_class$Species <- as.character(df_class$Species)
  out_class <- coerce_target_type(df_class, "Species", "classification")
  passed_class <- is.factor(out_class$Species)
  
  df_reg <- iris
  df_reg$Sepal.Length <- as.character(df_reg$Sepal.Length)
  out_reg <- coerce_target_type(df_reg, "Sepal.Length", "regression")
  passed_reg <- is.numeric(out_reg$Sepal.Length)
  
  print_and_store_result(
    "coerce_target_type: classification -> factor target",
    passed_class
  )
  print_and_store_result(
    "coerce_target_type: regression -> numeric target",
    passed_reg
  )
  invisible(TRUE)
}

###############################################################################
# Unit Tests - split_data
###############################################################################

#' Test: split_data
#'
#' Validates split sizes, non-empty splits, and partition integrity.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_split_data <- function() {
  # Classification: iris
  splits_c <- split_data(iris, "Species", 0.7, seed = 123, task = "classification")
  train_c  <- splits_c$train_set
  test_c   <- splits_c$test_set
  
  correct_sizes_c <- (nrow(train_c) + nrow(test_c) == nrow(iris)) &&
    (nrow(train_c) == round(0.7 * nrow(iris))) &&
    nrow(train_c) > 0 && nrow(test_c) > 0
  
  all_classes_in_train <- length(unique(train_c$Species)) >= 2
  
  no_overlap_c <- length(intersect(
    rownames(train_c), rownames(test_c)
  )) == 0
  
  # Regression: mtcars
  splits_r <- split_data(mtcars, "mpg", 0.7, seed = 123, task = "regression")
  train_r  <- splits_r$train_set
  test_r   <- splits_r$test_set
  
  correct_sizes_r <- (nrow(train_r) + nrow(test_r) == nrow(mtcars)) &&
    (nrow(train_r) == floor(0.7 * nrow(mtcars))) &&
    nrow(train_r) > 0 && nrow(test_r) > 0
  
  no_overlap_r <- length(intersect(
    rownames(train_r), rownames(test_r)
  )) == 0
  
  print_and_store_result("split_data: classification correct sizes & non-empty", correct_sizes_c)
  print_and_store_result("split_data: classification train has >=2 classes", all_classes_in_train)
  print_and_store_result("split_data: classification train/test disjoint", no_overlap_c)
  print_and_store_result("split_data: regression correct sizes & non-empty", correct_sizes_r)
  print_and_store_result("split_data: regression train/test disjoint", no_overlap_r)
  invisible(TRUE)
}

###############################################################################
# Unit Tests - Dummy Encoder
###############################################################################

#' Test: fit_dummy_encoder / transform_with_encoder
#'
#' Ensures dummy encoding is created and applied correctly.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_dummy_encoder <- function() {
  # Normal case: iris predictors
  err <- NULL
  passed_main <- tryCatch({
    dv <- fit_dummy_encoder(iris, "Species", fullRank = TRUE)
    encoded <- transform_with_encoder(dv, iris)
    is.list(dv) &&
      is.data.frame(encoded) &&
      nrow(encoded) == nrow(iris) &&
      ncol(encoded) >= 1L &&
      all(vapply(encoded, is.numeric, logical(1)))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  msg_main <- if (!is.null(err)) conditionMessage(err) else NULL
  
  # Error case: no predictors
  df_only_target <- iris["Species"]
  err2 <- NULL
  passed_err <- tryCatch({
    fit_dummy_encoder(df_only_target, "Species", fullRank = TRUE)
    FALSE
  }, error = function(e) {
    err2 <<- e
    TRUE
  })
  msg_err <- if (!is.null(err2)) conditionMessage(err2) else NULL
  
  print_and_store_result(
    "dummy encoder: encodes iris predictors to numeric frame",
    passed_main,
    msg_main
  )
  print_and_store_result(
    "dummy encoder: errors when no predictors",
    passed_err,
    msg_err
  )
  invisible(TRUE)
}

###############################################################################
# Unit Tests - perform_feature_selection
###############################################################################

#' Test: perform_feature_selection
#'
#' Checks that known informative \code{iris} predictors are selected.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_perform_feature_selection <- function() {
  selected <- tryCatch({
    perform_feature_selection(iris, "Species", seed = 123, rfe_folds = 3)
  }, error = function(e) character())
  
  expected <- c("Petal.Length", "Petal.Width")
  correct <- length(selected) > 0 && all(expected %in% selected)
  msg <- if (!length(selected)) {
    "No features selected"
  } else {
    paste("Selected:", paste(head(selected, 10), collapse = ", "))
  }
  print_and_store_result("perform_feature_selection: expected features present", correct, msg)
  invisible(TRUE)
}

###############################################################################
# Unit Tests - handle_class_imbalance
###############################################################################

#' Test: handle_class_imbalance
#'
#' Ensures up-sampling balances class counts.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_handle_class_imbalance <- function() {
  iris2 <- iris
  iris2$Species <- as.factor(iris2$Species)
  balanced <- handle_class_imbalance(iris2, "Species")
  counts <- table(balanced$Species)
  correct <- length(unique(counts)) == 1
  msg <- paste("Counts:", paste(counts, collapse = ", "))
  print_and_store_result("handle_class_imbalance: balanced classes", correct, msg)
  invisible(TRUE)
}

###############################################################################
# Unit Tests - default_tune_grid
###############################################################################

#' Test: default_tune_grid
#'
#' Validates default grids for supported kernels.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_default_tune_grid <- function() {
  g_lin  <- default_tune_grid("linear")
  g_rad  <- default_tune_grid("radial")
  g_poly <- default_tune_grid("polynomial")
  
  ok_lin  <- identical(sort(names(g_lin)),  c("C"))
  ok_rad  <- identical(sort(names(g_rad)),  sort(c("C", "sigma")))
  ok_poly <- identical(sort(names(g_poly)), sort(c("C", "degree", "scale")))
  
  print_and_store_result("default_tune_grid: linear has C", ok_lin)
  print_and_store_result("default_tune_grid: radial has C & sigma", ok_rad)
  print_and_store_result("default_tune_grid: polynomial has C, degree, scale", ok_poly)
  invisible(TRUE)
}

###############################################################################
# Unit Tests - calculate_performance
###############################################################################

#' Test: calculate_performance
#'
#' Verifies structures for classification and regression metrics, including NA handling.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_calculate_performance <- function() {
  set.seed(123)
  # Classification
  actuals_c <- factor(sample(c("A", "B"), 120, replace = TRUE))
  preds_c   <- factor(sample(c("A", "B"), 120, replace = TRUE), levels = levels(actuals_c))
  # inject some NA
  preds_c[sample.int(120, 5)] <- NA
  perf_c <- calculate_performance(preds_c, actuals_c, "classification")
  ok_c   <- inherits(perf_c, "confusionMatrix")
  
  # Regression
  actuals_r <- rnorm(120)
  preds_r   <- rnorm(120)
  # inject some NA
  preds_r[sample.int(120, 5)] <- NA
  perf_r <- calculate_performance(preds_r, actuals_r, "regression")
  ok_r   <- is.numeric(perf_r) &&
    all(c("RMSE", "Rsquared", "MAE") %in% names(perf_r)) &&
    all(is.finite(perf_r[!is.na(perf_r)]))
  
  msg_r <- paste0(
    "Metrics: RMSE=", round(perf_r["RMSE"], 3),
    ", R2=", round(perf_r["Rsquared"], 3),
    ", MAE=", round(perf_r["MAE"], 3)
  )
  
  print_and_store_result("calculate_performance: classification object", ok_c)
  print_and_store_result("calculate_performance: regression metrics & NA handling", ok_r, msg_r)
  invisible(TRUE)
}

###############################################################################
# Unit Tests - setup_parallel_processing / stop_parallel_processing
###############################################################################

#' Test: setup_parallel_processing / stop_parallel_processing
#'
#' Confirms cluster creation and cleanup behavior for the exported helpers.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_parallel_processing <- function() {
  err_setup <- NULL
  cl <- tryCatch({
    setup_parallel_processing()
  }, error = function(e) {
    err_setup <<- e
    NULL
  })
  
  ok_setup <- !is.null(cl) && inherits(cl, "cluster")
  msg_setup <- if (!is.null(err_setup)) conditionMessage(err_setup) else NULL
  
  if (!is.null(cl)) {
    stop_parallel_processing(cl)
  }
  
  err_stop <- NULL
  ok_stop <- tryCatch({
    # Using a stopped cluster should result in error when used
    parallel::parLapply(cl, 1:2, function(x) x)
    FALSE
  }, error = function(e) {
    err_stop <<- e
    TRUE
  })
  msg_stop <- if (!is.null(err_stop)) conditionMessage(err_stop) else NULL
  
  print_and_store_result("parallel: setup cluster returns 'cluster' object", ok_setup, msg_setup)
  print_and_store_result("parallel: stopped cluster cannot be used", ok_stop, msg_stop)
  invisible(TRUE)
}

###############################################################################
# End-to-End Tests - fs_svm
###############################################################################

#' Test: fs_svm End-to-End (classification & regression)
#'
#' Runs classification and regression examples; on error, prints diagnostics.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_fs_svm <- function() {
  debug_dump <- function(tag, err_msg, kernel, task, grid = NULL, preproc = NULL) {
    grid_head <- tryCatch(capture.output(print(utils::head(grid))), error = function(e) "NA")
    sess <- tryCatch({
      si <- sessionInfo()
      paste0("R ", getRversion(), " | ",
             paste0(c(si$basePkgs, names(si$otherPkgs)), collapse = ", "))
    }, error = function(e) "sessionInfo() unavailable")
    paste0(
      "[", tag, "] ", err_msg, "\n",
      "task=", task, " | kernel=", kernel, "\n",
      "preProcess=", if (is.null(preproc)) "NULL" else paste(preproc, collapse = ","), "\n",
      "tuneGrid(head)=\n", paste(grid_head, collapse = "\n"), "\n",
      "session=", sess
    )
  }
  
  # Classification path
  class_err <- NULL
  class_out <- tryCatch({
    fs_svm(
      data            = iris,
      target          = "Species",
      task            = "classification",
      train_ratio     = 0.7,
      nfolds          = 3,
      seed            = 123,
      feature_select  = TRUE,
      class_imbalance = TRUE,
      kernel          = "radial"
    )
  }, error = function(e) { 
    dbg <- debug_dump(
      tag    = "classification",
      err_msg = conditionMessage(e),
      kernel  = "radial",
      task    = "classification",
      grid    = try(default_tune_grid("radial"), silent = TRUE),
      preproc = c("center","scale")
    )
    class_err <<- dbg
    NULL
  })
  
  ok_class <- !is.null(class_out) &&
    all(c("model", "test_set", "predictions", "performance", "selected_features") %in% names(class_out)) &&
    inherits(class_out$model, "train") &&
    length(class_out$predictions) == nrow(class_out$test_set) &&
    is.null(class_out$selected_features) == FALSE &&
    length(class_out$selected_features) > 0
  
  print_and_store_result(
    "fs_svm: classification pipeline (radial, feature_select=TRUE, class_imbalance=TRUE)",
    ok_class,
    ifelse(is.null(class_err), "", class_err)
  )
  
  # Regression path
  reg_err <- NULL
  reg_out <- tryCatch({
    fs_svm(
      data           = mtcars,
      target         = "mpg",
      task           = "regression",
      train_ratio    = 0.7,
      nfolds         = 3,
      seed           = 123,
      feature_select = TRUE,
      kernel         = "linear"
    )
  }, error = function(e) { 
    dbg <- debug_dump(
      tag    = "regression",
      err_msg = conditionMessage(e),
      kernel  = "linear",
      task    = "regression",
      grid    = try(default_tune_grid("linear"), silent = TRUE),
      preproc = c("center","scale")
    )
    reg_err <<- dbg
    NULL
  })
  
  ok_reg <- !is.null(reg_out) &&
    all(c("model", "test_set", "predictions", "performance", "selected_features") %in% names(reg_out)) &&
    inherits(reg_out$model, "train") &&
    length(reg_out$predictions) == nrow(reg_out$test_set) &&
    is.null(reg_out$selected_features) == FALSE &&
    length(reg_out$selected_features) > 0
  
  print_and_store_result(
    "fs_svm: regression pipeline (linear, feature_select=TRUE)",
    ok_reg,
    ifelse(is.null(reg_err), "", reg_err)
  )
  
  # Minimal classification path (no feature selection, no class imbalance)
  min_err <- NULL
  min_out <- tryCatch({
    fs_svm(
      data            = iris,
      target          = "Species",
      task            = "classification",
      train_ratio     = 0.7,
      nfolds          = 3,
      seed            = 123,
      feature_select  = FALSE,
      class_imbalance = FALSE,
      kernel          = "linear"
    )
  }, error = function(e) {
    min_err <<- e
    NULL
  })
  
  ok_min <- !is.null(min_out) &&
    all(c("model", "test_set", "predictions", "performance", "selected_features") %in% names(min_out)) &&
    is.null(min_out$selected_features)
  
  msg_min <- if (!is.null(min_err)) conditionMessage(min_err) else NULL
  
  print_and_store_result(
    "fs_svm: minimal classification pipeline (linear, feature_select=FALSE, class_imbalance=FALSE)",
    ok_min,
    msg_min
  )
  
  invisible(TRUE)
}

###############################################################################
# Runner
###############################################################################

#' Run All SVM Tests
#'
#' Executes the full fs_svm test suite and prints a summary and detailed results.
#'
#' @return Invisibly returns the test results data frame.
#' @export
run_svm_tests <- function() {
  init_test_results()
  cat("========== Running fs_svm Tests ==========\n")
  test_fs_svm_existence()
  test_validate_inputs()
  test_coerce_target_type()
  test_split_data()
  test_dummy_encoder()
  test_perform_feature_selection()
  test_handle_class_imbalance()
  test_default_tune_grid()
  test_calculate_performance()
  test_parallel_processing()
  test_fs_svm()
  cat("========== fs_svm Tests Completed ==========\n\n")
  res <- get_test_results()
  cat("Test Summary:\n")
  print(table(res$Result))
  cat("\nDetailed Results:\n")
  print(res)
  invisible(res)
}

# To execute the suite:
# run_svm_tests()
