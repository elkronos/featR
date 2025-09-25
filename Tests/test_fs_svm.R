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
# Testing Infrastructure - SVM
###############################################################################

#' Initialize Test Results Container
#'
#' Creates or resets the global results data frame used to record test outcomes.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
init_test_results <- function() {
  df <- data.frame(
    Test = character(),
    Result = factor(levels = c("PASS", "FAIL")),
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
  cat(sprintf("%-64s [%s]\n", test_name, result))
  if (!is.null(message) && nzchar(message)) cat("  ", message, "\n", sep = "")
  new_row <- data.frame(
    Test = test_name,
    Result = factor(result, levels = c("PASS", "FAIL")),
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
# Unit Tests
###############################################################################

#' Test: validate_inputs
#'
#' Executes validation scenarios for \code{validate_inputs()}.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_validate_inputs <- function() {
  valid_test <- tryCatch({
    validate_inputs(iris, "Species", "classification", 0.7, 5)
    TRUE
  }, error = function(e) FALSE)
  
  invalid_data <- tryCatch({
    validate_inputs(matrix(1:10), "Species", "classification", 0.7, 5)
    FALSE
  }, error = function(e) TRUE)
  
  invalid_target <- tryCatch({
    validate_inputs(iris, "NonExistent", "classification", 0.7, 5)
    FALSE
  }, error = function(e) TRUE)
  
  invalid_task <- tryCatch({
    validate_inputs(iris, "Species", "not_a_task", 0.7, 5)
    FALSE
  }, error = function(e) TRUE)
  
  invalid_train_ratio <- tryCatch({
    validate_inputs(iris, "Species",  "classification", 1.2, 5)
    FALSE
  }, error = function(e) TRUE)
  
  invalid_nfolds <- tryCatch({
    validate_inputs(iris, "Species", "classification", 0.7, 1)
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("validate_inputs: valid input", valid_test)
  print_and_store_result("validate_inputs: invalid data", invalid_data)
  print_and_store_result("validate_inputs: invalid target", invalid_target)
  print_and_store_result("validate_inputs: invalid task", invalid_task)
  print_and_store_result("validate_inputs: invalid train_ratio", invalid_train_ratio)
  print_and_store_result("validate_inputs: invalid nfolds", invalid_nfolds)
  invisible(TRUE)
}

#' Test: split_data
#'
#' Validates split sizes and partition integrity.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_split_data <- function() {
  splits <- split_data(iris, "Species", 0.7, seed = 123, task = "classification")
  train_set <- splits$train_set
  test_set  <- splits$test_set
  correct_split <- (nrow(train_set) == round(0.7 * nrow(iris))) &&
    (nrow(test_set) == nrow(iris) - nrow(train_set))
  print_and_store_result("split_data: correct split sizes", correct_split)
  invisible(TRUE)
}

#' Test: perform_feature_selection
#'
#' Checks that known informative \code{iris} predictors are selected.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_perform_feature_selection <- function() {
  selected <- tryCatch({
    perform_feature_selection(iris, "Species", seed = 123, rfe_folds = 5)
  }, error = function(e) character())
  
  expected <- c("Petal.Length", "Petal.Width")
  correct <- length(selected) > 0 && all(expected %in% selected)
  msg <- if (!length(selected)) "No features selected" else paste("Selected:", paste(head(selected, 10), collapse = ", "))
  print_and_store_result("perform_feature_selection: expected features present", correct, msg)
  invisible(TRUE)
}

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
  print_and_store_result("handle_class_imbalance: balanced classes", correct, paste("Counts:", paste(counts, collapse = ", ")))
  invisible(TRUE)
}

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
  
  print_and_store_result("default_tune_grid: linear", ok_lin)
  print_and_store_result("default_tune_grid: radial", ok_rad)
  print_and_store_result("default_tune_grid: polynomial", ok_poly)
  invisible(TRUE)
}

#' Test: calculate_performance
#'
#' Verifies structures for classification and regression metrics.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_calculate_performance <- function() {
  set.seed(123)
  # Classification
  actuals_c <- factor(sample(c("A", "B"), 120, replace = TRUE))
  preds_c   <- factor(sample(c("A", "B"), 120, replace = TRUE), levels = levels(actuals_c))
  perf_c    <- calculate_performance(preds_c, actuals_c, "classification")
  ok_c      <- inherits(perf_c, "confusionMatrix")
  
  # Regression
  actuals_r <- rnorm(120)
  preds_r   <- rnorm(120)
  perf_r    <- calculate_performance(preds_r, actuals_r, "regression")
  ok_r      <- is.numeric(perf_r) && all(c("RMSE", "Rsquared", "MAE") %in% names(perf_r))
  
  print_and_store_result("calculate_performance: classification object", ok_c)
  print_and_store_result("calculate_performance: regression metrics", ok_r,
                         paste0("Metrics: RMSE=", round(perf_r["RMSE"], 3),
                                ", R2=", round(perf_r["Rsquared"], 3),
                                ", MAE=", round(perf_r["MAE"], 3)))
  invisible(TRUE)
}

#' Test: setup_parallel_processing / stop_parallel_processing
#'
#' Confirms cluster creation and cleanup behavior.
#'
#' @return Invisibly returns \code{TRUE}.
#' @export
test_parallel_processing <- function() {
  cl <- setup_parallel_processing()
  ok_setup <- inherits(cl, "cluster")
  stop_parallel_processing(cl)
  
  ok_stop <- tryCatch({
    parallel::parLapply(cl, 1:2, function(x) x)
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("parallel: setup cluster", ok_setup)
  print_and_store_result("parallel: stop cluster", ok_stop)
  invisible(TRUE)
}

#' Test: fs_svm End-to-End (with debugging)
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
             paste0(c(names(si$basePkgs), names(si$otherPkgs)), collapse = ", "))
    }, error = function(e) "sessionInfo() unavailable")
    paste0(
      "[", tag, "] ", err_msg, "\n",
      "task=", task, " | kernel=", kernel, "\n",
      "preProcess=", if (is.null(preproc)) "NULL" else paste(preproc, collapse = ","), "\n",
      "tuneGrid(head)=\n", paste(grid_head, collapse = "\n"), "\n",
      "session=", sess
    )
  }
  
  # Classification
  class_err <- NULL
  class_out <- tryCatch({
    fs_svm(
      data = iris,
      target = "Species",
      task = "classification",
      train_ratio = 0.7,
      nfolds = 5,
      seed = 123,
      feature_select = TRUE,
      class_imbalance = TRUE,
      kernel = "radial"
    )
  }, error = function(e) { 
    dbg <- debug_dump(
      tag = "classification",
      err_msg = conditionMessage(e),
      kernel = "radial",
      task = "classification",
      grid = try(default_tune_grid("radial"), silent = TRUE),
      preproc = c("center","scale")
    )
    class_err <<- dbg
    NULL
  })
  
  ok_class <- !is.null(class_out) &&
    all(c("model", "test_set", "predictions", "performance") %in% names(class_out))
  print_and_store_result("fs_svm: classification pipeline", ok_class, ifelse(is.null(class_err), "", class_err))
  
  # Regression
  reg_err <- NULL
  reg_out <- tryCatch({
    fs_svm(
      data = mtcars,
      target = "mpg",
      task = "regression",
      train_ratio = 0.7,
      nfolds = 5,
      seed = 123,
      feature_select = TRUE,
      kernel = "linear"
    )
  }, error = function(e) { 
    dbg <- debug_dump(
      tag = "regression",
      err_msg = conditionMessage(e),
      kernel = "linear",
      task = "regression",
      grid = try(default_tune_grid("linear"), silent = TRUE),
      preproc = c("center","scale")
    )
    reg_err <<- dbg
    NULL
  })
  
  ok_reg <- !is.null(reg_out) &&
    all(c("model", "test_set", "predictions", "performance") %in% names(reg_out))
  print_and_store_result("fs_svm: regression pipeline", ok_reg, ifelse(is.null(reg_err), "", reg_err))
  invisible(TRUE)
}

###############################################################################
# Parallel Backend Helpers
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
# Runner
###############################################################################

#' Run All Tests
#'
#' Executes the full test suite and prints a summary and detailed results.
#'
#' @return Invisibly returns the test results data frame.
#' @export
run_all_tests <- function() {
  init_test_results()
  cat("Running Comprehensive Unit and Acceptance Tests\n")
  cat("========================================================\n")
  test_validate_inputs()
  test_split_data()
  test_perform_feature_selection()
  test_handle_class_imbalance()
  test_default_tune_grid()
  test_calculate_performance()
  test_parallel_processing()
  test_fs_svm()
  cat("========================================================\n")
  cat("UAT Completed\n\n")
  res <- get_test_results()
  cat("Test Summary:\n")
  print(table(res$Result))
  cat("\nDetailed Results:\n")
  print(res)
  invisible(res)
}

# To execute the suite:
# run_all_tests()
