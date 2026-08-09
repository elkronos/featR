###############################################################################
# Testing Infrastructure - MARS (fs_mars)
###############################################################################
#
# Optional: if fs_mars and utilities are in a separate script/package, load here:
# source("fs_mars.R")
# library(yourpackage)
#
###############################################################################

# Global container for test results
test_results <- data.frame(
  Test   = character(),
  Result = character(),
  stringsAsFactors = FALSE
)

#' Helper: Print and Store Test Result
#'
#' @param test_name Character. Name of the test.
#' @param passed Logical. Whether the test passed.
#' @param note Optional character. Additional notes.
print_and_store_result <- function(test_name, passed, note = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-80s [%s]\n", test_name, result))
  if (!is.null(note) && nzchar(note)) cat("  Note: ", note, "\n", sep = "")
  test_results <<- rbind(
    test_results,
    data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)
  )
}

SEED <- 123

###############################################################################
# Helpers for MARS tests
###############################################################################

# Tiny tuning grid
.mars_quick_grid <- function() {
  define_hyperparameter_grid(degree = 1:2, nprune = c(3, 5))
}

# Small CV control
.mars_quick_ctrl <- function(train, responseName, number = 3, repeats = 1) {
  define_train_control(
    number       = number,
    repeats      = repeats,
    search       = "grid",
    train        = train,
    responseName = responseName,
    seed         = SEED
  )
}

###############################################################################
# Tests: Existence and Core Utilities
###############################################################################

test_fs_mars_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_mars") && is.function(fs_mars)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_mars: function exists and is callable", passed, note)
}

test_fs_mars_core_utilities_existence <- function() {
  fns <- c(
    "check_libraries",
    "check_response_column",
    "coerce_response",
    "handle_missing_values",
    "check_class_balance",
    "sample_data",
    "split_data",
    "balance_classes",
    "define_hyperparameter_grid",
    "preprocess_predictors",
    "define_train_control",
    "train_model",
    "evaluate_model",
    "maybe_register_parallel",
    "stop_parallel"
  )
  for (fn in fns) {
    err <- NULL
    passed <- tryCatch({
      exists(fn) && is.function(get(fn))
    }, error = function(e) {
      err <<- e
      FALSE
    })
    note <- if (!is.null(err)) conditionMessage(err) else NULL
    print_and_store_result(
      sprintf("Core utility: '%s' exists and is callable", fn),
      passed, note
    )
  }
}

###############################################################################
# Tests: Input Validation and Basic Utilities
###############################################################################

test_fs_mars_check_response_and_coerce <- function() {
  # check_response_column: missing vs existing
  df <- data.frame(y = rnorm(20), x = rnorm(20))
  
  err1 <- NULL
  passed1 <- tryCatch({
    check_response_column(df, "missing")
    FALSE
  }, error = function(e) {
    err1 <<- e
    grepl("does not exist", conditionMessage(e))
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "check_response_column: errors if response column missing",
    passed1, note1
  )
  
  err2 <- NULL
  passed2 <- tryCatch({
    check_response_column(df, "y")
    TRUE
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "check_response_column: passes when response column exists",
    passed2, note2
  )
  
  # coerce_response: character -> factor
  df_char <- data.frame(y = sample(c("A", "B"), 20, TRUE), x = rnorm(20))
  err3 <- NULL
  passed3 <- tryCatch({
    d2 <- coerce_response(df_char, "y")
    is.factor(d2$y)
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "coerce_response: converts character response to factor",
    passed3, note3
  )
  
  # coerce_response: sanitize factor levels with make.names()
  df_fac <- data.frame(
    y = factor(sample(c("class 1", "class-2"), 20, TRUE)),
    x = rnorm(20)
  )
  err4 <- NULL
  passed4 <- tryCatch({
    d3 <- coerce_response(df_fac, "y", make_factor_names = TRUE)
    lev <- levels(d3$y)
    all(!grepl(" ", lev)) && all(!grepl("-", lev))
  }, error = function(e) {
    err4 <<- e
    FALSE
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "coerce_response: applies make.names() to factor levels when requested",
    passed4, note4
  )
  
  # coerce_response: unsupported type errors
  df_bad <- data.frame(y = I(list(1, 2, 3)), x = rnorm(3))
  err5 <- NULL
  passed5 <- tryCatch({
    coerce_response(df_bad, "y")
    FALSE
  }, error = function(e) {
    err5 <<- e
    grepl("Response must be numeric", conditionMessage(e))
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    "coerce_response: errors for unsupported response types",
    passed5, note5
  )
}

test_fs_mars_handle_missing_values <- function() {
  df <- data.frame(
    y = c(rnorm(18), NA, NA),
    x = c(rnorm(17), NA, NA, NA)
  )
  err <- NULL
  passed <- tryCatch({
    clean <- handle_missing_values(df)
    sum(is.na(clean)) == 0L && nrow(clean) <= nrow(df)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "handle_missing_values: removes all rows containing NAs",
    passed, note
  )
}

###############################################################################
# Tests: Class Balance and Balancing
###############################################################################

test_fs_mars_class_balance_and_balancing <- function() {
  # Sufficient counts
  df_ok <- data.frame(
    y = factor(rep(c("A", "B"), c(10, 15))),
    x = rnorm(25)
  )
  err1 <- NULL
  passed1 <- tryCatch({
    check_class_balance(df_ok, "y")
    TRUE
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "check_class_balance: passes when each class has >= 2 samples",
    passed1, note1
  )
  
  # Too few in one class
  df_bad <- data.frame(
    y = factor(c(rep("A", 10), "B")),
    x = rnorm(11)
  )
  err2 <- NULL
  passed2 <- tryCatch({
    check_class_balance(df_bad, "y")
    FALSE
  }, error = function(e) {
    err2 <<- e
    grepl("Each class must have at least two samples", conditionMessage(e))
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "check_class_balance: errors when a class has fewer than 2 samples",
    passed2, note2
  )
  
  # balance_classes upsampling
  df_imb <- data.frame(
    y = factor(rep(c("A", "B"), c(35, 15))),
    x = rnorm(50)
  )
  err3 <- NULL
  passed3 <- tryCatch({
    bal <- balance_classes(df_imb, "y")
    cnt <- table(bal$y)
    min(cnt) == max(cnt)
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "balance_classes: oversamples minority classes up to max count",
    passed3, note3
  )
}

###############################################################################
# Tests: Sampling and Splitting
###############################################################################

test_fs_mars_sample_data <- function() {
  set.seed(SEED)
  df <- data.frame(y = rnorm(2000), x = rnorm(2000))
  
  # Down-sampling deterministic
  err1 <- NULL
  passed1 <- tryCatch({
    d1 <- sample_data(df, 1000, seed = SEED)
    d2 <- sample_data(df, 1000, seed = SEED)
    nrow(d1) == 1000L &&
      nrow(d2) == 1000L &&
      identical(d1[order(d1$y, d1$x), ], d2[order(d2$y, d2$x), ])
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "sample_data: reduces dataset size deterministically when nrow > sampleSize",
    passed1, note1
  )
  
  # No down-sampling when nrow <= sampleSize
  df_small <- data.frame(y = rnorm(100), x = rnorm(100))
  err2 <- NULL
  passed2 <- tryCatch({
    d3 <- sample_data(df_small, 1000, seed = SEED)
    nrow(d3) == 100L
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "sample_data: returns full data when nrow <= sampleSize",
    passed2, note2
  )
}

test_fs_mars_split_data <- function() {
  # Regression split sizes
  df_reg <- data.frame(y = rnorm(100), x = rnorm(100))
  err1 <- NULL
  passed1 <- tryCatch({
    sp <- split_data(df_reg, "y", 0.8, seed = SEED)
    nrow(sp$train) == 80L && nrow(sp$test) == 20L
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "split_data: creates requested train/test sizes for regression",
    passed1, note1
  )
  
  # Classification: ensure all classes appear in train & test
  y <- factor(rep(c("A", "B"), each = 50))
  df_cls <- data.frame(y = y, x = rnorm(100))
  err2 <- NULL
  passed2 <- tryCatch({
    sp <- split_data(df_cls, "y", 0.7, seed = SEED)
    all(table(sp$train$y) > 0L) && all(table(sp$test$y) > 0L)
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "split_data: preserves all classes in both train and test (classification)",
    passed2, note2
  )
}

###############################################################################
# Tests: Hyperparameter Grid and Preprocessing
###############################################################################

test_fs_mars_define_hyperparameter_grid <- function() {
  err <- NULL
  passed <- tryCatch({
    grid <- define_hyperparameter_grid(1:2, c(5, 5, 3))
    nrow(grid) == 4L &&
      all(sort(unique(grid$degree)) == c(1, 2)) &&
      all(sort(unique(grid$nprune)) == c(3, 5))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "define_hyperparameter_grid: returns sorted, unique grid combinations",
    passed, note
  )
}

test_fs_mars_preprocess_predictors <- function() {
  set.seed(SEED)
  # NZV + correlated numeric predictors
  y <- rnorm(100)
  x1 <- rnorm(100)
  x2 <- x1 + rnorm(100, sd = 0.01)  # highly correlated
  nzv <- rep(1, 100)                # zero variance
  df <- data.frame(y = y, x1 = x1, x2 = x2, nzv = nzv)
  
  err1 <- NULL
  passed1 <- tryCatch({
    sp <- split_data(df, "y", 0.8, seed = SEED)
    pp <- preprocess_predictors(sp$train, sp$test, "y",
                                corr_cut = 0.90,
                                remove_nzv = TRUE)
    removed <- unlist(pp$removed, use.names = FALSE)
    "nzv" %in% removed &&
      any(c("x1", "x2") %in% removed)
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "preprocess_predictors: removes NZV and at least one highly correlated predictor",
    passed1, note1
  )
  
  # Case with only response column should not error
  df_resp_only <- data.frame(y = rnorm(50))
  err2 <- NULL
  passed2 <- tryCatch({
    sp2 <- split_data(df_resp_only, "y", 0.8, seed = SEED)
    pp2 <- preprocess_predictors(sp2$train, sp2$test, "y",
                                 corr_cut = 0.9, remove_nzv = TRUE)
    is.list(pp2) && all(c("train", "test", "removed") %in% names(pp2))
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "preprocess_predictors: handles case with only response column gracefully",
    passed2, note2
  )
}

###############################################################################
# Tests: Train Control and Parallel Helpers
###############################################################################

test_fs_mars_define_train_control <- function() {
  # Classification control
  df <- data.frame(y = factor(rep(c("A", "B"), each = 30)), x = rnorm(60))
  err1 <- NULL
  passed1 <- tryCatch({
    ctrl <- define_train_control(
      number = 3, repeats = 1,
      search = "grid",
      train = df, responseName = "y",
      seed = SEED, tune_grid_n = 4
    )
    inherits(ctrl, "trainControl") &&
      ctrl$number == 3 &&
      ctrl$repeats == 1 &&
      length(ctrl$seeds) == 3 * 1 + 1 &&
      all(lengths(ctrl$seeds[1:3]) == 4) &&
      length(ctrl$seeds[[4]]) == 1L
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "define_train_control: returns valid trainControl with correctly sized seeds",
    passed1, note1
  )
}

test_fs_mars_parallel_helpers <- function() {
  # maybe_register_parallel and stop_parallel should never error
  err <- NULL
  passed <- tryCatch({
    cl <- maybe_register_parallel(verbose = FALSE)
    # cl is either NULL (no doParallel) or a cluster
    stop_parallel(cl)
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "maybe_register_parallel/stop_parallel: run without errors",
    passed, note
  )
}

###############################################################################
# Tests: Train and Evaluate (Regression and Classification)
###############################################################################

test_fs_mars_train_and_evaluate_regression <- function() {
  set.seed(SEED)
  df <- data.frame(
    y  = rnorm(120),
    x1 = rnorm(120),
    x2 = rnorm(120)
  )
  
  err <- NULL
  passed <- tryCatch({
    sp   <- split_data(df, "y", 0.8, seed = SEED)
    grid <- .mars_quick_grid()
    ctrl <- .mars_quick_ctrl(sp$train, "y")
    model <- train_model(sp$train, "y", "earth", ctrl, grid, seed = SEED)
    ev <- evaluate_model(model, sp$test, "y")
    
    inherits(model, "train") &&
      all(c("RMSE", "MAE", "R2") %in% names(ev$metrics)) &&
      is.numeric(ev$metrics$RMSE)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "train_model/evaluate_model: work for regression (earth)",
    passed, note
  )
}

test_fs_mars_train_and_evaluate_binary <- function() {
  set.seed(SEED)
  
  # Make a separable-ish dataset
  x1 <- rnorm(150)
  x2 <- rnorm(150)
  lin <- 1.5 * x1 - 1.0 * x2 + rnorm(150, sd = 0.5)
  
  # FIRST level is treated as positive class; align with evaluate_model logic
  y_raw <- ifelse(lin > 0, "pos", "neg")
  y <- factor(y_raw, levels = c("pos", "neg"))
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  
  err <- NULL
  passed <- tryCatch({
    sp <- split_data(df, "y", 0.8, seed = SEED)
    sp$train <- balance_classes(sp$train, "y")
    grid <- .mars_quick_grid()
    ctrl <- .mars_quick_ctrl(sp$train, "y")
    model <- train_model(sp$train, "y", "earth", ctrl, grid, seed = SEED)
    ev <- evaluate_model(model, sp$test, "y")
    
    base_ok <- all(c("Accuracy", "Kappa") %in% names(ev$metrics)) &&
      (is.matrix(ev$confusion_matrix) || is.table(ev$confusion_matrix))
    
    # If twoClassSummary and pROC/PRROC are available, ROC_AUC / PR_AUC should be present
    extra_ok <- TRUE
    if (identical(model$control$summaryFunction, caret::twoClassSummary)) {
      if (requireNamespace("pROC", quietly = TRUE)) {
        extra_ok <- extra_ok && "ROC_AUC" %in% names(ev$metrics) &&
          is.numeric(ev$metrics$ROC_AUC)
      }
      if (requireNamespace("PRROC", quietly = TRUE)) {
        extra_ok <- extra_ok && "PR_AUC" %in% names(ev$metrics) &&
          is.numeric(ev$metrics$PR_AUC)
      }
    }
    base_ok && extra_ok
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "train_model/evaluate_model: work for binary classification (earth)",
    passed, note
  )
}

###############################################################################
# Tests: fs_mars End-to-End
###############################################################################

test_fs_mars_end_to_end_regression <- function() {
  set.seed(SEED)
  df <- data.frame(
    y  = rnorm(140),
    x1 = rnorm(140),
    x2 = rnorm(140)
  )
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_mars(
      data         = df,
      responseName = "y",
      degree       = 1:2,
      nprune       = c(3, 5),
      number       = 3,
      repeats      = 1,
      seed         = SEED,
      sampleSize   = 2000,
      verbose      = FALSE,
      corr_cut     = 0.9
    )
    !is.null(res$model) &&
      all(c("RMSE", "MAE", "R2") %in% names(res$metrics)) &&
      "removed_predictors" %in% names(res$preprocessing)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_mars: runs end-to-end for regression and returns expected fields",
    passed, note
  )
}

test_fs_mars_end_to_end_binary_with_sanitized_levels <- function() {
  set.seed(SEED)
  x1 <- rnorm(160)
  x2 <- rnorm(160)
  lin <- 1.2 * x1 - 0.8 * x2 + rnorm(160, sd = 0.6)
  
  # Use non-syntactic labels; coerce_response should sanitize
  y_raw <- ifelse(lin > 0, "class 1", "class-2")
  y <- factor(y_raw, levels = c("class 1", "class-2"))
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_mars(
      data         = df,
      responseName = "y",
      degree       = 1:2,
      nprune       = c(3, 5),
      number       = 3,
      repeats      = 1,
      seed         = SEED,
      sampleSize   = 2000,
      verbose      = FALSE,
      corr_cut     = 0.9
    )
    base_ok <- !is.null(res$model) &&
      all(c("Accuracy", "Kappa") %in% names(res$metrics))
    
    # Check that levels in trainingData have been sanitized
    out_levels <- levels(res$model$trainingData$.outcome)
    base_ok &&
      all(!grepl(" ", out_levels)) &&
      all(!grepl("-", out_levels))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_mars: end-to-end binary classification with factor-level sanitization",
    passed, note
  )
}

test_fs_mars_class_balance_error_after_sampling <- function() {
  # fs_mars now checks class balance AFTER sampling and BEFORE splitting
  set.seed(SEED)
  y <- factor(c(rep("A", 10), "B"))  # B appears once
  df <- data.frame(y = y, x1 = rnorm(11), x2 = rnorm(11))
  
  err <- NULL
  passed <- tryCatch({
    fs_mars(
      data         = df,
      responseName = "y",
      degree       = 1:2,
      nprune       = c(3, 5),
      number       = 3,
      repeats      = 1,
      seed         = SEED,
      sampleSize   = 1000,
      verbose      = FALSE,
      corr_cut     = 0.9
    )
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Each class must have at least two samples", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_mars: errors when a class has <2 samples after sampling",
    passed, note
  )
}

###############################################################################
# Run All fs_mars Tests
###############################################################################

run_fs_mars_tests <- function() {
  cat("========== Running fs_mars Tests ==========\n")
  
  test_fs_mars_existence()
  test_fs_mars_core_utilities_existence()
  test_fs_mars_check_response_and_coerce()
  test_fs_mars_handle_missing_values()
  test_fs_mars_class_balance_and_balancing()
  test_fs_mars_sample_data()
  test_fs_mars_split_data()
  test_fs_mars_define_hyperparameter_grid()
  test_fs_mars_preprocess_predictors()
  test_fs_mars_define_train_control()
  test_fs_mars_parallel_helpers()
  test_fs_mars_train_and_evaluate_regression()
  test_fs_mars_train_and_evaluate_binary()
  test_fs_mars_end_to_end_regression()
  test_fs_mars_end_to_end_binary_with_sanitized_levels()
  test_fs_mars_class_balance_error_after_sampling()
  
  cat("========== fs_mars Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment the following line to run all tests when this script is executed:
# run_fs_mars_tests()
