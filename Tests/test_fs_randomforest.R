###############################################################################
# Testing Infrastructure - Random Forest (fs_randomforest)
###############################################################################

# Optional: if fs_randomforest is in a separate script/package, load it here, e.g.:
# source("fs_randomforest.R")
# library(yourpackage)

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

###############################################################################
# Tests: Existence and Input Validation
###############################################################################

test_fs_rf_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_randomforest") && is.function(fs_randomforest)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_randomforest: Function exists and is callable", passed, note)
}

test_fs_rf_input_validation_data_not_df <- function() {
  err <- NULL
  passed <- tryCatch({
    fs_randomforest("not a data frame", "target", type = "classification")
    FALSE
  }, error = function(e) {
    err <<- e
    TRUE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: errors if `data` is not a data.frame/data.table",
    passed, note
  )
}

test_fs_rf_input_validation_missing_target <- function() {
  df <- data.frame(A = sample(1:10, 100, replace = TRUE))
  err <- NULL
  passed <- tryCatch({
    fs_randomforest(df, "target", type = "classification")
    FALSE
  }, error = function(e) {
    err <<- e
    TRUE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: errors if target column is missing",
    passed, note
  )
}

test_fs_rf_input_validation_invalid_type <- function() {
  df <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    target = sample(1:2, 100, replace = TRUE)
  )
  err <- NULL
  passed <- tryCatch({
    fs_randomforest(df, "target", type = "not_a_type")
    FALSE
  }, error = function(e) {
    err <<- e
    TRUE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: errors cleanly for invalid `type`",
    passed, note
  )
}

test_fs_rf_input_validation_invalid_split_ratio <- function() {
  df <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    target = rnorm(100)
  )
  # split_ratio <= 0
  err1 <- NULL
  passed1 <- tryCatch({
    fs_randomforest(df, "target", type = "regression",
                    control = list(split_ratio = 0))
    FALSE
  }, error = function(e) {
    err1 <<- e
    TRUE
  })
  # split_ratio >= 1
  err2 <- NULL
  passed2 <- tryCatch({
    fs_randomforest(df, "target", type = "regression",
                    control = list(split_ratio = 1))
    FALSE
  }, error = function(e) {
    err2 <<- e
    TRUE
  })
  passed <- passed1 && passed2
  note <- paste(
    if (!is.null(err1)) paste0("<=0: ", conditionMessage(err1)) else "",
    if (!is.null(err2)) paste0(">=1: ", conditionMessage(err2)) else "",
    sep = " | "
  )
  print_and_store_result(
    "fs_randomforest: errors if `split_ratio` not in (0,1)",
    passed, note
  )
}

test_fs_rf_input_validation_invalid_seed <- function() {
  df <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    target = rnorm(100)
  )
  err <- NULL
  passed <- tryCatch({
    fs_randomforest(df, "target", type = "regression",
                    control = list(seed = "not_numeric"))
    FALSE
  }, error = function(e) {
    err <<- e
    TRUE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: errors if `control$seed` not coercible to integer",
    passed, note
  )
}

###############################################################################
# Tests: Core Functionality (Regression & Classification)
###############################################################################

test_fs_rf_regression_basic <- function() {
  set.seed(123)
  n <- 500
  df <- data.frame(
    A = sample(1:100, n, replace = TRUE),
    B = sample(1:50,  n, replace = TRUE),
    target = rnorm(n)
  )
  err <- NULL
  passed <- tryCatch({
    res <- fs_randomforest(
      data   = df,
      target = "target",
      type   = "regression",
      control = list(
        ntree            = 200,
        seed             = 42,
        importance       = TRUE,
        return_test_data = TRUE
      )
    )
    is.list(res) &&
      inherits(res, "fs_rf_result") &&
      inherits(res$model, "randomForest") &&
      is.numeric(res$predictions) &&
      is.list(res$metrics) &&
      all(c("RMSE", "MAE", "R2") %in% names(res$metrics)) &&
      (is.null(res$importance) || is.data.frame(res$importance)) &&
      is.data.frame(res$test_data) &&
      (length(res$train_index) + nrow(res$test_data) == nrow(df))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: regression on simple numeric data.frame works",
    passed, note
  )
}

test_fs_rf_classification_numeric <- function() {
  set.seed(456)
  n <- 600
  df <- data.frame(
    A = sample(1:10, n, replace = TRUE),
    B = sample(1:5,  n, replace = TRUE),
    target = factor(sample(c("neg", "pos"), n, replace = TRUE))
  )
  err <- NULL
  passed <- tryCatch({
    res <- fs_randomforest(
      data   = df,
      target = "target",
      type   = "classification",
      control = list(ntree = 250, seed = 7, importance = TRUE)
    )
    is.list(res) &&
      inherits(res, "fs_rf_result") &&
      is.factor(res$predictions) &&
      is.list(res$metrics) &&
      "accuracy" %in% names(res$metrics) &&
      inherits(res$confusion, "table")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: classification with numeric predictors works",
    passed, note
  )
}

test_fs_rf_classification_categorical <- function() {
  set.seed(789)
  n <- 700
  df <- data.frame(
    A = sample(1:10, n, replace = TRUE),
    B = factor(sample(c("yes", "no", "maybe"), n, replace = TRUE)),
    target = factor(sample(c("a", "b"), n, replace = TRUE))
  )
  err <- NULL
  passed <- tryCatch({
    res <- fs_randomforest(
      data   = df,
      target = "target",
      type   = "classification",
      control = list(ntree = 200, seed = 99, importance = TRUE)
    )
    base_ok <- is.factor(res$predictions) &&
      is.list(res$metrics) &&
      "accuracy" %in% names(res$metrics)
    imp_ok <- is.null(res$importance) ||
      (is.data.frame(res$importance) &&
         all(c("feature", "importance") %in% names(res$importance)))
    base_ok && imp_ok
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: classification with categorical predictor works",
    passed, note
  )
}

test_fs_rf_date_handling <- function() {
  set.seed(321)
  n <- 500
  df <- data.frame(
    A = sample(1:10, n, replace = TRUE),
    B = factor(sample(c("yes", "no"), n, replace = TRUE)),
    C = seq(as.Date("2001-01-01"), by = "day", length.out = n),
    target = factor(sample(c("x", "y"), n, replace = TRUE))
  )
  err <- NULL
  passed <- tryCatch({
    res <- fs_randomforest(
      data   = df,
      target = "target",
      type   = "classification",
      control = list(ntree = 150, seed = 123)
    )
    is.factor(res$predictions) &&
      "accuracy" %in% names(res$metrics)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: Date columns are converted and handled safely",
    passed, note
  )
}

###############################################################################
# Tests: Preprocess / Feature Selection Hooks
###############################################################################

test_fs_rf_preprocess_feature_select <- function() {
  set.seed(111)
  n <- 400
  df <- data.frame(
    A = sample(1:100, n, replace = TRUE),
    B = sample(1:50,  n, replace = TRUE),
    C = sample(letters[1:5], n, replace = TRUE),
    target = rnorm(n)
  )
  pre_fn <- function(d) {
    d$A <- d$A / max(d$A)
    d
  }
  fs_fn  <- function(d) {
    d$DROP_ME <- 1L
    d$DROP_ME <- NULL
    d
  }
  err <- NULL
  passed <- tryCatch({
    res <- fs_randomforest(
      data   = df,
      target = "target",
      type   = "regression",
      control = list(
        preprocess = pre_fn,
        feature_select = fs_fn,
        ntree = 150,
        seed  = 321
      )
    )
    inherits(res$model, "randomForest") &&
      is.list(res$metrics) &&
      all(c("RMSE", "MAE", "R2") %in% names(res$metrics))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: custom `preprocess` and `feature_select` hooks work",
    passed, note
  )
}

test_fs_rf_feature_select_removes_target <- function() {
  set.seed(222)
  n <- 200
  df <- data.frame(
    A = runif(n),
    B = runif(n),
    target = rnorm(n)
  )
  bad_fs <- function(d) {
    d$target <- NULL
    d
  }
  err <- NULL
  passed <- tryCatch({
    fs_randomforest(df, "target", type = "regression",
                    control = list(feature_select = bad_fs))
    FALSE
  }, error = function(e) {
    err <<- e
    TRUE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: feature_select removing target errors cleanly",
    passed, note
  )
}

###############################################################################
# Tests: Imputation, Zero-Variance, and Target NA Handling
###############################################################################

test_fs_rf_impute_and_nzv <- function() {
  set.seed(333)
  n <- 300
  df <- data.frame(
    A = sample(1:10, n, replace = TRUE),
    B = factor(sample(c("k","l","m"), n, replace = TRUE)),
    ZEROVAR = 1L,
    target = rnorm(n)
  )
  idx <- sample(seq_len(n), size = 25)
  df$A[idx] <- NA_real_
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_randomforest(
      data   = df,
      target = "target",
      type   = "regression",
      control = list(
        impute           = TRUE,
        drop_zerovar     = TRUE,
        ntree            = 150,
        seed             = 55,
        return_test_data = TRUE
      )
    )
    is.list(res) &&
      inherits(res$model, "randomForest") &&
      is.numeric(res$predictions) &&
      all(c("RMSE", "MAE", "R2") %in% names(res$metrics)) &&
      !("ZEROVAR" %in% names(res$test_data)) &&
      !any(is.na(res$test_data$A))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: imputation & zero-variance removal behave correctly",
    passed, note
  )
}

test_fs_rf_target_na_classification <- function() {
  set.seed(444)
  n <- 300
  target <- factor(sample(c("yes", "no"), n, replace = TRUE))
  na_idx <- sample(seq_len(n), size = 40)
  target_with_na <- target
  target_with_na[na_idx] <- NA
  
  df <- data.frame(
    A = rnorm(n),
    B = rnorm(n),
    target = target_with_na
  )
  
  non_na <- sum(!is.na(target_with_na))
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_randomforest(
      data   = df,
      target = "target",
      type   = "classification",
      control = list(
        ntree            = 150,
        seed             = 101,
        return_test_data = TRUE
      )
    )
    total_used <- length(res$train_index) + nrow(res$test_data)
    is.factor(res$predictions) &&
      "accuracy" %in% names(res$metrics) &&
      total_used == non_na
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: classification drops NA target rows and trains cleanly",
    passed, note
  )
}

test_fs_rf_target_na_regression <- function() {
  set.seed(555)
  n <- 300
  target <- rnorm(n)
  na_idx <- sample(seq_len(n), size = 30)
  target_with_na <- target
  target_with_na[na_idx] <- NA
  
  df <- data.frame(
    A = rnorm(n),
    B = rnorm(n),
    target = target_with_na
  )
  
  non_na <- sum(!is.na(target_with_na))
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_randomforest(
      data   = df,
      target = "target",
      type   = "regression",
      control = list(
        ntree            = 120,
        seed             = 202,
        return_test_data = TRUE
      )
    )
    total_used <- length(res$train_index) + nrow(res$test_data)
    is.numeric(res$predictions) &&
      all(c("RMSE", "MAE", "R2") %in% names(res$metrics)) &&
      total_used == non_na
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: regression drops NA target rows and trains cleanly",
    passed, note
  )
}

###############################################################################
# Tests: OOB Metrics and AUC / positive_class
###############################################################################

test_fs_rf_oob_toggle <- function() {
  set.seed(666)
  n <- 400
  df <- data.frame(
    A = rnorm(n),
    B = rnorm(n),
    target = factor(sample(c("c0", "c1"), n, replace = TRUE))
  )
  err <- NULL
  passed <- tryCatch({
    res_oob    <- fs_randomforest(
      df, "target", "classification",
      control = list(ntree = 150, seed = 1, oob = TRUE)
    )
    res_no_oob <- fs_randomforest(
      df, "target", "classification",
      control = list(ntree = 150, seed = 1, oob = FALSE)
    )
    (!is.null(res_oob$oob) && "accuracy" %in% names(res_oob$oob)) &&
      is.null(res_no_oob$oob)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: `control$oob` toggles presence of OOB metrics",
    passed, note
  )
}

test_fs_rf_positive_class_auc <- function() {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    print_and_store_result(
      "fs_randomforest: `control$positive_class` (AUC) test skipped (pROC missing)",
      TRUE,
      note = "Test skipped because pROC is not installed."
    )
    return(invisible(NULL))
  }
  
  set.seed(777)
  n <- 300
  x <- rnorm(n)
  target_raw <- ifelse(x > 0, "pos", "neg")
  # Two levels; "neg" first, "pos" second
  target <- factor(target_raw, levels = c("neg", "pos"))
  df <- data.frame(x = x, target = target)
  
  err <- NULL
  passed <- tryCatch({
    res_default <- fs_randomforest(
      df, "target", "classification",
      control = list(ntree = 200, seed = 99)
    )
    res_override <- fs_randomforest(
      df, "target", "classification",
      control = list(ntree = 200, seed = 99, positive_class = "pos")
    )
    auc_default  <- res_default$metrics$auc
    auc_override <- res_override$metrics$auc
    is.numeric(auc_default)  && is.finite(auc_default) &&
      is.numeric(auc_override) && is.finite(auc_override) &&
      identical(res_override$control$positive_class, "pos")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: `control$positive_class` accepted and AUC computed",
    passed, note
  )
}

###############################################################################
# Tests: Sample Size Downsampling and Parallel Training
###############################################################################

test_fs_rf_sample_size_downsampling <- function() {
  set.seed(888)
  n <- 1000
  df <- data.frame(
    x1 = rnorm(n),
    target = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  )
  sample_size <- 300L
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_randomforest(
      data   = df,
      target = "target",
      type   = "classification",
      control = list(
        sample_size      = sample_size,
        ntree            = 120,
        seed             = 10,
        return_test_data = TRUE
      )
    )
    total_used <- length(res$train_index) + nrow(res$test_data)
    total_used <= sample_size &&
      is.factor(res$predictions) &&
      "accuracy" %in% names(res$metrics)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: classification downsampling respects `sample_size` and trains",
    passed, note
  )
}

test_fs_rf_parallel_smoke <- function() {
  set.seed(999)
  cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 1L)
  n <- 600
  df <- data.frame(
    A = sample(1:100, n, replace = TRUE),
    B = sample(1:100, n, replace = TRUE),
    target = factor(sample(c("c0", "c1"), n, replace = TRUE))
  )
  
  err <- NULL
  passed <- tryCatch({
    if (is.finite(cores) && cores >= 2L) {
      requested_cores <- min(cores, 4L)
      ntree <- max(2L, requested_cores - 1L)  # ensure ntree can be < requested cores
      res <- fs_randomforest(
        data   = df,
        target = "target",
        type   = "classification",
        control = list(
          ntree      = ntree,
          n_cores    = requested_cores,
          seed       = 1001,
          importance = FALSE
        )
      )
      inherits(res$model, "randomForest") &&
        is.factor(res$predictions) &&
        "accuracy" %in% names(res$metrics) &&
        is.null(res$importance)
    } else {
      # Fallback: sequential path
      res <- fs_randomforest(
        data   = df,
        target = "target",
        type   = "classification",
        control = list(
          ntree      = 50,
          n_cores    = 1,
          seed       = 1001,
          importance = FALSE
        )
      )
      inherits(res$model, "randomForest") &&
        is.factor(res$predictions) &&
        "accuracy" %in% names(res$metrics) &&
        is.null(res$importance)
    }
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_randomforest: parallel training smoke test (handles n_cores vs ntree)",
    passed, note
  )
}

###############################################################################
# Run All fs_randomforest Tests
###############################################################################

run_fs_randomforest_tests <- function() {
  cat("========== Running fs_randomforest Tests ==========\n")
  
  test_fs_rf_existence()
  test_fs_rf_input_validation_data_not_df()
  test_fs_rf_input_validation_missing_target()
  test_fs_rf_input_validation_invalid_type()
  test_fs_rf_input_validation_invalid_split_ratio()
  test_fs_rf_input_validation_invalid_seed()
  
  test_fs_rf_regression_basic()
  test_fs_rf_classification_numeric()
  test_fs_rf_classification_categorical()
  test_fs_rf_date_handling()
  
  test_fs_rf_preprocess_feature_select()
  test_fs_rf_feature_select_removes_target()
  
  test_fs_rf_impute_and_nzv()
  test_fs_rf_target_na_classification()
  test_fs_rf_target_na_regression()
  
  test_fs_rf_oob_toggle()
  test_fs_rf_positive_class_auc()
  
  test_fs_rf_sample_size_downsampling()
  test_fs_rf_parallel_smoke()
  
  cat("========== fs_randomforest Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment to run:
# run_fs_randomforest_tests()
