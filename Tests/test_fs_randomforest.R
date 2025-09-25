###############################################################################
# Callable Test Runner for fs_randomforest()
# - No testthat project setup required.
# - Uses testthat expectations; failures will stop with a clear message.
# - Prints a concise summary with key metrics for quick eyeballing.
###############################################################################

run_fs_randomforest_tests <- function(auto_install = FALSE, verbose = TRUE) {
  # ---- Minimal dependency helper --------------------------------------------
  require_or_install <- function(pkgs) {
    for (p in pkgs) {
      if (!requireNamespace(p, quietly = TRUE)) {
        if (isTRUE(auto_install)) install.packages(p)
        if (!requireNamespace(p, quietly = TRUE)) {
          stop(sprintf("Package '%s' is required for tests.", p), call. = FALSE)
        }
      }
    }
  }
  
  require_or_install(c("testthat", "randomForest", "caret", "data.table"))
  # If you prefer attaching (not necessary since we namespace below):
  # library(testthat)
  
  # Short alias for printing
  say <- function(...) if (isTRUE(verbose)) cat(...)
  
  say("Running unit tests for fs_randomforest...\n\n")
  testthat::local_edition(3)
  set.seed(123)
  
  # Track simple timing and pass count
  t_start <- Sys.time()
  pass_ct <- 0L
  
  # -------------------- Test 1: Regression (numeric df) -----------------------
  testthat::test_that("Regression on simple numeric data.frame works", {
    n <- 500
    df <- data.frame(
      A = sample(1:100, n, replace = TRUE),
      B = sample(1:50,  n, replace = TRUE),
      target = rnorm(n)
    )
    res <- fs_randomforest(
      data = df, target = "target", type = "regression",
      control = list(ntree = 200, seed = 42, importance = TRUE, return_test_data = TRUE)
    )
    testthat::expect_s3_class(res, "fs_rf_result")
    testthat::expect_s3_class(res$model, "randomForest")
    testthat::expect_type(res$predictions, "double")
    testthat::expect_true(is.list(res$metrics) && all(c("RMSE", "MAE", "R2") %in% names(res$metrics)))
    testthat::expect_true(is.data.frame(res$importance) || is.null(res$importance))
    testthat::expect_true(is.data.frame(res$test_data))
  })
  pass_ct <- pass_ct + 1L
  say("✔ Test 1 (Regression): PASS\n")
  
  # ---------------- Test 2: Classification (numeric predictors) ---------------
  testthat::test_that("Classification on numeric predictors works", {
    n <- 600
    df <- data.frame(
      A = sample(1:10, n, replace = TRUE),
      B = sample(1:5,  n, replace = TRUE),
      target = factor(sample(c("neg", "pos"), n, replace = TRUE))
    )
    res <- fs_randomforest(
      data = df, target = "target", type = "classification",
      control = list(ntree = 250, seed = 7, importance = TRUE)
    )
    testthat::expect_s3_class(res, "fs_rf_result")
    testthat::expect_true(is.factor(res$predictions))
    testthat::expect_true(is.list(res$metrics) && "accuracy" %in% names(res$metrics))
    testthat::expect_true(inherits(res$confusion, "table"))
  })
  pass_ct <- pass_ct + 1L
  say("✔ Test 2 (Classification numeric): PASS\n")
  
  # -------------- Test 3: Classification with a categorical predictor ---------
  testthat::test_that("Classification with a categorical predictor works", {
    n <- 700
    df <- data.frame(
      A = sample(1:10, n, replace = TRUE),
      B = factor(sample(c("yes", "no", "maybe"), n, replace = TRUE)),
      target = factor(sample(c("a", "b"), n, replace = TRUE))
    )
    res <- fs_randomforest(
      df, "target", "classification",
      control = list(ntree = 200, seed = 99, importance = TRUE)
    )
    testthat::expect_true(is.factor(res$predictions))
    testthat::expect_true(is.list(res$metrics) && "accuracy" %in% names(res$metrics))
    if (!is.null(res$importance)) {
      testthat::expect_true(all(c("feature", "importance") %in% names(res$importance)))
    }
  })
  pass_ct <- pass_ct + 1L
  say("✔ Test 3 (Classification categorical): PASS\n")
  
  # --------------------- Test 4: Date columns are handled ---------------------
  testthat::test_that("Date columns are converted safely", {
    n <- 500
    df <- data.frame(
      A = sample(1:10, n, replace = TRUE),
      B = factor(sample(c("yes", "no"), n, replace = TRUE)),
      C = seq(as.Date("2001-01-01"), by = "day", length.out = n),
      target = factor(sample(c("x", "y"), n, replace = TRUE))
    )
    res <- fs_randomforest(df, "target", "classification",
                           control = list(ntree = 150, seed = 123))
    testthat::expect_true(is.factor(res$predictions))
    testthat::expect_true("accuracy" %in% names(res$metrics))
  })
  pass_ct <- pass_ct + 1L
  say("✔ Test 4 (Date handling): PASS\n")
  
  # ----------------- Test 5: Missing target should error cleanly --------------
  testthat::test_that("Missing target column errors clearly", {
    df <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = factor(sample(c("yes", "no"), 100, replace = TRUE))
    )
    testthat::expect_error(
      fs_randomforest(df, "target", "classification"),
      regexp = "Target column not found|Target column"
    )
  })
  pass_ct <- pass_ct + 1L
  say("✔ Test 5 (Missing target): PASS\n")
  
  # ----------------- Test 6: Invalid type should error cleanly ----------------
  testthat::test_that("Invalid `type` errors cleanly", {
    df <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(1:5, 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    testthat::expect_error(
      fs_randomforest(df, "target", type = "not_a_type"),
      regexp = "one of.*classification.*regression|must be one of|arg should be one of"
    )
  })
  pass_ct <- pass_ct + 1L
  say("✔ Test 6 (Invalid type): PASS\n")
  
  # ------------- Test 7: Custom preprocess + feature_select hooks -------------
  testthat::test_that("Custom preprocess & feature_select hooks work", {
    n <- 400
    df <- data.frame(
      A = sample(1:100, n, replace = TRUE),
      B = sample(1:50,  n, replace = TRUE),
      C = sample(letters[1:5], n, replace = TRUE),
      target = rnorm(n)
    )
    pre_fn <- function(d) { d$A <- d$A / max(d$A); d }
    fs_fn  <- function(d) { d$DROP_ME <- 1L; d$DROP_ME <- NULL; d }
    
    res <- fs_randomforest(df, "target", "regression",
                           control = list(preprocess = pre_fn, feature_select = fs_fn,
                                          ntree = 150, seed = 321))
    testthat::expect_s3_class(res$model, "randomForest")
    testthat::expect_true(all(c("RMSE", "MAE", "R2") %in% names(res$metrics)))
  })
  pass_ct <- pass_ct + 1L
  say("✔ Test 7 (Preprocess & Feature Select): PASS\n")
  
  # ------------- Test 8: Feature select removing target should error ----------
  testthat::test_that("Feature selection removing target errors cleanly", {
    n <- 200
    df <- data.frame(
      A = runif(n),
      B = runif(n),
      target = rnorm(n)
    )
    bad_fs <- function(d) { d$target <- NULL; d }
    testthat::expect_error(
      fs_randomforest(df, "target", "regression", control = list(feature_select = bad_fs)),
      regexp = "removed the target|removed.*target|Feature selection.*target"
    )
  })
  pass_ct <- pass_ct + 1L
  say("✔ Test 8 (Feature select removes target): PASS\n")
  
  # ------------------ Test 9: Parallel training smoke test --------------------
  testthat::test_that("Parallel training (if available) works", {
    cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 1L)
    n_cores <- if (is.finite(cores) && cores >= 2L) 2L else 1L
    
    n <- 600
    df <- data.frame(
      A = sample(1:100, n, replace = TRUE),
      B = sample(1:100, n, replace = TRUE),
      target = factor(sample(c("c0", "c1"), n, replace = TRUE))
    )
    res <- fs_randomforest(
      df, "target", "classification",
      control = list(ntree = 220, n_cores = n_cores, seed = 1001, importance = FALSE)
    )
    testthat::expect_s3_class(res$model, "randomForest")
    testthat::expect_true(is.factor(res$predictions))
    testthat::expect_true("accuracy" %in% names(res$metrics))
  })
  pass_ct <- pass_ct + 1L
  say("✔ Test 9 (Parallel): PASS\n")
  
  # --------------- Test 10: Imputation & zero-variance removal ----------------
  testthat::test_that("Imputation & zero-variance removal don’t break training", {
    n <- 300
    df <- data.frame(
      A = sample(1:10, n, replace = TRUE),
      B = factor(sample(c("k","l","m"), n, replace = TRUE)),
      ZEROVAR = 1L,
      target = rnorm(n)
    )
    set.seed(5)
    idx <- sample(seq_len(n), size = 25)
    df$A[idx] <- NA_real_
    
    res <- fs_randomforest(
      df, "target", "regression",
      control = list(impute = TRUE, drop_zerovar = TRUE, ntree = 150, seed = 55)
    )
    testthat::expect_s3_class(res$model, "randomForest")
    testthat::expect_type(res$predictions, "double")
    testthat::expect_true(all(c("RMSE", "MAE", "R2") %in% names(res$metrics)))
  })
  pass_ct <- pass_ct + 1L
  say("✔ Test 10 (Impute & NZV): PASS\n")
  
  # ------------------------------ Wrap up -------------------------------------
  dur <- round(as.numeric(difftime(Sys.time(), t_start, units = "secs")), 2)
  say("\nAll fs_randomforest tests completed successfully.\n")
  say(sprintf("Passed: %d  |  Duration: %.2f sec\n", pass_ct, dur))
  
  invisible(list(passed = pass_ct, seconds = dur))
}

# run_fs_randomforest_tests()
