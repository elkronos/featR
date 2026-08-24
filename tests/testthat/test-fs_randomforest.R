# Tests for fs_randomforest().
#
# Order matters here and differs from fs_boruta()/fs_lasso(): fs_randomforest()
# calls fs_require(c("randomForest", "caret")) as the *first* statement of its
# body, before match.arg() and before every assert_*() call. Argument
# validation therefore does NOT precede fs_require(), so even the validation
# tests below are gated on those two Suggests. Only formals() inspection runs
# unconditionally.
#
# Everything that actually grows a forest is additionally gated with
# skip_on_cran() and kept deliberately small (n <= 100, ntree <= 25,
# n_cores = 1) because this is one of the slowest suites in the package.

test_that("fs_randomforest signature and defaults are stable", {
  fx <- formals(fs_randomforest)
  expect_identical(names(fx), c("data", "target", "type", "control"))
  expect_identical(eval(fx$type), c("classification", "regression"))
  expect_identical(eval(fx$control), list())
})

test_that("fs_randomforest rejects bad data/target/type/control", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")

  d <- data.frame(y = c(1, 2, 3, 4), x = c(4, 3, 2, 1))

  expect_error(fs_randomforest("nope", "y", "regression"),
               "'data' must be a data\\.frame")
  expect_error(fs_randomforest(d[0, , drop = FALSE], "y", "regression"),
               "'data' must have at least one row and one column")
  expect_error(fs_randomforest(d, "missing_target", "regression"),
               "Column 'missing_target' not found in 'data'")
  expect_error(fs_randomforest(d, 1, "regression"),
               "'target' must be a single non-empty character string")
  expect_error(fs_randomforest(d, "y", type = "not_a_type"),
               "should be one of")
  expect_error(fs_randomforest(d, "y", "regression", control = "nope"),
               "'control' must be a list")
})

test_that("fs_randomforest validates control values with clear messages", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")

  d <- data.frame(y = c(1, 2, 3, 4), x = c(4, 3, 2, 1))
  bad <- function(ctrl) {
    fs_randomforest(d, "y", "regression", control = ctrl)
  }

  # garbage ntree of every flavour
  expect_error(bad(list(ntree = "many")),
               "'control\\$ntree' must be a single finite number")
  expect_error(bad(list(ntree = 2.5)),
               "'control\\$ntree' must be a whole number")
  expect_error(bad(list(ntree = 0)),
               "'control\\$ntree' must be between 1 and Inf")

  expect_error(bad(list(split_ratio = "half")),
               "'control\\$split_ratio' must be a single finite number")
  expect_error(bad(list(split_ratio = 1)),
               "'control\\$split_ratio' must be strictly between 0 and 1")
  expect_error(bad(list(split_ratio = 0)),
               "'control\\$split_ratio' must be strictly between 0 and 1")

  expect_error(bad(list(maxnodes = 1)),
               "'control\\$maxnodes' must be between 2 and Inf")
  expect_error(bad(list(mtry = 0)),
               "'control\\$mtry' must be between 1 and Inf")
  expect_error(bad(list(sample_size = 1.5)),
               "'control\\$sample_size' must be a whole number")

  expect_error(bad(list(oob = "yes")), "'control\\$oob' must be TRUE or FALSE")
  expect_error(bad(list(impute = NA)), "'control\\$impute' must be TRUE or FALSE")
  expect_error(bad(list(return_test_data = 1)),
               "'control\\$return_test_data' must be TRUE or FALSE")
  expect_error(bad(list(positive_class = 1)),
               "'control\\$positive_class' must be a single non-empty character string")
  expect_error(bad(list(seed = "later")),
               "'control\\$seed' must be a single finite number")
})

test_that("regression smoke: documented structure, metrics, and predictions", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")
  skip_on_cran()

  set.seed(1234)
  n <- 90
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- 3 * d$x1 - 2 * d$x2 + rnorm(n, sd = 0.4)

  res <- fs_randomforest(
    d, "y", "regression",
    control = list(ntree = 25, seed = 13, n_cores = 1,
                   scale_importance = FALSE, return_test_data = TRUE)
  )

  expect_s3_class(res, "fs_rf_result")
  expect_named(res, c("model", "metrics", "predictions", "probabilities",
                      "importance", "confusion", "oob", "target", "type",
                      "feature_names", "train_index", "control", "test_data"))

  expect_s3_class(res$model, "randomForest")
  expect_identical(res$target, "y")
  expect_identical(res$type, "regression")
  expect_identical(res$feature_names, c("x1", "x2", "x3"))

  # regression metrics
  expect_named(res$metrics, c("RMSE", "MAE", "R2"))
  expect_true(is.finite(res$metrics$RMSE))
  expect_true(is.finite(res$metrics$MAE))
  expect_true(is.finite(res$metrics$R2))

  # predictions line up with the held-out rows
  expect_true(is.numeric(res$predictions))
  expect_length(res$predictions, nrow(res$test_data))
  expect_type(res$train_index, "integer")
  expect_identical(length(res$train_index) + nrow(res$test_data), 90L)

  # classification-only slots stay NULL but are still named
  expect_null(res$probabilities)
  expect_null(res$confusion)

  # OOB metrics are available for a sequentially trained forest
  expect_named(res$oob, "RMSE")
  expect_true(is.finite(res$oob$RMSE))

  # importance table: documented columns, sorted descending
  expect_s3_class(res$importance, "data.frame")
  expect_identical(names(res$importance), c("feature", "importance"))
  expect_setequal(res$importance$feature, res$feature_names)
  imp_vals <- res$importance$importance
  expect_false(anyNA(imp_vals))
  expect_identical(imp_vals, sort(imp_vals, decreasing = TRUE))

  # the merged control list keeps the documented default schema
  expect_named(res$control,
               c("seed", "split_ratio", "sample_size", "ntree", "importance",
                 "scale_importance", "mtry", "nodesize", "maxnodes", "sampsize",
                 "classwt", "strata", "replace", "n_cores", "preprocess",
                 "feature_select", "impute", "drop_zerovar", "oob",
                 "return_test_data", "positive_class"))
  expect_identical(res$control$split_ratio, 0.75)
  expect_true(res$control$importance)
  expect_true(res$control$impute)
  expect_true(res$control$drop_zerovar)
  expect_true(res$control$oob)
  expect_true(res$control$replace)
  expect_null(res$control$mtry)
  expect_null(res$control$sample_size)
  expect_null(res$control$positive_class)
})

test_that("classification smoke: documented structure, metrics, and predictions", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")
  skip_if_not_installed("e1071") # caret::confusionMatrix() Kappa
  skip_on_cran()

  set.seed(2024)
  n <- 90
  grp <- factor(rep(c("a", "b", "c"), each = n / 3))
  d <- data.frame(
    y  = grp,
    x1 = as.numeric(grp) + rnorm(n, sd = 0.4),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )

  res <- fs_randomforest(
    d, "y", "classification",
    control = list(ntree = 25, seed = 17, n_cores = 1, return_test_data = TRUE)
  )

  expect_s3_class(res, "fs_rf_result")
  expect_named(res, c("model", "metrics", "predictions", "probabilities",
                      "importance", "confusion", "oob", "target", "type",
                      "feature_names", "train_index", "control", "test_data"))
  expect_identical(res$target, "y")
  expect_identical(res$type, "classification")
  expect_identical(res$feature_names, c("x1", "x2", "x3"))

  expect_named(res$metrics, c("accuracy", "kappa", "auc"))
  expect_gte(res$metrics$accuracy, 0)
  expect_lte(res$metrics$accuracy, 1)
  expect_true(is.numeric(res$metrics$kappa))
  expect_length(res$metrics$kappa, 1L)
  # three classes: the binary AUC branch is never entered, so AUC stays NA and
  # pROC is not needed
  expect_true(is.na(res$metrics$auc))

  expect_true(is.factor(res$predictions))
  expect_length(res$predictions, nrow(res$test_data))
  expect_identical(length(res$train_index) + nrow(res$test_data), 90L)

  expect_true(is.matrix(res$probabilities))
  expect_identical(nrow(res$probabilities), nrow(res$test_data))
  expect_setequal(colnames(res$probabilities), levels(d$y))

  expect_s3_class(res$confusion, "table")
  expect_identical(names(dimnames(res$confusion)), c("Observed", "Predicted"))
  expect_equal(sum(res$confusion), nrow(res$test_data))

  expect_named(res$oob, "accuracy")
  expect_length(res$oob$accuracy, 1L)

  expect_identical(names(res$importance), c("feature", "importance"))
  expect_setequal(res$importance$feature, res$feature_names)
})

test_that("a decoy column named 'target' does not hijack stratified downsampling", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")
  skip_on_cran()

  # Regression test for the old `by = target` data.table NSE bug: when a column
  # is literally named "target", a bare `by = target` groups by *that* column
  # instead of the user's target. Here the decoy is all-unique, so the buggy
  # grouping would keep one row per group (100 rows) instead of honouring
  # sample_size (80 rows).
  set.seed(9090)
  n <- 100
  grp <- factor(rep(c("a", "b", "c", "d"), each = n / 4))
  d <- data.frame(
    target = as.numeric(seq_len(n)), # decoy, NOT the target argument
    grp    = grp,
    x1     = rnorm(n),
    x2     = rnorm(n)
  )

  res <- fs_randomforest(
    d, target = "grp", type = "classification",
    control = list(sample_size = 80, ntree = 20, seed = 21, n_cores = 1,
                   return_test_data = TRUE)
  )

  expect_identical(res$target, "grp")
  # 25 rows per class * (80 / 100) = exactly 20 rows kept per class
  expect_identical(length(res$train_index) + nrow(res$test_data), 80L)

  # the decoy is treated as an ordinary predictor; the user's column is not
  expect_true("target" %in% res$feature_names)
  expect_false("grp" %in% res$feature_names)

  # stratification followed the user's column: equally sized classes in, equally
  # sized classes out
  tab <- table(res$test_data$grp)
  expect_length(tab, 4L)
  expect_identical(length(unique(as.integer(tab))), 1L)

  expect_named(res$metrics, c("accuracy", "kappa", "auc"))
  expect_length(res$predictions, nrow(res$test_data))
})

test_that("a character predictor is coerced to a factor and trains", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")
  skip_on_cran()

  set.seed(505)
  n <- 80
  d <- data.frame(
    y   = rnorm(n),
    chr = rep(c("aa", "bb", "cc", "dd"), length.out = n),
    x1  = rnorm(n),
    stringsAsFactors = FALSE
  )

  res <- fs_randomforest(
    d, "y", "regression",
    control = list(ntree = 20, seed = 31, n_cores = 1, return_test_data = TRUE)
  )

  # randomForest rejects character predictors outright, so reaching a fitted
  # model at all proves the internal factor coercion happened
  expect_s3_class(res$model, "randomForest")
  expect_true("chr" %in% res$feature_names)
  expect_true(is.factor(res$test_data$chr))
  expect_named(res$metrics, c("RMSE", "MAE", "R2"))
  expect_length(res$predictions, nrow(res$test_data))
  expect_false(anyNA(res$predictions))
})

test_that("NAs reaching the test split are imputed instead of crashing predict", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")
  skip_on_cran()

  # The source order is split -> factor coercion -> level alignment -> NZV ->
  # imputation, and the imputation values are learned on the training rows but
  # applied to NAs in *both* partitions. 55 of 80 rows are NA, so the 20-row
  # test split is certain to contain NAs while the 60-row training split still
  # keeps enough finite values for a median.
  set.seed(606)
  n <- 80
  d <- data.frame(
    y = rnorm(n),
    A = rnorm(n),
    B = rnorm(n),
    C = rnorm(n)
  )
  d$A[seq_len(55)] <- NA_real_

  res <- fs_randomforest(
    d, "y", "regression",
    control = list(ntree = 20, seed = 13, n_cores = 1, impute = TRUE,
                   return_test_data = TRUE)
  )

  expect_true("A" %in% res$feature_names)
  expect_false(anyNA(res$test_data))
  expect_false(anyNA(res$test_data$A))
  expect_false(anyNA(res$predictions))
  expect_length(res$predictions, nrow(res$test_data))
  # a single training median was written into every test-set NA, so that value
  # now repeats in the held-out column
  expect_gte(max(table(res$test_data$A)), 2L)
})

test_that("OOB metrics are refused with a warning when the forest is combined", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")
  skip_if_not_installed("foreach")
  skip_if_not_installed("doParallel")
  skip_on_cran()

  cores <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
  skip_if(is.na(cores) || cores < 2L, "fewer than two cores available")

  set.seed(808)
  n <- 60
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- 2 * d$x1 + rnorm(n, sd = 0.5)

  expect_warning(
    res <- fs_randomforest(
      d, "y", "regression",
      control = list(ntree = 10, n_cores = 2, oob = TRUE, importance = FALSE,
                     seed = 5)
    ),
    "OOB metrics are unavailable when the forest is trained in parallel"
  )

  expect_null(res$oob)
  expect_null(res$importance)
  expect_s3_class(res$model, "randomForest")
  expect_named(res$metrics, c("RMSE", "MAE", "R2"))
})

test_that("same seed reproduces results and the caller's RNG state is untouched", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")
  skip_on_cran()

  set.seed(303)
  n <- 80
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- d$x1 - 2 * d$x3 + rnorm(n, sd = 0.4)

  set.seed(777)
  rng_before <- .Random.seed

  res1 <- fs_randomforest(d, "y", "regression",
                          control = list(ntree = 20, seed = 99, n_cores = 1))

  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, rng_before)

  # without return_test_data the result carries exactly the documented slots
  expect_named(res1, c("model", "metrics", "predictions", "probabilities",
                       "importance", "confusion", "oob", "target", "type",
                       "feature_names", "train_index", "control"))

  res2 <- fs_randomforest(d, "y", "regression",
                          control = list(ntree = 20, seed = 99, n_cores = 1))

  expect_identical(res1$train_index, res2$train_index)
  expect_identical(res1$predictions, res2$predictions)
  expect_identical(res1$metrics, res2$metrics)
  expect_identical(res1$importance, res2$importance)
})
