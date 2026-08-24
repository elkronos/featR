# Tests for fs_randomforest().
#
# Argument and control validation now runs BEFORE
# fs_require(c("randomForest", "caret")), so validation tests need no skips and
# run everywhere. Everything that actually grows a forest is gated on those two
# Suggests plus skip_on_cran() and kept deliberately small (n <= 100,
# ntree <= 25, n_cores = 1) because this is one of the slowest suites in the
# package.

test_that("fs_randomforest signature and defaults are stable", {
  fx <- formals(fs_randomforest)
  expect_identical(names(fx),
                   c("data", "target", "task", "control", "seed", "verbose",
                     "n_cores"))
  expect_identical(eval(fx$task), c("classification", "regression"))
  expect_identical(eval(fx$control), list())
  # seed, verbose and n_cores are real arguments now, not control entries
  expect_null(fx$seed)
  expect_identical(fx$verbose, FALSE)
  expect_identical(fx$n_cores, 1L)
})

test_that("the renamed 'type' argument is rejected by argument matching", {
  d <- data.frame(y = c(1, 2, 3, 4), x = c(4, 3, 2, 1))
  expect_error(fs_randomforest(d, "y", type = "regression"), "unused argument")
})

test_that("fs_randomforest rejects bad data/target/task/control", {
  d <- data.frame(y = c(1, 2, 3, 4), x = c(4, 3, 2, 1))

  expect_error(fs_randomforest("nope", "y", "regression"),
               "'data' must be a data\\.frame")
  expect_error(fs_randomforest(d[0, , drop = FALSE], "y", "regression"),
               "'data' must have at least one row and one column")
  expect_error(fs_randomforest(d, "missing_target", "regression"),
               "Column 'missing_target' not found in 'data'")
  expect_error(fs_randomforest(d, 1, "regression"),
               "'target' must be a single non-empty character string")
  expect_error(fs_randomforest(d, "y", task = "not_a_task"), "should be one of")
  expect_error(fs_randomforest(d, "y", "regression", control = "nope"),
               "'control' must be a list")
  expect_error(fs_randomforest(d, "y", "regression", verbose = NA),
               "'verbose' must be TRUE or FALSE")
  expect_error(fs_randomforest(d, "y", "regression", seed = "later"),
               "'seed' must be a single finite number")
  expect_error(fs_randomforest(d, "y", "regression", n_cores = 0),
               "'n_cores' must be between 1 and Inf")
})

test_that("control entries that moved out of 'control' are rejected", {
  d <- data.frame(y = c(1, 2, 3, 4), x = c(4, 3, 2, 1))

  # seed and n_cores are arguments of fs_randomforest() now
  expect_error(fs_randomforest(d, "y", "regression", control = list(seed = 1)),
               "Unknown 'control' entries: seed")
  expect_error(fs_randomforest(d, "y", "regression",
                               control = list(n_cores = 2)),
               "Unknown 'control' entries: n_cores")
  # split_ratio was renamed
  expect_error(fs_randomforest(d, "y", "regression",
                               control = list(split_ratio = 0.5)),
               "Unknown 'control' entries: split_ratio")
  # plain typos are caught too
  expect_error(fs_randomforest(d, "y", "regression", control = list(ntre = 5)),
               "Unknown 'control' entries: ntre")
})

test_that("fs_randomforest validates control values with clear messages", {
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

  expect_error(bad(list(train_ratio = "half")),
               "'control\\$train_ratio' must be a single finite number")
  expect_error(bad(list(train_ratio = 1)),
               "'control\\$train_ratio' must be strictly between 0 and 1")
  expect_error(bad(list(train_ratio = 0)),
               "'control\\$train_ratio' must be strictly between 0 and 1")

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
})

test_that("regression smoke: fs_result shape, metrics, and predictions", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")
  skip_on_cran()

  set.seed(1234)
  n <- 90
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- 3 * d$x1 - 2 * d$x2 + rnorm(n, sd = 0.4)

  res <- fs_randomforest(
    d, "y", "regression",
    control = list(ntree = 25, scale_importance = FALSE,
                   return_test_data = TRUE),
    seed = 13, n_cores = 1
  )

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "randomforest")
  expect_identical(res$task, "regression")
  expect_s3_class(res$model, "randomForest")
  expect_false(is.null(res$call))

  # documented details, all present and in order
  expect_named(res$details,
               c("metrics", "predictions", "probabilities", "importance",
                 "confusion", "oob", "feature_names", "train_index",
                 "test_data", "control", "n_features"))
  expect_identical(res$details$feature_names, c("x1", "x2", "x3"))
  expect_identical(res$details$n_features, 3L)

  # a plain random forest ranks rather than selects: every predictor is kept
  expect_setequal(res$selected, c("x1", "x2", "x3"))
  # ...but reported in decreasing importance order
  expect_identical(res$selected, names(sort(res$scores, decreasing = TRUE)))
  expect_true(is.numeric(res$scores))
  expect_setequal(names(res$scores), c("x1", "x2", "x3"))
  expect_output(print(res), "randomforest")

  # regression metrics
  expect_named(res$details$metrics, c("RMSE", "MAE", "R2"))
  expect_true(is.finite(res$details$metrics$RMSE))
  expect_true(is.finite(res$details$metrics$MAE))
  expect_true(is.finite(res$details$metrics$R2))

  # predictions line up with the held-out rows
  expect_true(is.numeric(res$details$predictions))
  expect_length(res$details$predictions, nrow(res$details$test_data))
  expect_type(res$details$train_index, "integer")
  expect_identical(length(res$details$train_index) +
                     nrow(res$details$test_data), 90L)

  # classification-only slots stay NULL but are still named
  expect_null(res$details$probabilities)
  expect_null(res$details$confusion)

  # OOB metrics are available for a sequentially trained forest
  expect_named(res$details$oob, "RMSE")
  expect_true(is.finite(res$details$oob$RMSE))

  # importance table: documented columns, sorted descending
  expect_s3_class(res$details$importance, "data.frame")
  expect_identical(names(res$details$importance), c("feature", "importance"))
  expect_setequal(res$details$importance$feature, res$details$feature_names)
  imp_vals <- res$details$importance$importance
  expect_false(anyNA(imp_vals))
  expect_identical(imp_vals, sort(imp_vals, decreasing = TRUE))

  # the merged control list keeps the documented default schema
  expect_named(res$details$control,
               c("train_ratio", "sample_size", "ntree", "importance",
                 "scale_importance", "mtry", "nodesize", "maxnodes", "sampsize",
                 "classwt", "strata", "replace", "preprocess", "feature_select",
                 "impute", "drop_zerovar", "oob", "return_test_data",
                 "positive_class"))
  expect_identical(res$details$control$train_ratio, 0.75)
  expect_true(res$details$control$importance)
  expect_true(res$details$control$impute)
  expect_true(res$details$control$drop_zerovar)
  expect_true(res$details$control$oob)
  expect_true(res$details$control$replace)
  expect_null(res$details$control$mtry)
  expect_null(res$details$control$sample_size)
  expect_null(res$details$control$positive_class)
})

test_that("classification smoke: fs_result shape, metrics, and predictions", {
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
    control = list(ntree = 25, return_test_data = TRUE),
    seed = 17, n_cores = 1
  )

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "randomforest")
  expect_identical(res$task, "classification")
  expect_named(res$details,
               c("metrics", "predictions", "probabilities", "importance",
                 "confusion", "oob", "feature_names", "train_index",
                 "test_data", "control", "n_features"))
  expect_identical(res$details$feature_names, c("x1", "x2", "x3"))
  expect_setequal(res$selected, c("x1", "x2", "x3"))
  # x1 carries the signal, so it must rank first
  expect_identical(res$selected[1L], "x1")

  expect_named(res$details$metrics, c("accuracy", "kappa", "auc"))
  expect_gte(res$details$metrics$accuracy, 0)
  expect_lte(res$details$metrics$accuracy, 1)
  expect_true(is.numeric(res$details$metrics$kappa))
  expect_length(res$details$metrics$kappa, 1L)
  # three classes: the binary AUC branch is never entered, so AUC stays NA and
  # pROC is not needed
  expect_true(is.na(res$details$metrics$auc))

  expect_true(is.factor(res$details$predictions))
  expect_length(res$details$predictions, nrow(res$details$test_data))
  expect_identical(length(res$details$train_index) +
                     nrow(res$details$test_data), 90L)

  expect_true(is.matrix(res$details$probabilities))
  expect_identical(nrow(res$details$probabilities),
                   nrow(res$details$test_data))
  expect_setequal(colnames(res$details$probabilities), levels(d$y))

  expect_s3_class(res$details$confusion, "table")
  expect_identical(names(dimnames(res$details$confusion)),
                   c("Observed", "Predicted"))
  expect_equal(sum(res$details$confusion), nrow(res$details$test_data))

  expect_named(res$details$oob, "accuracy")
  expect_length(res$details$oob$accuracy, 1L)

  expect_identical(names(res$details$importance), c("feature", "importance"))
  expect_setequal(res$details$importance$feature, res$details$feature_names)
})

test_that("control$feature_select sees only the training rows", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")
  skip_on_cran()

  # Leakage regression: the hook used to run on the FULL data before the
  # split, so a supervised selection rule saw the held-out rows. It must now
  # receive train_ratio * n rows, never all of them.
  set.seed(4242)
  n <- 100
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- 2 * d$x1 + rnorm(n, sd = 0.4)

  seen <- new.env(parent = emptyenv())
  seen$rows <- NA_integer_
  seen$cols <- character(0)

  # deliberately supervised: rank predictors by |correlation| with the target
  hook <- function(dt) {
    df <- as.data.frame(dt)
    seen$rows <- nrow(df)
    seen$cols <- names(df)
    preds <- setdiff(names(df), "y")
    r <- vapply(preds, function(nm) abs(stats::cor(df[[nm]], df$y)),
                numeric(1L))
    keep <- names(sort(r, decreasing = TRUE))[seq_len(2L)]
    df[, c("y", keep), drop = FALSE]
  }

  res <- fs_randomforest(
    d, "y", "regression",
    control = list(ntree = 20, train_ratio = 0.75, feature_select = hook,
                   return_test_data = TRUE),
    seed = 11, n_cores = 1
  )

  # the hook ran on the training split only
  expect_false(seen$rows == n)
  expect_identical(seen$rows, length(res$details$train_index))
  expect_gte(seen$rows, 0.7 * n)
  expect_lte(seen$rows, 0.8 * n)
  expect_setequal(seen$cols, names(d))

  # the held-out rows are exactly the rows the hook never saw
  expect_identical(seen$rows + nrow(res$details$test_data), 100L)

  # what the hook kept is what the result reports as selected
  expect_length(res$selected, 2L)
  expect_true("x1" %in% res$selected) # the only real signal
  expect_true(all(res$selected %in% c("x1", "x2", "x3")))
  expect_setequal(res$details$feature_names, res$selected)
  expect_identical(res$details$n_features, 3L)
})

test_that("a feature_select hook must select existing columns and keep the target", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")

  set.seed(55)
  n <- 40
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- d$x1 + rnorm(n, sd = 0.3)

  expect_error(
    fs_randomforest(d, "y", "regression",
                    control = list(ntree = 5,
                                   feature_select = function(dt) {
                                     as.data.frame(dt)[, "x1", drop = FALSE]
                                   }),
                    seed = 1),
    "Feature selection removed the target column"
  )

  # now that the hook runs on the training split, invented columns cannot be
  # carried over to the held-out rows
  expect_error(
    fs_randomforest(d, "y", "regression",
                    control = list(ntree = 5,
                                   feature_select = function(dt) {
                                     df <- as.data.frame(dt)
                                     df$brand_new <- df$x1 * 2
                                     df
                                   }),
                    seed = 1),
    "column\\(s\\) not present in the data: brand_new"
  )

  expect_error(
    fs_randomforest(d, "y", "regression",
                    control = list(ntree = 5, feature_select = "nope"),
                    seed = 1),
    "'control\\$feature_select' must be a function"
  )
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
    d, target = "grp", task = "classification",
    control = list(sample_size = 80, ntree = 20, return_test_data = TRUE),
    seed = 21, n_cores = 1
  )

  expect_identical(res$task, "classification")
  # 25 rows per class * (80 / 100) = exactly 20 rows kept per class
  expect_identical(length(res$details$train_index) +
                     nrow(res$details$test_data), 80L)

  # the decoy is treated as an ordinary predictor; the user's column is not
  expect_true("target" %in% res$details$feature_names)
  expect_false("grp" %in% res$details$feature_names)
  expect_true("target" %in% res$selected)
  expect_false("grp" %in% res$selected)

  # stratification followed the user's column: equally sized classes in,
  # equally sized classes out
  tab <- table(res$details$test_data$grp)
  expect_length(tab, 4L)
  expect_identical(length(unique(as.integer(tab))), 1L)

  expect_named(res$details$metrics, c("accuracy", "kappa", "auc"))
  expect_length(res$details$predictions, nrow(res$details$test_data))
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
    control = list(ntree = 20, return_test_data = TRUE),
    seed = 31, n_cores = 1
  )

  # randomForest rejects character predictors outright, so reaching a fitted
  # model at all proves the internal factor coercion happened
  expect_s3_class(res$model, "randomForest")
  expect_true("chr" %in% res$details$feature_names)
  expect_true("chr" %in% res$selected)
  expect_true(is.factor(res$details$test_data$chr))
  expect_named(res$details$metrics, c("RMSE", "MAE", "R2"))
  expect_length(res$details$predictions, nrow(res$details$test_data))
  expect_false(anyNA(res$details$predictions))
})

test_that("NAs reaching the test split are imputed instead of crashing predict", {
  skip_if_not_installed("randomForest")
  skip_if_not_installed("caret")
  skip_on_cran()

  # The source order is split -> feature_select -> factor coercion -> level
  # alignment -> NZV -> imputation, and the imputation values are learned on
  # the training rows but applied to NAs in *both* partitions. 55 of 80 rows
  # are NA, so the 20-row test split is certain to contain NAs while the
  # 60-row training split still keeps enough finite values for a median.
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
    control = list(ntree = 20, impute = TRUE, return_test_data = TRUE),
    seed = 13, n_cores = 1
  )

  expect_true("A" %in% res$details$feature_names)
  expect_false(anyNA(res$details$test_data))
  expect_false(anyNA(res$details$test_data$A))
  expect_false(anyNA(res$details$predictions))
  expect_length(res$details$predictions, nrow(res$details$test_data))
  # a single training median was written into every test-set NA, so that value
  # now repeats in the held-out column
  expect_gte(max(table(res$details$test_data$A)), 2L)
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
      control = list(ntree = 10, oob = TRUE, importance = FALSE),
      seed = 5, n_cores = 2
    ),
    "OOB metrics are unavailable when the forest is trained in parallel"
  )

  expect_null(res$details$oob)
  expect_null(res$details$importance)
  # no importance means no comparable per-feature score
  expect_null(res$scores)
  expect_setequal(res$selected, c("x1", "x2", "x3"))
  expect_identical(res$details$n_features, 3L)
  expect_s3_class(res$model, "randomForest")
  expect_named(res$details$metrics, c("RMSE", "MAE", "R2"))
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

  res1 <- fs_randomforest(d, "y", "regression", control = list(ntree = 20),
                          seed = 99, n_cores = 1)

  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, rng_before)

  # without return_test_data the slot is still named, but empty
  expect_true("test_data" %in% names(res1$details))
  expect_null(res1$details$test_data)

  res2 <- fs_randomforest(d, "y", "regression", control = list(ntree = 20),
                          seed = 99, n_cores = 1)

  expect_identical(res1$details$train_index, res2$details$train_index)
  expect_identical(res1$details$predictions, res2$details$predictions)
  expect_identical(res1$details$metrics, res2$details$metrics)
  expect_identical(res1$details$importance, res2$details$importance)
  expect_identical(res1$selected, res2$selected)
  expect_identical(res1$scores, res2$scores)
})
