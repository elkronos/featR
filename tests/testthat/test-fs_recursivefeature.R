# Tests for fs_recursivefeature().
#
# fs_recursivefeature() calls fs_require("caret") as the *first* statement of
# its body, before every assert_*() call, so argument-validation tests still
# need skip_if_not_installed("caret"). Only formals() inspection and R's own
# argument matching (the removed 'early_stop' argument) run unconditionally.
#
# Model-fitting tests deliberately use caret::lmFuncs / model_method = "lm" so
# that this suite stays fast and needs no randomForest; the default
# feature_funcs (caret::rfFuncs) is exercised by the fs_randomforest suite.
# Anything that fits a model is additionally gated with skip_on_cran() and kept
# small (n = 80, sizes = c(2, 3), cv number = 3).

test_that("fs_recursivefeature signature and defaults are stable", {
  fx <- formals(fs_recursivefeature)
  expect_identical(
    names(fx),
    c("data", "response_var", "seed", "rfe_control", "train_control", "sizes",
      "parallel", "feature_funcs", "handle_categorical", "return_final_model",
      "model_method")
  )
  expect_null(fx$seed)
  expect_null(fx$sizes)
  expect_null(fx$feature_funcs)
  expect_identical(fx$parallel, FALSE)
  expect_identical(fx$handle_categorical, FALSE)
  expect_identical(fx$return_final_model, FALSE)
  expect_identical(fx$model_method, "rf")
  expect_identical(eval(fx$rfe_control), list(method = "cv", number = 5))
  expect_identical(eval(fx$train_control), list(method = "cv", number = 5))
})

test_that("the removed 'early_stop' argument is rejected by argument matching", {
  # No skips: R rejects the unmatched argument before the body (and therefore
  # before fs_require()) ever runs.
  expect_false("early_stop" %in% names(formals(fs_recursivefeature)))

  d <- data.frame(y = as.numeric(1:10), x = as.numeric(10:1))
  expect_error(fs_recursivefeature(d, "y", early_stop = FALSE),
               "unused argument")
})

test_that("fs_recursivefeature validates data, flags, sizes, and model_method", {
  skip_if_not_installed("caret")

  d <- data.frame(y = as.numeric(1:20), x1 = as.numeric(20:1),
                  x2 = rep(c(1, 2, 3, 4), each = 5))

  expect_error(fs_recursivefeature("nope", "y"), "'data' must be a data\\.frame")
  expect_error(fs_recursivefeature(d[0, , drop = FALSE], "y"),
               "'data' must have at least one row and one column")
  expect_error(fs_recursivefeature(d, "y", parallel = "yes"),
               "'parallel' must be TRUE or FALSE")
  expect_error(fs_recursivefeature(d, "y", handle_categorical = NA),
               "'handle_categorical' must be TRUE or FALSE")
  expect_error(fs_recursivefeature(d, "y", return_final_model = 1),
               "'return_final_model' must be TRUE or FALSE")
  expect_error(fs_recursivefeature(d, "y", model_method = 1),
               "'model_method' must be a single non-empty character string")
  expect_error(fs_recursivefeature(d, "y", sizes = "wide"),
               "'sizes' must be a numeric vector or NULL")
})

test_that("fs_recursivefeature validates response_var", {
  skip_if_not_installed("caret")

  d <- data.frame(y = as.numeric(1:20), x1 = as.numeric(20:1),
                  x2 = rep(c(1, 2, 3, 4), each = 5))

  expect_error(fs_recursivefeature(d, "not_a_column"),
               "'response_var' name not found in 'data'")
  expect_error(fs_recursivefeature(d, 99),
               "'response_var' index is out of bounds")
  expect_error(fs_recursivefeature(d, 0),
               "'response_var' index is out of bounds")
  expect_error(fs_recursivefeature(d, 1.5),
               "'response_var' must be a single finite integer index or a column name")
  expect_error(fs_recursivefeature(d, c(1L, 2L)),
               "'response_var' must be a single finite integer index or a column name")
  expect_error(fs_recursivefeature(d, TRUE),
               "'response_var' must be a single column name \\(character\\) or a single column index \\(integer\\)")
})

test_that("fs_recursivefeature validates rfe_control before resampling", {
  skip_if_not_installed("caret")

  d <- data.frame(y = as.numeric(1:20), x1 = as.numeric(20:1),
                  x2 = rep(c(1, 2, 3, 4), each = 5))

  expect_error(fs_recursivefeature(d, "y", rfe_control = "cv"),
               "'rfe_control' must be a list")
  expect_error(fs_recursivefeature(d, "y", rfe_control = list(number = 3)),
               "'rfe_control' must contain 'method'")
  expect_error(fs_recursivefeature(d, "y", rfe_control = list(method = "cv")),
               "'rfe_control' must contain 'number'")
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = list(method = "cv", number = 0)),
    "'rfe_control\\$number' must be between 1 and Inf"
  )
})

test_that("missing values in the predictors stop with actionable guidance", {
  skip_if_not_installed("caret")

  # 20 of 40 rows carry an NA and the split holds out at most a fifth of the
  # rows, so the training partition is guaranteed to contain missing predictors
  # no matter how the stratified split falls. The stop() fires before
  # caret::rfe() is reached, so no model is ever fitted here.
  set.seed(111)
  d <- data.frame(y = rnorm(40), x1 = rnorm(40), x2 = rnorm(40))
  d$x1[seq_len(20)] <- NA_real_

  expect_error(
    fs_recursivefeature(d, "y", seed = 1,
                        rfe_control = list(method = "cv", number = 3),
                        sizes = c(1, 2)),
    "Predictors contain missing values"
  )
})

test_that("regression smoke: documented PascalCase structure and TestMetrics", {
  skip_if_not_installed("caret")
  skip_on_cran()

  set.seed(707)
  n <- 80
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  d <- data.frame(
    x1 = x1,
    x2 = x2,
    x3 = rnorm(n),
    x4 = rnorm(n),
    y  = 3 * x1 - 2 * x2 + rnorm(n, sd = 0.4)
  )

  res <- fs_recursivefeature(
    d, "y",
    seed = 42,
    rfe_control = list(method = "cv", number = 3),
    sizes = c(2, 3),
    feature_funcs = caret::lmFuncs
  )

  expect_named(res, c("ResponseName", "TaskType", "TrainIndex", "TestIndex",
                      "Preprocessor", "RFE", "OptimalNumberOfVariables",
                      "OptimalVariables", "VariableImportance",
                      "ResamplingResults", "TestMetrics", "FinalModel",
                      "FinalModelVariables"))

  expect_identical(res$ResponseName, "y")
  expect_identical(res$TaskType, "regression")
  expect_s3_class(res$RFE, "rfe")

  expect_type(res$OptimalVariables, "character")
  expect_gte(length(res$OptimalVariables), 1L)
  expect_true(all(res$OptimalVariables %in% c("x1", "x2", "x3", "x4")))
  expect_true(all(c("x1", "x2") %in% res$OptimalVariables))
  expect_true(is.numeric(res$OptimalNumberOfVariables))
  expect_length(res$OptimalNumberOfVariables, 1L)

  # TestMetrics is caret::postResample() on the held-out rows
  expect_true(is.numeric(res$TestMetrics))
  expect_true("RMSE" %in% names(res$TestMetrics))
  expect_true(is.finite(res$TestMetrics[["RMSE"]]))

  expect_s3_class(res$VariableImportance, "data.frame")
  expect_s3_class(res$ResamplingResults, "data.frame")

  # opt-in slots stay NULL but remain named
  expect_null(res$Preprocessor)
  expect_null(res$FinalModel)
  expect_null(res$FinalModelVariables)

  # the 80/20 partition covers every row exactly once
  expect_identical(sort(c(res$TrainIndex, res$TestIndex)), seq_len(n))
  expect_length(intersect(res$TrainIndex, res$TestIndex), 0L)
})

test_that("return_final_model trains on the training rows only, not the full data", {
  skip_if_not_installed("caret")
  skip_on_cran()

  set.seed(808)
  n <- 80
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  d <- data.frame(
    x1 = x1,
    x2 = x2,
    x3 = rnorm(n),
    x4 = rnorm(n),
    y  = 3 * x1 - 2 * x2 + rnorm(n, sd = 0.4)
  )

  res <- fs_recursivefeature(
    d, "y",
    seed = 7,
    rfe_control = list(method = "cv", number = 3),
    train_control = list(method = "cv", number = 3),
    sizes = c(2, 3),
    feature_funcs = caret::lmFuncs,
    return_final_model = TRUE,
    model_method = "lm"
  )

  expect_s3_class(res$FinalModel, "train")

  # caret::train() keeps its fitting frame in $trainingData (response renamed
  # to .outcome); its row count is the honesty check.
  training_data <- res$FinalModel$trainingData
  expect_s3_class(training_data, "data.frame")
  expect_true(".outcome" %in% names(training_data))

  expect_identical(nrow(training_data), length(res$TrainIndex))
  expect_false(nrow(training_data) == n)
  expect_gte(nrow(training_data), 0.7 * n)
  expect_lte(nrow(training_data), 0.9 * n)

  expect_type(res$FinalModelVariables, "character")
  expect_true(all(res$FinalModelVariables %in% res$OptimalVariables))
  expect_identical(res$FinalModelVariables,
                   attr(res$FinalModel, "predictors_used"))
})

test_that("out-of-range 'sizes' are filtered with a warning", {
  skip_if_not_installed("caret")
  skip_on_cran()

  set.seed(909)
  n <- 80
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  d <- data.frame(
    x1 = x1,
    x2 = x2,
    x3 = rnorm(n),
    x4 = rnorm(n),
    y  = 3 * x1 - 2 * x2 + rnorm(n, sd = 0.4)
  )

  # response_var given as a column index as well: "y" is column 5
  expect_warning(
    res <- fs_recursivefeature(
      d, 5,
      seed = 3,
      rfe_control = list(method = "cv", number = 3),
      sizes = c(2, 3, 99),
      feature_funcs = caret::lmFuncs
    ),
    "value(s) of 'sizes' outside",
    fixed = TRUE
  )

  expect_identical(res$ResponseName, "y")
  expect_s3_class(res$RFE, "rfe")
  expect_true(all(res$OptimalVariables %in% c("x1", "x2", "x3", "x4")))
})

test_that("'sizes' entirely out of range is an error, not a silent empty run", {
  skip_if_not_installed("caret")

  set.seed(121)
  d <- data.frame(y = rnorm(40), x1 = rnorm(40), x2 = rnorm(40))

  expect_error(
    suppressWarnings(
      fs_recursivefeature(d, "y", seed = 2,
                          rfe_control = list(method = "cv", number = 3),
                          sizes = c(50, 99))
    ),
    "No valid 'sizes' remain within the range of available predictors"
  )
})

test_that("same seed reproduces OptimalVariables and leaves the RNG untouched", {
  skip_if_not_installed("caret")
  skip_on_cran()

  set.seed(303)
  n <- 80
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  d <- data.frame(
    x1 = x1,
    x2 = x2,
    x3 = rnorm(n),
    x4 = rnorm(n),
    y  = 3 * x1 - 2 * x2 + rnorm(n, sd = 0.4)
  )

  run <- function() {
    fs_recursivefeature(
      d, "y",
      seed = 99,
      rfe_control = list(method = "cv", number = 3),
      sizes = c(2, 3),
      feature_funcs = caret::lmFuncs
    )
  }

  set.seed(777)
  rng_before <- .Random.seed

  res1 <- run()

  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, rng_before)

  res2 <- run()

  expect_identical(res1$OptimalVariables, res2$OptimalVariables)
  expect_identical(res1$OptimalNumberOfVariables,
                   res2$OptimalNumberOfVariables)
  expect_identical(res1$TrainIndex, res2$TrainIndex)
  expect_identical(res1$TestIndex, res2$TestIndex)
  expect_identical(res1$TestMetrics, res2$TestMetrics)
})
