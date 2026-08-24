# Tests for fs_recursivefeature().
#
# Argument validation runs BEFORE fs_require("caret"), so those tests need no
# skips. Anything that reaches caret::rfe() (or even caret::createDataPartition)
# is gated on the caret Suggests.
#
# Model-fitting tests deliberately pass rfe_control$functions = caret::lmFuncs
# and model_method = "lm" so that this suite stays fast and needs no
# randomForest; the default function set (caret::rfFuncs) is exercised by the
# fs_randomforest suite. Anything that fits a model is additionally gated with
# skip_on_cran() and kept small (n = 80, sizes = c(2, 3), cv number = 3).

test_that("fs_recursivefeature signature and defaults are stable", {
  fx <- formals(fs_recursivefeature)
  expect_identical(
    names(fx),
    c("data", "target", "sizes", "train_ratio", "rfe_control", "train_control",
      "model_method", "handle_categorical", "return_final_model", "seed",
      "verbose", "parallel")
  )
  expect_null(fx$sizes)
  expect_identical(fx$train_ratio, 0.8)
  expect_identical(eval(fx$rfe_control), list(method = "cv", number = 5))
  expect_identical(eval(fx$train_control), list(method = "cv", number = 5))
  expect_identical(fx$model_method, "rf")
  expect_identical(fx$handle_categorical, FALSE)
  expect_identical(fx$return_final_model, FALSE)
  expect_null(fx$seed)
  expect_identical(fx$verbose, FALSE)
  expect_identical(fx$parallel, FALSE)
})

test_that("removed and renamed arguments are rejected by argument matching", {
  # No skips: R rejects unmatched arguments before the body (and therefore
  # before fs_require()) ever runs.
  expect_false("early_stop" %in% names(formals(fs_recursivefeature)))
  expect_false("response_var" %in% names(formals(fs_recursivefeature)))
  expect_false("feature_funcs" %in% names(formals(fs_recursivefeature)))

  d <- data.frame(y = as.numeric(1:10), x = as.numeric(10:1))
  expect_error(fs_recursivefeature(d, "y", early_stop = FALSE),
               "unused argument")
  # renamed to 'target'
  expect_error(fs_recursivefeature(d, response_var = "y"), "unused argument")
  # the RFE function set moved into rfe_control$functions
  expect_error(fs_recursivefeature(d, "y", feature_funcs = NULL),
               "unused argument")
})

test_that("fs_recursivefeature validates data, target, flags, sizes, and train_ratio", {
  d <- data.frame(y = as.numeric(1:20), x1 = as.numeric(20:1),
                  x2 = rep(c(1, 2, 3, 4), each = 5))

  expect_error(fs_recursivefeature("nope", "y"), "'data' must be a data\\.frame")
  expect_error(fs_recursivefeature(d[0, , drop = FALSE], "y"),
               "'data' must have at least one row and one column")
  expect_error(fs_recursivefeature(d, "not_a_column"),
               "Column 'not_a_column' not found in 'data'")
  expect_error(fs_recursivefeature(d, 1),
               "'target' must be a single non-empty character string")
  expect_error(fs_recursivefeature(d, c("y", "x1")),
               "'target' must be a single non-empty character string")

  expect_error(fs_recursivefeature(d, "y", sizes = "wide"),
               "'sizes' must be a numeric vector or NULL")
  expect_error(fs_recursivefeature(d, "y", train_ratio = 0),
               "'train_ratio' must be strictly between 0 and 1")
  expect_error(fs_recursivefeature(d, "y", train_ratio = 1),
               "'train_ratio' must be strictly between 0 and 1")
  expect_error(fs_recursivefeature(d, "y", train_ratio = "most"),
               "'train_ratio' must be a single finite number")
  expect_error(fs_recursivefeature(d, "y", model_method = 1),
               "'model_method' must be a single non-empty character string")
  expect_error(fs_recursivefeature(d, "y", handle_categorical = NA),
               "'handle_categorical' must be TRUE or FALSE")
  expect_error(fs_recursivefeature(d, "y", return_final_model = 1),
               "'return_final_model' must be TRUE or FALSE")
  expect_error(fs_recursivefeature(d, "y", verbose = NA),
               "'verbose' must be TRUE or FALSE")
  expect_error(fs_recursivefeature(d, "y", parallel = "yes"),
               "'parallel' must be TRUE or FALSE")
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
  # the function set now travels in rfe_control and must be a caret list
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = list(method = "cv", number = 3,
                                                   functions = "lmFuncs")),
    "must be a caret RFE function set"
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
    fs_recursivefeature(d, "y",
                        sizes = c(1, 2),
                        rfe_control = list(method = "cv", number = 3),
                        seed = 1),
    "Predictors contain missing values"
  )
})

test_that("regression smoke: fs_result with the documented snake_case details", {
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
    sizes = c(2, 3),
    rfe_control = list(method = "cv", number = 3, functions = caret::lmFuncs),
    seed = 42
  )

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "rfe")
  expect_identical(res$task, "regression")
  expect_false(is.null(res$call))
  # return_final_model = FALSE, so the rfe object is the model
  expect_s3_class(res$model, "rfe")

  expect_named(res$details,
               c("rfe", "optimal_size", "test_metrics", "resampling_results",
                 "variable_importance", "preprocessor", "train_index",
                 "test_index", "final_model_variables", "n_features"))
  expect_s3_class(res$details$rfe, "rfe")

  expect_type(res$selected, "character")
  expect_gte(length(res$selected), 1L)
  expect_true(all(res$selected %in% c("x1", "x2", "x3", "x4")))
  expect_true(all(c("x1", "x2") %in% res$selected))
  expect_identical(res$selected, as.character(res$details$rfe$optVariables))

  expect_true(is.numeric(res$details$optimal_size))
  expect_length(res$details$optimal_size, 1L)
  expect_identical(res$details$n_features, 4L)

  # scores come from caret::varImp() on the rfe object
  expect_true(is.numeric(res$scores))
  expect_false(anyNA(res$scores))
  expect_true(all(names(res$scores) %in% c("x1", "x2", "x3", "x4")))
  expect_true(all(res$selected %in% names(res$scores)))
  expect_setequal(names(res$scores),
                  rownames(res$details$variable_importance))

  # test_metrics is caret::postResample() on the held-out rows
  expect_true(is.numeric(res$details$test_metrics))
  expect_true("RMSE" %in% names(res$details$test_metrics))
  expect_true(is.finite(res$details$test_metrics[["RMSE"]]))

  expect_s3_class(res$details$variable_importance, "data.frame")
  expect_s3_class(res$details$resampling_results, "data.frame")

  # opt-in slots stay NULL but remain named
  expect_null(res$details$preprocessor)
  expect_null(res$details$final_model_variables)

  # the 80/20 partition covers every row exactly once
  expect_identical(sort(c(res$details$train_index, res$details$test_index)),
                   seq_len(n))
  expect_length(intersect(res$details$train_index, res$details$test_index), 0L)

  expect_output(print(res), "rfe")
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
    sizes = c(2, 3),
    train_ratio = 0.8,
    rfe_control = list(method = "cv", number = 3, functions = caret::lmFuncs),
    train_control = list(method = "cv", number = 3),
    model_method = "lm",
    return_final_model = TRUE,
    seed = 7
  )

  # with return_final_model the final model is the headline model, and the rfe
  # object is still available in details
  expect_s3_class(res$model, "train")
  expect_s3_class(res$details$rfe, "rfe")

  # caret::train() keeps its fitting frame in $trainingData (target renamed to
  # .outcome); its row count is the honesty check.
  training_data <- res$model$trainingData
  expect_s3_class(training_data, "data.frame")
  expect_true(".outcome" %in% names(training_data))

  expect_identical(nrow(training_data), length(res$details$train_index))
  expect_false(nrow(training_data) == n)
  expect_gte(nrow(training_data), 0.7 * n)
  expect_lte(nrow(training_data), 0.9 * n)

  expect_type(res$details$final_model_variables, "character")
  expect_true(all(res$details$final_model_variables %in% res$selected))
  expect_identical(res$details$final_model_variables,
                   attr(res$model, "predictors_used"))
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

  expect_warning(
    res <- fs_recursivefeature(
      d, "y",
      sizes = c(2, 3, 99),
      rfe_control = list(method = "cv", number = 3, functions = caret::lmFuncs),
      seed = 3
    ),
    "value(s) of 'sizes' outside",
    fixed = TRUE
  )

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "rfe")
  expect_s3_class(res$details$rfe, "rfe")
  expect_true(all(res$selected %in% c("x1", "x2", "x3", "x4")))
})

test_that("'sizes' entirely out of range is an error, not a silent empty run", {
  skip_if_not_installed("caret")

  set.seed(121)
  d <- data.frame(y = rnorm(40), x1 = rnorm(40), x2 = rnorm(40))

  expect_error(
    suppressWarnings(
      fs_recursivefeature(d, "y",
                          sizes = c(50, 99),
                          rfe_control = list(method = "cv", number = 3),
                          seed = 2)
    ),
    "No valid 'sizes' remain within the range of available predictors"
  )
})

test_that("same seed reproduces the selection and leaves the RNG untouched", {
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
      sizes = c(2, 3),
      rfe_control = list(method = "cv", number = 3,
                         functions = caret::lmFuncs),
      seed = 99
    )
  }

  set.seed(777)
  rng_before <- .Random.seed

  res1 <- run()

  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, rng_before)

  res2 <- run()

  expect_identical(res1$selected, res2$selected)
  expect_identical(res1$scores, res2$scores)
  expect_identical(res1$details$optimal_size, res2$details$optimal_size)
  expect_identical(res1$details$train_index, res2$details$train_index)
  expect_identical(res1$details$test_index, res2$details$test_index)
  expect_identical(res1$details$test_metrics, res2$details$test_metrics)
})

test_that("handle_categorical = TRUE one-hot encodes factor predictors", {
  # COVERAGE: the encoder path (rfe_fit_encoder / rfe_apply_encoder) was only
  # ever reached by unit tests of the helpers, never through fs_recursivefeature
  # itself, so the align-then-encode ordering fixed earlier was untested
  # end to end.
  skip_on_cran()
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")

  n <- 60L
  grp <- rep(c("a", "b", "c"), length.out = n)
  wobble <- rep(c(-0.3, 0.1, 0.2, -0.1, 0.25, -0.15), 10)
  d <- data.frame(
    y     = ifelse(grp == "a", 5, ifelse(grp == "b", 0, -5)) + wobble,
    grp   = factor(grp),               # informative factor
    chr   = rep(c("p", "q"), 30),      # character, must be coerced
    num   = seq(-1, 1, length.out = n),
    stringsAsFactors = FALSE
  )

  res <- fs_recursivefeature(
    d, "y", sizes = c(2, 4), train_ratio = 0.7,
    handle_categorical = TRUE, seed = 11
  )

  expect_s3_class(res, "fs_result")
  expect_type(res$selected, "character")
  expect_gt(length(res$selected), 0L)
  # Selected names come from the encoded design, so the factor appears as
  # dummy columns rather than as "grp" itself.
  expect_false("grp" %in% res$selected)
  expect_true(any(grepl("^grp", res$selected)))
  # The held-out evaluation still happened on rows the selection never saw.
  expect_true(all(c("RMSE", "Rsquared", "MAE") %in%
                    names(res$details$test_metrics)))
  expect_true(is.finite(res$details$test_metrics[["RMSE"]]))
})
