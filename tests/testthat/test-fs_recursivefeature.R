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
               "'sizes' must be a vector of whole numbers, or NULL")
  # fractional sizes used to reach caret::rfe() and show up as a "1.5
  # variables" row in the resampling results
  expect_error(fs_recursivefeature(d, "y", sizes = c(1.5, 2)),
               "'sizes' must be a vector of whole numbers, or NULL")
  expect_error(fs_recursivefeature(d, "y", sizes = c(1, NA)),
               "'sizes' must be a vector of whole numbers, or NULL")
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

  # scores: full-size importance averaged over every resample, one entry per
  # candidate; caret's varImp() table (optimal-size survivors) is a subset
  expect_true(is.numeric(res$scores))
  expect_false(anyNA(res$scores))
  expect_setequal(names(res$scores), c("x1", "x2", "x3", "x4"))
  expect_true(all(res$selected %in% names(res$scores)))
  expect_true(all(rownames(res$details$variable_importance) %in%
                    names(res$scores)))

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

test_that("held-out accuracy counts predictions of classes absent from the test rows", {
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  skip_if_not_installed("e1071")
  skip_on_cran()

  # Regression test: with a character target the test factor was rebuilt from
  # the test rows alone, so a class missing from them was not a level and
  # caret::postResample() silently dropped every prediction of it, inflating
  # the accuracy.
  set.seed(5150)
  d <- data.frame(
    y  = c(rep("a", 28), rep("b", 28), rep("c", 4)),
    x1 = c(rnorm(28, 0, 0.3), rnorm(28, 5, 0.3), rnorm(4, 10, 0.3)),
    x2 = rnorm(60),
    stringsAsFactors = FALSE
  )
  # The split is the first draw after seeding, so it can be replayed. With
  # p = 0.9 all four "c" rows land in training; plant a "c"-looking test row.
  train_idx <- withr::with_seed(
    3, featR:::fs_split_index(factor(d$y), p = 0.9)
  )
  test_idx <- setdiff(seq_len(nrow(d)), train_idx)
  expect_false("c" %in% d$y[test_idx])
  d$x1[test_idx[d$y[test_idx] == "a"][1L]] <- 10

  res <- fs_recursivefeature(
    d, "y", sizes = 1:2, train_ratio = 0.9,
    rfe_control = list(method = "cv", number = 3), seed = 3
  )
  expect_identical(res$details$test_index, test_idx)

  pred <- predict(res$details$rfe, d[test_idx, c("x1", "x2")])$pred
  expect_true("c" %in% as.character(pred))
  reference <- mean(as.character(pred) == d$y[test_idx])
  expect_equal(res$details$test_metrics[["Accuracy"]], reference)
})

test_that("a factor target with an unused level runs", {
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  skip_if_not_installed("e1071")
  skip_on_cran()

  # Regression test: the unused "setosa" level reached randomForest, which
  # stops with "Can't have empty classes in y".
  two <- iris[iris$Species != "setosa", ]
  expect_identical(nlevels(two$Species), 3L)
  res <- fs_recursivefeature(two, "Species", sizes = 1:2,
                             rfe_control = list(method = "cv", number = 3),
                             seed = 1)
  expect_s3_class(res, "fs_result")
  expect_identical(res$task, "classification")
  expect_true(is.finite(res$details$test_metrics[["Accuracy"]]))
})

test_that("a missing target value is a clear error", {
  skip_if_not_installed("caret")

  d <- data.frame(y = c(NA, rnorm(29)), x1 = rnorm(30), x2 = rnorm(30))
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = list(method = "cv", number = 3,
                                                   functions = caret::lmFuncs),
                        seed = 1),
    "The target contains missing values"
  )
})

test_that("non-syntactic predictor names work and are reported unchanged", {
  skip_if_not_installed("caret")
  skip_on_cran()

  # Regression test: caret::lmFuncs rebuilds data frames with check.names =
  # TRUE, so names like "a b" failed inside every resample ("replacement has
  # 1 row, data has 0").
  set.seed(77)
  n <- 80
  d <- data.frame(
    `my y` = rnorm(n), `a b` = rnorm(n), `c-d` = rnorm(n), `1st` = rnorm(n),
    check.names = FALSE
  )
  d$`my y` <- 3 * d$`a b` - 2 * d$`c-d` + rnorm(n, sd = 0.3)

  res <- fs_recursivefeature(
    d, "my y", sizes = 1:2,
    rfe_control = list(method = "cv", number = 3, functions = caret::lmFuncs),
    return_final_model = TRUE, model_method = "lm",
    train_control = list(method = "cv", number = 3),
    seed = 4
  )
  user_names <- c("a b", "c-d", "1st")
  expect_true(all(c("a b", "c-d") %in% res$selected))
  expect_true(all(res$selected %in% user_names))
  expect_true(all(names(res$scores) %in% user_names))
  expect_setequal(rownames(res$details$variable_importance), names(res$scores))
  expect_true(all(res$details$final_model_variables %in% user_names))
  expect_true(is.finite(res$details$test_metrics[["RMSE"]]))
})

test_that("parallel = TRUE restores the previously registered backend", {
  skip_if_not_installed("caret")
  skip_if_not_installed("foreach")
  skip_if_not_installed("doParallel")
  skip_on_cran()
  cores <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
  skip_if(is.na(cores) || cores < 2L, "fewer than two cores available")

  foreach::registerDoSEQ()
  before <- foreach::getDoParName()
  res <- fs_recursivefeature(
    mtcars, "mpg", sizes = c(2, 3),
    rfe_control = list(method = "cv", number = 3, functions = caret::lmFuncs),
    seed = 1, parallel = TRUE
  )
  expect_identical(foreach::getDoParName(), before)
  seq_res <- fs_recursivefeature(
    mtcars, "mpg", sizes = c(2, 3),
    rfe_control = list(method = "cv", number = 3, functions = caret::lmFuncs),
    seed = 1
  )
  expect_identical(res$selected, seq_res$selected)
})

test_that("degenerate targets, names and single predictors fail clearly", {
  # REGRESSION: these surfaced as caret errors wrapped by foreach:
  # 'task 1 failed - "Need at least two classes to do classification."',
  # 'column name "" cannot match any column' (an empty-string class),
  # 'task 1 failed - "argument 1 is not a vector"' (one predictor,
  # classification), and "undefined columns selected" (an empty name).
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  set.seed(31)
  n <- 40
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  ctl <- list(method = "cv", number = 2)

  d$y <- factor(rep("a", n), levels = c("a", "b"))
  expect_error(fs_recursivefeature(d, "y", rfe_control = ctl),
               "at least two observed classes")

  d$y <- factor(ifelse(d$x1 > 0, "", "b"))
  expect_error(fs_recursivefeature(d, "y", rfe_control = ctl),
               "empty-string class label")

  d$y <- factor(ifelse(d$x1 > 0, "a", "b"))
  expect_error(fs_recursivefeature(d[c("x1", "y")], "y", rfe_control = ctl),
               "at least two candidate predictors")

  names(d)[2] <- ""
  expect_error(fs_recursivefeature(d, "y", rfe_control = ctl),
               "empty or NA column names")
})

test_that("seeded parallel runs are reproducible", {
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  skip_if_not_installed("foreach")
  skip_if_not_installed("doParallel")
  skip_on_cran()
  cores <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
  skip_if(is.na(cores) || cores < 2L, "fewer than two cores available")

  # REGRESSION: only the worker RNG streams were seeded, but foreach hands
  # resamples to whichever worker is free, so the stream used for a given
  # resample (and hence its random forest) changed from run to run; about
  # half of all repeat runs disagreed.
  set.seed(1)
  n <- 120
  d <- data.frame(matrix(rnorm(n * 6), n))
  d$y <- d$X1 + 0.5 * d$X2 + rnorm(n)
  run <- function() {
    fs_recursivefeature(d, "y", seed = 7, parallel = TRUE,
                        rfe_control = list(method = "cv", number = 6),
                        verbose = FALSE)$details$resampling_results
  }
  first <- run()
  for (i in 1:4) {
    expect_identical(run(), first)
  }
})

test_that("row indices in rfe_control are refused rather than misapplied", {
  # Second-round statistical review: rfe() runs on the training split drawn
  # inside fs_recursivefeature(), so rfe_control$index (row numbers of
  # `data`) pointed at different observations of that split and the
  # resamples silently trained and tested on the wrong rows.
  skip_if_not_installed("caret")
  set.seed(3)
  n <- 60
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- d$x1 + rnorm(n)
  ctl <- list(method = "cv", number = 2, functions = caret::lmFuncs,
              index = list(F1 = 1:25, F2 = 26:45))
  expect_error(fs_recursivefeature(d, "y", rfe_control = ctl, seed = 1),
               "rfe_control\\$index")
  expect_error(featR:::rfe_validate_train_control(
    list(method = "cv", number = 2, indexOut = list(1:3))
  ), "train_control\\$indexOut")
})

# ---- Round-three regression tests ------------------------------------------

lm_ctl <- function(number = 3) {
  list(method = "cv", number = number, functions = caret::lmFuncs)
}

test_that("a dummy name colliding with another column is an error (C0)", {
  # REGRESSION: factor g (level "1") encodes to "g1", next to a numeric
  # "g1"; setdiff() collapsed the two and caret resolved "g1" to the noise
  # dummy, so the real predictor was silently lost (held-out R2 ~ 0).
  skip_if_not_installed("caret")
  set.seed(1)
  n <- 60
  d <- data.frame(g = factor(sample(c("0", "1"), n, TRUE)), g1 = rnorm(n),
                  w = rnorm(n))
  d$y <- 3 * d$g1 + rnorm(n)
  expect_error(
    fs_recursivefeature(d, "y", sizes = 1:2, handle_categorical = TRUE,
                        rfe_control = lm_ctl(), seed = 1),
    "collide with existing columns: g1"
  )
})

test_that("handle_categorical = TRUE reports names without backticks (C1, C79)", {
  skip_if_not_installed("caret")
  skip_on_cran()
  set.seed(1)
  n <- 100
  d <- data.frame(`x 1` = rnorm(n), `b-2` = rnorm(n), c3 = rnorm(n),
                  check.names = FALSE)
  d$`my grp` <- factor(sample(c("a", "b", "c"), n, TRUE))
  d$y <- 2 * d$`x 1` + d$`b-2` + 3 * (d$`my grp` == "b") + rnorm(n, sd = 0.3)
  res <- fs_recursivefeature(d, "y", handle_categorical = TRUE,
                             rfe_control = lm_ctl(), seed = 1)
  reported <- c(res$selected, names(res$scores),
                rownames(res$details$variable_importance))
  expect_false(any(grepl("`", reported, fixed = TRUE)))
  expect_true(all(c("x 1", "b-2", "my grpb") %in% res$selected))
  num_sel <- intersect(res$selected, names(d))
  expect_true(all(c("x 1", "b-2") %in% num_sel))
  expect_silent(d[num_sel])
})

test_that("factor predictors with lmFuncs and no encoding fail clearly (C16)", {
  # REGRESSION: lmFuncs ranks dummy names ("gb"), which caret then used to
  # subset x: 'task 1 failed - "undefined columns selected"'.
  skip_if_not_installed("caret")
  skip_on_cran()
  set.seed(1)
  n <- 90
  d <- data.frame(a = rnorm(n), b = rnorm(n),
                  g = factor(sample(letters[1:3], n, TRUE)))
  d$y <- 2 * d$a + rnorm(n)
  expect_error(
    fs_recursivefeature(d, "y", sizes = 1:3, rfe_control = lm_ctl(), seed = 1),
    "set handle_categorical = TRUE"
  )
})

test_that("infinite values in a predictor or the target are rejected (C23)", {
  # No skips: the check runs before fs_require("caret").
  set.seed(1)
  n <- 40
  d <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  d$y <- d$a + rnorm(n)
  d1 <- d
  d1$b[3] <- Inf
  expect_error(fs_recursivefeature(d1, "y", seed = 1),
               "finite values; column\\(s\\) with Inf/-Inf: b")
  d2 <- d
  d2$y[3] <- -Inf
  expect_error(fs_recursivefeature(d2, "y", seed = 1),
               "finite values; column\\(s\\) with Inf/-Inf: y")
})

test_that("factor levels absent from the training rows are not encoded (C35)", {
  # REGRESSION: unused and test-only levels became all-zero candidate dummies.
  skip_if_not_installed("caret")
  skip_on_cran()
  set.seed(1)
  n <- 80
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- d$x1 + rnorm(n)
  d$g <- factor(sample(c("p", "q"), n, TRUE),
                levels = c("p", "q", "r", "never"))
  res <- suppressWarnings(fs_recursivefeature(
    d, "y", handle_categorical = TRUE, rfe_control = lm_ctl(), seed = 3
  ))
  expect_identical(res$details$preprocessor$lvls$g, c("p", "q"))
  expect_identical(res$details$n_features, 4L)

  # A held-out row whose level the training rows never saw is reported.
  test_row <- setdiff(seq_len(n), res$details$train_index)[1L]
  d$g[test_row] <- "r"
  expect_warning(
    fs_recursivefeature(d, "y", handle_categorical = TRUE,
                        rfe_control = lm_ctl(), seed = 3),
    "1 held-out value\\(s\\) have a factor level not seen"
  )
})

test_that("indexFinal is refused like index/indexOut (C37)", {
  skip_if_not_installed("caret")
  expect_error(featR:::rfe_validate_train_control(
    list(method = "cv", number = 3, indexFinal = 1:10)
  ), "train_control\\$indexFinal")
  d <- data.frame(y = rnorm(30), x1 = rnorm(30), x2 = rnorm(30))
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = lm_ctl(),
                        train_control = list(method = "cv", number = 3,
                                             indexFinal = 1:10),
                        model_method = "lm", return_final_model = TRUE),
    "train_control\\$indexFinal"
  )
})

test_that("scores average full-size importance over every resample (C42)", {
  # REGRESSION: scores came from caret::varImp(rfe), which averages only the
  # resamples where a variable survived to the optimal size, so noise
  # variables that survived once outranked selected ones and candidates that
  # never survived had no score at all.
  skip_if_not_installed("caret")
  skip_on_cran()
  set.seed(5)
  n <- 100
  p <- 20
  df <- as.data.frame(matrix(rnorm(n * p), n))
  df$y <- df$V1 + rnorm(n)
  res <- fs_recursivefeature(df, "y", sizes = 5,
                             rfe_control = lm_ctl(5), seed = 1)
  expect_setequal(names(res$scores), paste0("V", 1:p))
  expect_false(anyNA(res$scores))
  v <- res$details$rfe$variables
  full <- v[v$Variables == max(v$Variables), ]
  expected <- tapply(full$Overall, full$var, mean)
  expect_equal(res$scores[names(expected)], c(expected), ignore_attr = TRUE)
  expect_identical(names(sort(res$scores, decreasing = TRUE))[1L], "V1")
})

test_that("a sequential call does not use a caller's foreach backend for the final model (C53)", {
  skip_if_not_installed("caret")
  skip_if_not_installed("foreach")
  skip_on_cran()
  prev <- featR:::foreach_backend_snapshot()
  withr::defer(featR:::foreach_backend_restore(prev))
  calls <- 0L
  spy <- function(obj, expr, envir, data) {
    calls <<- calls + 1L
    foreach:::doSEQ(obj, expr, envir, data)
  }
  foreach::setDoPar(spy, NULL, function(data, item) {
    switch(item, workers = 4L, name = "spy", version = "1", NULL)
  })
  set.seed(1)
  d <- data.frame(y = rnorm(60), x1 = rnorm(60), x2 = rnorm(60),
                  x3 = rnorm(60))
  res <- fs_recursivefeature(d, "y", sizes = 1:2, rfe_control = lm_ctl(),
                             train_control = list(method = "cv", number = 3),
                             model_method = "lm", return_final_model = TRUE,
                             seed = 1)
  expect_identical(calls, 0L)
  expect_false(res$model$control$allowParallel)
})

test_that("constant predictors are removed before RFE with a warning (C58)", {
  # REGRESSION: a constant column reached randomForest, which never returns
  # for a classification forest on one constant predictor (subset size 1).
  # lmFuncs keeps this fast; the removal happens before any function set runs.
  skip_if_not_installed("caret")
  skip_on_cran()
  set.seed(2)
  n <- 60
  d <- data.frame(y = rnorm(n), x1 = rnorm(n), x2 = rnorm(n), k = 0,
                  f = factor("u"))
  expect_warning(
    res <- fs_recursivefeature(d, "y", rfe_control = lm_ctl(), seed = 1),
    "constant on the training rows before RFE: k, f"
  )
  expect_identical(res$details$n_features, 2L)
  expect_false(any(c("k", "f") %in% names(res$scores)))
})

test_that("e1071 is checked before any resampling for classification (C61)", {
  skip_if_not_installed("caret")
  local_mocked_bindings(
    fs_require = function(pkgs, purpose = NULL) {
      if ("e1071" %in% pkgs) stop("e1071 missing (mocked)", call. = FALSE)
      invisible(TRUE)
    },
    rfe_perform = function(...) stop("rfe_perform was reached", call. = FALSE)
  )
  d <- data.frame(y = factor(rep(c("a", "b"), 20)), x1 = rnorm(40),
                  x2 = rnorm(40))
  expect_error(fs_recursivefeature(d, "y", seed = 1), "e1071 missing")
})

test_that("rfe_control method, number and functions are validated (C70)", {
  skip_if_not_installed("caret")
  d <- data.frame(y = rnorm(30), x1 = rnorm(30), x2 = rnorm(30))
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = list(method = "cv", number = 1)),
    "'rfe_control\\$number' must be at least 2 for method = 'cv'"
  )
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = list(method = "foo", number = 3)),
    "'rfe_control\\$method' must be one of"
  )
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = list(method = "cv", number = 3,
                                                   functions = list(a = 1))),
    "missing or not a function: summary, fit, pred, rank, selectSize, selectVar"
  )
  bad_set <- caret::lmFuncs
  bad_set$rank <- NULL
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = list(method = "cv", number = 3,
                                                   functions = bad_set)),
    "missing or not a function: rank"
  )
  expect_error(featR:::rfe_validate_train_control(
    list(method = "cv", number = 1)
  ), "'train_control\\$number' must be at least 2")
  # LOOCV / boot with number = 1 stay valid
  expect_silent(featR:::rfe_validate_rfe_control(
    list(method = "boot", number = 1)
  ))
})

test_that("train_control and model_method are validated before the search (C71)", {
  skip_if_not_installed("caret")
  local_mocked_bindings(
    rfe_perform = function(...) stop("rfe_perform was reached", call. = FALSE)
  )
  d <- data.frame(y = rnorm(30), x1 = rnorm(30), x2 = rnorm(30))
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = lm_ctl(),
                        train_control = list(method = "cv"),
                        return_final_model = TRUE, model_method = "lm"),
    "provide 'train_control\\$number'"
  )
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = lm_ctl(),
                        model_method = "nonexistent",
                        return_final_model = TRUE),
    "'model_method' = 'nonexistent' is not in caret's built-in model library"
  )
  # ignored (and so not validated) without a final model
  expect_error(
    fs_recursivefeature(d, "y", rfe_control = lm_ctl(),
                        model_method = "nonexistent"),
    "rfe_perform was reached"
  )
})

test_that("one-level categorical predictors are removed with a warning under handle_categorical = TRUE (C69)", {
  # Previously caret::dummyVars() failed with "contrasts can be applied only
  # to factors with 2 or more levels"; handle_categorical = FALSE already
  # removed such columns as constant.
  skip_if_not_installed("caret")
  skip_on_cran()
  set.seed(11)
  n <- 60
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
                  k = "same", f = factor(rep("z", n)))
  d$y <- d$x1 + rnorm(n, sd = 0.3)
  for (hc in c(TRUE, FALSE)) {
    expect_warning(
      res <- fs_recursivefeature(
        d, "y", handle_categorical = hc, seed = 1,
        rfe_control = list(method = "cv", number = 3,
                           functions = caret::lmFuncs)
      ),
      "constant on the training rows before RFE: k, f"
    )
    expect_false(any(c("k", "f") %in% names(res$scores)))
    expect_true("x1" %in% selected(res))
  }
})

test_that("rfe_control$method is matched case-insensitively like caret", {
  # REGRESSION: the method whitelist rejected 'lgocv', which caret accepts.
  out <- featR:::rfe_validate_rfe_control(list(method = "lgocv", number = 3))
  expect_identical(out$method, "LGOCV")
  expect_error(featR:::rfe_validate_rfe_control(list(method = "nope", number = 3)),
               "must be one of")
})

test_that("a large-offset predictor is not removed as constant", {
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  skip_on_cran()
  set.seed(4)
  n <- 120
  d <- data.frame(ts = 1.7e9 + sample(0:20, n, TRUE), w = rnorm(n))
  d$y <- (d$ts - 1.7e9) + rnorm(n, sd = 0.5)
  expect_no_warning(
    res <- fs_recursivefeature(d, "y", sizes = 1:2,
                               rfe_control = list(method = "cv", number = 3,
                                                  functions = caret::lmFuncs),
                               seed = 1),
    message = "constant"
  )
  expect_true("ts" %in% names(res$scores))
})

test_that("a predictor constant inside one CV fold does not hang RFE", {
  # REGRESSION: a rare indicator (one non-zero row) is constant in the fold
  # that holds that row out; caret's size-1 randomForest fit on it never
  # returned. The guarded function set drops it inside that resample only.
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  skip_on_cran()
  set.seed(1)
  n <- 60
  d <- data.frame(y = factor(sample(c("a", "b"), n, TRUE)), x1 = rnorm(n),
                  x2 = rnorm(n), k = c(1, rep(0, n - 1)))
  res <- fs_recursivefeature(d, "y", rfe_control = list(method = "cv",
                                                        number = 10),
                             seed = 1)
  expect_s3_class(res, "fs_result")
  expect_setequal(names(res$scores), c("x1", "x2", "k"))
})

test_that("rfe_guard_rf_funcs ranks every column and handles all-constant fits", {
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  f <- featR:::rfe_guard_rf_funcs(caret::rfFuncs)
  set.seed(2)
  x <- data.frame(a = rnorm(30), b = 0, c = rnorm(30))
  y <- factor(rep(c("u", "v"), 15))
  obj <- f$fit(x, y, first = TRUE, last = FALSE)
  rk <- f$rank(obj, x, y)
  expect_setequal(rk$var, c("a", "b", "c"))
  expect_identical(rk$var[3L], "b")
  expect_equal(nrow(f$pred(obj, x)), 30L)
  const <- f$fit(x["b"], y, first = FALSE, last = FALSE)
  expect_s3_class(const, "featr_constant_fit")
  p <- f$pred(const, x["b"])
  expect_true(all(c("pred", "u", "v") %in% names(p)))
  expect_identical(f$rank(const, x["b"], y)$var, "b")
  # Non-rf function sets are left untouched.
  expect_identical(featR:::rfe_guard_rf_funcs(caret::lmFuncs), caret::lmFuncs)
})
