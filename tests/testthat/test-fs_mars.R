# Tests for fs_mars().
#
# All argument validation in fs_mars() runs BEFORE fs_require(c("caret",
# "earth")), so those tests need no skips and run everywhere. The pure helpers
# (mars_coerce_response(), mars_importance_vector(), mars_predictor_scores())
# only need data.table, which featR Imports, so they are tested helper-level
# without skips too. Everything that reaches caret::train()/earth is gated on
# the caret and earth Suggests plus skip_on_cran(), and kept small (n <= 120,
# 3 folds, 1 repeat, n_cores = 1).

test_that("fs_mars validates arguments before requiring caret/earth", {
  d <- data.frame(y = c(1, 2, 3, 4), x = c(4, 3, 2, 1))

  expect_error(fs_mars(1L, "y"), "'data' must be a data\\.frame")
  expect_error(fs_mars(d[0, ], "y"),
               "'data' must have at least one row and one column")
  expect_error(fs_mars(d, "nope"), "Column 'nope' not found in 'data'")
  expect_error(fs_mars(d, 1), "single non-empty character string")
  expect_error(fs_mars(d, c("y", "x")), "single non-empty character string")

  # the split proportion is 'train_ratio' everywhere now
  expect_error(fs_mars(d, "y", train_ratio = 0),
               "'train_ratio' must be strictly between 0 and 1")
  expect_error(fs_mars(d, "y", train_ratio = 1),
               "'train_ratio' must be strictly between 0 and 1")
  expect_error(fs_mars(d, "y", train_ratio = 1.5),
               "'train_ratio' must be strictly between 0 and 1")
  expect_error(fs_mars(d, "y", train_ratio = "a"),
               "'train_ratio' must be a single finite number")

  expect_error(fs_mars(d, "y", degree = 0),
               "'degree' must be a vector of positive whole numbers")
  expect_error(fs_mars(d, "y", degree = 1.5),
               "'degree' must be a vector of positive whole numbers")
  expect_error(fs_mars(d, "y", degree = numeric(0)),
               "'degree' must be a vector of positive whole numbers")
  expect_error(fs_mars(d, "y", nprune = 1),
               "'nprune' must be a vector of whole numbers >= 2")
  expect_error(fs_mars(d, "y", nprune = c(3, NA)),
               "'nprune' must be a vector of whole numbers >= 2")
  expect_error(fs_mars(d, "y", tuneLength = 0),
               "'tuneLength' must be between 1 and Inf")
  expect_error(fs_mars(d, "y", search = "nope"), "'search' must be either")
  expect_error(fs_mars(d, "y", number = 1), "'number' must be between 2 and Inf")
  expect_error(fs_mars(d, "y", repeats = 0),
               "'repeats' must be between 1 and Inf")
  expect_error(fs_mars(d, "y", sample_size = 0),
               "'sample_size' must be between 1 and Inf")
  expect_error(fs_mars(d, "y", corr_cut = 1.5),
               "'corr_cut' must be between 0 and 1")
  expect_error(fs_mars(d, "y", remove_nzv = NA),
               "'remove_nzv' must be TRUE or FALSE")
  expect_error(fs_mars(d, "y", seed = 1.5), "single whole number")
  expect_error(fs_mars(d, "y", seed = "a"),
               "'seed' must be a single finite number")
  expect_error(fs_mars(d, "y", verbose = NA), "'verbose' must be TRUE or FALSE")
  expect_error(fs_mars(d, "y", n_cores = 0),
               "'n_cores' must be between 1 and Inf")
})

test_that("the arguments dropped in the API pass are rejected by matching", {
  # No skips: R rejects unmatched arguments before the body (and therefore
  # before fs_require()) ever runs.
  d <- data.frame(y = c(1, 2, 3, 4), x = c(4, 3, 2, 1))

  # 'method' is gone: fs_mars() is earth-only
  expect_error(fs_mars(d, "y", method = "earth"), "unused argument")
  # renamed
  expect_error(fs_mars(d, "y", p = 0.8), "unused argument")
  expect_error(fs_mars(d, "y", sampleSize = 100), "unused argument")
  expect_error(fs_mars(d, responseName = "y"), "unused argument")
  # folded into 'verbose'
  expect_error(fs_mars(d, "y", show_warnings = TRUE), "unused argument")
  expect_error(fs_mars(d, "y", verbose_iter = TRUE), "unused argument")
})

test_that("fs_mars signature and defaults are stable", {
  fx <- formals(fs_mars)
  expect_identical(
    names(fx),
    c("data", "target", "train_ratio", "degree", "nprune", "tuneLength",
      "search", "number", "repeats", "sample_size", "corr_cut", "remove_nzv",
      "seed", "verbose", "n_cores")
  )
  expect_identical(fx$train_ratio, 0.8)
  expect_identical(eval(fx$degree), 1:3)
  expect_identical(eval(fx$nprune), c(5, 10, 15))
  expect_identical(fx$tuneLength, 10L)
  expect_identical(fx$search, "grid")
  expect_identical(fx$number, 5)
  expect_identical(fx$repeats, 3)
  expect_identical(fx$sample_size, 10000)
  expect_identical(fx$corr_cut, 0.95)
  expect_identical(fx$remove_nzv, TRUE)
  # featR never seeds the RNG unless the caller asks for it
  expect_null(fx$seed)
  # quiet by default
  expect_identical(fx$verbose, FALSE)
  # sequential by default: no cluster is ever created
  expect_identical(fx$n_cores, 1L)
})

test_that("mars_coerce_response sanitizes factor levels without merging classes", {
  # mars_coerce_response() runs after fs_require(c("caret", "earth")) inside
  # fs_mars(), but it only needs data.table (an Import), so the level-merging
  # regression is pinned down helper-level and runs everywhere.
  dt <- data.table::data.table(
    y = factor(c("class 1", "class.1", "class 1", "class.1"),
               levels = c("class 1", "class.1")),
    x = c(1, 2, 3, 4)
  )
  out <- featR:::mars_coerce_response(dt, "y", make_factor_names = TRUE,
                                      verbose = FALSE)
  # Both labels sanitize to "class.1"; make.names(unique = TRUE) must keep them
  # apart. Which of the two gets the ".1" suffix depends on level ordering and
  # locale collation, so assert the invariant (no merging, a 1:1 relabeling)
  # rather than a specific assignment.
  expect_length(levels(out$y), 2L)
  expect_setequal(levels(out$y), c("class.1", "class.1.1"))
  labs <- as.character(out$y)
  expect_identical(labs[1L], labs[3L]) # both rows were "class 1"
  expect_identical(labs[2L], labs[4L]) # both rows were "class.1"
  expect_false(labs[1L] == labs[2L])   # the two classes stayed distinct
  expect_length(unique(out$y), 2L)

  dt_chr <- data.table::data.table(y = c("a", "b", "a", "b"), x = c(1, 2, 3, 4))
  expect_message(
    featR:::mars_coerce_response(dt_chr, "y", verbose = TRUE),
    "Coercing character target 'y' to factor"
  )
  expect_s3_class(dt_chr$y, "factor")

  dt_num <- data.table::data.table(y = c(1, 2, 3), x = c(3, 2, 1))
  out_num <- featR:::mars_coerce_response(dt_num, "y", verbose = FALSE)
  expect_true(is.numeric(out_num$y))

  dt_bad <- data.table::data.table(y = as.Date("2024-01-01") + 0:3,
                                   x = c(1, 2, 3, 4))
  expect_error(
    featR:::mars_coerce_response(dt_bad, "y", verbose = FALSE),
    "Target must be numeric \\(regression\\) or factor \\(classification\\)"
  )
})

test_that("mars_importance_vector reads caret's importance table", {
  vi <- list(importance = data.frame(Overall = c(3, 0),
                                     row.names = c("a", "b")))
  expect_identical(featR:::mars_importance_vector(vi), c(a = 3, b = 0))

  # the bare data.frame is accepted too
  expect_identical(featR:::mars_importance_vector(vi$importance),
                   c(a = 3, b = 0))

  # several numeric columns (one per class) are averaged
  vi_multi <- list(importance = data.frame(cls_a = c(2, 4), cls_b = c(4, 8),
                                           row.names = c("p", "q")))
  expect_identical(featR:::mars_importance_vector(vi_multi), c(p = 3, q = 6))

  expect_null(featR:::mars_importance_vector(NULL))
  expect_null(featR:::mars_importance_vector(list(importance = NULL)))
})

test_that("mars_predictor_scores maps encoded names back onto predictors", {
  # caret one-hot encodes factors before earth sees them, so "grpb"/"grpc"
  # belong to the predictor "grp" and the largest of the two is kept.
  imp <- c(x1 = 4, grpb = 2, grpc = 7, unrelated = 1)
  out <- featR:::mars_predictor_scores(imp, c("x1", "grp", "x2"))

  expect_identical(names(out), c("x1", "grp", "x2"))
  expect_identical(out[["x1"]], 4)
  expect_identical(out[["grp"]], 7)
  expect_identical(out[["x2"]], 0) # never used by the model

  # no importance at all: every candidate scores 0
  expect_identical(featR:::mars_predictor_scores(NULL, c("a", "b")),
                   c(a = 0, b = 0))
})

test_that("regression returns an fs_result end-to-end on data.frame and data.table", {
  skip_if_not_installed("caret")
  skip_if_not_installed("earth")
  skip_on_cran()

  set.seed(2024)
  n <- 120
  df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  df$y <- 2 * df$x1 - df$x2 + rnorm(n, sd = 0.5)

  res_df <- fs_mars(df, "y", degree = 1, nprune = c(3, 5), number = 3,
                    repeats = 1, seed = 42, n_cores = 1)

  expect_s3_class(res_df, "fs_result")
  expect_identical(res_df$method, "mars")
  expect_identical(res_df$task, "regression")
  expect_s3_class(res_df$model, "train")
  expect_false(is.null(res_df$call))

  # documented details, all present and in order
  expect_named(res_df$details,
               c("predictions", "metrics", "confusion_matrix", "varimp",
                 "removed_predictors", "train_index", "test_data",
                 "n_features"))
  expect_named(res_df$details$metrics, c("RMSE", "MAE", "R2"))
  expect_true(all(vapply(res_df$details$metrics, is.numeric, logical(1L))))
  expect_named(res_df$details$removed_predictors, c("nzv", "corr"))
  expect_null(res_df$details$confusion_matrix) # regression
  expect_type(res_df$details$train_index, "integer")
  expect_identical(res_df$details$n_features, 3L)
  expect_gt(length(res_df$details$predictions), 0L)
  expect_identical(length(res_df$details$predictions),
                   nrow(res_df$details$test_data))

  # selection is read off the fitted model
  expect_type(res_df$selected, "character")
  expect_true(all(res_df$selected %in% c("x1", "x2", "x3")))
  expect_true("x1" %in% res_df$selected) # the dominant term
  expect_true(is.numeric(res_df$scores))
  expect_setequal(names(res_df$scores), c("x1", "x2", "x3"))
  expect_true(all(res_df$scores[res_df$selected] > 0))
  expect_true(all(res_df$scores >= 0))

  expect_output(print(res_df), "mars")

  # The regression this suite exists for: fs_split_index() flattens the matrix
  # that caret::createDataPartition(list = FALSE) returns, so subsetting the
  # internal data.table with it must not error. data.table is an Import, so no
  # extra skip is needed for this half of the test.
  dt <- data.table::as.data.table(df)
  res_dt <- fs_mars(dt, "y", degree = 1, nprune = c(3, 5), number = 3,
                    repeats = 1, seed = 42, n_cores = 1)

  expect_s3_class(res_dt, "fs_result")
  expect_s3_class(res_dt$model, "train")
  expect_named(res_dt$details$metrics, c("RMSE", "MAE", "R2"))
  # same data and same seed: the two input classes must agree
  expect_equal(res_dt$details$metrics$RMSE, res_df$details$metrics$RMSE)
  expect_identical(res_dt$selected, res_df$selected)

  # the caller's data.table is never mutated in place
  expect_identical(names(dt), c("x1", "x2", "x3", "y"))
  expect_identical(nrow(dt), 120L)
})

test_that("the same seed reproduces the result and leaves the caller's RNG alone", {
  skip_if_not_installed("caret")
  skip_if_not_installed("earth")
  skip_on_cran()

  set.seed(909)
  n <- 100
  df <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  df$y <- df$x1 - 0.5 * df$x2 + rnorm(n, sd = 0.4)

  set.seed(1)
  invisible(stats::runif(1))
  state_before <- .Random.seed

  res1 <- fs_mars(df, "y", degree = 1, nprune = c(3, 5), number = 3,
                  repeats = 1, seed = 7, n_cores = 1)

  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, state_before)

  res2 <- fs_mars(df, "y", degree = 1, nprune = c(3, 5), number = 3,
                  repeats = 1, seed = 7, n_cores = 1)

  expect_equal(res2$details$metrics, res1$details$metrics)
  expect_identical(res2$selected, res1$selected)
  expect_equal(res2$scores, res1$scores)
})

test_that("binary classification with non-syntactic labels runs and keeps both classes", {
  skip_if_not_installed("caret")
  skip_if_not_installed("earth")
  skip_on_cran()

  set.seed(77)
  n <- 140
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  raw_levels <- c("class 1", "class-2")
  y <- factor(ifelse(1.5 * x1 - x2 > 0, raw_levels[1L], raw_levels[2L]),
              levels = raw_levels)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  # The classes are separable by construction, so earth's internal glm emits
  # "fitted probabilities numerically 0 or 1" warnings; they are expected here
  # and only add noise to the check log.
  res <- suppressWarnings(
    fs_mars(df, "y", degree = 1, nprune = c(3, 5), number = 3,
            repeats = 1, seed = 7, n_cores = 1)
  )

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "mars")
  expect_identical(res$task, "classification")
  expect_s3_class(res$model, "train")
  expect_true(all(c("Accuracy", "Kappa") %in% names(res$details$metrics)))
  expect_true(is.numeric(res$details$metrics$Accuracy))

  cm <- res$details$confusion_matrix
  expect_true(is.table(cm) || is.matrix(cm))

  # The confusion matrix is built from the (sanitized) response levels, so it
  # is the public evidence that make.names(unique = TRUE) kept the classes apart.
  lev <- colnames(cm)
  expect_length(lev, 2L)
  expect_setequal(lev, make.names(raw_levels, unique = TRUE))
  expect_identical(anyDuplicated(lev), 0L)
  expect_identical(lev, make.names(lev))

  expect_true(all(res$selected %in% c("x1", "x2")))
  expect_setequal(names(res$scores), c("x1", "x2"))
  expect_output(print(res), "classification")
})
