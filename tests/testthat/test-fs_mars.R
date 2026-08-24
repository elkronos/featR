# Tests for fs_mars().
#
# All argument validation in fs_mars() (including the 'p' check that mirrors
# fs_split_index()) runs BEFORE fs_require(c("caret", "earth")), so those
# tests need no skips and run everywhere. mars_coerce_response() only needs
# data.table, which featR Imports, so it is tested helper-level without skips
# too. Everything that reaches caret::train()/earth is gated on the caret and
# earth Suggests plus skip_on_cran().

test_that("fs_mars validates arguments before requiring caret/earth", {
  d <- data.frame(y = c(1, 2, 3, 4), x = c(4, 3, 2, 1))

  expect_error(fs_mars(1L, "y"), "'data' must be a data\\.frame")
  expect_error(fs_mars(d[0, ], "y"),
               "'data' must have at least one row and one column")
  expect_error(fs_mars(d, "nope"), "Column 'nope' not found in 'data'")
  expect_error(fs_mars(d, 1), "single non-empty character string")
  expect_error(fs_mars(d, c("y", "x")), "single non-empty character string")

  # 'p' is validated with the same wording fs_split_index() uses
  expect_error(fs_mars(d, "y", p = 0), "'p' must be strictly between 0 and 1")
  expect_error(fs_mars(d, "y", p = 1), "'p' must be strictly between 0 and 1")
  expect_error(fs_mars(d, "y", p = 1.5), "'p' must be strictly between 0 and 1")
  expect_error(fs_mars(d, "y", p = "a"), "'p' must be a single finite number")

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
  expect_error(fs_mars(d, "y", method = 1), "single non-empty character string")
  expect_error(fs_mars(d, "y", search = "nope"), "'search' must be either")
  expect_error(fs_mars(d, "y", number = 1), "'number' must be between 2 and Inf")
  expect_error(fs_mars(d, "y", repeats = 0), "'repeats' must be between 1 and Inf")
  expect_error(fs_mars(d, "y", seed = 1.5), "single whole number")
  expect_error(fs_mars(d, "y", seed = "a"), "'seed' must be a single finite number")
  expect_error(fs_mars(d, "y", sampleSize = 0),
               "'sampleSize' must be between 1 and Inf")
  expect_error(fs_mars(d, "y", show_warnings = "yes"),
               "'show_warnings' must be TRUE or FALSE")
  expect_error(fs_mars(d, "y", verbose = NA), "'verbose' must be TRUE or FALSE")
  expect_error(fs_mars(d, "y", corr_cut = 1.5),
               "'corr_cut' must be between 0 and 1")
  expect_error(fs_mars(d, "y", remove_nzv = NA),
               "'remove_nzv' must be TRUE or FALSE")
  expect_error(fs_mars(d, "y", verbose_iter = "yes"),
               "'verbose_iter' must be TRUE or FALSE")
  expect_error(fs_mars(d, "y", n_cores = 0), "'n_cores' must be between 1 and Inf")
  expect_error(fs_mars(d, "y", tuneLength = 0),
               "'tuneLength' must be between 1 and Inf")
})

test_that("fs_mars signature and defaults are stable", {
  fx <- formals(fs_mars)
  expect_identical(
    names(fx),
    c("data", "responseName", "p", "degree", "nprune", "method", "search",
      "number", "repeats", "seed", "sampleSize", "show_warnings", "verbose",
      "corr_cut", "remove_nzv", "verbose_iter", "n_cores", "tuneLength")
  )
  expect_identical(fx$p, 0.8)
  expect_identical(eval(fx$degree), 1:3)
  expect_identical(eval(fx$nprune), c(5, 10, 15))
  expect_identical(fx$method, "earth")
  expect_identical(fx$search, "grid")
  expect_identical(fx$number, 5)
  expect_identical(fx$repeats, 3)
  # featR never seeds the RNG unless the caller asks for it
  expect_null(fx$seed)
  expect_identical(fx$sampleSize, 10000)
  expect_identical(fx$show_warnings, TRUE)
  expect_identical(fx$verbose, TRUE)
  expect_identical(fx$corr_cut, 0.95)
  expect_identical(fx$remove_nzv, TRUE)
  expect_identical(fx$verbose_iter, FALSE)
  # sequential by default: no cluster is ever created
  expect_identical(fx$n_cores, 1L)
  expect_true("tuneLength" %in% names(fx))
  expect_identical(fx$tuneLength, 10L)
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
    "Coercing character response 'y' to factor"
  )
  expect_s3_class(dt_chr$y, "factor")

  dt_num <- data.table::data.table(y = c(1, 2, 3), x = c(3, 2, 1))
  out_num <- featR:::mars_coerce_response(dt_num, "y", verbose = FALSE)
  expect_true(is.numeric(out_num$y))

  dt_bad <- data.table::data.table(y = as.Date("2024-01-01") + 0:3,
                                   x = c(1, 2, 3, 4))
  expect_error(
    featR:::mars_coerce_response(dt_bad, "y", verbose = FALSE),
    "Response must be numeric \\(regression\\) or factor \\(classification\\)"
  )
})

test_that("regression runs end-to-end on data.frame and on data.table input", {
  skip_if_not_installed("caret")
  skip_if_not_installed("earth")
  skip_on_cran()

  set.seed(2024)
  n <- 120
  df <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  df$y <- 2 * df$x1 - df$x2 + rnorm(n, sd = 0.5)

  res_df <- fs_mars(df, "y", degree = 1, nprune = c(3, 5), number = 3,
                    repeats = 1, seed = 42, verbose = FALSE, n_cores = 1)

  expect_true(all(c("model", "predictions", "metrics", "preprocessing") %in%
                    names(res_df)))
  expect_s3_class(res_df$model, "train")
  expect_named(res_df$metrics, c("RMSE", "MAE", "R2"))
  expect_true(all(vapply(res_df$metrics, is.numeric, logical(1L))))
  expect_gt(length(res_df$predictions), 0L)
  expect_identical(names(res_df$preprocessing), "removed_predictors")
  expect_named(res_df$preprocessing$removed_predictors, c("nzv", "corr"))

  # The regression this suite exists for: fs_split_index() flattens the matrix
  # that caret::createDataPartition(list = FALSE) returns, so subsetting the
  # internal data.table with it must not error. data.table is an Import, so no
  # extra skip is needed for this half of the test.
  dt <- data.table::as.data.table(df)
  res_dt <- fs_mars(dt, "y", degree = 1, nprune = c(3, 5), number = 3,
                    repeats = 1, seed = 42, verbose = FALSE, n_cores = 1)

  expect_s3_class(res_dt$model, "train")
  expect_named(res_dt$metrics, c("RMSE", "MAE", "R2"))
  # same data and same seed: the two input classes must agree
  expect_equal(res_dt$metrics$RMSE, res_df$metrics$RMSE)

  # the caller's data.table is never mutated in place
  expect_identical(names(dt), c("x1", "x2", "x3", "y"))
  expect_identical(nrow(dt), 120L)
})

test_that("the same seed reproduces the regression metrics and leaves the caller's RNG alone", {
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
                  repeats = 1, seed = 7, verbose = FALSE, n_cores = 1)

  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, state_before)

  res2 <- fs_mars(df, "y", degree = 1, nprune = c(3, 5), number = 3,
                  repeats = 1, seed = 7, verbose = FALSE, n_cores = 1)

  expect_equal(res2$metrics, res1$metrics)
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
            repeats = 1, seed = 7, verbose = FALSE, n_cores = 1)
  )

  expect_s3_class(res$model, "train")
  expect_true(all(c("Accuracy", "Kappa") %in% names(res$metrics)))
  expect_true(is.numeric(res$metrics$Accuracy))
  expect_true(is.table(res$confusion_matrix) || is.matrix(res$confusion_matrix))

  # The confusion matrix is built from the (sanitized) response levels, so it
  # is the public evidence that make.names(unique = TRUE) kept the classes apart.
  lev <- colnames(res$confusion_matrix)
  expect_length(lev, 2L)
  expect_identical(lev, make.names(raw_levels, unique = TRUE))
  expect_identical(anyDuplicated(lev), 0L)
  expect_identical(lev, make.names(lev))
})
