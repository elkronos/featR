# Tests for fs_boruta().
#
# All argument validation in fs_boruta() runs before fs_require("Boruta"),
# so validation tests need no skips. Target-content checks and real Boruta
# runs require the suggested Boruta package (plus caret for the cutoff_cor
# pruning path).

test_that("fs_boruta validates arguments before requiring Boruta", {
  d <- data.frame(y = c(1, 2, 3, 4), x = c(4, 3, 2, 1))

  expect_error(fs_boruta("nope", "y"), "'data' must be a data.frame or matrix")
  expect_error(fs_boruta(d, "missing_target"),
               "Column 'missing_target' not found in 'data'")
  expect_error(fs_boruta(d, 1), "single non-empty character string")
  expect_error(fs_boruta(d, "y", cutoff_features = 2.5),
               "'cutoff_features' must be a whole number")
  expect_error(fs_boruta(d, "y", cutoff_features = 0),
               "'cutoff_features' must be between 1 and Inf")
  expect_error(fs_boruta(d, "y", cutoff_cor = 1.5),
               "'cutoff_cor' must be between 0 and 1")
  expect_error(fs_boruta(d, "y", doTrace = -1),
               "'doTrace' must be between 0 and Inf")
  expect_error(fs_boruta(d, "y", maxRuns = 0),
               "'maxRuns' must be between 1 and Inf")
  expect_error(fs_boruta(d, "y", resolve_tentative = "yes"),
               "'resolve_tentative' must be TRUE or FALSE")
})

test_that("fs_boruta signature and defaults are stable", {
  fx <- formals(fs_boruta)
  expect_identical(
    names(fx),
    c("data", "target_var", "seed", "doTrace", "maxRuns",
      "cutoff_features", "cutoff_cor", "resolve_tentative")
  )
  expect_identical(fx$doTrace, 0)
  expect_null(fx$seed)
  expect_null(fx$cutoff_features)
  expect_identical(fx$maxRuns, 250)
  expect_identical(fx$cutoff_cor, 0.7)
  expect_identical(fx$resolve_tentative, TRUE)
})

test_that("boruta_preprocess_predictors coerces supported types and drops target", {
  d <- data.frame(
    y     = 1:5,
    num   = c(1.5, 2.5, 3.5, 4.5, 5.5),
    chr   = letters[1:5],
    lgl   = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    dte   = as.Date("2024-01-01") + 0:4,
    stamp = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:4,
    stringsAsFactors = FALSE
  )

  out <- featR:::boruta_preprocess_predictors(d, "y")

  expect_identical(names(out), c("num", "chr", "lgl", "dte", "stamp"))
  expect_true(is.factor(out$chr))
  expect_true(is.factor(out$lgl))
  expect_true(is.numeric(out$dte))
  expect_identical(out$dte, as.numeric(d$dte))
  expect_true(is.numeric(out$stamp))
})

test_that("boruta_preprocess_predictors rejects unsupported column types", {
  d <- data.frame(y = 1:3)
  d$bad <- complex(real = 1:3, imaginary = 1)
  expect_error(
    featR:::boruta_preprocess_predictors(d, "y"),
    "Unsupported variable types found in columns: bad"
  )
})

test_that("boruta_remove_highly_correlated early paths need no caret", {
  preds <- data.frame(
    a  = c(1, 2, 3, 4, 5),
    b  = c(5, 4, 3, 2, 1),
    f1 = factor(c("u", "v", "u", "v", "u")),
    f2 = factor(c("v", "v", "u", "u", "u"))
  )
  helper <- featR:::boruta_remove_highly_correlated

  # a single selected feature is returned untouched
  expect_identical(helper(preds, "a"), "a")
  # with at most one numeric feature in the selection, nothing is pruned
  expect_identical(helper(preds, c("a", "f1", "f2")), c("a", "f1", "f2"))
  # unknown features are an error
  expect_error(helper(preds, c("a", "zz")), "not found in `predictors`")
  # cutoff_cor is validated even for trivial selections
  expect_error(helper(preds, "a", cutoff_cor = 2),
               "'cutoff_cor' must be between 0 and 1")
})

test_that("fs_boruta validates target content", {
  skip_if_not_installed("Boruta")

  d_chr <- data.frame(y = c("a", "b", "a", "b"), x = c(1, 2, 3, 4),
                      stringsAsFactors = FALSE)
  expect_error(fs_boruta(d_chr, "y"),
               "must be numeric \\(regression\\) or factor")

  d_na_y <- data.frame(y = factor(c("a", NA, "b", "a")), x = c(1, 2, 3, 4))
  expect_error(fs_boruta(d_na_y, "y"), "`target_var` contains missing values")

  d_na_x <- data.frame(y = factor(c("a", "b", "a", "b")), x = c(1, NA, 3, 4))
  expect_error(fs_boruta(d_na_x, "y"), "Predictors contain missing values")
})

test_that("fs_boruta selects informative features reproducibly on a factor target", {
  skip_if_not_installed("Boruta")
  skip_on_cran()

  set.seed(101)
  n <- 120
  y <- factor(rep(c("a", "b"), each = n / 2))
  d <- data.frame(
    y  = y,
    x1 = ifelse(y == "b", 3, 0) + rnorm(n, sd = 0.5),
    x2 = ifelse(y == "b", -2, 2) + rnorm(n, sd = 0.5),
    n1 = rnorm(n),
    n2 = runif(n)
  )

  set.seed(1)
  invisible(stats::runif(1))
  state_before <- .Random.seed

  res <- fs_boruta(d, "y", seed = 42, maxRuns = 15, cutoff_cor = NULL)

  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, state_before)

  expect_named(res, c("selected_features", "boruta_obj"))
  expect_type(res$selected_features, "character")
  expect_s3_class(res$boruta_obj, "Boruta")
  expect_true(all(c("x1", "x2") %in% res$selected_features))
  expect_true(all(res$selected_features %in% setdiff(names(d), "y")))

  res2 <- fs_boruta(d, "y", seed = 42, maxRuns = 15, cutoff_cor = NULL)
  expect_identical(res$selected_features, res2$selected_features)
})

test_that("fs_boruta caps the number of returned features via cutoff_features", {
  skip_if_not_installed("Boruta")
  skip_on_cran()

  set.seed(202)
  n <- 100
  y <- factor(rep(c("a", "b"), each = n / 2))
  d <- data.frame(
    y  = y,
    x1 = ifelse(y == "b", 3, 0) + rnorm(n, sd = 0.4),
    x2 = ifelse(y == "b", -3, 0) + rnorm(n, sd = 0.4),
    n1 = rnorm(n)
  )

  res <- fs_boruta(d, "y", seed = 5, maxRuns = 12, cutoff_features = 1,
                   cutoff_cor = NULL)
  expect_length(res$selected_features, 1L)
  expect_true(res$selected_features %in% c("x1", "x2"))
})

test_that("fs_boruta prunes duplicated features via cutoff_cor", {
  skip_if_not_installed("Boruta")
  skip_if_not_installed("caret")
  skip_on_cran()

  set.seed(303)
  n  <- 100
  y  <- factor(rep(c("a", "b"), each = n / 2))
  x1 <- ifelse(y == "b", 3, 0) + rnorm(n, sd = 0.3)
  d <- data.frame(
    y      = y,
    x1     = x1,
    x1_dup = x1, # perfectly correlated duplicate
    z      = rnorm(n)
  )

  res <- fs_boruta(d, "y", seed = 11, maxRuns = 15, cutoff_cor = 0.9)

  expect_named(res, c("selected_features", "boruta_obj"))
  # of a perfectly correlated pair, at most one member survives pruning
  expect_lte(sum(c("x1", "x1_dup") %in% res$selected_features), 1L)
  # but the informative signal itself is not dropped wholesale
  expect_true(any(c("x1", "x1_dup") %in% res$selected_features))
})
