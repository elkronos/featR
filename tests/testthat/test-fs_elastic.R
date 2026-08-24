# Tests for fs_elastic().
#
# Argument-type validation in fs_elastic() runs BEFORE
# fs_require(c("caret", "glmnet", "Matrix")), so those tests need no skips and
# run everywhere. Data-content checks (task inference, zero-variance
# detection, PCA sizing) happen AFTER fs_require(), so they are covered
# helper-level via featR::: (which needs no Suggests) and again through the
# public API in gated tests. Anything that reaches caret::train()/glmnet is
# gated on the caret/glmnet/Matrix Suggests, plus skip_on_cran() when a model
# is actually fitted; the PCA path additionally needs irlba.

test_that("fs_elastic validates argument types before requiring caret/glmnet/Matrix", {
  d <- data.frame(y = c(1, 2, 3, 4), x1 = c(4, 3, 2, 1))

  expect_error(fs_elastic(1L, y ~ x1), "'data' must be a data\\.frame")
  expect_error(fs_elastic(d[0, ], y ~ x1),
               "'data' must have at least one row and one column")
  expect_error(fs_elastic(d, "y ~ x1"), "'formula' must be a formula")
  expect_error(fs_elastic(d, y ~ x1, alpha_seq = "a"),
               "'alpha_seq' must be a numeric vector with values in \\[0, 1\\]")
  expect_error(fs_elastic(d, y ~ x1, alpha_seq = numeric(0)),
               "values in \\[0, 1\\]")
  expect_error(fs_elastic(d, y ~ x1, alpha_seq = c(0.5, NA)),
               "values in \\[0, 1\\]")
  expect_error(fs_elastic(d, y ~ x1, alpha_seq = 1.5), "values in \\[0, 1\\]")
  expect_error(fs_elastic(d, y ~ x1, alpha_seq = -0.1), "values in \\[0, 1\\]")
  expect_error(fs_elastic(d, y ~ x1, lambda_seq = -1),
               "'lambda_seq' must be a numeric vector of non-negative values")
  expect_error(fs_elastic(d, y ~ x1, lambda_seq = numeric(0)),
               "non-negative values")
  expect_error(fs_elastic(d, y ~ x1, lambda_seq = c(1, NA)),
               "non-negative values")
  expect_error(fs_elastic(d, y ~ x1, metric = 1),
               "'metric' must be a single non-empty character string")
  expect_error(fs_elastic(d, y ~ x1, use_pca = NA),
               "'use_pca' must be TRUE or FALSE")
  expect_error(fs_elastic(d, y ~ x1, verbose = "yes"),
               "'verbose' must be TRUE or FALSE")
  expect_error(fs_elastic(d, y ~ x1, seed = 1.5), "single whole number")
  expect_error(fs_elastic(d, y ~ x1, seed = "a"),
               "'seed' must be a single finite number")
  expect_error(fs_elastic(d, y ~ x1, cores = 0),
               "'cores' must be between 1 and Inf")
  expect_error(fs_elastic(d, y ~ x1, cores = 1.5),
               "'cores' must be a whole number")
})

test_that("fs_elastic signature and defaults are stable", {
  fx <- formals(fs_elastic)
  expect_identical(
    names(fx),
    c("data", "formula", "alpha_seq", "lambda_seq", "trControl", "metric",
      "use_pca", "nPCs", "cores", "verbose", "seed")
  )
  expect_identical(eval(fx$alpha_seq), seq(0, 1, by = 0.1))
  expect_identical(eval(fx$lambda_seq), 10^seq(-3, 3, length.out = 100))
  expect_null(fx$trControl)
  expect_null(fx$metric)
  expect_identical(fx$use_pca, FALSE)
  expect_null(fx$nPCs)
  # sequential by default: no cluster is ever created
  expect_identical(fx$cores, 1L)
  expect_identical(fx$verbose, TRUE)
  # featR never seeds the RNG unless the caller asks for it
  expect_null(fx$seed)
})

test_that("elastic_infer_task coerces responses and flags 2-value numerics (helper-level)", {
  # elastic_infer_task() runs after fs_require() inside fs_elastic(), so these
  # checks are unreachable through the public API without caret/glmnet/Matrix;
  # test the helper directly so they run everywhere.
  reg <- featR:::elastic_infer_task(c(1.5, 2.5, 3.5, 4.5))
  expect_identical(reg$task, "regression")
  expect_identical(reg$metric, "RMSE")
  expect_true(is.numeric(reg$y))

  expect_warning(
    two_valued <- featR:::elastic_infer_task(c(0, 1, 0, 1)),
    "treated as regression"
  )
  expect_identical(two_valued$task, "regression")
  expect_true(is.numeric(two_valued$y))

  expect_message(
    lgl <- featR:::elastic_infer_task(c(TRUE, FALSE, TRUE, FALSE)),
    "Logical response converted to a two-level factor"
  )
  expect_identical(lgl$task, "classification")
  expect_identical(lgl$metric, "Accuracy")
  expect_s3_class(lgl$y, "factor")
  expect_identical(levels(lgl$y), c("FALSE", "TRUE"))

  chr <- featR:::elastic_infer_task(c("a", "b", "a", "b"))
  expect_identical(chr$task, "classification")
  expect_s3_class(chr$y, "factor")

  fac <- featR:::elastic_infer_task(factor(c("a", "b", "a")))
  expect_identical(fac$task, "classification")
  expect_identical(fac$metric, "Accuracy")

  expect_error(featR:::elastic_infer_task(factor(c("a", "a"))),
               "Classification outcome must have at least 2 levels")
  expect_error(featR:::elastic_infer_task(as.Date("2024-01-01") + 0:3),
               "Unsupported response type")
})

test_that("elastic_zero_sd_cols names constant columns (helper-level)", {
  m <- cbind(a = c(1, 2, 3, 4), b = c(5, 5, 5, 5), c = c(2, 4, 6, 8))
  expect_identical(featR:::elastic_zero_sd_cols(m), "b")

  # unnamed columns fall back to positional V-names
  m_unnamed <- matrix(c(1, 2, 3, 4, 7, 7, 7, 7), nrow = 4L)
  expect_identical(featR:::elastic_zero_sd_cols(m_unnamed), "V2")

  # with fewer than two rows the variance is undefined, so nothing is reported
  expect_identical(featR:::elastic_zero_sd_cols(m[1, , drop = FALSE]),
                   character(0))
})

test_that("elastic_check_variance errors and names the offending columns", {
  ok <- cbind(a = c(1, 2, 3), b = c(3, 5, 9))
  expect_null(featR:::elastic_check_variance(ok, "model training"))

  one_bad <- cbind(a = c(1, 2, 3, 4), bad = c(5, 5, 5, 5))
  expect_error(
    featR:::elastic_check_variance(one_bad, "model training"),
    "Zero-variance predictor column detected before model training: 'bad'"
  )
  expect_error(featR:::elastic_check_variance(one_bad, "model training"),
               "Remove constant columns and retry")

  many_bad <- cbind(
    a = as.numeric(1:8),
    matrix(1, nrow = 8L, ncol = 7L,
           dimnames = list(NULL, paste0("z", 1:7)))
  )
  expect_error(
    featR:::elastic_check_variance(many_bad, "PCA with scaling"),
    "Zero-variance predictor columns detected before PCA with scaling"
  )
  # only the first five names are shown, the rest are counted
  expect_error(featR:::elastic_check_variance(many_bad, "PCA with scaling"),
               "\\(and 2 more\\)")
})

test_that("elastic_pca requires nPCs strictly less than min(dim(x))", {
  x <- matrix(as.numeric(1:40), nrow = 10L, ncol = 4L)

  expect_error(featR:::elastic_pca(x, use_pca = TRUE, nPCs = NULL),
               "Please set a positive 'nPCs' when 'use_pca' is TRUE")
  expect_error(featR:::elastic_pca(x, use_pca = TRUE, nPCs = 0),
               "'nPCs' must be between 1 and Inf")
  # nPCs equal to min(dim(x)) is rejected: irlba::prcomp_irlba() needs strictly less
  expect_error(featR:::elastic_pca(x, use_pca = TRUE, nPCs = 4),
               "strictly less than min\\(nrow\\(x\\), ncol\\(x\\)\\)")
  expect_error(featR:::elastic_pca(x, use_pca = TRUE, nPCs = 5),
               "strictly less than min\\(nrow\\(x\\), ncol\\(x\\)\\)")

  # use_pca = FALSE is a pass-through that never touches irlba
  passthrough <- featR:::elastic_pca(x, use_pca = FALSE)
  expect_identical(passthrough$x, x)
  expect_null(passthrough$pca)
})

test_that("data-content checks surface through the public API", {
  skip_if_not_installed("caret")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")

  d_const <- data.frame(
    y  = as.numeric(1:20),
    x1 = as.numeric(20:1),
    x2 = rep(1, 20)
  )
  expect_error(
    fs_elastic(d_const, y ~ x1 + x2, alpha_seq = 0.5,
               lambda_seq = c(0.1, 1), cores = 1, verbose = FALSE),
    "Zero-variance predictor column detected before model training: 'x2'"
  )

  d_date <- data.frame(
    y  = as.Date("2024-01-01") + 0:9,
    x1 = as.numeric(1:10),
    x2 = as.numeric(c(2, 4, 1, 7, 3, 9, 5, 8, 6, 10))
  )
  expect_error(
    fs_elastic(d_date, y ~ x1 + x2, alpha_seq = 0.5,
               lambda_seq = c(0.1, 1), cores = 1, verbose = FALSE),
    "Unsupported response type"
  )
})

test_that("the PCA path validates nPCs through the public API", {
  skip_if_not_installed("caret")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("irlba")

  set.seed(505)
  d <- data.frame(y = rnorm(20), x1 = rnorm(20), x2 = rnorm(20))

  expect_error(
    fs_elastic(d, y ~ x1 + x2, alpha_seq = 0.5, lambda_seq = c(0.1, 1),
               use_pca = TRUE, nPCs = NULL, cores = 1, verbose = FALSE),
    "Please set a positive 'nPCs' when 'use_pca' is TRUE"
  )
  # the predictor matrix is 20 x 2, so nPCs = 2 is not strictly less than min(dim)
  expect_error(
    fs_elastic(d, y ~ x1 + x2, alpha_seq = 0.5, lambda_seq = c(0.1, 1),
               use_pca = TRUE, nPCs = 2, cores = 1, verbose = FALSE),
    "strictly less than min\\(nrow\\(x\\), ncol\\(x\\)\\)"
  )
})

test_that("regression smoke run returns the documented elements", {
  skip_if_not_installed("caret")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_on_cran()

  set.seed(404)
  n <- 90
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- 2 * d$x1 - d$x2 + rnorm(n, sd = 0.5)
  form <- y ~ x1 + x2 + x3
  alphas <- 0.5

  ctrl <- caret::trainControl(method = "cv", number = 3)

  set.seed(1)
  invisible(stats::runif(1))
  state_before <- .Random.seed

  res <- fs_elastic(
    data       = d,
    formula    = form,
    alpha_seq  = alphas,
    lambda_seq = c(0.01, 0.1, 1),
    trControl  = ctrl,
    cores      = 1,
    verbose    = FALSE,
    seed       = 42
  )

  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, state_before)

  expect_named(
    res,
    c("coef", "best_alpha", "best_lambda", "metric_name", "metric_value",
      "task", "full_model", "pca_model", "use_pca", "formula")
  )
  expect_identical(res$task, "regression")
  expect_identical(res$metric_name, "RMSE")
  expect_false(res$use_pca)
  expect_null(res$pca_model)
  expect_identical(res$formula, form)
  expect_s3_class(res$full_model, "train")

  expect_length(res$best_alpha, 1L)
  expect_true(res$best_alpha %in% alphas)
  expect_true(is.numeric(res$best_lambda))
  expect_length(res$best_lambda, 1L)
  expect_true(is.numeric(res$metric_value))
  expect_length(res$metric_value, 1L)

  # intercept plus the three predictors
  expect_true(nrow(res$coef) == 4L)
})
