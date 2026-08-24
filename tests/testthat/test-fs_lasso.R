# Tests for fs_lasso(). Validation runs before fs_require() in the source, so
# every validation test below is unconditional; anything that reaches
# glmnet::cv.glmnet() is gated on the 'glmnet' and 'Matrix' Suggests.

test_that("validation rejects bad x/y/alpha/nfolds before any Suggests are needed", {
  x <- data.frame(x1 = 1:10, x2 = 10:1)
  y <- as.numeric(1:10)
  expect_error(fs_lasso(list(), y), "must be a data\\.frame or matrix")
  expect_error(fs_lasso(x, letters[1:10]), "'y' must be a numeric vector")
  expect_error(fs_lasso(x, c(1:9, NA)), "non-finite values")
  expect_error(fs_lasso(x, as.numeric(1:9)), "same number of rows")
  expect_error(fs_lasso(x, y, alpha = 0), "numeric value in \\(0, 1\\]")
  expect_error(fs_lasso(x, y, alpha = 1.5), "numeric value in \\(0, 1\\]")
  expect_error(fs_lasso(x, y, nfolds = 1), "between 2 and Inf")
  expect_error(fs_lasso(x, y, seed = 1.5), "single whole number")
  expect_error(fs_lasso(x, y, return_model = "yes"), "TRUE or FALSE")
})

test_that("custom_folds with a double NA gives a clear error, not a TRUE/FALSE crash", {
  x <- data.frame(x1 = 1:9, x2 = 9:1)
  y <- as.numeric(1:9)
  bad <- c(1, 2, NA, 1, 2, 3, 1, 2, 3)  # double vector with a genuine NA

  cnd <- tryCatch(fs_lasso(x, y, nfolds = 3, custom_folds = bad),
                  error = identity)
  expect_s3_class(cnd, "error")
  expect_match(conditionMessage(cnd), "fold IDs must be complete", fixed = TRUE)
  expect_false(grepl("missing value where TRUE/FALSE needed",
                     conditionMessage(cnd), fixed = TRUE))
})

test_that("gapped, oversized, mistyped, or misaligned custom_folds all error", {
  x <- data.frame(x1 = 1:9, x2 = 9:1)
  y <- as.numeric(1:9)

  # Fold 2 never appears: now a hard error (upgraded from a warning)
  expect_error(fs_lasso(x, y, nfolds = 3, custom_folds = rep(c(1L, 3L), length.out = 9)),
               "leaves some folds")
  expect_error(fs_lasso(x, y, nfolds = 3, custom_folds = rep(1L, 9)),
               "leaves some folds")
  expect_error(fs_lasso(x, y, nfolds = 3, custom_folds = c(rep(1:3, 2), 1L, 2L, 4L)),
               "greater than 'nfolds'")
  expect_error(fs_lasso(x, y, nfolds = 3, custom_folds = rep(1:3, 5)),
               "same length as 'y'")
  expect_error(fs_lasso(x, y, nfolds = 3, custom_folds = c(1.5, rep(1:2, 4))),
               "must be an integer vector")
  expect_error(fs_lasso(x, y, nfolds = 3, custom_folds = c(0L, rep(1:2, 4))),
               "invalid IDs")
})

test_that("error messages do not carry a legacy 'Error: ' prefix", {
  x <- data.frame(x1 = 1:8, x2 = 8:1)
  bad_calls <- list(
    function() fs_lasso(x, letters[1:8]),
    function() fs_lasso(x, as.numeric(1:8), alpha = 2),
    function() fs_lasso(x, as.numeric(1:8), nfolds = 3,
                        custom_folds = rep(c(1L, 3L), 4))
  )
  for (call_fn in bad_calls) {
    cnd <- tryCatch(call_fn(), error = identity)
    expect_s3_class(cnd, "error")
    expect_false(grepl("^Error: ", conditionMessage(cnd)))
  }
})

test_that("data.frame with scattered NAs runs end-to-end with one leakage warning (NA regression)", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")

  set.seed(101)
  n <- 90
  x <- data.frame(
    x1  = rnorm(n),
    x2  = rnorm(n),
    x3  = rnorm(n),
    cat = sample(c("a", "b", "c"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  y <- 2 * x$x1 - 3 * x$x2 + rnorm(n, sd = 0.5)
  # Scatter NAs: these rows must survive to imputation, not be dropped by
  # model.matrix() (the old path died with a "different numbers of rows" error)
  x$x1[c(3, 17, 40)] <- NA
  x$x2[c(8, 55)] <- NA
  x$x3[c(1, 30, 62, 88)] <- NA

  expect_warning(
    res <- fs_lasso(x, y, nfolds = 3, seed = 7),
    "imputed with column means"
  )
  expect_named(res, c("importance", "lambda_min", "lambda_1se"))
  expect_s3_class(res$importance, "data.frame")
  expect_identical(names(res$importance),
                   c("Variable", "Coefficient", "AbsCoefficient"))
  expect_true(is.numeric(res$lambda_min) && length(res$lambda_min) == 1L)
  expect_true(is.numeric(res$lambda_1se) && length(res$lambda_1se) == 1L)
  # All rows were kept: numeric columns and expanded factor levels all present
  expect_true(all(c("x1", "x2", "x3") %in% res$importance$Variable))
})

test_that("an all-NA predictor column is an error naming the column", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")

  x <- data.frame(x1 = as.numeric(1:20), allna = rep(NA_real_, 20))
  y <- as.numeric(20:1)
  expect_error(fs_lasso(x, y, nfolds = 3),
               "entirely missing \\(all NA\\).*'allna'")
})

test_that("a non-numeric matrix is rejected with guidance", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")

  xm <- matrix(letters[1:20], nrow = 10)
  expect_error(fs_lasso(xm, as.numeric(1:10), nfolds = 3),
               "Non-numeric matrices are not supported")
})

test_that("gaussian smoke: sorted importance, documented structure, model opt-in", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_on_cran()

  set.seed(202)
  n <- 120
  x <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  y <- 3 * x$x1 - 2 * x$x2 + rnorm(n, sd = 0.5)

  res <- fs_lasso(x, y, nfolds = 3, seed = 11)
  expect_named(res, c("importance", "lambda_min", "lambda_1se"))
  expect_false("model" %in% names(res))

  imp <- res$importance
  expect_identical(imp$AbsCoefficient,
                   sort(imp$AbsCoefficient, decreasing = TRUE))
  expect_identical(rownames(imp), as.character(seq_len(nrow(imp))))
  expect_identical(imp$AbsCoefficient, abs(imp$Coefficient))
  # The dominant true signal should top the ranking
  expect_identical(imp$Variable[1L], "x1")

  res_model <- fs_lasso(x, y, nfolds = 3, seed = 11, return_model = TRUE)
  expect_named(res_model, c("importance", "lambda_min", "lambda_1se", "model"))
  expect_s3_class(res_model$model, "cv.glmnet")
})

test_that("valid custom_folds (as doubles) are accepted end-to-end", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_on_cran()

  set.seed(404)
  n <- 60
  x <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- x$x1 + rnorm(n, sd = 0.3)
  folds <- as.numeric(rep(1:3, length.out = n))

  res <- fs_lasso(x, y, nfolds = 3, custom_folds = folds)
  expect_named(res, c("importance", "lambda_min", "lambda_1se"))
  expect_true(is.finite(res$lambda_min))
})

test_that("same seed reproduces results and the caller's RNG state is untouched", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_on_cran()

  set.seed(303)
  n <- 100
  x <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  y <- x$x1 - 2 * x$x3 + rnorm(n, sd = 0.4)

  set.seed(777)
  rng_before <- .Random.seed
  res1 <- fs_lasso(x, y, nfolds = 3, seed = 99)
  expect_identical(.Random.seed, rng_before)

  res2 <- fs_lasso(x, y, nfolds = 3, seed = 99)
  expect_identical(res1$lambda_min, res2$lambda_min)
  expect_identical(res1$lambda_1se, res2$lambda_1se)
  expect_identical(res1$importance, res2$importance)
})
