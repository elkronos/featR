# Tests for fs_lasso(). Argument validation runs before fs_require() in the
# source, so every validation test below is unconditional; anything that builds
# the design matrix or reaches glmnet::cv.glmnet() is gated on the 'glmnet' and
# 'Matrix' Suggests, plus skip_on_cran() when a model is actually fitted.

lasso_toy_data <- function(n = 10L) {
  data.frame(
    y  = as.numeric(seq_len(n)),
    x1 = as.numeric(seq_len(n)),
    x2 = as.numeric(rev(seq_len(n)))
  )
}

test_that("fs_lasso signature and defaults are stable", {
  fx <- formals(fs_lasso)
  expect_identical(
    names(fx),
    c("data", "target", "alpha", "nfolds", "standardize", "custom_folds",
      "impute", "return_model", "seed", "verbose", "parallel", "n_cores")
  )
  expect_identical(fx$alpha, 1)
  expect_identical(fx$nfolds, 5)
  expect_identical(fx$standardize, TRUE)
  expect_null(fx$custom_folds)
  # missing values are an error unless the caller opts into imputation
  expect_identical(eval(fx$impute), c("none", "mean"))
  expect_identical(fx$return_model, FALSE)
  # featR never seeds the RNG unless the caller asks for it
  expect_null(fx$seed)
  expect_identical(fx$verbose, FALSE)
  # sequential by default: no cluster is ever created
  expect_identical(fx$parallel, FALSE)
  expect_identical(fx$n_cores, 2L)
})

test_that("validation rejects bad data/target/alpha/nfolds before any Suggests are needed", {
  d <- lasso_toy_data()

  expect_error(fs_lasso(list(), "y"), "must be a data\\.frame or matrix")
  expect_error(fs_lasso(d[0, ], "y"),
               "must have at least one row and one column")
  expect_error(fs_lasso(d, 1), "'target' must be a single non-empty character string")
  expect_error(fs_lasso(d, "nope"), "Column 'nope' not found in 'data'")
  expect_error(fs_lasso(data.frame(y = as.numeric(1:5)), "y"),
               "at least one predictor column")

  d_chr <- d
  d_chr$y <- letters[seq_len(nrow(d_chr))]
  expect_error(fs_lasso(d_chr, "y"), "must be numeric")

  d_na <- d
  d_na$y[3L] <- NA
  expect_error(fs_lasso(d_na, "y"), "non-finite values")

  expect_error(fs_lasso(d, "y", alpha = 0), "numeric value in \\(0, 1\\]")
  expect_error(fs_lasso(d, "y", alpha = 1.5), "numeric value in \\(0, 1\\]")
  # glmnet::cv.glmnet() refuses nfolds < 3, so featR rejects it up front rather
  # than letting glmnet complain about an argument the caller did not set.
  expect_error(fs_lasso(d, "y", nfolds = 1), "between 3 and Inf")
  expect_error(fs_lasso(d, "y", nfolds = 2), "between 3 and Inf")
  expect_error(fs_lasso(d, "y", seed = 1.5), "single whole number")
  expect_error(fs_lasso(d, "y", return_model = "yes"), "TRUE or FALSE")
  expect_error(fs_lasso(d, "y", standardize = NA), "TRUE or FALSE")
  expect_error(fs_lasso(d, "y", impute = "median"), "should be one of")
})

test_that("a non-numeric matrix is rejected with guidance", {
  xm <- matrix(letters[1:20], nrow = 10)
  expect_error(fs_lasso(xm, "V1"), "Non-numeric matrices are not supported")
})

test_that("custom_folds with a double NA gives a clear error, not a TRUE/FALSE crash", {
  d <- lasso_toy_data(9L)
  bad <- c(1, 2, NA, 1, 2, 3, 1, 2, 3)  # double vector with a genuine NA

  cnd <- tryCatch(fs_lasso(d, "y", nfolds = 3, custom_folds = bad),
                  error = identity)
  expect_s3_class(cnd, "error")
  expect_match(conditionMessage(cnd), "fold IDs must be complete", fixed = TRUE)
  expect_false(grepl("missing value where TRUE/FALSE needed",
                     conditionMessage(cnd), fixed = TRUE))
})

test_that("gapped, oversized, mistyped, or misaligned custom_folds all error", {
  d <- lasso_toy_data(9L)

  # Fold 2 never appears: a hard error, not a warning
  expect_error(fs_lasso(d, "y", nfolds = 3,
                        custom_folds = rep(c(1L, 3L), length.out = 9)),
               "leaves some folds")
  expect_error(fs_lasso(d, "y", nfolds = 3, custom_folds = rep(1L, 9)),
               "leaves some folds")
  expect_error(fs_lasso(d, "y", nfolds = 3,
                        custom_folds = c(rep(1:3, 2), 1L, 2L, 4L)),
               "greater than 'nfolds'")
  expect_error(fs_lasso(d, "y", nfolds = 3, custom_folds = rep(1:3, 5)),
               "one entry per row")
  expect_error(fs_lasso(d, "y", nfolds = 3,
                        custom_folds = c(1.5, rep(1:2, 4))),
               "must be an integer vector")
  expect_error(fs_lasso(d, "y", nfolds = 3, custom_folds = c(0L, rep(1:2, 4))),
               "invalid IDs")
})

test_that("error messages do not carry a legacy 'Error: ' prefix", {
  d <- lasso_toy_data(8L)
  d_chr <- d
  d_chr$y <- letters[1:8]

  bad_calls <- list(
    function() fs_lasso(d_chr, "y"),
    function() fs_lasso(d, "y", alpha = 2),
    function() fs_lasso(d, "y", nfolds = 3, custom_folds = rep(c(1L, 3L), 4))
  )
  for (call_fn in bad_calls) {
    cnd <- tryCatch(call_fn(), error = identity)
    expect_s3_class(cnd, "error")
    expect_false(grepl("^Error: ", conditionMessage(cnd)))
  }
})

test_that("an all-NA predictor column is an error naming the column", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")

  d <- data.frame(y = as.numeric(20:1), x1 = as.numeric(1:20),
                  allna = rep(NA_real_, 20))
  # the all-NA check fires under either impute policy
  expect_error(fs_lasso(d, "y", nfolds = 3),
               "entirely missing \\(all NA\\).*'allna'")
  expect_error(fs_lasso(d, "y", nfolds = 3, impute = "mean"),
               "entirely missing \\(all NA\\).*'allna'")
})

test_that("impute = 'none' errors on NAs and names the offending columns", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")

  set.seed(101)
  n <- 40
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- d$x1 + rnorm(n, sd = 0.3)
  d$x1[c(3, 17)] <- NA
  d$x2[8] <- NA

  cnd <- tryCatch(fs_lasso(d, "y", nfolds = 3), error = identity)
  expect_s3_class(cnd, "error")
  expect_match(conditionMessage(cnd), "Missing values in predictor column")
  expect_match(conditionMessage(cnd), "'x1'")
  expect_match(conditionMessage(cnd), "'x2'")
  expect_false(grepl("'x3'", conditionMessage(cnd), fixed = TRUE))
  # the message points at the fix
  expect_match(conditionMessage(cnd), "impute = ", fixed = TRUE)
})

test_that("data.frame with scattered NAs runs end-to-end under impute = 'mean' (NA regression)", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_on_cran()

  set.seed(101)
  n <- 90
  d <- data.frame(
    x1  = rnorm(n),
    x2  = rnorm(n),
    x3  = rnorm(n),
    cat = sample(c("a", "b", "c"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  d$y <- 2 * d$x1 - 3 * d$x2 + rnorm(n, sd = 0.5)
  # Scatter NAs: these rows must survive to imputation, not be dropped by
  # model.matrix() (the old path died with a "different numbers of rows" error)
  d$x1[c(3, 17, 40)] <- NA
  d$x2[c(8, 55)] <- NA
  d$x3[c(1, 30, 62, 88)] <- NA

  expect_warning(
    res <- fs_lasso(d, "y", nfolds = 3, impute = "mean", seed = 7),
    "imputed with column means"
  )
  expect_s3_class(res, "fs_result")
  expect_s3_class(res$scores, "data.frame")
  expect_identical(names(res$scores),
                   c("Variable", "Coefficient", "AbsCoefficient"))
  expect_true(is.numeric(res$details$lambda_min) &&
                length(res$details$lambda_min) == 1L)
  expect_true(is.numeric(res$details$lambda_1se) &&
                length(res$details$lambda_1se) == 1L)
  # All rows were kept: numeric columns and expanded factor levels all present
  expect_true(all(c("x1", "x2", "x3") %in% res$scores$Variable))
  expect_true(any(grepl("^cat", res$scores$Variable)))

  # and the same data is a hard error under the default policy
  expect_error(fs_lasso(d, "y", nfolds = 3, seed = 7),
               "Missing values in predictor column")
})

test_that("fs_lasso returns an fs_result with the documented shape", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_on_cran()

  set.seed(202)
  n <- 120
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- 3 * d$x1 - 2 * d$x2 + rnorm(n, sd = 0.5)

  res <- fs_lasso(d, "y", nfolds = 3, seed = 11)

  expect_s3_class(res, "fs_result")
  expect_named(res, c("selected", "scores", "method", "task", "model",
                      "details", "call"))
  expect_identical(res$method, "lasso")
  expect_identical(res$task, "regression")
  expect_true(is.character(res$selected))
  expect_identical(selected(res), res$selected)
  expect_null(res$model)

  expect_s3_class(res$scores, "data.frame")
  expect_named(res$scores, c("Variable", "Coefficient", "AbsCoefficient"))
  expect_named(res$details,
               c("lambda_min", "lambda_1se", "coefficients", "n_features"))
  expect_identical(res$details$n_features, nrow(res$scores))
  expect_s3_class(res$details$coefficients, "data.frame")
  expect_identical(names(res$details$coefficients),
                   c("Variable", "Coefficient", "AbsCoefficient"))

  # selected == the non-zero raw coefficients, and nothing else
  raw <- res$details$coefficients
  expect_setequal(res$selected, raw$Variable[raw$Coefficient != 0])
  expect_true(all(res$selected %in% res$scores$Variable))

  # print() works and reports the method
  expect_output(print(res), "fs_result")
  expect_output(print(res), "lasso")
})

test_that("gaussian smoke: sorted importance, stable rownames, model opt-in", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_on_cran()

  set.seed(202)
  n <- 120
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- 3 * d$x1 - 2 * d$x2 + rnorm(n, sd = 0.5)

  res <- fs_lasso(d, "y", nfolds = 3, seed = 11)

  imp <- res$scores
  expect_identical(imp$AbsCoefficient,
                   sort(imp$AbsCoefficient, decreasing = TRUE))
  expect_identical(rownames(imp), as.character(seq_len(nrow(imp))))
  expect_identical(imp$AbsCoefficient, abs(imp$Coefficient))
  # The dominant true signal should top the ranking
  expect_identical(imp$Variable[1L], "x1")

  raw <- res$details$coefficients
  expect_identical(raw$AbsCoefficient,
                   sort(raw$AbsCoefficient, decreasing = TRUE))
  expect_identical(rownames(raw), as.character(seq_len(nrow(raw))))

  res_model <- fs_lasso(d, "y", nfolds = 3, seed = 11, return_model = TRUE)
  expect_s3_class(res_model$model, "cv.glmnet")
})

test_that("scores rank on the standardized scale, details$coefficients stay raw", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_on_cran()

  set.seed(909)
  n <- 150
  d <- data.frame(
    big   = rnorm(n, sd = 100),
    small = rnorm(n, sd = 1),
    noise = rnorm(n, sd = 1)
  )
  d$y <- 0.05 * d$big + 1 * d$small + rnorm(n, sd = 0.5)

  res <- fs_lasso(d, "y", nfolds = 3, seed = 5)
  raw <- res$details$coefficients
  std <- res$scores

  # the raw ranking is dominated by the narrow-scale predictor's big coefficient
  expect_identical(raw$Variable[1L], "small")
  # the standardized ranking is dominated by the wide-scale predictor
  expect_identical(std$Variable[1L], "big")

  # scores are exactly the raw coefficients times each column's SD
  sds <- vapply(d[, c("big", "small", "noise")], stats::sd, numeric(1))
  raw_v <- stats::setNames(raw$Coefficient, raw$Variable)
  std_v <- stats::setNames(std$Coefficient, std$Variable)
  expect_equal(std_v[names(raw_v)], raw_v * sds[names(raw_v)])

  # standardize = FALSE leaves the raw table as the scores
  res_raw <- fs_lasso(d, "y", nfolds = 3, standardize = FALSE, seed = 5)
  expect_identical(res_raw$scores, res_raw$details$coefficients)
})

test_that("valid custom_folds (as doubles) are accepted end-to-end", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_on_cran()

  set.seed(404)
  n <- 60
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- d$x1 + rnorm(n, sd = 0.3)
  folds <- as.numeric(rep(1:3, length.out = n))

  res <- fs_lasso(d, "y", nfolds = 3, custom_folds = folds)
  expect_s3_class(res, "fs_result")
  expect_true(is.finite(res$details$lambda_min))
})

test_that("same seed reproduces results and the caller's RNG state is untouched", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")
  skip_on_cran()

  set.seed(303)
  n <- 100
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$y <- d$x1 - 2 * d$x3 + rnorm(n, sd = 0.4)

  set.seed(777)
  rng_before <- .Random.seed
  res1 <- fs_lasso(d, "y", nfolds = 3, seed = 99)
  expect_identical(.Random.seed, rng_before)

  res2 <- fs_lasso(d, "y", nfolds = 3, seed = 99)
  expect_identical(res1$details$lambda_min, res2$details$lambda_min)
  expect_identical(res1$details$lambda_1se, res2$details$lambda_1se)
  expect_identical(res1$scores, res2$scores)
  expect_identical(res1$details$coefficients, res2$details$coefficients)
  expect_identical(res1$selected, res2$selected)
})
