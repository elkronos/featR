# Tests for fs_boruta().
#
# All argument validation in fs_boruta() runs before fs_require("Boruta"),
# so validation tests need no skips. Target-content checks and real Boruta
# runs require the suggested Boruta package. Correlation pruning is now built
# from stats::cor() alone, so no caret skip is needed anywhere.

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
  expect_error(fs_boruta(d, "y", maxRuns = 0),
               "'maxRuns' must be between 11 and Inf")
  # Boruta itself refuses fewer than 11 runs; featR catches it first so the
  # error names the featR argument rather than surfacing from the dependency.
  expect_error(fs_boruta(d, "y", maxRuns = 10),
               "'maxRuns' must be between 11 and Inf")
  expect_error(fs_boruta(d, "y", resolve_tentative = "yes"),
               "'resolve_tentative' must be TRUE or FALSE")
  expect_error(fs_boruta(d, "y", verbose = "yes"),
               "'verbose' must be TRUE or FALSE")
})

test_that("fs_boruta signature and defaults are stable", {
  fx <- formals(fs_boruta)
  expect_identical(
    names(fx),
    c("data", "target", "maxRuns", "cutoff_features", "cutoff_cor",
      "resolve_tentative", "seed", "verbose")
  )
  expect_identical(fx$maxRuns, 250)
  expect_null(fx$cutoff_features)
  expect_identical(fx$cutoff_cor, 0.7)
  expect_identical(fx$resolve_tentative, TRUE)
  expect_null(fx$seed)
  expect_identical(fx$verbose, FALSE)
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

test_that("boruta_prune_correlated early paths are no-ops and still validate", {
  preds <- data.frame(
    a  = c(1, 2, 3, 4, 5),
    b  = c(5, 4, 3, 2, 1),
    f1 = factor(c("u", "v", "u", "v", "u")),
    f2 = factor(c("v", "v", "u", "u", "u"))
  )
  helper <- featR:::boruta_prune_correlated
  imp <- c(a = 2, b = 1, f1 = 0.5, f2 = 0.25)

  # a single selected feature is returned untouched
  out1 <- helper(preds, "a", imp)
  expect_identical(out1$keep, "a")
  expect_identical(out1$dropped, character(0))

  # with at most one numeric feature in the selection, nothing is pruned
  out2 <- helper(preds, c("a", "f1", "f2"), imp)
  expect_identical(out2$keep, c("a", "f1", "f2"))
  expect_identical(out2$dropped, character(0))

  # unknown features are an error
  expect_error(helper(preds, c("a", "zz"), imp), "not found in `predictors`")

  # cutoff_cor is validated even for trivial selections
  expect_error(helper(preds, "a", imp, cutoff_cor = 2),
               "'cutoff_cor' must be between 0 and 1")
})

test_that("boruta_prune_correlated keeps the higher-importance member", {
  # x1 and x1_dup are correlated well above 0.9; z is not correlated with
  # either, so the only decision is which member of the pair survives.
  x1 <- c(1, 2, 3, 4, 5, 6, 7, 8)
  preds <- data.frame(
    x1     = x1,
    x1_dup = x1 + c(0.01, -0.01, 0.02, 0, 0.01, -0.02, 0, 0.01),
    z      = c(3, 1, 4, 1, 5, 9, 2, 6)
  )
  feats  <- c("x1", "x1_dup", "z")
  helper <- featR:::boruta_prune_correlated

  # sanity: the pair is above the cutoff and z is well below it
  cm <- abs(stats::cor(preds))
  expect_gt(cm["x1", "x1_dup"], 0.9)
  expect_lt(cm["x1", "z"], 0.9)
  expect_lt(cm["x1_dup", "z"], 0.9)

  # x1_dup is the more important member -> x1 is dropped
  out_a <- helper(preds, feats, c(x1 = 2.5, x1_dup = 9.9, z = 1.0),
                  cutoff_cor = 0.9)
  expect_identical(out_a$keep, c("x1_dup", "z"))
  expect_identical(out_a$dropped, "x1")

  # flip the importances and the decision flips with them: this is what
  # caret::findCorrelation() could not do, since the correlation structure is
  # identical in both cases.
  out_b <- helper(preds, feats, c(x1 = 9.9, x1_dup = 2.5, z = 1.0),
                  cutoff_cor = 0.9)
  expect_identical(out_b$keep, c("x1", "z"))
  expect_identical(out_b$dropped, "x1_dup")

  # a missing importance ranks last, so that feature is the one dropped
  out_c <- helper(preds, feats, c(x1 = 9.9, z = 1.0), cutoff_cor = 0.9)
  expect_identical(out_c$dropped, "x1_dup")
})

test_that("boruta_median_importance ignores -Inf history entries", {
  fake <- list(
    ImpHistory = matrix(
      c(1, 3, 5,       # x1
        -Inf, -Inf, 2, # x2, rejected in the first two runs
        0, 0, 0),      # shadowMax (must be ignored)
      nrow = 3,
      dimnames = list(NULL, c("x1", "x2", "shadowMax"))
    ),
    finalDecision = stats::setNames(
      factor(c("Confirmed", "Rejected"),
             levels = c("Tentative", "Confirmed", "Rejected")),
      c("x1", "x2")
    )
  )

  imp <- featR:::boruta_median_importance(fake)
  expect_identical(names(imp), c("x1", "x2"))
  expect_equal(unname(imp[["x1"]]), 3)
  expect_equal(unname(imp[["x2"]]), 2)

  # alignment pads absent attributes with NA and keeps the requested order
  aligned <- featR:::boruta_median_importance(fake, features = c("x2", "zz"))
  expect_identical(names(aligned), c("x2", "zz"))
  expect_true(is.na(aligned[["zz"]]))
})

test_that("fs_boruta validates target content", {
  skip_if_not_installed("Boruta")

  d_chr <- data.frame(y = c("a", "b", "a", "b"), x = c(1, 2, 3, 4),
                      stringsAsFactors = FALSE)
  expect_error(fs_boruta(d_chr, "y"),
               "must be numeric \\(regression\\) or factor")

  d_na_y <- data.frame(y = factor(c("a", NA, "b", "a")), x = c(1, 2, 3, 4))
  expect_error(fs_boruta(d_na_y, "y"), "`target` contains missing values")

  d_na_x <- data.frame(y = factor(c("a", "b", "a", "b")), x = c(1, NA, 3, 4))
  expect_error(fs_boruta(d_na_x, "y"), "Predictors contain missing values")
})

test_that("fs_boruta returns a reproducible fs_result on a factor target", {
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

  res <- fs_boruta(d, "y", maxRuns = 15, cutoff_cor = NULL, seed = 42)

  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, state_before)

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "boruta")
  expect_identical(res$task, "classification")
  expect_s3_class(res$model, "Boruta")
  expect_false(is.null(res$call))

  expect_type(res$selected, "character")
  expect_true(all(c("x1", "x2") %in% res$selected))
  expect_true(all(res$selected %in% setdiff(names(d), "y")))

  # scores are the median Boruta importance of every candidate feature
  expect_true(is.numeric(res$scores))
  expect_setequal(names(res$scores), setdiff(names(d), "y"))
  expect_gt(res$scores[["x1"]], res$scores[["n1"]])

  # documented details, all present and named
  expect_named(res$details,
               c("boruta_obj", "decisions", "dropped_correlated", "n_features"))
  expect_s3_class(res$details$boruta_obj, "Boruta")
  expect_setequal(names(res$details$decisions), setdiff(names(d), "y"))
  expect_true(all(as.character(res$details$decisions) %in%
                    c("Confirmed", "Tentative", "Rejected")))
  expect_identical(res$details$dropped_correlated, character(0))
  expect_identical(res$details$n_features, 4L)

  expect_output(print(res), "boruta")

  res2 <- fs_boruta(d, "y", maxRuns = 15, cutoff_cor = NULL, seed = 42)
  expect_identical(res$selected, res2$selected)
})

test_that("fs_boruta reports a regression task for a numeric target", {
  skip_if_not_installed("Boruta")
  skip_on_cran()

  set.seed(404)
  n <- 60
  x1 <- rnorm(n)
  d <- data.frame(
    y  = 3 * x1 + rnorm(n, sd = 0.3),
    x1 = x1,
    n1 = rnorm(n)
  )

  res <- fs_boruta(d, "y", maxRuns = 15, cutoff_cor = NULL, seed = 7)
  expect_s3_class(res, "fs_result")
  expect_identical(res$task, "regression")
  expect_true("x1" %in% res$selected)
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

  res <- fs_boruta(d, "y", maxRuns = 12, cutoff_features = 1,
                   cutoff_cor = NULL, seed = 5)
  expect_length(res$selected, 1L)
  expect_true(res$selected %in% c("x1", "x2"))
})

test_that("fs_boruta prunes duplicated features via cutoff_cor", {
  skip_if_not_installed("Boruta")
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

  res <- fs_boruta(d, "y", maxRuns = 15, cutoff_cor = 0.9, seed = 11)

  expect_s3_class(res, "fs_result")
  # of a perfectly correlated pair, at most one member survives pruning
  expect_lte(sum(c("x1", "x1_dup") %in% res$selected), 1L)
  # but the informative signal itself is not dropped wholesale
  expect_true(any(c("x1", "x1_dup") %in% res$selected))

  # the pruned member is reported and never also reported as selected
  dropped <- res$details$dropped_correlated
  expect_true(all(dropped %in% c("x1", "x1_dup", "z")))
  expect_length(intersect(res$selected, dropped), 0L)
  # the survivor is the member Boruta scored higher
  pair_dropped <- intersect(c("x1", "x1_dup"), dropped)
  if (length(pair_dropped) == 1L) {
    kept <- setdiff(c("x1", "x1_dup"), pair_dropped)
    expect_gte(res$scores[[kept]], res$scores[[pair_dropped]])
  }
})
