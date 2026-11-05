# Tests for fs_svm().
#
# fs_svm() validates the typo-prone arguments (kernel, select_method, task)
# and then all of its data/option arguments BEFORE calling fs_require(), so
# every validation test below runs everywhere -- including the SVM-RFE/kernel
# compatibility guard, which needs no model fit. Helpers that rely only on
# base R (svm_coerce_target, svm_align_levels, svm_tune_grid,
# svm_center_scale, svm_rfe_size_ladder) are exercised directly via featR:::
# so their behavior is covered without caret installed; anything reaching
# caret/kernlab/e1071/randomForest is gated and, when it fits a model, also
# skipped on CRAN.

# Deterministic two-class frame (no RNG): x1/x2 separate the classes, x3 is
# an uninformative but non-constant covariate.
svm_toy_classification <- function() {
  grp <- rep(c("a", "b"), each = 30)
  wobble <- rep(c(-0.2, 0.1, 0.3, -0.1, 0.2, -0.3), 10)
  data.frame(
    y  = factor(grp),
    x1 = ifelse(grp == "b", 2, 0) + wobble,
    x2 = ifelse(grp == "b", -1, 1) + rev(wobble),
    x3 = seq(-1, 1, length.out = 60)
  )
}

svm_toy_regression <- function() {
  d <- svm_toy_classification()
  data.frame(
    y  = 2 * d$x1 - d$x2 + d$x3,
    x1 = d$x1,
    x2 = d$x2,
    x3 = d$x3
  )
}

# Deterministic SVM-RFE frame (no RNG). x1 and x2 genuinely drive the label
# (a 3.0 shift between the classes on top of a small oscillation), while n1
# and n2 are pure noise: their patterns have periods 10 and 6, and each class
# spans exactly 30 rows, so both features have *identical* distributions in
# the two classes and carry no signal at all.
svm_rfe_toy <- function() {
  grp <- rep(c("a", "b"), each = 30)
  w1 <- rep(c(-0.6, 0.2, 0.5, -0.3, 0.4, -0.2), 10)
  w2 <- rep(c(0.3, -0.5, 0.6, -0.4, 0.1, 0.5, -0.6, 0.2, -0.1, 0.4), 6)
  data.frame(
    y  = factor(grp),
    x1 = ifelse(grp == "b", 1.5, -1.5) + w1,
    x2 = ifelse(grp == "b", 1.5, -1.5) + w2,
    n1 = rep(c(-1, -0.5, 0, 0.5, 1, 0.25, -0.25, 0.75, -0.75, 0.1), 6),
    n2 = rep(c(0.4, -0.9, 0.15, -0.35, 0.8, -0.2), 10)
  )
}

test_that("kernel, select_method and task are validated before fs_require()", {
  d <- svm_toy_classification()

  # match.arg() fires first, so a typo'd kernel fails immediately
  expect_error(fs_svm(d, "y", "classification", kernel = "banana"),
               "should be one of")
  expect_error(fs_svm(d, "y", "classification", select_method = "nope"),
               "should be one of")
  expect_error(fs_svm(d, "y", "classifcation"),
               "'task' must be either 'classification' or 'regression'")
  expect_error(fs_svm(d, "y", 1), "single non-empty character string")
  expect_error(fs_svm(d, "y", c("classification", "regression")),
               "single non-empty character string")
})

test_that("data, target and numeric options are validated before fs_require()", {
  d <- svm_toy_classification()

  expect_error(fs_svm("nope", "y", "classification"),
               "'data' must be a data.frame")
  expect_error(fs_svm(d, "absent", "classification"),
               "Column 'absent' not found in 'data'")
  expect_error(fs_svm(d, "y", "classification", train_ratio = 0),
               "'train_ratio' must be a single numeric value in \\(0, 1\\)")
  expect_error(fs_svm(d, "y", "classification", train_ratio = 1),
               "'train_ratio' must be a single numeric value in \\(0, 1\\)")
  expect_error(fs_svm(d, "y", "classification", train_ratio = "a"),
               "'train_ratio' must be a single finite number")
  expect_error(fs_svm(d, "y", "classification", nfolds = 1),
               "'nfolds' must be between 2 and Inf")
  expect_error(fs_svm(d, "y", "classification", nfolds = 2.5),
               "'nfolds' must be a whole number")
  expect_error(fs_svm(d[, "y", drop = FALSE], "y", "classification"),
               "at least one predictor column")

  d_na_y <- d
  d_na_y$y[3] <- NA
  expect_error(fs_svm(d_na_y, "y", "classification"),
               "'target' contains missing values")

  d_na_x <- d
  d_na_x$x1[5] <- NA
  expect_error(fs_svm(d_na_x, "y", "classification"),
               "Predictor columns contain missing values")

  expect_error(fs_svm(d, "y", "regression"),
               "For regression, the target must be numeric")

  d_one <- d
  d_one$y <- factor(rep("a", nrow(d_one)))
  expect_error(fs_svm(d_one, "y", "classification"),
               "at least two classes")

  expect_error(fs_svm(d[1, , drop = FALSE], "y", "classification"),
               "must contain at least 2 rows")
})

test_that("logical flags, n_features and tune_grid are validated early", {
  d <- svm_toy_classification()

  expect_error(fs_svm(d, "y", "classification", feature_select = "yes"),
               "'feature_select' must be TRUE or FALSE")
  expect_error(fs_svm(d, "y", "classification", class_imbalance = NA),
               "'class_imbalance' must be TRUE or FALSE")
  expect_error(fs_svm(d, "y", "classification", verbose = NA),
               "'verbose' must be TRUE or FALSE")
  expect_error(fs_svm(d, "y", "classification", n_features = 0),
               "'n_features' must be between 1 and Inf")
  expect_error(fs_svm(d, "y", "classification", n_features = 1.5),
               "'n_features' must be a whole number")
  expect_error(fs_svm(d, "y", "classification", tune_grid = "C"),
               "'tune_grid' must be a data.frame")
  expect_error(fs_svm(d, "y", "classification", n_cores = 0),
               "'n_cores' must be between 1 and Inf")
})

test_that("SVM-RFE refuses a non-linear kernel with an actionable message", {
  # This guard needs no model fit and runs before fs_require(), so it is not
  # skipped anywhere.
  d <- svm_toy_classification()

  expect_error(
    fs_svm(d, "y", "classification", kernel = "radial",
           feature_select = TRUE, select_method = "svm_rfe"),
    "SVM-RFE requires a linear kernel"
  )
  expect_error(
    fs_svm(d, "y", "classification", kernel = "polynomial",
           feature_select = TRUE, select_method = "svm_rfe"),
    'select_method = "rf_rfe"',
    fixed = TRUE
  )

  # The guard is scoped: with selection off, or with the random-forest
  # screener, a non-linear kernel is fine and validation simply continues to
  # the next check (tune_grid).
  expect_error(
    fs_svm(d, "y", "classification", kernel = "radial", tune_grid = "C"),
    "'tune_grid' must be a data.frame"
  )
  expect_error(
    fs_svm(d, "y", "classification", kernel = "radial",
           feature_select = TRUE, select_method = "rf_rfe", tune_grid = "C"),
    "'tune_grid' must be a data.frame"
  )
})

test_that("fs_svm signature and defaults are stable", {
  fx <- formals(fs_svm)
  expect_identical(
    names(fx),
    c("data", "target", "task", "train_ratio", "nfolds", "kernel",
      "tune_grid", "feature_select", "select_method", "n_features",
      "class_imbalance", "seed", "verbose", "n_cores")
  )
  expect_identical(fx$train_ratio, 0.7)
  expect_identical(fx$nfolds, 5)
  expect_null(fx$tune_grid)
  expect_identical(fx$feature_select, FALSE)
  expect_null(fx$n_features)
  expect_identical(fx$class_imbalance, FALSE)
  expect_null(fx$seed)
  expect_identical(fx$verbose, FALSE)
  expect_identical(fx$n_cores, 1L)
  expect_identical(eval(fx$kernel), c("linear", "radial", "polynomial"))
  expect_identical(eval(fx$select_method), c("svm_rfe", "rf_rfe"))
})

test_that("svm_coerce_target guards numeric classification targets", {
  coerce <- featR:::svm_coerce_target

  # More than 10 distinct values almost certainly means regression
  d_many <- data.frame(y = as.numeric(1:20), x = as.numeric(20:1))
  expect_error(coerce(d_many, "y", "classification"),
               "did you mean task = 'regression'\\?")

  # At most 10 distinct values is coerced, with a message
  d_few <- data.frame(y = rep(1:3, 4), x = as.numeric(1:12))
  expect_message(out <- coerce(d_few, "y", "classification"),
                 "Coercing numeric target 'y' \\(3 unique values\\)")
  expect_true(is.factor(out$y))
  expect_identical(levels(out$y), c("1", "2", "3"))

  # Character targets become factors without a message
  d_chr <- data.frame(y = rep(c("u", "v"), 5), x = as.numeric(1:10),
                      stringsAsFactors = FALSE)
  expect_silent(out_chr <- coerce(d_chr, "y", "classification"))
  expect_true(is.factor(out_chr$y))

  # Regression coerces to numeric
  d_reg <- data.frame(y = factor(c("1", "2", "3")), x = c(1, 2, 3))
  out_reg <- coerce(d_reg, "y", "regression")
  expect_true(is.numeric(out_reg$y))
})

test_that("svm_align_levels drops test rows with unseen predictor levels", {
  align <- featR:::svm_align_levels

  train <- data.frame(
    y = factor(c("a", "b", "a", "b")),
    f = factor(c("p", "q", "p", "q")),
    n = c(1, 2, 3, 4)
  )
  test <- data.frame(
    y = factor(c("a", "b", "a")),
    f = factor(c("p", "q", "zzz")), # 'zzz' never seen in training
    n = c(5, 6, 7)
  )

  expect_warning(
    out <- align(train, test, "y"),
    "Dropped 1 test row\\(s\\) containing predictor factor levels unseen"
  )
  expect_identical(nrow(out$test_set), 2L)
  expect_identical(levels(out$test_set$f), levels(out$train_set$f))
  expect_false(anyNA(out$test_set$f))
  # numeric columns are untouched
  expect_identical(out$test_set$n, c(5, 6))

  # every test row unseen -> informative error rather than an empty frame
  test_all_bad <- test
  test_all_bad$f <- factor(c("zzz", "yyy", "xxx"))
  expect_error(
    suppressWarnings(align(train, test_all_bad, "y")),
    "All test rows contained predictor factor levels unseen"
  )

  # nothing to align when predictors are numeric
  train_num <- data.frame(y = factor(c("a", "b")), n = c(1, 2))
  test_num <- data.frame(y = factor(c("a", "b")), n = c(3, 4))
  expect_silent(out_num <- align(train_num, test_num, "y"))
  expect_identical(out_num$test_set, test_num)
})

test_that("svm_tune_grid returns the documented per-kernel grids", {
  grid <- featR:::svm_tune_grid

  lin <- grid("linear")
  expect_s3_class(lin, "data.frame")
  expect_identical(names(lin), "C")
  expect_identical(nrow(lin), 16L) # 2^seq(-5, 10)

  rad <- grid("radial")
  expect_identical(names(rad), c("sigma", "C"))
  expect_identical(nrow(rad), 11L * 12L)

  poly <- grid("polynomial")
  expect_identical(names(poly), c("degree", "scale", "C"))
  expect_identical(nrow(poly), 4L * 3L * 12L)

  expect_error(grid("banana"), "should be one of")
})

test_that("svm_center_scale standardizes and survives constant columns", {
  x <- data.frame(a = c(1, 2, 3, 4), b = c(5, 5, 5, 5), cc = c(-1, 0, 1, 2))
  out <- featR:::svm_center_scale(x)

  expect_true(is.matrix(out))
  expect_identical(colnames(out), c("a", "b", "cc"))
  expect_equal(unname(colMeans(out)), c(0, 0, 0))
  # the constant column is centered but not rescaled, so it collapses to zeros
  expect_equal(unname(apply(out, 2L, stats::sd)), c(1, 0, 1))
  expect_false(anyNA(out))
  expect_null(attr(out, "scaled:center"))
  expect_null(attr(out, "scaled:scale"))
})

test_that("svm_rfe_size_ladder builds a short powers-of-two ladder", {
  ladder <- featR:::svm_rfe_size_ladder

  expect_identical(ladder(1), 1L)
  expect_identical(ladder(4), c(1L, 2L, 4L))
  expect_identical(ladder(5), c(1L, 2L, 4L, 5L))
  expect_identical(ladder(32), c(1L, 2L, 4L, 8L, 16L, 32L))

  big <- ladder(100)
  expect_lte(length(big), 6L)
  expect_identical(big[1], 1L)
  expect_identical(big[length(big)], 100L)
})

test_that("svm_performance drops NA pairs with a warning and stays consistent", {
  skip_if_not_installed("caret")

  preds <- c(1, 2, NA, 4)
  actuals <- c(1.5, 2.5, 3, 3.5)

  expect_warning(
    perf <- featR:::svm_performance(preds, actuals, "regression"),
    "1 prediction/actual pair\\(s\\) contained NA"
  )
  expect_identical(names(perf), c("RMSE", "Rsquared", "MAE"))
  # MAE is computed from the same NA-filtered vectors as RMSE
  expect_equal(unname(perf["MAE"]), mean(abs(c(1, 2, 4) - c(1.5, 2.5, 3.5))))

  expect_silent(
    clean <- featR:::svm_performance(c(1, 2, 3), c(1.5, 2.5, 3.5), "regression")
  )
  expect_identical(names(clean), c("RMSE", "Rsquared", "MAE"))
})

test_that("svm_rfe_rank ranks the informative pair above the noise pair", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_if_not_installed("e1071")
  skip_on_cran()

  d <- svm_rfe_toy()

  # No RNG anywhere: the frame is deterministic, and supplying n_features
  # skips the cross-validated size search.
  res <- suppressWarnings(featR:::svm_rfe_rank(
    x = d[, c("x1", "x2", "n1", "n2")],
    y = d$y,
    task = "classification",
    n_features = 2L
  ))

  expect_setequal(res$ranking, c("x1", "x2", "n1", "n2"))
  # rank 1 = eliminated last = most important
  expect_setequal(res$ranking[1:2], c("x1", "x2"))
  expect_setequal(res$ranking[3:4], c("n1", "n2"))
  expect_setequal(res$selected, c("x1", "x2"))
  expect_identical(res$selected, res$ranking[1:2])

  # the reported criterion is w^2 from the first, full-feature fit
  expect_true(is.numeric(res$scores))
  expect_setequal(names(res$scores), c("x1", "x2", "n1", "n2"))
  expect_true(all(res$scores >= 0))
  expect_gt(max(res$scores[c("x1", "x2")]), max(res$scores[c("n1", "n2")]))

  # an explicit n_features skips the size search entirely
  expect_null(res$sizes)
  expect_null(res$size_scores)
  expect_true(is.na(res$size_metric))
})

test_that("svm_rfe_rank picks a subset size when n_features is NULL", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_on_cran()

  d <- svm_rfe_toy()

  set.seed(4)
  res <- suppressWarnings(featR:::svm_rfe_rank(
    x = d[, c("x1", "x2", "n1", "n2")],
    y = d$y,
    task = "classification",
    nfolds = 3
  ))

  expect_identical(res$sizes, c(1L, 2L, 4L))
  expect_identical(names(res$size_scores), c("1", "2", "4"))
  expect_true(is.numeric(res$size_scores))
  expect_identical(res$size_metric, "accuracy")
  expect_true(length(res$selected) %in% res$sizes)
  expect_identical(res$selected, utils::head(res$ranking, length(res$selected)))
})

test_that("classification smoke returns the documented fs_result", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_if_not_installed("e1071")
  skip_on_cran()

  d <- svm_toy_classification()

  set.seed(1)
  invisible(stats::runif(1))
  state_before <- .Random.seed

  res <- fs_svm(
    d, "y", "classification",
    nfolds = 3, kernel = "linear",
    tune_grid = data.frame(C = 1),
    seed = 42
  )

  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, state_before)

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "svm_linear")
  expect_identical(res$task, "classification")
  expect_s3_class(res$model, "train")
  expect_false(is.null(res$call))

  # no selection ran, so every encoded predictor is "selected" and there are
  # no comparable per-feature scores
  expect_setequal(res$selected, c("x1", "x2", "x3"))
  expect_null(res$scores)

  expect_named(res$details,
               c("test_set", "predictions", "performance", "selection",
                 "encoder", "n_features"))
  expect_null(res$details$selection)
  expect_identical(res$details$n_features, 3L)
  expect_s3_class(res$details$encoder, "dummyVars")
  expect_s3_class(res$details$performance, "confusionMatrix")
  expect_length(res$details$predictions, nrow(res$details$test_set))
  expect_true(is.factor(res$details$test_set$y))
  # the classes are well separated, so accuracy should be high
  expect_gt(unname(res$details$performance$overall["Accuracy"]), 0.7)

  expect_output(print(res), "svm_linear")
})

test_that("regression smoke returns a named RMSE/Rsquared/MAE vector", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_on_cran()

  d <- svm_toy_regression()

  res <- fs_svm(
    d, "y", "regression",
    nfolds = 3, kernel = "linear",
    tune_grid = data.frame(C = 1),
    seed = 7
  )

  expect_s3_class(res, "fs_result")
  expect_identical(res$task, "regression")
  perf <- res$details$performance
  expect_true(is.numeric(perf))
  expect_identical(names(perf), c("RMSE", "Rsquared", "MAE"))
  expect_true(all(is.finite(perf[c("RMSE", "MAE")])))
  expect_length(res$details$predictions, nrow(res$details$test_set))
})

test_that("the same seed reproduces the run", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_on_cran()

  d <- svm_toy_regression()
  args <- list(
    data = d, target = "y", task = "regression",
    nfolds = 3, kernel = "linear",
    tune_grid = data.frame(C = 1), seed = 123
  )

  res1 <- do.call(fs_svm, args)
  res2 <- do.call(fs_svm, args)
  expect_equal(res1$details$performance, res2$details$performance)
  expect_equal(res1$details$predictions, res2$details$predictions)
})

test_that("class_imbalance routes up-sampling through trainControl", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_if_not_installed("e1071")
  skip_on_cran()

  d <- svm_toy_classification()
  # make class 'b' the minority without disturbing the separation
  d <- rbind(d[d$y == "a", ], d[d$y == "b", ][1:10, ])

  res <- fs_svm(
    d, "y", "classification",
    nfolds = 3, kernel = "linear",
    tune_grid = data.frame(C = 1),
    class_imbalance = TRUE,
    seed = 21
  )

  # Up-sampling must be delegated to caret so it happens inside each
  # resample; the old implementation duplicated rows before CV instead.
  expect_false(is.null(res$model$control$sampling))
  expect_s3_class(res$details$performance, "confusionMatrix")

  res_off <- fs_svm(
    d, "y", "classification",
    nfolds = 3, kernel = "linear",
    tune_grid = data.frame(C = 1),
    seed = 21
  )
  expect_null(res_off$model$control$sampling)
})

test_that("feature_select = TRUE runs SVM-RFE end to end", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_if_not_installed("e1071")
  skip_on_cran()

  d <- svm_rfe_toy()

  res <- suppressWarnings(fs_svm(
    d, "y", "classification",
    nfolds = 3, kernel = "linear",
    tune_grid = data.frame(C = 1),
    feature_select = TRUE,
    select_method = "svm_rfe",
    n_features = 2,
    seed = 13
  ))

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "svm_linear")
  expect_length(res$selected, 2L)
  expect_setequal(res$selected, c("x1", "x2"))

  # scores cover every encoded predictor, not just the survivors
  expect_true(is.numeric(res$scores))
  expect_setequal(names(res$scores), c("x1", "x2", "n1", "n2"))
  expect_identical(res$details$n_features, 4L)

  sel <- res$details$selection
  expect_named(sel, c("method", "ranking", "scores", "sizes", "size_scores",
                      "size_metric"))
  expect_identical(sel$method, "svm_rfe")
  expect_setequal(sel$ranking, c("x1", "x2", "n1", "n2"))
  expect_identical(res$selected, sel$ranking[1:2])
  expect_null(sel$sizes)
  expect_s3_class(res$details$performance, "confusionMatrix")
})

test_that("select_method = 'rf_rfe' keeps the random-forest screening path", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_if_not_installed("e1071")
  skip_if_not_installed("randomForest")
  skip_on_cran()

  d <- svm_toy_classification()

  res <- suppressWarnings(fs_svm(
    d, "y", "classification",
    nfolds = 3, kernel = "linear",
    tune_grid = data.frame(C = 1),
    feature_select = TRUE,
    select_method = "rf_rfe",
    seed = 99
  ))

  expect_s3_class(res, "fs_result")
  expect_type(res$selected, "character")
  expect_gt(length(res$selected), 0L)
  expect_true(all(res$selected %in% c("x1", "x2", "x3")))

  expect_identical(res$details$selection$method, "rf_rfe")
  expect_true(is.numeric(res$scores))
  expect_setequal(names(res$scores), c("x1", "x2", "x3"))
  expect_setequal(res$details$selection$ranking, c("x1", "x2", "x3"))
  expect_null(res$details$selection$sizes)
  expect_s3_class(res$details$performance, "confusionMatrix")
})

test_that("SVM-RFE ranks a regression target (eps-svr weight extraction)", {
  # COVERAGE: every prior SVM-RFE test used a factor target, so the eps-svr
  # branch of svm_rfe_weights() -- where kernlab returns a bare xmatrix and
  # coef rather than one block per pairwise problem -- had never executed.
  skip_on_cran()
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")

  t <- svm_rfe_toy()
  d <- data.frame(
    y  = 3 * t$x1 - 2 * t$x2,   # driven entirely by x1 and x2
    x1 = t$x1,
    x2 = t$x2,
    n1 = t$n1,                  # pure noise by construction
    n2 = t$n2
  )

  res <- fs_svm(
    d, "y", "regression",
    nfolds = 3, kernel = "linear",
    tune_grid = data.frame(C = 1),
    feature_select = TRUE, select_method = "svm_rfe",
    seed = 5
  )

  expect_s3_class(res, "fs_result")
  expect_identical(res$task, "regression")
  sel <- res$details$selection
  expect_identical(sel$method, "svm_rfe")
  # The two genuine drivers must outrank the two noise columns.
  expect_setequal(sel$ranking[1:2], c("x1", "x2"))
  expect_setequal(sel$ranking[3:4], c("n1", "n2"))
  # Weights are real numbers for every predictor, not NA from a failed
  # extraction, and the criterion is non-negative (it is w^2).
  expect_true(all(is.finite(sel$scores)))
  expect_true(all(sel$scores >= 0))
  expect_identical(names(res$details$performance), c("RMSE", "Rsquared", "MAE"))
})

test_that("svm_center_scale applies reference statistics to new rows", {
  ref <- matrix(c(1, 2, 3, 4, 10, 10, 10, 10), ncol = 2,
                dimnames = list(NULL, c("a", "k")))
  new <- matrix(c(5, 11), ncol = 2, dimnames = list(NULL, c("a", "k")))
  out <- featR:::svm_center_scale(new, ref = ref)
  expect_equal(unname(out[1, "a"]), (5 - 2.5) / stats::sd(1:4))
  # zero-variance reference column: centered with the reference mean only
  expect_equal(unname(out[1, "k"]), 1)
})

test_that("SVM-RFE weights reproduce kernlab's decision values", {
  skip_if_not_installed("kernlab")
  skip_on_cran()

  # For each one-against-one machine, x %*% w - b must equal kernlab's own
  # decision value; the criterion is the sum of w^2 over the machines.
  x <- featR:::svm_center_scale(iris[, 1:4])
  fit <- featR:::svm_rfe_ksvm(x, iris$Species, "classification")
  xm <- kernlab::xmatrix(fit)
  cf <- kernlab::coef(fit)
  dec <- kernlab::predict(fit, x, type = "decision")
  expect_length(xm, 3L)
  ref <- numeric(4)
  for (i in seq_along(xm)) {
    w <- crossprod(xm[[i]], cf[[i]])
    expect_equal(as.numeric(x %*% w - kernlab::b(fit)[i]), dec[, i],
                 tolerance = 1e-8)
    ref <- ref + as.numeric(w)^2
  }
  expect_equal(unname(featR:::svm_rfe_weights(fit, colnames(x))), ref)
})

test_that("SVM-RFE size search re-ranks inside each fold (no selection bias)", {
  # REGRESSION: the size search used to score the top-k of a ranking computed
  # on ALL training rows, including each fold's held-out rows. On pure noise
  # that made the cross-validated accuracy of small subsets look far better
  # than chance (about 0.9 instead of 0.5).
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_on_cran()

  # Every elimination after the first (full-data) one must run on a strict
  # subset of the rows: the fold's training rows.
  seen_rows <- integer(0)
  real_eliminate <- featR:::svm_rfe_eliminate
  local_mocked_bindings(
    svm_rfe_eliminate = function(x, y, task, verbose = FALSE) {
      seen_rows <<- c(seen_rows, nrow(x))
      real_eliminate(x, y, task, verbose)
    },
    .package = "featR"
  )
  d <- svm_rfe_toy()
  set.seed(3)
  featR:::svm_rfe_rank(d[, c("x1", "x2", "n1", "n2")], d$y,
                       "classification", nfolds = 3)
  expect_length(seen_rows, 4L)           # full data + one per fold
  expect_identical(seen_rows[1], 60L)
  expect_true(all(seen_rows[-1] < 60L))
  expect_identical(sum(60L - seen_rows[-1]), 60L)  # folds partition the rows

  # Statistical check on pure noise: honest CV accuracy stays near chance.
  local_mocked_bindings(svm_rfe_eliminate = real_eliminate,
                        .package = "featR")
  set.seed(11)
  n <- 40
  x <- matrix(stats::rnorm(n * 64), n,
              dimnames = list(NULL, paste0("v", seq_len(64))))
  y <- factor(rep(c("a", "b"), n / 2))
  res <- featR:::svm_rfe_rank(x, y, "classification", nfolds = 5)
  expect_lt(max(res$size_scores), 0.8)
})

test_that("non-syntactic predictor names survive encoding without backticks", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_on_cran()

  # REGRESSION: dummyVars() copied the formula backticks into the encoded
  # names, so `selected` held "`a b`" instead of the user's column "a b".
  d <- svm_rfe_toy()
  names(d) <- c("y", "x 1", "x-2", "n1", "n2")
  d$`f g` <- factor(rep(c("u", "v", "w"), 20))

  dv <- featR:::svm_dummy_encoder(d, "y")
  enc <- featR:::svm_encode(dv, d)
  expect_false(any(grepl("`", names(enc), fixed = TRUE)))
  expect_true(all(c("x 1", "x-2", "f gv", "f gw") %in% names(enc)))

  res <- suppressWarnings(fs_svm(
    d, "y", "classification",
    nfolds = 3, tune_grid = data.frame(C = 1),
    feature_select = TRUE, n_features = 2, seed = 1
  ))
  expect_setequal(res$selected, c("x 1", "x-2"))
  expect_true(all(res$selected %in% names(d)))
  expect_false(any(grepl("`", names(res$scores), fixed = TRUE)))
})

test_that("n_cores > 1 stops its cluster and restores the foreach backend", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_if_not_installed("e1071")
  skip_if_not_installed("doParallel")
  skip_if_not_installed("foreach")
  skip_on_cran()
  skip_if(parallel::detectCores() < 2L, "needs 2 cores")

  foreach::registerDoSEQ()
  n_conn <- nrow(showConnections())
  res <- fs_svm(svm_toy_classification(), "y", "classification",
                nfolds = 3, tune_grid = data.frame(C = 1),
                n_cores = 2, seed = 3)
  expect_s3_class(res, "fs_result")
  expect_identical(nrow(showConnections()), n_conn)
  expect_identical(foreach::getDoParName(), "doSEQ")
})

test_that("fs_svm rejects a fractional seed before any work", {
  expect_error(
    fs_svm(svm_toy_classification(), "y", "classification", seed = 1.5),
    "'seed' must be a whole number"
  )
})

test_that("encoded names keep literal backticks and backslashes (svm_encode)", {
  # REGRESSION: svm_encode() deleted every backtick from dummyVars()'s names,
  # but backtick() escapes a literal backtick as \`, so column "a`b" was
  # reported as "a\b" -- the name of a different column.
  d <- data.frame(y = rnorm(20), `a\`b` = rnorm(20), `a\\b` = rnorm(20),
                  `\`ab` = rnorm(20), check.names = FALSE)
  dv <- svm_dummy_encoder(d, "y")
  enc <- svm_encode(dv, d)
  expect_identical(names(enc), c("a`b", "a\\b", "`ab"))
})

test_that("fs_svm rejects empty class labels and empty column names clearly", {
  # REGRESSION: an empty-string class failed inside caret with "subscript out
  # of bounds", and an empty column name with "undefined columns selected".
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  set.seed(41)
  n <- 40
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- factor(ifelse(d$x1 > 0, "", "b"))
  expect_error(fs_svm(d, "y", task = "classification", nfolds = 2),
               "empty-string class label")
  d$y <- factor(ifelse(d$x1 > 0, "a", "b"))
  names(d)[2] <- ""
  expect_error(fs_svm(d, "y", task = "classification", nfolds = 2),
               "empty or NA column names")
})

test_that("rf_rfe scores average each resample once, not once per surviving subset size", {
  # Second-round statistical review: caret::rfe() records a variable's
  # importance once for every subset size it survived to. Averaging all rows
  # weighted each resample by how far the variable survived there, i.e. by
  # how important it looked, inflating marginal predictors. "a" is worth 10
  # in Fold1 (survives sizes 1-3) and 0 in Fold2 (only the full size): its
  # mean importance across the two resamples is 5, not (3 * 10 + 0) / 4.
  rfe_obj <- list(variables = data.frame(
    Overall   = c(10, 10, 10, 1, 1, 0, 4),
    var       = c("a", "a", "a", "b", "b", "a", "b"),
    Variables = c(1L, 2L, 3L, 2L, 3L, 3L, 3L),
    Resample  = c("Fold1", "Fold1", "Fold1", "Fold1", "Fold1", "Fold2", "Fold2"),
    stringsAsFactors = FALSE
  ))
  out <- featR:::svm_rf_importance(rfe_obj, c("a", "b", "z"))
  expect_equal(out[["a"]], 5)
  expect_equal(out[["b"]], 2.5)
  expect_true(is.na(out[["z"]]))
})

test_that("seeded runs with n_cores > 1 are reproducible", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_if_not_installed("e1071")
  skip_if_not_installed("randomForest")
  skip_if_not_installed("doParallel")
  skip_if_not_installed("foreach")
  skip_on_cran()
  skip_if(parallel::detectCores() < 2L, "needs 2 cores")

  # REGRESSION: caret's default seeds = NA left each resample on whatever
  # worker stream it landed on (foreach load-balances), so the rf_rfe
  # importances and the tuning results changed between identical seeded runs.
  set.seed(1)
  n <- 150
  d <- data.frame(matrix(rnorm(n * 6), n))
  d$y <- factor(ifelse(d$X1 + 0.5 * d$X2 + rnorm(n) > 0, "a", "b"))
  run <- function() {
    r <- fs_svm(d, "y", "classification", feature_select = TRUE,
                select_method = "rf_rfe", seed = 7, n_cores = 2,
                tune_grid = data.frame(C = c(0.1, 1)))
    list(r$scores, r$model$results)
  }
  first <- run()
  for (i in 1:3) {
    expect_identical(run(), first)
  }
})

test_that("fs_svm rejects Inf in predictors or target before any engine runs", {
  # REGRESSION: only NA was checked, so Inf surfaced as caret's "missing value
  # where TRUE/FALSE needed", a misleading "near-constant" SVM-RFE error, or
  # kernlab's "No Support Vectors found".
  d <- svm_toy_regression()
  d1 <- d
  d1$x2[3] <- Inf
  expect_error(fs_svm(d1, "y", "regression"), "finite values.*x2")
  expect_error(fs_svm(d1, "y", "regression", feature_select = TRUE),
               "finite values.*x2")
  d2 <- d
  d2$y[3] <- -Inf
  expect_error(fs_svm(d2, "y", "regression"), "finite values.*y")
})

test_that("svm_coerce_target drops unused target levels before the split", {
  # REGRESSION: an unused level reached createDataPartition(), which warned
  # "Some classes have no records" on every call.
  d <- svm_toy_classification()
  d$y <- factor(as.character(d$y), levels = c("a", "b", "unused"))
  out <- featR:::svm_coerce_target(d, "y", "classification")
  expect_identical(levels(out$y), c("a", "b"))

  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_if_not_installed("e1071")
  skip_on_cran()
  expect_no_warning(fs_svm(d, "y", "classification", nfolds = 3,
                           tune_grid = data.frame(C = 1), seed = 1))
})

test_that("svm_align_levels uses the factor levels observed in training", {
  # REGRESSION: levels(col_train) of a subset factor is the FULL level set, so
  # a test-only level was not dropped and unused levels became all-zero
  # training dummies (selected, and warned about by caret/kernlab).
  f <- factor(c("p", "q", "p", "q", "rare"), levels = c("p", "q", "rare", "zz"))
  full <- data.frame(y = 1:5, g = f)
  train <- full[1:4, , drop = FALSE]
  test <- full[c(2, 5), , drop = FALSE]
  expect_warning(
    out <- featR:::svm_align_levels(train, test, "y"),
    "Dropped 1 test row"
  )
  expect_identical(levels(out$train_set$g), c("p", "q"))
  expect_identical(levels(out$test_set$g), c("p", "q"))
  expect_identical(nrow(out$test_set), 1L)

  # factor and character predictors now behave the same
  train_chr <- train
  train_chr$g <- as.character(train_chr$g)
  test_chr <- test
  test_chr$g <- as.character(test_chr$g)
  out_chr <- suppressWarnings(featR:::svm_align_levels(train_chr, test_chr, "y"))
  expect_identical(out_chr$test_set, out$test_set)
})

test_that("svm_center_scale treats round-off constants as constant and survives tiny units", {
  # REGRESSION: only sd == 0 was neutralised, so a column constant up to
  # round-off (0.3 vs 0.1 + 0.2, sd ~ 4e-17) was divided by its round-off sd
  # and became a full-weight indicator; a column in units of 1e-170 had sd
  # underflow to exactly 0 and was left unscaled.
  x <- cbind(k = rep(c(0.3, 0.1 + 0.2), 10), a = seq_len(20) * 1e-170,
             b = seq_len(20))
  out <- featR:::svm_center_scale(x)
  expect_lt(max(abs(out[, "k"])), 1e-10)
  expect_equal(unname(stats::sd(out[, "a"])), 1)
  expect_equal(unname(out[, "a"]), unname(out[, "b"]))

  skip_if_not_installed("kernlab")
  skip_on_cran()
  set.seed(2)
  n <- 40
  y <- factor(rep(c("a", "b"), each = 20))
  xx <- cbind(sig = stats::rnorm(n) + as.integer(y) * 2,
              z1 = stats::rnorm(n), z2 = stats::rnorm(n),
              const = ifelse(y == "b", 0.1 + 0.2, 0.3))
  r <- featR:::svm_rfe_rank(xx, y, "classification", n_features = 1)
  expect_identical(r$ranking[length(r$ranking)], "const")
  expect_identical(r$selected, "sig")
})

test_that("fs_svm stops when a dummy name collides with another column", {
  # REGRESSION: factor g with level "1" encodes to "g1" next to a numeric
  # "g1"; selection by name then kept the noise dummy and dropped the real
  # predictor (test R^2 0.04 instead of 0.99), and `selected` named the wrong
  # column.
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  set.seed(1)
  n <- 60
  d <- data.frame(g = factor(sample(c("0", "1"), n, TRUE)),
                  g1 = stats::rnorm(n), w = stats::rnorm(n))
  d$y <- 3 * d$g1 + stats::rnorm(n, sd = 0.3)
  expect_error(
    fs_svm(d, "y", "regression", feature_select = TRUE, n_features = 1,
           seed = 1, nfolds = 3),
    "collide with existing columns: g1"
  )
  d2 <- data.frame(g = factor(sample(c("a", "b"), n, TRUE)),
                   gb = stats::rnorm(n))
  d2$y <- 3 * (d2$g == "b") + stats::rnorm(n, sd = 0.3)
  expect_error(fs_svm(d2, "y", "regression", seed = 1, nfolds = 3),
               "collide with existing columns: gb")
})

test_that("regression predictions are an unnamed numeric vector", {
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_on_cran()
  res <- fs_svm(svm_toy_regression(), "y", "regression", nfolds = 3,
                tune_grid = data.frame(C = 1), seed = 1)
  expect_null(names(res$details$predictions))
  expect_type(res$details$predictions, "double")
  expect_length(res$details$predictions, nrow(res$details$test_set))
})

test_that("SVM-RFE warns when no candidate size could be scored", {
  # REGRESSION: when every fold failed, all p features were kept silently.
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_on_cran()
  x <- matrix(c(0.1, 0.9, -0.4, 0.3, 0.8, -0.6, 0.2, 0.5), 2,
              dimnames = list(NULL, paste0("v", 1:4)))
  expect_warning(
    r <- featR:::svm_rfe_rank(x, factor(c("a", "b")), "classification",
                              nfolds = 5),
    "could not score any candidate size.*keeping all 4"
  )
  expect_true(all(is.na(r$size_scores)))
  expect_length(r$selected, 4L)
})

test_that("rf_rfe truncation by n_features follows the reported scores", {
  # REGRESSION: truncation used caret's optVariables order (survivorship-
  # weighted), so a lower-scored feature could be kept over a higher one.
  skip_if_not_installed("caret")
  skip_if_not_installed("kernlab")
  skip_if_not_installed("e1071")
  skip_if_not_installed("randomForest")
  skip_on_cran()
  local_mocked_bindings(
    svm_feature_selection = function(x_enc, y, ...) {
      list(selected = c("x3", "x1", "x2"),
           scores = c(x1 = 3, x2 = 2, x3 = 1), fallback = FALSE)
    },
    .package = "featR"
  )
  res <- fs_svm(svm_toy_classification(), "y", "classification",
                nfolds = 3, tune_grid = data.frame(C = 1),
                feature_select = TRUE, select_method = "rf_rfe",
                n_features = 2, seed = 1)
  expect_identical(res$selected, c("x1", "x2"))
  expect_identical(res$details$selection$ranking[1:2], res$selected)
})

test_that("rf_rfe screens out constant predictors instead of hanging", {
  # REGRESSION: randomForest never returns on a constant size-1 subset, so
  # rf_rfe hung on a data set with a constant column.
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  skip_on_cran()
  set.seed(1)
  x <- data.frame(x1 = stats::rnorm(60), x2 = stats::rnorm(60), k = 0)
  y <- factor(ifelse(x$x1 + stats::rnorm(60, sd = 0.3) > 0, "a", "b"))
  out <- featR:::svm_feature_selection(x, y, rfe_folds = 3)
  expect_false("k" %in% out$selected)
  expect_identical(names(out$scores), c("x1", "x2", "k"))
  expect_true(is.na(out$scores[["k"]]))
  expect_error(featR:::svm_feature_selection(x["k"], y),
               "Every encoded predictor is constant")
})

test_that("rf_rfe fallback screens by permutation importance, not impurity", {
  # REGRESSION: the fallback kept every predictor with positive impurity
  # importance, which every predictor used in a split has -- no screening.
  skip_if_not_installed("caret")
  skip_if_not_installed("randomForest")
  skip_on_cran()
  local_mocked_bindings(rfe = function(...) stop("simulated rfe failure"),
                        .package = "caret")
  set.seed(5)
  n <- 200
  x <- as.data.frame(matrix(stats::rnorm(n * 22), n))
  y <- factor(ifelse(x$V1 + x$V2 + stats::rnorm(n, sd = 0.5) > 0, "a", "b"))
  expect_warning(
    out <- featR:::svm_feature_selection(x, y),
    "permutation importance"
  )
  expect_true(out$fallback)
  expect_true(all(c("V1", "V2") %in% out$selected))
  expect_lt(length(out$selected), 22L)
  expect_true(any(out$scores <= 0))
})
