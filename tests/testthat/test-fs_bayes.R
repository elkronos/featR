# Tests for fs_bayes().
#
# Argument-type validation in fs_bayes() runs BEFORE fs_require(c("brms",
# "loo")), so those tests need no skips and run everywhere. Data-content
# validation (target/predictor existence) happens AFTER fs_require(), so
# exercising it through the public API is gated on brms/loo; the same checks
# are also covered helper-level via featR::: so they run everywhere. The
# loo_compare()-driven selection rule is likewise tested at helper level with
# a hand-made comparison table, so it needs neither brms nor loo.

test_that("fs_bayes validates argument types before requiring brms/loo", {
  d <- data.frame(y = c(1.5, 2.5, 3.5), x1 = c(1, 2, 3))

  expect_error(fs_bayes(1L, "y", "x1"), "'data' must be a data.frame")
  expect_error(fs_bayes(d, 1, "x1"), "single non-empty character string")
  expect_error(fs_bayes(d, "y", 1:2),
               "'predictors' must be a character vector")
  expect_error(fs_bayes(d, "y", character(0)),
               "'predictors' must be a character vector")
  expect_error(fs_bayes(d, "y", c("x1", NA)),
               "'predictors' must be a character vector")
  expect_error(fs_bayes(d, "y", "x1", date_col = 5),
               "'date_col' must be a single non-empty")
  expect_error(fs_bayes(d, "y", "x1", brm_args = "iter"),
               "'brm_args' must be a list")
  expect_error(fs_bayes(d, "y", "x1", parallel_combinations = NA),
               "'parallel_combinations' must be TRUE or FALSE")
  expect_error(fs_bayes(d, "y", "x1", max_comb_size = 1.5),
               "'max_comb_size' must be a whole number")
  expect_error(fs_bayes(d, "y", "x1", sample_combinations = 0),
               "'sample_combinations' must be between 1 and Inf")
  expect_error(fs_bayes(d, "y", "x1", seed = "a"),
               "'seed' must be a single finite number")
  expect_error(fs_bayes(d, "y", "x1", verbose = "yes"),
               "'verbose' must be TRUE or FALSE")
  expect_error(fs_bayes(d, "y", "x1", n_cores = 0),
               "'n_cores' must be between 1 and Inf")
  expect_error(fs_bayes(d, "y", "x1", rule = "elbow"), "should be one of")
})

test_that("fs_bayes rejects the removed and renamed arguments", {
  d <- data.frame(y = c(1, 2, 3), x1 = c(3, 2, 1))

  expect_error(
    fs_bayes(d, "y", "x1", early_stop_threshold = 0.1),
    "unused argument"
  )
  # show_progress folded into verbose
  expect_error(
    fs_bayes(d, "y", "x1", show_progress = FALSE),
    "unused argument"
  )
  # response_col / predictor_cols were renamed to target / predictors
  expect_error(
    fs_bayes(d, response_col = "y", predictor_cols = "x1"),
    "unused argument"
  )
})

test_that("fs_bayes signature and defaults are stable", {
  fx <- formals(fs_bayes)
  expect_identical(
    names(fx),
    c("data", "target", "predictors", "date_col", "brm_family", "prior",
      "brm_args", "rule", "max_comb_size", "sample_combinations",
      "parallel_combinations", "seed", "verbose", "n_cores")
  )
  expect_null(fx$date_col)
  expect_null(fx$prior)
  expect_null(fx$max_comb_size)
  expect_null(fx$sample_combinations)
  expect_null(fx$seed)
  expect_identical(eval(fx$brm_args), list())
  expect_identical(eval(fx$rule), c("1se", "best"))
  expect_identical(fx$parallel_combinations, FALSE)
  expect_identical(fx$verbose, FALSE)
  expect_identical(fx$n_cores, 1L)
  expect_false(any(c("response_col", "predictor_cols", "show_progress") %in%
                     names(fx)))
})

test_that("bayes_validate_data catches missing or mistyped columns (helper-level)", {
  # fs_require(c("brms", "loo")) runs before bayes_validate_data() inside
  # fs_bayes(), so these data checks are unreachable through the public API
  # on machines without brms; test the helper directly so they run everywhere.
  d <- data.frame(y = c(1, 2, 3), x1 = c(1, 2, 3), x2 = c(3, 2, 1))

  expect_error(featR:::bayes_validate_data(d, "nope", "x1"),
               "Response column 'nope' not found")
  expect_error(featR:::bayes_validate_data(d, "y", character(0)),
               "At least one predictor column")
  expect_error(featR:::bayes_validate_data(d, "y", c("x1", "zz")),
               "Missing predictor columns: zz")
  expect_error(featR:::bayes_validate_data(d, "y", c("y", "x1")),
               "must not also appear in 'predictors'")
  expect_error(featR:::bayes_validate_data(d, "y", "x1", date_col = "when"),
               "Date column 'when' not found")

  d_fac <- data.frame(y = factor(c("a", "b", "a")), x1 = c(1, 2, 3))
  expect_error(
    featR:::bayes_validate_data(d_fac, "y", "x1"),
    "gaussian\\(\\) family requires a numeric response"
  )

  expect_true(featR:::bayes_validate_data(d, "y", c("x1", "x2")))
})

test_that("bayes_task_from_family maps brms families onto featR tasks", {
  task_of <- featR:::bayes_task_from_family

  expect_identical(task_of(stats::gaussian()), "regression")
  expect_identical(task_of(stats::poisson()), "regression")
  expect_identical(task_of(stats::binomial()), "classification")
  expect_identical(task_of(list(family = "bernoulli")), "classification")
  expect_identical(task_of(list(family = "cumulative")), "classification")
  # anything unrecognisable falls back to regression rather than erroring
  expect_identical(task_of(NULL), "regression")
  expect_identical(task_of(list()), "regression")
  expect_identical(task_of(list(family = "made_up")), "regression")
})

test_that("bayes_generate_predictor_combinations enumerates and caps sizes", {
  gen <- featR:::bayes_generate_predictor_combinations

  combos <- gen(c("a", "b", "c"))
  expect_length(combos, 7L) # 2^3 - 1 non-empty subsets
  expect_identical(lengths(combos), c(1L, 1L, 1L, 2L, 2L, 2L, 3L))
  expect_true(all(vapply(combos, is.character, logical(1))))

  expect_length(gen(c("a", "b", "c"), max_comb_size = 2), 6L)
  # max_comb_size beyond the predictor count is capped, not an error
  expect_length(gen(c("a", "b"), max_comb_size = 5), 3L)
  # asking for more sampled combinations than exist leaves the set untouched
  expect_length(gen(c("a", "b", "c"), sample_combinations = 100), 7L)

  expect_error(gen(character(0)), "at least one predictor")
  expect_error(gen(c("a", "b"), max_comb_size = 1.5),
               "'max_comb_size' must be a whole number")
})

test_that("combination sampling is seed-reproducible and leaves RNG state alone", {
  gen <- featR:::bayes_generate_predictor_combinations

  set.seed(1)
  invisible(stats::runif(1))
  state_before <- .Random.seed

  s1 <- gen(letters[1:5], sample_combinations = 4, seed = 99)
  # a supplied seed must not disturb the caller's RNG state
  expect_identical(.Random.seed, state_before)

  s2 <- gen(letters[1:5], sample_combinations = 4, seed = 99)
  expect_identical(s1, s2)
  expect_length(s1, 4L)
})

test_that("bayes_add_week_feature builds ISO week ids (helper-level)", {
  dt <- data.table::as.data.table(
    data.frame(y = c(1, 2), x = c(3, 4),
               when = as.Date(c("2024-01-01", "2024-12-31")))
  )
  out <- featR:::bayes_add_week_feature(dt, "when", "x")
  expect_true("iso_week_id" %in% names(out$data))
  expect_identical(out$predictors, c("x", "iso_week_id"))
  # ISO year boundaries: 2024-01-01 is 2024-W01 and 2024-12-31 is 2025-W01
  expect_identical(out$data$iso_week_id, c(202401L, 202501L))
})

test_that("bayes_pick_model implements the 1se and best rules", {
  pick <- featR:::bayes_pick_model

  results <- list(
    list(preds = c("x1", "x2", "x3"), loo_val = -10.0), # highest elpd
    list(preds = "x1",                loo_val = -10.8), # within 1 se, smallest
    list(preds = c("x1", "x2"),       loo_val = -10.4), # within 1 se
    list(preds = "x2",                loo_val = -30.0)  # far worse
  )
  idx <- 1:4

  # loo_compare() orders rows best-first and names them after the list
  comparison <- matrix(
    c(0, -0.4, -0.8, -20,
      0,  1.0,  1.2,   2),
    ncol = 2,
    dimnames = list(c("model1", "model3", "model2", "model4"),
                    c("elpd_diff", "se_diff"))
  )

  # 'best' keeps the raw elpd maximum, i.e. the first comparison row
  expect_identical(pick(results, idx, comparison, "best"), 1L)

  # '1se' keeps the most parsimonious model whose elpd difference from the
  # best is within one standard error of that difference
  expect_identical(pick(results, idx, comparison, "1se"), 2L)

  # model4 is outside one standard error, so parsimony does not rescue it
  expect_false(identical(pick(results, idx, comparison, "1se"), 4L))

  # without a usable comparison table both rules fall back to the raw maximum
  expect_identical(pick(results, idx, NULL, "1se"), 1L)
  expect_identical(pick(results, idx, NULL, "best"), 1L)
  expect_identical(pick(results, idx, matrix(1:4, ncol = 2), "1se"), 1L)

  # no usable model at all
  expect_identical(pick(results, integer(0), comparison, "1se"), NA_integer_)
})

test_that("bayes_pick_model applies the 1se rule to a data.frame comparison", {
  # REGRESSION: loo::loo_compare() returns a matrix in some loo versions and a
  # data.frame in others. An earlier is.matrix() guard silently disabled the
  # whole 1se rule whenever loo returned a data.frame, so the documented
  # default behaved exactly like rule = "best". Both containers must work.
  pick <- featR:::bayes_pick_model

  results <- list(
    list(preds = c("x1", "x2", "x3"), loo_val = -10.0),
    list(preds = "x1",                loo_val = -10.8),
    list(preds = c("x1", "x2"),       loo_val = -10.4),
    list(preds = "x2",                loo_val = -30.0)
  )
  idx <- 1:4

  cmp_df <- data.frame(
    elpd_diff = c(0, -0.4, -0.8, -20),
    se_diff   = c(0,  1.0,  1.2,   2),
    row.names = c("model1", "model3", "model2", "model4")
  )

  # The parsimonious-within-1-SE model, not the raw elpd maximum
  expect_identical(pick(results, idx, cmp_df, "1se"), 2L)
  expect_identical(pick(results, idx, cmp_df, "best"), 1L)

  # A two-dimensional object missing the required columns still falls back
  bad_df <- data.frame(a = 1:2, b = 3:4,
                       row.names = c("model1", "model2"))
  expect_identical(pick(results, idx, bad_df, "1se"), 1L)
})

test_that("bayes_loo_comparison declines to compare fewer than two loo objects", {
  cmp <- featR:::bayes_loo_comparison
  results <- list(list(loo = NULL), list(loo = NULL))

  expect_null(cmp(results, 1L))
  # non-loo objects are refused rather than passed to loo::loo_compare()
  expect_null(cmp(results, 1:2))
})

test_that("fs_bayes reports missing target and predictor columns", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_on_cran() # loading the brms namespace alone is slow

  d <- data.frame(y = c(1, 2, 3, 4), x1 = c(4, 3, 2, 1))
  expect_error(fs_bayes(d, "nope", "x1", verbose = FALSE),
               "Response column 'nope' not found")
  expect_error(fs_bayes(d, "y", c("x1", "zz"), verbose = FALSE),
               "Missing predictor columns: zz")
})

test_that("fs_bayes end-to-end smoke run returns the documented fs_result", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  # Every candidate model compiles a Stan program, so this needs a working
  # C++ toolchain wired up to rstan/cmdstanr -- not merely an installed brms.
  # CI runners and CRAN check machines frequently have the package without a
  # usable compiler, where every fit returns NULL and the run legitimately
  # errors. Opt in explicitly on a machine known to have the toolchain:
  #   FEATR_TEST_BRMS=true Rscript -e 'devtools::test()'
  skip_if_not(
    identical(Sys.getenv("FEATR_TEST_BRMS"), "true"),
    "set FEATR_TEST_BRMS=true to run the brms end-to-end test"
  )

  x1 <- seq(-2, 2, length.out = 40)
  x2 <- rep(c(-1, 1), 20)
  d <- data.frame(y = 1 + 2 * x1 + sin(seq_len(40)), x1 = x1, x2 = x2)

  # fs_bayes() no longer hardcodes warmup, so brms' own default of iter / 2
  # applies; an explicit warmup below iter is still accepted.
  res <- suppressWarnings(suppressMessages(fs_bayes(
    d, "y", c("x1", "x2"),
    brm_args = list(chains = 1, iter = 300, refresh = 0),
    verbose = FALSE
  )))

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "bayes")
  expect_identical(res$task, "regression")
  expect_s3_class(res$model, "brmsfit")
  expect_false(is.null(res$call))

  # no per-feature score is comparable across combination models
  expect_null(res$scores)
  expect_type(res$selected, "character")
  expect_gt(length(res$selected), 0L)
  expect_true(all(res$selected %in% c("x1", "x2")))

  expect_named(
    res$details,
    c("data", "mae", "rmse", "formula", "best_elpd", "loo_comparison",
      "n_failed_fits", "n_features")
  )
  expect_identical(res$details$n_features, 2L)
  expect_identical(res$details$n_failed_fits, 0L)
  expect_true(data.table::is.data.table(res$details$data))
  expect_true(all(c("fitted_values", "residuals", "abs_residuals",
                    "squared_residuals") %in% names(res$details$data)))
  expect_true(is.numeric(res$details$mae))
  expect_true(is.numeric(res$details$rmse))
  expect_match(res$details$formula, "^y ~ ")

  # The statistical fix: selection goes through loo::loo_compare(). Assert the
  # contract (a table with the comparison columns), not the container class --
  # loo_compare() has returned a matrix in some versions and a data.frame in
  # others, and either is fine here.
  expect_false(is.null(res$details$loo_comparison))
  expect_true(all(c("elpd_diff", "se_diff") %in%
                    colnames(res$details$loo_comparison)))
  expect_gte(nrow(res$details$loo_comparison), 2L)

  expect_output(print(res), "bayes")
})
