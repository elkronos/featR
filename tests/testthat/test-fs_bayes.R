# Tests for fs_bayes().
#
# Argument-type validation in fs_bayes() runs BEFORE fs_require(c("brms",
# "loo")), so those tests need no skips and run everywhere. Data-content
# validation (response/predictor existence) happens AFTER fs_require(), so
# exercising it through the public API is gated on brms/loo; the same checks
# are also covered helper-level via featR::: so they run everywhere.

test_that("fs_bayes validates argument types before requiring brms/loo", {
  d <- data.frame(y = c(1.5, 2.5, 3.5), x1 = c(1, 2, 3))

  expect_error(fs_bayes(1L, "y", "x1"), "'data' must be a data.frame")
  expect_error(fs_bayes(d, 1, "x1"), "single non-empty character string")
  expect_error(fs_bayes(d, "y", 1:2),
               "'predictor_cols' must be a character vector")
  expect_error(fs_bayes(d, "y", character(0)),
               "'predictor_cols' must be a character vector")
  expect_error(fs_bayes(d, "y", c("x1", NA)),
               "'predictor_cols' must be a character vector")
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
  expect_error(fs_bayes(d, "y", "x1", show_progress = "yes"),
               "'show_progress' must be TRUE or FALSE")
})

test_that("fs_bayes rejects the removed early_stop_threshold argument", {
  d <- data.frame(y = c(1, 2, 3), x1 = c(3, 2, 1))
  expect_error(
    fs_bayes(d, "y", "x1", early_stop_threshold = 0.1),
    "unused argument"
  )
})

test_that("fs_bayes signature and defaults are stable", {
  fx <- formals(fs_bayes)
  expect_identical(
    names(fx),
    c("data", "response_col", "predictor_cols", "date_col", "brm_family",
      "prior", "brm_args", "parallel_combinations", "n_cores",
      "max_comb_size", "sample_combinations", "seed", "show_progress",
      "verbose")
  )
  expect_null(fx$date_col)
  expect_null(fx$prior)
  expect_null(fx$n_cores)
  expect_null(fx$max_comb_size)
  expect_null(fx$sample_combinations)
  expect_null(fx$seed)
  expect_identical(fx$parallel_combinations, FALSE)
  expect_identical(fx$show_progress, TRUE)
  expect_identical(fx$verbose, TRUE)
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
  expect_error(featR:::bayes_validate_data(d, "y", "x1", date_col = "when"),
               "Date column 'when' not found")

  d_fac <- data.frame(y = factor(c("a", "b", "a")), x1 = c(1, 2, 3))
  expect_error(
    featR:::bayes_validate_data(d_fac, "y", "x1"),
    "gaussian\\(\\) family requires a numeric response"
  )

  expect_true(featR:::bayes_validate_data(d, "y", c("x1", "x2")))
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
  expect_identical(out$predictor_cols, c("x", "iso_week_id"))
  # ISO year boundaries: 2024-01-01 is 2024-W01 and 2024-12-31 is 2025-W01
  expect_identical(out$data$iso_week_id, c(202401L, 202501L))
})

test_that("fs_bayes reports missing response and predictor columns", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_on_cran() # loading the brms namespace alone is slow

  d <- data.frame(y = c(1, 2, 3, 4), x1 = c(4, 3, 2, 1))
  expect_error(fs_bayes(d, "nope", "x1", verbose = FALSE),
               "Response column 'nope' not found")
  expect_error(fs_bayes(d, "y", c("x1", "zz"), verbose = FALSE),
               "Missing predictor columns: zz")
})

test_that("fs_bayes end-to-end smoke run returns the documented elements", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")

  x1 <- seq(-2, 2, length.out = 40)
  x2 <- rep(c(-1, 1), 20)
  d <- data.frame(y = 1 + 2 * x1 + sin(seq_len(40)), x1 = x1, x2 = x2)

  # fs_bayes() no longer hardcodes warmup, so brms' own default of iter / 2
  # applies; an explicit warmup below iter is still accepted.
  res <- suppressWarnings(suppressMessages(fs_bayes(
    d, "y", c("x1", "x2"),
    brm_args = list(chains = 1, iter = 300, refresh = 0),
    show_progress = FALSE,
    verbose = FALSE
  )))

  expect_named(
    res,
    c("Model", "Data", "MAE", "RMSE", "SelectedPredictors",
      "SelectedFormula", "BestELPD")
  )
})
