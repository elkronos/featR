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

  # A family generator passed without parentheses used to die with
  # "object of type 'closure' is not subsettable".
  expect_error(
    featR:::bayes_validate_data(d, "y", "x1", brm_family = stats::gaussian),
    "Note the parentheses"
  )
  expect_error(
    featR:::bayes_validate_data(d, "y", "x1", brm_family = "gaussian"),
    "must be a family object"
  )
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

test_that("sampling draws distinct subsets without enumerating them", {
  # REGRESSION: every subset used to be materialized with combn() before
  # sampling, so `sample_combinations` blew up on memory at exactly the scale
  # it exists to bound. 28 predictors is 2^28 - 1 = 268435455 subsets, which
  # cannot be enumerated; this must return promptly.
  gen <- featR:::bayes_generate_predictor_combinations
  preds <- paste0("x", seq_len(28))

  drawn <- gen(preds, sample_combinations = 25, seed = 7)
  expect_length(drawn, 25L)
  expect_true(all(vapply(drawn, is.character, logical(1))))
  # Every subset is non-empty, within range, and drawn from the pool
  expect_true(all(lengths(drawn) >= 1L))
  expect_true(all(lengths(drawn) <= length(preds)))
  expect_true(all(unlist(drawn) %in% preds))
  # ... and all 25 are distinct
  keys <- vapply(drawn, function(s) paste(sort(s), collapse = "|"),
                 character(1L))
  expect_identical(anyDuplicated(keys), 0L)

  # Reproducible under a seed, and different without one being shared
  expect_identical(gen(preds, sample_combinations = 25, seed = 7), drawn)
  expect_false(identical(gen(preds, sample_combinations = 25, seed = 8), drawn))

  # max_comb_size still bounds the subset sizes on the sampled path
  small <- gen(preds, max_comb_size = 3, sample_combinations = 20, seed = 3)
  expect_length(small, 20L)
  expect_true(all(lengths(small) <= 3L))
})

test_that("an unbounded search errors instead of exhausting memory", {
  gen <- featR:::bayes_generate_predictor_combinations
  preds <- paste0("x", seq_len(28))

  expect_error(gen(preds), "Reduce 'max_comb_size'")
  expect_error(gen(preds), "sample_combinations")
  # A bounded search of the same predictors is fine.
  expect_length(gen(preds, max_comb_size = 2), 28L + choose(28L, 2L))
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

test_that("bayes_pick_model reads model labels from loo >= 2.10's 'model' column", {
  # REGRESSION: loo 2.10.0 changed loo_compare() to return a data.frame that
  # ends with `rownames(comp) <- NULL`, moving the model labels into a new
  # "model" column. Reading rownames() there yields "1", "2", ... -- the
  # positions of the elpd-sorted rows, not the positions in `results` -- which
  # passed every guard and made both rules pick the wrong model. The mock below
  # mirrors that real shape; do not give it rownames.
  pick <- featR:::bayes_pick_model

  results <- list(
    list(preds = c("x1", "x2", "x3"), loo_val = -10.0), # highest elpd
    list(preds = "x1",                loo_val = -10.8), # within 1 se, smallest
    list(preds = c("x1", "x2"),       loo_val = -10.4), # within 1 se
    list(preds = "x2",                loo_val = -30.0)  # far worse
  )
  idx <- 1:4

  cmp <- data.frame(
    model     = c("model1", "model3", "model2", "model4"),
    elpd_diff = c(0, -0.4, -0.8, -20),
    se_diff   = c(0,  1.0,  1.2,   2),
    stringsAsFactors = FALSE
  )
  # loo >= 2.10.0 leaves the automatic row labels, so rownames() carries no
  # model information at all. Assert that, so the mock cannot drift back to
  # the pre-2.10 shape and start passing for the wrong reason.
  expect_identical(rownames(cmp), as.character(1:4))

  expect_identical(pick(results, idx, cmp, "best"), 1L)
  expect_identical(pick(results, idx, cmp, "1se"), 2L)

  # Labels that are not the "model<i>" names bayes_loo_comparison() assigns are
  # not an invertible mapping, so both rules fall back to the raw elpd maximum
  # rather than guessing.
  cmp_unknown <- cmp
  cmp_unknown$model <- c("a", "b", "c", "d")
  expect_identical(pick(results, idx, cmp_unknown, "1se"), 1L)
  expect_identical(pick(results, idx, cmp_unknown, "best"), 1L)
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

test_that("fs_bayes rejects a fractional seed up front", {
  d <- data.frame(y = c(1.5, 2.5, 3.5), x1 = c(1, 2, 3))
  expect_error(fs_bayes(d, "y", "x1", seed = 1.5),
               "'seed' must be a whole number")
})

test_that("bayes_fit_model records the sampler warnings it muffles", {
  skip_if_not_installed("brms")
  skip_on_cran() # loading the brms namespace alone is slow

  # REGRESSION: every brm() warning (divergences, R-hat, ESS) was muffled and
  # discarded, so an unconverged model could be selected without any signal.
  local_mocked_bindings(
    brm = function(...) {
      warning("There were 12 divergent transitions after warmup.")
      structure(list(), class = "fake_brmsfit")
    },
    .package = "brms"
  )
  expect_silent(
    fit <- featR:::bayes_fit_model(
      data.frame(y = 1:3, x = 1:3), "y ~ x", stats::gaussian(), NULL, list()
    )
  )
  expect_s3_class(fit$model, "fake_brmsfit")
  expect_identical(fit$warnings,
                   "There were 12 divergent transitions after warmup.")
})

test_that("bayes_warn_diagnostics summarizes sampler and Pareto-k problems", {
  results <- list(
    list(preds = "x1", model = "m", formula_str = "y ~ x1",
         fit_warnings = "The largest R-hat is 1.07", n_bad_k = 0L),
    list(preds = "x2", model = "m", formula_str = "y ~ x2",
         fit_warnings = character(0), n_bad_k = 3L),
    list(preds = "x3", model = NULL, formula_str = "y ~ x3",
         fit_warnings = "ignored: failed fit", n_bad_k = NA_integer_)
  )
  w <- character(0)
  withCallingHandlers(
    out <- featR:::bayes_warn_diagnostics(results, chosen = 1L),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 2L)
  expect_match(w[1], "1 of 2 fitted model\\(s\\), INCLUDING the selected")
  expect_match(w[1], "R-hat is 1.07", fixed = TRUE)
  expect_match(w[2], "1 of 2 fitted model\\(s\\) have observations with Pareto k")
  expect_false(grepl("INCLUDING", w[2]))

  clean <- list(list(preds = "x1", model = "m", formula_str = "y ~ x1",
                     fit_warnings = character(0), n_bad_k = 0L))
  expect_silent(featR:::bayes_warn_diagnostics(clean, 1L))
})

test_that("fs_bayes surfaces diagnostics of the muffled fits", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_on_cran()

  # Pipeline test without Stan: the fits are mocked, everything after them
  # (selection, diagnostics, result assembly) is real.
  fake_eval <- function(preds, data, target, brm_family, prior, brm_args,
                        verbose = FALSE) {
    list(preds = preds, model = structure(list(), class = "fake"),
         loo = NULL, loo_val = -10 - length(preds),
         formula_str = paste("y ~", paste(preds, collapse = " + ")),
         fit_warnings = if ("x2" %in% preds) "divergent transitions" else
           character(0),
         n_bad_k = if (length(preds) == 1L) 2L else 0L)
  }
  local_mocked_bindings(
    bayes_evaluate_combination = fake_eval,
    bayes_add_metrics_to_data = function(data, model, verbose = FALSE) data,
    .package = "featR"
  )
  d <- data.frame(y = c(1, 2, 3, 4), x1 = c(4, 3, 1, 2), x2 = c(1, 1, 2, 2))
  w <- character(0)
  res <- withCallingHandlers(
    fs_bayes(d, "y", c("x1", "x2")),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  # loo objects are NULL, so the comparison is unavailable and the raw elpd
  # maximum (the single-predictor x1 model) is selected.
  expect_identical(res$selected, "x1")
  expect_true(any(grepl("sampling warnings for 2 of 3", w)))
  expect_true(any(grepl("2 of 3 fitted model\\(s\\), INCLUDING the selected model have observations with Pareto k", w)))
})

test_that("bayes_validate_data guards iso_week_id, duplicates and bad dates (helper-level)", {
  # REGRESSION: with date_col, a column named "iso_week_id" -- even the
  # target -- was overwritten by reference with the week ids, so the search
  # regressed the target on its own replacement. Repeated predictors produced
  # duplicate candidate models ("x1 + x1"); a duplicated column name silently
  # used its first copy; an unparseable date column failed with as.Date()'s
  # bare "character string is not in a standard unambiguous format".
  d <- data.frame(iso_week_id = c(1, 2, 3), x1 = c(1, 2, 3),
                  when = as.Date("2024-01-01") + c(0, 7, 14))
  expect_error(featR:::bayes_validate_data(d, "iso_week_id", "x1",
                                           date_col = "when"),
               "adds a predictor named 'iso_week_id'")
  d$y <- c(3, 1, 2)
  expect_error(featR:::bayes_validate_data(d, "y", c("x1", "iso_week_id"),
                                           date_col = "when"),
               "adds a predictor named 'iso_week_id'")
  expect_true(featR:::bayes_validate_data(d, "y", c("x1", "iso_week_id")))

  expect_error(featR:::bayes_validate_data(d, "y", c("x1", "x1")),
               "must not contain duplicates; repeated: x1")
  d2 <- d
  names(d2)[names(d2) == "iso_week_id"] <- "x1"
  expect_error(featR:::bayes_validate_data(d2, "y", "x1"),
               "duplicated column names among the columns used: x1")

  dt <- data.table::data.table(y = 1:2, x = 3:4, when = c("soon", "later"))
  expect_error(featR:::bayes_add_week_feature(dt, "when", "x"),
               "Date column 'when' could not be converted to Date")
})

# ---- Regression tests for the fs_bayes review fixes -------------------------
# These mock brms::brm() / loo::loo() so no Stan program is ever compiled.

collect_warnings <- function(expr) {
  w <- character(0)
  value <- withCallingHandlers(expr, warning = function(cond) {
    w <<- c(w, conditionMessage(cond))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = w)
}

fake_loo <- function(elpd) {
  structure(
    list(estimates = matrix(c(elpd, 1), 1L, 2L,
                            dimnames = list("elpd_loo", c("Estimate", "SE"))),
         diagnostics = list(pareto_k = rep(0.1, 5L))),
    class = c("psis_loo", "loo")
  )
}

bayes_test_data <- function(n = 20L) {
  data.frame(y = sin(seq_len(n)), a = cos(seq_len(n)),
             b = seq_len(n) %% 3)
}

test_that("a predictor named 'data' works with date_col", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_on_cran()
  # REGRESSION: data[!is.na(data[["iso_week_id"]])] resolved `data` to the
  # predictor column inside data.table's i -> "subscript out of bounds".
  local_mocked_bindings(
    bayes_evaluate_combination = function(preds, data, target, brm_family,
                                          prior, brm_args, verbose = FALSE) {
      list(preds = preds, model = structure(list(), class = "fake"),
           loo = NULL, loo_val = -10 - length(preds),
           formula_str = paste("y ~", paste(preds, collapse = " + ")),
           fit_warnings = character(0), n_bad_k = 0L)
    },
    bayes_add_metrics_to_data = function(data, model, verbose = FALSE) data,
    .package = "featR"
  )
  d <- data.frame(y = sin(1:12), data = cos(1:12),
                  dt = as.Date("2024-01-01") + 7 * (0:11))
  res <- fs_bayes(d, "y", "data", date_col = "dt")
  expect_identical(res$selected, "data")
  expect_equal(nrow(res$details$data), 12L)
})

test_that("metric column names are guarded and MAE never partial-matches", {
  # REGRESSION: a target/predictor named 'residuals' (etc.) was overwritten
  # in details$data, and `$abs_residuals` partial-matched a user column.
  d <- data.frame(y = c(1, 2, 3), residuals = c(3, 1, 2))
  expect_error(featR:::bayes_validate_data(d, "y", "residuals"),
               "would overwrite the column\\(s\\) used .*: residuals")
  names(d)[1] <- "fitted_values"
  names(d)[2] <- "x"
  expect_error(featR:::bayes_validate_data(d, "fitted_values", "x"),
               "fitted_values")

  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_on_cran()
  local_mocked_bindings(
    bayes_evaluate_combination = function(preds, data, target, brm_family,
                                          prior, brm_args, verbose = FALSE) {
      list(preds = preds, model = structure(list(), class = "fake"),
           loo = NULL, loo_val = -10, formula_str = "y ~ abs_residuals_cm",
           fit_warnings = character(0), n_bad_k = 0L)
    },
    # Families without an Estimate matrix add no metric columns.
    bayes_add_metrics_to_data = function(data, model, verbose = FALSE) data,
    .package = "featR"
  )
  d2 <- data.frame(y = c(1, 2, 3, 4), abs_residuals_cm = c(100, 200, 300, 400))
  res <- fs_bayes(d2, "y", "abs_residuals_cm")
  expect_true(is.na(res$details$mae))
  expect_true(is.na(res$details$rmse))
})

test_that("fs_bayes rejects Inf before any model is fitted", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_on_cran()
  # REGRESSION: na.omit() kept Inf, so every Stan fit failed behind a
  # misleading "toolchain" error after the whole search had run.
  local_mocked_bindings(brm = function(...) stop("brm must not be called"),
                        .package = "brms")
  d <- bayes_test_data()
  d$a[3] <- Inf
  expect_error(fs_bayes(d, "y", c("a", "b")),
               "fs_bayes\\(\\) requires finite values; column\\(s\\) with Inf/-Inf: a")
  d <- bayes_test_data()
  d$y[5] <- -Inf
  expect_error(fs_bayes(d, "y", c("a", "b")), "Inf/-Inf: y")
})

test_that("loo() is pinned to the brms core budget, not getOption('mc.cores')", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_on_cran()
  withr::local_options(mc.cores = 4L)
  seen_cores <- list()
  local_mocked_bindings(brm = function(...) structure(list(), class = "fake"),
                        .package = "brms")
  local_mocked_bindings(
    loo = function(x, ...) {
      seen_cores[[length(seen_cores) + 1L]] <<- list(...)$cores
      fake_loo(-10)
    },
    .package = "loo"
  )
  out <- featR:::bayes_evaluate_combination(
    "a", bayes_test_data(), "y", stats::gaussian(), NULL,
    list(chains = 1L, cores = 1L)
  )
  expect_equal(out$loo_val, -10)
  expect_identical(seen_cores, list(1L))
})

test_that("fs_bayes(seed=) seeds the samplers and leaves the caller's RNG alone", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_on_cran()
  # REGRESSION: brm(seed = NA) made rstan draw a Stan seed from the caller's
  # global stream once per fit, so seeded calls moved the caller's RNG and
  # sampled differently every time.
  seeds <- numeric(0)
  local_mocked_bindings(
    brm = function(..., seed = NA) {
      seeds <<- c(seeds, seed)
      if (is.na(seed)) invisible(sample.int(10L, 1L)) # rstan's check_seed()
      stop("stop after recording the seed")
    },
    .package = "brms"
  )
  run <- function(...) {
    seeds <<- numeric(0)
    suppressWarnings(try(fs_bayes(bayes_test_data(), "y", c("a", "b"), ...),
                         silent = TRUE))
    seeds
  }
  set.seed(99)
  s0 <- .Random.seed
  s1 <- run(seed = 1)
  expect_identical(.Random.seed, s0)
  s2 <- run(seed = 1)
  expect_length(s1, 3L)
  expect_false(anyNA(s1))
  expect_identical(s1, s2)
  expect_length(unique(s1), 3L)
  expect_false(identical(run(seed = 2), s1))
  # An explicit brm_args$seed still wins, for every model.
  expect_identical(run(seed = 1, brm_args = list(seed = 7)), rep(7, 3))
  # No seed: unchanged behaviour (brms default NA).
  expect_true(all(is.na(run())))
})

test_that("fitted models whose loo() fails are reported, not dropped silently", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_on_cran()
  local_mocked_bindings(
    brm = function(formula, ...) {
      structure(list(f = deparse(formula)), class = "fake")
    },
    .package = "brms"
  )
  local_mocked_bindings(
    loo = function(x, ...) {
      if (grepl("a + b", x$f, fixed = TRUE)) stop("loo boom")
      fake_loo(if (grepl("a", x$f)) -10 else -50)
    },
    loo_compare = function(x, ...) stop("compare boom"),
    .package = "loo"
  )
  local_mocked_bindings(
    bayes_add_metrics_to_data = function(data, model, verbose = FALSE) data,
    .package = "featR"
  )
  out <- collect_warnings(fs_bayes(bayes_test_data(), "y", c("a", "b")))
  res <- out$value
  expect_identical(res$selected, "a")
  expect_identical(res$details$n_loo_failed, 1L)
  expect_identical(res$details$n_compared, 2L)
  expect_identical(res$details$n_failed_fits, 0L)
  expect_true(any(grepl(
    "1 of 3 fitted model\\(s\\) have no finite elpd_loo because loo\\(\\) failed.*a \\+ b.*loo boom",
    out$warnings)))
  # D6: a failing loo_compare() is announced rather than silently turning
  # rule = "1se" into "best".
  expect_true(any(grepl("loo_compare\\(\\) failed \\(compare boom\\).*1se",
                        out$warnings)))
})

test_that("brm() error messages are quoted in the failure warning and error", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_on_cran()
  local_mocked_bindings(
    brm = function(...) stop("Stan model failed: invalid prior"),
    .package = "brms"
  )
  w <- character(0)
  err <- tryCatch(
    withCallingHandlers(
      fs_bayes(bayes_test_data(), "y", c("a", "b")),
      warning = function(cond) {
        w <<- c(w, conditionMessage(cond))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "No valid models were fitted")
  expect_match(err, "invalid prior", fixed = TRUE)
  expect_true(any(grepl("3 of 3 model fits failed.*First error .*invalid prior",
                        w)))

  fit <- featR:::bayes_fit_model(bayes_test_data(), "y ~ a", stats::gaussian(),
                                 NULL, list())
  expect_null(fit$model)
  expect_identical(fit$error, "Stan model failed: invalid prior")
})

test_that("bayes_loo_comparison warns when loo_compare() fails (helper-level)", {
  skip_if_not_installed("loo")
  local_mocked_bindings(loo_compare = function(x, ...) stop("compare boom"),
                        .package = "loo")
  results <- list(list(loo = fake_loo(-10)), list(loo = fake_loo(-11)))
  expect_warning(
    out <- featR:::bayes_loo_comparison(results, 1:2),
    "loo_compare\\(\\) failed \\(compare boom\\)"
  )
  expect_null(out)
})
