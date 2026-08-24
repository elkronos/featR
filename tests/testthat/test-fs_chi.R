# Tests for fs_chi().
#
# fs_chi() only touches suggested packages (furrr/future) when parallel =
# TRUE. Every test below uses the sequential path, which relies solely on
# Imports (data.table, withr), so no skips are required anywhere.

chi_result_cols <- c("feature", "n", "df", "p_value", "adj_p_value",
                     "significant", "method", "correction_applied",
                     "min_expected")

chi_detail_names <- c("results", "sig_level", "p_adjust_method", "n_features")

test_that("fs_chi validates scalar options and data", {
  d <- data.frame(
    f      = factor(rep(c("A", "B"), 5)),
    target = factor(rep(c("Y", "N"), 5))
  )

  expect_error(fs_chi(d, "target", sig_level = 0), "strictly between 0 and 1")
  expect_error(fs_chi(d, "target", sig_level = 1), "strictly between 0 and 1")
  expect_error(fs_chi(d, "target", sig_level = "a"),
               "'sig_level' must be a single finite number")
  expect_error(fs_chi(d, "target", simulation_B = 99),
               "'simulation_B' must be between 100")
  expect_error(fs_chi(d, "target", simulation_B = 100.5),
               "'simulation_B' must be a whole number")
  expect_error(fs_chi(d, "target", continuity_correction = "yes"),
               "'continuity_correction' must be TRUE or FALSE")
  expect_error(fs_chi(d, "target", verbose = NA),
               "'verbose' must be TRUE or FALSE")
  expect_error(fs_chi(d, "target", parallel = NA),
               "'parallel' must be TRUE or FALSE")
  expect_error(fs_chi(d, "target", n_cores = 0),
               "'n_cores' must be between 1 and Inf")
  expect_error(fs_chi("nope", "target"), "'data' must be a data.frame")
  expect_error(fs_chi(d, "absent"), "Column 'absent' not found in 'data'")
  expect_error(fs_chi(d, "target", p_adjust_method = "nope"),
               "Invalid p_adjust_method")

  d_one_level <- data.frame(target = factor(c("Y", NA, NA)),
                            f = factor(c("a", "b", "a")))
  expect_error(fs_chi(d_one_level, "target"), "at least 2 non-NA levels")
})

test_that("fs_chi defaults are sequential and stable", {
  fx <- formals(fs_chi)
  expect_identical(
    names(fx),
    c("data", "target", "sig_level", "continuity_correction",
      "p_adjust_method", "simulation_B", "seed", "verbose",
      "parallel", "n_cores")
  )
  expect_identical(fx$sig_level, 0.05)
  expect_null(fx$continuity_correction)
  expect_identical(fx$p_adjust_method, "bonferroni")
  expect_identical(fx$simulation_B, 2000)
  expect_null(fx$seed)
  expect_identical(fx$verbose, FALSE)
  expect_identical(fx$parallel, FALSE)
  expect_identical(fx$n_cores, 2L)
})

test_that("fs_chi separates associated from independent features (asymptotic path)", {
  n <- 200
  d <- data.frame(
    f_strong = rep(c("A", "B"), each = n / 2),          # mirrors the target
    f_noise  = factor(rep(c("X", "Y"), times = n / 2)), # balanced vs target
    num_col  = seq_len(n),
    target   = factor(rep(c("Yes", "No"), each = n / 2)),
    stringsAsFactors = FALSE
  )

  out <- fs_chi(d, "target")

  expect_s3_class(out, "fs_result")
  expect_identical(out$method, "chi")
  expect_identical(out$task, "classification")
  expect_null(out$model)
  expect_named(out$details, chi_detail_names)

  res <- out$details$results
  expect_s3_class(res, "data.frame")
  expect_identical(names(res), chi_result_cols)

  # only categorical features are tested; characters are coerced to factor
  # on a copy, leaving the input untouched
  expect_setequal(res$feature, c("f_strong", "f_noise"))
  expect_false("num_col" %in% res$feature)
  expect_true(is.character(d$f_strong))
  expect_identical(out$details$n_features, 2L)
  expect_identical(out$details$sig_level, 0.05)
  expect_identical(out$details$p_adjust_method, "bonferroni")

  expect_type(out$selected, "character")
  expect_true(all(out$selected %in% res$feature))

  expect_true("f_strong" %in% out$selected)
  expect_false("f_noise" %in% out$selected)
  # results are ordered by adjusted p-value, so the strong feature is first
  expect_identical(res$feature[1], "f_strong")

  # scores are the adjusted p-values of every tested feature
  expect_true(is.numeric(out$scores))
  expect_setequal(names(out$scores), c("f_strong", "f_noise"))
  expect_identical(unname(out$scores), res$adj_p_value)

  # deterministic 2x2 tables with large counts: asymptotic test, df = 1,
  # auto Yates correction, all expected cell counts exactly 50
  expect_true(all(res$method == "asymptotic"))
  expect_true(all(res$correction_applied))
  expect_identical(res$df, c(1, 1))
  expect_identical(res$n, c(200L, 200L))
  expect_identical(res$min_expected, c(50, 50))

  # bonferroni can only increase p-values
  expect_true(all(res$adj_p_value >= res$p_value))
  expect_true(all(res$significant %in% c(TRUE, FALSE)))

  expect_output(print(out), "chi")
})

test_that("fs_chi p-value adjustment is controllable and case-insensitive", {
  d <- data.frame(
    f1     = factor(rep(c("A", "B"), each = 20)),  # mirrors the target
    f2     = factor(rep(c("A", "B"), times = 20)), # independent of target
    target = factor(rep(c("Y", "N"), each = 20))
  )

  out_none <- fs_chi(d, "target", p_adjust_method = "none")
  expect_identical(out_none$details$results$adj_p_value,
                   out_none$details$results$p_value)
  expect_identical(out_none$details$p_adjust_method, "none")

  out_upper <- fs_chi(d, "target", p_adjust_method = "BONFERRONI")
  upper_res <- out_upper$details$results
  expect_identical(
    upper_res$adj_p_value,
    pmin(1, upper_res$p_value * nrow(upper_res))
  )

  # continuity correction can be disabled explicitly for 2x2 tables
  out_nocorr <- fs_chi(d, "target", continuity_correction = FALSE)
  expect_false(any(out_nocorr$details$results$correction_applied))
})

test_that("fs_chi simulation path is triggered, seeded, and skips degenerate features", {
  n <- 20
  d <- data.frame(
    f_sim   = factor(c(rep("A", 16), rep("B", 4))),
    f_const = factor(c(rep("only", 17), NA, NA, NA)),
    target  = factor(rep(c("Y", "N"), times = n / 2))
  )

  set.seed(1)
  invisible(stats::runif(1))
  state_before <- .Random.seed

  out1 <- fs_chi(d, "target", simulation_B = 200, seed = 77)

  # seeding is local: the caller's RNG state is untouched
  expect_identical(.Random.seed, state_before)

  res <- out1$details$results
  sim_row <- res[res$feature == "f_sim", , drop = FALSE]

  # low expected counts (min 2 in the rare "B" row) force simulation
  expect_identical(sim_row$method, "simulation")
  expect_identical(sim_row$min_expected, 2)
  expect_identical(sim_row$n, 20L)
  expect_false(sim_row$correction_applied)
  # simulation-based tests have no asymptotic degrees of freedom
  expect_true(all(is.na(res$df[res$method %in% "simulation"])))

  # the degenerate single-level feature is skipped but reports its true
  # non-NA count, and sorts last because its adjusted p-value is NA
  const_row <- res[res$feature == "f_const", , drop = FALSE]
  expect_identical(const_row$n, 17L)
  expect_true(is.na(const_row$p_value))
  expect_true(is.na(const_row$method))
  expect_identical(res$feature[nrow(res)], "f_const")
  expect_false("f_const" %in% out1$selected)
  # a skipped feature still appears in the scores, with an NA adjusted p-value
  expect_true(is.na(out1$scores[["f_const"]]))

  # same seed, same simulated p-values
  out2 <- fs_chi(d, "target", simulation_B = 200, seed = 77)
  expect_identical(out1$details$results$p_value,
                   out2$details$results$p_value)
})

test_that("fs_chi returns an empty, well-formed fs_result without factor features", {
  d <- data.frame(
    target = factor(rep(c("a", "b"), 5)),
    x      = 1:10,
    z      = seq(0.1, 1, by = 0.1)
  )
  out <- fs_chi(d, "target")

  expect_s3_class(out, "fs_result")
  expect_named(out$details, chi_detail_names)
  expect_identical(names(out$details$results), chi_result_cols)
  expect_identical(nrow(out$details$results), 0L)
  expect_identical(out$details$n_features, 0L)
  expect_identical(out$selected, character(0))
  expect_length(out$scores, 0L)
  expect_output(print(out), "Selected 0 of 0")
})
