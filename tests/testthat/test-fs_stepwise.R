# Tests for fs_stepwise(). The source calls fs_require("MASS", ...) as its very
# first statement, so every block that calls fs_stepwise() -- validation
# included -- is gated on the 'MASS' Suggest. Only the signature test, which
# just reads formals(), runs unconditionally.
#
# The priority here is the side-effect regressions: the implementation must
# not write a log file, must not assign anything into the global environment,
# and must not touch the RNG.

# Deterministic 150-row design (no RNG): y = 2*x1 + 3*x2 plus a tiny
# oscillating error, with two columns that carry no signal. The predictors are
# sampled sine/cosine waves at clearly different frequencies, so the design is
# well conditioned, and the signal-to-noise ratio is large enough that AIC
# cannot drop x1 or x2 in any direction.
step_toy <- function(n = 150L) {
  i <- seq_len(n)
  x1 <- sin(i)
  x2 <- cos(i / 2)
  data.frame(
    x1     = x1,
    x2     = x2,
    noise1 = sin(i / 3),
    noise2 = cos(i / 5),
    y      = 2 * x1 + 3 * x2 + 0.02 * sin(11 * i)
  )
}

test_that("fs_stepwise has a stable signature without 'seed'/'return_models'", {
  fx <- formals(fs_stepwise)
  expect_identical(
    names(fx),
    c("data", "target", "direction", "verbose", "...")
  )
  expect_identical(eval(fx$direction), c("both", "backward", "forward"))
  expect_identical(fx$verbose, FALSE)
  # The old API's arguments are gone. Note that the function still takes '...'
  # (forwarded to MASS::stepAIC()), so supplying them is silently ignored
  # downstream rather than raising an "unused argument" error.
  expect_false(any(c("seed", "return_models", "dependent_var", "step_type") %in%
                     names(fx)))
})

test_that("stepwise selection returns an fs_result that keeps the true signal", {
  skip_if_not_installed("MASS")
  d <- step_toy()

  out <- fs_stepwise(d, "y", direction = "both")

  expect_s3_class(out, "fs_result")
  expect_identical(out$method, "stepwise")
  expect_identical(out$task, "regression")
  expect_s3_class(out$model, "lm")
  expect_type(out$selected, "character")
  expect_true(is.call(out$call))

  # y is built from x1 and x2 only, so both must survive the search
  expect_true(all(c("x1", "x2") %in% out$selected))

  # known answer: with a near-noiseless design the OLS estimates recover the
  # generating coefficients
  cf <- stats::coef(out$model)
  expect_equal(cf[["x1"]], 2, tolerance = 0.05)
  expect_equal(cf[["x2"]], 3, tolerance = 0.05)

  # documented details, all present and named
  expect_named(out$details,
               c("final_model", "coefficients", "selected_terms", "direction",
                 "n_features", "dropped_na_rows"))
  expect_identical(out$details$final_model, out$model)
  expect_identical(out$details$selected_terms, out$selected)
  expect_identical(out$details$direction, "both")
  expect_identical(out$details$n_features, 4L)
  expect_identical(out$details$dropped_na_rows, 0L)

  expect_true(is.matrix(out$details$coefficients))
  expect_identical(colnames(out$details$coefficients),
                   c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  expect_true(all(c("(Intercept)", "x1", "x2") %in%
                    rownames(out$details$coefficients)))
  expect_identical(rownames(out$details$coefficients), names(cf))

  # scores are |t| of the retained coefficients, intercept excluded
  expect_true(is.numeric(out$scores))
  expect_false("(Intercept)" %in% names(out$scores))
  expect_identical(names(out$scores),
                   setdiff(rownames(out$details$coefficients), "(Intercept)"))
  expect_equal(
    unname(out$scores),
    unname(abs(out$details$coefficients[names(out$scores), "t value"]))
  )
  expect_true(all(c("x1", "x2") %in% names(out$scores)))

  expect_output(print(out), "stepwise")
})

test_that("fs_stepwise writes no log file and no other output to disk", {
  skip_if_not_installed("MASS")
  d <- step_toy()

  withr::with_tempdir({
    out <- expect_silent(fs_stepwise(d, "y", direction = "both"))
    expect_s3_class(out$model, "lm")

    # the old implementation dropped a script_log.log in the working directory
    expect_false(file.exists("script_log.log"))
    expect_identical(list.files(".", all.files = TRUE, no.. = TRUE),
                     character(0))
  })
})

test_that("fs_stepwise assigns nothing into the global environment", {
  skip_if_not_installed("MASS")
  d <- step_toy()

  before <- ls(globalenv(), all.names = TRUE)
  out <- fs_stepwise(d, "y", direction = "both")
  after <- ls(globalenv(), all.names = TRUE)

  expect_identical(after, before)
  expect_false(exists(".fs_stepwise_data", envir = globalenv(),
                      inherits = FALSE))
  expect_false(exists(".fs_stepwise_args", envir = globalenv(),
                      inherits = FALSE))
  expect_s3_class(out$model, "lm")
})

test_that("the fitted model carries its data in a private environment", {
  skip_if_not_installed("MASS")
  out <- fs_stepwise(step_toy(), "y", direction = "both")

  fit_env <- environment(stats::terms(out$model))
  expect_true(is.environment(fit_env))
  expect_false(identical(fit_env, globalenv()))
  expect_true(exists(".fs_stepwise_data", envir = fit_env, inherits = FALSE))
  expect_s3_class(get(".fs_stepwise_data", envir = fit_env), "data.frame")
  # the temporary stepAIC() argument bundle is cleaned up again
  expect_false(exists(".fs_stepwise_args", envir = fit_env, inherits = FALSE))
})

test_that("fs_stepwise leaves the caller's RNG state untouched", {
  skip_if_not_installed("MASS")
  set.seed(20260809)
  rng_before <- .Random.seed
  invisible(fs_stepwise(step_toy(), "y", direction = "both"))
  expect_identical(.Random.seed, rng_before)
})

test_that("the returned model works with predict(), summary() and update()", {
  skip_if_not_installed("MASS")
  d <- step_toy()
  out <- fs_stepwise(d, "y", direction = "both")
  m <- out$model

  preds <- stats::predict(m, newdata = d[1:5, , drop = FALSE])
  expect_length(preds, 5L)
  expect_true(all(is.finite(preds)))
  # known answer: predicting on the training rows reproduces the fitted values
  expect_equal(unname(stats::predict(m, newdata = d)),
               unname(stats::fitted(m)))

  s <- summary(m)
  expect_s3_class(s, "summary.lm")
  expect_equal(s$coefficients, out$details$coefficients)
  expect_gt(s$r.squared, 0.99)

  # documented contract: update() refits when the data is passed explicitly
  m2 <- stats::update(m, . ~ . - x1, data = d)
  expect_s3_class(m2, "lm")
  expect_false("x1" %in% attr(stats::terms(m2), "term.labels"))

  # ... while plain update() re-evaluates the call in the caller's
  # environment, where the private data object is not visible
  expect_error(stats::update(m), "\\.fs_stepwise_data")
})

test_that("all three search directions run and keep the true predictors", {
  skip_if_not_installed("MASS")
  d <- step_toy()

  for (dir in c("both", "forward", "backward")) {
    out <- fs_stepwise(d, "y", direction = dir)
    expect_s3_class(out, "fs_result")
    expect_s3_class(out$model, "lm")
    expect_true(all(c("x1", "x2") %in% out$selected))
    expect_identical(out$details$direction, dir)
  }
})

test_that("verbose = TRUE narrates the search and enables the stepAIC trace", {
  skip_if_not_installed("MASS")
  d <- step_toy()
  quiet <- fs_stepwise(d, "y", direction = "both")

  msgs <- capture_messages(
    trace_out <- utils::capture.output(
      loud <- fs_stepwise(d, "y", direction = "both", verbose = TRUE)
    )
  )
  all_msgs <- paste(msgs, collapse = "")

  expect_match(all_msgs,
               "Starting stepwise selection ('both') for target 'y'.",
               fixed = TRUE)
  expect_match(all_msgs, "Stepwise selection complete:", fixed = TRUE)
  expect_gt(length(trace_out), 0L)
  expect_identical(loud$selected, quiet$selected)
})

test_that("a 'trace' argument passed through ... is dropped with a warning", {
  skip_if_not_installed("MASS")
  d <- step_toy()

  expect_warning(out <- fs_stepwise(d, "y", direction = "both", trace = 2),
                 "Argument 'trace' supplied via")
  expect_s3_class(out$model, "lm")
  expect_true(all(c("x1", "x2") %in% out$selected))
})

test_that("rows with missing values are dropped with a counted warning", {
  skip_if_not_installed("MASS")
  d <- step_toy()
  d$x1[c(2L, 40L)] <- NA
  d$y[7L] <- NA

  expect_warning(out <- fs_stepwise(d, "y", direction = "both"),
                 "Dropped 3 row\\(s\\) with missing values")
  expect_identical(stats::nobs(out$model), 147L)
  expect_identical(out$details$dropped_na_rows, 3L)
  expect_true(all(c("x1", "x2") %in% out$selected))
})

test_that("a non-syntactic target is backticked into the formula", {
  skip_if_not_installed("MASS")
  d <- step_toy()
  names(d)[names(d) == "y"] <- "my var"

  # without backticking, as.formula("my var ~ .") would not even parse
  out <- fs_stepwise(d, "my var", direction = "both")

  expect_s3_class(out$model, "lm")
  expect_true(all(c("x1", "x2") %in% out$selected))
  expect_match(
    paste(deparse(stats::formula(out$model)), collapse = " "),
    "`my var`",
    fixed = TRUE
  )
})

test_that("fs_stepwise validates data, target, direction and verbose", {
  skip_if_not_installed("MASS")
  d <- data.frame(y = c(1, 2, 3, 4, 5, 6), x = c(2, 1, 4, 3, 6, 5))

  expect_error(fs_stepwise(list(a = 1), "y"), "'data' must be a data\\.frame")
  expect_error(fs_stepwise(d, 1),
               "'target' must be a single non-empty character string")
  expect_error(fs_stepwise(d, c("y", "x")),
               "'target' must be a single non-empty character string")
  expect_error(fs_stepwise(d, "nope"), "Column 'nope' not found in 'data'")
  expect_error(fs_stepwise(d, "y", direction = 1),
               "must be NULL or a character vector")
  expect_error(fs_stepwise(d, "y", direction = "sideways"),
               "should be one of")
  expect_error(fs_stepwise(d, "y", verbose = NA), "'verbose' must be TRUE or FALSE")

  # linear regression only: the response has to be numeric
  d_chr <- data.frame(y = c("a", "b", "a", "b"), x = c(1, 2, 3, 4))
  expect_error(fs_stepwise(d_chr, "y"),
               "fs_stepwise\\(\\) fits linear regressions only")

  # nothing left to select from
  expect_error(fs_stepwise(data.frame(y = c(1, 2, 3)), "y"),
               "at least one predictor besides the target")
})
