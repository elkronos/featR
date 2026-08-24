# Tests for the shared internal utilities.

test_that("assert helpers validate correctly", {
  expect_silent(assert_string("x", "arg"))
  expect_error(assert_string(1, "arg"), "single non-empty")
  expect_error(assert_flag(NA, "flag"), "TRUE or FALSE")
  expect_error(assert_number("a", "n"), "finite number")
  expect_error(assert_number(2, "n", upper = 1), "between")
  expect_identical(assert_count(3, "k"), 3L)
  expect_error(assert_count(2.5, "k"), "whole number")
  # beyond the integer range: a clear error, not NA reaching if ()
  expect_error(assert_count(3e9, "k"), "must be at most")
  expect_error(assert_data_frame(1, "data"), "data.frame")
  expect_error(assert_target(data.frame(a = 1), "b"), "not found")
})

test_that("%||% returns first non-NULL", {
  expect_identical(NULL %||% 2, 2)
  expect_identical(1 %||% 2, 1)
})

test_that("resolve_cores is sequential by default and capped", {
  expect_identical(resolve_cores(NULL), 1L)
  expect_identical(resolve_cores(1), 1L)
  expect_lte(resolve_cores(10000), parallel::detectCores())
  expect_error(resolve_cores(0), "between")
})

test_that("local_seed is reproducible and restores prior RNG state", {
  set.seed(1)
  before <- .Random.seed
  f <- function() {
    local_seed(42)
    stats::runif(1)
  }
  a <- f()
  expect_identical(.Random.seed, before)
  b <- f()
  expect_identical(a, b)
  # NULL is a no-op
  expect_silent(local_seed(NULL))
})

test_that("as_dt copies rather than aliases", {
  dt <- data.table::data.table(a = 1:3)
  out <- as_dt(dt)
  out[, a := 99L]
  expect_identical(dt$a, 1:3)
  expect_error(as_dt(1L), "must be a data.frame")
})

test_that("fs_require errors informatively for missing packages", {
  expect_error(fs_require("definitelyNotARealPkg123"), "not installed")
  expect_true(fs_require("stats"))
})

test_that("backtick wraps only non-syntactic names", {
  expect_identical(backtick(c("ok", "not ok")), c("ok", "`not ok`"))
})

# Shared filter machinery from R/utils-filter.R, behind the `output` argument
# of both fs_supervised() and fs_unsupervised(). Both helpers are internal,
# hence featR:::.

test_that("filter_mask never selects an NA score, under keep or remove", {
  scores <- c(low = 0.1, undefined = NA_real_, high = 0.9)

  keep <- suppressWarnings(
    featR:::filter_mask(scores, threshold = 0.5, direction = "above",
                        action = "keep", include_equal = FALSE)
  )
  dropped <- suppressWarnings(
    featR:::filter_mask(scores, threshold = 0.5, direction = "above",
                        action = "remove", include_equal = FALSE)
  )
  expect_identical(keep, c(low = FALSE, undefined = FALSE, high = TRUE))
  # Negating the comparison must not resurrect the undefined feature.
  expect_identical(dropped, c(low = TRUE, undefined = FALSE, high = FALSE))

  expect_warning(
    featR:::filter_mask(scores, 0.5, "above", "keep"),
    "1 feature\\(s\\) had undefined scores and were excluded: undefined"
  )
  expect_warning(
    featR:::filter_mask(scores, 0.5, "above", "remove"),
    "1 feature\\(s\\) had undefined scores and were excluded: undefined"
  )
})

test_that("filter_mask's warning names at most five undefined features", {
  scores <- c(ok = 1, na1 = NA_real_, na2 = NA_real_, na3 = NA_real_,
              na4 = NA_real_, na5 = NA_real_, na6 = NA_real_)
  msg <- tryCatch(
    featR:::filter_mask(scores, 0, "above", "keep"),
    warning = function(w) conditionMessage(w)
  )
  expect_true(grepl("6 feature(s) had undefined scores", msg, fixed = TRUE))
  expect_true(grepl("na1, na2, na3, na4, na5, ...", msg, fixed = TRUE))
  expect_false(grepl("na6", msg, fixed = TRUE))

  short <- tryCatch(
    featR:::filter_mask(c(a = NA_real_, b = NA_real_, c = NA_real_),
                        0, "above", "keep"),
    warning = function(w) conditionMessage(w)
  )
  expect_true(
    grepl("3 feature(s) had undefined scores and were excluded: a, b, c",
          short, fixed = TRUE)
  )
  expect_false(grepl("...", short, fixed = TRUE))
})

test_that("filter_mask's include_equal switches strict and inclusive tests", {
  scores <- c(lo = 1, eq = 2, hi = 3)

  expect_identical(
    featR:::filter_mask(scores, 2, "above", "keep", include_equal = FALSE),
    c(lo = FALSE, eq = FALSE, hi = TRUE)
  )
  expect_identical(
    featR:::filter_mask(scores, 2, "above", "keep", include_equal = TRUE),
    c(lo = FALSE, eq = TRUE, hi = TRUE)
  )
  expect_identical(
    featR:::filter_mask(scores, 2, "below", "keep", include_equal = FALSE),
    c(lo = TRUE, eq = FALSE, hi = FALSE)
  )
  expect_identical(
    featR:::filter_mask(scores, 2, "below", "keep", include_equal = TRUE),
    c(lo = TRUE, eq = TRUE, hi = FALSE)
  )
  expect_identical(
    featR:::filter_mask(scores, 2, "above", "remove", include_equal = FALSE),
    c(lo = TRUE, eq = TRUE, hi = FALSE)
  )
  expect_identical(
    featR:::filter_mask(scores, 2, "above", "remove", include_equal = TRUE),
    c(lo = TRUE, eq = FALSE, hi = FALSE)
  )
  expect_identical(
    featR:::filter_mask(scores, 2, "below", "remove", include_equal = TRUE),
    c(lo = FALSE, eq = FALSE, hi = TRUE)
  )
  # Fully defined scores warn about nothing.
  expect_silent(featR:::filter_mask(scores, 2, "above", "keep"))
})

test_that("filter_output subsets by index, not by duplicated column name", {
  dt <- data.table::data.table(first = c(1, 2, 3), second = c(10, 20, 30))
  data.table::setnames(dt, c("dup", "dup"))
  expect_identical(names(dt), c("dup", "dup"))

  scores <- c(dup = 0.1, dup = 0.9)
  mask <- c(dup = FALSE, dup = TRUE)

  mat <- featR:::filter_output(dt, scores, mask, "matrix")
  expect_true(is.matrix(mat))
  expect_identical(dim(mat), c(3L, 1L))
  expect_equal(as.numeric(mat[, 1L]), c(10, 20, 30))

  expect_identical(featR:::filter_output(dt, scores, mask, "mask"), mask)
  expect_identical(
    unname(featR:::filter_output(dt, scores, mask, "indices")),
    2L
  )
  expect_identical(featR:::filter_output(dt, scores, mask, "names"), "dup")

  dtres <- featR:::filter_output(dt, scores, mask, "dt")
  expect_true(data.table::is.data.table(dtres))
  expect_identical(ncol(dtres), 1L)
  expect_equal(dtres[[1L]], c(10, 20, 30))

  dfres <- featR:::filter_output(dt, scores, mask, "data.frame")
  expect_identical(class(dfres), "data.frame")
  expect_equal(dfres[[1L]], c(10, 20, 30))

  lst <- featR:::filter_output(dt, scores, mask, "list")
  expect_identical(names(lst),
                   c("filtered", "mask", "indices", "names", "scores"))
  expect_equal(as.numeric(lst$filtered[, 1L]), c(10, 20, 30))
  expect_identical(lst$scores, scores)
})

test_that("filter_output shapes an empty selection for all seven out values", {
  dt <- data.table::data.table(a = c(1, 2, 3), b = c(4, 5, 6))
  scores <- c(a = 1, b = 2)
  mask <- c(a = FALSE, b = FALSE)

  mat <- featR:::filter_output(dt, scores, mask, "matrix")
  expect_true(is.matrix(mat))
  expect_identical(dim(mat), c(3L, 0L))
  # base R gives a zero-column matrix NULL dimnames, not character(0)
  expect_null(colnames(mat))

  dfres <- featR:::filter_output(dt, scores, mask, "data.frame")
  expect_identical(class(dfres), "data.frame")
  expect_identical(dim(dfres), c(3L, 0L))

  # Only the zero-column part of the "dt" shape is documented as guaranteed.
  dtres <- featR:::filter_output(dt, scores, mask, "dt")
  expect_true(data.table::is.data.table(dtres))
  expect_identical(ncol(dtres), 0L)
  expect_true(nrow(dtres) %in% c(0L, 3L))

  expect_identical(featR:::filter_output(dt, scores, mask, "mask"), mask)

  idx <- featR:::filter_output(dt, scores, mask, "indices")
  expect_type(idx, "integer")
  expect_length(idx, 0L)

  expect_identical(featR:::filter_output(dt, scores, mask, "names"),
                   character(0))

  lst <- featR:::filter_output(dt, scores, mask, "list")
  expect_identical(names(lst),
                   c("filtered", "mask", "indices", "names", "scores"))
  expect_identical(dim(lst$filtered), c(3L, 0L))
  expect_identical(lst$mask, mask)
  expect_length(lst$indices, 0L)
  expect_identical(lst$names, character(0))
  expect_identical(lst$scores, scores)
})
