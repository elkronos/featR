# Tests for the shared internal utilities.

test_that("assert helpers validate correctly", {
  expect_silent(assert_string("x", "arg"))
  expect_error(assert_string(1, "arg"), "single non-empty")
  expect_error(assert_flag(NA, "flag"), "TRUE or FALSE")
  expect_error(assert_number("a", "n"), "finite number")
  expect_error(assert_number(2, "n", upper = 1), "between")
  expect_identical(assert_count(3, "k"), 3L)
  expect_error(assert_count(2.5, "k"), "whole number")
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
