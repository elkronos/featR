# Tests for fs_infogain(). Information gain only needs Imports (data.table),
# so everything in this file runs unconditionally.

# Deterministic 100-row toy set (no RNG): the target has entropy
# H(y) = -(0.25*log2(0.25) + 0.25*log2(0.25) + 0.5*log2(0.5)) = 1.5 bits,
# 'dup' mirrors the target perfectly, 'noise' carries exactly 0.5 bits by
# construction (n1 -> {a, c}, n2 -> {b, c}, each half entropy 1 bit), and
# 'const' carries none.
ig_toy <- function() {
  y <- rep(c("a", "b", "c", "c"), 25)
  data.frame(
    dup    = y,
    noise  = rep(c("n1", "n2"), 50),
    const  = rep(1, 100),
    target = factor(y),
    stringsAsFactors = FALSE
  )
}

test_that("input validation errors are informative", {
  df <- data.frame(A = 1:4, target = c(1, 2, 1, 2))
  expect_error(fs_infogain("nope", "target"), "must be a data\\.frame or a list")
  expect_error(fs_infogain(df, target = 1), "single non-empty character string")
  expect_error(fs_infogain(df, "target", remove_na = "yes"), "TRUE or FALSE")
  expect_error(fs_infogain(df, "target", numeric_bins = 2.5), "whole number")
  expect_error(fs_infogain(df, "target", numeric_bins = 0), "between")
  expect_error(fs_infogain(df, "missing"), "not found in 'data'")
  expect_error(fs_infogain(list(df, 1), "target"),
               "All elements in 'data' must be data\\.frames")
  expect_error(fs_infogain(list(), "target"), "non-empty list")
  expect_error(fs_infogain(list(ok = df, bad = data.frame(A = 1:3)), "target"),
               "position 2")
})

test_that("single data.frame input returns a plain data.frame with Variable/InfoGain", {
  res <- fs_infogain(ig_toy(), "target")
  expect_identical(class(res), "data.frame")
  expect_identical(names(res), c("Variable", "InfoGain"))
  expect_identical(sort(res$Variable), sort(c("dup", "noise", "const")))
  expect_true(is.numeric(res$InfoGain))
})

test_that("information gain matches hand-computed log2 entropies", {
  res <- fs_infogain(ig_toy(), "target")
  ig <- res$InfoGain[match(c("dup", "noise", "const"), res$Variable)]

  # Perfect predictor recovers the full target entropy H(y) = 1.5 bits
  expect_equal(ig[1L], 1.5, tolerance = 1e-8)
  # 'noise' halves: H(y | noise) = 1 bit, so IG = 1.5 - 1 = 0.5
  expect_equal(ig[2L], 0.5, tolerance = 1e-8)
  # A constant predictor carries no information (source returns exactly 0)
  expect_identical(ig[3L], 0)
})

test_that("duplicate-of-target column ranks first; IG non-negative, finite, bounded", {
  res <- fs_infogain(ig_toy(), "target")
  expect_identical(res$Variable[which.max(res$InfoGain)], "dup")
  expect_true(all(is.finite(res$InfoGain)))
  expect_true(all(res$InfoGain >= 0))
  expect_true(all(res$InfoGain <= 1.5 + 1e-8))
})

test_that("list input gains an Origin column; unnamed/NA elements fall back to Data_Frame_<i>", {
  d1 <- data.frame(A = rep(1:2, 25), target = rep(c("x", "y"), 25))
  d2 <- data.frame(B = rep(c("u", "v"), 30), target = rep(c("x", "y"), 30))

  res <- fs_infogain(list(first = d1, second = d2), "target")
  expect_identical(class(res), "data.frame")
  expect_identical(names(res), c("Variable", "InfoGain", "Origin"))
  expect_setequal(unique(res$Origin), c("first", "second"))
  expect_identical(res$Variable[res$Origin == "first"], "A")
  expect_identical(res$Variable[res$Origin == "second"], "B")

  res_unnamed <- fs_infogain(list(d1, d2), "target")
  expect_setequal(unique(res_unnamed$Origin), c("Data_Frame_1", "Data_Frame_2"))

  lst <- list(d1, d2)
  names(lst) <- c("named", NA)
  res_na_name <- fs_infogain(lst, "target")
  expect_setequal(unique(res_na_name$Origin), c("named", "Data_Frame_2"))
})

test_that("Date predictors expand into _year/_month/_day and the original is dropped", {
  df <- data.frame(
    when   = as.Date("2019-11-15") + 0:99,
    x      = rep(1:4, 25),
    target = rep(c("p", "q"), 50)
  )
  res <- fs_infogain(df, "target")
  expect_true(all(c("when_year", "when_month", "when_day") %in% res$Variable))
  expect_false("when" %in% res$Variable)
  expect_true(all(is.finite(res$InfoGain)))
})

test_that("POSIXct predictors are expanded like Dates", {
  df <- data.frame(
    stamp  = as.POSIXct("2021-06-01 12:00:00", tz = "UTC") + 86400 * (0:59),
    target = rep(c("p", "q"), 30)
  )
  res <- fs_infogain(df, "target")
  expect_true(all(c("stamp_year", "stamp_month", "stamp_day") %in% res$Variable))
  expect_false("stamp" %in% res$Variable)
  expect_true(all(is.finite(res$InfoGain)))
})

test_that("data.table input works and the caller's table is not modified", {
  df <- data.frame(
    when   = as.Date("2020-01-01") + 0:49,
    A      = rep(1:2, 25),
    target = rep(c("p", "q"), 25)
  )
  dt <- data.table::as.data.table(df)
  names_before <- names(data.table::copy(dt))

  res <- fs_infogain(dt, "target")
  expect_identical(class(res), "data.frame")
  expect_true(all(c("when_year", "when_month", "when_day") %in% res$Variable))

  # By-reference expansion must not leak back into the caller's object
  expect_identical(names(dt), names_before)
  expect_s3_class(dt$when, "Date")
})

test_that("a date-like target is treated as categorical days (known answer)", {
  # 'A' identifies the day exactly: IG = H(target) = log2(5) bits
  df <- data.frame(
    A      = rep(letters[1:5], 20),
    target = as.Date("2020-01-01") + rep(0:4, 20)
  )
  res <- fs_infogain(df, "target")
  expect_equal(res$InfoGain[res$Variable == "A"], log2(5), tolerance = 1e-8)
})

test_that("numeric predictors/targets are discretized; numeric_bins override accepted", {
  df <- data.frame(
    x      = rep(seq(-2, 2, length.out = 10), 10),
    target = rep(0:9, each = 10)
  )
  res_auto <- fs_infogain(df, "target")
  res_bins <- fs_infogain(df, "target", numeric_bins = 3)
  for (res in list(res_auto, res_bins)) {
    expect_identical(names(res), c("Variable", "InfoGain"))
    expect_true(all(is.finite(res$InfoGain)))
    expect_true(all(res$InfoGain >= 0))
  }
})

test_that("scattered NAs are handled per pair; remove_na makes no difference then", {
  df <- data.frame(
    A      = c(rep(1:2, 20), NA, NA),
    B      = c(NA, rep(c("u", "v"), 20), NA),
    target = c(rep(c("x", "y"), 20), NA, "x"),
    stringsAsFactors = FALSE
  )
  res_drop <- fs_infogain(df, "target", remove_na = TRUE)
  res_keep <- fs_infogain(df, "target", remove_na = FALSE)

  expect_identical(sort(res_drop$Variable), c("A", "B"))
  expect_true(all(is.finite(res_drop$InfoGain)))
  expect_true(all(res_drop$InfoGain >= 0))
  # Rows with NA target are excluded per pair anyway (documented behavior)
  expect_equal(res_drop, res_keep)
})

test_that("an all-NA target errors under remove_na = TRUE and yields NA IG otherwise", {
  df <- data.frame(A = rep(1:2, 10), target = rep(NA_real_, 20))
  expect_error(fs_infogain(df, "target", remove_na = TRUE), "No rows available")

  res <- fs_infogain(df, "target", remove_na = FALSE)
  expect_identical(res$Variable, "A")
  expect_true(all(is.na(res$InfoGain)))
})

test_that("a target-only data.frame yields an empty result with the documented names", {
  res <- fs_infogain(data.frame(target = 1:10), "target")
  expect_identical(class(res), "data.frame")
  expect_identical(names(res), c("Variable", "InfoGain"))
  expect_identical(nrow(res), 0L)
})

test_that("fs_infogain leaves the caller's RNG state untouched", {
  set.seed(20260809)
  rng_before <- .Random.seed
  invisible(fs_infogain(ig_toy(), "target"))
  expect_identical(.Random.seed, rng_before)
})
