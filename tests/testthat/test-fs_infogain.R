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

# Deterministic 64-row cardinality trap (no RNG). The target is balanced, so
# H(y) = 1 bit.
#   id   = 64 distinct labels -> H(y | id) = 0, so raw IG = 1 bit (the
#          maximum attainable) while H(id) = log2(64) = 6 bits.
#   good = 2 levels, 32/32, each 87.5% pure -> raw IG = 1 - H(0.875)
#          = 0.4564 bits with H(good) = 1 bit.
# Raw information gain therefore ranks the useless identifier ABOVE the
# genuinely informative predictor; the gain ratio reverses that.
ig_cardinality_toy <- function() {
  data.frame(
    id     = paste0("id", seq_len(64)),
    good   = rep(c("L", "H"), each = 32),
    target = c(rep("a", 28), rep("b", 4), rep("b", 28), rep("a", 4)),
    stringsAsFactors = FALSE
  )
}

# H(y | good) for the 87.5% / 12.5% split above.
ig_h_conditional <- -(0.875 * log2(0.875) + 0.125 * log2(0.125))

test_that("input validation errors are informative", {
  df <- data.frame(A = 1:4, target = c(1, 2, 1, 2))
  expect_error(fs_infogain("nope", "target"), "must be a data\\.frame or a list")
  expect_error(fs_infogain(df, target = 1), "single non-empty character string")
  expect_error(fs_infogain(df, "target", remove_na = "yes"), "TRUE or FALSE")
  expect_error(fs_infogain(df, "target", verbose = "yes"), "TRUE or FALSE")
  expect_error(fs_infogain(df, "target", numeric_bins = 2.5), "whole number")
  expect_error(fs_infogain(df, "target", numeric_bins = 0), "between")
  expect_error(fs_infogain(df, "target", normalize = "sqrt"), "should be one of")
  expect_error(fs_infogain(df, "target", top_n = 0), "between")
  expect_error(fs_infogain(df, "target", top_n = 1.5), "whole number")
  expect_error(fs_infogain(df, "missing"), "not found in 'data'")
  expect_error(fs_infogain(list(df, 1), "target"),
               "All elements in 'data' must be data\\.frames")
  expect_error(fs_infogain(list(), "target"), "non-empty list")
  expect_error(fs_infogain(list(ok = df, bad = data.frame(A = 1:3)), "target"),
               "position 2")
})

test_that("the default return is an fs_result with the documented pieces", {
  res <- fs_infogain(ig_toy(), "target")

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "infogain")
  expect_identical(res$task, "classification")
  expect_null(res$model)
  expect_true(is.call(res$call))

  expect_type(res$scores, "double")
  expect_identical(sort(names(res$scores)), sort(c("dup", "noise", "const")))
  expect_type(res$selected, "character")

  expect_named(res$details,
               c("table", "normalize", "numeric_bins", "n_features"))
  expect_identical(res$details$normalize, "none")
  expect_null(res$details$numeric_bins)
  expect_identical(res$details$n_features, 3L)
  # Not a list input, so there is nothing to collide.
  expect_null(res$details$collisions)

  # details$table is exactly the historical Variable/InfoGain data.frame.
  tab <- res$details$table
  expect_identical(class(tab), "data.frame")
  expect_identical(names(tab), c("Variable", "InfoGain"))
  expect_identical(sort(tab$Variable), sort(c("dup", "noise", "const")))
  expect_true(is.numeric(tab$InfoGain))
})

test_that("information gain matches hand-computed log2 entropies", {
  res <- fs_infogain(ig_toy(), "target")

  # Perfect predictor recovers the full target entropy H(y) = 1.5 bits
  expect_equal(res$scores[["dup"]], 1.5, tolerance = 1e-8)
  # 'noise' halves: H(y | noise) = 1 bit, so IG = 1.5 - 1 = 0.5
  expect_equal(res$scores[["noise"]], 0.5, tolerance = 1e-8)
  # A constant predictor carries no information (source returns exactly 0)
  expect_identical(res$scores[["const"]], 0)

  # The same numbers are in details$table.
  tab <- res$details$table
  expect_equal(tab$InfoGain[match("dup", tab$Variable)], 1.5, tolerance = 1e-8)
  expect_equal(tab$InfoGain[match("noise", tab$Variable)], 0.5,
               tolerance = 1e-8)
})

test_that("scores rank descending and only positive scores are selected", {
  res <- fs_infogain(ig_toy(), "target")

  expect_identical(names(res$scores), c("dup", "noise", "const"))
  expect_false(is.unsorted(rev(res$scores)))
  expect_true(all(is.finite(res$scores)))
  expect_true(all(res$scores >= 0))
  expect_true(all(res$scores <= 1.5 + 1e-8))

  # top_n = NULL: every feature with a score strictly greater than zero.
  expect_identical(res$selected, c("dup", "noise"))
  expect_false("const" %in% res$selected)
})

test_that("top_n truncates the ranking and ignores the zero-score floor", {
  expect_identical(fs_infogain(ig_toy(), "target", top_n = 1)$selected, "dup")
  expect_identical(
    fs_infogain(ig_toy(), "target", top_n = 2)$selected,
    c("dup", "noise")
  )
  # top_n selects by rank, not by a score floor, and never asks for more
  # features than exist.
  expect_identical(
    fs_infogain(ig_toy(), "target", top_n = 10)$selected,
    c("dup", "noise", "const")
  )
})

test_that("gain_ratio demotes a high-cardinality predictor that raw IG favors", {
  df <- ig_cardinality_toy()

  raw <- fs_infogain(df, "target")
  expect_equal(raw$scores[["id"]], 1, tolerance = 1e-8)
  expect_equal(raw$scores[["good"]], 1 - ig_h_conditional, tolerance = 1e-8)
  # The identifier wins on raw information gain -- the bias being corrected.
  expect_gt(raw$scores[["id"]], raw$scores[["good"]])
  expect_identical(names(raw$scores)[1L], "id")
  expect_identical(fs_infogain(df, "target", top_n = 1)$selected, "id")

  gr <- fs_infogain(df, "target", normalize = "gain_ratio")
  # IG / H(X): 1 / log2(64) for the identifier, 0.4564 / 1 for 'good'.
  expect_equal(gr$scores[["id"]], 1 / 6, tolerance = 1e-8)
  expect_equal(gr$scores[["good"]], 1 - ig_h_conditional, tolerance = 1e-8)
  expect_gt(gr$scores[["good"]], gr$scores[["id"]])
  expect_identical(names(gr$scores)[1L], "good")
  expect_identical(
    fs_infogain(df, "target", normalize = "gain_ratio", top_n = 1)$selected,
    "good"
  )

  expect_identical(gr$details$normalize, "gain_ratio")
  expect_identical(names(gr$details$table),
                   c("Variable", "InfoGain", "SplitEntropy", "GainRatio"))
  tab <- gr$details$table
  expect_equal(tab$SplitEntropy[match("id", tab$Variable)], 6,
               tolerance = 1e-8)
  expect_equal(tab$SplitEntropy[match("good", tab$Variable)], 1,
               tolerance = 1e-8)
  # The raw gains are still reported alongside the normalized ones.
  expect_equal(tab$InfoGain[match("id", tab$Variable)], 1, tolerance = 1e-8)
})

test_that("gain_ratio divides by zero nowhere: a constant predictor scores 0", {
  res <- fs_infogain(ig_toy(), "target", normalize = "gain_ratio")

  expect_true(all(is.finite(res$scores)))
  expect_identical(res$scores[["const"]], 0)
  # dup: IG 1.5 / H(dup) 1.5 = 1; noise: IG 0.5 / H(noise) 1 = 0.5
  expect_equal(res$scores[["dup"]], 1, tolerance = 1e-8)
  expect_equal(res$scores[["noise"]], 0.5, tolerance = 1e-8)
  expect_identical(res$selected, c("dup", "noise"))
})

test_that("list input keeps Origin and falls back to Data_Frame_<i>", {
  d1 <- data.frame(A = rep(1:2, 25), target = rep(c("x", "y"), 25))
  d2 <- data.frame(B = rep(c("u", "v"), 30), target = rep(c("x", "y"), 30))

  res <- fs_infogain(list(first = d1, second = d2), "target")
  tab <- res$details$table
  expect_identical(class(tab), "data.frame")
  expect_identical(names(tab), c("Variable", "InfoGain", "Origin"))
  expect_setequal(unique(tab$Origin), c("first", "second"))
  expect_identical(tab$Variable[tab$Origin == "first"], "A")
  expect_identical(tab$Variable[tab$Origin == "second"], "B")

  # selected/scores cover the union across data.frames.
  expect_setequal(names(res$scores), c("A", "B"))
  expect_setequal(res$selected, c("A", "B"))
  expect_identical(res$details$n_features, 2L)
  expect_identical(nrow(res$details$collisions), 0L)

  res_unnamed <- fs_infogain(list(d1, d2), "target")
  expect_setequal(unique(res_unnamed$details$table$Origin),
                  c("Data_Frame_1", "Data_Frame_2"))

  lst <- list(d1, d2)
  names(lst) <- c("named", NA)
  res_na_name <- fs_infogain(lst, "target")
  expect_setequal(unique(res_na_name$details$table$Origin),
                  c("named", "Data_Frame_2"))
})

test_that("a name colliding across data.frames keeps the higher score", {
  # 'shared' predicts the target perfectly in d1 and not at all in d2.
  d1 <- data.frame(
    shared = rep(c("u", "v"), 25),
    target = rep(c("x", "y"), 25),
    stringsAsFactors = FALSE
  )
  d2 <- data.frame(
    shared = rep(c("u", "v"), 24),
    other  = rep(c("p", "p", "q", "q"), 12),
    target = rep(c("x", "x", "y", "y"), 12),
    stringsAsFactors = FALSE
  )

  res <- fs_infogain(list(first = d1, second = d2), "target")

  # Every scored row survives in the table ...
  expect_identical(nrow(res$details$table), 3L)
  # ... but the union of names is scored once, at the higher value.
  expect_setequal(names(res$scores), c("shared", "other"))
  expect_identical(res$details$n_features, 2L)
  expect_equal(res$scores[["shared"]], 1, tolerance = 1e-8)
  expect_equal(res$scores[["other"]], 1, tolerance = 1e-8)

  coll <- res$details$collisions
  expect_identical(class(coll), "data.frame")
  expect_identical(names(coll), c("Variable", "Origin", "Score", "Kept"))
  expect_identical(nrow(coll), 2L)
  expect_identical(unique(coll$Variable), "shared")
  expect_identical(coll$Kept, c(TRUE, FALSE))
  expect_identical(coll$Origin[coll$Kept], "first")
  expect_equal(coll$Score, c(1, 0), tolerance = 1e-8)
})

test_that("Date predictors expand into _year/_month/_day and the original is dropped", {
  df <- data.frame(
    when   = as.Date("2019-11-15") + 0:99,
    x      = rep(1:4, 25),
    target = rep(c("p", "q"), 50)
  )
  res <- fs_infogain(df, "target")

  expect_true(all(c("when_year", "when_month", "when_day") %in%
                    names(res$scores)))
  expect_false("when" %in% names(res$scores))
  expect_true(all(c("when_year", "when_month", "when_day") %in%
                    res$details$table$Variable))
  expect_false("when" %in% res$details$table$Variable)
  expect_true(all(is.finite(res$scores)))
})

test_that("POSIXct predictors are expanded like Dates", {
  df <- data.frame(
    stamp  = as.POSIXct("2021-06-01 12:00:00", tz = "UTC") + 86400 * (0:59),
    target = rep(c("p", "q"), 30)
  )
  res <- fs_infogain(df, "target")

  expect_true(all(c("stamp_year", "stamp_month", "stamp_day") %in%
                    names(res$scores)))
  expect_false("stamp" %in% names(res$scores))
  expect_true(all(is.finite(res$scores)))
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
  expect_s3_class(res, "fs_result")
  expect_true(all(c("when_year", "when_month", "when_day") %in%
                    names(res$scores)))

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
  expect_equal(res$scores[["A"]], log2(5), tolerance = 1e-8)
  expect_identical(res$task, "classification")
})

test_that("numeric predictors/targets are discretized; numeric_bins override accepted", {
  df <- data.frame(
    x      = rep(seq(-2, 2, length.out = 10), 10),
    target = rep(0:9, each = 10)
  )
  res_auto <- fs_infogain(df, "target")
  res_bins <- fs_infogain(df, "target", numeric_bins = 3)

  for (res in list(res_auto, res_bins)) {
    expect_s3_class(res, "fs_result")
    expect_identical(names(res$details$table), c("Variable", "InfoGain"))
    expect_true(all(is.finite(res$scores)))
    expect_true(all(res$scores >= 0))
  }
  expect_identical(res_auto$details$numeric_bins, NULL)
  expect_identical(res_bins$details$numeric_bins, 3L)
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

  expect_identical(sort(res_drop$details$table$Variable), c("A", "B"))
  expect_true(all(is.finite(res_drop$scores)))
  expect_true(all(res_drop$scores >= 0))
  # Rows with NA target are excluded per pair anyway (documented behavior)
  expect_equal(res_drop$details$table, res_keep$details$table)
  expect_equal(res_drop$scores, res_keep$scores)
})

test_that("an all-NA target errors under remove_na = TRUE and yields NA scores otherwise", {
  df <- data.frame(A = rep(1:2, 10), target = rep(NA_real_, 20))
  expect_error(fs_infogain(df, "target", remove_na = TRUE), "No rows available")

  res <- fs_infogain(df, "target", remove_na = FALSE)
  expect_identical(res$details$table$Variable, "A")
  expect_true(all(is.na(res$details$table$InfoGain)))
  expect_true(all(is.na(res$scores)))
  # An undefined score is never selected, with or without top_n.
  expect_identical(res$selected, character(0))
  expect_identical(
    fs_infogain(df, "target", remove_na = FALSE, top_n = 1)$selected,
    character(0)
  )
  # NA propagates through the gain ratio instead of becoming NaN.
  gr <- fs_infogain(df, "target", remove_na = FALSE, normalize = "gain_ratio")
  expect_true(all(is.na(gr$scores)))
})

test_that("a target-only data.frame yields an empty result with the documented shape", {
  res <- fs_infogain(data.frame(target = 1:10), "target")

  expect_s3_class(res, "fs_result")
  expect_identical(res$selected, character(0))
  expect_length(res$scores, 0L)
  expect_true(is.numeric(res$scores))
  expect_identical(res$details$n_features, 0L)

  tab <- res$details$table
  expect_identical(class(tab), "data.frame")
  expect_identical(names(tab), c("Variable", "InfoGain"))
  expect_identical(nrow(tab), 0L)
})

test_that("verbose is quiet by default and reports when switched on", {
  expect_silent(fs_infogain(ig_toy(), "target"))

  msgs <- capture_messages(fs_infogain(ig_toy(), "target", verbose = TRUE))
  expect_true(any(grepl("Scored 3 features", msgs, fixed = TRUE)))
  expect_true(any(grepl("normalize = 'none'", msgs, fixed = TRUE)))
  expect_true(any(grepl("Selected 2 features", msgs, fixed = TRUE)))
})

test_that("the result prints", {
  res <- fs_infogain(ig_toy(), "target")
  expect_output(print(res), "fs_result")
  expect_output(print(res), "infogain")
})

test_that("fs_infogain's formals match the unified API", {
  fi <- formals(fs_infogain)

  expect_identical(
    names(fi),
    c("data", "target", "numeric_bins", "normalize", "top_n", "remove_na",
      "verbose")
  )
  expect_identical(eval(fi$normalize), c("none", "gain_ratio"))
  expect_null(fi$numeric_bins)
  expect_null(fi$top_n)
  expect_identical(fi$remove_na, TRUE)
  expect_identical(fi$verbose, FALSE)
})

test_that("fs_infogain leaves the caller's RNG state untouched", {
  set.seed(20260809)
  rng_before <- .Random.seed
  invisible(fs_infogain(ig_toy(), "target"))
  expect_identical(.Random.seed, rng_before)
})

test_that("automatic bin count is capped so cut() cannot receive NA", {
  # REGRESSION: the Freedman-Diaconis count is range / bin_width, which is
  # unbounded. A near-constant column beside one extreme outlier drove it past
  # .Machine$integer.max, as.integer() returned NA, and cut() then failed with
  # "invalid number of intervals". More bins than observations cannot describe
  # the data anyway, so the count is capped at n.
  ig_bins <- featR:::ig_bins

  x <- c(as.numeric(1:99), 1e12)
  # Confirm the fixture really is the pathological case: the raw FD count
  # overflows integer range.
  iqr <- stats::IQR(x)
  raw <- ceiling(diff(range(x)) / (2 * iqr / (length(x)^(1 / 3))))
  expect_gt(raw, .Machine$integer.max)
  expect_true(is.na(suppressWarnings(as.integer(raw))))

  b <- ig_bins(x)
  expect_type(b, "integer")
  expect_false(is.na(b))
  expect_lte(b, length(x))
  expect_gte(b, 2L)
  # The point of the cap: cut() accepts the result.
  expect_silent(cut(x, breaks = b))

  # Ordinary and degenerate columns are unaffected.
  expect_identical(ig_bins(as.numeric(1:10)), 5L)
  expect_identical(ig_bins(rep(5, 20)), 2L)

  # And it survives the public API on such a column.
  d <- data.frame(x = x, target = rep(c("a", "b"), 50),
                  stringsAsFactors = FALSE)
  res <- fs_infogain(d, "target")
  expect_true(all(is.finite(res$scores)))
})

test_that("date expansion refuses to overwrite an existing column", {
  # REGRESSION: the expansion wrote <col>_year/_month/_day with
  # data.table::set(), which overwrites in place. `exclude = target` stopped
  # the target from being EXPANDED but not from being OVERWRITTEN, so a target
  # named e.g. "when_year" was silently replaced by the year of "when" before
  # it was discretized.
  d_clash <- data.frame(
    when      = as.Date("2020-01-01") + 0:9,
    when_year = rep(c(1, 2), 5)
  )
  expect_error(featR:::ig_expand_dates(d_clash),
               "would overwrite existing column")
  expect_error(featR:::ig_expand_dates(d_clash), "when_year")

  # Excluding the clashing column does not make overwriting it safe.
  expect_error(featR:::ig_expand_dates(d_clash, exclude = "when_year"),
               "would overwrite existing column")

  # Through the public API, with the clashing name as the target.
  expect_error(fs_infogain(d_clash, "when_year"),
               "would overwrite existing column")

  # No clash: expansion proceeds and the original date column is dropped.
  d_ok <- data.frame(when = as.Date("2020-01-01") + 0:9, v = 1:10)
  out <- featR:::ig_expand_dates(d_ok)
  expect_true(all(c("when_year", "when_month", "when_day") %in% names(out)))
  expect_false("when" %in% names(out))
})
