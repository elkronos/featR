# Tests for fs_unsupervised().
#
# The scoring methods use only stats (var/mad/IQR) and base R, with
# data.table as the column container; all are Imports, so every test in this
# file runs unconditionally and no skip_*() calls are needed.
#
# Deterministic 5-row toy frame (no RNG):
#   spread = 1, 2, 3, 4, 100 -> var 1902.5, mad 1.4826, IQR 2, range 99,
#                               missing_prop 0,   n_unique 5
#   flat   = 2, 2, 2, 2, 2   -> every dispersion score is exactly 0,
#                               missing_prop 0,   n_unique 1
#   gappy  = 1, NA, 3, NA, 5 -> scored on c(1, 3, 5) when na_rm = TRUE,
#                               missing_prop 0.4, n_unique 3
unsup_x <- function() {
  data.frame(
    spread = c(1, 2, 3, 4, 100),
    flat   = c(2, 2, 2, 2, 2),
    gappy  = c(1, NA, 3, NA, 5)
  )
}

# Scores every column while passing threshold/direction/action explicitly, so
# the missing_prop advisory (which only fires when all three are defaulted)
# never interferes, and include_equal = TRUE keeps the zero-score columns so
# the "nothing selected" warning never fires either.
unsup_all_scores <- function(x, method) {
  fs_unsupervised(
    x = x, method = method, threshold = 0, direction = "above",
    action = "keep", include_equal = TRUE, output = "list"
  )$scores
}

test_that("every method reproduces its base R computation on a known frame", {
  x <- unsup_x()

  var_scores <- unsup_all_scores(x, "variance")
  expect_equal(
    var_scores,
    vapply(x, function(col) stats::var(col, na.rm = TRUE), numeric(1L))
  )
  expect_equal(var_scores[["spread"]], 1902.5)
  expect_equal(var_scores[["flat"]], 0)
  expect_equal(var_scores[["gappy"]], 4)

  mad_scores <- unsup_all_scores(x, "mad")
  expect_equal(
    mad_scores,
    vapply(x, function(col) stats::mad(col, na.rm = TRUE), numeric(1L))
  )
  # stats::mad() applies the 1.4826 normal-consistency constant: the raw
  # median absolute deviation of 'spread' is 1, the reported score is 1.4826.
  expect_equal(mad_scores[["spread"]], 1.4826)
  expect_equal(mad_scores[["gappy"]], 2 * 1.4826)
  expect_false(isTRUE(all.equal(mad_scores[["spread"]], 1)))

  iqr_scores <- unsup_all_scores(x, "iqr")
  expect_equal(
    iqr_scores,
    vapply(x, function(col) stats::IQR(col, na.rm = TRUE), numeric(1L))
  )
  expect_equal(iqr_scores[["spread"]], 2)
  expect_equal(iqr_scores[["flat"]], 0)

  range_scores <- unsup_all_scores(x, "range")
  expect_equal(range_scores, c(spread = 99, flat = 0, gappy = 4))

  missing_scores <- unsup_all_scores(x, "missing_prop")
  expect_equal(
    missing_scores,
    vapply(x, function(col) sum(is.na(col)) / length(col), numeric(1L))
  )
  expect_equal(missing_scores, c(spread = 0, flat = 0, gappy = 0.4))

  unique_scores <- unsup_all_scores(x, "n_unique")
  expect_equal(
    unique_scores,
    vapply(x, function(col) length(unique(col[!is.na(col)])), numeric(1L))
  )
  expect_equal(unique_scores, c(spread = 5, flat = 1, gappy = 3))
})

test_that("missing_prop warns that the defaults KEEP the emptiest features", {
  x <- data.frame(
    holes = c(1, NA, 3, NA),  # missing_prop 0.5
    full  = c(1, 2, 3, 4)     # missing_prop 0
  )

  expect_warning(
    kept <- fs_unsupervised(x, method = "missing_prop", output = "names"),
    "KEEPS the features with the most missing values"
  )
  # The advisory is warranted: the defaults really do keep the worst column.
  expect_identical(kept, "holes")

  defaults <- capture_warnings(
    fs_unsupervised(x, method = "missing_prop", output = "names")
  )
  expect_length(defaults, 1L)
  expect_match(defaults, "Consider action = 'remove' or direction = 'below'")

  # An explicit direction opts out of the advisory entirely.
  below <- capture_warnings(
    fs_unsupervised(x, method = "missing_prop", direction = "below",
                    threshold = 0.25, output = "names")
  )
  expect_identical(below, character(0))
  expect_silent(
    fs_unsupervised(x, method = "missing_prop", direction = "below",
                    threshold = 0.25, output = "names")
  )
  expect_identical(
    fs_unsupervised(x, method = "missing_prop", direction = "below",
                    threshold = 0.25, output = "names"),
    "full"
  )

  # So does an explicit action, and other methods never see the advisory.
  removed <- capture_warnings(
    fs_unsupervised(x, method = "missing_prop", action = "remove",
                    output = "names")
  )
  expect_identical(removed, character(0))
  expect_silent(fs_unsupervised(x, method = "variance", output = "names"))
})

test_that("arguments are validated before any data is touched", {
  # `x` is unusable, so only argument validation can produce these errors.
  expect_error(
    fs_unsupervised("not a table", method = "variance",
                    direction = "sideways"),
    "should be one of"
  )
  expect_error(fs_unsupervised("not a table", method = "nope"),
               "should be one of")
  expect_error(
    fs_unsupervised("not a table", method = "variance", action = "discard"),
    "should be one of"
  )
  expect_error(
    fs_unsupervised("not a table", method = "variance", output = "tibble"),
    "should be one of"
  )
  expect_error(
    fs_unsupervised("not a table", method = "variance", threshold = -1),
    "'threshold' must be between 0 and Inf"
  )
  expect_error(
    fs_unsupervised("not a table", method = "variance", threshold = NA),
    "'threshold' must be a single finite number"
  )
  expect_error(
    fs_unsupervised("not a table", method = "variance", include_equal = 1L),
    "'include_equal' must be TRUE or FALSE"
  )
  expect_error(
    fs_unsupervised("not a table", method = "variance", na_rm = "yes"),
    "'na_rm' must be TRUE or FALSE"
  )

  # Only once the arguments are sound does the input type get checked.
  expect_error(fs_unsupervised("not a table", method = "variance"),
               "'x' must be a data.frame, data.table, or matrix")
  expect_error(
    fs_unsupervised(data.frame(a = 1:4, b = letters[1:4]),
                    method = "variance"),
    "All columns of 'x' must be numeric"
  )
})

test_that("an undefined (NA) score is excluded under BOTH keep and remove", {
  # With na_rm = FALSE the NA in 'dirty' makes its variance undefined.
  x <- data.frame(
    clean = c(1, 2, 3, 4),
    dirty = c(1, NA, 3, 4),
    flat  = c(2, 2, 2, 2)
  )

  expect_warning(
    kept <- fs_unsupervised(x, method = "variance", threshold = 0,
                            direction = "above", action = "keep",
                            na_rm = FALSE, output = "names"),
    "undefined scores and were excluded: dirty"
  )
  expect_identical(kept, "clean")

  expect_warning(
    dropped <- fs_unsupervised(x, method = "variance", threshold = 0,
                               direction = "above", action = "remove",
                               na_rm = FALSE, output = "names"),
    "undefined scores and were excluded: dirty"
  )
  expect_identical(dropped, "flat")

  mask_keep <- suppressWarnings(
    fs_unsupervised(x, method = "variance", threshold = 0,
                    direction = "above", action = "keep", na_rm = FALSE,
                    output = "mask")
  )
  mask_remove <- suppressWarnings(
    fs_unsupervised(x, method = "variance", threshold = 0,
                    direction = "above", action = "remove", na_rm = FALSE,
                    output = "mask")
  )
  expect_identical(mask_keep, c(clean = TRUE, dirty = FALSE, flat = FALSE))
  expect_identical(mask_remove, c(clean = FALSE, dirty = FALSE, flat = TRUE))

  lst <- suppressWarnings(
    fs_unsupervised(x, method = "variance", threshold = 0,
                    direction = "above", action = "keep", na_rm = FALSE,
                    output = "list")
  )
  expect_true(is.na(lst$scores[["dirty"]]))
  expect_identical(lst$meta$n_input_cols, 3L)
  expect_identical(lst$meta$n_kept_cols, 1L)
})

test_that("duplicated column names are resolved by index, not by name", {
  # Both columns are called "dup"; only the SECOND has non-zero variance.
  x <- matrix(
    c(1, 1, 1, 1,
      0, 1, 2, 3),
    nrow = 4,
    dimnames = list(NULL, c("dup", "dup"))
  )
  args <- list(x = x, method = "variance", threshold = 0,
               direction = "above", action = "keep")

  idx <- do.call(fs_unsupervised, c(args, list(output = "indices")))
  expect_type(idx, "integer")
  expect_identical(unname(idx), 2L)

  mask <- do.call(fs_unsupervised, c(args, list(output = "mask")))
  expect_identical(unname(mask), c(FALSE, TRUE))

  mat <- do.call(fs_unsupervised, c(args, list(output = "matrix")))
  expect_true(is.matrix(mat))
  expect_identical(dim(mat), c(4L, 1L))
  expect_equal(as.numeric(mat[, 1L]), c(0, 1, 2, 3))

  df <- do.call(fs_unsupervised, c(args, list(output = "data.frame")))
  expect_identical(class(df), "data.frame")
  expect_equal(df[[1L]], c(0, 1, 2, 3))

  dt <- do.call(fs_unsupervised, c(args, list(output = "dt")))
  expect_true(data.table::is.data.table(dt))
  expect_identical(ncol(dt), 1L)
  expect_equal(dt[[1L]], c(0, 1, 2, 3))

  lst <- do.call(fs_unsupervised, c(args, list(output = "list")))
  expect_equal(as.numeric(lst$filtered[, 1L]), c(0, 1, 2, 3))
  expect_identical(unname(lst$indices), 2L)
  expect_length(lst$names, 1L)
})

test_that("all seven `output` shapes have the documented type and agree", {
  x <- unsup_x()
  pick <- function(output) {
    fs_unsupervised(x = x, method = "n_unique", threshold = 2,
                    direction = "above", action = "keep", output = output)
  }

  mat <- pick("matrix")
  dt <- pick("dt")
  df <- pick("data.frame")
  mask <- pick("mask")
  idx <- pick("indices")
  nm <- pick("names")
  lst <- pick("list")

  expect_true(is.matrix(mat))
  expect_identical(dim(mat), c(5L, 2L))
  expect_identical(colnames(mat), c("spread", "gappy"))

  expect_true(data.table::is.data.table(dt))
  expect_identical(names(dt), c("spread", "gappy"))
  expect_identical(nrow(dt), 5L)

  expect_identical(class(df), "data.frame")
  expect_identical(names(df), c("spread", "gappy"))
  expect_identical(nrow(df), 5L)

  expect_identical(mask, c(spread = TRUE, flat = FALSE, gappy = TRUE))
  expect_type(idx, "integer")
  expect_identical(unname(idx), c(1L, 3L))
  expect_identical(nm, c("spread", "gappy"))

  expect_identical(unname(idx), which(unname(mask)))
  expect_identical(nm, names(mask)[idx])
  expect_identical(nm, colnames(mat))

  expect_named(lst, c("filtered", "mask", "indices", "names", "scores",
                      "meta"))
  expect_identical(lst$mask, mask)
  expect_identical(lst$names, nm)
  expect_identical(lst$filtered, mat)
  expect_named(
    lst$meta,
    c("method", "threshold", "direction", "action", "include_equal", "na_rm",
      "n_input_cols", "n_kept_cols")
  )
  expect_identical(lst$meta$method, "n_unique")
  expect_identical(lst$meta$threshold, 2)
  expect_identical(lst$meta$direction, "above")
  expect_identical(lst$meta$action, "keep")
  expect_identical(lst$meta$n_input_cols, 3L)
  expect_identical(lst$meta$n_kept_cols, 2L)
})

test_that("an impossible threshold yields empty shapes that preserve nrow", {
  x <- unsup_x()
  empty_out <- function(output) {
    suppressWarnings(
      fs_unsupervised(x = x, method = "variance", threshold = 1e6,
                      direction = "above", action = "keep", output = output)
    )
  }

  expect_warning(
    fs_unsupervised(x = x, method = "variance", threshold = 1e6,
                    direction = "above", action = "keep", output = "names"),
    "No features meet the specified unsupervised selection criteria"
  )

  mat <- empty_out("matrix")
  expect_true(is.matrix(mat))
  expect_identical(dim(mat), c(5L, 0L))
  # base R gives a zero-column matrix NULL dimnames, not character(0)
  expect_null(colnames(mat))

  df <- empty_out("data.frame")
  expect_identical(class(df), "data.frame")
  expect_identical(dim(df), c(5L, 0L))

  # The source documents that the "dt" shape is subject to data.table's own
  # representation of zero-column tables, so only ncol is asserted here.
  dt <- empty_out("dt")
  expect_true(data.table::is.data.table(dt))
  expect_identical(ncol(dt), 0L)
  expect_true(nrow(dt) %in% c(0L, 5L))

  expect_identical(empty_out("mask"),
                   c(spread = FALSE, flat = FALSE, gappy = FALSE))
  expect_length(empty_out("indices"), 0L)
  expect_type(empty_out("indices"), "integer")
  expect_identical(empty_out("names"), character(0))

  lst <- empty_out("list")
  expect_identical(dim(lst$filtered), c(5L, 0L))
  expect_length(lst$indices, 0L)
  expect_identical(lst$names, character(0))
  expect_length(lst$scores, 3L)
  expect_identical(lst$meta$n_kept_cols, 0L)
})

test_that("fs_unsupervised's formals are stable and use `output`", {
  fu <- formals(fs_unsupervised)

  expect_identical(
    names(fu),
    c("x", "method", "threshold", "direction", "action", "include_equal",
      "na_rm", "output", "log_progress")
  )
  # Sibling fs_supervised() spells this argument "out"; this one is "output".
  expect_true("output" %in% names(fu))
  expect_false("out" %in% names(fu))

  expect_identical(
    eval(fu$method),
    c("variance", "mad", "iqr", "range", "missing_prop", "n_unique")
  )
  expect_identical(eval(fu$direction), c("above", "below"))
  expect_identical(eval(fu$action), c("keep", "remove"))
  expect_identical(
    eval(fu$output),
    c("matrix", "dt", "data.frame", "mask", "indices", "names", "list")
  )
  expect_identical(fu$threshold, 0)
  expect_identical(fu$include_equal, FALSE)
  expect_identical(fu$na_rm, TRUE)
  expect_identical(fu$log_progress, FALSE)
})
