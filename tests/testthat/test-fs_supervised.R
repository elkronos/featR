# Tests for fs_supervised().
#
# Scoring uses base R plus stats (correlation, lm/anova) and data.table only
# as the column container; all three are Imports, so every test in this file
# runs unconditionally and no skip_*() calls are needed.
#
# Deterministic 4-row toy set (no RNG). Against y = 1, 2, 3, 4:
#   strong = 1, 2, 3, 4 -> r =  1        -> |r| = 1
#   mirror = 4, 3, 2, 1 -> r = -1        -> |r| = 1
#   weak   = 1, 0, 1, 0 -> r = -1/sqrt(5) -> |r| = 0.4472136
# A threshold of 0.5 therefore splits the set cleanly into {strong, mirror}
# and {weak}.
sup_x <- function() {
  matrix(
    c(1, 2, 3, 4,
      4, 3, 2, 1,
      1, 0, 1, 0),
    nrow = 4,
    dimnames = list(NULL, c("strong", "mirror", "weak"))
  )
}

sup_y <- c(1, 2, 3, 4)

test_that("an undefined (NA) score is excluded under BOTH keep and remove", {
  # 'const' has zero variance, so its correlation with y is undefined. The
  # NA-inversion regression: under action = "remove" the negated comparison
  # must NOT turn that NA into a selected feature.
  x <- data.frame(
    strong = c(1, 2, 3, 4),
    const  = c(5, 5, 5, 5),
    weak   = c(1, 0, 1, 0)
  )
  y <- c(1, 2, 3, 4)

  expect_warning(
    kept <- fs_supervised(x, y, method = "correlation", threshold = 0.5,
                          direction = "above", action = "keep",
                          out = "names"),
    "undefined scores and were excluded: const"
  )
  expect_identical(kept, "strong")

  expect_warning(
    dropped <- fs_supervised(x, y, method = "correlation", threshold = 0.5,
                             direction = "above", action = "remove",
                             out = "names"),
    "undefined scores and were excluded: const"
  )
  expect_identical(dropped, "weak")

  mask_keep <- suppressWarnings(
    fs_supervised(x, y, method = "correlation", threshold = 0.5,
                  direction = "above", action = "keep", out = "mask")
  )
  mask_remove <- suppressWarnings(
    fs_supervised(x, y, method = "correlation", threshold = 0.5,
                  direction = "above", action = "remove", out = "mask")
  )
  expect_identical(mask_keep, c(strong = TRUE, const = FALSE, weak = FALSE))
  expect_identical(mask_remove, c(strong = FALSE, const = FALSE, weak = TRUE))
  expect_false(mask_keep[["const"]])
  expect_false(mask_remove[["const"]])

  # The undefined score is still reported back to the caller.
  lst <- suppressWarnings(
    fs_supervised(x, y, method = "correlation", threshold = 0.5,
                  direction = "above", action = "keep", out = "list")
  )
  expect_true(is.na(lst$scores[["const"]]))
  expect_identical(lst$meta$n_input_cols, 3L)
  expect_identical(lst$meta$n_kept_cols, 1L)
})

test_that("duplicated column names are resolved by index, not by name", {
  # Both columns are called "dup"; only the SECOND clears the threshold, so
  # the returned data must carry the second column's values.
  x <- matrix(
    c(0, 1, 0, 1,
      1, 2, 3, 4),
    nrow = 4,
    dimnames = list(NULL, c("dup", "dup"))
  )
  y <- c(1, 2, 3, 4)

  args <- list(x = x, y = y, method = "correlation", threshold = 0.5,
               direction = "above", action = "keep")

  idx <- do.call(fs_supervised, c(args, list(out = "indices")))
  expect_type(idx, "integer")
  expect_identical(unname(idx), 2L)

  mask <- do.call(fs_supervised, c(args, list(out = "mask")))
  expect_identical(unname(mask), c(FALSE, TRUE))

  mat <- do.call(fs_supervised, c(args, list(out = "matrix")))
  expect_true(is.matrix(mat))
  expect_identical(dim(mat), c(4L, 1L))
  expect_equal(as.numeric(mat[, 1L]), c(1, 2, 3, 4))

  df <- do.call(fs_supervised, c(args, list(out = "data.frame")))
  expect_identical(class(df), "data.frame")
  expect_identical(dim(df), c(4L, 1L))
  expect_equal(df[[1L]], c(1, 2, 3, 4))

  dt <- do.call(fs_supervised, c(args, list(out = "dt")))
  expect_true(data.table::is.data.table(dt))
  expect_identical(ncol(dt), 1L)
  expect_equal(dt[[1L]], c(1, 2, 3, 4))

  lst <- do.call(fs_supervised, c(args, list(out = "list")))
  expect_equal(as.numeric(lst$filtered[, 1L]), c(1, 2, 3, 4))
  expect_identical(unname(lst$indices), 2L)
  expect_length(lst$names, 1L)
})

test_that("an impossible threshold yields empty shapes that preserve nrow", {
  x <- sup_x()
  empty_out <- function(out) {
    suppressWarnings(
      fs_supervised(x = x, y = sup_y, method = "correlation", threshold = 1.1,
                    direction = "above", action = "keep", out = out)
    )
  }

  expect_warning(
    fs_supervised(x = x, y = sup_y, method = "correlation", threshold = 1.1,
                  direction = "above", action = "keep", out = "names"),
    "No features meet the specified supervised selection criteria"
  )

  mat <- empty_out("matrix")
  expect_true(is.matrix(mat))
  expect_identical(dim(mat), c(4L, 0L))
  # base R gives a zero-column matrix NULL dimnames, not character(0)
  expect_null(colnames(mat))

  df <- empty_out("data.frame")
  expect_identical(class(df), "data.frame")
  expect_identical(dim(df), c(4L, 0L))

  # The documented data.table caveat: only the zero-column part is promised.
  dt <- empty_out("dt")
  expect_true(data.table::is.data.table(dt))
  expect_identical(ncol(dt), 0L)
  expect_true(nrow(dt) %in% c(0L, 4L))

  mask <- empty_out("mask")
  expect_identical(mask, c(strong = FALSE, mirror = FALSE, weak = FALSE))

  idx <- empty_out("indices")
  expect_type(idx, "integer")
  expect_length(idx, 0L)

  nm <- empty_out("names")
  expect_identical(nm, character(0))

  lst <- empty_out("list")
  expect_identical(dim(lst$filtered), c(4L, 0L))
  expect_length(lst$indices, 0L)
  expect_identical(lst$names, character(0))
  expect_length(lst$scores, 3L)
  expect_identical(lst$meta$n_kept_cols, 0L)
})

test_that("all seven `out` shapes have the documented type and agree", {
  x <- sup_x()
  pick <- function(out) {
    fs_supervised(x = x, y = sup_y, method = "correlation", threshold = 0.5,
                  direction = "above", action = "keep", out = out)
  }

  mat <- pick("matrix")
  dt <- pick("dt")
  df <- pick("data.frame")
  mask <- pick("mask")
  idx <- pick("indices")
  nm <- pick("names")
  lst <- pick("list")

  expect_true(is.matrix(mat))
  expect_type(mat, "double")
  expect_identical(dim(mat), c(4L, 2L))
  expect_identical(colnames(mat), c("strong", "mirror"))

  expect_true(data.table::is.data.table(dt))
  expect_identical(names(dt), c("strong", "mirror"))
  expect_identical(nrow(dt), 4L)

  expect_identical(class(df), "data.frame")
  expect_identical(names(df), c("strong", "mirror"))
  expect_identical(nrow(df), 4L)

  expect_type(mask, "logical")
  expect_identical(mask, c(strong = TRUE, mirror = TRUE, weak = FALSE))

  expect_type(idx, "integer")
  expect_identical(unname(idx), c(1L, 2L))

  expect_type(nm, "character")
  expect_identical(nm, c("strong", "mirror"))

  expect_type(lst, "list")
  expect_named(
    lst,
    c("filtered", "mask", "indices", "names", "scores", "meta")
  )

  # mask / indices / names are mutually consistent
  expect_identical(unname(idx), which(unname(mask)))
  expect_identical(nm, names(mask)[idx])
  expect_identical(nm, colnames(mat))
  expect_identical(sum(mask), length(nm))
  expect_identical(lst$mask, mask)
  expect_identical(lst$names, nm)
  expect_identical(lst$filtered, mat)
  expect_identical(names(lst$scores), c("strong", "mirror", "weak"))

  expect_named(
    lst$meta,
    c("method_arg", "method_used", "threshold", "direction", "action",
      "include_equal", "na_rm", "n_input_cols", "n_kept_cols")
  )
  expect_identical(lst$meta$method_arg, "correlation")
  expect_identical(lst$meta$method_used, "correlation")
  expect_identical(lst$meta$threshold, 0.5)
  expect_identical(lst$meta$direction, "above")
  expect_identical(lst$meta$action, "keep")
  expect_false(lst$meta$include_equal)
  expect_true(lst$meta$na_rm)
  expect_identical(lst$meta$n_input_cols, 3L)
  expect_identical(lst$meta$n_kept_cols, 2L)
})

test_that("correlation scores equal abs(stats::cor(x, y)) (known answer)", {
  x <- data.frame(
    up   = c(1, 2, 3, 4),
    alt  = c(1, 0, 1, 0),
    bent = c(2, 4, 7, 9)
  )
  y <- c(1, 2, 3, 4)

  lst <- fs_supervised(x, y, method = "correlation", threshold = 0,
                       direction = "above", action = "keep",
                       include_equal = TRUE, out = "list")

  expected <- vapply(x, function(col) abs(stats::cor(col, y)), numeric(1L))
  expect_equal(lst$scores, expected)

  # Hand-checked values: a perfect linear feature and 1 / sqrt(5).
  expect_equal(lst$scores[["up"]], 1)
  expect_equal(lst$scores[["alt"]], 1 / sqrt(5))
  expect_true(all(lst$scores >= 0 & lst$scores <= 1))
})

test_that("the ANOVA path reproduces anova(lm())'s F statistic", {
  # 'wide' has group means 1.5 and 10.5 with within-group MS 0.5, so
  # F = (81 / 1) / (1 / 2) = 162. 'mild' gives F = (1 / 1) / (4 / 2) = 0.5.
  x <- data.frame(
    wide = c(1, 2, 10, 11),
    mild = c(1, 3, 2, 4)
  )
  y_fac <- factor(c("a", "a", "b", "b"))

  lst <- fs_supervised(x, y_fac, method = "anova", threshold = 0,
                       direction = "above", action = "keep", out = "list")

  expected <- vapply(
    x,
    function(col) stats::anova(stats::lm(col ~ y_fac))[["F value"]][1L],
    numeric(1L)
  )
  expect_equal(lst$scores, expected)
  expect_equal(lst$scores[["wide"]], 162)
  expect_equal(lst$scores[["mild"]], 0.5)
  expect_identical(lst$meta$method_used, "anova")
  expect_identical(lst$names, c("wide", "mild"))
})

test_that("method = 'auto' resolves by the type of y and reports it", {
  x <- sup_x()

  num <- fs_supervised(x, sup_y, method = "auto", out = "list")
  expect_identical(num$meta$method_arg, "auto")
  expect_identical(num$meta$method_used, "correlation")

  y_fac <- factor(c("a", "a", "b", "b"))
  fac <- fs_supervised(x, y_fac, method = "auto", out = "list")
  expect_identical(fac$meta$method_arg, "auto")
  expect_identical(fac$meta$method_used, "anova")

  chr <- fs_supervised(x, c("a", "a", "b", "b"), method = "auto", out = "list")
  expect_identical(chr$meta$method_used, "anova")

  lgl <- fs_supervised(x, c(TRUE, TRUE, FALSE, FALSE), method = "auto",
                       out = "list")
  expect_identical(lgl$meta$method_used, "anova")

  # The resolution note is emitted only for "auto", and only when logging.
  msgs <- capture_messages(
    fs_supervised(x, sup_y, method = "auto", out = "names", log_progress = TRUE)
  )
  expect_true(any(grepl("method = 'auto' resolved to 'correlation'", msgs,
                        fixed = TRUE)))
  expect_true(any(grepl("threshold scale", msgs, fixed = TRUE)))

  msgs_explicit <- capture_messages(
    fs_supervised(x, sup_y, method = "correlation", out = "names",
                  log_progress = TRUE)
  )
  expect_false(any(grepl("resolved to", msgs_explicit, fixed = TRUE)))

  expect_silent(fs_supervised(x, sup_y, method = "auto", out = "names"))
})

test_that("fs_supervised validates its scalar arguments and y", {
  x <- sup_x()

  expect_error(fs_supervised(x, sup_y, direction = "sideways"),
               "should be one of")
  expect_error(fs_supervised(x, sup_y, action = "discard"),
               "should be one of")
  expect_error(fs_supervised(x, sup_y, method = "not_a_method"),
               "should be one of")
  expect_error(fs_supervised(x, sup_y, out = "tibble"), "should be one of")

  expect_error(fs_supervised(x, sup_y, threshold = -0.1),
               "'threshold' must be between 0 and Inf")
  expect_error(fs_supervised(x, sup_y, threshold = "big"),
               "'threshold' must be a single finite number")
  expect_error(fs_supervised(x, sup_y, threshold = c(0.1, 0.2)),
               "'threshold' must be a single finite number")
  expect_error(fs_supervised(x, sup_y, threshold = Inf),
               "'threshold' must be a single finite number")
  expect_error(fs_supervised(x, sup_y, include_equal = 1L),
               "'include_equal' must be TRUE or FALSE")
  expect_error(fs_supervised(x, sup_y, na_rm = NA),
               "'na_rm' must be TRUE or FALSE")
  expect_error(fs_supervised(x, sup_y, log_progress = "yes"),
               "'log_progress' must be TRUE or FALSE")

  # Argument validation happens before `x` is ever converted.
  expect_error(fs_supervised("not a table", sup_y, direction = "sideways"),
               "should be one of")
  expect_error(fs_supervised("not a table", sup_y),
               "'x' must be a data.frame, data.table, or matrix")
  expect_error(
    fs_supervised(data.frame(a = 1:4, b = letters[1:4]), sup_y),
    "All columns of 'x' must be numeric"
  )

  expect_error(fs_supervised(x, sup_y[1:3]),
               "Length of `y` must equal number of rows in `x`")
  expect_error(fs_supervised(x, as.list(sup_y)),
               "`y` must be numeric, factor, character, or logical")
  expect_error(fs_supervised(x, cbind(sup_y, sup_y)),
               "`y` must be a vector, not a matrix or data frame")
  expect_error(
    fs_supervised(x, factor(c("a", "a", "b", "b")), method = "correlation"),
    "requires numeric `y`"
  )
  expect_error(fs_supervised(x, sup_y, method = "anova"),
               "requires categorical `y`")
})

test_that("fs_supervised's formals are stable and use `out`", {
  fx <- formals(fs_supervised)

  expect_identical(
    names(fx),
    c("x", "y", "method", "threshold", "direction", "action", "include_equal",
      "na_rm", "out", "log_progress")
  )
  # Sibling fs_unsupervised() spells this argument "output"; this one is "out".
  expect_true("out" %in% names(fx))
  expect_false("output" %in% names(fx))

  expect_identical(eval(fx$method), c("auto", "correlation", "anova"))
  expect_identical(eval(fx$direction), c("above", "below"))
  expect_identical(eval(fx$action), c("keep", "remove"))
  expect_identical(
    eval(fx$out),
    c("matrix", "dt", "data.frame", "mask", "indices", "names", "list")
  )
  expect_identical(fx$threshold, 0)
  expect_identical(fx$include_equal, FALSE)
  expect_identical(fx$na_rm, TRUE)
  expect_identical(fx$log_progress, FALSE)
})
