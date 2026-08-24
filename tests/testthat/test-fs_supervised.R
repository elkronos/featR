# Tests for fs_supervised().
#
# Scoring uses base R plus stats (correlation, lm/anova) and data.table only
# as the column container; all three are Imports, so every test in this file
# runs unconditionally and no skip_*() calls are needed.
#
# Deterministic 4-row toy set (no RNG). Against the target y = 1, 2, 3, 4:
#   strong = 1, 2, 3, 4 -> r =  1        -> |r| = 1
#   mirror = 4, 3, 2, 1 -> r = -1        -> |r| = 1
#   weak   = 1, 0, 1, 0 -> r = -1/sqrt(5) -> |r| = 0.4472136
# A threshold of 0.5 therefore splits the set cleanly into {strong, mirror}
# and {weak}. The target now lives inside `data`, as the column named "y".
sup_data <- function() {
  data.frame(
    strong = c(1, 2, 3, 4),
    mirror = c(4, 3, 2, 1),
    weak   = c(1, 0, 1, 0),
    y      = c(1, 2, 3, 4)
  )
}

# Two numeric features and a categorical target for the ANOVA path.
# 'wide' has group means 1.5 and 10.5 with within-group MS 0.5, so
# F = (81 / 1) / (1 / 2) = 162. 'mild' gives F = (1 / 1) / (4 / 2) = 0.5.
sup_anova_data <- function(grp = factor(c("a", "a", "b", "b"))) {
  data.frame(
    wide = c(1, 2, 10, 11),
    mild = c(1, 3, 2, 4),
    grp  = grp
  )
}

test_that("output = 'result' is the default and carries the documented details", {
  df <- sup_data()
  res <- fs_supervised(df, target = "y", method = "correlation",
                       threshold = 0.5)

  expect_s3_class(res, "fs_result")
  expect_identical(res$selected, c("strong", "mirror"))
  expect_identical(res$method, "supervised_correlation")
  expect_identical(res$task, "regression")
  expect_null(res$model)
  expect_true(is.call(res$call))

  # The target column is scored against, never scored.
  expect_identical(names(res$scores), c("strong", "mirror", "weak"))
  expect_false("y" %in% names(res$scores))

  expect_named(
    res$details,
    c("mask", "indices", "filtered", "threshold", "direction", "action",
      "n_features")
  )
  expect_identical(res$details$mask,
                   c(strong = TRUE, mirror = TRUE, weak = FALSE))
  expect_identical(unname(res$details$indices), c(1L, 2L))
  expect_true(data.table::is.data.table(res$details$filtered))
  expect_identical(names(res$details$filtered), c("strong", "mirror"))
  expect_identical(nrow(res$details$filtered), 4L)
  expect_identical(res$details$threshold, 0.5)
  expect_identical(res$details$direction, "above")
  expect_identical(res$details$action, "keep")
  expect_identical(res$details$n_features, 3L)

  # The result agrees with the shape-selecting outputs.
  expect_identical(
    fs_supervised(df, target = "y", method = "correlation", threshold = 0.5,
                  output = "names"),
    res$selected
  )
  expect_identical(
    fs_supervised(df, target = "y", method = "correlation", threshold = 0.5,
                  output = "mask"),
    res$details$mask
  )

  # A matrix carrying the target column still works.
  expect_identical(
    fs_supervised(as.matrix(df), target = "y", method = "correlation",
                  threshold = 0.5, output = "names"),
    c("strong", "mirror")
  )
})

test_that("an undefined (NA) score is excluded under BOTH keep and remove", {
  # 'const' has zero variance, so its correlation with the target is
  # undefined. The NA-inversion regression: under action = "remove" the
  # negated comparison must NOT turn that NA into a selected feature.
  df <- data.frame(
    strong = c(1, 2, 3, 4),
    const  = c(5, 5, 5, 5),
    weak   = c(1, 0, 1, 0),
    y      = c(1, 2, 3, 4)
  )

  expect_warning(
    kept <- fs_supervised(df, target = "y", method = "correlation",
                          threshold = 0.5, direction = "above",
                          action = "keep", output = "names"),
    "undefined scores and were excluded: const"
  )
  expect_identical(kept, "strong")

  expect_warning(
    dropped <- fs_supervised(df, target = "y", method = "correlation",
                             threshold = 0.5, direction = "above",
                             action = "remove", output = "names"),
    "undefined scores and were excluded: const"
  )
  expect_identical(dropped, "weak")

  mask_keep <- suppressWarnings(
    fs_supervised(df, target = "y", method = "correlation", threshold = 0.5,
                  direction = "above", action = "keep", output = "mask")
  )
  mask_remove <- suppressWarnings(
    fs_supervised(df, target = "y", method = "correlation", threshold = 0.5,
                  direction = "above", action = "remove", output = "mask")
  )
  expect_identical(mask_keep, c(strong = TRUE, const = FALSE, weak = FALSE))
  expect_identical(mask_remove, c(strong = FALSE, const = FALSE, weak = TRUE))
  expect_false(mask_keep[["const"]])
  expect_false(mask_remove[["const"]])

  # The undefined score is still reported back to the caller, in every shape.
  lst <- suppressWarnings(
    fs_supervised(df, target = "y", method = "correlation", threshold = 0.5,
                  direction = "above", action = "keep", output = "list")
  )
  expect_true(is.na(lst$scores[["const"]]))
  expect_identical(lst$meta$n_input_cols, 3L)
  expect_identical(lst$meta$n_kept_cols, 1L)

  res <- suppressWarnings(
    fs_supervised(df, target = "y", method = "correlation", threshold = 0.5,
                  direction = "above", action = "remove")
  )
  expect_true(is.na(res$scores[["const"]]))
  expect_identical(res$selected, "weak")
  expect_false(res$details$mask[["const"]])
})

test_that("duplicated column names are resolved by index, not by name", {
  # Both feature columns are called "dup"; only the SECOND clears the
  # threshold, so the returned data must carry the second column's values.
  x <- data.table::data.table(
    first  = c(0, 1, 0, 1),
    second = c(1, 2, 3, 4),
    y      = c(1, 2, 3, 4)
  )
  data.table::setnames(x, c("dup", "dup", "y"))
  expect_identical(names(x), c("dup", "dup", "y"))

  args <- list(data = x, target = "y", method = "correlation",
               threshold = 0.5, direction = "above", action = "keep")

  idx <- do.call(fs_supervised, c(args, list(output = "indices")))
  expect_type(idx, "integer")
  expect_identical(unname(idx), 2L)

  mask <- do.call(fs_supervised, c(args, list(output = "mask")))
  expect_identical(unname(mask), c(FALSE, TRUE))

  mat <- do.call(fs_supervised, c(args, list(output = "matrix")))
  expect_true(is.matrix(mat))
  expect_identical(dim(mat), c(4L, 1L))
  expect_equal(as.numeric(mat[, 1L]), c(1, 2, 3, 4))

  df <- do.call(fs_supervised, c(args, list(output = "data.frame")))
  expect_identical(class(df), "data.frame")
  expect_identical(dim(df), c(4L, 1L))
  expect_equal(df[[1L]], c(1, 2, 3, 4))

  dt <- do.call(fs_supervised, c(args, list(output = "dt")))
  expect_true(data.table::is.data.table(dt))
  expect_identical(ncol(dt), 1L)
  expect_equal(dt[[1L]], c(1, 2, 3, 4))

  lst <- do.call(fs_supervised, c(args, list(output = "list")))
  expect_equal(as.numeric(lst$filtered[, 1L]), c(1, 2, 3, 4))
  expect_identical(unname(lst$indices), 2L)
  expect_length(lst$names, 1L)

  res <- do.call(fs_supervised, args)
  expect_identical(unname(res$details$indices), 2L)
  expect_identical(res$selected, "dup")
  expect_equal(res$details$filtered[[1L]], c(1, 2, 3, 4))
  expect_identical(res$details$n_features, 2L)

  # The caller's table is untouched by the extraction of the target.
  expect_identical(names(x), c("dup", "dup", "y"))
  expect_identical(ncol(x), 3L)
})

test_that("an impossible threshold yields empty shapes that preserve nrow", {
  df <- sup_data()
  empty_out <- function(output) {
    suppressWarnings(
      fs_supervised(df, target = "y", method = "correlation",
                    threshold = 1.1, direction = "above", action = "keep",
                    output = output)
    )
  }

  expect_warning(
    fs_supervised(df, target = "y", method = "correlation", threshold = 1.1,
                  direction = "above", action = "keep", output = "names"),
    "No features meet the specified supervised selection criteria"
  )

  mat <- empty_out("matrix")
  expect_true(is.matrix(mat))
  expect_identical(dim(mat), c(4L, 0L))
  # base R gives a zero-column matrix NULL dimnames, not character(0)
  expect_null(colnames(mat))

  out_df <- empty_out("data.frame")
  expect_identical(class(out_df), "data.frame")
  expect_identical(dim(out_df), c(4L, 0L))

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

  res <- empty_out("result")
  expect_s3_class(res, "fs_result")
  expect_identical(res$selected, character(0))
  expect_length(res$scores, 3L)
  expect_identical(ncol(res$details$filtered), 0L)
  expect_length(res$details$indices, 0L)
  expect_identical(res$details$n_features, 3L)
})

test_that("all eight `output` shapes have the documented type and agree", {
  df <- sup_data()
  pick <- function(output) {
    fs_supervised(df, target = "y", method = "correlation", threshold = 0.5,
                  direction = "above", action = "keep", output = output)
  }

  res <- pick("result")
  mat <- pick("matrix")
  dt <- pick("dt")
  out_df <- pick("data.frame")
  mask <- pick("mask")
  idx <- pick("indices")
  nm <- pick("names")
  lst <- pick("list")

  expect_s3_class(res, "fs_result")

  expect_true(is.matrix(mat))
  expect_type(mat, "double")
  expect_identical(dim(mat), c(4L, 2L))
  expect_identical(colnames(mat), c("strong", "mirror"))

  expect_true(data.table::is.data.table(dt))
  expect_identical(names(dt), c("strong", "mirror"))
  expect_identical(nrow(dt), 4L)

  expect_identical(class(out_df), "data.frame")
  expect_identical(names(out_df), c("strong", "mirror"))
  expect_identical(nrow(out_df), 4L)

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

  # ... and so is the fs_result
  expect_identical(res$selected, nm)
  expect_identical(res$details$mask, mask)
  expect_identical(res$details$indices, idx)
  expect_identical(res$scores, lst$scores)
  expect_identical(names(res$details$filtered), names(dt))

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

test_that("correlation scores equal abs(stats::cor(feature, target)) (known answer)", {
  df <- data.frame(
    up   = c(1, 2, 3, 4),
    alt  = c(1, 0, 1, 0),
    bent = c(2, 4, 7, 9),
    y    = c(1, 2, 3, 4)
  )

  res <- fs_supervised(df, target = "y", method = "correlation",
                       threshold = 0, direction = "above", action = "keep",
                       include_equal = TRUE)

  expected <- vapply(
    df[c("up", "alt", "bent")],
    function(col) abs(stats::cor(col, df$y)),
    numeric(1L)
  )
  expect_equal(res$scores, expected)

  # Hand-checked values: a perfect linear feature and 1 / sqrt(5).
  expect_equal(res$scores[["up"]], 1)
  expect_equal(res$scores[["alt"]], 1 / sqrt(5))
  expect_true(all(res$scores >= 0 & res$scores <= 1))
})

test_that("the ANOVA path reproduces anova(lm())'s F statistic", {
  df <- sup_anova_data()

  res <- fs_supervised(df, target = "grp", method = "anova", threshold = 0,
                       direction = "above", action = "keep")

  expected <- vapply(
    df[c("wide", "mild")],
    function(col) stats::anova(stats::lm(col ~ df$grp))[["F value"]][1L],
    numeric(1L)
  )
  expect_equal(res$scores, expected)
  expect_equal(res$scores[["wide"]], 162)
  expect_equal(res$scores[["mild"]], 0.5)
  expect_identical(res$method, "supervised_anova")
  expect_identical(res$task, "classification")
  expect_identical(res$selected, c("wide", "mild"))
})

test_that("method = 'auto' resolves by the type of the target and reports it", {
  num <- fs_supervised(sup_data(), target = "y", method = "auto")
  expect_identical(num$method, "supervised_correlation")
  expect_identical(num$task, "regression")

  fac <- fs_supervised(sup_anova_data(), target = "grp", method = "auto")
  expect_identical(fac$method, "supervised_anova")
  expect_identical(fac$task, "classification")

  # Character and logical targets are scored with ANOVA, but the intended
  # task is genuinely ambiguous, so it is reported as NA rather than guessed.
  chr <- fs_supervised(sup_anova_data(grp = c("a", "a", "b", "b")),
                       target = "grp", method = "auto")
  expect_identical(chr$method, "supervised_anova")
  expect_true(is.na(chr$task))

  lgl <- fs_supervised(sup_anova_data(grp = c(TRUE, TRUE, FALSE, FALSE)),
                       target = "grp", method = "auto")
  expect_identical(lgl$method, "supervised_anova")
  expect_true(is.na(lgl$task))

  # The old `list` shape still reports both the requested and resolved method.
  lst <- fs_supervised(sup_data(), target = "y", method = "auto",
                       output = "list")
  expect_identical(lst$meta$method_arg, "auto")
  expect_identical(lst$meta$method_used, "correlation")

  # The resolution note is emitted only for "auto", and only when verbose.
  msgs <- capture_messages(
    fs_supervised(sup_data(), target = "y", method = "auto",
                  output = "names", verbose = TRUE)
  )
  expect_true(any(grepl("method = 'auto' resolved to 'correlation'", msgs,
                        fixed = TRUE)))
  expect_true(any(grepl("threshold scale", msgs, fixed = TRUE)))

  msgs_explicit <- capture_messages(
    fs_supervised(sup_data(), target = "y", method = "correlation",
                  output = "names", verbose = TRUE)
  )
  expect_false(any(grepl("resolved to", msgs_explicit, fixed = TRUE)))

  expect_silent(
    fs_supervised(sup_data(), target = "y", method = "auto", output = "names")
  )
})

test_that("fs_supervised validates its arguments and its target column", {
  df <- sup_data()

  expect_error(fs_supervised(df, "y", direction = "sideways"),
               "should be one of")
  expect_error(fs_supervised(df, "y", action = "discard"), "should be one of")
  expect_error(fs_supervised(df, "y", method = "not_a_method"),
               "should be one of")
  expect_error(fs_supervised(df, "y", output = "tibble"), "should be one of")

  expect_error(fs_supervised(df, "y", threshold = -0.1),
               "'threshold' must be between 0 and Inf")
  expect_error(fs_supervised(df, "y", threshold = "big"),
               "'threshold' must be a single finite number")
  expect_error(fs_supervised(df, "y", threshold = c(0.1, 0.2)),
               "'threshold' must be a single finite number")
  expect_error(fs_supervised(df, "y", threshold = Inf),
               "'threshold' must be a single finite number")
  expect_error(fs_supervised(df, "y", include_equal = 1L),
               "'include_equal' must be TRUE or FALSE")
  expect_error(fs_supervised(df, "y", na_rm = NA),
               "'na_rm' must be TRUE or FALSE")
  expect_error(fs_supervised(df, "y", verbose = "yes"),
               "'verbose' must be TRUE or FALSE")
  expect_error(fs_supervised(df, target = 1),
               "'target' must be a single non-empty character string")

  # Argument validation happens before `data` is ever converted.
  expect_error(fs_supervised("not a table", "y", direction = "sideways"),
               "should be one of")
  expect_error(fs_supervised("not a table", "y"),
               "'data' must be a data.frame, data.table, or matrix")

  expect_error(fs_supervised(df, "nope"), "Column 'nope' not found in 'data'")
  expect_error(
    fs_supervised(data.frame(a = 1:4, b = letters[1:4], y = c(1, 2, 3, 4)),
                  "y"),
    "All feature columns of 'data' must be numeric"
  )
  expect_error(
    fs_supervised(data.frame(y = c(1, 2, 3, 4)), "y"),
    "at least one feature column besides 'target'"
  )

  # A target that is neither numeric nor categorical cannot be scored.
  expect_error(
    fs_supervised(
      data.frame(a = c(1, 2, 3, 4), b = c(4, 3, 2, 1),
                 y = as.Date("2020-01-01") + 0:3),
      "y"
    ),
    "must be numeric, factor, character, or logical"
  )

  expect_error(
    fs_supervised(sup_anova_data(), target = "grp", method = "correlation"),
    "requires a numeric target"
  )
  expect_error(fs_supervised(df, "y", method = "anova"),
               "requires a categorical target")
})

test_that("the result prints", {
  res <- fs_supervised(sup_data(), target = "y", method = "correlation",
                       threshold = 0.5)
  expect_output(print(res), "fs_result")
  expect_output(print(res), "supervised_correlation")
})

test_that("fs_supervised's formals match the unified API", {
  fx <- formals(fs_supervised)

  expect_identical(
    names(fx),
    c("data", "target", "method", "threshold", "direction", "action",
      "include_equal", "na_rm", "output", "verbose")
  )
  # The x/y pair, the old `out` spelling and `log_progress` are all gone.
  expect_false(any(c("x", "y", "out", "log_progress") %in% names(fx)))

  expect_identical(eval(fx$method), c("auto", "correlation", "anova"))
  expect_identical(eval(fx$direction), c("above", "below"))
  expect_identical(eval(fx$action), c("keep", "remove"))
  expect_identical(
    eval(fx$output),
    c("result", "matrix", "dt", "data.frame", "mask", "indices", "names",
      "list")
  )
  # Both filter front ends now spell the shape argument the same way.
  expect_identical(eval(fx$output), eval(formals(fs_unsupervised)$output))
  expect_identical(fx$threshold, 0)
  expect_identical(fx$include_equal, FALSE)
  expect_identical(fx$na_rm, TRUE)
  expect_identical(fx$verbose, FALSE)
})
