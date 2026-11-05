# Internal validation and dependency helpers shared across featR.
# None of these are exported.

#' Null-coalescing operator: `x` unless it is NULL, in which case `y`
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Require suggested packages at runtime
#'
#' Stops with an informative message when packages listed in Suggests are
#' needed but not installed. Never installs anything.
#'
#' @param pkgs Character vector of package names.
#' @param purpose Optional short string describing what they are needed for.
#' @return Invisibly `TRUE` when all packages are available.
#' @noRd
fs_require <- function(pkgs, purpose = NULL) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "%s %s %s required%s but not installed. Install with: install.packages(c(%s))",
        if (length(missing) > 1L) "Packages" else "Package",
        paste0("'", missing, "'", collapse = ", "),
        if (length(missing) > 1L) "are" else "is",
        if (is.null(purpose)) "" else paste0(" for ", purpose),
        paste0("\"", missing, "\"", collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Assert that `x` is a data.frame (optionally a matrix) with at least one
#' row and one column
#' @noRd
assert_data_frame <- function(x, arg = "data", allow_matrix = FALSE) {
  ok <- is.data.frame(x) || (allow_matrix && is.matrix(x))
  if (!ok) {
    stop(sprintf("'%s' must be a data.frame%s.", arg,
                 if (allow_matrix) " or matrix" else ""), call. = FALSE)
  }
  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop(sprintf("'%s' must have at least one row and one column.", arg),
         call. = FALSE)
  }
  invisible(x)
}

#' Assert that `x` is a single non-empty string
#' @noRd
assert_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("'%s' must be a single non-empty character string.", arg),
         call. = FALSE)
  }
  invisible(x)
}

#' Assert that `x` is a single TRUE/FALSE
#' @noRd
assert_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("'%s' must be TRUE or FALSE.", arg), call. = FALSE)
  }
  invisible(x)
}

#' Assert that `x` is a single finite number, optionally within bounds
#' @noRd
assert_number <- function(x, arg, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(sprintf("'%s' must be a single finite number.", arg), call. = FALSE)
  }
  if (x < lower || x > upper) {
    stop(sprintf("'%s' must be between %s and %s.", arg, lower, upper),
         call. = FALSE)
  }
  invisible(x)
}

#' Assert that `x` is a single whole number >= `lower`
#'
#' Invisibly returns `as.integer(x)`, so callers can write
#' `n <- assert_count(n, "n")` to validate and coerce in one step.
#'
#' Wholeness is tested with `trunc()`, not `as.integer()`: a value beyond
#' `.Machine$integer.max` coerces to `NA` (with a coercion warning) and would
#' otherwise put `NA` into `if ()`. Such values are rejected with their own
#' message, since the return value has to be a representable integer.
#' @noRd
assert_count <- function(x, arg, lower = 1L) {
  assert_number(x, arg, lower = lower)
  if (x != trunc(x)) {
    stop(sprintf("'%s' must be a whole number.", arg), call. = FALSE)
  }
  if (abs(x) > .Machine$integer.max) {
    stop(sprintf("'%s' must be at most %d.", arg, .Machine$integer.max),
         call. = FALSE)
  }
  invisible(as.integer(x))
}

#' Assert that `target` names a column of `data`
#' @noRd
assert_target <- function(data, target, arg = "target") {
  assert_string(target, arg)
  if (!target %in% names(data)) {
    stop(sprintf("Column '%s' not found in 'data'.", target), call. = FALSE)
  }
  invisible(target)
}

#' Wrap non-syntactic names in backticks for formula construction
#'
#' Inside backticks the R parser still processes escapes, so a backslash or
#' backtick in the name is escaped first; otherwise a name such as `a\nb`
#' would parse as a *different* symbol (containing a newline) and a name
#' containing a backtick would not parse at all. Empty names cannot be
#' referenced in a formula and are an error.
#' @noRd
backtick <- function(x) {
  if (anyNA(x) || any(!nzchar(x))) {
    stop("Column names used in a model formula must be non-empty and not NA.",
         call. = FALSE)
  }
  needs <- x != make.names(x)
  esc <- gsub("\\", "\\\\", x[needs], fixed = TRUE)
  esc <- gsub("`", "\\`", esc, fixed = TRUE)
  x[needs] <- paste0("`", esc, "`")
  x
}

#' Assert that column names are usable as unique lookup keys
#'
#' Rejects duplicated, empty and `NA` column names, naming the offenders.
#' Methods that look columns up by name (or hand them to a modeling engine
#' that does) otherwise resolve every duplicate to its first copy, or fail
#' deep inside the engine ("undefined columns selected", "subscript out of
#' bounds", "duplicated name ... using '.'").
#'
#' @param data A data.frame (or matrix with column names).
#' @param fn Name of the calling function, for the message.
#' @return Invisibly `data`.
#' @noRd
assert_column_names <- function(data, fn) {
  nms <- colnames(data)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms))) {
    stop(sprintf("'data' must not have empty or NA column names; name every column before calling %s().",
                 fn), call. = FALSE)
  }
  dupes <- unique(nms[duplicated(nms)])
  if (length(dupes) > 0L) {
    stop(sprintf("'data' must have unique column names; duplicated: %s%s. Rename the duplicates before calling %s().",
                 paste(utils::head(dupes, 5L), collapse = ", "),
                 if (length(dupes) > 5L) ", ..." else "", fn),
         call. = FALSE)
  }
  invisible(data)
}

#' Assert that no class label of a classification target is an empty string
#'
#' A factor level `""` cannot be used as a dimname lookup key, so caret and
#' randomForest fail on it with "subscript out of bounds" or 'column name ""
#' cannot match any column'.
#' @noRd
assert_class_labels <- function(y, target) {
  lv <- if (is.factor(y)) levels(y) else unique(as.character(y[!is.na(y)]))
  if (any(!nzchar(lv))) {
    stop(sprintf("Target '%s' has an empty-string class label; recode it (e.g. to \"none\") first.",
                 target), call. = FALSE)
  }
  invisible(y)
}

#' Assert that numeric columns hold no infinite values
#'
#' `anyNA()` does not catch `Inf`/`-Inf`, which then reach modeling engines
#' and fail there with opaque messages (or are silently zeroed). Checks every
#' numeric (double/integer) column among `cols` and names the offenders.
#'
#' @param data A data.frame or matrix.
#' @param cols Column names to check (default: all).
#' @param fn Name of the calling function, for the message.
#' @return Invisibly `data`.
#' @noRd
assert_finite_columns <- function(data, cols = colnames(data), fn) {
  bad <- character(0)
  for (nm in cols) {
    x <- if (is.matrix(data)) data[, nm] else data[[nm]]
    if (is.numeric(x) && any(is.infinite(x))) bad <- c(bad, nm)
  }
  if (length(bad) > 0L) {
    stop(sprintf("%s() requires finite values; column(s) with Inf/-Inf: %s%s.",
                 fn, paste(utils::head(bad, 5L), collapse = ", "),
                 if (length(bad) > 5L) ", ..." else ""),
         call. = FALSE)
  }
  invisible(data)
}

#' Identify numerically constant numeric vectors
#'
#' Exact `sd == 0` misses columns that are constant up to floating-point
#' round-off (e.g. `1 + 1e-16 * rnorm(n)`), which centring/scaling then blows
#' up to full weight. A column is treated as constant when its spread is
#' at round-off level relative to its magnitude: the default `tol` of
#' `64 * .Machine$double.eps` (about 1.4e-14) flags spreads of a few dozen
#' units in the last place, while a large-offset column that genuinely varies
#' (an epoch timestamp in seconds or nanoseconds that varies by more than
#' about 1e-14 of its value) is kept. The test uses the range (no
#' squaring, so no underflow/overflow for tiny or huge units).
#'
#' @param x Numeric vector (NA ignored).
#' @param tol Relative tolerance.
#' @return `TRUE` when `x` is constant to within `tol`.
#' @noRd
is_numerically_constant <- function(x, tol = 64 * .Machine$double.eps) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(TRUE)
  r <- range(x)
  spread <- r[2L] - r[1L]
  if (spread == 0) return(TRUE)
  scale <- max(abs(r))
  spread <= tol * scale
}

#' Assert that encoded (dummy) column names are unique
#'
#' One-hot encoding names a dummy `<var><level>`, which can coincide with
#' another column (factor `g` with level `1` -> `g1`) or the target. Name-based
#' selection would then silently resolve to the wrong copy.
#'
#' @param nms Encoded predictor names.
#' @param target Target name (may be `NULL`).
#' @param fn Name of the calling function.
#' @noRd
assert_unique_encoded_names <- function(nms, target = NULL, fn) {
  all_nms <- c(target, nms)
  dupes <- unique(all_nms[duplicated(all_nms)])
  if (length(dupes) > 0L) {
    stop(sprintf("One-hot encoding in %s() produced column name(s) that collide with existing columns: %s. Rename the factor/level or the colliding column first.",
                 fn, paste(utils::head(dupes, 5L), collapse = ", ")),
         call. = FALSE)
  }
  invisible(nms)
}

#' Assert that a classification target has at least two observed classes
#' @noRd
assert_two_classes <- function(y, target, fn, where = "the data") {
  y <- droplevels(as.factor(y[!is.na(y)]))
  if (nlevels(y) < 2L) {
    stop(sprintf("%s(): target '%s' has fewer than two observed classes in %s.",
                 fn, target, where), call. = FALSE)
  }
  invisible(y)
}
