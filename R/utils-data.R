# Internal data-handling helpers shared across featR.

# Make data.table's [.data.table NSE work inside this package.
.datatable.aware <- TRUE

#' Convert to data.table without mutating the caller's object
#'
#' `as.data.table()` already copies, but an input that is *already* a
#' data.table would be aliased; this helper always returns a copy so that
#' by-reference operations inside featR can never mutate user data.
#'
#' @param data data.frame, data.table, or matrix.
#' @param arg Argument name used in the error message.
#' @return A data.table copy. A matrix keeps its own column names (empty ones
#'   stay empty); `V1`, `V2`, ... are used only when it has none. Anything
#'   else is an error.
#' @noRd
as_dt <- function(data, arg = "data") {
  if (data.table::is.data.table(data)) {
    return(data.table::copy(data))
  }
  if (is.matrix(data)) {
    dt <- data.table::as.data.table(data)
    # as.data.table() replaces empty matrix colnames with V<j>, which can
    # collide with a real column (colnames c("V3", "") -> "V3", "V3") or name
    # a column that does not exist. Keep the matrix's own names, as the
    # data.frame path does; V<j> names are generated only when the matrix has
    # no colnames at all.
    nms <- colnames(data)
    if (!is.null(nms)) {
      nms[is.na(nms)] <- ""
      data.table::setnames(dt, nms)
    }
    return(dt)
  }
  if (is.data.frame(data)) {
    return(data.table::as.data.table(data))
  }
  stop(sprintf("'%s' must be a data.frame, data.table, or matrix.", arg),
       call. = FALSE)
}

#' Overflow/underflow-safe spread of a numeric column
#'
#' `stats::sd()` squares deviations, so a column whose magnitude is beyond
#' about 1e154 overflows to `Inf` and one below about 1e-154 underflows to 0
#' (or to an inaccurate subnormal value). Dividing by the column's largest
#' absolute value first makes the relative spread immune to both. Used by the
#' zero-variance tests of fs_pca()/fs_svd(): a column is numerically constant
#' when `rel <= tol`, and a column whose raw sd is not faithfully
#' representable (`representable = FALSE`) can still be standardised exactly
#' after dividing it by `scale`, because standardisation is invariant to
#' rescaling a column.
#'
#' @param x NA-free, finite numeric vector.
#' @return A list with `rel` (sd of `x / max(abs(x))`, 0 for an all-zero
#'   column), `scale` (`max(abs(x))`) and `representable` (whether
#'   `stats::sd(x)` agrees with `rel * scale`).
#' @noRd
safe_col_spread <- function(x) {
  m <- max(abs(x))
  if (length(x) < 2L || !is.finite(m) || m == 0) {
    return(list(rel = 0, scale = m, representable = TRUE))
  }
  s_rel <- stats::sd(x / m)
  s_raw <- stats::sd(x)
  target <- s_rel * m
  ok <- is.finite(s_raw) && is.finite(target) &&
    (s_rel == 0 || (target > 0 && abs(s_raw - target) <= 1e-6 * target))
  list(rel = s_rel, scale = m, representable = ok)
}

#' Reject empty or NA column names (duplicates allowed)
#'
#' For functions that subset by position and so tolerate duplicated names,
#' but whose output is keyed by name: an empty or NA name cannot be traced
#' back to a column.
#'
#' @param nms Character vector of column names.
#' @param fn Calling function name, for the message.
#' @return `nms`, invisibly.
#' @noRd
assert_nonempty_names <- function(nms, fn) {
  if (anyNA(nms) || any(!nzchar(nms))) {
    stop(sprintf("'data' must not have empty or NA column names; name every column before calling %s().",
                 fn), call. = FALSE)
  }
  invisible(nms)
}
