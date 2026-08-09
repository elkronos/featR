# Internal validation and dependency helpers shared across featR.
# None of these are exported.

#' Null-coalescing operator
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

#' Assert that `x` is a data.frame (optionally allowing a matrix)
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

#' Assert that `x` is a single whole number >= `lower`; returns it as integer
#' @noRd
assert_count <- function(x, arg, lower = 1L) {
  assert_number(x, arg, lower = lower)
  if (x != as.integer(x)) {
    stop(sprintf("'%s' must be a whole number.", arg), call. = FALSE)
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
#' @noRd
backtick <- function(x) {
  needs <- x != make.names(x)
  x[needs] <- paste0("`", x[needs], "`")
  x
}
