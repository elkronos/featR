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
#' @return A data.table copy. Anything else is an error.
#' @noRd
as_dt <- function(data, arg = "data") {
  if (data.table::is.data.table(data)) {
    return(data.table::copy(data))
  }
  if (is.data.frame(data) || is.matrix(data)) {
    return(data.table::as.data.table(data))
  }
  stop(sprintf("'%s' must be a data.frame, data.table, or matrix.", arg),
       call. = FALSE)
}
