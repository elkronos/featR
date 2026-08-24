# Internal parallelism helper shared across featR.

#' Resolve a user-requested worker count
#'
#' featR is sequential by default: `NULL` or 1 means one worker. Requests are
#' capped at `parallel::detectCores()` (treated as 1 when detection fails), so
#' the result is never larger than what was asked for and never larger than
#' the machine reports. Over-requesting is therefore silently capped rather
#' than an error; zero, negative, and fractional counts are errors.
#'
#' featR never auto-detects a "use all cores" default (CRAN limits checks to 2
#' cores, and grabbing every core is hostile to users' machines).
#'
#' @param n_cores Requested worker count, or `NULL` for sequential.
#' @param arg Argument name used in error messages.
#' @return Integer >= 1.
#' @noRd
resolve_cores <- function(n_cores = 1L, arg = "n_cores") {
  if (is.null(n_cores)) {
    return(1L)
  }
  n <- assert_count(n_cores, arg, lower = 1L)
  max_cores <- parallel::detectCores()
  if (is.na(max_cores)) {
    max_cores <- 1L
  }
  min(n, max_cores)
}
