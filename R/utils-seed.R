# Internal RNG helper shared across featR.

#' Temporarily set the RNG seed
#'
#' When `seed` is not `NULL`, sets it for the duration of the *calling*
#' function and restores the previous RNG state (including its absence) on
#' exit, via `withr::local_seed()`. No-op when `seed` is `NULL`. featR never
#' seeds the RNG unless the user asks for it.
#'
#' @param seed Single number or `NULL`.
#' @param .envir Environment whose exit triggers restoration.
#' @return Invisibly `NULL`.
#' @noRd
local_seed <- function(seed, .envir = parent.frame()) {
  if (is.null(seed)) {
    return(invisible(NULL))
  }
  assert_number(seed, "seed")
  withr::local_seed(as.integer(seed), .local_envir = .envir)
  invisible(NULL)
}
