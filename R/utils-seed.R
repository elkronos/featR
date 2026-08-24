# Internal RNG helper shared across featR.

#' Temporarily set the RNG seed
#'
#' When `seed` is not `NULL`, sets it for the duration of the *calling*
#' function and restores the previous RNG state on exit, via
#' `withr::local_seed()`. Restoration covers the absence of a state too: if
#' the caller had never drawn a random number, `.Random.seed` is removed again
#' rather than left behind. A call therefore never perturbs the caller's
#' random stream, and two calls with the same seed give the same answer.
#'
#' No-op when `seed` is `NULL`, which is the default everywhere in featR:
#' featR never seeds the RNG unless the user asks for it.
#'
#' @param seed Single finite number (coerced with `as.integer()`), or `NULL`.
#' @param .envir Environment whose exit triggers restoration; defaults to the
#'   caller, which is what scopes the seed to the calling function.
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
