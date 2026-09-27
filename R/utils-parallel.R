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

#' Snapshot the currently registered foreach %dopar% backend
#'
#' foreach keeps its registered backend (`fun`, `data`, `info`) in an internal
#' environment and exports a setter (`foreach::setDoPar()`) but no getter, so
#' the environment is read directly. An empty environment (nothing ever
#' registered) snapshots as `NULL`.
#'
#' @return A list with `fun`, `data`, and `info`, or `NULL`.
#' @noRd
foreach_backend_snapshot <- function() {
  g <- get0(".foreachGlobals", envir = asNamespace("foreach"),
            inherits = FALSE)
  if (!is.environment(g) || !exists("fun", envir = g, inherits = FALSE)) {
    return(NULL)
  }
  list(
    fun  = get("fun", envir = g, inherits = FALSE),
    data = get0("data", envir = g, inherits = FALSE),
    info = get0("info", envir = g, inherits = FALSE)
  )
}

#' Restore a foreach backend captured by `foreach_backend_snapshot()`
#'
#' With nothing captured, the sequential backend is registered, which is what
#' an unregistered session behaves as.
#' @noRd
foreach_backend_restore <- function(prev) {
  if (is.null(prev) || !is.function(prev$fun)) {
    foreach::registerDoSEQ()
  } else {
    foreach::setDoPar(prev$fun, prev$data, prev$info)
  }
  invisible(NULL)
}

#' Start a PSOCK cluster, register it with foreach, and tear it down on exit
#'
#' The single implementation behind every opt-in parallel path in featR.
#' Teardown is scheduled on `.envir` *before* the backend is registered, so a
#' failure in registration cannot leak the cluster. On exit the cluster is
#' stopped and the caller's previous foreach backend -- including one they
#' registered themselves -- is restored, rather than forcing the session back
#' to sequential execution or leaving it pointing at a stopped cluster.
#'
#' @param n_cores Worker count, already resolved via `resolve_cores()`.
#' @param seed Optional seed; when supplied, reproducible L'Ecuyer-CMRG
#'   streams are set on the workers.
#' @param .envir Environment whose exit triggers the teardown.
#' @return The cluster object, invisibly.
#' @noRd
local_parallel_cluster <- function(n_cores, seed = NULL,
                                   .envir = parent.frame()) {
  fs_require(c("foreach", "doParallel"), "parallel execution")
  prev <- foreach_backend_snapshot()
  cl <- parallel::makeCluster(n_cores)
  withr::defer({
    try(parallel::stopCluster(cl), silent = TRUE)
    try(foreach_backend_restore(prev), silent = TRUE)
  }, envir = .envir)
  doParallel::registerDoParallel(cl)
  if (!is.null(seed)) {
    parallel::clusterSetRNGStream(cl, iseed = as.integer(seed))
  }
  invisible(cl)
}
