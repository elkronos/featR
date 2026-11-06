# Internal train/test split helper shared across featR.

#' Stratified train/test split indices
#'
#' Thin wrapper around `caret::createDataPartition()` that returns a plain
#' integer vector. Always use this instead of the raw matrix result: indexing
#' a data.table with the matrix that `createDataPartition(list = FALSE)`
#' returns is an error.
#'
#' Stratification is caret's: the classes of a factor/character outcome, or
#' quantile groups of a numeric one. caret is a suggested package, so this
#' checks for it first. The draw uses the RNG.
#'
#' @param y Outcome vector used for stratification.
#' @param p Proportion of data for the training set (strictly between 0 and 1).
#' @param seed Optional seed, applied locally and restored (see `local_seed`).
#'   Default `NULL`, which leaves the RNG alone.
#' @return Integer vector of training-row indices.
#' @noRd
fs_split_index <- function(y, p = 0.8, seed = NULL) {
  fs_require("caret", "data splitting")
  assert_number(p, "p")
  if (p <= 0 || p >= 1) {
    stop("'p' must be strictly between 0 and 1.", call. = FALSE)
  }
  # caret's own message for this ("y must have at least 2 data points") names
  # an argument the user never passed.
  if (length(y) < 2L) {
    stop("At least 2 rows are needed to split the data into training and ",
         "test sets.", call. = FALSE)
  }
  local_seed(seed)
  as.vector(caret::createDataPartition(y = y, p = p, list = FALSE))
}
