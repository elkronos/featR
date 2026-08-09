# Unsupervised, univariate, filter-based feature selection.
# Selection-mask and output-shaping machinery shared with fs_supervised()
# lives in R/utils-filter.R.

#' Compute unsupervised per-feature scores
#'
#' Methods:
#' - "variance": sample variance (denominator n - 1)
#' - "mad": median absolute deviation via stats::mad(), which applies the
#'   default consistency constant 1.4826 (normal-consistent)
#' - "iqr": interquartile range
#' - "range": max - min
#' - "missing_prop": proportion of missing values
#' - "n_unique": number of unique non-NA values
#'
#' `method` is validated by the exported caller before this helper runs.
#'
#' @param dt data.table of numeric feature columns.
#' @param method One of "variance", "mad", "iqr", "range", "missing_prop",
#'   "n_unique".
#' @param na_rm Single flag; remove NAs when computing scores (where
#'   applicable).
#' @param log_progress Single flag; emit progress messages.
#' @return Named numeric vector of scores (length = ncol(dt)).
#' @noRd
unsup_scores <- function(dt,
                         method,
                         na_rm = TRUE,
                         log_progress = FALSE) {
  if (log_progress) {
    message(sprintf(
      "Computing unsupervised feature scores using method '%s'...", method
    ))
  }

  scores <- switch(
    method,

    "variance" = vapply(
      dt,
      function(col) stats::var(col, na.rm = na_rm),
      numeric(1L)
    ),

    "mad" = vapply(
      dt,
      function(col) stats::mad(col, na.rm = na_rm),
      numeric(1L)
    ),

    "iqr" = vapply(
      dt,
      function(col) stats::IQR(col, na.rm = na_rm),
      numeric(1L)
    ),

    "range" = vapply(
      dt,
      function(col) {
        if (na_rm) {
          col2 <- col[!is.na(col)]
          if (length(col2) == 0L) {
            return(NA_real_)
          }
          r <- range(col2)
        } else {
          if (all(is.na(col))) {
            return(NA_real_)
          }
          r <- range(col)
        }
        diff(r)
      },
      numeric(1L)
    ),

    "missing_prop" = vapply(
      dt,
      function(col) {
        n <- length(col)
        if (n == 0L) {
          return(NA_real_)
        }
        sum(is.na(col)) / n
      },
      numeric(1L)
    ),

    "n_unique" = vapply(
      dt,
      function(col) length(unique(col[!is.na(col)])),
      numeric(1L)
    )
  )

  scores <- as.numeric(scores)
  names(scores) <- names(dt)
  scores
}

#' Unsupervised Filter-Based Feature Selection
#'
#' Performs unsupervised, univariate, filter-based feature selection by
#' scoring each feature using a chosen unsupervised criterion and selecting
#' or dropping features based on a threshold on that score.
#'
#' Supported methods:
#' \itemize{
#'   \item \code{"variance"}: Sample variance.
#'   \item \code{"mad"}: Median absolute deviation, computed with
#'     \code{stats::mad()}'s default consistency constant 1.4826
#'     (normal-consistent). Scores are therefore on the standard-deviation
#'     (sigma) estimate scale, not the raw median-absolute-deviation scale,
#'     and thresholds should be chosen accordingly.
#'   \item \code{"iqr"}: Interquartile range.
#'   \item \code{"range"}: Max - Min.
#'   \item \code{"missing_prop"}: Proportion of missing values. Note that
#'     with the default \code{threshold = 0}, \code{direction = "above"},
#'     and \code{action = "keep"}, this KEEPS the features with the most
#'     missing values, which is almost never the intent; a warning suggests
#'     \code{action = "remove"} or \code{direction = "below"} when the
#'     defaults are used with this method.
#'   \item \code{"n_unique"}: Number of unique non-NA values.
#' }
#'
#' Features whose score is undefined (\code{NA}) are never selected, under
#' both \code{action = "keep"} and \code{action = "remove"}; a warning
#' reports how many such features were excluded.
#'
#' @param x Numeric matrix, data.frame, or data.table (all numeric columns).
#' @param method One of \code{"variance"}, \code{"mad"}, \code{"iqr"},
#'   \code{"range"}, \code{"missing_prop"}, \code{"n_unique"}.
#' @param threshold Non-negative, finite numeric scalar threshold applied to
#'   the feature scores. Default 0.
#' @param direction One of \code{"above"}, \code{"below"}; compares scores to
#'   \code{threshold}.
#' @param action One of \code{"keep"}, \code{"remove"}; determines whether
#'   features meeting the condition are retained or dropped.
#' @param include_equal Logical; if TRUE, comparisons are inclusive
#'   (greater/less than or equal) instead of strict.
#' @param na_rm Logical; if TRUE, remove NAs when computing scores (where
#'   applicable).
#' @param output One of \code{"matrix"}, \code{"dt"}, \code{"data.frame"},
#'   \code{"mask"}, \code{"indices"}, \code{"names"}, \code{"list"}.
#'   \itemize{
#'     \item \code{"matrix"} (default): numeric matrix of selected features.
#'     \item \code{"dt"}: data.table of selected features.
#'     \item \code{"data.frame"}: data.frame of selected features.
#'     \item \code{"mask"}: logical vector of length \code{ncol(x)}.
#'     \item \code{"indices"}: integer vector of selected column indices.
#'     \item \code{"names"}: character vector of selected column names.
#'     \item \code{"list"}: list with components:
#'       \code{filtered} (matrix), \code{mask}, \code{indices}, \code{names},
#'       \code{scores}, and \code{meta}.
#'   }
#' @param log_progress Logical; emit progress messages.
#'
#' @return Depends on the `output` argument (see above). When no feature
#'   meets the selection criteria, a warning is issued and the tabular shapes
#'   (\code{"matrix"}, \code{"dt"}, \code{"data.frame"}) are returned with
#'   zero columns while preserving the input row count.
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(200), ncol = 5)
#'
#' # Keep features with variance > 0.5
#' out_var <- fs_unsupervised(
#'   x = X,
#'   method = "variance",
#'   threshold = 0.5,
#'   direction = "above",
#'   action = "keep",
#'   output = "list"
#' )
#'
#' # Remove features with missing proportion >= 0.2
#' X_na <- X
#' X_na[sample(length(X_na), 20)] <- NA
#' out_missing <- fs_unsupervised(
#'   x = X_na,
#'   method = "missing_prop",
#'   threshold = 0.2,
#'   direction = "above",
#'   action = "remove",
#'   include_equal = TRUE,
#'   output = "matrix"
#' )
#' @export
fs_unsupervised <- function(x,
                            method = c("variance",
                                       "mad",
                                       "iqr",
                                       "range",
                                       "missing_prop",
                                       "n_unique"),
                            threshold = 0,
                            direction = c("above", "below"),
                            action = c("keep", "remove"),
                            include_equal = FALSE,
                            na_rm = TRUE,
                            output = c("matrix", "dt", "data.frame",
                                       "mask", "indices", "names", "list"),
                            log_progress = FALSE) {
  # Capture before the arguments are touched: missing() is unreliable after
  # reassignment.
  used_defaults <- missing(direction) && missing(action) && missing(threshold)

  method <- match.arg(method)
  direction <- match.arg(direction)
  action <- match.arg(action)
  output <- match.arg(output)
  assert_number(threshold, "threshold", lower = 0)
  assert_flag(include_equal, "include_equal")
  assert_flag(na_rm, "na_rm")
  assert_flag(log_progress, "log_progress")

  if (method == "missing_prop" && used_defaults) {
    warning(
      paste0(
        "method = 'missing_prop' with the default threshold = 0, ",
        "direction = 'above', and action = 'keep' KEEPS the features with ",
        "the most missing values, which is almost never the intent. ",
        "Consider action = 'remove' or direction = 'below'."
      ),
      call. = FALSE
    )
  }

  dt <- as_dt(x, arg = "x")
  if (!all(vapply(dt, is.numeric, logical(1L)))) {
    stop("All columns of 'x' must be numeric.", call. = FALSE)
  }

  scores <- unsup_scores(
    dt = dt,
    method = method,
    na_rm = na_rm,
    log_progress = log_progress
  )

  mask <- filter_mask(
    scores = scores,
    threshold = threshold,
    direction = direction,
    action = action,
    include_equal = include_equal
  )
  keep_idx <- which(mask)

  if (length(keep_idx) == 0L) {
    warning("No features meet the specified unsupervised selection criteria.",
            call. = FALSE)
  }

  if (log_progress) {
    op <- if (direction == "above") ">" else "<"
    if (include_equal) op <- paste0(op, "=")
    verb <- if (action == "keep") "Retaining" else "Removing"
    message(sprintf(
      "%s features with unsupervised score %s %s", verb, op, threshold
    ))
    message(sprintf("Kept %d of %d features.", length(keep_idx), ncol(dt)))
  }

  res <- filter_output(dt, scores, mask, output)
  if (output == "list") {
    res$meta <- list(
      method = method,
      threshold = threshold,
      direction = direction,
      action = action,
      include_equal = include_equal,
      na_rm = na_rm,
      n_input_cols = ncol(dt),
      n_kept_cols = length(keep_idx)
    )
  }
  res
}
