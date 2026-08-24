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
#' @param na_rm Single flag; remove NAs when computing scores. Ignored by
#'   "missing_prop" and "n_unique", which always account for NAs the same way.
#' @param verbose Single flag; emit progress messages.
#' @return Named numeric vector of scores (length = ncol(dt)).
#' @noRd
unsup_scores <- function(dt,
                         method,
                         na_rm = TRUE,
                         verbose = FALSE) {
  if (verbose) {
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
      function(col) {
        # stats::IQR() delegates to stats::quantile(), which *errors* on NAs
        # when na.rm = FALSE instead of returning NA the way var() and mad()
        # do. Keep the documented NA policy: an undefined score, not a crash.
        if (!na_rm && anyNA(col)) {
          return(NA_real_)
        }
        stats::IQR(col, na.rm = na_rm)
      },
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
#' No target is involved, so this is the right tool for the first cleaning
#' pass -- dropping constant or near-constant columns, columns that are mostly
#' missing, or columns with too few distinct values -- and it is safe to run
#' before a train/test split, since nothing about the outcome informs it. It
#' says nothing about whether a feature is \emph{useful}: a high-variance
#' column can be pure noise, and a low-variance one can be the best predictor
#' you have. Use [fs_supervised()] or a model-based method for that judgment.
#'
#' Supported methods:
#' \itemize{
#'   \item \code{"variance"}: Sample variance (denominator n - 1).
#'   \item \code{"mad"}: Median absolute deviation, computed with
#'     \code{stats::mad()}'s default consistency constant 1.4826
#'     (normal-consistent). Scores are therefore on the standard-deviation
#'     (sigma) estimate scale, not the raw median-absolute-deviation scale,
#'     and thresholds should be chosen accordingly.
#'   \item \code{"iqr"}: Interquartile range.
#'   \item \code{"range"}: Max - Min.
#'   \item \code{"missing_prop"}: Proportion of missing values, in
#'     \code{[0, 1]}. Note that with the default \code{threshold = 0},
#'     \code{direction = "above"}, and \code{action = "keep"}, this KEEPS the
#'     features with the most missing values, which is almost never the
#'     intent; a warning suggests \code{action = "remove"} or
#'     \code{direction = "below"}. The warning fires only when all three of
#'     \code{threshold}, \code{direction}, and \code{action} are left at their
#'     defaults, so passing any one of them explicitly opts out of the
#'     advisory.
#'   \item \code{"n_unique"}: Number of unique non-NA values.
#' }
#'
#' \code{na_rm} affects only the methods that summarize the observed values
#' (\code{"variance"}, \code{"mad"}, \code{"iqr"}, \code{"range"});
#' \code{"missing_prop"} and \code{"n_unique"} always look at the whole
#' column and treat NA as NA.
#'
#' Features whose score is undefined (\code{NA}) are never selected, under
#' both \code{action = "keep"} and \code{action = "remove"}; a warning
#' reports how many such features were excluded.
#'
#' Columns are subset by integer index, never by name, so duplicated column
#' names cannot select the wrong columns.
#'
#' @param data A data.frame, data.table, or matrix; all columns must be
#'   numeric. Every column is a candidate feature. The input is copied, never
#'   modified in place.
#' @param method One of \code{"variance"} (default), \code{"mad"},
#'   \code{"iqr"}, \code{"range"}, \code{"missing_prop"}, \code{"n_unique"}.
#' @param threshold Non-negative, finite numeric scalar threshold applied to
#'   the feature scores. Default 0. The scales differ by method (a variance is
#'   in squared units, \code{"missing_prop"} is in \code{[0, 1]},
#'   \code{"n_unique"} is a count), so a threshold is not portable between
#'   methods.
#' @param direction One of \code{"above"} (default), \code{"below"}; compares
#'   scores to \code{threshold}.
#' @param action One of \code{"keep"} (default), \code{"remove"}; determines
#'   whether features meeting the condition are retained or dropped.
#' @param include_equal Logical; if TRUE, comparisons are inclusive
#'   (greater/less than or equal) instead of strict. Default FALSE.
#' @param na_rm Logical; if TRUE (the default), remove NAs when computing
#'   scores. Has no effect on \code{"missing_prop"} and \code{"n_unique"},
#'   which always account for NAs the same way.
#' @param output One of \code{"result"} (default), \code{"matrix"},
#'   \code{"dt"}, \code{"data.frame"}, \code{"mask"}, \code{"indices"},
#'   \code{"names"}, \code{"list"}.
#'   \itemize{
#'     \item \code{"result"} (default): an \code{fs_result} object (see
#'       Value).
#'     \item \code{"matrix"}: numeric matrix of the selected features.
#'     \item \code{"dt"}: data.table of the selected features.
#'     \item \code{"data.frame"}: data.frame of the selected features.
#'     \item \code{"mask"}: logical vector of length \code{ncol(data)}, named
#'       after the columns of `data`; TRUE marks a selected column.
#'     \item \code{"indices"}: integer vector of the selected column indices,
#'       named after the selected columns.
#'     \item \code{"names"}: character vector of the selected column names.
#'     \item \code{"list"}: list with components \code{filtered} (matrix),
#'       \code{mask}, \code{indices}, \code{names}, \code{scores} (all as
#'       above), and \code{meta}, a list recording \code{method},
#'       \code{threshold}, \code{direction}, \code{action},
#'       \code{include_equal}, \code{na_rm}, \code{n_input_cols}, and
#'       \code{n_kept_cols}.
#'   }
#' @param verbose Logical; emit progress messages. Default FALSE.
#'
#' @return With the default \code{output = "result"}, an object of class
#'   `fs_result` with elements:
#'   \itemize{
#'     \item `selected`: character vector of selected feature names.
#'     \item `scores`: named numeric vector of per-feature scores, in column
#'       order, `NA` where the score is undefined.
#'     \item `method`: `"unsupervised_"` followed by the scoring method, for
#'       example `"unsupervised_variance"`.
#'     \item `task`: `NA_character_` (unsupervised selection has no task).
#'     \item `model`: `NULL`.
#'     \item `details`: a list holding, in this order, `mask` (the logical
#'       keep-mask over the candidate features), `indices` (the selected
#'       column indices, named after the selected columns), `filtered` (the
#'       selected columns as a data.table), `threshold`, `direction`,
#'       `action`, and `n_features` (the number of candidate features, that
#'       is `ncol(data)`).
#'     \item `call`: the matched call.
#'   }
#'   Any other `output` returns that shape instead, exactly as documented
#'   above. When no feature meets the selection criteria, a warning is issued
#'   and the tabular shapes come back empty: \code{"matrix"} and
#'   \code{"data.frame"} have zero columns and keep the input row count, while
#'   the \code{"dt"} shape (and `details$filtered`) is only guaranteed to have
#'   zero columns -- data.table represents a zero-column table as having zero
#'   rows, so its row count is not preserved.
#'
#' @examples
#' df <- data.frame(
#'   spread = c(1, 2, 3, 4, 100),
#'   flat   = c(2, 2, 2, 2, 2),
#'   gappy  = c(1, NA, 3, NA, 5)
#' )
#'
#' # Default: an fs_result
#' res <- fs_unsupervised(df, method = "variance", threshold = 0.5)
#' res$selected
#' res$scores
#' res$details$filtered
#'
#' # The classic shapes are still available
#' fs_unsupervised(df, method = "variance", threshold = 0.5,
#'                 output = "matrix")
#'
#' # Remove features with missing proportion >= 0.2
#' fs_unsupervised(df, method = "missing_prop", threshold = 0.2,
#'                 direction = "above", action = "remove",
#'                 include_equal = TRUE, output = "names")
#' @export
fs_unsupervised <- function(data,
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
                            output = c("result", "matrix", "dt", "data.frame",
                                       "mask", "indices", "names", "list"),
                            verbose = FALSE) {
  cl <- match.call()

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
  assert_flag(verbose, "verbose")

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

  dt <- as_dt(data, arg = "data")
  if (!all(vapply(dt, is.numeric, logical(1L)))) {
    stop("All columns of 'data' must be numeric.", call. = FALSE)
  }

  scores <- unsup_scores(
    dt = dt,
    method = method,
    na_rm = na_rm,
    verbose = verbose
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

  if (verbose) {
    op <- if (direction == "above") ">" else "<"
    if (include_equal) op <- paste0(op, "=")
    verb <- if (action == "keep") "Retaining" else "Removing"
    message(sprintf(
      "%s features with unsupervised score %s %s", verb, op, threshold
    ))
    message(sprintf("Kept %d of %d features.", length(keep_idx), ncol(dt)))
  }

  if (output == "result") {
    return(new_fs_result(
      selected = filter_output(dt, scores, mask, "names"),
      scores = scores,
      method = paste0("unsupervised_", method),
      task = NA_character_,
      model = NULL,
      details = list(
        mask = mask,
        indices = keep_idx,
        filtered = filter_output(dt, scores, mask, "dt"),
        threshold = threshold,
        direction = direction,
        action = action,
        n_features = ncol(dt)
      ),
      call = cl
    ))
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
