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

#' Unsupervised filter that scores each feature without a target
#'
#' Scores each column of `data` with a summary that ignores any outcome
#' (variance, spread, missingness, or number of distinct values) and keeps or
#' drops each column by comparing its score with `threshold`.
#'
#' @details
#' Use it for the first cleaning pass: dropping constant or near-constant
#' columns, columns that are mostly missing, or columns with too few distinct
#' values. No outcome informs the scores, so it is safe to run before a
#' train/test split. For the same reason it says nothing about whether a
#' feature is useful. A high-variance column can be pure noise and a
#' low-variance one can be your best predictor; [fs_supervised()] or a
#' model-based method answers that question.
#'
#' The six values of `method`:
#' \itemize{
#'   \item `"variance"` (default): sample variance (denominator n - 1), in
#'     squared units. For values below about `1e-162` in magnitude the
#'     variance underflows to 0, so to find constant columns in extreme units
#'     use `"range"` with threshold 0 or `"n_unique"` with threshold 1.
#'   \item `"mad"`: median absolute deviation from `stats::mad()`, which
#'     multiplies by the consistency constant 1.4826. Scores are on the
#'     scale of a standard-deviation estimate, not the raw median absolute
#'     deviation; choose thresholds on that scale.
#'   \item `"iqr"`: interquartile range.
#'   \item `"range"`: maximum minus minimum.
#'   \item `"missing_prop"`: proportion of missing values, in `[0, 1]`. With
#'     the default `threshold = 0`, `direction = "above"`, and
#'     `action = "keep"`, this keeps the columns that have any missing values
#'     and drops the complete ones, which is almost never what you want. A
#'     warning then suggests `action = "remove"` or `direction = "below"`. The
#'     warning fires only when all three of `threshold`, `direction`, and
#'     `action` are left at their defaults; passing any one of them
#'     explicitly silences it.
#'   \item `"n_unique"`: number of distinct non-`NA` values.
#' }
#'
#' A feature whose score is `NA` is never selected, whether `action` is
#' `"keep"` or `"remove"`, and a warning reports how many features were
#' excluded this way.
#'
#' Columns are subset by position, never by name, so duplicated column names
#' cannot pull in the wrong column. Every column needs a non-empty,
#' non-`NA` name. A matrix keeps its own column names; `V1`, `V2`, ... are
#' used only when it has none.
#'
#' @param data A data.frame, data.table, or matrix whose columns are all
#'   numeric. Every column is a candidate feature. The input is copied, never
#'   modified in place.
#' @param method One of `"variance"` (default), `"mad"`, `"iqr"`, `"range"`,
#'   `"missing_prop"`, or `"n_unique"`. See Details.
#' @param threshold Non-negative, finite number compared with each feature's
#'   score. Default 0. Each method has its own scale (a variance is in squared
#'   units, `"missing_prop"` lies in `[0, 1]`, `"n_unique"` is a count), so a
#'   threshold chosen for one method does not carry over to another.
#' @param direction `"above"` (default) or `"below"`: which side of
#'   `threshold` a score must fall on to meet the condition.
#' @param action `"keep"` (default) retains the features that meet the
#'   condition; `"remove"` drops them.
#' @param include_equal Logical. If `TRUE`, a score equal to `threshold`
#'   meets the condition. Default `FALSE`.
#' @param na_rm Logical. If `TRUE` (the default), `NA`s are removed before
#'   computing `"variance"`, `"mad"`, `"iqr"`, and `"range"`; if `FALSE`, an
#'   `NA` in a column makes its score `NA` under those methods.
#'   `"missing_prop"` and `"n_unique"` ignore `na_rm`.
#' @param output Shape of the return value.
#'   \itemize{
#'     \item `"result"` (default): an `fs_result` object (see Value).
#'     \item `"matrix"`, `"dt"`, `"data.frame"`: the selected features as a
#'       numeric matrix, a data.table, or a data.frame.
#'     \item `"mask"`: logical vector of length `ncol(data)`, named after the
#'       columns of `data`; `TRUE` marks a selected column.
#'     \item `"indices"`: integer positions of the selected columns, named
#'       after them.
#'     \item `"names"`: character vector of the selected column names.
#'     \item `"list"`: a list with `filtered` (matrix), `mask`, `indices`,
#'       `names`, `scores`, and `meta`. `meta` records `method`, `threshold`,
#'       `direction`, `action`, `include_equal`, `na_rm`, `n_input_cols`, and
#'       `n_kept_cols`.
#'   }
#' @param verbose Logical; print progress messages. Default `FALSE`.
#'
#' @return With `output = "result"` (the default), an `fs_result` object:
#'   \itemize{
#'     \item `selected`: character vector of selected feature names.
#'     \item `scores`: named numeric vector with one score per column, in
#'       column order, `NA` where the score is undefined.
#'     \item `method`: `"unsupervised_"` followed by the method, for example
#'       `"unsupervised_variance"`.
#'     \item `task`: `NA_character_`, since there is no target.
#'     \item `model`: `NULL`.
#'     \item `details`: a list holding, in this order, `mask` (the logical
#'       keep-mask over the columns), `indices` (selected positions, named),
#'       `filtered` (the selected columns as a data.table), `threshold`,
#'       `direction`, `action`, and `n_features` (`ncol(data)`).
#'     \item `call`: the matched call.
#'   }
#'   Any other `output` returns the shape described under `output`.
#'
#'   When no feature meets the criteria, a warning is issued and the tabular
#'   shapes come back empty. `"matrix"` and `"data.frame"` then have zero
#'   columns and keep the input row count. The `"dt"` shape and
#'   `details$filtered` have zero columns and also zero rows, because
#'   data.table represents a zero-column table that way.
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
  assert_nonempty_names(names(dt), "fs_unsupervised")
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
