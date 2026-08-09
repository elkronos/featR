# Supervised, univariate, filter-based feature selection.
# Selection-mask and output-shaping machinery shared with fs_unsupervised()
# lives in R/utils-filter.R.

#' Absolute Pearson correlation of each feature with a numeric target
#'
#' @param dt data.table of numeric feature columns.
#' @param y Numeric target vector.
#' @param na_rm Single flag; drop NA pairs per feature before scoring.
#' @return Numeric vector of |r| scores, NA where undefined.
#' @noRd
sup_score_correlation <- function(dt, y, na_rm) {
  y_num <- as.numeric(y)
  tol <- .Machine$double.eps^0.5
  vapply(
    dt,
    function(col) {
      if (na_rm) {
        idx <- !is.na(col) & !is.na(y_num)
        col2 <- col[idx]
        y2 <- y_num[idx]
        if (length(col2) < 2L) {
          return(NA_real_)
        }
        sd_col <- stats::sd(col2)
        sd_y <- stats::sd(y2)
        if (!is.finite(sd_col) || sd_col < tol ||
            !is.finite(sd_y) || sd_y < tol) {
          return(NA_real_)
        }
        val <- stats::cor(col2, y2)
      } else {
        # Let cor() handle NAs according to 'use', then sanitize the result.
        val <- stats::cor(col, y_num, use = "everything")
      }
      if (is.na(val) || !is.finite(val)) {
        return(NA_real_)
      }
      abs(val)
    },
    numeric(1L)
  )
}

#' One-way ANOVA F statistic of each feature against a categorical target
#'
#' @param dt data.table of numeric feature columns.
#' @param y Categorical target vector (coerced to factor).
#' @param na_rm Single flag; drop NA pairs per feature before scoring.
#' @return Numeric vector of F statistics, NA where undefined.
#' @noRd
sup_score_anova <- function(dt, y, na_rm) {
  y_fac <- as.factor(y)
  vapply(
    dt,
    function(col) {
      if (na_rm) {
        idx <- !is.na(col) & !is.na(y_fac)
        col2 <- col[idx]
        y2 <- y_fac[idx]
      } else {
        col2 <- col
        y2 <- y_fac
      }

      if (length(col2) < 2L || length(unique(y2[!is.na(y2)])) < 2L) {
        return(NA_real_)
      }

      fit <- tryCatch(
        stats::lm(x ~ y, data = data.frame(x = col2, y = y2)),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        return(NA_real_)
      }

      a <- tryCatch(stats::anova(fit), error = function(e) NULL)
      if (is.null(a) || nrow(a) < 1L) {
        return(NA_real_)
      }

      val <- a$`F value`[1L]
      if (!is.numeric(val) || length(val) != 1L || !is.finite(val)) {
        return(NA_real_)
      }
      val
    },
    numeric(1L)
  )
}

#' Compute supervised per-feature scores
#'
#' Validates `y`, resolves `method = "auto"` (numeric `y` -> "correlation",
#' categorical `y` -> "anova"), and scores every column of `dt` against `y`.
#' Returning the resolved method alongside the scores keeps a single source
#' of truth for which method was actually used.
#'
#' @param dt data.table of numeric feature columns.
#' @param y Target vector (numeric, factor, character, or logical).
#' @param method One of "auto", "correlation", "anova".
#' @param na_rm Single flag; drop NA pairs per feature before scoring.
#' @param log_progress Single flag; emit progress messages.
#' @return List with `scores` (named numeric vector, one element per column
#'   of `dt`) and `method` (the resolved scoring method actually used).
#' @noRd
sup_scores <- function(dt,
                       y,
                       method = c("auto", "correlation", "anova"),
                       na_rm = TRUE,
                       log_progress = FALSE) {
  method <- match.arg(method)

  if (is.matrix(y) || is.data.frame(y)) {
    stop("`y` must be a vector, not a matrix or data frame.", call. = FALSE)
  }
  if (length(y) != nrow(dt)) {
    stop("Length of `y` must equal number of rows in `x`.", call. = FALSE)
  }

  is_y_numeric <- is.numeric(y)
  is_y_categorical <- is.factor(y) || is.character(y) || is.logical(y)
  if (!is_y_numeric && !is_y_categorical) {
    stop("`y` must be numeric, factor, character, or logical.", call. = FALSE)
  }

  if (method == "auto") {
    method <- if (is_y_numeric) "correlation" else "anova"
    if (log_progress) {
      message(sprintf(
        paste0(
          "method = 'auto' resolved to '%s'. Note that the threshold scale ",
          "differs by method: |r| lies in [0, 1] for 'correlation', while ",
          "the ANOVA F statistic lies in [0, Inf)."
        ),
        method
      ))
    }
  }

  if (method == "correlation" && !is_y_numeric) {
    stop("`method = \"correlation\"` requires numeric `y`.", call. = FALSE)
  }
  if (method == "anova" && !is_y_categorical) {
    stop(
      "`method = \"anova\"` requires categorical `y` (factor/character/logical).",
      call. = FALSE
    )
  }

  if (log_progress) {
    message(sprintf(
      "Computing supervised feature scores using method '%s'...", method
    ))
  }

  scores <- switch(
    method,
    "correlation" = sup_score_correlation(dt, y, na_rm),
    "anova" = sup_score_anova(dt, y, na_rm)
  )

  scores <- as.numeric(scores)
  names(scores) <- names(dt)

  list(scores = scores, method = method)
}

#' Supervised Filter-Based Feature Selection
#'
#' Performs supervised, univariate, filter-based feature selection by scoring
#' each feature with respect to a target `y` and selecting or dropping
#' features based on a threshold on the score.
#'
#' Supported methods:
#' \itemize{
#'   \item \code{"correlation"}: Absolute Pearson correlation (numeric target),
#'     so scores lie in \code{[0, 1]}.
#'   \item \code{"anova"}: One-way ANOVA F-statistic (categorical target),
#'     so scores lie in \code{[0, Inf)}.
#'   \item \code{"auto"}: Chooses \code{"correlation"} for numeric `y` and
#'     \code{"anova"} for categorical `y`. Because the two score scales
#'     differ, a message reports the resolved method when
#'     \code{log_progress = TRUE}.
#' }
#'
#' Features whose score is undefined (\code{NA}) are never selected, under
#' both \code{action = "keep"} and \code{action = "remove"}; a warning
#' reports how many such features were excluded.
#'
#' @param x Numeric matrix, data.frame, or data.table (all numeric columns).
#' @param y Target vector (numeric, factor, character, or logical) with one
#'   element per row of `x`.
#' @param method One of \code{"auto"}, \code{"correlation"}, \code{"anova"}.
#' @param threshold Non-negative, finite numeric scalar threshold applied to
#'   the feature scores (not to `y` directly). Default 0.
#' @param direction One of \code{"above"}, \code{"below"}; compares scores to
#'   \code{threshold}.
#' @param action One of \code{"keep"}, \code{"remove"}; determines whether
#'   features meeting the condition are retained or dropped.
#' @param include_equal Logical; if TRUE, comparisons use >= / <= instead of
#'   > / <.
#' @param na_rm Logical; if TRUE, rows with NA in `x` or `y` are dropped when
#'   computing scores.
#' @param out One of \code{"matrix"}, \code{"dt"}, \code{"data.frame"},
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
#' @return Depends on the `out` argument (see above). When no feature meets
#'   the selection criteria, a warning is issued and the tabular shapes
#'   (\code{"matrix"}, \code{"dt"}, \code{"data.frame"}) are returned with
#'   zero columns while preserving the input row count.
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(200), ncol = 5)
#' y_num <- rnorm(nrow(X))
#' y_fac <- factor(sample(letters[1:3], nrow(X), replace = TRUE))
#'
#' # Correlation-based selection (numeric y)
#' out_corr <- fs_supervised(
#'   x = X, y = y_num,
#'   method = "correlation",
#'   threshold = 0.3,
#'   direction = "above",
#'   action = "keep",
#'   out = "list"
#' )
#'
#' # ANOVA-based selection (factor y)
#' out_anova <- fs_supervised(
#'   x = X, y = y_fac,
#'   method = "anova",
#'   threshold = 1.0,
#'   direction = "above",
#'   action = "keep",
#'   out = "matrix"
#' )
#' @export
fs_supervised <- function(x,
                          y,
                          method = c("auto", "correlation", "anova"),
                          threshold = 0,
                          direction = c("above", "below"),
                          action = c("keep", "remove"),
                          include_equal = FALSE,
                          na_rm = TRUE,
                          out = c("matrix", "dt", "data.frame",
                                  "mask", "indices", "names", "list"),
                          log_progress = FALSE) {
  method_arg <- match.arg(method)
  direction <- match.arg(direction)
  action <- match.arg(action)
  out <- match.arg(out)
  assert_number(threshold, "threshold", lower = 0)
  assert_flag(include_equal, "include_equal")
  assert_flag(na_rm, "na_rm")
  assert_flag(log_progress, "log_progress")

  dt <- as_dt(x, arg = "x")
  if (!all(vapply(dt, is.numeric, logical(1L)))) {
    stop("All columns of 'x' must be numeric.", call. = FALSE)
  }

  scored <- sup_scores(
    dt = dt,
    y = y,
    method = method_arg,
    na_rm = na_rm,
    log_progress = log_progress
  )
  scores <- scored$scores
  method_used <- scored$method

  mask <- filter_mask(
    scores = scores,
    threshold = threshold,
    direction = direction,
    action = action,
    include_equal = include_equal
  )
  keep_idx <- which(mask)

  if (length(keep_idx) == 0L) {
    warning("No features meet the specified supervised selection criteria.",
            call. = FALSE)
  }

  if (log_progress) {
    op <- if (direction == "above") ">" else "<"
    if (include_equal) op <- paste0(op, "=")
    verb <- if (action == "keep") "Retaining" else "Removing"
    message(sprintf(
      "%s features with supervised score %s %s", verb, op, threshold
    ))
    message(sprintf("Kept %d of %d features.", length(keep_idx), ncol(dt)))
  }

  res <- filter_output(dt, scores, mask, out)
  if (out == "list") {
    res$meta <- list(
      method_arg = method_arg,
      method_used = method_used,
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
