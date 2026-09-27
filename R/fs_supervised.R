# Supervised, univariate, filter-based feature selection.
# Selection-mask and output-shaping machinery shared with fs_unsupervised()
# lives in R/utils-filter.R.

#' Is a numeric vector (numerically) constant?
#'
#' Scale-invariant: the spread is compared to the magnitude of the values, so
#' a column measured on a tiny scale (say, values around 1e-10) is not
#' mistaken for a constant one, while a constant column carrying only
#' floating-point noise still is.
#'
#' @param x NA-free numeric vector.
#' @return A single flag.
#' @noRd
sup_is_degenerate <- function(x) {
  if (length(x) < 2L) {
    return(TRUE)
  }
  s <- stats::sd(x)
  if (!is.finite(s)) {
    return(TRUE)
  }
  s <= 64 * .Machine$double.eps * max(abs(x))
}

#' Absolute Pearson correlation of each feature with a numeric target
#'
#' Both NA policies share one code path: with `na_rm = TRUE` incomplete pairs
#' are dropped per feature, with `na_rm = FALSE` any NA makes the score `NA`.
#' A (numerically) constant feature or target has an undefined correlation and
#' scores `NA`, without the "standard deviation is zero" warning that
#' `stats::cor()` would add on top of the one `filter_mask()` emits.
#'
#' @param dt data.table of numeric feature columns.
#' @param y Numeric target vector.
#' @param na_rm Single flag; drop NA pairs per feature before scoring.
#' @return Numeric vector of |r| scores, NA where undefined.
#' @noRd
sup_score_correlation <- function(dt, y, na_rm) {
  y_num <- as.numeric(y)
  vapply(
    dt,
    function(col) {
      col <- as.numeric(col)
      if (na_rm) {
        idx <- !is.na(col) & !is.na(y_num)
        col2 <- col[idx]
        y2 <- y_num[idx]
      } else {
        if (anyNA(col) || anyNA(y_num)) {
          return(NA_real_)
        }
        col2 <- col
        y2 <- y_num
      }
      if (sup_is_degenerate(col2) || sup_is_degenerate(y2)) {
        return(NA_real_)
      }
      val <- stats::cor(col2, y2)
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
#' @param na_rm Single flag; drop NA pairs per feature before scoring. When
#'   `FALSE`, an NA anywhere in the feature or the target makes that feature's
#'   score `NA`.
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
        # The documented contract is that an NA anywhere in the feature or
        # the target makes the score undefined (the correlation path does the
        # same), so incomplete rows must not be dropped silently here.
        if (anyNA(col) || anyNA(y_fac)) {
          return(NA_real_)
        }
        col2 <- col
        y2 <- y_fac
      }

      # Only the groups actually observed count towards the degrees of
      # freedom (unused factor levels and levels emptied by NA removal).
      y2 <- droplevels(y2)
      n <- length(col2)
      k <- nlevels(y2)
      if (n <= k || k < 2L || any(!is.finite(col2)) ||
          sup_is_degenerate(col2)) {
        return(NA_real_)
      }

      # Closed-form one-way ANOVA F = (SSB / (k - 1)) / (SSW / (n - k)),
      # identical to stats::anova(lm(x ~ g)) but without its "essentially
      # perfect fit" warning, and exact at the boundaries: a feature that
      # separates the groups perfectly (no within-group spread) scores Inf --
      # the strongest possible score -- instead of a ~1e30 value whose size
      # is rounding noise. A constant feature (no spread at all) is NA.
      grand <- mean(col2)
      grp_mean <- tapply(col2, y2, mean)
      grp_n <- tabulate(y2, nbins = k)
      ssb <- sum(grp_n * (grp_mean - grand)^2)
      ssw <- sum((col2 - grp_mean[as.integer(y2)])^2)
      # Within-group spread at or below the rounding floor of the residuals
      # themselves counts as none at all.
      if (ssw <= n * (64 * .Machine$double.eps * max(abs(col2)))^2) {
        return(Inf)
      }
      (ssb / (k - 1L)) / (ssw / (n - k))
    },
    numeric(1L)
  )
}

#' Learning task implied by the target column
#'
#' A numeric target means regression, a factor target means classification.
#' Anything else (character, logical) is left undeclared: `fs_supervised()`
#' can score such targets with ANOVA, but the intended task is genuinely
#' ambiguous, so `NA` is reported rather than guessed.
#'
#' @param y Target vector.
#' @return A single string, or `NA_character_`.
#' @noRd
sup_task <- function(y) {
  if (is.numeric(y)) {
    return("regression")
  }
  if (is.factor(y)) {
    return("classification")
  }
  NA_character_
}

#' Compute supervised per-feature scores
#'
#' Validates the target, resolves `method = "auto"` (numeric target ->
#' "correlation", categorical target -> "anova"), and scores every column of
#' `dt` against `y`. Returning the resolved method alongside the scores keeps
#' a single source of truth for which method was actually used.
#'
#' @param dt data.table of numeric feature columns.
#' @param y Target vector (numeric, factor, character, or logical), already
#'   extracted from the `target` column of `data`.
#' @param method One of "auto", "correlation", "anova".
#' @param na_rm Single flag; drop NA pairs per feature before scoring.
#' @param verbose Single flag; emit progress messages.
#' @return List with `scores` (named numeric vector, one element per column
#'   of `dt`) and `method` (the resolved scoring method actually used).
#' @noRd
sup_scores <- function(dt,
                       y,
                       method = c("auto", "correlation", "anova"),
                       na_rm = TRUE,
                       verbose = FALSE) {
  method <- match.arg(method)

  if (!is.null(dim(y))) {
    stop("The 'target' column must be an atomic vector, not a matrix or data frame.",
         call. = FALSE)
  }

  is_y_numeric <- is.numeric(y)
  is_y_categorical <- is.factor(y) || is.character(y) || is.logical(y)
  if (!is_y_numeric && !is_y_categorical) {
    stop("The 'target' column must be numeric, factor, character, or logical.",
         call. = FALSE)
  }

  if (method == "auto") {
    method <- if (is_y_numeric) "correlation" else "anova"
    if (verbose) {
      message(sprintf(
        paste0(
          "method = 'auto' resolved to '%s'. Note that the threshold scale ",
          "differs by method: |r| lies in [0, 1] for 'correlation', while ",
          "the ANOVA F statistic lies in [0, Inf]."
        ),
        method
      ))
    }
  }

  if (method == "correlation" && !is_y_numeric) {
    stop("`method = \"correlation\"` requires a numeric target.", call. = FALSE)
  }
  if (method == "anova" && !is_y_categorical) {
    stop(
      "`method = \"anova\"` requires a categorical target (factor, character, or logical).",
      call. = FALSE
    )
  }

  if (verbose) {
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
#' Performs supervised, univariate, filter-based feature selection: every
#' column of `data` except `target` is scored against the target, and
#' features are selected or dropped by comparing that score to `threshold`.
#'
#' This is one of the cheapest supervised screens in featR: no predictive
#' model is fitted, each feature is scored on its own, and the cost is linear
#' in the number of columns, so it scales to wide data. The flip side is that
#' scoring is strictly univariate: it cannot see interactions between
#' features, and it will happily keep a whole group of near-duplicate columns
#' that all correlate with the target. Use [fs_correlation()] to prune that
#' redundancy afterwards, or a wrapper such as [fs_recursivefeature()] or
#' [fs_svm()] when the joint contribution is what matters.
#'
#' Supported methods:
#' \itemize{
#'   \item \code{"correlation"}: Absolute Pearson correlation (numeric
#'     target), so scores lie in \code{[0, 1]}. A constant feature (or
#'     target) has no defined correlation and scores \code{NA}; the check is
#'     relative to the column's magnitude, so features measured on a very
#'     small scale are scored normally.
#'   \item \code{"anova"}: One-way ANOVA F-statistic (categorical target),
#'     so scores lie in \code{[0, Inf]}: a feature that separates the
#'     classes perfectly (no spread within any class) scores \code{Inf}, and
#'     a constant feature scores \code{NA}.
#'   \item \code{"auto"}: Chooses \code{"correlation"} for a numeric target
#'     and \code{"anova"} for a categorical one. Because the two score scales
#'     differ, a message reports the resolved method when
#'     \code{verbose = TRUE}.
#' }
#'
#' Features whose score is undefined (\code{NA}) are never selected, under
#' both \code{action = "keep"} and \code{action = "remove"}; a warning
#' reports how many such features were excluded.
#'
#' Columns are subset by integer index, never by name, so duplicated column
#' names cannot select the wrong columns.
#'
#' @param data A data.frame, data.table, or matrix containing `target` and
#'   the candidate feature columns. Every column other than `target` must be
#'   numeric. The input is copied, never modified in place.
#' @param target Character. Name of the target column in `data`. It is
#'   removed from the candidate features and used as the scoring target. The
#'   target column itself may be numeric, factor, character, or logical; any
#'   other type is an error.
#' @param method One of \code{"auto"} (default), \code{"correlation"},
#'   \code{"anova"}.
#' @param threshold Non-negative, finite numeric scalar threshold applied to
#'   the feature scores (not to the target directly). Default 0. Note that the
#'   two methods put scores on different scales: \code{[0, 1]} for
#'   \code{"correlation"} and \code{[0, Inf]} for \code{"anova"}.
#' @param direction One of \code{"above"} (default), \code{"below"}; compares
#'   scores to \code{threshold}.
#' @param action One of \code{"keep"} (default), \code{"remove"}; determines
#'   whether features meeting the condition are retained or dropped.
#' @param include_equal Logical; if TRUE, comparisons are inclusive
#'   (greater/less than or equal) instead of strict. Default FALSE.
#' @param na_rm Logical; if TRUE (the default), rows with NA in a feature or
#'   in the target are dropped when computing that feature's score. If FALSE,
#'   an NA anywhere in a feature or the target makes that feature's score
#'   undefined.
#' @param output One of \code{"result"} (default), \code{"matrix"},
#'   \code{"dt"}, \code{"data.frame"}, \code{"mask"}, \code{"indices"},
#'   \code{"names"}, \code{"list"}. Throughout, "candidate features" means the
#'   columns of `data` other than `target`, in their original order.
#'   \itemize{
#'     \item \code{"result"} (default): an \code{fs_result} object (see
#'       Value).
#'     \item \code{"matrix"}: numeric matrix of the selected features.
#'     \item \code{"dt"}: data.table of the selected features.
#'     \item \code{"data.frame"}: data.frame of the selected features.
#'     \item \code{"mask"}: logical vector, one element per candidate feature,
#'       named after the candidate features; TRUE marks a selected column.
#'     \item \code{"indices"}: integer vector of the selected column indices,
#'       indexing the candidate features (that is, `data` without `target`),
#'       named after the selected columns.
#'     \item \code{"names"}: character vector of the selected column names.
#'     \item \code{"list"}: list with components \code{filtered} (matrix),
#'       \code{mask}, \code{indices}, \code{names}, \code{scores} (all as
#'       above), and \code{meta}, a list recording \code{method_arg} (the
#'       method as requested), \code{method_used} (the method after
#'       \code{"auto"} was resolved), \code{threshold}, \code{direction},
#'       \code{action}, \code{include_equal}, \code{na_rm},
#'       \code{n_input_cols}, and \code{n_kept_cols}.
#'   }
#' @param verbose Logical; emit progress messages. Default FALSE.
#'
#' @return With the default \code{output = "result"}, an object of class
#'   `fs_result` with elements:
#'   \itemize{
#'     \item `selected`: character vector of selected feature names.
#'     \item `scores`: named numeric vector of per-feature scores, in column
#'       order, `NA` where the score is undefined.
#'     \item `method`: `"supervised_"` followed by the resolved scoring
#'       method, for example `"supervised_correlation"`.
#'     \item `task`: `"regression"` for a numeric target,
#'       `"classification"` for a factor target, `NA` otherwise.
#'     \item `model`: `NULL`.
#'     \item `details`: a list holding, in this order, `mask` (the logical
#'       keep-mask over the candidate features), `indices` (the selected
#'       column indices, named after the selected columns), `filtered` (the
#'       selected columns as a data.table), `threshold`, `direction`,
#'       `action`, and `n_features` (the number of candidate features, that
#'       is `ncol(data) - 1`).
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
#'   strong = c(1, 2, 3, 4),
#'   mirror = c(4, 3, 2, 1),
#'   weak   = c(1, 0, 1, 0),
#'   y      = c(1, 2, 3, 4)
#' )
#'
#' # Default: an fs_result
#' res <- fs_supervised(df, target = "y", method = "correlation",
#'                      threshold = 0.5)
#' res$selected
#' res$scores
#' res$details$filtered
#'
#' # The classic shapes are still available
#' fs_supervised(df, target = "y", method = "correlation", threshold = 0.5,
#'               output = "names")
#'
#' # ANOVA against a factor target
#' df_fac <- data.frame(
#'   wide = c(1, 2, 10, 11),
#'   mild = c(1, 3, 2, 4),
#'   grp  = factor(c("a", "a", "b", "b"))
#' )
#' fs_supervised(df_fac, target = "grp", method = "anova", threshold = 1,
#'               output = "matrix")
#' @export
fs_supervised <- function(data,
                          target,
                          method = c("auto", "correlation", "anova"),
                          threshold = 0,
                          direction = c("above", "below"),
                          action = c("keep", "remove"),
                          include_equal = FALSE,
                          na_rm = TRUE,
                          output = c("result", "matrix", "dt", "data.frame",
                                     "mask", "indices", "names", "list"),
                          verbose = FALSE) {
  cl <- match.call()

  method_arg <- match.arg(method)
  direction <- match.arg(direction)
  action <- match.arg(action)
  output <- match.arg(output)
  assert_number(threshold, "threshold", lower = 0)
  assert_flag(include_equal, "include_equal")
  assert_flag(na_rm, "na_rm")
  assert_flag(verbose, "verbose")
  assert_string(target, "target")

  dt <- as_dt(data, arg = "data")
  assert_target(dt, target)

  # Index-based, so a data source with duplicated column names keeps every
  # candidate feature and only the first `target` column is consumed.
  target_idx <- match(target, names(dt))
  y <- dt[[target_idx]]

  feature_idx <- setdiff(seq_along(dt), target_idx)
  if (length(feature_idx) == 0L) {
    stop("'data' must contain at least one feature column besides 'target'.",
         call. = FALSE)
  }
  features <- dt[, feature_idx, with = FALSE]

  if (!all(vapply(features, is.numeric, logical(1L)))) {
    stop("All feature columns of 'data' must be numeric.", call. = FALSE)
  }

  scored <- sup_scores(
    dt = features,
    y = y,
    method = method_arg,
    na_rm = na_rm,
    verbose = verbose
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

  if (verbose) {
    op <- if (direction == "above") ">" else "<"
    if (include_equal) op <- paste0(op, "=")
    verb <- if (action == "keep") "Retaining" else "Removing"
    message(sprintf(
      "%s features with supervised score %s %s", verb, op, threshold
    ))
    message(sprintf("Kept %d of %d features.", length(keep_idx),
                    ncol(features)))
  }

  if (output == "result") {
    return(new_fs_result(
      selected = filter_output(features, scores, mask, "names"),
      scores = scores,
      method = paste0("supervised_", method_used),
      task = sup_task(y),
      model = NULL,
      details = list(
        mask = mask,
        indices = keep_idx,
        filtered = filter_output(features, scores, mask, "dt"),
        threshold = threshold,
        direction = direction,
        action = action,
        n_features = ncol(features)
      ),
      call = cl
    ))
  }

  res <- filter_output(features, scores, mask, output)
  if (output == "list") {
    res$meta <- list(
      method_arg = method_arg,
      method_used = method_used,
      threshold = threshold,
      direction = direction,
      action = action,
      include_equal = include_equal,
      na_rm = na_rm,
      n_input_cols = ncol(features),
      n_kept_cols = length(keep_idx)
    )
  }
  res
}
