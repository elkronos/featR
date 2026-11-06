# Supervised, univariate, filter-based feature selection.
# Selection-mask and output-shaping machinery shared with fs_unsupervised()
# lives in R/utils-filter.R.

#' Is a numeric vector (numerically) constant?
#'
#' Scale-invariant: the spread is measured on `x / max(abs(x))`, so a column
#' measured on a tiny or huge scale (values around 1e-170 or 1e170, where
#' `stats::sd(x)` itself underflows to 0 or overflows to `Inf`) is not
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
  m <- max(abs(x))
  if (!is.finite(m) || m == 0) {
    return(TRUE)
  }
  s <- stats::sd(x / m)
  !is.finite(s) || s <= 64 * .Machine$double.eps
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
      # Pearson r is scale-invariant; dividing by the magnitudes keeps the
      # sums of squares inside double range for extreme units.
      val <- stats::cor(col2 / max(abs(col2)), y2 / max(abs(y2)))
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
      # F is scale-invariant; work on col2 / max|col2| so the sums of squares
      # cannot over- or underflow for features in extreme units.
      col2 <- col2 / max(abs(col2))
      grand <- mean(col2)
      grp_mean <- tapply(col2, y2, mean)
      grp_n <- tabulate(y2, nbins = k)
      ssb <- sum(grp_n * (grp_mean - grand)^2)
      ssw <- sum((col2 - grp_mean[as.integer(y2)])^2)
      # Within-group spread at or below the rounding floor of the residuals
      # themselves counts as none at all.
      if (ssw <= n * (64 * .Machine$double.eps)^2) {
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

#' Supervised filter that scores each feature against a target
#'
#' Scores every column of `data` except `target` against the target, one
#' feature at a time, and keeps or drops each feature by comparing its score
#' with `threshold`. A numeric target is scored by absolute Pearson
#' correlation, a categorical target by the one-way ANOVA F statistic.
#'
#' @details
#' No model is fitted, so the cost grows linearly with the number of columns
#' and the function copes with wide data. Each feature is scored on its own,
#' which has two consequences: the filter cannot see interactions, and it
#' keeps every member of a group of near-duplicate columns that all track the
#' target. Prune that redundancy afterwards with [fs_correlation()], or use a
#' wrapper such as [fs_recursivefeature()] or [fs_svm()] when the joint
#' contribution of features is what you care about.
#'
#' The three values of `method`:
#' \itemize{
#'   \item `"correlation"` requires a numeric target. The score is the
#'     absolute Pearson correlation, in `[0, 1]`. A constant feature or a
#'     constant target has no defined correlation and scores `NA`. The
#'     constancy check is relative to each column's magnitude, so a feature
#'     measured on a very small or very large scale is scored normally.
#'   \item `"anova"` requires a factor, character, or logical target. The
#'     score is the one-way ANOVA F statistic, in `[0, Inf]`. A feature that
#'     separates the classes perfectly (no spread within any class) scores
#'     `Inf`, and a constant feature scores `NA`.
#'   \item `"auto"` (the default) uses `"correlation"` for a numeric target
#'     and `"anova"` otherwise. The two scales differ, so a threshold that
#'     suits one is meaningless for the other; with `verbose = TRUE` a message
#'     reports which method was chosen.
#' }
#'
#' A feature whose score is `NA` is never selected, whether `action` is
#' `"keep"` or `"remove"`, and a warning reports how many features were
#' excluded this way.
#'
#' Columns are subset by position, never by name, so duplicated feature names
#' cannot pull in the wrong column. The target's name must be unique, and
#' every column needs a non-empty, non-`NA` name. A matrix keeps its own
#' column names; `V1`, `V2`, ... are used only when it has none.
#'
#' @param data A data.frame, data.table, or matrix holding `target` and the
#'   candidate features. Every column other than `target` must be numeric.
#'   The input is copied, never modified in place.
#' @param target Single string naming the target column of `data`. The target
#'   may be numeric, factor, character, or logical; any other type is an
#'   error. It is removed from the candidates and used only for scoring.
#' @param method One of `"auto"` (default), `"correlation"`, or `"anova"`.
#'   See Details.
#' @param threshold Non-negative, finite number compared with each feature's
#'   score. Default 0. The scale depends on the method: `[0, 1]` for
#'   `"correlation"`, `[0, Inf]` for `"anova"`. With the defaults
#'   (`threshold = 0`, `direction = "above"`, `action = "keep"`) nothing is
#'   filtered: any non-constant feature, pure noise included, has a sample
#'   \eqn{|r| > 0} or \eqn{F > 0}, so every feature with a defined score is
#'   kept. Set a threshold to make a real selection.
#' @param direction `"above"` (default) or `"below"`: which side of
#'   `threshold` a score must fall on to meet the condition.
#' @param action `"keep"` (default) retains the features that meet the
#'   condition; `"remove"` drops them.
#' @param include_equal Logical. If `TRUE`, a score equal to `threshold`
#'   meets the condition. Default `FALSE`.
#' @param na_rm Logical. If `TRUE` (the default), rows with `NA` in a feature
#'   or in the target are left out when that feature is scored. If `FALSE`,
#'   an `NA` anywhere in a feature or in the target makes that feature's score
#'   `NA`.
#' @param output Shape of the return value. "Candidate features" means the
#'   columns of `data` other than `target`, in their original order.
#'   \itemize{
#'     \item `"result"` (default): an `fs_result` object (see Value).
#'     \item `"matrix"`, `"dt"`, `"data.frame"`: the selected features as a
#'       numeric matrix, a data.table, or a data.frame.
#'     \item `"mask"`: logical vector with one element per candidate feature,
#'       named after the candidates; `TRUE` marks a selected feature.
#'     \item `"indices"`: integer positions of the selected features among
#'       the candidates (that is, in `data` without `target`), named after
#'       the selected features.
#'     \item `"names"`: character vector of the selected feature names.
#'     \item `"list"`: a list with `filtered` (matrix), `mask`, `indices`,
#'       `names`, `scores`, and `meta`. `meta` records `method_arg` (the
#'       method as requested), `method_used` (after `"auto"` is resolved),
#'       `threshold`, `direction`, `action`, `include_equal`, `na_rm`,
#'       `n_input_cols`, and `n_kept_cols`.
#'   }
#' @param verbose Logical; print progress messages. Default `FALSE`.
#'
#' @return With `output = "result"` (the default), an `fs_result` object:
#'   \itemize{
#'     \item `selected`: character vector of selected feature names.
#'     \item `scores`: named numeric vector with one score per candidate
#'       feature, in column order, `NA` where the score is undefined.
#'     \item `method`: `"supervised_"` followed by the method actually used,
#'       for example `"supervised_correlation"`.
#'     \item `task`: `"regression"` for a numeric target,
#'       `"classification"` for a factor target, and `NA` for a character or
#'       logical target, whose intended task is ambiguous.
#'     \item `model`: `NULL`.
#'     \item `details`: a list holding, in this order, `mask` (the logical
#'       keep-mask over the candidates), `indices` (selected positions, named),
#'       `filtered` (the selected columns as a data.table), `threshold`,
#'       `direction`, `action`, and `n_features` (the number of candidates,
#'       `ncol(data) - 1`).
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
#' @seealso The [Filter methods](https://elkronos.github.io/featR/articles/filters.html)
#'   guide, which runs this function on a worked example.
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
  assert_nonempty_names(names(dt), "fs_supervised")
  assert_target(dt, target)
  # Only the first `target` column is consumed below; a second copy would
  # otherwise become a candidate feature and leak the target into the
  # selection under the target's own name.
  if (sum(names(dt) == target) > 1L) {
    stop(sprintf("Column name '%s' (the target) appears more than once in 'data'; rename the duplicates.",
                 target), call. = FALSE)
  }

  # Index-based, so a data source with duplicated feature names keeps every
  # candidate feature.
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
