# The common return value shared by every featR selection function.

#' Construct an fs_result object
#'
#' Internal constructor. Every `fs_*()` selection function returns one of
#' these so that downstream code can rely on a single shape regardless of
#' which method produced it. The result is a plain list of exactly seven
#' elements -- `selected`, `scores`, `method`, `task`, `model`, `details`,
#' `call`, in that order -- carrying the class `fs_result`. `fs_pca()` and
#' `fs_svd()` are dimensionality reduction rather than selection and return
#' their own lists instead.
#'
#' Only `selected` and `method` are required; everything else has a default,
#' and the checks here are deliberately shallow (type, not content) because
#' each caller owns the meaning of its own `details`.
#'
#' @param selected Character vector of selected feature names (possibly
#'   empty). `NULL` is accepted and stored as `character(0)`.
#' @param scores Optional per-feature scores: a named numeric vector, or a
#'   data.frame with a character/factor column naming the features and a
#'   numeric column holding the score (the first column of each kind is used
#'   when the print and summary methods need a plain vector). `NULL` when the
#'   method does not produce comparable per-feature scores.
#' @param method Single string naming the method (for example "lasso").
#' @param task Single string, "classification", "regression", or `NA`. `NULL`
#'   is stored as `NA_character_`.
#' @param model Optional fitted model object.
#' @param details Named list of method-specific extras.
#' @param call The matched call of the user-facing function.
#' @return An object of class `fs_result`.
#' @noRd
new_fs_result <- function(selected,
                          scores = NULL,
                          method,
                          task = NA_character_,
                          model = NULL,
                          details = list(),
                          call = NULL) {
  if (is.null(selected)) {
    selected <- character(0)
  }
  if (!is.character(selected)) {
    stop("'selected' must be a character vector.", call. = FALSE)
  }
  assert_string(method, "method")
  if (!is.list(details)) {
    stop("'details' must be a list.", call. = FALSE)
  }
  if (!is.null(scores) && !is.numeric(scores) && !is.data.frame(scores)) {
    stop("'scores' must be a named numeric vector, a data.frame, or NULL.",
         call. = FALSE)
  }
  # print() and summary() key the score table by name, so an unnamed numeric
  # would produce a malformed table rather than a clear error.
  if (is.numeric(scores) && length(scores) > 0L && is.null(names(scores))) {
    stop("'scores' must be named when supplied as a numeric vector.",
         call. = FALSE)
  }
  if (!is.null(task) &&
      (length(task) != 1L || !(is.character(task) || is.na(task)))) {
    stop("'task' must be a single string or NA.", call. = FALSE)
  }

  structure(
    list(
      selected = selected,
      scores   = scores,
      method   = method,
      task     = if (is.null(task)) NA_character_ else task,
      model    = model,
      details  = details,
      call     = call
    ),
    class = "fs_result"
  )
}

#' Number of features considered by a result, when knowable
#'
#' Prefers `details$n_features`, which the method records explicitly, and
#' falls back to the number of scored features. The order matters: some
#' methods score only a subset of the candidates (for example
#' `caret::varImp()` on an `rfe` object reports just the optimal-size
#' variables), so counting scores would understate how many features were
#' actually considered and make `print()` say "Selected 2 of 2".
#'
#' @param x An `fs_result` object.
#' @return A single integer, or `NA_integer_` when neither source is recorded.
#' @noRd
fs_result_n_considered <- function(x) {
  n <- x$details$n_features
  if (!is.null(n) && length(n) == 1L && is.finite(n)) {
    return(as.integer(n))
  }
  if (is.data.frame(x$scores)) {
    return(nrow(x$scores))
  }
  if (!is.null(x$scores)) {
    return(length(x$scores))
  }
  NA_integer_
}

#' Do smaller scores mean stronger features for this method?
#'
#' Most featR methods report an importance or an association strength, where
#' larger is better. `fs_chi()` reports adjusted p-values, where smaller is
#' better, so the ranked display has to invert for it. The threshold filters
#' (`fs_unsupervised()`, `fs_supervised()`) record which side of the threshold
#' they kept in `details$direction` and `details$action`; when the kept side is
#' the low one (`direction = "below"` with `action = "keep"`, or `"above"` with
#' `"remove"`, e.g. keeping features with little missingness) smaller is
#' better too, otherwise the kept features would be ranked last and truncated
#' out of the table.
#'
#' @param x An `fs_result` object.
#' @return `TRUE` when smaller scores are better.
#' @noRd
fs_result_lower_is_better <- function(x) {
  if (identical(x$method, "chi")) {
    return(TRUE)
  }
  # fs_correlation(prune = TRUE) keeps the LEAST redundant variables (a
  # smaller max |r|), so smaller is the kept side there too.
  if (fs_result_correlation_pruned(x)) {
    return(TRUE)
  }
  direction <- x$details$direction
  action <- x$details$action
  if (is.character(direction) && length(direction) == 1L &&
      is.character(action) && length(action) == 1L) {
    return((direction == "below" && action == "keep") ||
             (direction == "above" && action == "remove"))
  }
  FALSE
}

#' Is this an fs_correlation() result with prune = TRUE?
#'
#' `fs_correlation()` records `redundant` (every variable in a flagged pair).
#' With `prune = FALSE` that is exactly `selected`; with `prune = TRUE` the
#' selection is every variable not in `dropped`, which never equals the
#' redundant set (a flagged pair always drops one member). An explicit
#' `details$prune` flag, if present, wins.
#'
#' @param x An `fs_result` object.
#' @return `TRUE` for a pruned correlation result.
#' @noRd
fs_result_correlation_pruned <- function(x) {
  if (!is.character(x$method) || length(x$method) != 1L ||
      !startsWith(x$method, "correlation_")) {
    return(FALSE)
  }
  if (is.logical(x$details$prune) && length(x$details$prune) == 1L) {
    return(isTRUE(x$details$prune))
  }
  red <- x$details$redundant
  is.character(red) && !setequal(as.character(x$selected), red)
}

#' Should the summary table list selected features ahead of the rest?
#'
#' For most methods the selection is a cut on the score, so ranking by score
#' already puts the kept features first. Two are not:
#' `fs_mars()` selects every predictor used by the final earth model, which
#' can carry a zero or negative GCV importance while pruned predictors score
#' 0; and `fs_correlation(prune = TRUE)` keeps one member of each redundant
#' pair, whose score equals that of the dropped partner. Ranking by score
#' alone would then list a selected feature below unselected ones, or
#' truncate it out of the table.
#'
#' @param x An `fs_result` object.
#' @return `TRUE` when selected features should be ranked first.
#' @noRd
fs_result_selected_first <- function(x) {
  identical(x$method, "mars") || fs_result_correlation_pruned(x)
}

#' Which scored features were selected, aligned with the score vector?
#'
#' Matches by position when the result carries a logical keep-mask aligned
#' with the scores (the threshold filters do, as `details$mask`), so that
#' duplicated feature names -- which `fs_supervised()` and
#' `fs_unsupervised()` keep and resolve by index -- are marked correctly.
#' Otherwise falls back to matching by name.
#'
#' @param x An `fs_result` object.
#' @param sv The (unordered) score vector from `fs_result_score_vector()`.
#' @return Logical vector, one element per element of `sv`.
#' @noRd
fs_result_selected_mask <- function(x, sv) {
  mask <- x$details$mask
  if (is.logical(mask) && length(mask) == length(sv) &&
      (is.null(names(mask)) || identical(unname(names(mask)), unname(names(sv))))) {
    return(unname(mask) %in% TRUE)
  }
  names(sv) %in% x$selected
}

#' Are this method's scores signed, with magnitude carrying the strength?
#'
#' Only `fs_lasso()` reports signed scores (standardized coefficients, whose
#' sign is the direction of the effect), so only there is the ranking by
#' absolute value. Elsewhere a negative score ranks below a positive one:
#' permutation importance and Boruta importance go negative when a feature
#' does worse than its permuted copy, and ranking those by `abs()` would list
#' a worse-than-noise feature above a useful one. (`fs_mars()` can select a
#' predictor with a negative GCV importance; `summary()` handles that by
#' listing selected predictors first, see `fs_result_selected_first()`.)
#'
#' @param x An `fs_result` object.
#' @return `TRUE` when scores should be ranked by magnitude.
#' @noRd
fs_result_signed_scores <- function(x) {
  identical(x$method, "lasso")
}

#' Scores as a named numeric vector, when possible
#' @noRd
fs_result_score_vector <- function(x) {
  s <- x$scores
  if (is.null(s)) {
    return(NULL)
  }
  if (is.numeric(s)) {
    return(s)
  }
  # data.frame: first character/factor column names the features, first
  # numeric column holds the score
  nm_col <- which(vapply(s, function(col) is.character(col) || is.factor(col),
                         logical(1L)))
  num_col <- which(vapply(s, is.numeric, logical(1L)))
  if (length(nm_col) == 0L || length(num_col) == 0L) {
    return(NULL)
  }
  stats::setNames(s[[num_col[1L]]], as.character(s[[nm_col[1L]]]))
}

#' Extract the selected features from a featR result
#'
#' Returns the names of the features a selection function kept. It gives the
#' same value as `x$selected` and works on the result of any `fs_*()`
#' selection function. Code that calls `selected()` keeps working if the
#' internal layout of the result changes.
#'
#' @param x An `fs_result` object.
#' @param ... Unused, for future methods.
#' @return A character vector of selected feature names. When nothing met the
#'   selection criteria it is `character(0)`, never `NULL`.
#' @examples
#' res <- fs_unsupervised(
#'   data.frame(a = c(1, 5, 2, 8), b = c(1, 1, 1, 1)),
#'   method = "variance", threshold = 0.5
#' )
#' selected(res)
#'
#' # Empty selections are returned as character(0), not NULL
#' none <- suppressWarnings(
#'   fs_unsupervised(
#'     data.frame(a = c(1, 5, 2, 8), b = c(1, 1, 1, 1)),
#'     method = "variance", threshold = 1e6
#'   )
#' )
#' selected(none)
#' @export
selected <- function(x, ...) {
  UseMethod("selected")
}

#' @rdname selected
#' @export
selected.fs_result <- function(x, ...) {
  x$selected
}

#' Print a featR result
#'
#' Prints the method, how many features were kept, and their names.
#'
#' @details
#' The output has at most five lines:
#' 1. A `<fs_result>` header naming the method, followed by the task in
#'    parentheses when one is known.
#' 2. "Selected k of N features", or "Selected k features" when the number
#'    of candidates cannot be recovered from the result.
#' 3. The selected names, truncated to the first `n` with a "... (m more)"
#'    tail. This line is left out when the selection is empty.
#' 4. A "Model:" line giving the class of `x$model`, when the method fitted
#'    one.
#' 5. A "Details:" line naming the elements of `x$details`.
#'
#' @param x An `fs_result` object.
#' @param n Maximum number of selected names to list. Default 10.
#' @param ... Unused, for consistency with the generic.
#' @return `x`, invisibly. Called for the printed output.
#' @examples
#' fs_unsupervised(
#'   data.frame(a = c(1, 5, 2, 8), b = c(1, 1, 1, 1)),
#'   method = "variance", threshold = 0.5
#' )
#' @export
print.fs_result <- function(x, n = 10L, ...) {
  task_str <- if (is.na(x$task)) "" else paste0(" (", x$task, ")")
  cat("<fs_result> ", x$method, task_str, "\n", sep = "")

  n_sel <- length(x$selected)
  n_all <- fs_result_n_considered(x)
  header <- if (is.na(n_all)) {
    sprintf("Selected %d feature%s", n_sel, if (n_sel == 1L) "" else "s")
  } else {
    sprintf("Selected %d of %d feature%s", n_sel, n_all,
            if (n_all == 1L) "" else "s")
  }
  cat(header, "\n", sep = "")

  if (n_sel > 0L) {
    shown <- utils::head(x$selected, n)
    cat("  ", paste(shown, collapse = ", "), sep = "")
    if (n_sel > length(shown)) {
      cat(", ... (", n_sel - length(shown), " more)", sep = "")
    }
    cat("\n")
  }

  if (!is.null(x$model)) {
    cat("Model: ", class(x$model)[1L], " (in $model)\n", sep = "")
  }
  if (length(x$details) > 0L) {
    cat("Details: ", paste(names(x$details), collapse = ", "),
        " (in $details)\n", sep = "")
  }
  invisible(x)
}

#' Summarize a featR result
#'
#' Prints the result, the call that produced it, and a table of scores ranked
#' strongest first.
#'
#' @details
#' The output starts with everything `print()` shows, then the recorded call.
#' For methods that produce per-feature scores it ends with a table of every
#' scored feature, with columns `feature`, `score` (rounded to four
#' significant digits), and `selected` (`*` marks the kept features). The
#' table is truncated to `n` rows with a "... (m more)" tail.
#'
#' @section How rows are ranked:
#' For most methods a larger score means a stronger feature, so rows are in
#' decreasing order of score. A negative importance (a feature that did
#' worse than its permuted copy) therefore ranks below every positive one.
#' The other rules are:
#' \itemize{
#'   \item [fs_lasso()] scores are signed standardized coefficients. The sign
#'     gives the direction of the effect, not its strength, so rows are in
#'     decreasing order of absolute value.
#'   \item [fs_chi()] scores are adjusted p-values, where smaller is
#'     stronger, so rows are in increasing order and the most significant
#'     feature comes first.
#'   \item [fs_unsupervised()] and [fs_supervised()] also use increasing
#'     order when they kept the low side of the threshold
#'     (`direction = "below"` with `action = "keep"`, or `"above"` with
#'     `"remove"`), so the kept features come first.
#' }
#'
#' Two methods select by something other than a cut on the score. For them
#' the selected features are listed first, each group ordered by score, so
#' a kept feature is never ranked below, or truncated out behind, a dropped
#' one:
#' \itemize{
#'   \item [fs_mars()] selects every predictor the final earth model uses.
#'     A used predictor can have a zero or negative GCV importance while
#'     pruned predictors score 0.
#'   \item [fs_correlation()] with `prune = TRUE` scores each variable by its
#'     largest absolute correlation with any other (larger means more
#'     redundant) and keeps one member of each redundant pair. Rows are in
#'     increasing order within each group, least redundant first. With
#'     `prune = FALSE` the selection is the redundant set itself, and rows
#'     are in decreasing order of score.
#' }
#'
#' When the result records a per-feature keep-mask (`details$mask`, as the
#' threshold filters do), the `selected` marks are matched by position, so
#' two features sharing a name are marked separately. Otherwise they are
#' matched by name.
#'
#' @param object An `fs_result` object.
#' @param n Maximum number of rows in the score table. Default 20.
#' @param ... Unused, for consistency with the generic.
#' @return `object`, invisibly. Called for the printed output.
#' @examples
#' res <- fs_unsupervised(
#'   data.frame(a = c(1, 5, 2, 8), b = c(1, 1, 1, 1)),
#'   method = "variance", threshold = 0.5
#' )
#' summary(res)
#' @export
summary.fs_result <- function(object, n = 20L, ...) {
  print(object)

  if (!is.null(object$call)) {
    cat("\nCall:\n  ")
    print(object$call)
  }

  sv <- fs_result_score_vector(object)
  if (!is.null(sv) && length(sv) > 0L) {
    sel <- fs_result_selected_mask(object, sv)
    # Ascending sort key for the score: smallest key = strongest.
    key <- if (fs_result_lower_is_better(object)) {
      sv
    } else if (fs_result_signed_scores(object)) {
      -abs(sv)
    } else {
      -sv
    }
    ord <- if (fs_result_selected_first(object)) {
      order(!sel, key, na.last = TRUE)
    } else {
      order(key, na.last = TRUE)
    }
    sv <- sv[ord]
    sel <- sel[ord]
    shown <- utils::head(sv, n)
    cat("\nScores (", length(sv), " feature",
        if (length(sv) == 1L) "" else "s", ", ranked):\n", sep = "")
    marks <- ifelse(utils::head(sel, n), "*", " ")
    out <- data.frame(
      feature = names(shown),
      score = unname(signif(shown, 4)),
      selected = marks,
      stringsAsFactors = FALSE
    )
    print(out, row.names = FALSE)
    if (length(sv) > length(shown)) {
      cat("  ... (", length(sv) - length(shown), " more)\n", sep = "")
    }
    cat("(* = selected)\n")
  }

  invisible(object)
}
