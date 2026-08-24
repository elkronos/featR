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
#' better, so the ranked display has to invert for it.
#'
#' @param x An `fs_result` object.
#' @return `TRUE` when smaller scores are better.
#' @noRd
fs_result_lower_is_better <- function(x) {
  identical(x$method, "chi")
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
#' The generic accessor for the one thing every featR method produces: the
#' names of the features it kept. Equivalent to `x$selected`, but stable
#' against future changes in the internal layout, and safe to call on the
#' result of any `fs_*()` selection function.
#'
#' @param x An `fs_result` object.
#' @param ... Unused, for future methods.
#' @return A character vector of selected feature names, possibly empty when
#'   nothing met the selection criteria.
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
#' Prints a compact summary of what a selection run kept.
#'
#' @details
#' The output is at most five lines: a `<fs_result>` header naming the method,
#' followed by the task in parentheses when one is known; a count, either
#' "Selected k of N features" or, when the number of candidates cannot be
#' recovered, "Selected k features"; the selected names, truncated to the
#' first `n` with a "... (m more)" tail; a "Model:" line giving the class of
#' `x$model` when the method fitted one; and a "Details:" line naming the
#' elements of `x$details`. Nothing is printed for the names when the
#' selection is empty.
#'
#' @param x An `fs_result` object.
#' @param n Maximum number of features to list (default 10).
#' @param ... Unused, for consistency with the generic.
#' @return `x`, invisibly. Called for its printed output.
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
#' Prints the result, the call that produced it, and a ranked score table.
#'
#' @details
#' Everything `print()` shows, then the recorded call, then -- for methods
#' that produce per-feature scores -- a table of every scored feature ranked
#' by decreasing absolute score, with columns `feature`, `score` (four
#' significant digits) and `selected` (`*` marks the features that were kept).
#' The table is truncated to `n` rows with a "... (m more)" tail.
#'
#' The ranking direction follows the method. For most methods a larger score
#' means a stronger feature, and rows are ordered by decreasing absolute score
#' so that signed scores such as regression coefficients sort sensibly. For
#' methods that score by p-value, where smaller is better, rows are ordered
#' ascending instead, so the most significant feature is listed first.
#'
#' @param object An `fs_result` object.
#' @param n Maximum number of scored features to show (default 20).
#' @param ... Unused, for consistency with the generic.
#' @return `object`, invisibly. Called for its printed output.
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
    ord <- if (fs_result_lower_is_better(object)) {
      order(sv, decreasing = FALSE, na.last = TRUE)
    } else {
      order(abs(sv), decreasing = TRUE, na.last = TRUE)
    }
    sv <- sv[ord]
    shown <- utils::head(sv, n)
    cat("\nScores (", length(sv), " feature",
        if (length(sv) == 1L) "" else "s", ", ranked):\n", sep = "")
    marks <- ifelse(names(shown) %in% object$selected, "*", " ")
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
