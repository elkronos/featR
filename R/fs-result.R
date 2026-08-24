# The common return value shared by every featR selection function.

#' Construct an fs_result object
#'
#' Internal constructor. Every `fs_*()` selection function returns one of
#' these so that downstream code can rely on a single shape regardless of
#' which method produced it.
#'
#' @param selected Character vector of selected feature names (possibly empty).
#' @param scores Optional named numeric vector, or data.frame whose first
#'   column is the feature name, giving a per-feature score. `NULL` when the
#'   method does not produce comparable per-feature scores.
#' @param method Single string naming the method (for example "lasso").
#' @param task Single string, "classification", "regression", or `NA`.
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
#' @noRd
fs_result_n_considered <- function(x) {
  if (is.data.frame(x$scores)) {
    return(nrow(x$scores))
  }
  if (!is.null(x$scores)) {
    return(length(x$scores))
  }
  n <- x$details$n_features
  if (is.null(n)) NA_integer_ else as.integer(n)
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
#' @param x An `fs_result` object.
#' @param ... Unused, for future methods.
#' @return A character vector of selected feature names.
#' @examples
#' res <- fs_unsupervised(
#'   data.frame(a = c(1, 5, 2, 8), b = c(1, 1, 1, 1)),
#'   method = "variance", threshold = 0.5
#' )
#' selected(res)
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
#' @param x An `fs_result` object.
#' @param n Maximum number of features to list (default 10).
#' @param ... Unused, for consistency with the generic.
#' @return `x`, invisibly.
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
    ord <- order(abs(sv), decreasing = TRUE, na.last = TRUE)
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
