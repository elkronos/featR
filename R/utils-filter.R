# Internal filter machinery shared by fs_supervised() and fs_unsupervised().
# These helpers exist exactly once so the two front ends cannot drift apart
# (they previously carried verbatim copies of the same functions, which was a
# collation-order overwrite hazard).

#' Build a logical keep-mask from per-feature scores
#'
#' Compares `scores` to `threshold` according to `direction` and
#' `include_equal`, then applies `action`: `"keep"` selects the features that
#' meet the condition, `"remove"` selects the features that do not.
#'
#' NA policy: features with undefined (NA) scores are NEVER selected, under
#' both `action = "keep"` and `action = "remove"` -- negating the comparison
#' must not turn an unknown score into a selection. When any score is NA a
#' single warning reports how many features were excluded, listing up to five
#' of their names (or their positions, when `scores` is unnamed).
#'
#' All scalar arguments are validated by the exported callers before this
#' helper runs.
#'
#' @param scores Named numeric vector of per-feature scores.
#' @param threshold Single non-negative finite number.
#' @param direction `"above"` or `"below"`.
#' @param action `"keep"` or `"remove"`.
#' @param include_equal Single flag; if `TRUE`, comparisons use >= / <=.
#' @return Logical vector with the same length and names as `scores`;
#'   `TRUE` marks a column to keep.
#' @noRd
filter_mask <- function(scores, threshold, direction, action,
                        include_equal = FALSE) {
  cmp <- if (direction == "above") {
    if (include_equal) scores >= threshold else scores > threshold
  } else {
    if (include_equal) scores <= threshold else scores < threshold
  }

  mask <- if (action == "keep") cmp else !cmp

  na_idx <- which(is.na(scores))
  if (length(na_idx) > 0L) {
    mask[na_idx] <- FALSE
    na_names <- names(scores)[na_idx]
    if (is.null(na_names)) {
      na_names <- paste0("column ", na_idx)
    }
    shown <- utils::head(na_names, 5L)
    warning(
      sprintf(
        "%d feature(s) had undefined scores and were excluded: %s%s",
        length(na_idx),
        paste(shown, collapse = ", "),
        if (length(na_idx) > 5L) ", ..." else ""
      ),
      call. = FALSE
    )
  }

  mask
}

#' Shape the result of a filter-based feature selection
#'
#' Single implementation of the seven output shapes shared by the `output`
#' argument of `fs_supervised()` and `fs_unsupervised()`. Those front ends
#' handle their eighth shape, `"result"`, themselves; they call this helper
#' twice on the way (for `"names"` and `"dt"`) to fill the `fs_result`.
#'
#' Columns are subset by integer index, never by name, so duplicated column
#' names cannot select the wrong columns, and `mask` is trusted to be aligned
#' to `dt` positionally.
#'
#' Empty selections: `"matrix"` and `"data.frame"` (and the `filtered` element
#' of `"list"`) are `nrow(dt)` x 0, so the input row count survives. The `"dt"`
#' shape is only guaranteed to have zero columns, because data.table
#' represents a zero-column table as having zero rows. `"mask"` comes back
#' all-FALSE, and `"indices"` and `"names"` come back as empty integer and
#' character vectors.
#'
#' @param dt data.table holding all candidate feature columns.
#' @param scores Named numeric vector of per-feature scores.
#' @param mask Logical keep-mask, as returned by `filter_mask()`.
#' @param out One of `"matrix"`, `"dt"`, `"data.frame"`, `"mask"`,
#'   `"indices"`, `"names"`, `"list"`.
#' @return The selected columns in the requested shape. For `"list"`, a list
#'   with `filtered` (matrix), `mask`, `indices`, `names`, and `scores`;
#'   callers append their own `meta` element.
#' @noRd
filter_output <- function(dt, scores, mask, out) {
  keep_idx <- which(mask)
  keep_names <- names(dt)[keep_idx]

  if (out == "mask") {
    return(mask)
  }
  if (out == "indices") {
    return(keep_idx)
  }
  if (out == "names") {
    return(keep_names)
  }

  if (length(keep_idx) == 0L) {
    empty_mat <- matrix(
      numeric(0),
      nrow = nrow(dt),
      ncol = 0L,
      dimnames = list(NULL, character(0))
    )
    return(switch(
      out,
      "matrix" = empty_mat,
      "dt" = data.table::as.data.table(empty_mat),
      "data.frame" = as.data.frame(empty_mat),
      "list" = list(
        filtered = empty_mat,
        mask = mask,
        indices = keep_idx,
        names = keep_names,
        scores = scores
      )
    ))
  }

  filtered_dt <- dt[, keep_idx, with = FALSE]

  switch(
    out,
    "matrix" = as.matrix(filtered_dt),
    "dt" = filtered_dt,
    "data.frame" = as.data.frame(filtered_dt),
    "list" = list(
      filtered = as.matrix(filtered_dt),
      mask = mask,
      indices = keep_idx,
      names = keep_names,
      scores = scores
    )
  )
}
