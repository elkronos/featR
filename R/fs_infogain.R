# Information gain feature selection for featR.
# Uses only Imports (data.table, stats, utils); date handling is base R.

#' Shannon entropy of a vector (in bits), ignoring NA values
#'
#' Works for numeric, character, or factor vectors (treated categorically).
#' Empty factor levels contribute nothing.
#'
#' @param x A vector.
#' @return A single numeric value.
#' @noRd
ig_entropy <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0L) {
    return(0)
  }
  freq <- as.numeric(table(x))
  if (!length(freq)) {
    return(0)
  }
  prob <- freq / n
  prob <- prob[prob > 0]
  if (!length(prob)) {
    return(0)
  }
  val <- -sum(prob * log2(prob))
  if (!is.finite(val)) 0 else val
}

#' Sturges' rule for a bin count (>= 2)
#' @noRd
ig_sturges_bins <- function(n) {
  max(2L, ceiling(log2(n) + 1))
}

#' Number of histogram bins: max(Freedman-Diaconis, Sturges), >= 2
#' @noRd
ig_bins <- function(x) {
  n <- sum(!is.na(x))
  if (n <= 1L) {
    return(2L)
  }

  iqr_x <- stats::IQR(x, na.rm = TRUE)
  range_x <- diff(range(x, na.rm = TRUE))

  if (iqr_x == 0 || range_x == 0) {
    return(2L)
  }

  fd_bin_width <- 2 * iqr_x / (n^(1 / 3))
  if (!is.finite(fd_bin_width) || fd_bin_width <= 0) {
    return(ig_sturges_bins(n))
  }

  fd_bins <- ceiling(range_x / fd_bin_width)
  bins <- max(fd_bins, ig_sturges_bins(n))
  max(2L, as.integer(bins))
}

#' Discretize a numeric vector into an ordered factor
#'
#' A vector with one unique non-NA value yields a single-level factor
#' (NA positions stay NA).
#'
#' @param x Numeric vector.
#' @param bins Optional integer; when `NULL`, `ig_bins()` decides.
#' @return Ordered factor of bin membership.
#' @noRd
ig_discretize <- function(x, bins = NULL) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) <= 1L) {
    return(factor(ifelse(is.na(x), NA, ux[1L]), ordered = TRUE))
  }
  if (is.null(bins)) {
    bins <- ig_bins(x)
  }
  bins <- max(2L, as.integer(bins))
  cut(x, breaks = bins, include.lowest = TRUE, ordered_result = TRUE)
}

#' Is a column Date or POSIXt?
#' @noRd
ig_is_date_like <- function(x) {
  inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")
}

#' Expand date-like columns into *_year, *_month, *_day
#'
#' Uses base R (`as.POSIXlt()`) instead of lubridate. POSIXlt input columns
#' are converted to POSIXct first because data.table cannot hold POSIXlt
#' columns. The target column (via `exclude`) is never touched.
#'
#' @param df data.frame or data.table.
#' @param exclude Character vector of column names to skip (e.g. the target).
#' @param remove_original Logical; drop the original date columns.
#' @return A data.table copy with expanded columns.
#' @noRd
ig_expand_dates <- function(df, exclude = character(), remove_original = TRUE) {
  # data.table cannot hold POSIXlt columns: convert them to POSIXct first.
  lt_cols <- names(df)[vapply(df, inherits, logical(1L), what = "POSIXlt")]
  for (col in lt_cols) {
    df[[col]] <- as.POSIXct(df[[col]])
  }

  dt <- as_dt(df)
  cand <- setdiff(names(dt), exclude)

  date_cols <- cand[vapply(dt[, cand, with = FALSE], ig_is_date_like, logical(1L))]

  if (length(date_cols) > 0L) {
    for (col in date_cols) {
      lt <- as.POSIXlt(dt[[col]])
      data.table::set(dt, j = paste0(col, "_year"), value = lt$year + 1900L)
      data.table::set(dt, j = paste0(col, "_month"), value = lt$mon + 1L)
      data.table::set(dt, j = paste0(col, "_day"), value = lt$mday)
    }
    if (remove_original) {
      for (col in date_cols) {
        data.table::set(dt, j = col, value = NULL)
      }
    }
  }
  dt
}

#' Coerce the target to categorical
#'
#' Numeric targets are discretized via `ig_discretize()` (breaks computed from
#' all non-NA values of the full vector, so this must be called ONCE on the
#' full-length target). Date-like targets become factors of their
#' YYYY-MM-DD representation.
#'
#' @param y Target vector.
#' @param numeric_bins Optional integer override for numeric discretization.
#' @return Factor (or ordered factor) of the same length as `y`.
#' @noRd
ig_categorical_target <- function(y, numeric_bins = NULL) {
  if (ig_is_date_like(y)) {
    return(factor(as.Date(y)))
  }
  if (is.numeric(y)) {
    return(ig_discretize(y, bins = numeric_bins))
  }
  if (is.logical(y) || is.character(y)) {
    return(factor(y))
  }
  if (is.factor(y)) {
    return(y)
  }
  factor(y)
}

#' Conditional entropy H(Y | X) via a contingency table
#'
#' Vectorized replacement for the per-level `which()` loop. Inputs are
#' expected to be NA-free and equal length.
#'
#' @param x Factor (or vector coercible by `table()`).
#' @param y Factor (or vector coercible by `table()`).
#' @return A single numeric value (bits).
#' @noRd
ig_cond_entropy <- function(x, y) {
  tab <- table(x, y)
  n <- sum(tab)
  if (n == 0L) {
    return(0)
  }
  row_tot <- rowSums(tab)
  keep <- row_tot > 0
  tab <- tab[keep, , drop = FALSE]
  row_tot <- row_tot[keep]
  if (!length(row_tot)) {
    return(0)
  }
  p <- tab / row_tot
  plogp <- p * log2(p)
  plogp[!is.finite(plogp)] <- 0
  h_rows <- -rowSums(plogp)
  val <- sum((row_tot / n) * h_rows)
  if (!is.finite(val)) 0 else val
}

#' Information gain for one predictor against a pre-discretized target
#'
#' Complete cases are taken per pair (rows are not dropped globally), but the
#' target must already be categorical with breaks computed once on the full
#' data so results are comparable across predictors.
#'
#' @param x Predictor vector (numeric/character/factor/logical/date-like).
#' @param y_cat Full-length categorical target from `ig_categorical_target()`.
#' @param numeric_bins Optional integer for predictor discretization.
#' @return Numeric information gain (NA when no complete cases).
#' @noRd
ig_info_gain_one <- function(x, y_cat, numeric_bins = NULL) {
  ok <- !is.na(x) & !is.na(y_cat)
  x <- x[ok]
  y <- y_cat[ok]

  if (!length(x) || !length(y)) {
    return(NA_real_)
  }

  # Discretize/normalize the predictor to a factor
  if (ig_is_date_like(x)) {
    x <- factor(as.Date(x))
  } else if (is.numeric(x)) {
    x <- ig_discretize(x, bins = numeric_bins)
  } else if (is.logical(x) || is.character(x)) {
    x <- factor(x)
  }
  # factors are left as-is

  # A constant predictor carries no information
  if (length(unique(x[!is.na(x)])) <= 1L) {
    return(0)
  }

  ig <- ig_entropy(y) - ig_cond_entropy(x, y)

  # Robustness clamp against tiny negatives / non-finites
  if (!is.finite(ig)) ig <- 0
  if (ig < 0) ig <- 0

  as.numeric(ig)
}

#' Information gain for a single data.frame
#'
#' @param df data.frame/data.table with predictors and target.
#' @param target Character target column name.
#' @param numeric_bins Optional integer bin override.
#' @param remove_na If `TRUE`, drop rows with NA in the TARGET only.
#' @return data.frame with columns Variable and InfoGain.
#' @noRd
ig_single <- function(df, target, numeric_bins = NULL, remove_na = TRUE) {
  assert_target(df, target)

  # Expand date-like predictors, but never touch the target column.
  # Work on a plain data.frame afterwards: only column extraction and base
  # row subsetting are needed, which avoids data.table i/j scoping pitfalls
  # when user columns share names with internal variables.
  dt <- ig_expand_dates(df, exclude = target, remove_original = TRUE)
  data.table::setDF(dt)

  if (remove_na) {
    dt <- dt[!is.na(dt[[target]]), , drop = FALSE]
  }
  if (!nrow(dt)) {
    stop("No rows available after filtering on the target.")
  }

  predictors <- setdiff(names(dt), target)
  if (!length(predictors)) {
    return(data.frame(Variable = character(), InfoGain = numeric(),
                      stringsAsFactors = FALSE))
  }

  # Discretize the target ONCE, up front, using all rows with a non-NA
  # target, so bin count and cut() breaks are shared across predictors.
  y_cat <- ig_categorical_target(dt[[target]], numeric_bins = numeric_bins)

  ig <- vapply(
    predictors,
    function(col) ig_info_gain_one(dt[[col]], y_cat, numeric_bins = numeric_bins),
    numeric(1L)
  )

  data.frame(Variable = predictors, InfoGain = ig, stringsAsFactors = FALSE)
}

#' Information gain across a list of data.frames
#'
#' @param dfs_list Named or unnamed list of data.frames.
#' @param target Character target column present in each data.frame.
#' @param numeric_bins Optional integer bin override (passed through).
#' @param remove_na Logical; drop rows with NA target (per data.frame).
#' @return data.frame with columns Variable, InfoGain, Origin.
#' @noRd
ig_multiple <- function(dfs_list, target, numeric_bins = NULL, remove_na = TRUE) {
  if (!length(dfs_list)) {
    stop("'data' must be a non-empty list of data.frames.")
  }

  results_list <- vector("list", length(dfs_list))
  nm <- names(dfs_list)

  for (i in seq_along(dfs_list)) {
    df <- dfs_list[[i]]
    if (!inherits(df, "data.frame")) {
      stop("Element ", i, " of 'data' is not a data.frame.")
    }
    if (!(target %in% names(df))) {
      stop("Target '", target, "' not found in data.frame at position ", i, ".")
    }

    res <- ig_single(df, target, numeric_bins = numeric_bins,
                     remove_na = remove_na)

    origin_name <- if (!is.null(nm)) {
      this_name <- nm[i]
      if (!is.na(this_name) && nzchar(this_name)) this_name else paste0("Data_Frame_", i)
    } else {
      paste0("Data_Frame_", i)
    }

    if (nrow(res)) res$Origin <- origin_name
    results_list[[i]] <- res
  }

  out <- data.table::rbindlist(results_list, use.names = TRUE, fill = TRUE)
  data.table::setDF(out)
  out
}

#' Feature Selection via Information Gain
#'
#' Accepts either a single data.frame or a list of data.frames and computes
#' the information gain (in bits) of each predictor relative to the specified
#' target.
#'
#' @details
#' * Numeric predictors are discretized into
#'   `max(Freedman-Diaconis, Sturges)` bins (never fewer than 2), unless
#'   `numeric_bins` overrides the count.
#' * Numeric targets are discretized the same way, ONCE on all rows with a
#'   non-NA target, so bin breaks are shared and information gain is
#'   comparable across predictors.
#' * Date-like predictors are expanded into `*_year`, `*_month`, `*_day`
#'   columns (base R; the originals are dropped). Date-like targets are
#'   treated as categorical days.
#' * NAs are handled per predictor/target pair; rows are never dropped
#'   globally for other predictors.
#' * `remove_na` has a deliberately narrow effect: rows with NA in the target
#'   are excluded per pair anyway, so the observable difference is only when
#'   the target is entirely NA -- `remove_na = TRUE` stops with an error,
#'   while `remove_na = FALSE` returns `NA` information gain for every
#'   predictor.
#'
#' @param data A data.frame, or a list of data.frames each containing `target`.
#' @param target Character. Name of the target column.
#' @param numeric_bins Optional integer (>= 2 after clamping) overriding the
#'   automatic bin calculation for numeric columns. Default `NULL`.
#' @param remove_na Logical. If `TRUE` (default), rows with NA in the target
#'   are removed up front. See Details for its narrow practical effect.
#' @return Always a data.frame. For a single data.frame input it has columns
#'   `Variable` and `InfoGain`; for a list input it additionally has `Origin`
#'   (the list element name, or `Data_Frame_<i>` when unnamed).
#' @examples
#' # Single data.frame:
#' df <- data.frame(
#'   A = rep(1:10, each = 10),
#'   B = rep(c("yes", "no"), 50),
#'   when = as.Date("2020-01-01") + rep(0:24, 4),
#'   target = rep(1:2, 50)
#' )
#' fs_infogain(df, "target")
#'
#' # List of data.frames:
#' df1 <- data.frame(A = rep(1:5, 20), target = rep(1:2, 50))
#' df2 <- data.frame(B = rep(c("yes", "no"), 50), target = rep(letters[1:2], 50))
#' fs_infogain(list(df1 = df1, df2 = df2), "target")
#' @export
fs_infogain <- function(data, target, numeric_bins = NULL, remove_na = TRUE) {
  assert_string(target, "target")
  assert_flag(remove_na, "remove_na")
  if (!is.null(numeric_bins)) {
    numeric_bins <- assert_count(numeric_bins, "numeric_bins", lower = 1L)
  }

  if (inherits(data, "data.frame")) {
    assert_data_frame(data)
    ig_single(
      df = data,
      target = target,
      numeric_bins = numeric_bins,
      remove_na = remove_na
    )
  } else if (is.list(data)) {
    if (!all(vapply(data, function(x) inherits(x, "data.frame"), logical(1L)))) {
      stop("All elements in 'data' must be data.frames.")
    }
    ig_multiple(
      dfs_list = data,
      target = target,
      numeric_bins = numeric_bins,
      remove_na = remove_na
    )
  } else {
    stop("Input 'data' must be a data.frame or a list of data.frames.")
  }
}
