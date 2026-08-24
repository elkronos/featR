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
#' Computed in one pass over `table(x, y)` (rows with no observations are
#' skipped) rather than by looping over the levels of `x`. Inputs are expected
#' to be NA-free and of equal length.
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

#' Information gain and split entropy for one predictor
#'
#' Complete cases are taken per pair (rows are not dropped globally), but the
#' target must already be categorical with breaks computed once on the full
#' data so results are comparable across predictors. The split entropy H(X)
#' is computed on exactly the same complete cases and after the same
#' discretization, so `InfoGain / SplitEntropy` is a well-defined gain ratio.
#'
#' @param x Predictor vector (numeric/character/factor/logical/date-like).
#' @param y_cat Full-length categorical target from `ig_categorical_target()`.
#' @param numeric_bins Optional integer for predictor discretization.
#' @return Numeric vector of length two: information gain and split entropy,
#'   both `NA` when there are no complete cases.
#' @noRd
ig_score_one <- function(x, y_cat, numeric_bins = NULL) {
  ok <- !is.na(x) & !is.na(y_cat)
  x <- x[ok]
  y <- y_cat[ok]

  if (!length(x) || !length(y)) {
    return(c(NA_real_, NA_real_))
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

  # A constant predictor carries no information and has no split entropy
  if (length(unique(x[!is.na(x)])) <= 1L) {
    return(c(0, 0))
  }

  ig <- ig_entropy(y) - ig_cond_entropy(x, y)

  # Robustness clamp against tiny negatives / non-finites
  if (!is.finite(ig)) ig <- 0
  if (ig < 0) ig <- 0

  c(as.numeric(ig), as.numeric(ig_entropy(x)))
}

#' Gain ratio from information gain and split entropy
#'
#' `NA` propagates. A predictor whose split entropy is (numerically) zero --
#' a constant column -- scores 0 instead of dividing by zero.
#'
#' @param info_gain Numeric vector of information gains, in bits.
#' @param split_entropy Numeric vector of predictor entropies H(X), in bits.
#' @return Numeric vector of gain ratios, same length as `info_gain`.
#' @noRd
ig_gain_ratio <- function(info_gain, split_entropy) {
  tol <- .Machine$double.eps^0.5
  out <- rep(NA_real_, length(info_gain))
  ok <- !is.na(info_gain) & !is.na(split_entropy)
  if (any(ok)) {
    num <- info_gain[ok]
    den <- split_entropy[ok]
    out[ok] <- ifelse(den > tol, num / den, 0)
  }
  out
}

#' Information gain for a single data.frame
#'
#' @param df data.frame/data.table with predictors and target.
#' @param target Character target column name.
#' @param numeric_bins Optional integer bin override.
#' @param remove_na If `TRUE`, drop rows with NA in the TARGET only.
#' @return data.frame with columns Variable, InfoGain, and SplitEntropy.
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
                      SplitEntropy = numeric(), stringsAsFactors = FALSE))
  }

  # Discretize the target ONCE, up front, using all rows with a non-NA
  # target, so bin count and cut() breaks are shared across predictors.
  y_cat <- ig_categorical_target(dt[[target]], numeric_bins = numeric_bins)

  scored <- vapply(
    predictors,
    function(col) ig_score_one(dt[[col]], y_cat, numeric_bins = numeric_bins),
    numeric(2L)
  )

  data.frame(
    Variable = predictors,
    InfoGain = as.numeric(scored[1L, ]),
    SplitEntropy = as.numeric(scored[2L, ]),
    stringsAsFactors = FALSE
  )
}

#' Information gain across a list of data.frames
#'
#' @param dfs_list Named or unnamed list of data.frames.
#' @param target Character target column present in each data.frame.
#' @param numeric_bins Optional integer bin override (passed through).
#' @param remove_na Logical; drop rows with NA target (per data.frame).
#' @return data.frame with columns Variable, InfoGain, SplitEntropy, Origin.
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

#' Feature names that appear in more than one scored data.frame
#'
#' The union of features across a list of data.frames can contain the same
#' name twice. `fs_infogain()` keeps the higher score; this helper records
#' every row involved so nothing is silently dropped.
#'
#' @param tab Scored table with columns Variable, the score column, and
#'   (optionally) Origin.
#' @param score_col Name of the column holding the score.
#' @return data.frame with columns Variable, Origin, Score, Kept; zero rows
#'   when no name collides.
#' @noRd
ig_collisions <- function(tab, score_col) {
  variables <- as.character(tab$Variable)
  idx <- which(variables %in% unique(variables[duplicated(variables)]))

  out <- data.frame(
    Variable = variables[idx],
    Origin = if ("Origin" %in% names(tab)) {
      as.character(tab$Origin)[idx]
    } else {
      rep(NA_character_, length(idx))
    },
    Score = as.numeric(tab[[score_col]])[idx],
    Kept = rep(FALSE, length(idx)),
    stringsAsFactors = FALSE
  )

  if (nrow(out) > 0L) {
    key <- out$Score
    key[is.na(key)] <- -Inf
    out <- out[order(out$Variable, -key), , drop = FALSE]
    out$Kept <- !duplicated(out$Variable)
    row.names(out) <- NULL
  }
  out
}

#' Feature Selection via Information Gain
#'
#' Accepts either a single data.frame or a list of data.frames and scores
#' every predictor by its information gain (in bits) with respect to the
#' target, optionally normalized to a gain ratio.
#'
#' @details
#' Use this to rank candidate predictors cheaply, before any model is fitted:
#' it answers "how many bits of uncertainty about the target does knowing this
#' one predictor remove?". It needs no distributional assumptions and handles
#' mixed column types, but it is a univariate filter -- each predictor is
#' scored on its own, so two redundant copies of the same information both
#' score highly, and a predictor that only matters in combination with another
#' scores low. Treat the ranking as a shortlist, not a final feature set.
#'
#' * Numeric predictors are discretized into `max(Freedman-Diaconis, Sturges)`
#'   equal-width bins (never fewer than 2), unless `numeric_bins` overrides
#'   the count. Under the automatic rule, a column with a zero range or a zero
#'   interquartile range falls back to 2 bins; a column with a single distinct
#'   value becomes a single-level factor either way and scores 0.
#' * The target is always treated categorically, so `task` is always
#'   `"classification"`. Numeric targets are discretized the same way, ONCE
#'   on all rows with a non-NA target, so bin breaks are shared and scores
#'   are comparable across predictors.
#' * Date-like predictors are expanded into `*_year`, `*_month`, `*_day`
#'   columns (base R; the originals are dropped). Date-like targets are
#'   treated as categorical days.
#' * NAs are handled per predictor/target pair; rows are never dropped
#'   globally for other predictors. A predictor whose score is undefined
#'   (`NA`) is never selected.
#' * `remove_na` has a deliberately narrow effect: rows with NA in the target
#'   are excluded per pair anyway, so the observable difference is only when
#'   the target is entirely NA -- `remove_na = TRUE` stops with an error,
#'   while `remove_na = FALSE` returns `NA` information gain for every
#'   predictor.
#'
#' @section Cardinality bias and the gain ratio:
#' Raw information gain systematically favors predictors with many distinct
#' levels. In the limit, a near-unique identifier column splits the data into
#' near-singleton groups, drives the conditional entropy H(Y | X) to zero and
#' therefore attains the largest gain any predictor can attain, H(Y) -- while
#' carrying no generalizable signal whatsoever. Comparing raw gains across
#' predictors of different cardinality is therefore comparing unlike things.
#'
#' `normalize = "gain_ratio"` applies Quinlan's correction: each predictor's
#' gain is divided by that predictor's own split entropy H(X), the entropy of
#' its (discretized) level distribution. H(X) grows with cardinality -- it is
#' `log2(k)` for a predictor with `k` equally frequent levels -- so dividing
#' by it charges a predictor for the fineness of the split it makes. A
#' constant predictor has `H(X) = 0` and, rather than dividing by zero, is
#' assigned a gain ratio of 0 (its gain is zero too). Because gain ratios are
#' scaled gains, do not compare them against thresholds calibrated for raw
#' gains in bits.
#'
#' @param data A data.frame (a data.table is accepted and is copied, never
#'   modified in place), or a list of data.frames each containing `target`.
#' @param target Character. Name of the target column.
#' @param numeric_bins Optional whole number >= 1 (values below 2 are clamped
#'   to 2) overriding the automatic bin count for numeric predictors and for a
#'   numeric target. Default `NULL` (bins chosen per column).
#' @param normalize One of `"none"` (default, raw information gain in bits)
#'   or `"gain_ratio"` (gain divided by the predictor's split entropy). See
#'   the section on cardinality bias.
#' @param top_n Optional whole number >= 1. When supplied, `selected` holds
#'   the `top_n` highest-scoring features (fewer if fewer were scored). This
#'   is a rank cut, not a score floor: a zero-scoring feature is selected if
#'   the ranking reaches it. When `NULL` (default), `selected` instead holds
#'   every feature whose score is strictly greater than 0.
#' @param remove_na Logical. If `TRUE` (default), rows with NA in the target
#'   are removed up front. See Details for its narrow practical effect.
#' @param verbose Logical. If `TRUE`, report how many features were scored,
#'   how many names collided across data.frames, and how many were selected.
#'   Default `FALSE`.
#' @return An object of class `fs_result` with elements:
#' * `selected`: character vector of selected feature names, ordered by
#'   decreasing score. Features with an undefined (`NA`) score are never
#'   selected.
#' * `scores`: named numeric vector of the (possibly normalized) score for
#'   every candidate feature, ranked in decreasing order with `NA` scores
#'   last. For list input this is the union of features across data.frames;
#'   a name occurring in several data.frames keeps its highest score.
#' * `method`: `"infogain"`.
#' * `task`: `"classification"` (the target is always discretized).
#' * `model`: `NULL` (this is a filter; nothing is fitted).
#' * `details`: a list holding `table` (the full scored table, one row per
#'   scored column of each input: `Variable`, `InfoGain`, plus `SplitEntropy`
#'   and `GainRatio` when `normalize = "gain_ratio"`, plus `Origin` for list
#'   input), `normalize` (as resolved), `numeric_bins` (the requested bin
#'   count as an integer, `NULL` when automatic), `n_features` (the number of
#'   distinct scored features, i.e. `length(scores)`), and, for list input,
#'   `collisions` (a table of the feature names found in more than one
#'   data.frame, with `Kept` marking the row whose score won; zero rows when
#'   there are none).
#' * `call`: the matched call.
#' @examples
#' # Single data.frame:
#' df <- data.frame(
#'   A = rep(1:10, each = 10),
#'   B = rep(c("yes", "no"), 50),
#'   when = as.Date("2020-01-01") + rep(0:24, 4),
#'   target = rep(1:2, 50)
#' )
#' res <- fs_infogain(df, target = "target")
#' res$selected
#' res$scores
#' res$details$table
#'
#' # Normalize by split entropy to offset the bias toward many-leveled
#' # predictors, and keep the two best-ranked features:
#' fs_infogain(df, target = "target", normalize = "gain_ratio", top_n = 2)
#'
#' # List of data.frames:
#' df1 <- data.frame(A = rep(1:5, 20), target = rep(1:2, 50))
#' df2 <- data.frame(B = rep(c("yes", "no"), 50), target = rep(letters[1:2], 50))
#' fs_infogain(list(df1 = df1, df2 = df2), target = "target")
#' @export
fs_infogain <- function(data,
                        target,
                        numeric_bins = NULL,
                        normalize = c("none", "gain_ratio"),
                        top_n = NULL,
                        remove_na = TRUE,
                        verbose = FALSE) {
  cl <- match.call()

  normalize <- match.arg(normalize)
  assert_string(target, "target")
  assert_flag(remove_na, "remove_na")
  assert_flag(verbose, "verbose")
  if (!is.null(numeric_bins)) {
    numeric_bins <- assert_count(numeric_bins, "numeric_bins", lower = 1L)
  }
  if (!is.null(top_n)) {
    top_n <- assert_count(top_n, "top_n", lower = 1L)
  }

  is_list_input <- !inherits(data, "data.frame") && is.list(data)

  if (inherits(data, "data.frame")) {
    assert_data_frame(data)
    tab <- ig_single(
      df = data,
      target = target,
      numeric_bins = numeric_bins,
      remove_na = remove_na
    )
  } else if (is_list_input) {
    if (!all(vapply(data, function(x) inherits(x, "data.frame"), logical(1L)))) {
      stop("All elements in 'data' must be data.frames.")
    }
    tab <- ig_multiple(
      dfs_list = data,
      target = target,
      numeric_bins = numeric_bins,
      remove_na = remove_na
    )
  } else {
    stop("Input 'data' must be a data.frame or a list of data.frames.")
  }

  if (normalize == "gain_ratio") {
    tab$GainRatio <- ig_gain_ratio(tab$InfoGain, tab$SplitEntropy)
    score_col <- "GainRatio"
  } else {
    score_col <- "InfoGain"
  }

  # Column order: Variable, InfoGain, [SplitEntropy, GainRatio], [Origin].
  # Under normalize = "none" the split entropy is only an intermediate value,
  # so it is dropped and the table keeps Variable and InfoGain alone.
  keep_cols <- c(
    "Variable",
    "InfoGain",
    if (normalize == "gain_ratio") c("SplitEntropy", "GainRatio"),
    if ("Origin" %in% names(tab)) "Origin"
  )
  tab <- tab[, keep_cols, drop = FALSE]
  row.names(tab) <- NULL

  collisions <- ig_collisions(tab, score_col)

  scores <- stats::setNames(
    as.numeric(tab[[score_col]]),
    as.character(tab$Variable)
  )
  scores <- scores[order(scores, decreasing = TRUE, na.last = TRUE)]
  dup <- duplicated(names(scores))
  if (any(dup)) {
    # Ranked descending already, so the first occurrence is the higher score.
    scores <- scores[!dup]
  }

  n_features <- length(scores)

  selected <- if (is.null(top_n)) {
    names(scores)[!is.na(scores) & scores > 0]
  } else {
    utils::head(names(scores)[!is.na(scores)], top_n)
  }

  details <- list(
    table = tab,
    normalize = normalize,
    numeric_bins = numeric_bins,
    n_features = n_features
  )
  if (is_list_input) {
    details$collisions <- collisions
  }

  if (verbose) {
    message(sprintf(
      "Scored %d feature%s by information gain (normalize = '%s').",
      n_features, if (n_features == 1L) "" else "s", normalize
    ))
    if (nrow(collisions) > 0L) {
      message(sprintf(
        "%d feature name(s) occurred in more than one data.frame; kept the highest score.",
        length(unique(collisions$Variable))
      ))
    }
    message(sprintf(
      "Selected %d feature%s.",
      length(selected), if (length(selected) == 1L) "" else "s"
    ))
  }

  new_fs_result(
    selected = selected,
    scores = scores,
    method = "infogain",
    task = "classification",
    model = NULL,
    details = details,
    call = cl
  )
}
