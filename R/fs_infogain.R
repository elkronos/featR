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
  # A tiny IQR beside a large range (a near-constant column with one extreme
  # outlier) drives the Freedman-Diaconis count arbitrarily high. Unbounded,
  # as.integer() would return NA past .Machine$integer.max and cut() would
  # then fail with "invalid number of intervals". More bins than observations
  # cannot describe the data anyway, so cap there.
  bins <- min(bins, max(2L, n))
  max(2L, as.integer(bins))
}

#' Discretize a numeric vector into an ordered factor
#'
#' A vector with one unique non-NA value yields a single-level factor
#' (NA positions stay NA).
#'
#' Infinite values cannot be binned by `cut(breaks = <count>)`, which needs a
#' finite range. The bin count and equal-width breaks are therefore computed
#' from the finite values alone, and the outermost breaks are widened to
#' `-Inf` / `Inf`, so `-Inf` and `Inf` fall into the lowest and highest bins.
#' A column with fewer than two distinct finite values is kept categorical
#' instead (for example `c(1, 1, Inf)` has two levels). Finite values that
#' differ only by floating-point round-off (e.g. `0.3` and `0.1 + 0.2`) are
#' too close for distinct equal-width breaks; coinciding breaks are merged,
#' and when a single bin is all that is left, or the finite values' range is
#' within `1024 * .Machine$double.eps` of their magnitude, they are treated
#' as one value.
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
  all_finite <- all(is.finite(ux))
  xf <- if (all_finite) x[!is.na(x)] else x[is.finite(x)]
  if (!all_finite && length(unique(xf)) < 2L) {
    return(factor(x, ordered = TRUE))
  }
  if (is.null(bins)) {
    bins <- ig_bins(xf)
  }
  bins <- max(2L, as.integer(bins))
  # Same equal-width breaks cut(breaks = <count>) builds for the finite
  # values. When those values differ only by round-off (0.3 vs 0.1 + 0.2),
  # the break spacing is below the floating-point resolution and adjacent
  # breaks coincide, where cut() stops with "'breaks' are not unique".
  # Merge coinciding breaks instead; if a single bin is all that is left, the
  # finite values are constant to machine precision.
  rx <- range(xf)
  breaks <- unique(seq.int(rx[1L], rx[2L], length.out = bins + 1L))
  nb <- length(breaks)
  if (nb < 3L || is_numerically_constant(xf)) {
    # One distinct finite value up to round-off: one level, plus a level each
    # for -Inf / Inf if present (as for the exactly-constant case).
    v <- x
    v[is.finite(v)] <- xf[1L]
    return(factor(v, ordered = TRUE))
  }
  if (all_finite) {
    # Widen the outer edges as cut() does, keeping its labels unchanged.
    dx <- rx[2L] - rx[1L]
    breaks[c(1L, nb)] <- c(rx[1L] - dx / 1000, rx[2L] + dx / 1000)
  } else {
    # Open the outer edges up to hold -Inf and Inf.
    breaks[c(1L, nb)] <- c(-Inf, Inf)
  }
  cut(x, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
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
    # The expansion writes <col>_year/_month/_day. If any of those names is
    # already taken, data.table::set() would overwrite that column in place --
    # silently, and including the target, whose discretization happens
    # afterwards. Refuse instead of corrupting the input.
    new_names <- as.vector(outer(date_cols, c("_year", "_month", "_day"),
                                 paste0))
    clashes <- intersect(new_names, names(dt))
    if (length(clashes) > 0L) {
      stop("Expanding the date column(s) ",
           paste(date_cols, collapse = ", "),
           " would overwrite existing column(s): ",
           paste(utils::head(clashes, 5L), collapse = ", "),
           if (length(clashes) > 5L) ", ..." else "",
           ". Rename them before calling fs_infogain().", call. = FALSE)
    }
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

  h_y <- ig_entropy(y)
  ig <- h_y - ig_cond_entropy(x, y)

  # H(Y) and H(Y | X) are computed separately, so an exactly independent
  # predictor comes out as a rounding residue of either sign (2.2e-16 bits,
  # say) rather than 0. A positive residue would pass the default `score > 0`
  # selection rule, so anything within rounding error of zero IS zero.
  if (!is.finite(ig) || ig <= 1024 * .Machine$double.eps * max(1, h_y)) {
    ig <- 0
  }

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
  # Predictors are looked up by name, so a duplicated name would score its
  # first copy and silently never look at the second, and an empty or NA
  # name looks up NULL and would be scored NA. Reject instead.
  assert_column_names(df, "fs_infogain")
  # A list column cannot be discretized or tabulated; it failed deep in the
  # entropy code with "unimplemented type 'list' in 'orderVector1'".
  list_cols <- names(df)[vapply(df, is.list, logical(1L))]
  if (length(list_cols) > 0L) {
    stop("List columns are not supported: ",
         paste(utils::head(list_cols, 5L), collapse = ", "),
         if (length(list_cols) > 5L) ", ..." else "",
         ". Convert or drop them before calling fs_infogain().", call. = FALSE)
  }

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

#' Rank features by information gain
#'
#' Scores every predictor by its information gain about the target, in bits,
#' optionally divided by the predictor's own entropy to give a gain ratio.
#' Accepts a single data frame or a list of data frames.
#'
#' @details
#' Information gain is the drop in the entropy of the target once you know
#' the value of one predictor: how many bits of uncertainty about the target
#' that predictor removes. It makes no distributional assumptions and handles
#' mixed column types, which makes it a cheap way to rank candidates before
#' fitting any model.
#'
#' It is a univariate filter. Each predictor is scored on its own, so two
#' copies of the same information both score highly, and a predictor that
#' matters only in combination with another scores low. Treat the ranking as
#' a shortlist rather than a final feature set.
#'
#' The default selection rule keeps every feature with a score above 0,
#' which screens out almost nothing. On a finite sample, a predictor
#' unrelated to the target still has a positive gain in almost every data
#' set, and more so with more bins or levels. In a 200-row example with 50
#' pure-noise columns, all 50 are selected. Use `top_n`, or compare against
#' gains computed on a permuted target, to make a real cut.
#'
#' @section How columns are prepared:
#' \itemize{
#'   \item Numeric predictors are cut into equal-width bins. The number of
#'     bins is the larger of the Freedman-Diaconis and Sturges counts, at
#'     least 2, unless `numeric_bins` sets it. Under the automatic rule, a
#'     column with zero range or zero interquartile range gets 2 bins. A
#'     column with a single distinct value is one level either way and
#'     scores 0.
#'   \item Character and logical predictors become factors.
#'   \item Date and date-time predictors are replaced by three numeric
#'     columns, `<name>_year`, `<name>_month`, and `<name>_day`, which are
#'     then binned like any numeric column. If one of those names already
#'     exists in `data`, the call stops.
#'   \item The target is always treated as categorical, so `task` is always
#'     `"classification"`. A numeric target is binned by the same rule, once,
#'     on all rows where it is observed, so every predictor is scored against
#'     the same bins. A date target is treated as one class per day.
#' }
#'
#' @section Missing values:
#' Missing values are handled for each predictor-target pair separately: a
#' missing value in one predictor does not remove that row for the others.
#' A predictor with no complete pairs scores `NA` and is never selected.
#'
#' `remove_na` has a narrow effect. Rows with a missing target are excluded
#' pair by pair anyway, so the only difference shows when the target is
#' entirely missing: `remove_na = TRUE` stops with an error, and
#' `remove_na = FALSE` returns an `NA` score for every predictor.
#'
#' @section Cardinality bias and the gain ratio:
#' Raw information gain favors predictors with many distinct levels. A
#' near-unique identifier column splits the data into groups of about one
#' row, which drives the conditional entropy H(Y | X) to zero and gives it
#' the largest gain any predictor can reach, H(Y), while carrying no signal
#' that generalizes. Raw gains of predictors with different numbers of levels
#' are therefore not comparable.
#'
#' `normalize = "gain_ratio"` applies Quinlan's correction: each predictor's
#' gain is divided by its split entropy H(X), the entropy of its (binned)
#' level distribution. H(X) grows with the number of levels (it is
#' `log2(k)` for `k` equally frequent levels), so the division penalizes a
#' predictor for how finely it splits the data. A constant predictor has
#' H(X) = 0 and gets a gain ratio of 0 instead of a division by zero. Gain
#' ratios are on a different scale from raw gains, so do not reuse a
#' threshold chosen for gains in bits.
#'
#' @param data A data frame, or a list of data frames that each contain
#'   `target`. A data.table is copied, never modified in place. Column names
#'   must be unique, non-empty, and not `NA` within each data frame, and
#'   list columns are not supported.
#' @param target Name of the target column.
#' @param numeric_bins Optional whole number of bins for numeric predictors
#'   and a numeric target, replacing the automatic count. Values below 2 are
#'   raised to 2. Default `NULL` (bins chosen per column).
#' @param normalize `"none"` (default) for raw information gain in bits, or
#'   `"gain_ratio"` for gain divided by the predictor's split entropy. See
#'   "Cardinality bias and the gain ratio".
#' @param top_n Optional whole number of at least 1. When supplied,
#'   `selected` holds the `top_n` highest-scoring features (fewer if fewer
#'   were scored). This is a rank cut, not a score floor, so a feature
#'   scoring 0 is selected if the ranking reaches it. When `NULL` (default),
#'   `selected` holds every feature with a score strictly greater than 0. A
#'   gain within floating-point rounding of zero, as an exactly independent
#'   predictor produces, is reported as exactly 0 and is not selected.
#' @param remove_na If `TRUE` (default), rows with a missing target are
#'   removed first. See "Missing values" for why this rarely changes the
#'   result.
#' @param verbose If `TRUE`, report how many features were scored, how many
#'   names occurred in more than one data frame, and how many were selected.
#'   Default `FALSE`.
#' @return An `fs_result` object with:
#' \describe{
#'   \item{`selected`}{Selected feature names in decreasing order of score.
#'     A feature with an `NA` score is never selected.}
#'   \item{`scores`}{Named numeric vector of the score (gain or gain ratio)
#'     for every candidate feature, in decreasing order with `NA` last. For
#'     list input it covers the union of features across data frames, and a
#'     name found in several data frames keeps its highest score.}
#'   \item{`method`}{`"infogain"`.}
#'   \item{`task`}{`"classification"`, because the target is always treated
#'     as categorical.}
#'   \item{`model`}{`NULL`; no model is fitted.}
#'   \item{`details`}{A list with:
#'     \itemize{
#'       \item `table`: the full scored table, one row per scored column of
#'         each input, with columns `Variable` and `InfoGain`, plus
#'         `SplitEntropy` and `GainRatio` when
#'         `normalize = "gain_ratio"`, plus `Origin` for list input.
#'       \item `normalize`: the value used.
#'       \item `numeric_bins`: the requested bin count as an integer, or
#'         `NULL` when automatic.
#'       \item `n_features`: the number of distinct scored features, equal
#'         to `length(scores)`.
#'       \item `collisions` (list input only): the feature names found in
#'         more than one data frame, with `Kept` marking the row whose score
#'         was used. It has zero rows when no name repeats.
#'     }}
#'   \item{`call`}{The matched call.}
#' }
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
#' @seealso The [Filter methods](https://elkronos.github.io/featR/articles/filters.html)
#'   guide, which runs this function on a worked example.
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
