# Correlation-based feature selection.
# polycor (polychoric) and foreach/doParallel (parallel point-biserial) are
# Suggests and are only touched at run time. The redundancy pruning below is
# built from base stats only, so caret is not needed here.

#' Find and prune highly correlated variables
#'
#' Finds pairs of variables whose absolute correlation exceeds `threshold`
#' and, by default, drops variables until no such pair remains, keeping one
#' representative of each correlated group.
#'
#' @details
#' This is an unsupervised redundancy filter. It looks only at how the
#' variables relate to one another, never at an outcome, so when two
#' variables are nearly interchangeable it cannot keep the one that predicts
#' better. It keeps whichever is less correlated with the rest of the data.
#' Use [fs_boruta()] when the choice within a correlated group should depend
#' on importance for a target.
#'
#' @section How pruning works:
#' Pruning is greedy. While any pair of retained variables still has an
#' absolute correlation above `threshold`, the strongest such pair is taken,
#' and the member with the larger mean absolute correlation to the other
#' retained variables is dropped (on an exact tie, the later column). Afterward
#' no two retained variables have a computable correlation above
#' `threshold`. The surviving set is not guaranteed to be the smallest one
#' with that property, and it can depend on the order in which pairs are
#' resolved.
#'
#' This borrows the tie-break of `caret::findCorrelation()` but not its scan
#' order: caret visits columns in order of mean correlation rather than
#' pairs in order of strength, so the two can drop different variables.
#' Variables that were never flagged are always kept.
#'
#' @section Correlations that cannot be computed:
#' A correlation that cannot be computed is `NA`. Such pairs are never
#' flagged and never pruned, because an unknown correlation is no evidence of
#' redundancy, and a warning reports how many there are. When no pair
#' exceeds `threshold`, a message (not a warning) says so, whatever
#' `verbose` is set to.
#'
#' @param data A data frame or matrix with at least 2 columns. Column names
#'   must be unique, non-empty, and not `NA`; duplicated names are an error
#'   because a correlation matrix with duplicated names is ambiguous. The
#'   allowed column types depend on `method`:
#'   \itemize{
#'     \item `"pearson"`, `"spearman"`, `"kendall"`: all numeric.
#'     \item `"polychoric"`: all ordered factors.
#'     \item `"pointbiserial"`: numeric, or any type with exactly 2 distinct
#'       non-missing values. A numeric column with more than 2 distinct
#'       values counts as continuous, and a column with exactly 2 counts as
#'       dichotomous. At least one of each is needed, or every correlation
#'       is `NA` (with a warning).
#'   }
#' @param threshold Absolute-correlation cutoff between 0 and 1. Pairs with
#'   an absolute correlation strictly greater than `threshold` are flagged.
#'   Required; there is no default.
#' @param method One of `"pearson"` (default), `"spearman"`, `"kendall"`,
#'   `"polychoric"` (needs the suggested polycor package), or
#'   `"pointbiserial"`. The point-biserial correlation is the Pearson
#'   correlation between a continuous variable and a 0/1 indicator of a
#'   dichotomous one, where 1 marks the second sorted value (for a factor,
#'   the second level). Only continuous-dichotomous pairs have one, so under
#'   this method every continuous-continuous and dichotomous-dichotomous
#'   cell stays `NA` and those pairs are never flagged.
#' @param prune If `TRUE` (default), `selected` is the reduced set left after
#'   pruning (see "How pruning works"). If `FALSE`, nothing is dropped and
#'   `selected` is every variable in at least one flagged pair: the
#'   redundant set, not the set to keep.
#' @param na.rm How to handle missing values. If `TRUE`, they are removed
#'   pairwise for `"pearson"`, `"spearman"`, and `"kendall"`; within each
#'   pair for `"pointbiserial"`; and casewise (rows complete across all
#'   columns) for `"polychoric"`. If `FALSE` (default), a missing value makes
#'   the affected correlations `NA`, except under `"polychoric"`, which stops
#'   with an error instead of silently dropping rows.
#' @param sample_frac Fraction of rows, in (0, 1], to sample without
#'   replacement before computing correlations. The sample size is rounded up
#'   and is never below one row. Default 1 (no sampling).
#' @param output_format Shape of `details$corr_matrix`: `"matrix"` (default)
#'   or `"data.frame"` (long form).
#' @param diag_value Single number, or `NA`, written on the diagonal of
#'   `details$corr_matrix`. The diagonal never takes part in flagging,
#'   scoring, or pruning. Default 0.
#' @param seed Optional integer seed for the row sampling. It is applied for
#'   this call only and the previous RNG state is restored afterward. It has
#'   no effect when `sample_frac = 1`, and parallel point-biserial
#'   computation uses no random numbers. Default `NULL` (featR does not
#'   touch the RNG).
#' @param verbose If `TRUE`, report row sampling, the correlation method, and
#'   any variables dropped by pruning. Default `FALSE`.
#' @param parallel If `TRUE`, compute point-biserial correlations in parallel
#'   with the suggested foreach and doParallel packages. Every other method
#'   computes the matrix in one call and ignores this. Default `FALSE`.
#' @param n_cores Number of workers when `parallel = TRUE` and
#'   `method = "pointbiserial"`, a whole number of at least 1, capped at the
#'   detected core count. Default 2.
#'
#' @return An `fs_result` object with:
#' \describe{
#'   \item{`selected`}{With `prune = TRUE`, every variable not in
#'     `details$dropped`; no two of them have a computable absolute
#'     correlation above `threshold`. With `prune = FALSE`, both members of
#'     every flagged pair.}
#'   \item{`scores`}{Named numeric vector giving each variable's largest
#'     absolute correlation with any other variable. Larger means more
#'     redundant. `NA` when no correlation with that variable could be
#'     computed.}
#'   \item{`method`}{`paste0("correlation_", method)`, for example
#'     `"correlation_pearson"`.}
#'   \item{`task`}{`NA`; the filter uses no outcome.}
#'   \item{`model`}{`NULL`; no model is fitted.}
#'   \item{`details`}{A list with:
#'     \itemize{
#'       \item `corr_matrix`: the correlation matrix with `diag_value` on
#'         the diagonal, or, when `output_format = "data.frame"`, the same
#'         values in long form with columns `Var1`, `Var2`, `Correlation`.
#'       \item `pairs`: a data frame of the flagged pairs (`Var1`, `Var2`,
#'         `Correlation`), strongest absolute correlation first, whatever
#'         `output_format` is.
#'       \item `dropped`: variables removed by pruning, in the order they
#'         were dropped; empty when `prune = FALSE`.
#'       \item `redundant`: every variable in at least one flagged pair,
#'         which is `selected` as it would be under `prune = FALSE`.
#'       \item `n_features`: the number of variables considered.
#'     }}
#'   \item{`call`}{The matched call.}
#' }
#'
#' @examples
#' d <- data.frame(
#'   a = c(1, 2, 3, 4, 5, 6),
#'   b = c(2, 4, 6, 8, 10, 12),
#'   c = c(1.5, 0.9, 2.1, 0.4, 1.1, 0.8)
#' )
#' res <- fs_correlation(d, threshold = 0.9)
#' res$selected
#' res$details$pairs
#'
#' # Both members of every flagged pair, with nothing dropped
#' fs_correlation(d, threshold = 0.9, prune = FALSE)$selected
#' @seealso The [Filter methods](https://elkronos.github.io/featR/articles/filters.html)
#'   guide, which runs this function on a worked example.
#' @export
fs_correlation <- function(data, threshold, method = "pearson", prune = TRUE,
                           na.rm = FALSE, sample_frac = 1,
                           output_format = "matrix", diag_value = 0,
                           seed = NULL, verbose = FALSE,
                           parallel = FALSE, n_cores = 2L) {
  cl <- match.call()

  # Validate inputs
  corr_validate_inputs(
    data          = data,
    threshold     = threshold,
    method        = method,
    prune         = prune,
    na.rm         = na.rm,
    sample_frac   = sample_frac,
    output_format = output_format,
    diag_value    = diag_value,
    verbose       = verbose,
    parallel      = parallel,
    n_cores       = n_cores
  )
  # Validate the seed even when sample_frac = 1 leaves it unused, so a
  # fractional or non-numeric seed is an error here as everywhere else.
  if (!is.null(seed)) {
    assert_count(seed, "seed", lower = -.Machine$integer.max)
  }

  # Method-specific suggested packages (fail early with a clear error)
  if (identical(method, "polychoric")) {
    fs_require("polycor", "polychoric correlations")
  }
  if (identical(method, "pointbiserial") && isTRUE(parallel)) {
    fs_require(c("foreach", "doParallel"), "parallel point-biserial computation")
  }

  # Sample rows if requested (seeding is local and opt-in)
  data <- corr_sample_data(data, sample_frac, seed, verbose)

  # Calculate the correlation matrix
  corr_matrix <- corr_calculate_correlation(
    data     = data,
    method   = method,
    na.rm    = na.rm,
    parallel = parallel,
    n_cores  = n_cores,
    verbose  = verbose
  )

  # Ensure square named matrix
  if (is.null(colnames(corr_matrix))) {
    colnames(corr_matrix) <- make.names(seq_len(ncol(corr_matrix)))
  }
  if (is.null(rownames(corr_matrix))) {
    rownames(corr_matrix) <- colnames(corr_matrix)
  }

  # Set the diagonal as specified (after any method-specific fill)
  diag(corr_matrix) <- diag_value

  # NA correlations can never exceed the threshold, so the affected pairs can
  # never be selected; warn once so this is not silent.
  na_mask <- is.na(corr_matrix)
  diag(na_mask) <- FALSE
  n_na <- sum(na_mask)
  if (n_na > 0L) {
    warning(sprintf(
      paste0("%d off-diagonal correlation value(s) are NA; ",
             "variable pairs with NA correlations can never be selected."),
      n_na
    ))
  }

  all_vars <- rownames(corr_matrix)

  # Find high-correlation pairs and the variables they involve
  high_corr_idx <- corr_find_high_correlation(corr_matrix, threshold)
  pairs <- corr_pair_table(corr_matrix, high_corr_idx)

  redundant <- intersect(all_vars, unique(c(pairs$Var1, pairs$Var2)))
  if (length(redundant) == 0L) {
    message("No variables meet the correlation threshold.")
  }

  # Per-variable score: strongest absolute correlation with any other variable
  scores <- corr_max_abs_scores(corr_matrix)

  # Reduce each correlated group to a single representative
  dropped <- character(0)
  if (isTRUE(prune)) {
    dropped <- corr_prune_redundant(corr_matrix, threshold)
    selected_vars <- setdiff(all_vars, dropped)
    if (isTRUE(verbose) && length(dropped) > 0L) {
      message(sprintf(
        "Dropped %d redundant variable(s) (|r| > %g): %s",
        length(dropped), threshold, paste(dropped, collapse = ", ")
      ))
    }
  } else {
    selected_vars <- redundant
  }

  # Optional reshape
  if (identical(output_format, "data.frame")) {
    cm_df <- as.data.frame(as.table(corr_matrix), stringsAsFactors = FALSE)
    names(cm_df) <- c("Var1", "Var2", "Correlation")
    corr_matrix <- cm_df
  }

  new_fs_result(
    selected = selected_vars,
    scores   = scores,
    method   = paste0("correlation_", method),
    task     = NA_character_,
    model    = NULL,
    details  = list(
      corr_matrix = corr_matrix,
      pairs       = pairs,
      dropped     = dropped,
      redundant   = redundant,
      n_features  = length(all_vars)
    ),
    call = cl
  )
}

# ----------------------------- Helpers --------------------------------------

#' Validate fs_correlation() inputs
#' @noRd
corr_validate_inputs <- function(data, threshold, method, prune, na.rm,
                                 sample_frac, output_format, diag_value,
                                 verbose, parallel, n_cores) {
  assert_data_frame(data, "data", allow_matrix = TRUE)

  if (ncol(data) < 2L) {
    stop("`data` must have at least 2 columns to compute correlations.")
  }

  # A correlation matrix with duplicated dimnames is ambiguous: every lookup
  # into it (pruning, pair reporting, scoring) resolves to the first match, so
  # the second copy's correlations would be silently replaced by the first's
  # and both columns could be dropped together. Reject rather than guess.
  nms <- colnames(data)
  if (is.null(nms) || anyDuplicated(nms) > 0L) {
    if (is.null(nms)) {
      stop("`data` must have column names.", call. = FALSE)
    }
    dupes <- unique(nms[duplicated(nms)])
    stop("`data` must have unique column names; duplicated: ",
         paste(utils::head(dupes, 5L), collapse = ", "),
         if (length(dupes) > 5L) ", ..." else "",
         ". Rename the duplicates before calling fs_correlation().",
         call. = FALSE)
  }
  # An empty or NA name cannot be looked up in the correlation matrix's
  # dimnames (indexing by "" matches nothing), so pruning failed with
  # "subscript out of bounds".
  if (anyNA(nms) || any(!nzchar(nms))) {
    stop("`data` must not have empty or NA column names.", call. = FALSE)
  }

  assert_string(method, "method")
  valid_methods <- c("pearson", "spearman", "kendall", "polychoric", "pointbiserial")
  if (!(method %in% valid_methods)) {
    stop("Invalid `method`. Choose one of: ", paste(valid_methods, collapse = ", "), ".")
  }

  assert_number(threshold, "threshold", lower = 0, upper = 1)
  assert_flag(prune, "prune")
  assert_flag(na.rm, "na.rm")

  assert_number(sample_frac, "sample_frac", lower = 0, upper = 1)
  if (sample_frac <= 0) {
    stop("`sample_frac` must be greater than 0.")
  }

  assert_string(output_format, "output_format")
  if (!(output_format %in% c("matrix", "data.frame"))) {
    stop("`output_format` must be 'matrix' or 'data.frame'.")
  }

  # diag_value: a numeric scalar or a logical NA. A character NA would pass
  # an is.na() test but turn the whole correlation matrix into character.
  if (!(length(diag_value) == 1L &&
        (is.numeric(diag_value) ||
         (is.logical(diag_value) && is.na(diag_value))))) {
    stop("`diag_value` must be a single numeric value or NA.")
  }

  assert_flag(verbose, "verbose")
  assert_flag(parallel, "parallel")
  assert_count(n_cores, "n_cores")

  df <- as.data.frame(data)

  # Column-type checks per method
  if (method %in% c("pearson", "spearman", "kendall")) {
    if (!all(vapply(df, is.numeric, logical(1L)))) {
      stop("All columns must be numeric for method '", method, "'.")
    }
  } else if (identical(method, "pointbiserial")) {
    ok <- vapply(df, function(x) is.numeric(x) || corr_is_dichotomous(x), logical(1L))
    if (!all(ok)) {
      stop("For 'pointbiserial', all columns must be numeric or dichotomous (exactly 2 unique non-NA values).")
    }
    # Require at least one continuous and one dichotomous variable; otherwise no valid pairs exist
    has_cont <- any(vapply(df, corr_is_continuous, logical(1L)))
    has_dich <- any(vapply(df, corr_is_dichotomous, logical(1L)))
    if (!has_cont || !has_dich) {
      warning("No valid continuous-dichotomous pairs found for 'pointbiserial'. Result will be an NA matrix.")
    }
  } else if (identical(method, "polychoric")) {
    if (!all(vapply(df, is.ordered, logical(1L)))) {
      stop("All columns must be ordered factors for method 'polychoric'.")
    }
  }

  invisible(TRUE)
}

#' Sample data (row-wise)
#'
#' Optional local seeding via local_seed(); the previous RNG state is
#' restored when this helper exits.
#' @noRd
corr_sample_data <- function(data, sample_frac, seed = NULL, verbose = FALSE) {
  # Early exit: no sampling if sample_frac is effectively 1
  if (sample_frac == 1) return(data)

  n_rows <- nrow(data)
  if (is.null(n_rows) || n_rows < 1L) {
    stop("`data` must have at least one row.")
  }

  local_seed(seed)

  size <- max(1L, min(n_rows, ceiling(sample_frac * n_rows)))
  idx  <- sample.int(n_rows, size = size, replace = FALSE)

  if (isTRUE(verbose)) {
    message(sprintf("Sampling %d/%d rows (%.1f%%).", size, n_rows, 100 * size / n_rows))
  }

  data[idx, , drop = FALSE]
}

#' Calculate correlation matrix (method dispatch)
#' @noRd
corr_calculate_correlation <- function(data, method, na.rm, parallel, n_cores, verbose) {
  if (identical(method, "pointbiserial")) {
    return(corr_calculate_pointbiserial_correlation(data, na.rm, parallel, n_cores, verbose))
  } else if (identical(method, "polychoric")) {
    return(corr_calculate_polychoric_correlation(data, na.rm, verbose))
  } else {
    use_opt <- if (isTRUE(na.rm)) "pairwise.complete.obs" else "everything"
    if (isTRUE(verbose)) {
      message("Calculating ", method, " correlation matrix (use = '", use_opt, "').")
    }
    data <- as.data.frame(data)
    return(stats::cor(data, method = method, use = use_opt))
  }
}

#' Point-biserial correlation matrix
#'
#' The point-biserial correlation IS the Pearson correlation between the
#' continuous variable and a 0/1 coding of the dichotomous variable, so it is
#' computed directly with stats::cor() (the indicator is 1 for the second
#' sorted unique value, e.g. the second factor level). Cells for pairs that
#' are not continuous-dichotomous remain NA.
#' @noRd
corr_calculate_pointbiserial_correlation <- function(data, na.rm, parallel, n_cores, verbose) {
  data <- as.data.frame(data)
  p    <- ncol(data)

  cm <- matrix(NA_real_, nrow = p, ncol = p)
  colnames(cm) <- colnames(data)
  rownames(cm) <- colnames(data)

  cont_idx <- which(vapply(data, corr_is_continuous, logical(1L)))
  dich_idx <- which(vapply(data, corr_is_dichotomous, logical(1L)))

  if (length(cont_idx) == 0L || length(dich_idx) == 0L) {
    if (isTRUE(verbose)) {
      message("No continuous-dichotomous pairs available; returning NA matrix.")
    }
    return(cm)
  }

  # Build all i (continuous) x j (dichotomous) pairs (exclude i==j just in case)
  pairs <- expand.grid(i = cont_idx, j = dich_idx,
                       KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$i != pairs$j, , drop = FALSE]

  if (nrow(pairs) == 0L) {
    if (isTRUE(verbose)) {
      message("No valid distinct pairs; returning NA matrix.")
    }
    return(cm)
  }

  # Compute with or without parallelism
  if (isTRUE(parallel)) {
    n_cores <- resolve_cores(n_cores)
    if (isTRUE(verbose)) {
      message("Running point-biserial in parallel on ", n_cores, " cores.")
    }

    # Stops the cluster and restores the caller's own foreach backend on exit.
    local_parallel_cluster(n_cores)

    `%dopar%` <- foreach::`%dopar%`

    k <- NULL # silence R CMD check: `k` is the foreach iterator below
    res <- foreach::foreach(
      k = seq_len(nrow(pairs)),
      .combine = rbind
    ) %dopar% {
      i <- pairs$i[k]; j <- pairs$j[k]
      xi <- data[[i]]
      xj <- data[[j]]
      # Robust: treat y as 2-level factor even if not coded 0/1
      grp <- if (is.factor(xj)) droplevels(xj) else factor(xj)
      r <- tryCatch(
        stats::cor(xi, as.numeric(grp == sort(unique(grp))[2]),
                   use = if (na.rm) "complete.obs" else "everything"),
        error = function(e) NA_real_
      )
      c(i = i, j = j, r = r)
    }

    # Normalize 'res' shape & names to avoid subscript issues on single-row results
    if (is.null(res) || length(res) == 0L) {
      return(cm)
    }
    if (is.vector(res) && length(res) == 3L && is.null(dim(res))) {
      res <- matrix(res, nrow = 1L, byrow = TRUE)
      colnames(res) <- c("i", "j", "r")
    }
    res <- as.data.frame(res, stringsAsFactors = FALSE)
    if (ncol(res) == 3L && !all(c("i", "j", "r") %in% names(res))) {
      names(res) <- c("i", "j", "r")
    }
    if (!all(c("i", "j", "r") %in% names(res))) {
      stop("Internal error: unexpected result shape from parallel point-biserial computation.")
    }

    if (nrow(res) > 0L) {
      ii <- as.integer(res$i)
      jj <- as.integer(res$j)
      rr <- as.numeric(res$r)
      for (k in seq_len(nrow(res))) {
        i <- ii[k]; j <- jj[k]; r <- rr[k]
        cm[i, j] <- r
        cm[j, i] <- r
      }
    }
  } else {
    if (isTRUE(verbose)) {
      message("Running point-biserial sequentially.")
    }
    for (k in seq_len(nrow(pairs))) {
      i <- pairs$i[k]; j <- pairs$j[k]
      xi <- data[[i]]
      xj <- data[[j]]
      grp <- if (is.factor(xj)) droplevels(xj) else factor(xj)
      r <- tryCatch(
        stats::cor(xi, as.numeric(grp == sort(unique(grp))[2]),
                   use = if (na.rm) "complete.obs" else "everything"),
        error = function(e) NA_real_
      )
      cm[i, j] <- r
      cm[j, i] <- r
    }
  }

  cm
}

#' Polychoric correlation matrix
#'
#' NA semantics: with na.rm = TRUE, casewise (complete-case) deletion is
#' applied. With na.rm = FALSE, missing values are an error, because
#' polycor::hetcor() cannot propagate NA the way stats::cor() does and
#' silently deleting cases would contradict the other methods.
#' @noRd
corr_calculate_polychoric_correlation <- function(data, na.rm, verbose) {
  data <- as.data.frame(data)

  if (!isTRUE(na.rm) && anyNA(data)) {
    stop("`data` contains missing values and na.rm = FALSE. Method 'polychoric' cannot propagate NAs into the correlation matrix; set na.rm = TRUE to use casewise deletion, or handle the missing values before calling fs_correlation().")
  }

  if (isTRUE(verbose)) {
    message("Calculating polychoric correlation matrix via polycor::hetcor().")
  }

  # Casewise deletion (a no-op when the data are already complete)
  res <- polycor::hetcor(data, use = "complete.obs")
  mat <- res$correlations
  # Ensure square with dimnames
  if (is.null(colnames(mat))) colnames(mat) <- make.names(seq_len(ncol(mat)))
  if (is.null(rownames(mat))) rownames(mat) <- colnames(mat)
  mat
}

#' Find high-correlation pairs (upper triangle only)
#'
#' Strictly greater than `threshold`; NA cells are never flagged. Returns the
#' two-column index matrix produced by `which(..., arr.ind = TRUE)`.
#' @noRd
corr_find_high_correlation <- function(corr_matrix, threshold) {
  # Work only on numeric entries
  m <- as.matrix(corr_matrix)
  # Identify indices with |r| > threshold and not NA
  idx <- which(!is.na(m) & abs(m) > threshold, arr.ind = TRUE)
  # keep upper triangle to avoid duplicates
  idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]
  idx
}

#' Flagged-pair table, strongest absolute correlation first
#' @noRd
corr_pair_table <- function(corr_matrix, idx) {
  m <- as.matrix(corr_matrix)

  if (nrow(idx) == 0L) {
    return(data.frame(
      Var1 = character(0), Var2 = character(0), Correlation = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- data.frame(
    Var1        = rownames(m)[idx[, 1]],
    Var2        = colnames(m)[idx[, 2]],
    Correlation = as.numeric(m[idx]),
    stringsAsFactors = FALSE
  )
  out <- out[order(abs(out$Correlation), decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Maximum absolute correlation of each variable with any other variable
#'
#' The diagonal is excluded (it holds `diag_value`, not a correlation), and NA
#' cells are ignored. Variables with no computable correlation score NA.
#' @noRd
corr_max_abs_scores <- function(corr_matrix) {
  m <- abs(as.matrix(corr_matrix))
  diag(m) <- NA_real_

  scores <- apply(m, 1L, function(v) {
    v <- v[!is.na(v)]
    if (length(v) == 0L) NA_real_ else max(v)
  })

  stats::setNames(as.numeric(scores), rownames(m))
}

#' Drop redundant members of correlated groups
#'
#' A strongest-pair-first variant of the caret::findCorrelation() heuristic,
#' in base stats: while any pair among the retained variables still exceeds
#' `threshold`, take the strongest such pair and drop whichever member has the
#' LARGER mean absolute correlation to the other retained variables. caret
#' instead scans columns in order of mean correlation, so the two can drop
#' different variables. An exact tie drops the later column.
#' NA (and non-finite) correlations are treated as 0, so a pair whose
#' correlation is unknown is never called redundant.
#'
#' @param corr_matrix A square, named correlation matrix.
#' @param threshold Absolute-correlation cutoff.
#' @return Character vector of dropped variable names, in the order dropped.
#' @noRd
corr_prune_redundant <- function(corr_matrix, threshold) {
  m <- abs(as.matrix(corr_matrix))
  m[!is.finite(m)] <- 0
  diag(m) <- 0

  keep <- rownames(m)
  dropped <- character(0)

  while (length(keep) > 1L) {
    sub <- m[keep, keep, drop = FALSE]
    if (!any(sub > threshold)) break

    # strongest remaining pair, upper triangle only
    ut <- sub
    ut[lower.tri(ut, diag = TRUE)] <- -Inf
    hits <- which(ut == max(ut), arr.ind = TRUE)
    i <- hits[1L, 1L]
    j <- hits[1L, 2L]

    # mean absolute correlation with the other retained variables
    avg <- rowSums(sub) / (length(keep) - 1L)

    # i < j, so an exact tie drops the later column (caret's behavior)
    loser <- if (avg[[i]] > avg[[j]]) keep[[i]] else keep[[j]]

    dropped <- c(dropped, loser)
    keep <- setdiff(keep, loser)
  }

  dropped
}

#' Is dichotomous (exactly 2 unique non-NA values)
#' @noRd
corr_is_dichotomous <- function(x) {
  ux <- unique(x[!is.na(x)])
  length(ux) == 2L
}

#' Is continuous (numeric with > 2 unique non-NA values)
#' @noRd
corr_is_continuous <- function(x) {
  x <- x[!is.na(x)]
  is.numeric(x) && (length(unique(x)) > 2L)
}
