# Correlation-based feature selection.
# polycor (polychoric) and foreach/doParallel (parallel point-biserial) are
# Suggests and are only touched at run time. The redundancy pruning below is
# built from base stats only, so caret is not needed here.

#' Correlation-based feature selection
#'
#' Flags variable pairs whose absolute correlation exceeds `threshold` and,
#' by default, reduces each correlated group to a single representative.
#'
#' @details
#' This is an unsupervised redundancy filter: it looks only at how the
#' variables relate to one another and never at an outcome. When two variables
#' are near-interchangeable it therefore cannot prefer the one that predicts
#' better; it keeps whichever is least entangled with the rest of the data. Use
#' \code{fs_boruta()} when the choice inside a correlated group should be
#' driven by importance for a target.
#'
#' Pruning is greedy rather than group-wise: while any retained pair still
#' exceeds \code{threshold}, the strongest such pair is taken and the member
#' with the larger mean absolute correlation to the other retained variables is
#' dropped. No two retained variables end up with a computable correlation
#' above \code{threshold}, but the surviving set is not guaranteed to be the
#' smallest one with that property, and it depends on the order in which pairs
#' are resolved.
#'
#' Correlations that cannot be computed come back as \code{NA}. Those pairs are
#' never flagged and never pruned -- an unknown correlation is treated as no
#' evidence of redundancy -- and a warning reports how many there are. A
#' message (not a warning) is emitted when no pair at all exceeds
#' \code{threshold}, whatever \code{verbose} is set to.
#'
#' @param data A data frame or matrix with at least 2 columns and unique
#'   column names (a correlation matrix with duplicated dimnames is ambiguous,
#'   so duplicates are rejected rather than silently resolved to the first
#'   match). For \code{"pearson"}, \code{"spearman"},
#'   \code{"kendall"}: all columns must be numeric. For \code{"polychoric"}:
#'   all columns must be ordered factors. For \code{"pointbiserial"}: columns
#'   may be numeric (continuous) or dichotomous (exactly 2 unique non-NA values).
#' @param threshold Numeric between 0 and 1. Pairs with |correlation| > threshold
#'   are flagged as redundant. Required; there is no default.
#' @param method One of \code{"pearson"} (default), \code{"spearman"},
#'   \code{"kendall"}, \code{"polychoric"}, \code{"pointbiserial"}.
#'   Point-biserial correlations are computed as the Pearson correlation
#'   between the continuous variable and a 0/1 indicator of the dichotomous
#'   variable (1 for the second sorted unique value, e.g. the second factor
#'   level), which is the definition of the point-biserial coefficient. Only
#'   continuous-dichotomous pairs are defined under that method: cells for
#'   continuous-continuous and dichotomous-dichotomous pairs stay \code{NA},
#'   so those pairs can never be flagged.
#' @param prune Logical. If \code{TRUE} (default), \code{selected} is the
#'   reduced non-redundant set: variables are dropped one at a time until no
#'   two retained variables correlate above \code{threshold}, each step
#'   dropping the member of the strongest remaining pair with the HIGHER mean
#'   absolute correlation to the other retained variables (the
#'   \code{caret::findCorrelation()} heuristic, implemented here with base
#'   stats). Variables that were never flagged are always retained.
#'   If \code{FALSE}, \code{selected} is every variable appearing in at least
#'   one flagged pair, i.e. the redundant set, and nothing is dropped.
#' @param na.rm Logical. If \code{TRUE}, missing values are removed pairwise
#'   for \code{"pearson"}/\code{"spearman"}/\code{"kendall"}, per pair
#'   (complete observations within each variable pair) for
#'   \code{"pointbiserial"}, and casewise (complete cases across all columns)
#'   for \code{"polychoric"}. If \code{FALSE} (default), missing values
#'   propagate NA into the affected correlations, except for
#'   \code{"polychoric"}, which stops with an error when missing values are
#'   present (silently deleting cases would contradict the behavior of the
#'   other methods). Default \code{FALSE}.
#' @param sample_frac Numeric in (0, 1]. Fraction of rows sampled without
#'   replacement (rounded up, never below one row) before computing
#'   correlations. Default \code{1} (no sampling).
#' @param output_format \code{"matrix"} (default) or \code{"data.frame"} for the
#'   correlation matrix stored in \code{details$corr_matrix}.
#' @param diag_value Single numeric value, or \code{NA}, assigned to the
#'   diagonal of the reported correlation matrix. It is cosmetic: the diagonal
#'   never takes part in flagging, scoring, or pruning. Default \code{0}.
#' @param seed Optional integer seed for reproducible sampling, applied
#'   locally; the previous RNG state is restored afterwards. Default
#'   \code{NULL} (never seeds).
#' @param verbose Logical. Emit progress messages (row sampling, the
#'   correlation method used, and any variables dropped by pruning). Default
#'   \code{FALSE}.
#' @param parallel Logical. Use parallel processing (via the suggested foreach
#'   and doParallel packages) for point-biserial computations. Ignored by every
#'   other method, all of which are single-call. Default \code{FALSE}
#'   (sequential).
#' @param n_cores Whole number >= 1. Number of workers if \code{parallel = TRUE}
#'   and \code{method = "pointbiserial"}; requests are capped at the detected
#'   core count. Default \code{2}.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector. With \code{prune = TRUE}, every variable
#'     that survives pruning (all columns except \code{details$dropped}), no
#'     two of which have a computable absolute correlation above
#'     \code{threshold}. With \code{prune = FALSE}, every variable appearing in
#'     at least one flagged pair (both members of each pair), which is the
#'     redundant set rather than the set to keep.}
#'   \item{scores}{Named numeric vector giving each variable's maximum absolute
#'     correlation with any other variable (NA when no correlation with that
#'     variable could be computed).}
#'   \item{method}{\code{paste0("correlation_", method)}, e.g.
#'     "correlation_pearson".}
#'   \item{task}{\code{NA_character_}; correlation filtering is unsupervised.}
#'   \item{model}{NULL; no model is fitted.}
#'   \item{details}{A list with `corr_matrix` (the correlation matrix, with the
#'     diagonal set to \code{diag_value}, reshaped to long form with columns
#'     Var1, Var2, Correlation when \code{output_format = "data.frame"}),
#'     `pairs` (a data.frame of the flagged pairs with columns Var1, Var2,
#'     Correlation, ordered by decreasing absolute correlation and unaffected
#'     by \code{output_format}), `dropped` (variables removed by pruning, in
#'     the order they were dropped; empty when \code{prune = FALSE}),
#'     `redundant` (every variable in at least one flagged pair, i.e.
#'     \code{selected} as it would be under \code{prune = FALSE}), and
#'     `n_features` (the number of variables considered).}
#'   \item{call}{The matched call.}
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
#' # keep the legacy view: both members of every flagged pair
#' fs_correlation(d, threshold = 0.9, prune = FALSE)$selected
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

  # diag_value: allow numeric scalar or any single NA
  if (!(length(diag_value) == 1L &&
        (is.numeric(diag_value) || is.na(diag_value)))) {
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

    # Remember whatever backend the caller had registered, so exiting restores
    # their session rather than forcing it sequential.
    prev_backend <- foreach::getDoParName()

    cl <- parallel::makeCluster(n_cores)
    # Register the teardown before registerDoParallel() can throw: otherwise a
    # failure there leaves the cluster running with no reference to stop it.
    on.exit({
      try(parallel::stopCluster(cl), silent = TRUE)
      if (is.null(prev_backend) || identical(prev_backend, "doSEQ")) {
        try(foreach::registerDoSEQ(), silent = TRUE)
      }
    }, add = TRUE)
    doParallel::registerDoParallel(cl)

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
#' Reimplements the caret::findCorrelation() heuristic with base stats: while
#' any pair among the retained variables still exceeds `threshold`, take the
#' strongest such pair and drop whichever member has the LARGER mean absolute
#' correlation to the other retained variables. The representative that
#' survives each group is therefore the member with the LOWEST mean absolute
#' correlation to everything else. An exact tie drops the later column.
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
