# Correlation-based feature selection.
# polycor (polychoric) and foreach/doParallel (parallel point-biserial) are
# Suggests and are only touched at run time.

#' Correlation-based feature selection
#'
#' Selects features from a dataset based on pairwise correlation.
#'
#' @param data A data frame or matrix. For \code{"pearson"}, \code{"spearman"},
#'   \code{"kendall"}: all columns must be numeric. For \code{"polychoric"}:
#'   all columns must be ordered factors. For \code{"pointbiserial"}: columns
#'   may be numeric (continuous) or dichotomous (exactly 2 unique non-NA values).
#' @param threshold Numeric in [0, 1]. Pairs with |correlation| > threshold are selected.
#' @param method One of \code{"pearson"} (default), \code{"spearman"},
#'   \code{"kendall"}, \code{"polychoric"}, \code{"pointbiserial"}.
#'   Point-biserial correlations are computed as the Pearson correlation
#'   between the continuous variable and a 0/1 indicator of the dichotomous
#'   variable (1 for the second sorted unique value, e.g. the second factor
#'   level), which is the definition of the point-biserial coefficient.
#' @param na.rm Logical. If \code{TRUE}, missing values are removed pairwise
#'   for \code{"pearson"}/\code{"spearman"}/\code{"kendall"}, per pair
#'   (complete observations within each variable pair) for
#'   \code{"pointbiserial"}, and casewise (complete cases across all columns)
#'   for \code{"polychoric"}. If \code{FALSE} (default), missing values
#'   propagate NA into the affected correlations, except for
#'   \code{"polychoric"}, which stops with an error when missing values are
#'   present (silently deleting cases would contradict the behaviour of the
#'   other methods). Default \code{FALSE}.
#' @param parallel Logical. Use parallel processing (via the suggested foreach
#'   and doParallel packages) for point-biserial computations. Default \code{FALSE}.
#' @param n_cores Integer >= 1. Number of workers if \code{parallel = TRUE};
#'   requests are capped at the detected core count. Default \code{2}.
#' @param sample_frac Numeric in (0, 1]. Fraction of rows to sample before computing
#'   correlations. Default \code{1} (no sampling).
#' @param output_format \code{"matrix"} (default) or \code{"data.frame"} for the
#'   correlation matrix.
#' @param diag_value Value to assign to the diagonal of the correlation matrix.
#'   Default \code{0}.
#' @param no_vars_message Message printed if no variable pairs exceed \code{threshold}.
#' @param seed Optional integer seed for reproducible sampling, applied
#'   locally; the previous RNG state is restored afterwards. Default
#'   \code{NULL} (never seeds).
#' @param verbose Logical. Print progress messages. Default \code{FALSE}.
#'
#' @return A list with:
#' \describe{
#'   \item{corr_matrix}{Correlation matrix (matrix or data frame, per \code{output_format}).}
#'   \item{selected_vars}{Character vector of all variables that appear in at
#'     least one pair with |r| > threshold. Note that this includes BOTH
#'     members of each high-correlation pair, i.e. the redundant set; the
#'     function does not choose which member of a pair to keep or drop (a
#'     keep/drop redesign is deferred).}
#' }
#'
#' @examples
#' d <- data.frame(
#'   a = c(1, 2, 3, 4, 5, 6),
#'   b = c(2, 4, 6, 8, 10, 12),
#'   c = c(1.5, 0.9, 2.1, 0.4, 1.1, 0.8)
#' )
#' res <- fs_correlation(d, threshold = 0.9)
#' res$selected_vars
#' @export
fs_correlation <- function(data, threshold, method = "pearson", na.rm = FALSE,
                           parallel = FALSE, n_cores = 2, sample_frac = 1,
                           output_format = "matrix", diag_value = 0,
                           no_vars_message = "No variables meet the correlation threshold.",
                           seed = NULL, verbose = FALSE) {
  # Validate inputs
  corr_validate_inputs(
    data            = data,
    threshold       = threshold,
    method          = method,
    na.rm           = na.rm,
    parallel        = parallel,
    n_cores         = n_cores,
    sample_frac     = sample_frac,
    output_format   = output_format,
    diag_value      = diag_value,
    no_vars_message = no_vars_message,
    verbose         = verbose
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

  # Find high-correlation pairs
  high_corr_idx <- corr_find_high_correlation(corr_matrix, threshold)

  if (nrow(high_corr_idx) == 0L) {
    message(no_vars_message)
    selected_vars <- character(0)
  } else {
    selected_vars <- unique(c(
      rownames(corr_matrix)[high_corr_idx[, 1]],
      colnames(corr_matrix)[high_corr_idx[, 2]]
    ))
  }

  # Optional reshape
  if (identical(output_format, "data.frame")) {
    cm_df <- as.data.frame(as.table(corr_matrix), stringsAsFactors = FALSE)
    names(cm_df) <- c("Var1", "Var2", "Correlation")
    corr_matrix <- cm_df
  }

  list(corr_matrix = corr_matrix, selected_vars = selected_vars)
}

# ----------------------------- Helpers --------------------------------------

#' Validate fs_correlation() inputs
#' @noRd
corr_validate_inputs <- function(data, threshold, method, na.rm, parallel,
                                 n_cores, sample_frac, output_format,
                                 diag_value, no_vars_message, verbose) {
  assert_data_frame(data, "data", allow_matrix = TRUE)

  if (ncol(data) < 2L) {
    stop("`data` must have at least 2 columns to compute correlations.")
  }

  assert_string(method, "method")
  valid_methods <- c("pearson", "spearman", "kendall", "polychoric", "pointbiserial")
  if (!(method %in% valid_methods)) {
    stop("Invalid `method`. Choose one of: ", paste(valid_methods, collapse = ", "), ".")
  }

  assert_number(threshold, "threshold", lower = 0, upper = 1)
  assert_flag(na.rm, "na.rm")
  assert_flag(parallel, "parallel")
  assert_count(n_cores, "n_cores")

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

  assert_string(no_vars_message, "no_vars_message")
  assert_flag(verbose, "verbose")

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

    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      try(parallel::stopCluster(cl), silent = TRUE)
      foreach::registerDoSEQ()
    }, add = TRUE)

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
