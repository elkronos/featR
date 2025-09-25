#' Correlation-based Feature Selection
#'
#' Selects features from a dataset based on pairwise correlation.
#'
#' @param data A data frame or matrix. For \code{"pearson"}, \code{"spearman"}, \code{"kendall"}:
#'   all columns must be numeric. For \code{"polychoric"}: all columns must be ordered factors.
#'   For \code{"pointbiserial"}: columns may be numeric (continuous) or dichotomous (exactly 2 values).
#' @param threshold Numeric in [0, 1]. Pairs with |correlation| > threshold are selected.
#' @param method One of \code{"pearson"} (default), \code{"spearman"}, \code{"kendall"},
#'   \code{"polychoric"}, \code{"pointbiserial"}.
#' @param na.rm Logical. If \code{TRUE}, correlations are computed with pairwise complete observations
#'   (or the closest equivalent for the method). Default \code{FALSE}.
#' @param parallel Logical. Use parallel processing for point-biserial computations. Default \code{FALSE}.
#' @param n_cores Integer >= 1. Number of cores if \code{parallel=TRUE}. Default \code{2}.
#' @param sample_frac Numeric in (0, 1]. Fraction of rows to sample before computing correlations.
#'   Default \code{1} (no sampling).
#' @param output_format \code{"matrix"} (default) or \code{"data.frame"} for the correlation matrix.
#' @param diag_value Value to assign to the diagonal of the correlation matrix. Default \code{0}.
#' @param no_vars_message Message printed if no variable pairs exceed \code{threshold}.
#' @param seed Optional integer seed for reproducible sampling. Default \code{NULL}.
#' @param verbose Logical. Print progress messages. Default \code{FALSE}.
#'
#' @return A list with:
#' \describe{
#'   \item{corr_matrix}{Correlation matrix (matrix or data frame, per \code{output_format}).}
#'   \item{selected_vars}{Character vector of variables involved in pairs with |r| > threshold.}
#' }
#'
#' @importFrom stats cor
#' @importFrom utils combn
#' @export
fs_correlation <- function(data, threshold, method = "pearson", na.rm = FALSE,
                           parallel = FALSE, n_cores = 2, sample_frac = 1,
                           output_format = "matrix", diag_value = 0,
                           no_vars_message = "No variables meet the correlation threshold.",
                           seed = NULL, verbose = FALSE) {
  # Validate inputs
  validate_inputs(data, threshold, method, output_format, sample_frac, n_cores)
  
  # Sample rows if requested (guarantee at least 1 row when possible)
  data <- sample_data(data, sample_frac, seed, verbose)
  
  # Load method-specific packages (fail early with clear error)
  load_required_packages(method, parallel)
  
  # Calculate the correlation matrix
  corr_matrix <- calculate_correlation(
    data        = data,
    method      = method,
    na.rm       = na.rm,
    parallel    = parallel,
    n_cores     = n_cores,
    verbose     = verbose
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
  
  # Find high-correlation pairs
  high_corr_idx <- find_high_correlation(corr_matrix, threshold)
  
  if (nrow(high_corr_idx) == 0) {
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

#' Validate Function Inputs
#' @noRd
validate_inputs <- function(data, threshold, method, output_format, sample_frac, n_cores) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("`data` must be a data frame or matrix.")
  }
  if (ncol(data) < 2L) {
    stop("`data` must have at least 2 columns to compute correlations.")
  }
  
  valid_methods <- c("pearson", "spearman", "kendall", "polychoric", "pointbiserial")
  if (!(method %in% valid_methods)) {
    stop("Invalid `method`. Choose one of: ", paste(valid_methods, collapse = ", "), ".")
  }
  
  if (!(is.numeric(threshold) && length(threshold) == 1L && threshold >= 0 && threshold <= 1)) {
    stop("`threshold` must be a single numeric value in [0, 1].")
  }
  
  if (!(output_format %in% c("matrix", "data.frame"))) {
    stop("`output_format` must be 'matrix' or 'data.frame'.")
  }
  
  if (!(is.numeric(sample_frac) && length(sample_frac) == 1L && sample_frac > 0 && sample_frac <= 1)) {
    stop("`sample_frac` must be a single numeric value in (0, 1].")
  }
  
  if (!(is.numeric(n_cores) && length(n_cores) == 1L && n_cores >= 1)) {
    stop("`n_cores` must be a numeric value >= 1.")
  }
  
  # Column-type checks per method
  if (method %in% c("pearson", "spearman", "kendall")) {
    if (!all(vapply(as.data.frame(data), is.numeric, logical(1)))) {
      stop("All columns must be numeric for method '", method, "'.")
    }
  } else if (identical(method, "pointbiserial")) {
    ok <- vapply(as.data.frame(data), function(x) is.numeric(x) || is_dichotomous(x), logical(1))
    if (!all(ok)) {
      stop("For 'pointbiserial', all columns must be numeric or dichotomous (exactly 2 unique non-NA values).")
    }
    # Require at least one continuous and one dichotomous variable; otherwise no valid pairs exist
    if (!any(vapply(as.data.frame(data), is_continuous, logical(1))) ||
        !any(vapply(as.data.frame(data), is_dichotomous, logical(1)))) {
      warning("No valid continuous–dichotomous pairs found for 'pointbiserial'. Result will be an NA matrix.")
    }
  } else if (identical(method, "polychoric")) {
    if (!all(vapply(as.data.frame(data), is.ordered, logical(1)))) {
      stop("All columns must be ordered factors for method 'polychoric'.")
    }
  }
}

#' Load Required Packages
#' @noRd
load_required_packages <- function(method, parallel) {
  if (identical(method, "polychoric")) {
    if (!requireNamespace("polycor", quietly = TRUE)) {
      stop("Package 'polycor' is required for 'polychoric'. Please install it.")
    }
  } else if (identical(method, "pointbiserial")) {
    if (!requireNamespace("ltm", quietly = TRUE)) {
      stop("Package 'ltm' is required for 'pointbiserial'. Please install it.")
    }
    if (isTRUE(parallel)) {
      if (!requireNamespace("doParallel", quietly = TRUE)) {
        stop("Package 'doParallel' is required for parallel processing. Please install it.")
      }
      if (!requireNamespace("foreach", quietly = TRUE)) {
        stop("Package 'foreach' is required for parallel processing. Please install it.")
      }
    }
  }
}

#' Sample Data (row-wise)
#' @noRd
sample_data <- function(data, sample_frac, seed = NULL, verbose = FALSE) {
  # Early exit
  if (identical(sample_frac, 1)) return(data)
  
  n_rows <- nrow(data)
  if (is.null(n_rows) || n_rows < 1L) {
    stop("`data` must have at least one row.")
  }
  
  if (!is.null(seed)) set.seed(as.integer(seed))
  size <- max(1L, min(n_rows, ceiling(sample_frac * n_rows)))
  idx  <- sample.int(n_rows, size = size, replace = FALSE)
  
  if (isTRUE(verbose)) message(sprintf("Sampling %d/%d rows (%.1f%%).", size, n_rows, 100 * size / n_rows))
  
  data[idx, , drop = FALSE]
}

#' Calculate Correlation Matrix
#' @noRd
calculate_correlation <- function(data, method, na.rm, parallel, n_cores, verbose) {
  if (identical(method, "pointbiserial")) {
    return(calculate_pointbiserial_correlation(data, na.rm, parallel, n_cores, verbose))
  } else if (identical(method, "polychoric")) {
    return(calculate_polychoric_correlation(data, verbose))
  } else {
    use_opt <- if (isTRUE(na.rm)) "pairwise.complete.obs" else "everything"
    if (isTRUE(verbose)) message("Calculating ", method, " correlation matrix (use = '", use_opt, "').")
    data <- as.data.frame(data)
    return(stats::cor(data, method = method, use = use_opt))
  }
}

#' Point-Biserial Correlation Matrix
#' @noRd
#' @importFrom foreach foreach %dopar%
calculate_pointbiserial_correlation <- function(data, na.rm, parallel, n_cores, verbose) {
  data <- as.data.frame(data)
  p    <- ncol(data)
  
  cm <- matrix(NA_real_, nrow = p, ncol = p)
  colnames(cm) <- colnames(data)
  rownames(cm) <- colnames(data)
  
  cont_idx <- which(vapply(data, is_continuous, logical(1)))
  dich_idx <- which(vapply(data, is_dichotomous, logical(1)))
  
  if (length(cont_idx) == 0L || length(dich_idx) == 0L) {
    if (isTRUE(verbose)) message("No continuous–dichotomous pairs available; returning NA matrix.")
    return(cm)
  }
  
  # Build all i (continuous) x j (dichotomous) pairs (exclude i==j just in case)
  pairs <- expand.grid(i = cont_idx, j = dich_idx, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$i != pairs$j, , drop = FALSE]
  
  if (nrow(pairs) == 0L) {
    if (isTRUE(verbose)) message("No valid distinct pairs; returning NA matrix.")
    return(cm)
  }
  
  # Compute with or without parallelism
  if (isTRUE(parallel)) {
    n_cores <- max(1L, as.integer(n_cores))
    n_cores <- min(n_cores, parallel::detectCores(logical = TRUE))
    if (isTRUE(verbose)) message("Running point-biserial in parallel on ", n_cores, " cores.")
    
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
    }, add = TRUE)
    
    `%dopar%` <- foreach::`%dopar%`
    
    res <- foreach::foreach(k = seq_len(nrow(pairs)), .combine = rbind, .packages = "ltm") %dopar% {
      i <- pairs$i[k]; j <- pairs$j[k]
      xi <- data[[i]]
      xj <- data[[j]]
      # Robust: treat y as 2-level factor even if not coded 0/1
      grp <- if (is.factor(xj)) stats::droplevels(xj) else factor(xj)
      r   <- tryCatch(
        ltm::biserial.cor(x = xi, y = grp, use = if (na.rm) "complete.obs" else "all.obs"),
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
    if (ncol(res) == 3L && !all(c("i","j","r") %in% names(res))) {
      names(res) <- c("i","j","r")
    }
    if (!all(c("i","j","r") %in% names(res))) {
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
    if (isTRUE(verbose)) message("Running point-biserial sequentially.")
    for (k in seq_len(nrow(pairs))) {
      i <- pairs$i[k]; j <- pairs$j[k]
      xi <- data[[i]]
      xj <- data[[j]]
      grp <- if (is.factor(xj)) stats::droplevels(xj) else factor(xj)
      r <- tryCatch(
        ltm::biserial.cor(x = xi, y = grp, use = if (na.rm) "complete.obs" else "all.obs"),
        error = function(e) NA_real_
      )
      cm[i, j] <- r
      cm[j, i] <- r
    }
  }
  
  cm
}

#' Polychoric Correlation Matrix
#' @noRd
calculate_polychoric_correlation <- function(data, verbose) {
  if (isTRUE(verbose)) message("Calculating polychoric correlation matrix via polycor::hetcor().")
  # polycor::hetcor() returns a list with element `correlations`
  res <- polycor::hetcor(as.data.frame(data))
  mat <- res$correlations
  # Ensure square with dimnames
  if (is.null(colnames(mat))) colnames(mat) <- make.names(seq_len(ncol(mat)))
  if (is.null(rownames(mat))) rownames(mat) <- colnames(mat)
  mat
}

#' Find High-Correlation Pairs (upper triangle only)
#' @noRd
find_high_correlation <- function(corr_matrix, threshold) {
  # Work only on numeric entries
  m <- as.matrix(corr_matrix)
  # Identify indices with |r| > threshold and not NA
  idx <- which(!is.na(m) & abs(m) > threshold, arr.ind = TRUE)
  # keep upper triangle to avoid duplicates
  idx <- idx[idx[, 1] < idx[, 2], , drop = FALSE]
  idx
}

#' Is Dichotomous (exactly 2 unique non-NA values)
#' @noRd
is_dichotomous <- function(x) {
  ux <- unique(x[!is.na(x)])
  length(ux) == 2L
}

#' Is Continuous (numeric with > 2 unique non-NA values)
#' @noRd
is_continuous <- function(x) {
  x <- x[!is.na(x)]
  is.numeric(x) && (length(unique(x)) > 2L)
}
