# Install RSpectra if needed:
# install.packages("RSpectra")

suppressPackageStartupMessages({
  library(memoise)
  library(RSpectra)
})

###############################################################################
# Debug utilities (toggle DEBUG to enable/disable)
###############################################################################
DEBUG <- FALSE  # <- set TRUE to see detailed logs

ts_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

dbg <- function(..., .prefix = "[DEBUG]") {
  if (isTRUE(DEBUG)) cat(sprintf("%s %s ", .prefix, ts_now()), sprintf(...), "\n")
}

###############################################################################
# Core helpers
###############################################################################

#' Validate & Coerce the Input to a Numeric Matrix
#'
#' Accepts a matrix or data.frame and returns a numeric matrix without missing values.
#'
#' @param x A matrix or data frame.
#' @return A numeric matrix (no NAs).
validate_and_coerce_matrix <- function(x) {
  dbg("validate_and_coerce_matrix(): type=%s", paste(class(x), collapse=","))
  if (is.data.frame(x)) {
    non_num <- !vapply(x, is.numeric, logical(1))
    if (any(non_num)) {
      bad <- paste(names(x)[non_num], collapse=", ")
      stop(sprintf("All columns in the data.frame must be numeric to convert to a matrix. Non-numeric: %s", bad))
    }
    x <- as.matrix(x)
  } else if (!is.matrix(x)) {
    stop("Input must be a matrix or a data.frame.")
  }
  if (any(is.na(x))) stop("Input contains missing values (NAs).")
  storage.mode(x) <- "double"
  dbg("validate_and_coerce_matrix(): dim=[%d x %d], storage.mode=%s", nrow(x), ncol(x), storage.mode(x))
  x
}

#' Scale the Matrix
#'
#' Scales/centers the input matrix per the chosen option.
#'
#' @param mat Numeric matrix.
#' @param scale_input TRUE (center & scale), "center", "scale", or FALSE.
#' @param verbose Logical; if TRUE, prints a message about the scaling.
#' @return The transformed matrix (same dimension as input).
scale_matrix <- function(mat, scale_input = TRUE, verbose = FALSE) {
  dbg("scale_matrix(): incoming dim=[%d x %d], scale_input=%s", nrow(mat), ncol(mat), deparse(substitute(scale_input)))
  if (isTRUE(scale_input)) {
    if (verbose) cat("Applying centering and scaling...\n")
    mat <- scale(mat, center = TRUE, scale = TRUE)
  } else if (is.character(scale_input)) {
    if (identical(scale_input, "center")) {
      if (verbose) cat("Applying centering only...\n")
      mat <- scale(mat, center = TRUE, scale = FALSE)
    } else if (identical(scale_input, "scale")) {
      if (verbose) cat("Applying scaling only...\n")
      mat <- scale(mat, center = FALSE, scale = TRUE)
    } else {
      stop("Invalid 'scale_input'. Use TRUE, FALSE, 'center', or 'scale'.")
    }
  } else if (isFALSE(scale_input)) {
    if (verbose) cat("No scaling applied.\n")
    # no-op
  } else {
    stop("Invalid 'scale_input'. Use TRUE, FALSE, 'center', or 'scale'.")
  }
  dbg("scale_matrix(): outgoing dim=[%d x %d]", nrow(mat), ncol(mat))
  mat
}

#' Truncate SVD Results
#'
#' @param svd_result A list with components u, d, v (as in base::svd()).
#' @param n_singular_values Positive integer number of singular values/vectors to keep.
#' @return A list with singular_values, left_singular_vectors, right_singular_vectors.
truncate_svd <- function(svd_result, n_singular_values) {
  dbg("truncate_svd(): requested n=%s, available=%d",
      as.character(n_singular_values), length(svd_result$d))
  if (!is.numeric(n_singular_values) || n_singular_values <= 0 || n_singular_values %% 1 != 0) {
    stop("'n_singular_values' must be a positive integer.")
  }
  if (n_singular_values > length(svd_result$d)) {
    stop("'n_singular_values' exceeds the available singular values.")
  }
  out <- list(
    singular_values = svd_result$d[1:n_singular_values],
    left_singular_vectors  = svd_result$u[, 1:n_singular_values, drop = FALSE],
    right_singular_vectors = svd_result$v[, 1:n_singular_values, drop = FALSE]
  )
  dbg("truncate_svd(): U=[%d x %d], V=[%d x %d]",
      nrow(out$left_singular_vectors), ncol(out$left_singular_vectors),
      nrow(out$right_singular_vectors), ncol(out$right_singular_vectors))
  out
}

###############################################################################
# SVD engine
###############################################################################

#' Internal: Perform SVD
#'
#' Computes exact or approximate SVD with optional scaling and truncation.
#'
#' @param matrix_data Numeric matrix or numeric data.frame.
#' @param scale_input TRUE, FALSE, "center", or "scale".
#' @param n_singular_values Integer; default min(dim(matrix_data)).
#' @param svd_method "auto", "exact", or "approx".
#' @param svd_threshold If min(nrow, ncol) > threshold and svd_method=="auto", use "approx".
#' @param approx_args List of extra args for RSpectra::svds (e.g., tol=..., opts=list(...)).
#' @param verbose Logical; emit progress messages if TRUE.
#' @return List with singular_values, left_singular_vectors, right_singular_vectors.
perform_svd_internal <- function(matrix_data,
                                 scale_input = TRUE,
                                 n_singular_values = min(dim(matrix_data)),
                                 svd_method = c("auto", "exact", "approx"),
                                 svd_threshold = 100,
                                 approx_args = list(),
                                 verbose = FALSE) {
  dbg("perform_svd_internal(): START")
  
  # Validate & coerce
  X <- validate_and_coerce_matrix(matrix_data)
  
  # n_singular_values default/repair
  if (is.null(n_singular_values) || !is.numeric(n_singular_values) || n_singular_values <= 0) {
    dbg("n_singular_values invalid (%s) -> reset to min(dim(X))", as.character(n_singular_values))
    n_singular_values <- min(dim(X))
  }
  n_singular_values <- as.integer(n_singular_values)
  dbg("n_singular_values=%d, X dim=[%d x %d]", n_singular_values, nrow(X), ncol(X))
  
  # Apply scaling if requested
  Xs <- scale_matrix(X, scale_input, verbose = verbose)
  dbg("After scaling: dim(Xs)=[%d x %d]", nrow(Xs), ncol(Xs))
  
  # Choose method
  svd_method <- match.arg(svd_method)
  if (identical(svd_method, "auto")) {
    svd_method <- if (min(dim(Xs)) > svd_threshold) "approx" else "exact"
    if (verbose) cat(sprintf("Auto-selected SVD method: %s\n", svd_method))
  }
  dbg("SVD method selected: %s (threshold=%d, min_dim=%d)", svd_method, as.integer(svd_threshold), min(dim(Xs)))
  
  # Compute SVD
  if (identical(svd_method, "exact")) {
    if (verbose) cat("Computing exact SVD via base::svd()...\n")
    s <- svd(Xs)
    dbg("Exact SVD done: length(d)=%d, U=[%d x %d], V=[%d x %d]",
        length(s$d), nrow(s$u), ncol(s$u), nrow(s$v), ncol(s$v))
    res <- truncate_svd(s, min(n_singular_values, length(s$d)))
    dbg("perform_svd_internal(): END (exact)")
    return(res)
  }
  
  if (identical(svd_method, "approx")) {
    k <- n_singular_values
    if (k >= min(dim(Xs))) {
      if (verbose) cat("Requested components >= min(dim); falling back to exact SVD.\n")
      dbg("Approx requested with k=%d >= min_dim=%d -> fallback to exact", k, min(dim(Xs)))
      s <- svd(Xs)
      res <- truncate_svd(s, min(k, length(s$d)))
      dbg("perform_svd_internal(): END (approx->exact fallback)")
      return(res)
    }
    if (verbose) cat("Computing approximate SVD via RSpectra::svds()...\n")
    dbg("Calling RSpectra::svds() with k=%d and args: %s", k, paste(names(approx_args), collapse=","))
    args_list <- c(list(A = Xs, k = k), approx_args)
    s <- do.call(RSpectra::svds, args_list)
    
    # Ensure descending order
    ord <- order(s$d, decreasing = TRUE)
    s$u <- s$u[, ord, drop = FALSE]
    s$d <- s$d[ord]
    s$v <- s$v[, ord, drop = FALSE]
    dbg("Approx SVD done: length(d)=%d, U=[%d x %d], V=[%d x %d]",
        length(s$d), nrow(s$u), ncol(s$u), nrow(s$v), ncol(s$v))
    
    res <- list(
      singular_values = s$d,
      left_singular_vectors  = s$u,
      right_singular_vectors = s$v
    )
    dbg("perform_svd_internal(): END (approx)")
    return(res)
  }
  
  stop("Unknown 'svd_method'.")
}

# Create a persistent memoised wrapper ONCE (so the cache isn't recreated every call).
.perform_svd_internal_memo <- memoise(perform_svd_internal)

###############################################################################
# Public API
###############################################################################

#' Main Function: fs_svd
#'
#' Computes the SVD of a matrix with options for scaling, truncation,
#' approximate computation, and optional memoisation of results.
#'
#' @param matrix_data Matrix or data.frame (numeric).
#' @param scale_input TRUE, FALSE, "center", or "scale".
#' @param n_singular_values Integer; default min(dim(matrix_data)).
#' @param svd_method "auto" (default), "exact", or "approx".
#' @param svd_threshold Numeric; if min(dim) > threshold and auto, use approx (default 100).
#' @param approx_args List of extra args for RSpectra::svds.
#' @param verbose Logical; print progress messages.
#' @param memoise_result Logical; if TRUE (default), use a persistent memoised wrapper.
#' @return A list with:
#'   \itemize{
#'     \item \code{singular_values}
#'     \item \code{left_singular_vectors}
#'     \item \code{right_singular_vectors}
#'   }
#' @examples
#' set.seed(123)
#' matrix_data <- matrix(rnorm(9), nrow = 3)
#' res <- fs_svd(matrix_data, scale_input = TRUE, n_singular_values = 2)
#' cat("Singular values:", res$singular_values, "\n")
#' print(res$left_singular_vectors)
#' print(res$right_singular_vectors)
fs_svd <- function(matrix_data,
                   scale_input = TRUE,
                   n_singular_values = min(dim(as.matrix(matrix_data))),
                   svd_method = c("auto", "exact", "approx"),
                   svd_threshold = 100,
                   approx_args = list(),
                   verbose = FALSE,
                   memoise_result = TRUE) {
  dbg("fs_svd(): memoise_result=%s", as.character(memoise_result))
  fun <- if (isTRUE(memoise_result)) .perform_svd_internal_memo else perform_svd_internal
  
  res <- fun(matrix_data = matrix_data,
             scale_input = scale_input,
             n_singular_values = n_singular_values,
             svd_method = svd_method,
             svd_threshold = svd_threshold,
             approx_args = approx_args,
             verbose = verbose)
  
  dbg("fs_svd(): RETURN sv.len=%d, U=[%d x %d], V=[%d x %d]",
      length(res$singular_values),
      nrow(res$left_singular_vectors),  ncol(res$left_singular_vectors),
      nrow(res$right_singular_vectors), ncol(res$right_singular_vectors))
  res
}
