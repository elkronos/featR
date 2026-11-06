# Singular value decomposition with optional scaling, truncation, and an
# approximate solver (RSpectra) for large matrices.

#' Validate and coerce the input to a numeric matrix
#'
#' Accepts a matrix or data.frame and returns a double matrix with at least
#' one row and one column and no non-finite values (NA, NaN, Inf).
#'
#' @param x A matrix or data frame.
#' @return A numeric matrix.
#' @noRd
svd_coerce_matrix <- function(x) {
  if (is.data.frame(x)) {
    non_num <- !vapply(x, is.numeric, logical(1L))
    if (any(non_num)) {
      stop(
        sprintf(
          paste0(
            "All columns in the data.frame must be numeric to convert to a ",
            "matrix. Non-numeric: %s"
          ),
          paste(names(x)[non_num], collapse = ", ")
        ),
        call. = FALSE
      )
    }
    x <- as.matrix(x)
  } else if (!is.matrix(x)) {
    stop("Input must be a matrix or a data.frame.", call. = FALSE)
  }

  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop("Input matrix must have at least one row and one column.",
         call. = FALSE)
  }

  storage.mode(x) <- "double"

  if (!all(is.finite(x))) {
    stop("Input contains non-finite values (NA/NaN/Inf).", call. = FALSE)
  }

  x
}

#' Center and/or scale the matrix per `scale_input`
#'
#' When scaling (dividing by column standard deviations) is requested,
#' zero-variance columns are rejected up front with an actionable error
#' instead of surfacing later as a generic non-finite-values failure. A column
#' counts as zero-variance when its standard deviation is at most
#' `1000 * .Machine$double.eps` times its largest absolute value, so the test
#' does not depend on the column's units.
#'
#' @param mat Numeric matrix.
#' @param scale_input TRUE (center and scale), "center", "scale", or FALSE.
#' @param verbose Single flag; emit progress messages.
#' @return The transformed matrix (same dimensions as the input).
#' @noRd
svd_scale_matrix <- function(mat, scale_input = TRUE, verbose = FALSE) {
  if (isTRUE(scale_input)) {
    do_center <- TRUE
    do_scale <- TRUE
  } else if (isFALSE(scale_input)) {
    do_center <- FALSE
    do_scale <- FALSE
  } else if (identical(scale_input, "center")) {
    do_center <- TRUE
    do_scale <- FALSE
  } else if (identical(scale_input, "scale")) {
    do_center <- FALSE
    do_scale <- TRUE
  } else {
    stop("Invalid 'scale_input'. Use TRUE, FALSE, 'center', or 'scale'.",
         call. = FALSE)
  }

  if (!do_center && !do_scale) {
    if (verbose) message("No scaling applied.")
    return(mat)
  }

  if (do_scale) {
    # The tolerance is relative to each column's magnitude: an absolute cut-off
    # would reject legitimately small-scale columns (values around 1e-9 have
    # standard deviations far below sqrt(eps)) while accepting constant
    # columns whose round-off leaves a tiny non-zero sd. The spread is
    # measured on col / max(abs(col)), so stats::sd() overflow (|x| beyond
    # ~1e154) or underflow (|x| below ~1e-154) cannot fake a zero variance.
    spreads <- lapply(seq_len(ncol(mat)), function(j) safe_col_spread(mat[, j]))
    rel_sds <- vapply(spreads, `[[`, numeric(1L), "rel")
    zero_var <- which(rel_sds <= 1000 * .Machine$double.eps)
    if (length(zero_var) > 0L) {
      offenders <- colnames(mat)[zero_var]
      if (is.null(colnames(mat))) {
        offenders <- paste0("column ", zero_var)
      }
      shown <- utils::head(offenders, 5L)
      stop(
        sprintf(
          paste0(
            "Cannot scale: %d column(s) have zero (or undefined) variance: ",
            "%s%s. Remove them or use scale_input = 'center' or ",
            "scale_input = FALSE."
          ),
          length(zero_var),
          paste(shown, collapse = ", "),
          if (length(zero_var) > 5L) ", ..." else ""
        ),
        call. = FALSE
      )
    }
  }

  if (do_scale) {
    # Both scale() modes (sd after centering, root mean square without) are
    # invariant to multiplying a column by a constant, so a column whose
    # variance is not representable in double precision is first divided by
    # its largest absolute value; the scaled result is unchanged.
    extreme <- which(!vapply(spreads, `[[`, logical(1L), "representable"))
    for (j in extreme) {
      mat[, j] <- mat[, j] / spreads[[j]]$scale
    }
  }

  if (verbose) {
    message(
      if (do_center && do_scale) {
        "Applying centering and scaling..."
      } else if (do_center) {
        "Applying centering only..."
      } else {
        "Applying scaling only..."
      }
    )
  }

  mat <- scale(mat, center = do_center, scale = do_scale)

  if (!all(is.finite(mat))) {
    stop("Scaling produced non-finite values (NA/NaN/Inf).", call. = FALSE)
  }

  mat
}

#' Truncate an SVD result to the leading singular triplets
#'
#' @param svd_result A list with components u, d, v (as in base::svd()).
#' @param n_singular_values Positive integer count of singular values and
#'   vectors to keep; must not exceed the number available.
#' @return A list with singular_values, left_singular_vectors,
#'   right_singular_vectors.
#' @noRd
svd_truncate <- function(svd_result, n_singular_values) {
  n <- assert_count(n_singular_values, "n_singular_values", lower = 1L)
  if (n > length(svd_result$d)) {
    stop("'n_singular_values' exceeds the available singular values.",
         call. = FALSE)
  }

  keep <- seq_len(n)
  list(
    singular_values = svd_result$d[keep],
    left_singular_vectors = svd_result$u[, keep, drop = FALSE],
    right_singular_vectors = svd_result$v[, keep, drop = FALSE]
  )
}

#' Singular value decomposition with optional scaling and truncation
#'
#' Computes the singular value decomposition (SVD) of a numeric matrix,
#' optionally centering and scaling it first and keeping only the leading
#' singular values and vectors. The SVD factors a matrix into
#' `u %*% diag(d) %*% t(v)`; the leading terms give the best low-rank
#' approximation of the matrix in the least-squares sense.
#'
#' @details
#' Use it for a low-rank summary of a numeric matrix: compression, denoising,
#' or a latent-factor representation. The components are linear combinations
#' of every input column, so nothing is dropped and the components cannot be
#' read as individual features; use one of featR's selection functions when
#' you need to name the features you keep. [fs_pca()] covers the same ground
#' with labeled output and variance-explained figures; `fs_svd()` is the
#' lower-level decomposition.
#'
#' The exact solver is `base::svd()`. The approximate solver is
#' `RSpectra::svds()` (from \pkg{RSpectra}, a Suggests dependency needed only
#' when that solver runs), and it can only return fewer than `min(dim(x))`
#' singular values. The default `n_singular_values = NULL` asks for all
#' `min(dim(x))` of them and so always takes the exact path; request fewer to
#' make the approximate solver available.
#'
#' With `svd_method = "auto"`, the approximate solver runs only when
#' `n_singular_values < min(dim(x))` and `min(dim(x)) > svd_threshold`.
#' Otherwise the exact solver runs, and a message explains why when a large
#' matrix ends up on the exact path because all singular values were
#' requested. `svd_method = "approx"` with `n_singular_values` equal to
#' `min(dim(x))` is not an error: it falls back to the exact solver with a
#' message. If `RSpectra::svds()` converges on fewer singular values than
#' requested, the call warns and recomputes them with the exact solver.
#'
#' Centering and scaling happen before the decomposition, so the returned
#' values and vectors factorize the transformed matrix. With the default
#' `scale_input = TRUE` they reconstruct `scale(x)`, not `x`. The column means
#' and standard deviations are not returned; keep them yourself if you need to
#' map results back to the original units.
#'
#' @param x Numeric matrix, or a data.frame whose columns are all numeric (it
#'   is coerced to a matrix). It must contain no `NA`, `NaN`, or `Inf` values.
#'   The argument is called `x` rather than `data` because it is a matrix of
#'   values, not a table of observations and features.
#' @param n_singular_values Positive whole number of singular values and
#'   vectors to keep, or `NULL` (default) for `min(dim(x))`. A value above
#'   `min(dim(x))` is an error.
#' @param scale_input How to transform the columns before decomposing:
#'   `TRUE` (default) centers and scales, `FALSE` leaves the matrix alone,
#'   `"center"` subtracts column means only, and `"scale"` divides only. Any
#'   other value is an error. The two dividing forms (`TRUE` and `"scale"`)
#'   need every column to have non-zero variance, meaning a standard deviation
#'   above `1000 * .Machine$double.eps` (about 2e-13) times the column's
#'   largest absolute value, so the check does not depend on units. Offending
#'   columns are named in the error, or numbered when the matrix has no column
#'   names. `"scale"` follows `base::scale()`: without centering it divides by
#'   the root mean square, not the standard deviation.
#' @param svd_method `"auto"` (default), `"exact"`, or `"approx"`. See
#'   Details for how `"auto"` decides and when `"approx"` falls back to the
#'   exact solver.
#' @param svd_threshold Positive number. With `svd_method = "auto"`, the
#'   approximate solver is considered only when `min(dim(x))` exceeds it.
#'   Default 100.
#' @param approx_args List of extra arguments passed to `RSpectra::svds()`,
#'   for example `tol` or `opts`. It must not include `A` or `k`, which are
#'   set from `x` and `n_singular_values`.
#' @param verbose Logical; print progress messages. Default `FALSE`.
#' @return A plain list, not an `fs_result`: `fs_svd()` reduces dimensions
#'   rather than selecting features. Writing `k` for the number kept
#'   (`n_singular_values`, or `min(dim(x))` by default), the components are:
#'   \itemize{
#'     \item `singular_values`: numeric vector of length `k`, in
#'       non-increasing order.
#'     \item `left_singular_vectors`: `nrow(x)` by `k` matrix (the `u` of
#'       `base::svd()`).
#'     \item `right_singular_vectors`: `ncol(x)` by `k` matrix (the `v` of
#'       `base::svd()`).
#'   }
#'   Singular vectors are unique only up to sign (and up to rotation within a
#'   block of tied singular values), so column signs can differ between the
#'   exact and approximate solvers and between platforms.
#' @examples
#' m <- matrix(
#'   c(4, 0, 0, 3,
#'     0, 5, 1, 2,
#'     2, 1, 6, 0,
#'     1, 3, 2, 7,
#'     5, 2, 0, 1,
#'     0, 4, 3, 2),
#'   nrow = 6, ncol = 4, byrow = TRUE
#' )
#'
#' # Two leading components of the centered and scaled matrix
#' res <- fs_svd(m, n_singular_values = 2, scale_input = TRUE)
#' res$singular_values
#' res$left_singular_vectors
#' res$right_singular_vectors
#'
#' # Share of the total (scaled) variance captured by those two components
#' all_values <- fs_svd(m, scale_input = TRUE)$singular_values
#' sum(res$singular_values^2) / sum(all_values^2)
#'
#' # Untransformed, the full decomposition reconstructs the input exactly
#' full <- fs_svd(m, scale_input = FALSE)
#' recon <- full$left_singular_vectors %*%
#'   (full$singular_values * t(full$right_singular_vectors))
#' all.equal(recon, m)
#' @export
fs_svd <- function(x,
                   n_singular_values = NULL,
                   scale_input = TRUE,
                   svd_method = c("auto", "exact", "approx"),
                   svd_threshold = 100,
                   approx_args = list(),
                   verbose = FALSE) {
  svd_method <- match.arg(svd_method)
  assert_flag(verbose, "verbose")
  assert_number(svd_threshold, "svd_threshold")
  if (svd_threshold <= 0) {
    stop("'svd_threshold' must be positive.", call. = FALSE)
  }
  if (!is.list(approx_args)) {
    stop(
      "'approx_args' must be a list of additional arguments for RSpectra::svds().",
      call. = FALSE
    )
  }
  if (any(c("A", "k") %in% names(approx_args))) {
    stop(
      paste0(
        "'approx_args' must not include 'A' or 'k'; these are set ",
        "internally from the input matrix and 'n_singular_values'."
      ),
      call. = FALSE
    )
  }

  mat <- svd_coerce_matrix(x)
  min_dim <- min(dim(mat))

  if (is.null(n_singular_values)) {
    k <- min_dim
  } else {
    k <- assert_count(n_singular_values, "n_singular_values", lower = 1L)
    if (k > min_dim) {
      stop(
        sprintf(
          "'n_singular_values' (%d) exceeds min(dim(x)) (%d).",
          k, min_dim
        ),
        call. = FALSE
      )
    }
  }

  xs <- svd_scale_matrix(mat, scale_input, verbose = verbose)

  if (svd_method == "auto") {
    if (k < min_dim && min_dim > svd_threshold) {
      svd_method <- "approx"
    } else {
      svd_method <- "exact"
      if (min_dim > svd_threshold && k == min_dim) {
        message(
          paste0(
            "svd_method = 'auto' selected the exact solver despite a large ",
            "matrix because all min(dim(x)) singular values were requested; ",
            "set n_singular_values < min(dim(x)) to enable the approximate ",
            "solver."
          )
        )
      }
    }
    if (verbose) {
      message(sprintf("Auto-selected SVD method: %s", svd_method))
    }
  }

  if (svd_method == "approx" && k >= min_dim) {
    message(
      paste0(
        "Requested n_singular_values equals min(dim(x)), but ",
        "RSpectra::svds() requires fewer; falling back to exact SVD."
      )
    )
    svd_method <- "exact"
  }

  if (svd_method == "exact") {
    if (verbose) message("Computing exact SVD via base::svd()...")
    s <- svd(xs)
    return(svd_truncate(s, k))
  }

  # Approximate path.
  fs_require("RSpectra", "approximate SVD")
  if (verbose) message("Computing approximate SVD via RSpectra::svds()...")
  s <- do.call(RSpectra::svds, c(list(A = xs, k = k), approx_args))

  # svds() only warns when fewer than k triplets converge and then returns
  # just those (possibly none), which would break the length-k contract.
  if (length(s$d) < k) {
    warning(
      sprintf(
        paste0(
          "RSpectra::svds() converged on only %d of %d singular values; ",
          "falling back to exact SVD. Loosen 'tol' or raise 'maxitr' in ",
          "approx_args$opts, or use svd_method = 'exact'."
        ),
        length(s$d), k
      ),
      call. = FALSE
    )
    return(svd_truncate(svd(xs), k))
  }

  # Ensure descending order of singular values.
  ord <- order(s$d, decreasing = TRUE)
  list(
    singular_values = s$d[ord],
    left_singular_vectors = s$u[, ord, drop = FALSE],
    right_singular_vectors = s$v[, ord, drop = FALSE]
  )
}
