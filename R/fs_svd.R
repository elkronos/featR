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
#' instead of surfacing later as a generic non-finite-values failure.
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
    sds <- apply(mat, 2L, stats::sd)
    zero_var <- which(!is.finite(sds) | sds < .Machine$double.eps^0.5)
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

#' Singular Value Decomposition with Optional Scaling and Truncation
#'
#' Computes the SVD of a matrix with options for centering/scaling,
#' truncation to the leading singular triplets, and an approximate solver
#' for large matrices.
#'
#' Reach for this when you want a low-rank summary of a numeric matrix -- the
#' leading components for compression, denoising, or a latent-factor
#' representation -- rather than a subset of the original columns. Because the
#' components are linear combinations of every input column, nothing is
#' dropped and individual features stay uninterpretable; use one of the
#' package's selection functions when you need to name the features you keep.
#' \code{\link{fs_pca}} covers the same ground with tidy, labeled output and
#' variance-explained figures; \code{fs_svd()} is the lower-level
#' decomposition.
#'
#' @details
#' The exact path uses \code{base::svd()}. The approximate path uses
#' \code{RSpectra::svds()} (package \pkg{RSpectra}, a Suggests dependency,
#' required only when that path is actually taken) and is only applicable
#' when fewer than \code{min(dim(x))} singular values are requested. By
#' default \code{n_singular_values = NULL} resolves to \code{min(dim(x))},
#' which implies the exact path; to enable the approximate solver on a large
#' matrix, request \code{n_singular_values < min(dim(x))}.
#'
#' With \code{svd_method = "auto"}, the approximate solver is chosen only
#' when \code{n_singular_values < min(dim(x))} and
#' \code{min(dim(x)) > svd_threshold}; otherwise the exact solver is used,
#' and a message explains why when a large matrix still ends up on the exact
#' path because all singular values were requested. Asking for
#' \code{svd_method = "approx"} outright when \code{n_singular_values} equals
#' \code{min(dim(x))} is not an error: it falls back to the exact solver with
#' a message, because \code{RSpectra::svds()} cannot return the full set.
#'
#' Centering and scaling, when requested, happen \emph{before} the
#' decomposition, so the returned triplets factorize the transformed matrix
#' rather than the raw input: with the default \code{scale_input = TRUE} they
#' reconstruct \code{scale(x)}, not \code{x}. The transformation itself is not
#' returned, so keep the column means and standard deviations yourself if you
#' need to map results back to the original units.
#'
#' @param x Numeric matrix, or a data.frame whose columns are all numeric
#'   (it is coerced to a matrix). Must contain no NA/NaN/Inf values. The
#'   argument is called \code{x} rather than \code{data} because it is a
#'   matrix of values, not a table of observations and features.
#' @param n_singular_values Positive whole number of singular values and
#'   vectors to keep, or NULL (default) for \code{min(dim(x))}. Values
#'   exceeding \code{min(dim(x))} are an error.
#' @param scale_input TRUE (center and scale, the default), FALSE (leave the
#'   matrix alone), \code{"center"} (subtract column means only), or
#'   \code{"scale"} (divide only). Any other value is an error. The two
#'   dividing forms (\code{TRUE} and \code{"scale"}) require every column to
#'   have non-zero variance, and offending columns are reported by name (or by
#'   position when the matrix has no column names); \code{"center"} has no
#'   such requirement. Note that \code{"scale"} inherits
#'   \code{base::scale()}'s semantics: with no centering it divides by the
#'   root mean square, not by the standard deviation.
#' @param svd_method \code{"auto"} (default), \code{"exact"}, or
#'   \code{"approx"}. See Details for how \code{"auto"} decides and when
#'   \code{"approx"} falls back to the exact solver.
#' @param svd_threshold Positive number; with \code{svd_method = "auto"},
#'   the approximate solver is considered only when \code{min(dim(x))}
#'   exceeds this value (default 100).
#' @param approx_args List of extra arguments passed to
#'   \code{RSpectra::svds()} (for example \code{tol} or \code{opts}). Must
#'   not include \code{A} or \code{k}, which are set internally.
#' @param verbose Logical; emit progress messages. Default FALSE.
#' @return A plain list. \code{fs_svd()} is dimensionality reduction rather
#'   than feature selection, so it returns its own decomposition structure and
#'   not the \code{fs_result} object produced by the package's selection
#'   functions. Writing \code{k} for the number of triplets kept
#'   (\code{n_singular_values}, or \code{min(dim(x))} by default), the
#'   components are:
#'   \itemize{
#'     \item \code{singular_values}: numeric vector of length \code{k}, in
#'       non-increasing order.
#'     \item \code{left_singular_vectors}: \code{nrow(x)} by \code{k} matrix
#'       (the \code{u} of \code{base::svd()}).
#'     \item \code{right_singular_vectors}: \code{ncol(x)} by \code{k} matrix
#'       (the \code{v} of \code{base::svd()}).
#'   }
#'   Singular vectors are unique only up to sign (and up to rotation within a
#'   tied block), so column signs may differ between the exact and approximate
#'   solvers and between platforms.
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

  # Ensure descending order of singular values.
  ord <- order(s$d, decreasing = TRUE)
  list(
    singular_values = s$d[ord],
    left_singular_vectors = s$u[, ord, drop = FALSE],
    right_singular_vectors = s$v[, ord, drop = FALSE]
  )
}
