# Principal component analysis helpers for featR.

# ------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------

#' Validate a dataset for PCA
#'
#' Ensures the input has at least two rows and at least one numeric column.
#'
#' @param data A data.frame or data.table.
#' @return Invisibly `TRUE`; stops with an informative error otherwise.
#' @noRd
pca_check_data <- function(data) {
  if (nrow(data) < 2L) {
    stop("Invalid data for PCA: need at least 2 rows.", call. = FALSE)
  }
  n_num <- sum(vapply(data, is.numeric, logical(1L)))
  if (n_num < 1L) {
    stop("Invalid data for PCA: need at least 1 numeric column.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Names of character/factor columns (label candidates, excluded from PCA)
#'
#' @param data A data.frame or data.table.
#' @return Character vector of column names.
#' @noRd
pca_label_cols <- function(data) {
  names(data)[vapply(data, function(col) is.character(col) || is.factor(col),
                     logical(1L))]
}

#' Compute PCA scores and loadings with automatic engine selection
#'
#' Rows with missing values in the numeric columns are dropped, as are
#' zero-variance columns. Small data (fewer than 1e7 cells) uses
#' `stats::prcomp()`; larger data uses `bigstatsr::big_SVD()` when the
#' suggested package 'bigstatsr' is installed, and falls back to `prcomp()`
#' with a message otherwise.
#'
#' @param data A data.frame or data.table.
#' @param label_cols Character vector of columns excluded from the features.
#' @param num_pc Number of PCs to retain, or `NULL` to use
#'   `min(2, max_possible)`.
#' @param scale_data,center_data Logical scaling/centering switches.
#' @param verbose Logical; emit progress messages. Default `FALSE`.
#' @return A list with `svd` (list `u` = scores, `d` = singular values or
#'   standard deviations, `v` = loadings), `var_explained` (proportion of
#'   total variance per computed PC), `num_pc` (resolved value), `rows_kept`
#'   (logical vector), and `numeric_cols` (character vector).
#' @noRd
pca_compute <- function(data,
                        label_cols = character(0),
                        num_pc = NULL,
                        scale_data = TRUE,
                        center_data = TRUE,
                        verbose = FALSE) {
  dt <- as_dt(data)

  # Numeric columns are those not declared as labels and actually numeric.
  candidate_cols <- setdiff(names(dt), label_cols)
  is_num <- vapply(dt[, candidate_cols, with = FALSE], is.numeric, logical(1L))
  numeric_cols <- candidate_cols[is_num]

  if (length(numeric_cols) == 0L) {
    stop("No numeric columns found for PCA after excluding label columns.",
         call. = FALSE)
  }

  # Keep rows with complete numeric cases only.
  rows_kept <- stats::complete.cases(dt[, numeric_cols, with = FALSE])
  if (!any(rows_kept)) {
    stop("All rows have missing values in numeric columns.", call. = FALSE)
  }
  Xdt <- dt[rows_kept, numeric_cols, with = FALSE]

  if (nrow(Xdt) < 2L) {
    stop("Not enough rows with complete numeric data to compute PCA (need at least 2).",
         call. = FALSE)
  }

  # Remove zero-variance numeric columns (avoids scaling errors).
  sds <- vapply(Xdt, stats::sd, numeric(1L))
  sds[!is.finite(sds)] <- 0
  keep_cols <- names(sds)[sds > 0]
  drop_cols <- setdiff(names(Xdt), keep_cols)

  if (length(keep_cols) == 0L) {
    stop("All numeric columns have zero variance; PCA is not defined.",
         call. = FALSE)
  }
  if (length(drop_cols) > 0L) {
    warning(sprintf("Removed %d zero-variance column(s): %s",
                    length(drop_cols), paste(drop_cols, collapse = ", ")),
            call. = FALSE)
  }

  Xdt <- Xdt[, keep_cols, with = FALSE]
  numeric_cols <- keep_cols

  n_rows <- nrow(Xdt)
  n_cols <- ncol(Xdt)

  max_possible <- min(n_rows - 1L, n_cols)
  if (max_possible < 1L) {
    stop("Not enough information to compute any principal component (check data dimensions).",
         call. = FALSE)
  }
  if (is.null(num_pc)) {
    num_pc <- min(2L, max_possible)
  } else if (num_pc > max_possible) {
    stop(sprintf("Requested %d PCs, but at most %d can be computed from the data.",
                 num_pc, max_possible), call. = FALSE)
  }
  num_pc <- as.integer(num_pc)

  # as.numeric() avoids integer overflow for very large n_rows * n_cols.
  large <- (as.numeric(n_rows) * n_cols) >= 1e7
  use_big <- FALSE
  if (large) {
    if (requireNamespace("bigstatsr", quietly = TRUE)) {
      use_big <- TRUE
    } else if (verbose) {
      message("Package 'bigstatsr' is not installed; falling back to prcomp for this large dataset.")
    }
  }

  if (!use_big) {
    if (verbose) {
      message("Using prcomp for PCA computation.")
    }
    pca_obj <- stats::prcomp(Xdt, center = center_data, scale. = scale_data)
    svd_parts <- list(u = pca_obj$x, d = pca_obj$sdev, v = pca_obj$rotation)
    # prcomp returns standard deviations for all PCs, so this is exact.
    var_explained <- pca_obj$sdev^2 / sum(pca_obj$sdev^2)
  } else {
    if (verbose) {
      message("Using bigstatsr::big_SVD for PCA computation (large dataset).")
    }

    backing <- tempfile()
    on.exit(unlink(paste0(backing, c(".bk", ".rds"))), add = TRUE)

    big_mat <- bigstatsr::FBM(n_rows, n_cols, backingfile = backing)
    big_mat[, ] <- as.matrix(Xdt)

    svd_obj <- bigstatsr::big_SVD(
      big_mat,
      fun.scaling = bigstatsr::big_scale(center = center_data,
                                         scale = scale_data),
      k = num_pc
    )

    d <- svd_obj$d
    # PCA scores are U %*% diag(D) for an SVD.
    scores <- svd_obj$u %*% diag(d, nrow = length(d), ncol = length(d))
    loadings <- svd_obj$v
    # big_SVD does not carry feature names; restore them.
    rownames(loadings) <- numeric_cols

    # big_SVD returns only the top-k singular values, so sum(d^2) is NOT the
    # total variance. Compute the total sum of squares of the (implicitly
    # centered/scaled) matrix explicitly from column statistics.
    n <- nrow(big_mat)
    cs <- bigstatsr::big_colstats(big_mat)
    if (scale_data && center_data) {
      total_ss <- (n - 1) * ncol(big_mat)
    } else if (center_data) {
      total_ss <- (n - 1) * sum(cs$var)
    } else {
      # Sum of raw squared entries: (n - 1) * var + n * mean^2 per column.
      total_ss <- sum((n - 1) * cs$var + n * cs$sum^2 / n^2)
    }
    var_explained <- d^2 / total_ss

    svd_parts <- list(u = scores, d = d, v = loadings)
  }

  list(
    svd = svd_parts,
    var_explained = var_explained,
    num_pc = num_pc,
    rows_kept = rows_kept,
    numeric_cols = numeric_cols
  )
}

#' Assemble tidy PCA results
#'
#' @param pca_fit Result of `pca_compute()`.
#' @param num_pc Number of PCs to report.
#' @param data Original data (labels are taken from it).
#' @param label_cols Character vector of label column names.
#' @param extra_label_col Optional extra label column name.
#' @return A list with `pc_loadings`, `pc_scores`, `var_explained`, `pca_df`,
#'   and `meta`.
#' @noRd
pca_results <- function(pca_fit,
                        num_pc,
                        data,
                        label_cols = character(0),
                        extra_label_col = NULL) {
  svd_parts <- pca_fit$svd
  rows_kept <- pca_fit$rows_kept
  numeric_cols <- pca_fit$numeric_cols

  n_avail <- ncol(svd_parts$u)
  if (num_pc > n_avail) {
    stop(sprintf("Requested %d PCs, but only %d were computed.",
                 num_pc, n_avail), call. = FALSE)
  }

  var_explained <- pca_fit$var_explained[seq_len(num_pc)]

  pc_loadings <- svd_parts$v[, seq_len(num_pc), drop = FALSE]
  pc_scores <- svd_parts$u[, seq_len(num_pc), drop = FALSE]
  colnames(pc_loadings) <- paste0("PC", seq_len(num_pc))
  colnames(pc_scores) <- paste0("PC", seq_len(num_pc))

  # Assemble labels: declared label columns plus the explicit extra label.
  dt_all <- as_dt(data)
  cols_for_labels <- unique(c(label_cols, extra_label_col))
  cols_for_labels <- cols_for_labels[cols_for_labels %in% names(dt_all)]
  label_data <- if (length(cols_for_labels) > 0L) {
    dt_all[rows_kept, cols_for_labels, with = FALSE]
  } else {
    data.table::data.table()
  }

  pca_df <- cbind(data.table::as.data.table(pc_scores), label_data)

  list(
    pc_loadings = pc_loadings,
    pc_scores = pc_scores,
    var_explained = var_explained,
    pca_df = pca_df,
    meta = list(
      numeric_cols = numeric_cols,
      rows_kept = rows_kept,
      n_rows_used = sum(rows_kept),
      n_cols_used = length(numeric_cols)
    )
  )
}

#' Build a ggplot of the first two PCs, colored by a label column
#'
#' Returns the plot object without printing it; the caller decides whether to
#' print. Uses the viridis discrete palette (from ggplot2) when there are more
#' than 9 unique labels, and the Set1 brewer palette otherwise.
#'
#' @param pca_result Result list from `pca_results()`.
#' @param label_col Name of the label column in `pca_result$pca_df`.
#' @return A ggplot object.
#' @noRd
pca_plot <- function(pca_result, label_col) {
  fs_require("ggplot2", "PCA plotting")

  dt <- pca_result$pca_df
  if (!label_col %in% names(dt)) {
    stop(sprintf("Label column '%s' not found in PCA results.", label_col),
         call. = FALSE)
  }
  if (!all(c("PC1", "PC2") %in% names(dt))) {
    stop("PCA results must contain PC1 and PC2 to plot.", call. = FALSE)
  }

  label_values <- dt[[label_col]]
  if (is.numeric(label_values)) {
    label_values <- factor(label_values)
  }

  plot_df <- data.frame(
    PC1 = dt[["PC1"]],
    PC2 = dt[["PC2"]],
    label = label_values
  )

  num_labels <- length(unique(stats::na.omit(plot_df$label)))

  # Bind plotting symbols locally so R CMD check does not flag the
  # non-standard evaluation inside aes(); the data mask takes precedence.
  PC1 <- PC2 <- label <- NULL

  p <- ggplot2::ggplot(plot_df,
                       ggplot2::aes(x = PC1, y = PC2, color = label)) +
    ggplot2::geom_point(alpha = 0.8, size = 2) +
    ggplot2::ggtitle("PCA Results") +
    ggplot2::xlab(sprintf("PC1 (%.2f%% variance)",
                          100 * pca_result$var_explained[1L])) +
    ggplot2::ylab(sprintf("PC2 (%.2f%% variance)",
                          100 * pca_result$var_explained[2L])) +
    ggplot2::labs(color = label_col)

  if (num_labels > 9L) {
    warning("More than 9 unique labels; using the viridis discrete palette.",
            call. = FALSE)
    p <- p + ggplot2::scale_color_viridis_d()
  } else {
    p <- p + ggplot2::scale_color_brewer(palette = "Set1")
  }

  p
}

# ------------------------------------------------------------------
# Public wrapper
# ------------------------------------------------------------------

#' Principal component analysis with tidy results and optional plotting
#'
#' Runs a PCA on the numeric columns of `data`. Character and factor columns
#' are excluded from the feature set and kept as label candidates; an
#' explicitly supplied `label_col` (numeric or not) is likewise excluded from
#' the features and only used for labeling. Rows with missing values in the
#' numeric columns and zero-variance columns are dropped before the
#' decomposition.
#'
#' @details
#' For small data (fewer than 1e7 cells) the decomposition uses
#' `stats::prcomp()`. For larger data it uses `bigstatsr::big_SVD()` on a
#' temporary file-backed matrix (the backing file is deleted on exit) when the
#' suggested package 'bigstatsr' is installed; otherwise it falls back to
#' `prcomp()` with a message.
#'
#' `var_explained` always reports the proportion of *total* variance
#' explained by each retained component. With the 'bigstatsr' engine only the
#' retained components are computed, so the entries do not sum to 1 (they sum
#' to the fraction of variance captured by those components).
#'
#' `plot` controls printing only, never construction. Whenever a plot can be
#' built at all (a `label_col` was supplied, at least two components were
#' retained, and the suggested package 'ggplot2' is installed) the ggplot
#' object is returned in `$plot`, whether or not `plot` is `TRUE`. Setting
#' `plot = TRUE` additionally prints it, and in that case 'ggplot2' is
#' required: its absence becomes an error rather than a silently missing
#' `$plot`. `plot = TRUE` without a `label_col`, or with fewer than two
#' retained components, warns and skips the plot.
#'
#' @param data A data.frame or data.table with at least two rows and at least
#'   one numeric column.
#' @param num_pc Number of principal components to retain, or `NULL` (the
#'   default) to retain `min(2, max_possible)` where `max_possible` is
#'   `min(nrow - 1, ncol)` of the usable numeric data. Explicit values larger
#'   than `max_possible` raise an error.
#' @param scale_data Logical; scale numeric columns to unit variance.
#'   Default `TRUE`.
#' @param center_data Logical; center numeric columns. Default `TRUE`.
#' @param label_col Optional name of a column used to label and color points;
#'   the column is excluded from the PCA features.
#' @param plot Logical; if `TRUE`, the PC1 vs PC2 scatterplot is printed.
#'   Default `FALSE`, unconditionally: it no longer depends on whether
#'   `label_col` was supplied. The plot object is returned in `$plot` either
#'   way whenever one can be built (see Details).
#' @param verbose Logical; emit progress messages. Default `FALSE`.
#'
#' @return A plain list. `fs_pca()` is dimensionality reduction rather than
#'   feature selection, so it returns its own PCA structure and not the
#'   `fs_result` object produced by the package's selection functions. The
#'   components are:
#' \itemize{
#'   \item `pc_loadings`: matrix of variable loadings (features x PCs).
#'   \item `pc_scores`: matrix of observation scores (rows kept x PCs).
#'   \item `var_explained`: proportion of total variance per retained PC
#'     (see Details for the large-data engine).
#'   \item `pca_df`: data.table of scores plus label columns.
#'   \item `meta`: list with `numeric_cols`, `rows_kept`, `n_rows_used`,
#'     `n_cols_used`.
#'   \item `plot`: the ggplot object; present only when a plot could be built.
#' }
#'
#' @examples
#' res <- fs_pca(mtcars, num_pc = 2, label_col = "cyl")
#' res$var_explained
#' head(res$pca_df)
#'
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # nothing is printed, but the ggplot object is still there
#'   res <- fs_pca(iris, label_col = "Species", verbose = TRUE)
#'   res$plot
#'
#'   # plot = TRUE prints it as well
#'   res <- fs_pca(iris, label_col = "Species", plot = TRUE)
#' }
#' }
#' @export
fs_pca <- function(data,
                   num_pc = NULL,
                   scale_data = TRUE,
                   center_data = TRUE,
                   label_col = NULL,
                   plot = FALSE,
                   verbose = FALSE) {
  assert_data_frame(data, "data")
  if (!is.null(num_pc)) {
    num_pc <- assert_count(num_pc, "num_pc", lower = 1L)
  }
  assert_flag(scale_data, "scale_data")
  assert_flag(center_data, "center_data")
  if (!is.null(label_col)) {
    assert_target(data, label_col, arg = "label_col")
  }
  assert_flag(plot, "plot")
  assert_flag(verbose, "verbose")

  pca_check_data(data)

  base_label_cols <- pca_label_cols(data)
  if (length(base_label_cols) > 0L && verbose) {
    message(sprintf(
      "Excluding %d non-numeric column(s) from the PCA features (kept as label candidates): %s",
      length(base_label_cols), paste(base_label_cols, collapse = ", ")
    ))
  }
  all_label_cols <- unique(c(base_label_cols, label_col))

  pca_fit <- pca_compute(
    data = data,
    label_cols = all_label_cols,
    num_pc = num_pc,
    scale_data = scale_data,
    center_data = center_data,
    verbose = verbose
  )
  num_pc <- pca_fit$num_pc

  results <- pca_results(
    pca_fit,
    num_pc = num_pc,
    data = data,
    label_cols = all_label_cols,
    extra_label_col = label_col
  )

  # `plot` decides whether to print, not whether to build: the ggplot object
  # is attached to the result whenever one can be built at all.
  can_plot <- !is.null(label_col) && num_pc >= 2L

  if (plot && !can_plot) {
    if (is.null(label_col)) {
      warning("plot = TRUE but no 'label_col' provided; skipping plot.",
              call. = FALSE)
    } else {
      warning("plot = TRUE requires at least 2 principal components; skipping plot.",
              call. = FALSE)
    }
  }

  # 'ggplot2' is only a Suggests, so build the plot silently when it happens to
  # be installed and let pca_plot() raise the actionable install error only
  # when the user actually asked for the plot to be drawn.
  if (can_plot && (plot || requireNamespace("ggplot2", quietly = TRUE))) {
    p <- pca_plot(results, label_col = label_col)
    results$plot <- p
    if (plot) {
      print(p)
    }
  }

  results
}
