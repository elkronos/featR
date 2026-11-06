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

#' Names of non-numeric columns (label candidates, excluded from PCA)
#'
#' Character and factor columns, but also logical, Date, and any other
#' non-numeric columns: none of them can enter the decomposition, so they are
#' carried as labels rather than silently dropped.
#'
#' @param data A data.frame or data.table.
#' @return Character vector of column names.
#' @noRd
pca_label_cols <- function(data) {
  names(data)[!vapply(data, is.numeric, logical(1L))]
}

#' Compute PCA scores and loadings with automatic engine selection
#'
#' Rows with missing values in the numeric columns are dropped, as are
#' zero-variance columns. Data with fewer than 1e7 cells uses
#' `stats::prcomp()`; from 1e7 cells up it uses `bigstatsr::big_SVD()` when the
#' suggested package 'bigstatsr' is installed, and otherwise falls back to
#' `prcomp()`, announcing the fallback only when `verbose = TRUE`.
#'
#' @param data A data.frame or data.table.
#' @param label_cols Character vector of columns excluded from the features.
#' @param num_pc Number of PCs to retain, or `NULL` to use
#'   `min(2, max_possible)`.
#' @param scale_data,center_data Logical scaling/centering switches.
#' @param verbose Logical; emit progress messages. Default `FALSE`.
#' @return A list with `svd` (list `u` = scores, `d` = standard deviations for
#'   the `prcomp()` engine or singular values for the `big_SVD()` engine,
#'   `v` = loadings), `var_explained` (proportion of *total* variance per
#'   computed PC: one entry per usable column under `prcomp()`, one entry per
#'   retained PC under `big_SVD()`), `num_pc` (the resolved number of PCs),
#'   `rows_kept` (logical vector over the rows of `data`), and `numeric_cols`
#'   (the feature columns that survived the zero-variance filter).
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

  # complete.cases() keeps Inf/-Inf. Such a column's sd is NaN, so the
  # zero-variance filter below dropped it with a misleading "zero-variance"
  # warning: a single infinite entry silently discarded the whole feature.
  inf_cols <- names(Xdt)[vapply(Xdt, function(col) any(is.infinite(col)),
                                logical(1L))]
  if (length(inf_cols) > 0L) {
    stop("Numeric columns contain infinite values (Inf/-Inf): ",
         paste(utils::head(inf_cols, 5L), collapse = ", "),
         if (length(inf_cols) > 5L) ", ..." else "",
         ". Remove or replace them before calling fs_pca().", call. = FALSE)
  }

  # Remove zero-variance numeric columns (avoids scaling errors). The test is
  # relative to each column's magnitude: a column that is constant up to
  # floating-point round-off (e.g. 0.3 next to 0.1 + 0.2) has a tiny non-zero
  # sd, and scaling it to unit variance would hand pure round-off noise a full
  # share of the decomposition. The spread is measured on col / max(abs(col)),
  # so a column in extreme units (|x| beyond ~1e154 or below ~1e-154, where
  # stats::sd() overflows or underflows) is not mistaken for a constant one.
  spreads <- lapply(Xdt, safe_col_spread)
  rel_sds <- vapply(spreads, `[[`, numeric(1L), "rel")
  keep_cols <- names(Xdt)[rel_sds > 1000 * .Machine$double.eps]
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

  # Columns whose variance is not representable in double precision. With
  # scaling, standardisation is invariant to rescaling a column, so dividing
  # such a column by its largest absolute value first gives exactly the same
  # decomposition; without scaling the covariance itself over- or underflows.
  extreme <- keep_cols[!vapply(spreads[keep_cols], `[[`, logical(1L),
                               "representable")]
  if (length(extreme) > 0L) {
    if (!scale_data) {
      stop("The variance of column(s) ",
           paste(utils::head(extreme, 5L), collapse = ", "),
           if (length(extreme) > 5L) ", ..." else "",
           " is not representable in double precision (values too large or ",
           "too small to square). Rescale them, or use scale_data = TRUE.",
           call. = FALSE)
    }
    for (nm in extreme) {
      data.table::set(Xdt, j = nm, value = Xdt[[nm]] / spreads[[nm]]$scale)
    }
  }

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

  if (use_big) {
    # bigstatsr::big_SVD() reaches RSpectra::eigs_sym(k, n = ncol(K)), which
    # requires k <= n - 1. max_possible above allows num_pc == n_cols, which
    # prcomp accepts but this engine does not, so validate before dispatch
    # rather than letting RSpectra raise the error.
    if (num_pc > n_cols - 1L) {
      stop(sprintf(
        paste0(
          "Requested %d PCs, but the large-data engine (bigstatsr::big_SVD) ",
          "can compute at most %d for %d columns. Reduce 'num_pc'."
        ),
        num_pc, n_cols - 1L, n_cols
      ), call. = FALSE)
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

  # A label column named like a score column (PC1, PC2, ...) would be
  # shadowed by the scores in pca_df, and the plot would colour by scores.
  clash <- intersect(cols_for_labels, colnames(pc_scores))
  if (length(clash) > 0L) {
    stop(sprintf(
      "Label column(s) %s clash with the principal-component score column names; rename them before calling fs_pca().",
      paste0("'", clash, "'", collapse = ", ")
    ), call. = FALSE)
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
#' print. A numeric label column is converted to a factor so the color scale
#' stays discrete. Up to 9 distinct labels use the Set1 brewer palette (Set1
#' has only 9 colors); above that the scale switches to ggplot2's discrete
#' viridis palette and a warning says so.
#'
#' @param pca_result Result list from `pca_results()`.
#' @param label_col Name of the label column in `pca_result$pca_df`.
#' @return A ggplot object. Errors when the suggested package 'ggplot2' is not
#'   installed, when `label_col` is absent, or when PC1/PC2 are missing.
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
#' Runs a principal component analysis (PCA) on the numeric columns of
#' `data` and returns loadings, scores, and the share of variance each
#' retained component explains. PCA finds the directions of greatest spread
#' in the data; use it to see how much of that spread a few components carry
#' and which columns drive them.
#'
#' @details
#' PCA is unsupervised dimensionality reduction, not feature selection. Every
#' component is a linear combination of all the retained numeric columns,
#' chosen without reference to any outcome. A PCA therefore does not shorten
#' the list of variables you have to measure, and the leading components are
#' the highest-variance directions, which need not be the ones related to a
#' response.
#'
#' Before the decomposition:
#' \itemize{
#'   \item Non-numeric columns (character, factor, logical, Date, ...) are
#'     left out of the features and carried along as labels. An explicit
#'     `label_col`, numeric or not, is also left out of the features.
#'   \item Rows with a missing value in any numeric feature column are
#'     dropped. An infinite value in a feature column is an error.
#'   \item Zero-variance columns are dropped with a warning. A column counts
#'     as zero-variance when its standard deviation is at most
#'     `1000 * .Machine$double.eps` (about 2e-13) times its largest absolute
#'     value. A column that is constant up to floating-point round-off is
#'     therefore dropped, while small-unit or large-offset columns
#'     (timestamps, say) are kept.
#' }
#'
#' Data with fewer than 1e7 cells is decomposed with `stats::prcomp()`. From
#' 1e7 cells up, if the suggested package 'bigstatsr' is installed,
#' `bigstatsr::big_SVD()` runs on a temporary file-backed matrix (the backing
#' file is deleted when the call returns) and can compute at most one fewer
#' component than there are usable columns. Without 'bigstatsr' the call falls
#' back to `prcomp()`, and says so only when `verbose = TRUE`.
#'
#' `var_explained` is the proportion of total variance explained by each
#' retained component under both engines. It sums to 1 only when every
#' available component is retained; with the default two components it sums
#' to the fraction those two capture. `prcomp()` computes every component, so
#' the total is the sum of all eigenvalues. `big_SVD()` computes only the top
#' `num_pc` singular values, so the total is recovered separately from the
#' column statistics of the (implicitly centered and scaled) matrix. Under
#' either engine, read the entries as shares of the whole, never as shares of
#' the components that happened to be computed.
#'
#' `plot` controls printing only, never construction. Whenever a plot can be
#' built (a `label_col` was supplied, at least two components were retained,
#' and the suggested package 'ggplot2' is installed), the ggplot object is
#' returned in `$plot`, whatever the value of `plot`. `plot = TRUE` also
#' prints it and makes 'ggplot2' required: its absence is then an error rather
#' than a missing `$plot`. `plot = TRUE` without a `label_col`, or with fewer
#' than two retained components, warns and skips the plot.
#'
#' @param data A data.frame or data.table with at least two rows and at least
#'   one numeric column. Column names must be unique and non-empty. A
#'   non-numeric (label) column named like a retained score column (`PC1`,
#'   `PC2`, ...) would be hidden by the scores in `pca_df`, so that is an
#'   error. Columns in extreme units (beyond about `1e154` or below `1e-154`)
#'   are kept, because the zero-variance check is relative to each column's
#'   magnitude; with `scale_data = FALSE` their variance cannot be represented
#'   and they are an error.
#' @param num_pc Number of principal components to retain: a whole number
#'   `>= 1`, or `NULL` (the default) for `min(2, max_possible)`.
#'   `max_possible` is `min(nrow - 1, ncol)` of the usable numeric data, after
#'   incomplete rows and zero-variance columns are dropped. A larger explicit
#'   value is an error.
#' @param scale_data Logical; scale numeric columns to unit variance. Default
#'   `TRUE`. `scale_data = TRUE` with `center_data = FALSE` is an error,
#'   because `stats::prcomp()` and `bigstatsr::big_scale()` handle uncentered
#'   scaling differently (the latter ignores it), and the result would depend
#'   on which engine the data size selected.
#' @param center_data Logical; center numeric columns. Default `TRUE`.
#' @param label_col Optional single string naming a column of `data` used to
#'   label and color points. It is left out of the PCA features and carried
#'   into `pca_df`. Default `NULL`: no labels, and therefore no plot.
#' @param plot Logical; if `TRUE`, print the PC1 vs PC2 scatterplot. Default
#'   `FALSE`, whether or not `label_col` is supplied. The plot object is
#'   returned in `$plot` either way whenever one can be built (see Details).
#' @param verbose Logical; print progress messages naming the non-numeric
#'   columns held back as labels, the engine chosen, and any fallback from
#'   'bigstatsr'. Default `FALSE`.
#'
#' @return A plain list, not an `fs_result`: `fs_pca()` reduces dimensions
#'   rather than selecting features, so it has no `selected` set to report.
#'   The components are:
#' \itemize{
#'   \item `pc_loadings`: numeric matrix of variable loadings, one row per
#'     retained numeric feature (named) and one column per retained component
#'     (`PC1`, `PC2`, ...).
#'   \item `pc_scores`: numeric matrix of observation scores, one row per kept
#'     observation and one column per retained component.
#'   \item `var_explained`: numeric vector of length `num_pc`, the proportion
#'     of total variance carried by each retained component (see Details).
#'   \item `pca_df`: data.table of the scores with the label columns (every
#'     non-numeric column, plus `label_col`) bound alongside, restricted to the
#'     kept rows.
#'   \item `meta`: list with `numeric_cols` (the features actually
#'     decomposed), `rows_kept` (logical vector over the rows of `data`),
#'     `n_rows_used`, and `n_cols_used`.
#'   \item `plot`: the ggplot object, present only when a plot could be built
#'     (a `label_col` was supplied, at least two components were retained, and
#'     'ggplot2' is installed). Test for it with `"plot" %in% names(res)`.
#' }
#'
#' @examples
#' res <- fs_pca(mtcars, num_pc = 2, label_col = "cyl")
#' res$var_explained
#' head(res$pca_df)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # plot = FALSE (the default) draws nothing, but $plot is built anyway
#'   res <- fs_pca(iris, label_col = "Species", verbose = TRUE)
#'   print(inherits(res$plot, "ggplot"))
#'
#'   # plot = TRUE draws it as well
#'   res <- fs_pca(iris, label_col = "Species", plot = TRUE)
#' }
#' @seealso The [Dimensionality reduction](https://elkronos.github.io/featR/articles/dimensionality-reduction.html)
#'   guide, which runs this function on a worked example.
#' @export
fs_pca <- function(data,
                   num_pc = NULL,
                   scale_data = TRUE,
                   center_data = TRUE,
                   label_col = NULL,
                   plot = FALSE,
                   verbose = FALSE) {
  assert_data_frame(data, "data")
  # Columns are selected by name, so a duplicated name would silently drop
  # the second copy from the decomposition.
  assert_column_names(data, "fs_pca")
  if (!is.null(num_pc)) {
    num_pc <- assert_count(num_pc, "num_pc", lower = 1L)
  }
  assert_flag(scale_data, "scale_data")
  assert_flag(center_data, "center_data")
  # The two engines disagree here: stats::prcomp(center = FALSE, scale. = TRUE)
  # divides by the root mean square, while bigstatsr::big_scale(center = FALSE,
  # scale = TRUE) returns sds of 1 and so silently ignores the request. Rather
  # than let the decomposition depend on whether the data crossed the size
  # threshold, refuse the combination.
  if (scale_data && !center_data) {
    stop("scale_data = TRUE with center_data = FALSE is not supported: the ",
         "small- and large-data engines scale uncentered data differently, so ",
         "the result would depend on the size of your input. Use ",
         "center_data = TRUE, or scale_data = FALSE.", call. = FALSE)
  }
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
