# ============================
# PCA Utilities (Data Table)
# ============================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)      # plotting
  library(RColorBrewer) # discrete palette
  library(viridis)      # large discrete palette
  library(bigstatsr)    # large PCA
  library(rlang)        # for tidy evaluation (.data)
})

# ------------------------------------------------
# 1) Validation & Helpers
# ------------------------------------------------

#' Check Data Validity for PCA
#' Ensures data is non-null, has >= 2 rows, and contains >= 1 numeric column.
#' Stops with an informative error if invalid.
check_data_validity <- function(data) {
  if (is.null(data)) stop("Invalid data for PCA: data is NULL.")
  dt <- as.data.table(data)
  if (nrow(dt) < 2L) stop("Invalid data for PCA: need at least 2 rows.")
  n_num <- sum(vapply(dt, is.numeric, logical(1)))
  if (n_num < 1L) stop("Invalid data for PCA: need at least 1 numeric column.")
  invisible(TRUE)
}

#' Identify Label Columns
#' Returns names of columns that are character or factor.
identify_label_cols <- function(data) {
  dt <- as.data.table(data)
  names(dt)[vapply(dt, function(col) is.character(col) || is.factor(col), logical(1))]
}

# ------------------------------------------------
# 2) PCA Computation
# ------------------------------------------------

#' Perform PCA on numeric subset (rows with complete numeric cases only).
#'
#' For small data (n_rows * n_cols < 1e7): uses base prcomp.
#' For large data: uses bigstatsr::big_SVD on a Filebacked Big Matrix.
#'
#' Returns a list with:
#' - svd: list(u = scores, d = singular values / sdev, v = loadings)
#' - rows_kept: logical index for rows used (complete numeric cases)
#' - numeric_cols: character vector of numeric column names used
.perform_pca <- function(data,
                         label_cols  = character(0),
                         num_pc      = 2L,
                         scale_data  = TRUE,
                         center_data = TRUE) {
  dt <- as.data.table(data)
  
  # Numeric columns are those not in label_cols and actually numeric
  candidate_cols <- setdiff(names(dt), label_cols)
  numeric_cols <- candidate_cols[vapply(dt[, ..candidate_cols], is.numeric, logical(1))]
  
  if (length(numeric_cols) == 0L) {
    stop("No numeric columns found for PCA after excluding label columns.")
  }
  
  # Keep rows with complete numeric cases only
  rows_kept <- complete.cases(dt[, ..numeric_cols])
  if (!any(rows_kept)) stop("All rows have missing values in numeric columns.")
  Xdt <- dt[rows_kept, ..numeric_cols]
  
  if (nrow(Xdt) < 2L) {
    stop("Not enough rows with complete numeric data to compute PCA (need at least 2).")
  }
  
  # Remove zero-variance numeric columns (avoids scaling error)
  sds <- vapply(Xdt, sd, numeric(1))
  sds[!is.finite(sds)] <- 0
  keep_cols <- names(sds)[sds > 0]
  drop_cols <- setdiff(names(Xdt), keep_cols)
  
  if (length(keep_cols) == 0L) {
    stop("All numeric columns have zero variance; PCA is not defined.")
  }
  if (length(drop_cols) > 0L) {
    warning(sprintf(
      "Removed %d zero-variance column(s): %s",
      length(drop_cols),
      paste(drop_cols, collapse = ", ")
    ))
  }
  
  Xdt <- Xdt[, ..keep_cols]
  numeric_cols <- keep_cols
  
  n_rows <- nrow(Xdt)
  n_cols <- ncol(Xdt)
  
  # Basic dimensional sanity
  if (n_cols < 1L) stop("Not enough numeric columns for PCA.")
  
  max_possible <- min(n_rows - 1L, n_cols)
  if (max_possible < 1L) {
    stop("Not enough information to compute any principal component (check data dimensions).")
  }
  if (num_pc > max_possible) {
    stop(sprintf(
      "Requested %d PCs, but at most %d can be computed from the data.",
      num_pc, max_possible
    ))
  }
  
  # Small vs large branch
  if ((n_rows * n_cols) < 1e7) {
    message("Using prcomp for PCA computation (small dataset).")
    
    pca_obj  <- prcomp(Xdt, center = center_data, scale. = scale_data)
    scores   <- pca_obj$x
    loadings <- pca_obj$rotation
    sdev     <- pca_obj$sdev
    
    svd <- list(u = scores, d = sdev, v = loadings)
    
  } else {
    message("Using bigstatsr::big_SVD for PCA computation (large dataset).")
    
    big_mat <- FBM(n_rows, n_cols, backingfile = tempfile())
    big_mat[,] <- as.matrix(Xdt)
    
    k <- as.integer(num_pc)
    if (k <= 0L) {
      stop("Requested number of PCs is not valid for the data dimensions.")
    }
    
    svd_obj <- big_SVD(
      big_mat,
      fun.scaling = big_scale(center = center_data, scale = scale_data),
      k = k
    )
    
    d        <- svd_obj$d
    # PCA scores are U %*% diag(D) for SVD
    scores   <- svd_obj$u %*% diag(d, nrow = length(d), ncol = length(d))
    loadings <- svd_obj$v
    
    svd <- list(u = scores, d = d, v = loadings)
  }
  
  list(svd = svd, rows_kept = rows_kept, numeric_cols = numeric_cols)
}

# ------------------------------------------------
# 3) Result Structuring
# ------------------------------------------------

#' Build a tidy PCA results object
#'
#' Returns a list with:
#' - pc_loadings:   matrix [features x PCs]
#' - pc_scores:     matrix [rows_kept x PCs]
#' - var_explained: numeric vector length num_pc (proportion of variance
#'                  relative to the PCs represented in svd$d)
#' - pca_df:        data.table of scores + chosen labels
#' - meta:          list with numeric_cols, rows_kept (logical), n_rows_used, n_cols_used
.create_pca_results <- function(pca_fit,
                                num_pc,
                                data,
                                label_cols      = character(0),
                                extra_label_col = NULL) {
  svd          <- pca_fit$svd
  rows_kept    <- pca_fit$rows_kept
  numeric_cols <- pca_fit$numeric_cols
  
  # Validate requested PCs vs computed
  n_avail <- ncol(svd$u)
  if (num_pc > n_avail) {
    stop(sprintf("Requested %d PCs, but only %d were computed.", num_pc, n_avail))
  }
  
  # Variance explained (relative to all PCs represented in svd$d)
  var_explained <- (svd$d[1:num_pc]^2) / sum(svd$d^2)
  
  # Loadings and scores
  pc_loadings <- svd$v[, 1:num_pc, drop = FALSE]
  pc_scores   <- svd$u[, 1:num_pc, drop = FALSE]
  colnames(pc_loadings) <- paste0("PC", seq_len(num_pc))
  colnames(pc_scores)   <- paste0("PC", seq_len(num_pc))
  
  # Assemble labels: include declared label_cols AND an explicit extra_label_col
  dt_all <- as.data.table(data)
  cols_for_labels <- unique(c(label_cols, extra_label_col))
  cols_for_labels <- cols_for_labels[cols_for_labels %in% names(dt_all)]
  label_data <- if (length(cols_for_labels)) {
    dt_all[rows_kept, ..cols_for_labels]
  } else {
    data.table()
  }
  
  pca_df <- cbind(data.table(pc_scores), label_data)
  
  list(
    pc_loadings   = pc_loadings,
    pc_scores     = pc_scores,
    var_explained = var_explained,
    pca_df        = pca_df,
    meta = list(
      numeric_cols = numeric_cols,
      rows_kept    = rows_kept,
      n_rows_used  = sum(rows_kept),
      n_cols_used  = length(numeric_cols)
    )
  )
}

# ------------------------------------------------
# 4) Visualization
# ------------------------------------------------

#' Plot first two PCs, colored by a label column in pca_df.
#' If many unique labels (> 9), uses viridis discrete palette.
plot_pca_results <- function(pca_result, label_col) {
  dt <- copy(pca_result$pca_df)
  
  if (!label_col %in% names(dt)) {
    stop(sprintf("Label column '%s' not found in PCA results.", label_col))
  }
  
  # If label is numeric, coerce to factor for coloring
  if (is.numeric(dt[[label_col]])) {
    dt[[label_col]] <- factor(dt[[label_col]])
  }
  
  num_labels <- length(unique(dt[[label_col]]))
  
  p <- ggplot(
    dt,
    aes(
      x     = .data$PC1,
      y     = .data$PC2,
      color = .data[[label_col]]
    )
  ) +
    geom_point(alpha = 0.8, size = 2) +
    ggtitle("PCA Results") +
    xlab(sprintf("PC1 (%.2f%% variance)", 100 * pca_result$var_explained[1])) +
    ylab(sprintf("PC2 (%.2f%% variance)", 100 * pca_result$var_explained[2]))
  
  if (num_labels > 9L) {
    warning("More than 9 unique labels; using viridis discrete palette.")
    p <- p + scale_color_viridis_d()
  } else {
    p <- p + scale_color_brewer(palette = "Set1")
  }
  
  print(p)
  invisible(p)
}

# ------------------------------------------------
# 5) Public Wrapper
# ------------------------------------------------

#' Full PCA analysis wrapper
#'
#' @param data        data.frame or data.table
#' @param num_pc      number of PCs to retain (integer >= 1; default 2)
#' @param scale_data  logical; scale numeric cols
#' @param center_data logical; center numeric cols
#' @param label_col   optional column name to attach/plot (can be numeric or
#'                    non-numeric). This column is excluded from PCA features
#'                    and only used as a label.
#' @param plot        logical; if TRUE and label_col supplied, plot PC1 vs PC2
#'
#' @return list with pc_loadings, pc_scores, var_explained, pca_df, meta
#'
#' @examples
#' # Basic use
#' res <- fs_pca(mtcars, num_pc = 2, label_col = "cyl", plot = TRUE)
fs_pca <- function(data,
                   num_pc      = 2L,
                   scale_data  = TRUE,
                   center_data = TRUE,
                   label_col   = NULL,
                   plot        = !is.null(label_col)) {
  check_data_validity(data)
  
  # Validate num_pc
  if (!is.numeric(num_pc) || length(num_pc) != 1L || is.na(num_pc) || num_pc < 1L) {
    stop("`num_pc` must be a single numeric value >= 1.")
  }
  num_pc <- as.integer(num_pc)
  
  # Validate label_col if provided
  if (!is.null(label_col) && !label_col %in% names(data)) {
    stop(sprintf("`label_col` '%s' not found in data.", label_col))
  }
  
  # Non-numeric label columns auto-detected;
  # label_col (even if numeric) treated as a label and excluded from PCA features
  base_label_cols <- identify_label_cols(data)
  extra_label     <- if (!is.null(label_col)) label_col else NULL
  all_label_cols  <- unique(c(base_label_cols, extra_label))
  
  # Compute PCA (keeps only complete rows for numeric subset)
  pca_fit <- .perform_pca(
    data        = data,
    label_cols  = all_label_cols,
    num_pc      = num_pc,
    scale_data  = scale_data,
    center_data = center_data
  )
  
  # Build tidy results; include extra label col
  results <- .create_pca_results(
    pca_fit,
    num_pc          = num_pc,
    data            = data,
    label_cols      = all_label_cols,
    extra_label_col = extra_label
  )
  
  # Optional plot if label_col provided and we have at least 2 PCs
  if (plot) {
    if (is.null(label_col)) {
      warning("plot = TRUE but no label_col provided; skipping plot.")
    } else if (num_pc < 2L) {
      warning("plot = TRUE requires at least 2 PCs; skipping plot.")
    } else {
      plot_pca_results(results, label_col = label_col)
    }
  }
  
  results
}
