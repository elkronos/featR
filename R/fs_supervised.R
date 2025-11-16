# =========================
# Package Utilities
# =========================

#' Ensure a Package is Installed and Loaded
#'
#' @param pkg Character scalar package name.
#' @return Invisibly TRUE if loaded.
ensure_package <- function(pkg) {
  if (!is.character(pkg) || length(pkg) != 1L || is.na(pkg) || !nzchar(pkg)) {
    stop("`pkg` must be a non-empty character scalar.", call. = FALSE)
  }
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Package '%s' not found. Installing...", pkg))
    tryCatch(
      install.packages(pkg, dependencies = TRUE),
      error = function(e) {
        stop(
          sprintf("Package '%s' could not be installed: %s",
                  pkg, conditionMessage(e)),
          call. = FALSE
        )
      }
    )
  }
  
  if (!suppressPackageStartupMessages(
    require(pkg, character.only = TRUE, quietly = TRUE)
  )) {
    stop(sprintf("Package '%s' failed to load.", pkg), call. = FALSE)
  }
  
  invisible(TRUE)
}

# =========================
# Validation & Coercion
# =========================

#' Validate a single non-negative, finite numeric scalar
.validate_threshold <- function(x, name = "threshold") {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    stop(sprintf("`%s` must be a single non-negative, finite numeric value.", name),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate a logical flag (single, non-NA)
.validate_logical_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a single non-NA logical value.", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Match and validate action argument
.validate_action <- function(action) {
  if (length(action) == 0L) {
    stop("`action` must be specified.", call. = FALSE)
  }
  match.arg(action[1L], c("keep", "remove"))
}

#' Match and validate threshold direction
.validate_direction <- function(direction) {
  if (length(direction) == 0L) {
    stop("`direction` must be specified.", call. = FALSE)
  }
  match.arg(direction[1L], c("above", "below"))
}

# =========================
# Data Conversion
# =========================

#' Convert Input Data to data.table with numeric-only columns
#'
#' @param x Numeric matrix, data.frame, or data.table.
#' @param log_progress Logical for progress messages.
#' @return data.table with numeric columns.
convert_to_datatable <- function(x, log_progress = FALSE) {
  ensure_package("data.table")
  
  # data.table already?
  if (data.table::is.data.table(x)) {
    if (!all(vapply(x, is.numeric, logical(1)))) {
      stop("All columns of the data.table must be numeric.", call. = FALSE)
    }
    if (log_progress) message("Input is already a data.table.")
    return(x)
  }
  
  # data.frame?
  if (is.data.frame(x)) {
    if (!all(vapply(x, is.numeric, logical(1)))) {
      stop("All columns of the data frame must be numeric.", call. = FALSE)
    }
    if (log_progress) message("Converting data frame to data.table...")
    dt <- data.table::as.data.table(x)
    return(dt)
  }
  
  # matrix?
  if (is.matrix(x)) {
    if (!is.numeric(x)) {
      stop("Matrix data must be numeric.", call. = FALSE)
    }
    if (log_progress) message("Converting numeric matrix to data.table...")
    if (is.null(colnames(x))) {
      colnames(x) <- sprintf("V%d", seq_len(ncol(x)))
    }
    dt <- data.table::as.data.table(x)
    if (!all(vapply(dt, is.numeric, logical(1)))) {
      stop("Converted matrix contains non-numeric columns unexpectedly.", call. = FALSE)
    }
    return(dt)
  }
  
  stop("`x` must be a numeric matrix, data frame, or data.table.", call. = FALSE)
}

# =========================
# Supervised Scores
# =========================

#' Compute supervised per-feature scores given x and y
#'
#' @param dt data.table of numeric features.
#' @param y Target vector.
#' @param method One of "auto", "correlation", "anova".
#' @param na_rm Logical; if TRUE, drop rows with NA in x or y for scoring.
#' @param log_progress Logical; progress messages.
#' @return Named numeric vector of scores (length = ncol(dt)).
compute_supervised_scores <- function(dt,
                                      y,
                                      method       = c("auto", "correlation", "anova"),
                                      na_rm        = TRUE,
                                      log_progress = FALSE) {
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data.table.", call. = FALSE)
  }
  if (!all(vapply(dt, is.numeric, logical(1)))) {
    stop("All columns of `dt` must be numeric.", call. = FALSE)
  }
  
  .validate_logical_flag(na_rm, "na_rm")
  
  # Validate y
  if (is.matrix(y) || is.data.frame(y)) {
    stop("`y` must be a vector, not a matrix or data frame.", call. = FALSE)
  }
  if (length(y) != nrow(dt)) {
    stop("Length of `y` must equal number of rows in `x`.", call. = FALSE)
  }
  
  # Determine target type
  is_y_numeric <- is.numeric(y)
  is_y_categorical <- is.factor(y) || is.character(y) || is.logical(y)
  
  if (!is_y_numeric && !is_y_categorical) {
    stop("`y` must be numeric, factor, character, or logical.", call. = FALSE)
  }
  
  method <- match.arg(method)
  
  if (method == "auto") {
    method <- if (is_y_numeric) "correlation" else "anova"
  }
  
  if (method == "correlation" && !is_y_numeric) {
    stop("`method = \"correlation\"` requires numeric `y`.", call. = FALSE)
  }
  if (method == "anova" && !is_y_categorical) {
    stop("`method = \"anova\"` requires categorical `y` (factor/character/logical).",
         call. = FALSE)
  }
  
  if (log_progress) {
    message(sprintf("Computing supervised feature scores using method '%s'...", method))
  }
  
  scores <- switch(
    method,
    
    "correlation" = {
      # Absolute Pearson correlation between each feature and numeric y
      y_num <- as.numeric(y)
      vapply(
        dt,
        function(col) {
          if (na_rm) {
            idx  <- !is.na(col) & !is.na(y_num)
            col2 <- col[idx]
            y2   <- y_num[idx]
            if (length(col2) < 2L || stats::var(col2) == 0 || stats::var(y2) == 0) {
              return(NA_real_)
            }
            val <- stats::cor(col2, y2)
          } else {
            # let cor() handle NAs according to 'use', then sanitize result
            val <- stats::cor(col, y_num, use = "everything")
          }
          
          # Robustly handle NA / non-finite correlations
          if (is.na(val) || !is.finite(val)) return(NA_real_)
          abs(val)
        },
        numeric(1)
      )
    },
    
    "anova" = {
      # One-way ANOVA F-statistic for each feature vs categorical y
      y_fac <- as.factor(y)
      vapply(
        dt,
        function(col) {
          if (na_rm) {
            idx  <- !is.na(col) & !is.na(y_fac)
            col2 <- col[idx]
            y2   <- y_fac[idx]
          } else {
            col2 <- col
            y2   <- y_fac
          }
          
          if (length(col2) < 2L || length(unique(y2)) < 2L) {
            return(NA_real_)
          }
          
          df <- data.frame(x = col2, y = y2)
          fit <- tryCatch(
            stats::lm(x ~ y, data = df),
            error = function(e) NULL
          )
          if (is.null(fit)) return(NA_real_)
          
          a <- tryCatch(
            stats::anova(fit),
            error = function(e) NULL
          )
          if (is.null(a) || nrow(a) < 1L) return(NA_real_)
          
          val <- a$`F value`[1L]
          if (!is.numeric(val) || length(val) != 1L || !is.finite(val)) {
            return(NA_real_)
          }
          val
        },
        numeric(1)
      )
    }
  )
  
  scores <- as.numeric(scores)
  names(scores) <- names(dt)
  scores
}

# =========================
# Selection Mask Utilities
# =========================

#' Build Selection Mask from Scores
#'
#' @param scores Named numeric vector of feature scores.
#' @param threshold Non-negative numeric scalar threshold.
#' @param direction "above" or "below".
#' @param action "keep" or "remove".
#' @param include_equal Logical; if TRUE, use >= / <= instead of > / <.
#' @return Logical vector mask (length = length(scores)) indicating columns to KEEP.
.selection_mask_from_scores <- function(scores,
                                        threshold,
                                        direction     = c("above", "below"),
                                        action        = c("keep", "remove"),
                                        include_equal = FALSE) {
  .validate_threshold(threshold, name = "threshold")
  direction <- .validate_direction(direction)
  action    <- .validate_action(action)
  .validate_logical_flag(include_equal, "include_equal")
  
  key <- paste(direction, include_equal)
  
  cmp <- switch(
    key,
    "above FALSE" = scores >  threshold,
    "above TRUE"  = scores >= threshold,
    "below FALSE" = scores <  threshold,
    "below TRUE"  = scores <= threshold,
    stop("Unexpected combination of `direction` and `include_equal`.", call. = FALSE)
  )
  
  # NA scores never meet the condition: they are treated as FALSE.
  cmp[is.na(cmp)] <- FALSE
  
  # For action = "keep", TRUE = keep; for "remove", invert.
  if (action == "keep") cmp else !cmp
}

# =========================
# Public API: fs_supervised
# =========================

#' Supervised Filter-Based Feature Selection
#'
#' Performs supervised, univariate, filter-based feature selection by scoring each
#' feature with respect to a target `y` and selecting/dropping features based on
#' a threshold on the score.
#'
#' Supported methods:
#' \itemize{
#'   \item \code{"correlation"}: Absolute Pearson correlation (numeric target).
#'   \item \code{"anova"}: One-way ANOVA F-statistic (categorical target).
#'   \item \code{"auto"}: Choose \code{"correlation"} for numeric `y`,
#'         \code{"anova"} for categorical `y`.
#' }
#'
#' @param x Numeric matrix, data.frame, or data.table (all numeric columns).
#' @param y Target vector (numeric, factor, character, or logical).
#' @param method One of \code{"auto"}, \code{"correlation"}, \code{"anova"}.
#' @param threshold Non-negative, finite numeric scalar threshold applied to
#'   the feature scores (not to `y` directly). Default 0.
#' @param direction One of \code{"above"}, \code{"below"}; compares scores to
#'   \code{threshold}.
#' @param action One of \code{"keep"}, \code{"remove"}; determines whether
#'   features meeting the condition are retained or dropped.
#' @param include_equal Logical; if TRUE, comparisons use >= / <= instead of > / <.
#' @param na_rm Logical; if TRUE, rows with NA in `x` or `y` are dropped when
#'   computing scores.
#' @param out One of \code{"matrix"}, \code{"dt"}, \code{"data.frame"},
#'   \code{"mask"}, \code{"indices"}, \code{"names"}, \code{"list"}.
#'   \itemize{
#'     \item \code{"matrix"} (default): numeric matrix of selected features.
#'     \item \code{"dt"}: data.table of selected features.
#'     \item \code{"data.frame"}: data.frame of selected features.
#'     \item \code{"mask"}: logical vector of length \code{ncol(x)}.
#'     \item \code{"indices"}: integer vector of selected column indices.
#'     \item \code{"names"}: character vector of selected column names.
#'     \item \code{"list"}: list with components:
#'       \code{filtered} (matrix), \code{mask}, \code{indices}, \code{names},
#'       \code{scores}, and \code{meta}.
#'   }
#' @param log_progress Logical; print progress messages.
#'
#' @return Depends on `out` argument (see above).
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(200), ncol = 5)
#' y_num <- rnorm(nrow(X))
#' y_fac <- factor(sample(letters[1:3], nrow(X), replace = TRUE))
#'
#' # Correlation-based selection (numeric y)
#' out_corr <- fs_supervised(
#'   x = X, y = y_num,
#'   method = "correlation",
#'   threshold = 0.3,
#'   direction = "above",
#'   action = "keep",
#'   out = "list"
#' )
#'
#' # ANOVA-based selection (factor y)
#' out_anova <- fs_supervised(
#'   x = X, y = y_fac,
#'   method = "anova",
#'   threshold = 1.0,
#'   direction = "above",
#'   action = "keep",
#'   out = "matrix"
#' )
fs_supervised <- function(x,
                          y,
                          method        = c("auto", "correlation", "anova"),
                          threshold     = 0,
                          direction     = c("above", "below"),
                          action        = c("keep", "remove"),
                          include_equal = FALSE,
                          na_rm         = TRUE,
                          out           = c("matrix", "dt", "data.frame",
                                            "mask", "indices", "names", "list"),
                          log_progress  = FALSE) {
  .validate_threshold(threshold, name = "threshold")
  .validate_logical_flag(include_equal, "include_equal")
  .validate_logical_flag(na_rm, "na_rm")
  
  direction  <- .validate_direction(direction)
  action     <- .validate_action(action)
  out        <- match.arg(out)
  method_arg <- match.arg(method)
  
  # Convert features to data.table
  dt <- convert_to_datatable(x, log_progress = log_progress)
  
  # Compute supervised scores (handles type checking & method "auto" logic)
  scores <- compute_supervised_scores(
    dt           = dt,
    y            = y,
    method       = method_arg,
    na_rm        = na_rm,
    log_progress = log_progress
  )
  
  # Determine what method was effectively used, for metadata
  is_y_numeric     <- is.numeric(y)
  is_y_categorical <- is.factor(y) || is.character(y) || is.logical(y)
  method_used <- if (method_arg == "auto") {
    if (is_y_numeric) "correlation" else "anova"
  } else {
    method_arg
  }
  
  # Build selection mask from scores
  mask <- .selection_mask_from_scores(
    scores        = scores,
    threshold     = threshold,
    direction     = direction,
    action        = action,
    include_equal = include_equal
  )
  
  # Indices and names to keep
  keep_idx   <- which(mask)
  keep_names <- names(scores)[keep_idx]
  
  # Handle case: no features kept
  if (length(keep_idx) == 0L) {
    warning("No features meet the specified supervised selection criteria.")
    
    if (out == "mask")       return(mask)
    if (out == "indices")    return(integer(0))
    if (out == "names")      return(character(0))
    if (out == "dt")         return(dt[, .SD, .SDcols = keep_names]) # empty dt
    if (out == "data.frame") return(as.data.frame(dt[, .SD, .SDcols = keep_names]))
    if (out == "list") {
      return(list(
        filtered = matrix(numeric(0), nrow = nrow(dt), ncol = 0),
        mask     = mask,
        indices  = keep_idx,
        names    = keep_names,
        scores   = scores,
        meta = list(
          method_arg    = method_arg,
          method_used   = method_used,
          threshold     = threshold,
          direction     = direction,
          action        = action,
          include_equal = include_equal,
          na_rm         = na_rm,
          n_input_cols  = ncol(dt),
          n_kept_cols   = 0L
        )
      ))
    }
    # default "matrix"
    return(matrix(
      numeric(0),
      nrow     = nrow(dt),
      ncol     = 0,
      dimnames = list(NULL, character(0))
    ))
  }
  
  # Subset
  filtered_dt <- dt[, ..keep_names]
  
  if (log_progress) {
    op <- if (direction == "above") ">" else "<"
    if (include_equal) op <- paste0(op, "=")
    verb <- if (action == "keep") "Retaining" else "Removing"
    message(sprintf(
      "%s features with supervised score %s %s",
      verb, op, threshold
    ))
    message(sprintf("Kept %d of %d features.", length(keep_idx), ncol(dt)))
  }
  
  # Return shapes
  switch(
    out,
    "dt"         = filtered_dt,
    "data.frame" = as.data.frame(filtered_dt),
    "mask"       = mask,
    "indices"    = keep_idx,
    "names"      = keep_names,
    "list"       = {
      list(
        filtered = as.matrix(filtered_dt),
        mask     = mask,
        indices  = keep_idx,
        names    = keep_names,
        scores   = scores,
        meta = list(
          method_arg    = method_arg,
          method_used   = method_used,
          threshold     = threshold,
          direction     = direction,
          action        = action,
          include_equal = include_equal,
          na_rm         = na_rm,
          n_input_cols  = ncol(dt),
          n_kept_cols   = length(keep_idx)
        )
      )
    },
    # default "matrix"
    as.matrix(filtered_dt)
  )
}
