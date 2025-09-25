# =========================
# Package Utilities
# =========================

#' Ensure a Package is Installed and Loaded
#'
#' @param pkg Character scalar package name.
#' @return Invisibly TRUE if loaded.
ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Package '%s' not found. Installing...", pkg))
    install.packages(pkg, dependencies = TRUE)
  }
  if (!suppressPackageStartupMessages(require(pkg, character.only = TRUE, quietly = TRUE))) {
    stop(sprintf("Package '%s' failed to load.", pkg), call. = FALSE)
  }
  invisible(TRUE)
}

# =========================
# Validation & Coercion
# =========================

#' Validate a single non-negative, finite numeric scalar
.validate_threshold <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    stop("`threshold` must be a single non-negative, finite numeric value.", call. = FALSE)
  }
}

#' Match and validate action argument
.validate_action <- function(action) {
  action <- match.arg(action, c("keep", "remove"))
  action
}

#' Match and validate threshold direction
.validate_direction <- function(direction) {
  direction <- match.arg(direction, c("above", "below"))
  direction
}

#' Convert Input Data to data.table with numeric-only columns
#'
#' @param data Numeric matrix, data.frame, or data.table.
#' @param log_progress Logical for progress messages.
#' @return data.table with numeric columns.
convert_to_datatable <- function(data, log_progress = FALSE) {
  ensure_package("data.table")
  
  # data.table already?
  if (data.table::is.data.table(data)) {
    if (!all(vapply(data, is.numeric, logical(1)))) {
      stop("All columns of the data.table must be numeric.", call. = FALSE)
    }
    if (log_progress) message("Input is already a data.table.")
    return(data)
  }
  
  # data.frame?
  if (is.data.frame(data)) {
    if (!all(vapply(data, is.numeric, logical(1)))) {
      stop("All columns of the data frame must be numeric.", call. = FALSE)
    }
    if (log_progress) message("Converting data frame to data.table...")
    dt <- data.table::as.data.table(data)
    return(dt)
  }
  
  # matrix?
  if (is.matrix(data)) {
    if (!is.numeric(data)) {
      stop("Matrix data must be numeric.", call. = FALSE)
    }
    if (log_progress) message("Converting numeric matrix to data.table...")
    # Preserve column names or create them if absent
    if (is.null(colnames(data))) {
      colnames(data) <- sprintf("V%d", seq_len(ncol(data)))
    }
    dt <- data.table::as.data.table(as.data.frame(data))
    # Ensure numeric after conversion
    if (!all(vapply(dt, is.numeric, logical(1)))) {
      stop("Converted matrix contains non-numeric columns unexpectedly.", call. = FALSE)
    }
    return(dt)
  }
  
  stop("`data` must be a numeric matrix, data frame, or data.table.", call. = FALSE)
}

# =========================
# Core Computations
# =========================

#' Compute Feature Variances
#'
#' @param dt data.table with numeric columns.
#' @param na_rm Logical; if TRUE, remove NAs when computing variance.
#' @param log_progress Logical for progress messages.
#' @return Named numeric vector of variances (length = ncol(dt)).
compute_feature_variances <- function(dt, na_rm = TRUE, log_progress = FALSE) {
  if (!data.table::is.data.table(dt)) stop("`dt` must be a data.table.", call. = FALSE)
  if (!all(vapply(dt, is.numeric, logical(1)))) {
    stop("All columns of `dt` must be numeric.", call. = FALSE)
  }
  if (log_progress) message("Calculating feature variances...")
  v <- dt[, lapply(.SD, stats::var, na.rm = na_rm)]
  v <- unlist(v, use.names = TRUE)
  # Replace NA variances (e.g., all NA in a column) with NA_real_
  v[is.na(v)] <- NA_real_
  v
}

#' Build Selection Mask from Variances
#'
#' @param variances Named numeric vector.
#' @param threshold Numeric scalar >= 0.
#' @param direction "above" or "below".
#' @param action "keep" or "remove".
#' @param include_equal Logical; if TRUE, use >= / <= instead of > / <.
#' @return Logical vector mask (length = length(variances)) indicating columns to KEEP.
selection_mask_from_variances <- function(variances,
                                          threshold,
                                          direction = c("above", "below"),
                                          action    = c("keep", "remove"),
                                          include_equal = FALSE) {
  .validate_threshold(threshold)
  direction <- .validate_direction(direction)
  action    <- .validate_action(action)
  
  # Compare, handling NA variances (treat as not meeting condition)
  cmp <- switch(
    paste(direction, include_equal),
    "above FALSE" = variances >  threshold,
    "above TRUE"  = variances >= threshold,
    "below FALSE" = variances <  threshold,
    "below TRUE"  = variances <= threshold
  )
  cmp[is.na(cmp)] <- FALSE
  
  # If action == "keep", we keep columns meeting condition; if "remove", keep those NOT meeting condition.
  keep_mask <- if (action == "keep") cmp else !cmp
  keep_mask
}

# =========================
# Public API
# =========================

#' Variance Thresholding for Feature Selection
#'
#' @param data Numeric matrix, data.frame, or data.table (all numeric columns).
#' @param threshold Non-negative, finite numeric scalar. Default 0.5.
#' @param direction One of "above", "below". Default "above".
#' @param action One of "keep", "remove". Default "keep".
#' @param include_equal Logical; include equality at the threshold (>= or <=). Default FALSE.
#' @param na_rm Logical; pass to variance computation (TRUE removes NAs). Default TRUE.
#' @param return One of "matrix","dt","data.frame","mask","indices","names","list".
#'   - "list" returns: filtered (matrix), mask, indices, names, variances, meta.
#'   Default is "matrix".
#' @param log_progress Logical; print progress messages. Default FALSE.
#' @return Depends on `return`.
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(1000), ncol = 10)
#' # Keep variance > 0.5 (strict)
#' m <- fs_variance(
#'   X, threshold = 0.5, direction = "above",
#'   action = "keep", include_equal = FALSE, return = "matrix"
#' )
#' # Return full details
#' out <- fs_variance(X, threshold = 0.5, return = "list")
fs_variance <- function(data,
                        threshold      = 0.5,
                        direction      = c("above", "below"),
                        action         = c("keep", "remove"),
                        include_equal  = FALSE,
                        na_rm          = TRUE,
                        return         = c("matrix", "dt", "data.frame", "mask", "indices", "names", "list"),
                        log_progress   = FALSE) {
  .validate_threshold(threshold)
  direction <- .validate_direction(direction[1])
  action    <- .validate_action(action[1])
  return    <- match.arg(return)
  
  # Convert and check
  dt <- convert_to_datatable(data, log_progress = log_progress)
  
  # Compute variances
  variances <- compute_feature_variances(dt, na_rm = na_rm, log_progress = log_progress)
  
  # Build selection mask
  mask <- selection_mask_from_variances(
    variances = variances,
    threshold = threshold,
    direction = direction,
    action    = action,
    include_equal = include_equal
  )
  
  # Indices and names to keep
  keep_idx   <- which(mask)
  keep_names <- names(variances)[keep_idx]
  
  if (length(keep_idx) == 0L) {
    warning("No features meet the specified variance criteria.")
    # Honor requested return type
    if (return == "mask")    return(mask)
    if (return == "indices") return(integer(0))
    if (return == "names")   return(character(0))
    if (return == "dt")      return(dt[, .SD, .SDcols = keep_names]) # empty dt
    if (return == "data.frame") return(as.data.frame(dt[, .SD, .SDcols = keep_names]))
    if (return == "list") {
      return(list(
        filtered  = matrix(numeric(0), nrow = nrow(dt), ncol = 0),
        mask      = mask,
        indices   = keep_idx,
        names     = keep_names,
        variances = variances,
        meta = list(
          threshold = threshold,
          direction = direction,
          action    = action,
          include_equal = include_equal,
          na_rm = na_rm,
          n_input_cols = ncol(dt),
          n_kept_cols  = 0L
        )
      ))
    }
    # "matrix" default
    return(matrix(numeric(0), nrow = nrow(dt), ncol = 0,
                  dimnames = list(NULL, character(0))))
  }
  
  # Subset
  filtered_dt <- dt[, ..keep_names]
  
  if (log_progress) {
    op   <- if (direction == "above") ">" else "<"
    if (include_equal) op <- paste0(op, "=")
    verb <- if (action == "keep") "Retaining" else "Removing"
    message(sprintf("%s features with variance %s %s", verb, op, threshold))
    message(sprintf("Kept %d of %d features.", length(keep_idx), ncol(dt)))
  }
  
  # Return shapes
  switch(return,
         "dt"         = filtered_dt,
         "data.frame" = as.data.frame(filtered_dt),
         "mask"       = mask,
         "indices"    = keep_idx,
         "names"      = keep_names,
         "list"       = {
           list(
             filtered  = as.matrix(filtered_dt),
             mask      = mask,
             indices   = keep_idx,
             names     = keep_names,
             variances = variances,
             meta = list(
               threshold = threshold,
               direction = direction,
               action    = action,
               include_equal = include_equal,
               na_rm = na_rm,
               n_input_cols = ncol(dt),
               n_kept_cols  = length(keep_idx)
             )
           )
         },
         # default "matrix"
         as.matrix(filtered_dt)
  )
}
