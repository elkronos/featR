# =========================
# Package Utilities
# =========================

#' Ensure a Package is Installed and Loaded
#'
#' NOTE: This helper will attempt to install the package if it is missing.
#' For package code, you may prefer to remove the install step and rely
#' solely on DESCRIPTION dependencies.
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
# Validation Helpers
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
# Unsupervised Scores
# =========================

#' Compute unsupervised per-feature scores
#'
#' Methods:
#' - "variance": sample variance
#' - "mad": median absolute deviation
#' - "iqr": interquartile range
#' - "range": max - min
#' - "missing_prop": proportion of missing values
#' - "n_unique": number of unique non-NA values
#'
#' @param dt data.table of numeric features.
#' @param method One of "variance","mad","iqr","range","missing_prop","n_unique".
#' @param na_rm Logical; if TRUE, remove NAs when computing scores (where applicable).
#' @param log_progress Logical; progress messages.
#' @return Named numeric vector of scores (length = ncol(dt)).
compute_unsupervised_scores <- function(dt,
                                        method       = c("variance",
                                                         "mad",
                                                         "iqr",
                                                         "range",
                                                         "missing_prop",
                                                         "n_unique"),
                                        na_rm        = TRUE,
                                        log_progress = FALSE) {
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data.table.", call. = FALSE)
  }
  if (!all(vapply(dt, is.numeric, logical(1)))) {
    stop("All columns of `dt` must be numeric.", call. = FALSE)
  }
  
  .validate_logical_flag(na_rm, "na_rm")
  
  method <- match.arg(method)
  
  if (log_progress) {
    message(sprintf("Computing unsupervised feature scores using method '%s'...", method))
  }
  
  scores <- switch(
    method,
    
    "variance" = {
      # Sample variance (denominator n - 1)
      vapply(
        dt,
        function(col) stats::var(col, na.rm = na_rm),
        numeric(1)
      )
    },
    
    "mad" = {
      # Median absolute deviation (robust spread)
      vapply(
        dt,
        function(col) stats::mad(col, na.rm = na_rm),
        numeric(1)
      )
    },
    
    "iqr" = {
      # Interquartile range
      vapply(
        dt,
        function(col) stats::IQR(col, na.rm = na_rm),
        numeric(1)
      )
    },
    
    "range" = {
      # Max - Min
      vapply(
        dt,
        function(col) {
          if (na_rm) {
            col2 <- col[!is.na(col)]
            if (length(col2) == 0L) return(NA_real_)
            r <- range(col2)
          } else {
            if (all(is.na(col))) return(NA_real_)
            r <- range(col)
          }
          diff(r)
        },
        numeric(1)
      )
    },
    
    "missing_prop" = {
      # Proportion of missing values in each column
      vapply(
        dt,
        function(col) {
          n <- length(col)
          if (n == 0L) return(NA_real_)
          sum(is.na(col)) / n
        },
        numeric(1)
      )
    },
    
    "n_unique" = {
      # Number of unique non-NA values in each column
      vapply(
        dt,
        function(col) {
          length(unique(col[!is.na(col)]))
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
  
  cmp[is.na(cmp)] <- FALSE
  
  if (action == "keep") cmp else !cmp
}

# =========================
# Public API: fs_unsupervised
# =========================

#' Unsupervised Filter-Based Feature Selection
#'
#' Performs unsupervised, univariate, filter-based feature selection by scoring
#' each feature using a chosen unsupervised criterion and selecting/dropping
#' features based on a threshold on that score.
#'
#' Supported methods:
#' \itemize{
#'   \item \code{"variance"}: Sample variance.
#'   \item \code{"mad"}: Median absolute deviation.
#'   \item \code{"iqr"}: Interquartile range.
#'   \item \code{"range"}: Max - Min.
#'   \item \code{"missing_prop"}: Proportion of missing values.
#'   \item \code{"n_unique"}: Number of unique non-NA values.
#' }
#'
#' @param x Numeric matrix, data.frame, or data.table (all numeric columns).
#' @param method One of \code{"variance"}, \code{"mad"}, \code{"iqr"},
#'   \code{"range"}, \code{"missing_prop"}, \code{"n_unique"}.
#' @param threshold Non-negative, finite numeric scalar threshold applied to
#'   the feature scores.
#' @param direction One of \code{"above"}, \code{"below"}; compares scores to
#'   \code{threshold}.
#' @param action One of \code{"keep"}, \code{"remove"}; determines whether
#'   features meeting the condition are retained or dropped.
#' @param include_equal Logical; if TRUE, comparisons use >= / <= instead of > / <.
#' @param na_rm Logical; if TRUE, remove NAs when computing scores (where applicable).
#' @param output One of \code{"matrix"}, \code{"dt"}, \code{"data.frame"},
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
#' @return Depends on `output` argument (see above).
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(200), ncol = 5)
#'
#' # Keep features with variance > 0.5
#' out_var <- fs_unsupervised(
#'   x = X,
#'   method = "variance",
#'   threshold = 0.5,
#'   direction = "above",
#'   action = "keep",
#'   output = "list"
#' )
#'
#' # Remove features with missing proportion >= 0.2
#' X_na <- X
#' X_na[sample(length(X_na), 20)] <- NA
#' out_missing <- fs_unsupervised(
#'   x = X_na,
#'   method = "missing_prop",
#'   threshold = 0.2,
#'   direction = "above",
#'   action = "remove",
#'   include_equal = TRUE,
#'   output = "matrix"
#' )
fs_unsupervised <- function(x,
                            method        = c("variance",
                                              "mad",
                                              "iqr",
                                              "range",
                                              "missing_prop",
                                              "n_unique"),
                            threshold     = 0,
                            direction     = c("above", "below"),
                            action        = c("keep", "remove"),
                            include_equal = FALSE,
                            na_rm         = TRUE,
                            output        = c("matrix", "dt", "data.frame",
                                              "mask", "indices", "names", "list"),
                            log_progress  = FALSE) {
  method <- match.arg(method)
  output <- match.arg(output)
  
  .validate_threshold(threshold, name = "threshold")
  .validate_logical_flag(include_equal, "include_equal")
  .validate_logical_flag(na_rm, "na_rm")
  
  # Convert features to data.table
  dt <- convert_to_datatable(x, log_progress = log_progress)
  
  # Compute unsupervised scores
  scores <- compute_unsupervised_scores(
    dt           = dt,
    method       = method,
    na_rm        = na_rm,
    log_progress = log_progress
  )
  
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
    warning("No features meet the specified unsupervised selection criteria.")
    
    if (output == "mask")       return(mask)
    if (output == "indices")    return(integer(0))
    if (output == "names")      return(character(0))
    
    if (output == "dt") {
      empty_dt <- data.table::as.data.table(
        matrix(
          numeric(0),
          nrow     = nrow(dt),
          ncol     = 0,
          dimnames = list(NULL, character(0))
        )
      )
      return(empty_dt)
    }
    
    if (output == "data.frame") {
      empty_df <- as.data.frame(
        matrix(
          numeric(0),
          nrow     = nrow(dt),
          ncol     = 0,
          dimnames = list(NULL, character(0))
        )
      )
      return(empty_df)
    }
    
    if (output == "list") {
      return(list(
        filtered = matrix(
          numeric(0),
          nrow     = nrow(dt),
          ncol     = 0,
          dimnames = list(NULL, character(0))
        ),
        mask     = mask,
        indices  = keep_idx,
        names    = keep_names,
        scores   = scores,
        meta = list(
          method        = method,
          threshold     = threshold,
          direction     = .validate_direction(direction),
          action        = .validate_action(action),
          include_equal = include_equal,
          na_rm         = na_rm,
          n_input_cols  = ncol(dt),
          n_kept_cols   = 0L
        )
      ))
    }
    # "matrix" default
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
    direction_resolved <- .validate_direction(direction)
    action_resolved    <- .validate_action(action)
    
    op <- if (direction_resolved == "above") ">" else "<"
    if (include_equal) op <- paste0(op, "=")
    verb <- if (action_resolved == "keep") "Retaining" else "Removing"
    
    message(sprintf(
      "%s features with unsupervised score %s %s",
      verb, op, threshold
    ))
    message(sprintf("Kept %d of %d features.", length(keep_idx), ncol(dt)))
  }
  
  # Return shapes
  switch(
    output,
    "dt"         = filtered_dt,
    "data.frame" = as.data.frame(filtered_dt),
    "mask"       = mask,
    "indices"    = keep_idx,
    "names"      = keep_names,
    "list"       = {
      direction_resolved <- .validate_direction(direction)
      action_resolved    <- .validate_action(action)
      list(
        filtered = as.matrix(filtered_dt),
        mask     = mask,
        indices  = keep_idx,
        names    = keep_names,
        scores   = scores,
        meta = list(
          method        = method,
          threshold     = threshold,
          direction     = direction_resolved,
          action        = action_resolved,
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
