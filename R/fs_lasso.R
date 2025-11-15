#' -----------------------------------------------------------------------------
#' Lasso with Cross-Validation and Feature Importance
#' -----------------------------------------------------------------------------
#' Utilities to validate inputs, prepare predictors, fit a glmnet cv model
#' (lasso / elastic net), and extract variable importance.
#'
#' Requires the packages: Matrix, glmnet, stats, parallel, doParallel, foreach
#' -----------------------------------------------------------------------------

#' Validate Input Parameters
#'
#' Checks the user-provided parameters for consistency and correctness.
#'
#' @param x A data frame or matrix of predictor variables.
#' @param y A numeric vector of response values (no NA/NaN/Inf).
#' @param alpha A numeric value in (0, 1]; 1 = lasso, (0,1) = elastic net.
#' @param nfolds Integer > 1 specifying the number of CV folds.
#' @param standardize Logical; whether glmnet should standardize predictors.
#' @param parallel Logical; whether to use a parallel backend for CV.
#' @param verbose Logical; whether to print progress messages.
#' @param seed Either NULL or a single integer for reproducibility.
#' @param custom_folds Optional integer vector of custom fold IDs (same length as y).
#' @param return_model Logical; whether to return the fitted cv.glmnet object.
#'
#' @return Invisibly returns TRUE if all parameters are valid; otherwise errors.
#' @keywords internal
validate_parameters <- function(x, y, alpha, nfolds, standardize,
                                parallel, verbose, seed, custom_folds,
                                return_model) {
  # x must be data.frame or matrix
  if (!inherits(x, c("data.frame", "matrix"))) {
    stop("Error: 'x' should be a data frame or matrix.")
  }
  
  # x must have at least one column
  if (NCOL(x) == 0L) {
    stop("Error: 'x' must have at least one predictor (one column).")
  }
  
  # y must be numeric vector with length matching nrow(x)
  if (!is.numeric(y)) {
    stop("Error: 'y' should be a numeric vector.")
  }
  if (any(!is.finite(y))) {
    stop("Error: 'y' contains non-finite values (NA/NaN/Inf).")
  }
  if (NROW(x) != length(y)) {
    stop("Error: 'x' and 'y' must have the same number of rows/observations.")
  }
  
  # alpha in (0, 1]
  if (!(is.numeric(alpha) && length(alpha) == 1 && is.finite(alpha) && alpha > 0 && alpha <= 1)) {
    stop("Error: 'alpha' must be a numeric value in (0, 1].")
  }
  
  # nfolds integer > 1
  if (!(is.numeric(nfolds) && length(nfolds) == 1 && nfolds > 1 && nfolds == as.integer(nfolds))) {
    stop("Error: 'nfolds' must be a single integer greater than 1.")
  }
  
  # logical flags
  if (!is.logical(standardize) || length(standardize) != 1 ||
      !is.logical(parallel)    || length(parallel)    != 1 ||
      !is.logical(verbose)     || length(verbose)     != 1 ||
      !is.logical(return_model)|| length(return_model)!= 1) {
    stop("Error: 'standardize', 'parallel', 'verbose', and 'return_model' must be single logical values.")
  }
  
  # seed: NULL or single integer
  if (!is.null(seed) && !(is.numeric(seed) && length(seed) == 1 && is.finite(seed) && seed == as.integer(seed))) {
    stop("Error: 'seed' must be a single integer value or NULL.")
  }
  
  # custom_folds checks
  if (!is.null(custom_folds)) {
    # allow integer or numeric-looks-like-integer
    if (!(is.integer(custom_folds) || all(custom_folds == as.integer(custom_folds)))) {
      stop("Error: 'custom_folds' must be an integer vector.")
    }
    if (length(custom_folds) != length(y)) {
      stop("Error: 'custom_folds' must be the same length as 'y'.")
    }
    if (any(!is.finite(custom_folds))) {
      stop("Error: 'custom_folds' contains non-finite values.")
    }
    if (any(custom_folds < 1)) {
      stop("Error: 'custom_folds' contains invalid IDs (must be >= 1).")
    }
    
    # Allow arbitrary fold labels, but relate them to nfolds
    unique_folds <- sort(unique(as.integer(custom_folds)))
    if (length(unique_folds) > nfolds) {
      stop("Error: 'custom_folds' defines more unique folds than 'nfolds'.")
    }
    if (max(unique_folds) > nfolds) {
      stop("Error: 'custom_folds' contains fold IDs greater than 'nfolds'.")
    }
    
    missing_folds <- setdiff(seq_len(nfolds), unique_folds)
    if (length(missing_folds) > 0) {
      warning("Some folds in 1..nfolds are not represented in 'custom_folds': ",
              paste(missing_folds, collapse = ", "))
    }
  }
  
  invisible(TRUE)
}

#' Handle Missing Values in a Numeric Matrix
#'
#' Imputes missing values column-wise using column means.
#'
#' @param x A numeric matrix.
#'
#' @return The matrix with missing values imputed.
#' @keywords internal
handle_missing_values <- function(x) {
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("Internal error: 'handle_missing_values' expects a numeric matrix.")
  }
  if (anyNA(x)) {
    col_means <- suppressWarnings(colMeans(x, na.rm = TRUE))
    # Replace columns whose mean is NA (all NA) with 0
    col_means[is.na(col_means)] <- 0
    for (j in seq_along(col_means)) {
      missing_idx <- which(is.na(x[, j]))
      if (length(missing_idx)) {
        x[missing_idx, j] <- col_means[j]
      }
    }
  }
  x
}

#' Prepare Predictors
#'
#' Ensures predictors are numeric, handles factors/characters via model.matrix,
#' applies column-mean imputation for missing values, and assigns default names if needed.
#'
#' @param x A data frame or matrix of predictors.
#'
#' @return A purely numeric dense matrix with no missing values.
#' @keywords internal
prepare_predictors <- function(x) {
  # If data.frame, build a numeric design matrix (no intercept)
  if (is.data.frame(x)) {
    # Preserve NA so they can be imputed later
    mm <- stats::model.matrix(~ . - 1, data = x, na.action = stats::na.pass)
  } else if (is.matrix(x)) {
    # If matrix and not numeric, be strict: require numeric predictors
    if (!is.numeric(x)) {
      stop("Error: Non-numeric matrices are not supported; please supply a data.frame so factors/characters can be handled via model.matrix.")
    } else {
      mm <- x
    }
  } else {
    stop("Internal error: 'x' must be a data.frame or matrix.")
  }
  
  # Ensure column names
  if (is.null(colnames(mm))) {
    colnames(mm) <- paste0("V", seq_len(ncol(mm)))
  }
  
  # Impute missing values with column means
  mm <- handle_missing_values(mm)
  
  # Final sanity checks
  if (!is.numeric(mm)) {
    stop("Internal error: predictors are not numeric after preparation.")
  }
  if (any(!is.finite(mm))) {
    stop("Internal error: predictors contain non-finite values after imputation.")
  }
  
  mm
}

#' Convert Predictors to a Sparse Matrix
#'
#' @param x A numeric dense matrix.
#'
#' @return A "dgCMatrix" sparse matrix.
#' @keywords internal
convert_to_sparse <- function(x) {
  # Prefer explicit constructor to ensure class
  Matrix::Matrix(x, sparse = TRUE)
}

#' Manage Parallel Cluster Setup and Teardown
#'
#' Sets up a parallel backend (if requested) and returns a handle for teardown.
#'
#' @param enable_parallel Logical; whether to enable parallel processing.
#' @param verbose Logical; whether to print status messages.
#'
#' @return A list with elements: cluster (or NULL), registered (logical).
#' @keywords internal
manage_parallel_cluster <- function(enable_parallel, verbose) {
  info <- list(cluster = NULL, registered = FALSE)
  
  # If parallel not requested, ensure a sequential backend is registered
  if (!enable_parallel) {
    if (verbose) message("Parallel processing disabled; running sequentially.")
    if (requireNamespace("foreach", quietly = TRUE)) {
      foreach::registerDoSEQ()
      info$registered <- TRUE
    }
    return(info)
  }
  
  # Check required namespaces
  have_parallel <- requireNamespace("parallel",   quietly = TRUE)
  have_doPar    <- requireNamespace("doParallel", quietly = TRUE)
  have_foreach  <- requireNamespace("foreach",    quietly = TRUE)
  
  if (!have_parallel || !have_doPar || !have_foreach) {
    if (verbose) {
      message("Parallel packages not fully available (need 'parallel', 'doParallel', and 'foreach'); continuing sequentially.")
    }
    if (have_foreach) foreach::registerDoSEQ()
    return(info)
  }
  
  # Determine usable cores
  total_cores <- parallel::detectCores()
  if (!is.finite(total_cores) || total_cores <= 1L) {
    if (verbose) {
      message("Only one core detected; parallel processing disabled.")
    }
    foreach::registerDoSEQ()
    return(info)
  }
  
  # Use up to (total_cores - 1) cores, but at least 1
  n_cores <- max(1L, total_cores - 1L)
  if (n_cores <= 1L) {
    if (verbose) {
      message("Insufficient cores for parallel processing; running sequentially.")
    }
    foreach::registerDoSEQ()
    return(info)
  }
  
  # Start a cluster and register it
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  if (verbose) {
    message("Parallel processing enabled using ", n_cores, " cores out of ", total_cores, " available.")
  }
  
  info$cluster <- cl
  info$registered <- TRUE
  info
}

#' Fit a Lasso/Elastic-Net Model with Cross-Validation
#'
#' @param x_sparse A sparse matrix ("dgCMatrix") of predictors.
#' @param y A numeric response vector.
#' @param alpha Numeric in (0,1]; 1 = lasso, (0,1) = elastic net.
#' @param nfolds Integer number of CV folds.
#' @param standardize Logical; whether to standardize predictors.
#' @param use_parallel Logical; whether to use parallel CV if backend registered.
#' @param custom_folds Optional integer vector of custom fold IDs.
#' @param seed Optional integer for reproducibility.
#' @param verbose Logical; whether to print status.
#'
#' @return The fitted cv.glmnet object.
#' @keywords internal
fit_lasso_model <- function(x_sparse, y, alpha, nfolds, standardize,
                            use_parallel, custom_folds, seed, verbose) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Setup parallel backend (and guarantee teardown)
  par_info <- manage_parallel_cluster(enable_parallel = use_parallel, verbose = verbose)
  
  # If a cluster is active and a seed is provided, set RNG streams for reproducibility
  if (!is.null(seed) && !is.null(par_info$cluster)) {
    parallel::clusterSetRNGStream(par_info$cluster, iseed = seed)
  }
  
  on.exit({
    # Teardown cluster if started
    if (!is.null(par_info$cluster)) {
      try(parallel::stopCluster(par_info$cluster), silent = TRUE)
    }
    # Ensure sequential backend afterward
    if (requireNamespace("foreach", quietly = TRUE)) {
      foreach::registerDoSEQ()
    }
  }, add = TRUE)
  
  # Only pass parallel=TRUE to cv.glmnet if a parallel backend is actually active
  parallel_flag <- isTRUE(use_parallel) &&
    requireNamespace("foreach", quietly = TRUE) &&
    foreach::getDoParWorkers() > 1L
  
  # Build argument list
  args <- list(
    x = x_sparse,
    y = y,
    alpha = alpha,
    nfolds = nfolds,
    standardize = standardize,
    parallel = parallel_flag,
    keep = TRUE
  )
  
  if (!is.null(custom_folds)) {
    args$foldid <- as.integer(custom_folds)
    args$nfolds <- NULL  # cv.glmnet will use length(unique(foldid))
  }
  
  do.call(glmnet::cv.glmnet, args)
}

#' Extract Variable Importance from a Fitted cv.glmnet Model
#'
#' Coefficients (excluding intercept) at lambda.min, ordered by |coef|.
#'
#' @param lasso_model A fitted cv.glmnet model.
#' @param feature_names Optional character vector of feature names; if NULL,
#'   will use colnames from the model matrix where available.
#'
#' @return A data.frame with columns: Variable, Coefficient, AbsCoefficient.
#' @keywords internal
extract_importance <- function(lasso_model, feature_names = NULL) {
  cf <- stats::coef(lasso_model, s = "lambda.min")
  # cf is a sparse matrix; first row is intercept
  cf_vec <- as.vector(cf)[-1]
  
  if (is.null(feature_names)) {
    all_names <- rownames(cf)
    feature_names <- if (!is.null(all_names)) all_names[-1] else paste0("V", seq_along(cf_vec))
  }
  
  if (length(feature_names) != length(cf_vec)) {
    stop("Internal error: length of 'feature_names' does not match number of coefficients.")
  }
  
  importance_df <- data.frame(
    Variable = feature_names,
    Coefficient = cf_vec,
    AbsCoefficient = abs(cf_vec),
    stringsAsFactors = FALSE
  )
  
  # Order by absolute coefficient, descending
  importance_df <- importance_df[order(-importance_df$AbsCoefficient), , drop = FALSE]
  rownames(importance_df) <- NULL
  importance_df
}

#' Main Function: fs_lasso
#'
#' Fit and evaluate a lasso (or elastic-net) model with cross-validation,
#' returning variable importance and (optionally) the fitted model.
#'
#' This function is currently designed for numeric regression (gaussian family).
#'
#' @param x A data frame or matrix of predictor variables.
#' @param y A numeric vector of response values.
#' @param alpha Numeric in (0, 1]; default 1 (lasso). Use (0,1) for elastic-net.
#' @param nfolds Integer > 1; default 5.
#' @param standardize Logical; default TRUE.
#' @param parallel Logical; default TRUE. Requires 'parallel' + 'doParallel' + 'foreach'.
#' @param verbose Logical; default FALSE.
#' @param seed Optional integer for reproducibility; default NULL.
#' @param return_model Logical; whether to include the fitted cv.glmnet object in output; default FALSE.
#' @param custom_folds Optional integer vector of custom fold IDs (same length as y); default NULL.
#'
#' @return A list with:
#'   \item{importance}{data.frame of variable importance at lambda.min}
#'   \item{lambda_min}{Numeric value of lambda minimizing CV error}
#'   \item{lambda_1se}{Numeric value of lambda within 1 SE of minimum}
#'   \item{model}{(Optional) The fitted cv.glmnet object if return_model = TRUE}
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   n <- 100
#'   X <- data.frame(
#'     x1 = rnorm(n),
#'     x2 = rnorm(n),
#'     cat = sample(letters[1:3], n, TRUE)  # non-numeric handled via model.matrix
#'   )
#'   y <- 2 * X$x1 - 3 * X$x2 + rnorm(n)
#'   result <- fs_lasso(x = X, y = y, verbose = TRUE, seed = 123)
#'   head(result$importance)
#' }
#' @export
fs_lasso <- function(x, y, alpha = 1, nfolds = 5, standardize = TRUE,
                     parallel = TRUE, verbose = FALSE, seed = NULL,
                     return_model = FALSE, custom_folds = NULL) {
  
  # Validate inputs
  validate_parameters(x, y, alpha, nfolds, standardize,
                      parallel, verbose, seed, custom_folds, return_model)
  
  # Prepare predictors -> dense numeric matrix with names, no NA
  x_dense <- prepare_predictors(x)
  
  # Sanity check: row alignment
  if (nrow(x_dense) != length(y)) {
    stop("Internal error: prepared predictor matrix and response 'y' have different numbers of rows.")
  }
  
  # Convert to sparse for glmnet
  x_sparse <- convert_to_sparse(x_dense)
  
  # Fit model
  lasso_model <- fit_lasso_model(x_sparse, y, alpha, nfolds, standardize,
                                 parallel, custom_folds, seed, verbose)
  
  # Importance
  feature_names <- colnames(x_dense)
  importance_df <- extract_importance(lasso_model, feature_names)
  
  # Build result
  out <- list(
    importance = importance_df,
    lambda_min = lasso_model$lambda.min,
    lambda_1se = lasso_model$lambda.1se
  )
  if (isTRUE(return_model)) {
    out$model <- lasso_model
  }
  out
}
