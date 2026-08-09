# Lasso / elastic-net feature selection for featR.
# Suggests: glmnet, Matrix (always); doParallel + foreach (only when
# parallel = TRUE and more than one worker resolves).

#' Validate fs_lasso() input parameters
#'
#' @param x A data frame or matrix of predictor variables.
#' @param y A numeric vector of response values (no NA/NaN/Inf).
#' @param alpha A numeric value in (0, 1]; 1 = lasso, (0, 1) = elastic net.
#' @param nfolds Integer > 1 specifying the number of CV folds.
#' @param standardize Logical; whether glmnet should standardize predictors.
#' @param parallel Logical; whether to use a parallel backend for CV.
#' @param verbose Logical; whether to print progress messages.
#' @param seed Either NULL or a single whole number for reproducibility.
#' @param custom_folds Optional integer vector of fold IDs (same length as y).
#' @param return_model Logical; whether to return the fitted cv.glmnet object.
#' @return Invisibly TRUE when all parameters are valid; otherwise errors.
#' @noRd
lasso_validate <- function(x, y, alpha, nfolds, standardize,
                           parallel, verbose, seed, custom_folds,
                           return_model) {
  assert_data_frame(x, arg = "x", allow_matrix = TRUE)

  if (!is.numeric(y)) {
    stop("'y' must be a numeric vector.")
  }
  if (any(!is.finite(y))) {
    stop("'y' contains non-finite values (NA/NaN/Inf).")
  }
  if (NROW(x) != length(y)) {
    stop("'x' and 'y' must have the same number of rows/observations.")
  }

  assert_number(alpha, "alpha")
  if (alpha <= 0 || alpha > 1) {
    stop("'alpha' must be a numeric value in (0, 1].")
  }

  assert_count(nfolds, "nfolds", lower = 2L)

  assert_flag(standardize, "standardize")
  assert_flag(parallel, "parallel")
  assert_flag(verbose, "verbose")
  assert_flag(return_model, "return_model")

  if (!is.null(seed)) {
    assert_number(seed, "seed")
    if (seed != round(seed) || abs(seed) > .Machine$integer.max) {
      stop("'seed' must be a single whole number (integer-sized) or NULL.")
    }
  }

  if (!is.null(custom_folds)) {
    if (!is.numeric(custom_folds)) {
      stop("'custom_folds' must be an integer vector.")
    }
    # Check for NA before any integer-ness comparison: comparing a double NA
    # yields NA and would crash the conditions below.
    if (anyNA(custom_folds)) {
      stop("'custom_folds' contains missing values (NA); fold IDs must be complete.")
    }
    if (any(!is.finite(custom_folds))) {
      stop("'custom_folds' contains non-finite values.")
    }
    if (!(is.integer(custom_folds) || all(custom_folds == round(custom_folds)))) {
      stop("'custom_folds' must be an integer vector.")
    }
    if (length(custom_folds) != length(y)) {
      stop("'custom_folds' must be the same length as 'y'.")
    }
    if (any(custom_folds < 1)) {
      stop("'custom_folds' contains invalid IDs (must be >= 1).")
    }
    # Range-check on the raw values before any as.integer() conversion, so
    # IDs beyond integer range cannot become NA.
    if (any(custom_folds > nfolds)) {
      stop("'custom_folds' contains fold IDs greater than 'nfolds'.")
    }

    unique_folds <- sort(unique(as.integer(custom_folds)))
    if (length(unique_folds) > nfolds) {
      stop("'custom_folds' defines more unique folds than 'nfolds'.")
    }

    missing_folds <- setdiff(seq_len(nfolds), unique_folds)
    if (length(missing_folds) > 0L) {
      stop("'custom_folds' leaves some folds in 1..nfolds empty (",
           paste(missing_folds, collapse = ", "),
           "); every fold must contain at least one observation.")
    }
  }

  invisible(TRUE)
}

#' Impute missing values in a numeric matrix with column means
#'
#' Columns that are entirely NA cannot be imputed and raise an error naming
#' the offending columns. When imputation does run, a single warning notes
#' that the means are computed on the full data prior to cross-validation.
#'
#' @param x A numeric matrix with column names.
#' @return The matrix with missing values imputed.
#' @noRd
lasso_impute_means <- function(x) {
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("Internal error: 'lasso_impute_means' expects a numeric matrix.")
  }
  if (anyNA(x)) {
    n_obs <- colSums(!is.na(x))
    if (any(n_obs == 0L)) {
      bad <- colnames(x)[n_obs == 0L]
      stop("Column(s) entirely missing (all NA): ",
           paste0("'", bad, "'", collapse = ", "),
           ". Remove or impute these columns before calling fs_lasso().")
    }
    warning("Missing predictor values were imputed with column means computed on the full data prior to cross-validation (mild information leakage; per-fold imputation is deferred).")
    col_means <- colMeans(x, na.rm = TRUE)
    for (j in seq_along(col_means)) {
      missing_idx <- which(is.na(x[, j]))
      if (length(missing_idx)) {
        x[missing_idx, j] <- col_means[j]
      }
    }
  }
  x
}

#' Prepare predictors as a numeric dense matrix without missing values
#'
#' Builds the design matrix through `stats::model.frame(na.action =
#' stats::na.pass)` so rows with NA survive to the imputation step:
#' `stats::model.matrix()` alone silently ignores its `na.action` argument
#' and would drop those rows.
#'
#' @param x A data frame or matrix of predictors.
#' @return A purely numeric dense matrix with no missing values.
#' @noRd
lasso_prepare <- function(x) {
  if (is.data.frame(x)) {
    mm <- stats::model.matrix(
      ~ . - 1,
      data = stats::model.frame(~ . - 1, as.data.frame(x),
                                na.action = stats::na.pass)
    )
  } else if (is.matrix(x)) {
    if (!is.numeric(x)) {
      stop("Non-numeric matrices are not supported; please supply a data.frame so factors/characters can be handled via model.matrix.")
    }
    mm <- x
  } else {
    stop("Internal error: 'x' must be a data.frame or matrix.")
  }

  if (is.null(colnames(mm))) {
    colnames(mm) <- paste0("V", seq_len(ncol(mm)))
  }

  mm <- lasso_impute_means(mm)

  if (!is.numeric(mm)) {
    stop("Internal error: predictors are not numeric after preparation.")
  }
  if (any(!is.finite(mm))) {
    stop("Internal error: predictors contain non-finite values after imputation.")
  }

  mm
}

#' Convert a numeric dense matrix to a sparse matrix
#' @noRd
lasso_sparse <- function(x) {
  Matrix::Matrix(x, sparse = TRUE)
}

#' Start and register a parallel cluster
#'
#' The caller is responsible for stopping the returned cluster (via
#' `on.exit()` registered immediately after this call). If registration
#' fails, the cluster is stopped here so it cannot leak.
#'
#' @param n_cores Integer > 1, already resolved via `resolve_cores()`.
#' @param verbose Logical; whether to print a status message.
#' @return The cluster object.
#' @noRd
lasso_cluster <- function(n_cores, verbose) {
  cl <- parallel::makeCluster(n_cores)
  tryCatch(
    doParallel::registerDoParallel(cl),
    error = function(e) {
      try(parallel::stopCluster(cl), silent = TRUE)
      stop(e)
    }
  )
  if (verbose) {
    message("Parallel cross-validation enabled with ", n_cores, " worker(s).")
  }
  cl
}

#' Fit a lasso/elastic-net model with cross-validation
#'
#' @param x_sparse A sparse matrix of predictors.
#' @param y A numeric response vector.
#' @param alpha Numeric in (0, 1].
#' @param nfolds Integer number of CV folds.
#' @param standardize Logical; whether to standardize predictors.
#' @param use_parallel Logical; whether to run CV in parallel.
#' @param n_cores Integer >= 1, already resolved via `resolve_cores()`.
#' @param custom_folds Optional integer vector of fold IDs.
#' @param seed Optional whole number for reproducibility.
#' @param verbose Logical; whether to print status messages.
#' @param return_model Logical; when TRUE, `keep = TRUE` is passed to
#'   `glmnet::cv.glmnet()` so prevalidated fits are retained.
#' @return The fitted cv.glmnet object.
#' @noRd
lasso_fit <- function(x_sparse, y, alpha, nfolds, standardize,
                      use_parallel, n_cores, custom_folds, seed, verbose,
                      return_model) {
  local_seed(seed)

  parallel_flag <- FALSE
  if (use_parallel && n_cores > 1L) {
    cl <- lasso_cluster(n_cores, verbose)
    on.exit({
      try(parallel::stopCluster(cl), silent = TRUE)
      foreach::registerDoSEQ()
    }, add = TRUE)
    if (!is.null(seed)) {
      parallel::clusterSetRNGStream(cl, iseed = as.integer(seed))
    }
    parallel_flag <- TRUE
  } else if (use_parallel && verbose) {
    message("Only one worker resolved; running cross-validation sequentially.")
  }

  args <- list(
    x = x_sparse,
    y = y,
    alpha = alpha,
    nfolds = nfolds,
    standardize = standardize,
    parallel = parallel_flag,
    keep = isTRUE(return_model)
  )

  if (!is.null(custom_folds)) {
    args$foldid <- as.integer(custom_folds)
    args$nfolds <- NULL  # cv.glmnet will use length(unique(foldid))
  }

  do.call(glmnet::cv.glmnet, args)
}

#' Extract variable importance from a fitted cv.glmnet model
#'
#' Coefficients (excluding the intercept) at `lambda.min`, ordered by
#' absolute value.
#'
#' @param lasso_model A fitted cv.glmnet model.
#' @param feature_names Optional character vector of feature names; when
#'   NULL, coefficient row names are used where available.
#' @return A data.frame with columns Variable, Coefficient, AbsCoefficient.
#' @noRd
lasso_importance <- function(lasso_model, feature_names = NULL) {
  cf <- stats::coef(lasso_model, s = "lambda.min")
  # cf is a sparse matrix; the first row is the intercept
  cf_vec <- as.vector(cf)[-1L]

  if (is.null(feature_names)) {
    all_names <- rownames(cf)
    feature_names <- if (!is.null(all_names)) all_names[-1L] else paste0("V", seq_along(cf_vec))
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

  importance_df <- importance_df[order(-importance_df$AbsCoefficient), , drop = FALSE]
  rownames(importance_df) <- NULL
  importance_df
}

#' Lasso Feature Selection with Cross-Validation
#'
#' Fits a lasso (or elastic-net) model with `glmnet::cv.glmnet()` and returns
#' variable importance and, optionally, the fitted model. Currently designed
#' for numeric regression (gaussian family).
#'
#' @details
#' Importance is the absolute value of each coefficient at `lambda.min`, on
#' the ORIGINAL predictor scale (glmnet standardizes internally for fitting
#' when `standardize = TRUE`, but reports coefficients back on the input
#' scale). Rankings therefore depend on the units of the predictors; rescale
#' the predictors yourself if you need scale-free comparisons.
#'
#' Missing predictor values are imputed with column means computed on the
#' full data before cross-validation (a warning is raised); columns that are
#' entirely NA are an error.
#'
#' @param x A data frame or matrix of predictor variables. Factors and
#'   characters in a data frame are expanded via `model.matrix()`; rows with
#'   missing values are preserved and mean-imputed.
#' @param y A numeric vector of response values (no NA/NaN/Inf).
#' @param alpha Numeric in (0, 1]; default 1 (lasso). Use values in (0, 1)
#'   for elastic-net.
#' @param nfolds Integer > 1; default 5.
#' @param standardize Logical; default TRUE.
#' @param parallel Logical; default FALSE. When TRUE, cross-validation runs
#'   on `n_cores` workers (requires the 'doParallel' and 'foreach' packages).
#' @param verbose Logical; default FALSE.
#' @param seed Optional whole number for reproducibility, applied locally and
#'   restored on exit; default NULL (never seeds by default).
#' @param return_model Logical; include the fitted cv.glmnet object in the
#'   output (and pass `keep = TRUE` to `cv.glmnet()`); default FALSE.
#' @param custom_folds Optional integer vector of fold IDs (same length as
#'   `y`, covering 1..`nfolds` with no empty folds); default NULL.
#' @param n_cores Integer >= 1; number of workers used only when
#'   `parallel = TRUE`. Default 2. Values above the detected core count are
#'   capped.
#' @return A list with:
#'   \item{importance}{data.frame of variable importance at `lambda.min` (see Details on scale-dependence).}
#'   \item{lambda_min}{Value of lambda minimizing CV error.}
#'   \item{lambda_1se}{Value of lambda within 1 SE of the minimum.}
#'   \item{model}{(Optional) The fitted cv.glmnet object if `return_model = TRUE`.}
#' @examples
#' \donttest{
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("Matrix", quietly = TRUE)) {
#'   n <- 100
#'   X <- data.frame(
#'     x1 = rnorm(n),
#'     x2 = rnorm(n),
#'     cat = sample(letters[1:3], n, TRUE)
#'   )
#'   y <- 2 * X$x1 - 3 * X$x2 + rnorm(n)
#'   result <- fs_lasso(x = X, y = y, seed = 123)
#'   head(result$importance)
#' }
#' }
#' @export
fs_lasso <- function(x, y, alpha = 1, nfolds = 5, standardize = TRUE,
                     parallel = FALSE, verbose = FALSE, seed = NULL,
                     return_model = FALSE, custom_folds = NULL,
                     n_cores = 2L) {

  lasso_validate(x, y, alpha, nfolds, standardize,
                 parallel, verbose, seed, custom_folds, return_model)
  n_cores <- resolve_cores(n_cores, "n_cores")

  fs_require(c("glmnet", "Matrix"), "lasso feature selection")
  if (parallel && n_cores > 1L) {
    fs_require(c("doParallel", "foreach"), "parallel cross-validation")
  }

  # Prepare predictors -> dense numeric matrix with names, no NA
  x_dense <- lasso_prepare(x)

  # Sanity check: row alignment
  if (nrow(x_dense) != length(y)) {
    stop("Internal error: prepared predictor matrix and response 'y' have different numbers of rows.")
  }

  # Convert to sparse for glmnet
  x_sparse <- lasso_sparse(x_dense)

  # Fit model
  lasso_model <- lasso_fit(x_sparse, y, alpha, nfolds, standardize,
                           parallel, n_cores, custom_folds, seed, verbose,
                           return_model)

  # Importance
  feature_names <- colnames(x_dense)
  importance_df <- lasso_importance(lasso_model, feature_names)

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
