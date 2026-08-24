# Lasso / elastic-net feature selection for featR.
# Suggests: glmnet, Matrix (always); doParallel + foreach (only when
# parallel = TRUE and more than one worker resolves).

#' Validate fs_lasso() input parameters
#'
#' Runs after the target column has been located and extracted, so the
#' response checks can name the offending column.
#'
#' @param y The extracted target vector.
#' @param target Name of the target column, used in error messages.
#' @param alpha A numeric value in (0, 1]; 1 = lasso, (0, 1) = elastic net.
#' @param nfolds Integer >= 3 specifying the number of CV folds (the smallest
#'   value `glmnet::cv.glmnet()` accepts).
#' @param standardize Logical; whether glmnet should standardize predictors.
#' @param custom_folds Optional integer vector of fold IDs (one per row of
#'   `data`).
#' @param return_model Logical; whether to return the fitted cv.glmnet object.
#' @param seed Either NULL or a single whole number for reproducibility.
#' @param verbose Logical; whether to print progress messages.
#' @param parallel Logical; whether to use a parallel backend for CV.
#' @return Invisibly TRUE when all parameters are valid; otherwise errors.
#' @noRd
lasso_validate <- function(y, target, alpha, nfolds, standardize, custom_folds,
                           return_model, seed, verbose, parallel) {
  if (!is.numeric(y)) {
    stop(sprintf(
      "Target column '%s' must be numeric: fs_lasso() fits the gaussian family only.",
      target
    ), call. = FALSE)
  }
  if (any(!is.finite(y))) {
    stop(sprintf(
      "Target column '%s' contains non-finite values (NA/NaN/Inf).",
      target
    ), call. = FALSE)
  }

  assert_number(alpha, "alpha")
  if (alpha <= 0 || alpha > 1) {
    stop("'alpha' must be a numeric value in (0, 1].", call. = FALSE)
  }

  # glmnet::cv.glmnet() itself refuses nfolds < 3 ("nfolds must be bigger than
  # 3"), so reject it here with a message that names featR's own argument.
  assert_count(nfolds, "nfolds", lower = 3L)

  assert_flag(standardize, "standardize")
  assert_flag(parallel, "parallel")
  assert_flag(verbose, "verbose")
  assert_flag(return_model, "return_model")

  if (!is.null(seed)) {
    assert_number(seed, "seed")
    if (seed != round(seed) || abs(seed) > .Machine$integer.max) {
      stop("'seed' must be a single whole number (integer-sized) or NULL.",
           call. = FALSE)
    }
  }

  if (!is.null(custom_folds)) {
    if (!is.numeric(custom_folds)) {
      stop("'custom_folds' must be an integer vector.", call. = FALSE)
    }
    # Check for NA before any integer-ness comparison: comparing a double NA
    # yields NA and would crash the conditions below.
    if (anyNA(custom_folds)) {
      stop("'custom_folds' contains missing values (NA); fold IDs must be complete.",
           call. = FALSE)
    }
    if (any(!is.finite(custom_folds))) {
      stop("'custom_folds' contains non-finite values.", call. = FALSE)
    }
    if (!(is.integer(custom_folds) || all(custom_folds == round(custom_folds)))) {
      stop("'custom_folds' must be an integer vector.", call. = FALSE)
    }
    if (length(custom_folds) != length(y)) {
      stop("'custom_folds' must have one entry per row of 'data'.", call. = FALSE)
    }
    if (any(custom_folds < 1)) {
      stop("'custom_folds' contains invalid IDs (must be >= 1).", call. = FALSE)
    }
    # Range-check on the raw values before any as.integer() conversion, so
    # IDs beyond integer range cannot become NA.
    if (any(custom_folds > nfolds)) {
      stop("'custom_folds' contains fold IDs greater than 'nfolds'.", call. = FALSE)
    }

    unique_folds <- sort(unique(as.integer(custom_folds)))
    if (length(unique_folds) > nfolds) {
      stop("'custom_folds' defines more unique folds than 'nfolds'.", call. = FALSE)
    }

    missing_folds <- setdiff(seq_len(nfolds), unique_folds)
    if (length(missing_folds) > 0L) {
      stop("'custom_folds' leaves some folds in 1..nfolds empty (",
           paste(missing_folds, collapse = ", "),
           "); every fold must contain at least one observation.",
           call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Apply the requested missing-value policy to a design matrix
#'
#' Columns that are entirely NA can never be imputed and raise an error naming
#' them, whichever policy is in force. Otherwise `impute = "none"` errors and
#' names the columns that carry NAs, while `impute = "mean"` fills them with
#' column means computed on the full data and warns about the leakage that
#' implies. Column names are design-matrix columns, so a factor level shows up
#' under its expanded dummy name.
#'
#' @param x A numeric matrix with column names.
#' @param impute Either "none" or "mean".
#' @return The matrix, with missing values imputed when `impute = "mean"`.
#' @noRd
lasso_handle_missing <- function(x, impute) {
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("Internal error: 'lasso_handle_missing' expects a numeric matrix.")
  }
  if (!anyNA(x)) {
    return(x)
  }

  n_obs <- colSums(!is.na(x))
  if (any(n_obs == 0L)) {
    bad <- colnames(x)[n_obs == 0L]
    stop("Column(s) entirely missing (all NA): ",
         paste0("'", bad, "'", collapse = ", "),
         ". Remove or impute these columns before calling fs_lasso().",
         call. = FALSE)
  }

  incomplete <- colnames(x)[colSums(is.na(x)) > 0L]
  if (identical(impute, "none")) {
    stop("Missing values in predictor column(s): ",
         paste0("'", incomplete, "'", collapse = ", "),
         ". Imputing before cross-validation leaks information across folds, ",
         "so fs_lasso() does not do it for you: impute before calling ",
         "fs_lasso(), or set impute = \"mean\" to accept the leak.",
         call. = FALSE)
  }

  warning("Missing predictor values were imputed with column means computed on the full data prior to cross-validation (mild information leakage; per-fold imputation is deferred).",
          call. = FALSE)
  col_means <- colMeans(x, na.rm = TRUE)
  for (j in seq_along(col_means)) {
    missing_idx <- which(is.na(x[, j]))
    if (length(missing_idx)) {
      x[missing_idx, j] <- col_means[j]
    }
  }
  x
}

#' Prepare predictors as a numeric dense matrix without missing values
#'
#' Builds the design matrix through `stats::model.frame(na.action =
#' stats::na.pass)` so rows with NA survive to the missing-value policy:
#' `stats::model.matrix()` alone silently ignores its `na.action` argument
#' and would drop those rows.
#'
#' @param x A data frame of predictors (target column already removed).
#' @param impute Either "none" or "mean".
#' @return A purely numeric dense matrix with no missing values.
#' @noRd
lasso_prepare <- function(x, impute) {
  if (!is.data.frame(x)) {
    stop("Internal error: 'lasso_prepare' expects a data.frame.")
  }

  mm <- stats::model.matrix(
    ~ . - 1,
    data = stats::model.frame(~ . - 1, x, na.action = stats::na.pass)
  )

  if (is.null(colnames(mm))) {
    colnames(mm) <- paste0("V", seq_len(ncol(mm)))
  }

  mm <- lasso_handle_missing(mm, impute)

  if (!is.numeric(mm)) {
    stop("Internal error: predictors are not numeric after preparation.")
  }
  if (any(!is.finite(mm))) {
    # Not an internal error: Inf/-Inf/NaN in the user's data reaches here
    # intact, so name the columns and say what to do about them.
    bad <- colnames(mm)[apply(mm, 2L, function(col) any(!is.finite(col)))]
    if (length(bad) == 0L) {
      bad <- "(unnamed column)"
    }
    stop("Predictors contain non-finite values (Inf, -Inf, or NaN) in: ",
         paste(utils::head(bad, 5L), collapse = ", "),
         if (length(bad) > 5L) sprintf(" (and %d more)", length(bad) - 5L) else "",
         ". Remove or replace those values before calling fs_lasso().",
         call. = FALSE)
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

#' Named coefficient vector at lambda.min, intercept dropped
#'
#' @param lasso_model A fitted cv.glmnet model.
#' @param feature_names Optional character vector of feature names; when
#'   NULL, coefficient row names are used where available.
#' @return A named numeric vector, one entry per design-matrix column.
#' @noRd
lasso_coefficients <- function(lasso_model, feature_names = NULL) {
  cf <- stats::coef(lasso_model, s = "lambda.min")
  # cf is a sparse matrix; the first row is the intercept
  cf_vec <- as.vector(cf)[-1L]

  if (is.null(feature_names)) {
    all_names <- rownames(cf)
    feature_names <- if (!is.null(all_names)) {
      all_names[-1L]
    } else {
      paste0("V", seq_along(cf_vec))
    }
  }

  if (length(feature_names) != length(cf_vec)) {
    stop("Internal error: length of 'feature_names' does not match number of coefficients.")
  }

  stats::setNames(cf_vec, feature_names)
}

#' Build the variable-importance table from a named coefficient vector
#'
#' @param coefs A named numeric vector of coefficients.
#' @return A data.frame with columns Variable, Coefficient, AbsCoefficient,
#'   ordered by decreasing absolute coefficient.
#' @noRd
lasso_importance <- function(coefs) {
  importance_df <- data.frame(
    Variable = names(coefs),
    Coefficient = unname(coefs),
    AbsCoefficient = abs(unname(coefs)),
    stringsAsFactors = FALSE
  )

  importance_df <- importance_df[order(-importance_df$AbsCoefficient), , drop = FALSE]
  rownames(importance_df) <- NULL
  importance_df
}

#' Lasso Feature Selection with Cross-Validation
#'
#' Fits a lasso (or elastic-net) model with `glmnet::cv.glmnet()` and reports
#' which predictors survive at `lambda.min`. Numeric outcomes only (gaussian
#' family).
#'
#' @details
#' Use this when the question is "which predictors keep a non-zero coefficient
#' under a cross-validated L1 penalty (or, for `alpha < 1`, an elastic-net
#' penalty)?". Selection happens at `lambda.min`, the penalty that minimizes
#' cross-validated error; the more conservative `lambda.1se` is reported in
#' `details` but is not used to select. The main caveat is that lasso tends to
#' keep one member of a group of strongly correlated predictors and zero out
#' the rest, so an absent feature is not evidence that it is unrelated to the
#' outcome.
#'
#' The design matrix is built internally from every column of `data` except
#' `target`, via `stats::model.frame(na.action = stats::na.pass)` followed by
#' `stats::model.matrix()`: factors and characters are expanded to dummies and
#' rows carrying NAs are preserved rather than silently dropped. The matrix
#' carries no intercept column of its own (`glmnet` fits its own intercept,
#' which is dropped from the reported coefficients), so `selected`, `scores`
#' and `details$coefficients` name design-matrix columns -- for a factor
#' predictor, its expanded dummy columns rather than the original column.
#'
#' `scores` ranks features on the STANDARDIZED coefficient scale when
#' `standardize = TRUE`: each coefficient is multiplied by the standard
#' deviation of its design-matrix column, which makes the ranking independent
#' of the units the predictors happen to be measured in. glmnet standardizes
#' internally for fitting but reports coefficients back on the input scale, so
#' those raw coefficients are kept in `details$coefficients`. With
#' `standardize = FALSE`, `scores` is the raw table and the two are identical.
#'
#' Raw coefficient magnitude is a scale-dependent notion of importance: a
#' predictor measured in small units earns a large coefficient for the same
#' effect. That applies to `details$coefficients` always, and to `scores` when
#' `standardize = FALSE`, so compare those numbers across predictors only when
#' the predictors share a scale.
#'
#' Missing predictor values are an error under the default
#' `impute = "none"`, because imputing before cross-validation lets the folds
#' see each other. `impute = "mean"` fills them with column means computed on
#' the whole data set -- not on the training part of each fold -- and warns
#' that this leaks. Columns that are entirely NA are an error either way.
#'
#' @param data A data.frame or data.table holding the target and the candidate
#'   predictors. A numeric matrix with column names is also accepted;
#'   non-numeric matrices are not (supply a data.frame so factors and
#'   characters can be expanded by `model.matrix()`).
#' @param target Single string naming the outcome column in `data`. It must be
#'   numeric and free of NA/NaN/Inf.
#' @param alpha Numeric in (0, 1]; default 1 (lasso). Use values in (0, 1)
#'   for elastic-net. `alpha = 0` (pure ridge) is rejected: it never sets a
#'   coefficient to exactly zero, so it cannot select.
#' @param nfolds Integer >= 3; default 5. Number of cross-validation folds
#'   passed to `glmnet::cv.glmnet()`, which rejects anything smaller than 3.
#'   When `custom_folds` is supplied those fold IDs define the folds instead,
#'   and `nfolds` only bounds which IDs are valid (so `custom_folds` must also
#'   define at least 3 folds).
#' @param standardize Logical; default TRUE. Passed to `glmnet::cv.glmnet()`
#'   and also controls the scale `scores` is ranked on (see Details).
#' @param custom_folds Optional integer vector of fold IDs (one per row of
#'   `data`, covering 1..`nfolds` with no empty folds); default NULL.
#' @param impute How to handle missing predictor values: `"none"` (default)
#'   errors and names the offending columns, `"mean"` imputes column means
#'   computed on the full data and warns about the leakage.
#' @param return_model Logical; keep the fitted cv.glmnet object in the result
#'   (and pass `keep = TRUE` to `cv.glmnet()`); default FALSE.
#' @param seed Optional whole number for reproducibility, applied locally and
#'   restored on exit; default NULL (never seeds by default).
#' @param verbose Logical; default FALSE. When TRUE, reports the parallel
#'   backend status and how many design-matrix columns were selected.
#' @param parallel Logical; default FALSE. When TRUE, cross-validation runs on
#'   `n_cores` workers (requires the 'doParallel' and 'foreach' packages); if
#'   `n_cores` resolves to one worker it stays sequential and says so under
#'   `verbose = TRUE`.
#' @param n_cores Integer >= 1; number of workers used only when
#'   `parallel = TRUE`. Default 2. Values above the detected core count are
#'   capped.
#' @return An `fs_result` object with:
#'   \item{selected}{Design-matrix columns whose raw coefficient at
#'     `lambda.min` is non-zero, ordered by decreasing `scores` magnitude.
#'     (Selection reads the raw coefficients, so a constant column that glmnet
#'     nonetheless gave a non-zero coefficient is reported even though its
#'     standardized score is 0.)}
#'   \item{scores}{data.frame with columns Variable, Coefficient and
#'     AbsCoefficient, one row per design-matrix column (including those
#'     shrunk to zero), ordered by decreasing AbsCoefficient, on the
#'     standardized scale when `standardize = TRUE` (see Details).}
#'   \item{method}{`"lasso"`, regardless of `alpha`.}
#'   \item{task}{`"regression"` (gaussian family only).}
#'   \item{model}{The fitted cv.glmnet object when `return_model = TRUE`, else
#'     NULL.}
#'   \item{details}{List of `lambda_min` (lambda minimizing CV error, the one
#'     selection uses), `lambda_1se` (largest lambda within 1 SE of the
#'     minimum; reported only), `coefficients` (the same three-column table as
#'     `scores` but always on the raw coefficient scale) and `n_features`
#'     (number of design-matrix columns considered).}
#'   \item{call}{The matched call.}
#' @examples
#' \donttest{
#' if (requireNamespace("glmnet", quietly = TRUE) &&
#'     requireNamespace("Matrix", quietly = TRUE)) {
#'   n <- 100
#'   df <- data.frame(
#'     x1 = rnorm(n),
#'     x2 = rnorm(n),
#'     cat = sample(letters[1:3], n, TRUE)
#'   )
#'   df$y <- 2 * df$x1 - 3 * df$x2 + rnorm(n)
#'   res <- fs_lasso(df, "y", seed = 123)
#'   selected(res)
#'   head(res$scores)
#' }
#' }
#' @export
fs_lasso <- function(data, target, alpha = 1, nfolds = 5, standardize = TRUE,
                     custom_folds = NULL, impute = c("none", "mean"),
                     return_model = FALSE, seed = NULL, verbose = FALSE,
                     parallel = FALSE, n_cores = 2L) {

  mc <- match.call()

  assert_data_frame(data, arg = "data", allow_matrix = TRUE)
  if (is.matrix(data) && !is.numeric(data)) {
    stop("Non-numeric matrices are not supported; please supply a data.frame so factors/characters can be handled via model.matrix.",
         call. = FALSE)
  }
  # A data.table would route the subsetting below through data.table's NSE;
  # as.data.frame() also copies, so the caller's object is never touched.
  data <- as.data.frame(data)

  assert_target(data, target)
  impute <- match.arg(impute)

  predictor_names <- setdiff(names(data), target)
  if (length(predictor_names) == 0L) {
    stop(sprintf("'data' must contain at least one predictor column besides '%s'.",
                 target), call. = FALSE)
  }

  y <- data[[target]]
  lasso_validate(y, target, alpha, nfolds, standardize, custom_folds,
                 return_model, seed, verbose, parallel)
  n_cores <- resolve_cores(n_cores, "n_cores")

  fs_require(c("glmnet", "Matrix"), "lasso feature selection")
  if (parallel && n_cores > 1L) {
    fs_require(c("doParallel", "foreach"), "parallel cross-validation")
  }

  # Prepare predictors -> dense numeric matrix with names, no NA
  x_dense <- lasso_prepare(data[, predictor_names, drop = FALSE], impute)

  # Sanity check: row alignment
  if (nrow(x_dense) != length(y)) {
    stop("Internal error: prepared predictor matrix and the target have different numbers of rows.")
  }

  # Convert to sparse for glmnet
  x_sparse <- lasso_sparse(x_dense)

  # Fit model
  lasso_model <- lasso_fit(x_sparse, y, alpha, nfolds, standardize,
                           parallel, n_cores, custom_folds, seed, verbose,
                           return_model)

  # Coefficients at lambda.min, on the raw scale and on the standardized one
  coefs_raw <- lasso_coefficients(lasso_model, colnames(x_dense))
  importance_raw <- lasso_importance(coefs_raw)

  if (isTRUE(standardize)) {
    sds <- apply(x_dense, 2L, stats::sd)
    sds[!is.finite(sds)] <- 0
    # Multiply positionally, not by name: model.matrix() can emit duplicate
    # column names (a factor's level dummy colliding with another column), and
    # sds[names(coefs_raw)] would then hand the first match's SD to every
    # duplicate. Both vectors are in design-matrix column order by
    # construction, so position is the reliable key.
    if (length(sds) != length(coefs_raw)) {
      stop("Internal error: standard-deviation and coefficient vectors have different lengths.")
    }
    scores_df <- lasso_importance(coefs_raw * unname(sds))
  } else {
    scores_df <- importance_raw
  }

  # Selection is decided on the raw coefficients (a constant column has a
  # standardized coefficient of 0 even when its raw coefficient is not).
  nonzero <- names(coefs_raw)[!is.na(coefs_raw) & coefs_raw != 0]
  selected_features <- scores_df$Variable[scores_df$Variable %in% nonzero]

  if (isTRUE(verbose)) {
    message(sprintf("Selected %d of %d design-matrix column(s) at lambda.min.",
                    length(selected_features), nrow(importance_raw)))
  }

  new_fs_result(
    selected = selected_features,
    scores   = scores_df,
    method   = "lasso",
    task     = "regression",
    model    = if (isTRUE(return_model)) lasso_model else NULL,
    details  = list(
      lambda_min   = lasso_model$lambda.min,
      lambda_1se   = lasso_model$lambda.1se,
      coefficients = importance_raw,
      n_features   = nrow(importance_raw)
    ),
    call = mc
  )
}
