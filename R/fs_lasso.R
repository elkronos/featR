# Lasso / elastic-net feature selection for featR.
# Suggests: glmnet (always); doParallel + foreach (only when parallel = TRUE
# and more than one worker resolves).

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

#' Strip formula backticks from design-matrix / term names
#'
#' `stats::model.matrix()`, `stats::lm()` and caret's formula interface name
#' a non-syntactic column after its deparsed term label, so a column "my var"
#' comes back as "`my var`" (and a factor "my grp" as "`my grp`b"). That
#' leading backticked segment is replaced by the name it quotes, so reported
#' names match the caller's columns. Names without a leading backtick are
#' returned unchanged. Shared by fs_lasso(), fs_elastic(), fs_stepwise(),
#' fs_mars() and fs_svm() (via svm_encode()), so a change here affects all of
#' them.
#'
#' @param x Character vector of names (NULL passes through).
#' @return `x` with the leading quoted segment unquoted.
#' @noRd
unbacktick_names <- function(x) {
  if (is.null(x)) {
    return(x)
  }
  quoted <- !is.na(x) & startsWith(x, "`")
  if (!any(quoted)) {
    return(x)
  }
  x[quoted] <- vapply(x[quoted], function(nm) {
    m <- regmatches(nm, regexpr("^`((?:[^`\\\\]|\\\\.)*)`", nm, perl = TRUE))
    if (length(m) == 0L) {
      return(nm)
    }
    # Let the parser undo deparse()'s escaping, so "\\n" becomes a newline
    # again rather than a literal "n".
    inner <- tryCatch(as.character(str2lang(m)), error = function(e) {
      gsub("\\\\(.)", "\\1", substr(m, 2L, nchar(m) - 1L), perl = TRUE)
    })
    paste0(inner, substr(nm, nchar(m) + 1L, nchar(nm)))
  }, character(1L), USE.NAMES = FALSE)
  x
}

#' Stop when a categorical predictor has fewer than two observed levels
#'
#' `stats::model.matrix()` (and `lm()`) cannot build contrasts for a factor or
#' character column with a single distinct non-missing value and fails with
#' the cryptic "contrasts can be applied only to factors with 2 or more
#' levels", which never names the column. Shared by fs_lasso() and
#' fs_stepwise().
#'
#' @param x A data frame of predictors.
#' @param fn Name of the calling function, for the message.
#' @return `x`, invisibly; errors naming the offending columns otherwise.
#' @noRd
lasso_check_categorical_levels <- function(x, fn) {
  bad <- names(x)[vapply(x, function(col) {
    (is.factor(col) || is.character(col)) &&
      length(unique(col[!is.na(col)])) < 2L
  }, logical(1L))]
  if (length(bad) > 0L) {
    stop(sprintf(
      "%s(): categorical predictor(s) with fewer than two observed levels: %s%s. A single-level factor/character column carries no information and cannot be dummy-coded; remove it before calling %s().",
      fn, paste0("'", utils::head(bad, 5L), "'", collapse = ", "),
      if (length(bad) > 5L) ", ..." else "", fn
    ), call. = FALSE)
  }
  invisible(x)
}

#' Prepare predictors as a numeric dense matrix without missing values
#'
#' Builds the design matrix through `stats::model.frame(na.action =
#' stats::na.pass)` so rows with NA survive to the missing-value policy:
#' `stats::model.matrix()` alone silently ignores its `na.action` argument
#' and would drop those rows. Design-matrix names must be unique (a factor
#' dummy such as `gb` for factor `g`, level `b`, can collide with a column
#' `gb`, or with the target), because `selected` and `scores` identify columns
#' by name; a collision is an error.
#'
#' @param x A data frame of predictors (target column already removed).
#' @param impute Either "none" or "mean".
#' @param target Name of the target column, checked for collisions too.
#' @return A purely numeric dense matrix with no missing values.
#' @noRd
lasso_prepare <- function(x, impute, target = NULL) {
  if (!is.data.frame(x)) {
    stop("Internal error: 'lasso_prepare' expects a data.frame.")
  }
  lasso_check_categorical_levels(x, "fs_lasso")

  mm <- stats::model.matrix(
    ~ . - 1,
    data = stats::model.frame(~ . - 1, x, na.action = stats::na.pass)
  )

  if (is.null(colnames(mm))) {
    colnames(mm) <- paste0("V", seq_len(ncol(mm)))
  }
  # Report the caller's column names, not model.matrix()'s backticked labels.
  colnames(mm) <- unbacktick_names(colnames(mm))
  assert_unique_encoded_names(colnames(mm), target, "fs_lasso")

  if (ncol(mm) < 2L) {
    stop("fs_lasso() needs at least two design-matrix columns (glmnet cannot ",
         "fit a single-column x).", call. = FALSE)
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

#' Fit a lasso/elastic-net model with cross-validation
#'
#' @param x A dense numeric matrix of predictors (from `lasso_prepare()`;
#'   sparse input is deliberately avoided, see the note in `fs_lasso()`).
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
lasso_fit <- function(x, y, alpha, nfolds, standardize,
                      use_parallel, n_cores, custom_folds, seed, verbose,
                      return_model) {
  local_seed(seed)

  parallel_flag <- FALSE
  if (use_parallel && n_cores > 1L) {
    local_parallel_cluster(n_cores, seed = seed)
    if (verbose) {
      message("Parallel cross-validation enabled with ", n_cores,
              " worker(s).")
    }
    parallel_flag <- TRUE
  } else if (use_parallel && verbose) {
    message("Only one worker resolved; running cross-validation sequentially.")
  }

  args <- list(
    x = x,
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

#' Named coefficient vector at the chosen lambda, intercept dropped
#'
#' @param lasso_model A fitted cv.glmnet model.
#' @param feature_names Optional character vector of feature names; when
#'   NULL, coefficient row names are used where available.
#' @param s Either "lambda.min" or "lambda.1se".
#' @return A named numeric vector, one entry per design-matrix column.
#' @noRd
lasso_coefficients <- function(lasso_model, feature_names = NULL,
                               s = "lambda.min") {
  cf <- stats::coef(lasso_model, s = s)
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

#' Lasso feature selection with cross-validation
#'
#' @description
#' Fits a lasso (or elastic-net) regression with `glmnet::cv.glmnet()` and
#' selects the predictors whose coefficient is non-zero at the
#' cross-validated penalty. The outcome must be numeric; only the gaussian
#' family is fitted.
#'
#' @details
#' The lasso adds an L1 penalty to least squares, which shrinks coefficients
#' and sets some of them to exactly zero. `fs_lasso()` chooses the penalty
#' strength (lambda) by `nfolds`-fold cross-validation and reports the
#' predictors that keep a non-zero coefficient. With `alpha < 1` the penalty
#' becomes an elastic net, a mix of L1 and L2 penalties; `fs_elastic()` tunes
#' `alpha` as well and also handles classification.
#'
#' By default selection happens at `lambda.min`, the penalty with the lowest
#' cross-validated error. `lambda.min` is tuned for prediction and tends to
#' over-select, keeping noise variables alongside the true ones.
#' `lambda = "1se"` selects at `lambda.1se`, the largest penalty whose CV
#' error is within one standard error of the minimum, which gives a sparser,
#' more conservative set. Both values are reported in `details` whichever one
#' is used.
#'
#' The lasso tends to keep one member of a group of strongly correlated
#' predictors and zero out the rest, so an absent feature is not evidence
#' that it is unrelated to the outcome.
#'
#' `fs_lasso()` reports no performance estimate. The lambdas are tuned on all
#' rows, so judging the selected set needs an outer resampling loop around
#' the whole call.
#'
#' @section How the design matrix is built:
#' Every column of `data` except `target` is a candidate predictor. The
#' design matrix comes from `stats::model.frame(na.action = stats::na.pass)`
#' followed by `stats::model.matrix()` without an intercept column, so rows
#' with NAs reach the missing-value policy instead of being dropped silently.
#' Factors and characters are expanded to dummy columns: the first factor
#' (in column order) gets one dummy per level and each later factor gets one
#' per level except the first. `glmnet` fits its own intercept, which is left
#' out of the reported coefficients.
#'
#' `selected`, `scores` and `details$coefficients` therefore name
#' design-matrix columns, so a factor predictor appears as its dummy columns
#' rather than under its own name. Non-syntactic column names are reported as
#' you wrote them, without the backticks `model.matrix()` adds.
#'
#' These inputs are errors:
#' * a design matrix with fewer than two columns (`glmnet` cannot fit a
#'   single-column `x`);
#' * a factor or character predictor with fewer than two observed levels;
#' * a dummy name that collides with another column or the target (factor
#'   `g` with level `b` next to a column `gb`), because `selected` and
#'   `scores` identify columns by name;
#' * Inf or -Inf in any predictor (NaN counts as a missing value);
#' * a column whose variance overflows double precision (values around 1e154
#'   or larger); rescale it first.
#'
#' @section How scores are scaled:
#' With `standardize = TRUE` (the default), `scores` ranks features on the
#' standardized coefficient scale: each coefficient is multiplied by the
#' standard deviation of its design-matrix column, so the ranking does not
#' depend on the units the predictors are measured in. `glmnet` standardizes
#' internally for fitting but reports coefficients on the input scale, and
#' those raw coefficients are kept in `details$coefficients`. With
#' `standardize = FALSE`, `scores` holds the raw coefficients and the two
#' tables are identical.
#'
#' A raw coefficient depends on scale: a predictor measured in small units
#' earns a large coefficient for the same effect. Compare
#' `details$coefficients` (and `scores` when `standardize = FALSE`) across
#' predictors only when the predictors share a scale.
#'
#' Constant columns get a coefficient of exactly 0 and are never selected.
#' The same holds for columns that are constant up to floating-point
#' round-off: standardization would otherwise inflate that round-off into a
#' full-scale predictor with a raw coefficient around 1e15, so such columns
#' are held at 0 during the fit. A coefficient whose standardized value is at
#' most `1e-10` times the largest standardized coefficient is floating-point
#' residue and is set to exactly 0. Without that step the redundant dummy of the first factor,
#' which is collinear with the intercept, kept a coefficient around 1e-13 and
#' was selected.
#'
#' @section Missing values:
#' Under the default `impute = "none"`, a missing predictor value is an error
#' that names the affected columns, because imputing before cross-validation
#' lets the folds see each other. `impute = "mean"` fills missing values with
#' column means computed on all rows, not on the training part of each fold,
#' and warns about that leak. A column that is entirely NA is an error under
#' either setting. The target must be free of NA, NaN and Inf.
#'
#' @param data A data.frame or data.table holding the target and the
#'   candidate predictors, with unique, non-empty column names. A numeric
#'   matrix with column names is also accepted. A non-numeric matrix is not:
#'   supply a data.frame so factors and characters can be expanded by
#'   `model.matrix()`.
#' @param target Single string naming the outcome column in `data`. It must
#'   be numeric and free of NA, NaN and Inf.
#' @param alpha Numeric in (0, 1]; default 1 (lasso). Values in (0, 1) give an
#'   elastic net. `alpha = 0` (pure ridge) is rejected because ridge never
#'   sets a coefficient to exactly zero, so it cannot select.
#' @param nfolds Integer >= 3; default 5. Number of cross-validation folds
#'   passed to `glmnet::cv.glmnet()`, which rejects anything smaller than 3.
#'   When `custom_folds` is supplied, those fold IDs define the folds and
#'   `nfolds` only bounds which IDs are valid, so `custom_folds` must also
#'   define at least 3 folds.
#' @param standardize Logical; default `TRUE`. Passed to
#'   `glmnet::cv.glmnet()`, and also sets the scale `scores` is ranked on (see
#'   "How scores are scaled").
#' @param custom_folds Optional integer vector of fold IDs, one per row of
#'   `data`, covering 1..`nfolds` with no empty fold; default `NULL`.
#' @param impute How to handle missing predictor values: `"none"` (default)
#'   errors and names the affected columns; `"mean"` imputes column means
#'   computed on the full data and warns about the leak.
#' @param return_model Logical; default `FALSE`. When `TRUE`, the fitted
#'   `cv.glmnet` object is kept in `model` and `keep = TRUE` is passed to
#'   `cv.glmnet()` so the prevalidated fits are retained.
#' @param seed Optional whole number; default `NULL`, which leaves the RNG
#'   alone. When given, the seed is set for this call only and the previous
#'   RNG state is restored on exit.
#' @param verbose Logical; default `FALSE`. When `TRUE`, reports the parallel
#'   backend status and how many design-matrix columns were selected.
#' @param parallel Logical; default `FALSE`. When `TRUE`, cross-validation
#'   runs on `n_cores` workers, which needs the 'doParallel' and 'foreach'
#'   packages. If `n_cores` resolves to one worker, the run stays sequential
#'   and says so when `verbose = TRUE`.
#' @param n_cores Integer >= 1; default 2. Number of workers, used only when
#'   `parallel = TRUE`. Values above the detected core count are capped.
#' @param lambda Penalty to select at: `"min"` (default, `lambda.min`) or
#'   `"1se"` (`lambda.1se`, sparser). See Details.
#' @return An `fs_result` object with:
#'   \item{selected}{Design-matrix columns whose coefficient at the chosen
#'     lambda is non-zero, ordered by decreasing `scores` magnitude. Constant
#'     and numerically constant columns always have coefficient 0, so they
#'     are never selected.}
#'   \item{scores}{data.frame with columns Variable, Coefficient and
#'     AbsCoefficient, one row per design-matrix column (including those
#'     shrunk to zero), ordered by decreasing AbsCoefficient. On the
#'     standardized scale when `standardize = TRUE`.}
#'   \item{method}{`"lasso"`, whatever the value of `alpha`.}
#'   \item{task}{`"regression"` (gaussian family only).}
#'   \item{model}{The fitted `cv.glmnet` object when `return_model = TRUE`,
#'     else `NULL`.}
#'   \item{details}{List of `lambda_min` (lambda minimizing CV error),
#'     `lambda_1se` (largest lambda within one standard error of the
#'     minimum), `lambda_used` (`"lambda.min"` or `"lambda.1se"`, the one that
#'     selection and all coefficients are read at), `coefficients` (the same
#'     three-column table as `scores`, always on the raw coefficient scale)
#'     and `n_features` (number of design-matrix columns considered).}
#'   \item{call}{The matched call.}
#' @examples
#' \donttest{
#' if (requireNamespace("glmnet", quietly = TRUE)) {
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
                     return_model = FALSE, lambda = c("min", "1se"),
                     seed = NULL, verbose = FALSE,
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
  # model.frame(~ .) keeps only one copy of a duplicated name, so the other
  # copy silently vanished from the design matrix and from the scores.
  assert_column_names(data, "fs_lasso")
  impute <- match.arg(impute)
  lambda <- match.arg(lambda)
  lambda_s <- paste0("lambda.", lambda)

  predictor_names <- setdiff(names(data), target)
  if (length(predictor_names) == 0L) {
    stop(sprintf("'data' must contain at least one predictor column besides '%s'.",
                 target), call. = FALSE)
  }

  y <- data[[target]]
  lasso_validate(y, target, alpha, nfolds, standardize, custom_folds,
                 return_model, seed, verbose, parallel)
  n_cores <- resolve_cores(n_cores, "n_cores")

  fs_require("glmnet", "lasso feature selection")
  if (parallel && n_cores > 1L) {
    fs_require(c("doParallel", "foreach"), "parallel cross-validation")
  }

  # Prepare predictors -> dense numeric matrix with names, no NA
  x_dense <- lasso_prepare(data[, predictor_names, drop = FALSE], impute,
                           target = target)

  # Sanity check: row alignment
  if (nrow(x_dense) != length(y)) {
    stop("Internal error: prepared predictor matrix and the target have different numbers of rows.")
  }

  # Fit on the dense matrix. glmnet's sparse-input standardization computes
  # the variance as E[x^2] - E[x]^2, which for a nonzero constant column (or
  # one in units small enough that x^2 underflows) is rounding noise or 0:
  # the lambda path becomes NaN and cv.glmnet() fails with "NA subscripts in
  # x[i,j]" or "need at least two non-NA values to interpolate". The dense
  # path handles such columns (their coefficient is simply 0), and x_dense is
  # already in memory, so a sparse copy saved nothing.
  sds <- apply(x_dense, 2L, stats::sd)
  if (any(!is.finite(sds))) {
    bad <- colnames(x_dense)[!is.finite(sds)]
    stop("Predictor column(s) whose variance overflows double precision: ",
         paste(utils::head(bad, 5L), collapse = ", "),
         if (length(bad) > 5L) ", ..." else "",
         ". glmnet would silently give them a zero coefficient; rescale them ",
         "before calling fs_lasso().", call. = FALSE)
  }
  # Columns constant up to round-off: glmnet's standardization would divide
  # by an sd of ~1e-17 and fit the round-off as a full-scale predictor (raw
  # coefficient ~1e15). Hold them at an exact constant so the fit gives them
  # coefficient 0, like a truly constant column.
  near_const <- vapply(seq_len(ncol(x_dense)), function(j) {
    is_numerically_constant(x_dense[, j])
  }, logical(1L))
  x_fit <- x_dense
  if (any(near_const)) {
    x_fit[, near_const] <- 0
    sds[near_const] <- 0
  }

  # Fit model
  lasso_model <- lasso_fit(x_fit, y, alpha, nfolds, standardize,
                           parallel, n_cores, custom_folds, seed, verbose,
                           return_model)

  # Coefficients at the chosen lambda, in design-matrix column order
  coefs_raw <- lasso_coefficients(lasso_model, colnames(x_dense), s = lambda_s)
  coefs_raw[is.na(coefs_raw)] <- 0
  # Zero out floating-point residue: the first factor is dummy-coded with all
  # of its levels, which are collinear with glmnet's intercept, and the
  # redundant level is often left at ~1e-13. The tolerance is relative to the
  # largest coefficient on the unit-free standardized scale.
  coefs_std <- coefs_raw * unname(sds)
  tol <- 1e-10 * max(abs(coefs_std), 0)
  coefs_raw[abs(coefs_std) <= tol] <- 0
  coefs_std[abs(coefs_std) <= tol] <- 0

  importance_raw <- lasso_importance(coefs_raw)
  scores_df <- if (isTRUE(standardize)) lasso_importance(coefs_std) else importance_raw

  # Design-matrix names are unique (lasso_prepare() enforces it), so the
  # name-based lookup below is unambiguous.
  nonzero <- names(coefs_raw)[coefs_raw != 0]
  selected_features <- scores_df$Variable[scores_df$Variable %in% nonzero]

  if (isTRUE(verbose)) {
    message(sprintf("Selected %d of %d design-matrix column(s) at %s.",
                    length(selected_features), nrow(importance_raw), lambda_s))
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
      lambda_used  = lambda_s,
      coefficients = importance_raw,
      n_features   = nrow(importance_raw)
    ),
    call = mc
  )
}
