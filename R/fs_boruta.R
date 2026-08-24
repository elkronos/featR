# Boruta all-relevant feature selection with importance-aware correlation
# pruning.
#
# Boruta is a Suggests package and is only touched at run time. The correlation
# pruning below is built from stats::cor() plus a small importance-ranked
# grouping loop, so this file no longer needs caret.

#' Preprocess predictors for Boruta feature selection
#'
#' Prepare predictors by excluding the target, coercing to a base data.frame,
#' converting character and logical variables to factor, converting Date/POSIXt
#' to numeric, and validating supported types. After processing, only numeric
#' and factor predictors are returned.
#'
#' @param data A data frame (or data-frame-like object) containing the dataset.
#' @param target A string with the name of the target column to be excluded.
#' @return A data frame containing only processed predictor variables.
#' @noRd
boruta_preprocess_predictors <- function(data, target) {
  # Coerce to base data.frame to avoid surprises with tibbles / data.table
  data <- as.data.frame(data)

  # Basic validation of target (do not require it to be present here)
  assert_string(target, "target")

  # Exclude the target variable from predictors (if present)
  predictors <- data[, setdiff(names(data), target), drop = FALSE]

  # Validate supported variable types before transformation:
  # numeric, factor, character, logical, Date, POSIXt (POSIXct/POSIXlt)
  unsupported_vars <- names(predictors)[vapply(
    predictors,
    function(x) {
      is.matrix(x) || is.array(x) ||
        (!is.numeric(x) &&
           !is.factor(x) &&
           !is.character(x) &&
           !is.logical(x) &&
           !inherits(x, "Date") &&
           !inherits(x, "POSIXt"))
    },
    logical(1L)
  )]

  if (length(unsupported_vars) > 0L) {
    stop(
      "Unsupported variable types found in columns: ",
      paste(unsupported_vars, collapse = ", ")
    )
  }

  # Convert logical variables to factors (TRUE/FALSE as levels)
  logical_vars <- names(predictors)[vapply(predictors, is.logical, logical(1L))]
  if (length(logical_vars) > 0L) {
    predictors[logical_vars] <- lapply(predictors[logical_vars], as.factor)
  }

  # Convert character variables to factors
  char_vars <- names(predictors)[vapply(predictors, is.character, logical(1L))]
  if (length(char_vars) > 0L) {
    predictors[char_vars] <- lapply(predictors[char_vars], as.factor)
  }

  # Convert Date variables to numeric (days since origin)
  date_vars <- names(predictors)[vapply(predictors, inherits, logical(1L), what = "Date")]
  if (length(date_vars) > 0L) {
    predictors[date_vars] <- lapply(predictors[date_vars], as.numeric)
  }

  # Convert POSIXt variables to numeric (seconds since origin). Route through
  # as.POSIXct() first: POSIXlt is a list-like structure that as.numeric()
  # cannot coerce directly.
  posix_vars <- names(predictors)[vapply(predictors, inherits, logical(1L), what = "POSIXt")]
  if (length(posix_vars) > 0L) {
    predictors[posix_vars] <- lapply(
      predictors[posix_vars],
      function(x) as.numeric(as.POSIXct(x))
    )
  }

  # Final check: after processing we should have only numeric or factor
  remaining_unsupported <- names(predictors)[vapply(
    predictors,
    function(x) !is.numeric(x) && !is.factor(x),
    logical(1L)
  )]

  if (length(remaining_unsupported) > 0L) {
    stop(
      "After processing, unsupported types remain in columns: ",
      paste(remaining_unsupported, collapse = ", ")
    )
  }

  predictors
}

#' Median Boruta importance for every attribute
#'
#' Boruta stores one importance value per attribute per run in `ImpHistory`;
#' runs in which an attribute had already been rejected are recorded as
#' `-Inf`. The median over the finite entries is the same statistic
#' `Boruta::attStats()` reports as `medianImp`, computed here straight from the
#' history so that a single importance definition drives the reported scores,
#' the correlation pruning, and the `cutoff_features` cap.
#'
#' @param boruta_obj A Boruta object.
#' @param features Optional character vector. When supplied, the result is
#'   aligned to it and attributes with no history become `NA`.
#' @return A named numeric vector of median importances.
#' @noRd
boruta_median_importance <- function(boruta_obj, features = NULL) {
  imp <- boruta_obj$ImpHistory
  out <- NULL

  if (is.matrix(imp) && nrow(imp) > 0L && !is.null(colnames(imp))) {
    attrs <- setdiff(colnames(imp), c("shadowMin", "shadowMean", "shadowMax"))
    if (length(attrs) > 0L) {
      out <- vapply(
        attrs,
        function(nm) {
          v <- imp[, nm]
          v <- v[is.finite(v)]
          if (length(v) == 0L) NA_real_ else stats::median(v)
        },
        numeric(1L)
      )
      names(out) <- attrs
    }
  }

  if (is.null(out)) {
    # holdHistory = FALSE (or an empty history): no importances to report
    nms <- names(boruta_obj$finalDecision)
    if (is.null(nms)) {
      nms <- character(0)
    }
    out <- stats::setNames(rep(NA_real_, length(nms)), nms)
  }

  if (!is.null(features)) {
    out <- out[features]
    names(out) <- features
  }

  out
}

#' Importance-aware correlation pruning
#'
#' Drops redundant members of correlated groups among `features`, keeping the
#' feature Boruta ranked highest. Features are visited in decreasing median
#' Boruta importance; each retained feature forms a group by absorbing every
#' still-undecided feature whose absolute Pearson correlation with it exceeds
#' `cutoff_cor`, and those absorbed features are dropped. Ties in importance
#' fall back to the order in which the features were supplied. Only numeric
#' predictors take part; factors are never pruned.
#'
#' This replaces the previous `caret::findCorrelation()` call, which picked the
#' member to drop from the correlation structure alone and could therefore
#' discard the feature Boruta ranked higher. Walking outwards from the most
#' important feature also avoids the transitive chaining that connected-
#' component grouping suffers from (a-b and b-c correlated, a-c not).
#'
#' @param predictors A data frame of predictor variables.
#' @param features Character vector of feature names to evaluate.
#' @param importance Named numeric vector of median Boruta importances.
#'   Features missing from it are treated as least important.
#' @param cutoff_cor Numeric threshold for absolute correlation, between 0 and 1.
#' @return A list with `keep` (the surviving features, in the order supplied)
#'   and `dropped` (the features removed by pruning, in the order supplied).
#' @noRd
boruta_prune_correlated <- function(predictors,
                                    features,
                                    importance = NULL,
                                    cutoff_cor = 0.7) {
  # Coerce to base data.frame
  predictors <- as.data.frame(predictors)

  assert_number(cutoff_cor, "cutoff_cor", lower = 0, upper = 1)

  # If at most one feature is selected, nothing to do
  if (length(features) <= 1L) {
    return(list(keep = features, dropped = character(0)))
  }

  # Validate that features exist in predictors
  unknown <- setdiff(features, names(predictors))
  if (length(unknown) > 0L) {
    stop(
      "`features` not found in `predictors`: ",
      paste(unknown, collapse = ", ")
    )
  }

  selected_data <- predictors[, features, drop = FALSE]

  # Keep only numeric variables for correlation
  numeric_vars <- names(selected_data)[vapply(selected_data, is.numeric, logical(1L))]
  if (length(numeric_vars) <= 1L) {
    return(list(keep = features, dropped = character(0)))
  }

  # Compute correlation matrix safely (pairwise complete obs)
  correlation_matrix <- stats::cor(
    selected_data[, numeric_vars, drop = FALSE],
    use = "pairwise.complete.obs"
  )

  # NA correlations arise from constant or near-constant (or all-NA) columns,
  # for which the correlation is undefined. Warn before treating them as 0.
  if (anyNA(correlation_matrix)) {
    n_na <- sum(is.na(correlation_matrix))
    warning(sprintf(
      paste0("%d NA value(s) in the correlation matrix (constant or ",
             "near-constant columns produce undefined correlations); ",
             "treating them as 0 for pruning."),
      n_na
    ))
    correlation_matrix[is.na(correlation_matrix)] <- 0
  }

  correlation_matrix <- abs(correlation_matrix)
  diag(correlation_matrix) <- 0

  if (is.null(importance)) {
    importance <- stats::setNames(numeric(0), character(0))
  }
  imp <- as.numeric(importance[numeric_vars])
  imp[!is.finite(imp)] <- -Inf
  names(imp) <- numeric_vars

  # Highest median importance first; ties keep the supplied order
  ranked <- numeric_vars[order(-imp, seq_along(numeric_vars))]

  dropped <- character(0)
  undecided <- ranked
  while (length(undecided) > 1L) {
    keeper <- undecided[1L]
    undecided <- undecided[-1L]
    redundant <- undecided[correlation_matrix[keeper, undecided] > cutoff_cor]
    if (length(redundant) > 0L) {
      dropped <- c(dropped, redundant)
      undecided <- setdiff(undecided, redundant)
    }
  }

  dropped <- intersect(features, dropped)
  list(keep = setdiff(features, dropped), dropped = dropped)
}

#' Feature selection using Boruta
#'
#' Runs the Boruta all-relevant feature selection algorithm on a dataset.
#' Preprocesses predictors, optionally seeds the RNG locally, optionally
#' resolves tentative features, and optionally prunes correlated features from
#' the confirmed set. Pruning is importance-aware: within a group of correlated
#' features the one with the highest median Boruta importance is kept.
#'
#' @param data A data frame (or data-frame-like object, or matrix).
#' @param target Name of the target column in `data`. A factor target is
#'   treated as classification, a numeric target as regression.
#' @param maxRuns Maximum number of Boruta iterations. Default 250.
#' @param cutoff_features Optional whole number capping the number of returned
#'   features. When supplied, the top features by median Boruta importance are
#'   retained. Default NULL (no cap).
#' @param cutoff_cor Numeric correlation cutoff between 0 and 1 used to drop
#'   redundant features from the selected set. Within each group of features
#'   correlated above the cutoff, the feature with the highest median Boruta
#'   importance is kept and the rest are dropped. Set NULL to skip this step.
#'   Default 0.7.
#' @param resolve_tentative Logical; if TRUE, apply `Boruta::TentativeRoughFix()`
#'   and return only confirmed attributes. If FALSE, tentative attributes are
#'   included in the selected set. Default TRUE.
#' @param seed Optional integer for reproducibility. Applied locally: the
#'   previous RNG state is restored when the function exits. Default NULL
#'   (the RNG is never seeded unless requested).
#' @param verbose Logical; if TRUE, report progress. This maps to Boruta's
#'   `doTrace = 1` (decisions are reported as they are made); FALSE maps to
#'   `doTrace = 0`. Default FALSE.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector of selected feature names, after
#'         optional correlation pruning and the optional `cutoff_features`
#'         cap.}
#'   \item{scores}{Named numeric vector of median Boruta importance for every
#'         candidate feature (`NA` for attributes with no importance history).}
#'   \item{method}{"boruta".}
#'   \item{task}{"classification" for a factor target, "regression" for a
#'         numeric one.}
#'   \item{model}{The Boruta object.}
#'   \item{details}{A list with `boruta_obj` (the Boruta object), `decisions`
#'         (the per-feature Confirmed/Tentative/Rejected factor),
#'         `dropped_correlated` (features removed by correlation pruning), and
#'         `n_features` (the number of candidate features).}
#'   \item{call}{The matched call.}
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("Boruta", quietly = TRUE)) {
#'   d <- data.frame(
#'     y  = factor(rep(c("a", "b"), each = 20)),
#'     x1 = rep(c(0, 1), each = 20) + seq(0, 1, length.out = 40),
#'     x2 = seq_len(40) %% 3
#'   )
#'   res <- fs_boruta(d, "y", maxRuns = 25, cutoff_cor = NULL, seed = 42)
#'   res$selected
#'   res$scores
#' }
#' }
#' @export
fs_boruta <- function(data,
                      target,
                      maxRuns = 250,
                      cutoff_features = NULL,
                      cutoff_cor = 0.7,
                      resolve_tentative = TRUE,
                      seed = NULL,
                      verbose = FALSE) {
  cl <- match.call()

  # ---- Input validation ----
  assert_data_frame(data, "data", allow_matrix = TRUE)

  # Coerce to base data.frame
  data <- as.data.frame(data)

  assert_target(data, target, "target")
  maxRuns <- assert_count(maxRuns, "maxRuns")
  if (!is.null(cutoff_features)) {
    cutoff_features <- assert_count(cutoff_features, "cutoff_features")
  }
  if (!is.null(cutoff_cor)) {
    assert_number(cutoff_cor, "cutoff_cor", lower = 0, upper = 1)
  }
  assert_flag(resolve_tentative, "resolve_tentative")
  assert_flag(verbose, "verbose")

  fs_require("Boruta", "Boruta feature selection")

  # Optional local seeding; the previous RNG state is restored on exit
  local_seed(seed)

  # Prepare y (target)
  y <- data[[target]]

  # Validate target type
  if (!is.factor(y) && !is.numeric(y)) {
    stop("`target` must be numeric (regression) or factor (classification).")
  }
  if (anyNA(y)) {
    stop("`target` contains missing values; please impute or remove them before calling `fs_boruta()`.")
  }
  task <- if (is.factor(y)) "classification" else "regression"

  # Prepare predictors
  predictors <- boruta_preprocess_predictors(data, target)

  # Enforce NA-free predictors for Boruta/randomForest
  if (anyNA(predictors)) {
    stop("Predictors contain missing values; please impute or remove them before calling `fs_boruta()`.")
  }

  # Run Boruta (no `seed` argument supported by Boruta itself)
  boruta_obj <- Boruta::Boruta(
    x = predictors,
    y = y,
    doTrace = if (isTRUE(verbose)) 1L else 0L,
    maxRuns = maxRuns
  )

  # Optionally resolve tentative features to confirmed/rejected
  if (isTRUE(resolve_tentative) &&
      any(boruta_obj$finalDecision == "Tentative")) {
    boruta_obj <- Boruta::TentativeRoughFix(boruta_obj)
  }

  # Get selected attributes:
  # - if resolve_tentative = TRUE: only confirmed (withTentative = FALSE)
  # - if resolve_tentative = FALSE: confirmed + tentative (withTentative = TRUE)
  selected_features <- Boruta::getSelectedAttributes(
    boruta_obj,
    withTentative = !isTRUE(resolve_tentative)
  )

  # Median importance for every candidate feature: the reported scores, and
  # the ranking used by both the pruning and the cutoff_features cap.
  importance <- boruta_median_importance(boruta_obj, features = names(predictors))

  dropped_correlated <- character(0)

  # Optionally prune correlated features from the selected set
  if (length(selected_features) > 0L && !is.null(cutoff_cor)) {
    pruned <- boruta_prune_correlated(
      predictors = predictors,
      features   = selected_features,
      importance = importance,
      cutoff_cor = cutoff_cor
    )
    selected_features  <- pruned$keep
    dropped_correlated <- pruned$dropped

    if (isTRUE(verbose) && length(dropped_correlated) > 0L) {
      message(sprintf(
        "Dropped %d correlated feature(s) (|r| > %g): %s",
        length(dropped_correlated), cutoff_cor,
        paste(dropped_correlated, collapse = ", ")
      ))
    }
  }

  # Optionally cap the number of features (keep top N by median importance)
  if (!is.null(cutoff_features) &&
      length(selected_features) > cutoff_features) {
    imp_sel <- as.numeric(importance[selected_features])
    imp_sel[!is.finite(imp_sel)] <- -Inf
    ord <- order(-imp_sel, seq_along(selected_features))
    selected_features <- selected_features[ord][seq_len(cutoff_features)]
  }

  new_fs_result(
    selected = selected_features,
    scores   = importance,
    method   = "boruta",
    task     = task,
    model    = boruta_obj,
    details  = list(
      boruta_obj         = boruta_obj,
      decisions          = boruta_obj$finalDecision,
      dropped_correlated = dropped_correlated,
      n_features         = ncol(predictors)
    ),
    call = cl
  )
}
