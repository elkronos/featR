# Boruta all-relevant feature selection with optional correlation pruning.
# Boruta and caret are Suggests and are only touched at run time.

#' Preprocess predictors for Boruta feature selection
#'
#' Prepare predictors by excluding the target, coercing to a base data.frame,
#' converting character and logical variables to factor, converting Date/POSIXt
#' to numeric, and validating supported types. After processing, only numeric
#' and factor predictors are returned.
#'
#' @param data A data frame (or data-frame-like object) containing the dataset.
#' @param target_var A string with the name of the target variable to be excluded.
#' @return A data frame containing only processed predictor variables.
#' @noRd
boruta_preprocess_predictors <- function(data, target_var) {
  # Coerce to base data.frame to avoid surprises with tibbles / data.table
  data <- as.data.frame(data)

  # Basic validation of target_var (do not require it to be present here)
  assert_string(target_var, "target_var")

  # Exclude the target variable from predictors (if present)
  predictors <- data[, setdiff(names(data), target_var), drop = FALSE]

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

#' Remove highly correlated features
#'
#' Remove features among `selected_features` if they are highly correlated
#' (absolute Pearson correlation) above `cutoff_cor`. Only numeric variables
#' are considered for correlation calculations. Requires the suggested caret
#' package (checked just before use).
#'
#' @param predictors A data frame of predictor variables.
#' @param selected_features Character vector of feature names to evaluate.
#' @param cutoff_cor Numeric threshold for absolute correlation, between 0 and 1.
#' @return A character vector of feature names after dropping highly
#'   correlated ones.
#' @noRd
boruta_remove_highly_correlated <- function(predictors,
                                            selected_features,
                                            cutoff_cor = 0.7) {
  # Coerce to base data.frame
  predictors <- as.data.frame(predictors)

  assert_number(cutoff_cor, "cutoff_cor", lower = 0, upper = 1)

  # If only one feature is selected, nothing to do
  if (length(selected_features) <= 1L) {
    return(selected_features)
  }

  # Validate that selected_features exist in predictors
  missing <- setdiff(selected_features, names(predictors))
  if (length(missing) > 0L) {
    stop(
      "`selected_features` not found in `predictors`: ",
      paste(missing, collapse = ", ")
    )
  }

  selected_data <- predictors[, selected_features, drop = FALSE]

  # Keep only numeric variables for correlation
  numeric_vars <- names(selected_data)[vapply(selected_data, is.numeric, logical(1L))]
  if (length(numeric_vars) <= 1L) {
    return(selected_features)
  }

  numeric_data <- selected_data[, numeric_vars, drop = FALSE]

  # Compute correlation matrix safely (pairwise complete obs)
  correlation_matrix <- stats::cor(numeric_data, use = "pairwise.complete.obs")

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

  # Identify indices to drop using caret's heuristic
  fs_require("caret", "correlation-based pruning")
  drop_idx <- caret::findCorrelation(correlation_matrix,
                                     cutoff = cutoff_cor,
                                     names = FALSE)

  if (length(drop_idx) > 0L) {
    drop_features <- numeric_vars[drop_idx]
    selected_features <- setdiff(selected_features, drop_features)
  }

  selected_features
}

#' Feature selection using Boruta
#'
#' Runs the Boruta all-relevant feature selection algorithm on a dataset.
#' Preprocesses predictors, optionally seeds the RNG locally, optionally
#' resolves tentative features, and optionally removes highly correlated
#' features among the selected set. Returns selected feature names and the
#' Boruta object.
#'
#' @param data A data frame (or data-frame-like object, or matrix).
#' @param target_var Name of the target variable.
#' @param seed Optional integer for reproducibility. Applied locally: the
#'   previous RNG state is restored when the function exits. Default NULL
#'   (the RNG is never seeded unless requested).
#' @param doTrace Integer verbosity for Boruta (0 silent, 1 reports
#'   decisions, 2 reports each importance run). Default 0.
#' @param maxRuns Maximum number of Boruta iterations. Default 250.
#' @param cutoff_features Optional whole number to cap the number of returned
#'   features. When specified, the top features by Boruta mean importance are
#'   retained.
#' @param cutoff_cor Numeric correlation cutoff between 0 and 1 used to drop
#'   redundant features via caret::findCorrelation() (requires the suggested
#'   caret package; set NULL to skip this step). Default 0.7.
#' @param resolve_tentative Logical; if TRUE, apply Boruta::TentativeRoughFix()
#'   and return only confirmed attributes. If FALSE, tentative attributes are
#'   included in the selected set.
#'
#' @return A list with:
#' \describe{
#'   \item{selected_features}{Character vector of selected feature names.}
#'   \item{boruta_obj}{The Boruta result object.}
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
#'   res <- fs_boruta(d, "y", seed = 42, maxRuns = 25, cutoff_cor = NULL)
#'   res$selected_features
#' }
#' }
#' @export
fs_boruta <- function(data,
                      target_var,
                      seed = NULL,
                      doTrace = 0,
                      maxRuns = 250,
                      cutoff_features = NULL,
                      cutoff_cor = 0.7,
                      resolve_tentative = TRUE) {
  # ---- Input validation ----
  assert_data_frame(data, "data", allow_matrix = TRUE)

  # Coerce to base data.frame
  data <- as.data.frame(data)

  assert_target(data, target_var, "target_var")
  doTrace <- assert_count(doTrace, "doTrace", lower = 0L)
  maxRuns <- assert_count(maxRuns, "maxRuns")
  if (!is.null(cutoff_features)) {
    cutoff_features <- assert_count(cutoff_features, "cutoff_features")
  }
  if (!is.null(cutoff_cor)) {
    assert_number(cutoff_cor, "cutoff_cor", lower = 0, upper = 1)
  }
  assert_flag(resolve_tentative, "resolve_tentative")

  fs_require("Boruta", "Boruta feature selection")

  # Optional local seeding; the previous RNG state is restored on exit
  local_seed(seed)

  # Prepare y (target)
  y <- data[[target_var]]

  # Validate target type
  if (!is.factor(y) && !is.numeric(y)) {
    stop("`target_var` must be numeric (regression) or factor (classification).")
  }
  if (anyNA(y)) {
    stop("`target_var` contains missing values; please impute or remove them before calling `fs_boruta()`.")
  }

  # Prepare predictors
  predictors <- boruta_preprocess_predictors(data, target_var)

  # Enforce NA-free predictors for Boruta/randomForest
  if (anyNA(predictors)) {
    stop("Predictors contain missing values; please impute or remove them before calling `fs_boruta()`.")
  }

  # Run Boruta (no `seed` argument supported by Boruta itself)
  boruta_obj <- Boruta::Boruta(
    x = predictors,
    y = y,
    doTrace = doTrace,
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

  # Early return if none
  if (length(selected_features) == 0L) {
    return(list(selected_features = character(0L), boruta_obj = boruta_obj))
  }

  # Optionally remove highly correlated among the selected
  if (!is.null(cutoff_cor)) {
    selected_features <- boruta_remove_highly_correlated(
      predictors = predictors,
      selected_features = selected_features,
      cutoff_cor = cutoff_cor
    )
  }

  # Optionally cap the number of features (keep top N by Boruta importance)
  if (!is.null(cutoff_features) &&
      length(selected_features) > cutoff_features) {
    att_stats <- Boruta::attStats(boruta_obj)
    # Keep only rows corresponding to currently selected features
    att_stats <- att_stats[rownames(att_stats) %in% selected_features, , drop = FALSE]

    if (!"meanImp" %in% colnames(att_stats)) {
      stop("`Boruta::attStats` does not provide `meanImp`; cannot rank features by importance.")
    }

    # Order by mean importance (descending)
    att_stats <- att_stats[order(att_stats[["meanImp"]], decreasing = TRUE), , drop = FALSE]
    selected_features <- rownames(att_stats)[seq_len(cutoff_features)]
  }

  list(selected_features = selected_features, boruta_obj = boruta_obj)
}
