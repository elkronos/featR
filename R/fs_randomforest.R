# Random forest fitting, importance, and evaluation for featR.

#' Print a progress message when verbose
#' @noRd
rf_message <- function(msg, verbose) {
  if (isTRUE(verbose)) message(msg)
}

#' Align factor levels between train and test predictors
#'
#' For every training column that is a factor, the corresponding test column
#' is re-leveled to the training levels. Test values unseen in training become
#' NA (and are later imputed when `control$impute = TRUE`).
#'
#' @param train,test data.frames.
#' @return A list with the aligned `train` and `test` data.frames.
#' @noRd
rf_align_levels <- function(train, test) {
  common <- intersect(names(train), names(test))
  for (nm in common) {
    if (is.factor(train[[nm]])) {
      test[[nm]] <- factor(test[[nm]], levels = levels(train[[nm]]))
    }
  }
  list(train = train, test = test)
}

#' Order features by decreasing importance
#'
#' Features without a score keep their supplied order behind the scored ones.
#'
#' @param features Character vector.
#' @param scores Named numeric vector, or NULL.
#' @return `features`, reordered.
#' @noRd
rf_rank_features <- function(features, scores) {
  if (is.null(scores) || length(features) < 2L) {
    return(features)
  }
  v <- as.numeric(scores[features])
  v[!is.finite(v)] <- -Inf
  features[order(-v, seq_along(features))]
}

#' Apply a user feature-selection hook to the training split
#'
#' The hook receives a data.table of the training rows only (never the test
#' rows) and must return a data.frame/data.table that still contains the
#' target. Because the same column set has to be applied to the held-out rows,
#' the hook may only select and reorder existing columns; creating new ones is
#' an error.
#'
#' @param train_df Training data.frame.
#' @param target Target column name.
#' @param fun The user-supplied hook.
#' @return Character vector of the predictor columns the hook kept.
#' @noRd
rf_apply_feature_select <- function(train_df, target, fun) {
  if (!is.function(fun)) {
    stop("'control$feature_select' must be a function(dt) -> dt.",
         call. = FALSE)
  }
  out <- fun(as_dt(train_df))
  if (!is.data.frame(out)) {
    stop("'control$feature_select' must return a data.frame/data.table.",
         call. = FALSE)
  }
  kept <- names(out)
  if (!target %in% kept) {
    stop("Feature selection removed the target column.", call. = FALSE)
  }
  unknown <- setdiff(kept, names(train_df))
  if (length(unknown) > 0L) {
    stop(sprintf(paste(
      "'control$feature_select' returned column(s) not present in the data: %s.",
      "The hook now runs on the training split only, so it must select",
      "existing columns rather than create new ones; do any feature",
      "engineering in 'control$preprocess' instead."
    ), paste(unknown, collapse = ", ")), call. = FALSE)
  }
  setdiff(kept, target)
}

#' Random forest importance and held-out evaluation
#'
#' Answers "how much does each predictor contribute to a random forest, and how
#' well does that forest do on rows it has never seen?" The pipeline runs in
#' this order: optional preprocessing of the full table, target cleaning,
#' optional downsampling, a stratified train/test split, the optional
#' `control$feature_select` hook on the training rows, character-to-factor
#' coercion and level alignment, near-zero-variance removal, imputation,
#' training (optionally across several workers), and evaluation on the held-out
#' rows. Permutation importance is reported as the per-feature score.
#'
#' Random forests take any mix of numeric, factor, character, and `Date`
#' predictors and need no scaling, which makes this a good default when the
#' feature types are messy. The main caveat is that a forest \emph{ranks}
#' rather than \emph{selects}: unless you supply `control$feature_select`,
#' `selected` is every predictor that reached the forest, ordered by
#' importance, and choosing the cutoff is left to you. Permutation importance
#' also tends to favour predictors with many distinct values and to split
#' credit between correlated predictors, so treat close scores as ties.
#'
#' @param data A data.frame or data.table with at least one row and one column,
#'   holding the target and the candidate predictors (every other column).
#'   `Date` columns are converted to numeric before modeling.
#' @param target Single string naming the target column of `data`; a column
#'   index is not accepted.
#' @param task One of `"classification"` (the default) or `"regression"`,
#'   matched with `match.arg()`. A classification target is coerced with
#'   `as.factor()` and must retain at least two levels; a regression target is
#'   coerced with `as.numeric()`. Either way, rows whose target is `NA` after
#'   coercion are dropped with a warning.
#' @param control List of method-specific options; every accepted entry and its
#'   default is listed under Details. Unknown entries are an error, not a
#'   silent no-op. Default `list()`, i.e. all defaults.
#' @param seed Optional single finite number for reproducibility, truncated to
#'   an integer with `as.integer()`. Applied for the duration of the call only;
#'   the previous RNG state is restored on exit. With more than one worker it
#'   is also handed to `parallel::clusterSetRNGStream()`, and is then the only
#'   way to make the run reproducible. Default `NULL` (the RNG is never seeded
#'   unless requested).
#' @param verbose Logical; emit progress messages (split sizes, how many
#'   predictors the hook kept, the tree/worker counts, and a note when AUC is
#'   skipped because 'pROC' is missing). Default `FALSE`.
#' @param n_cores Whole number >= 1. Number of workers used to grow the forest.
#'   Default `1L` (sequential; no cluster is created). Capped at the detected
#'   core count and at `control$ntree`; an effective value above 1 requires the
#'   suggested packages 'foreach' and 'doParallel', and costs the OOB metrics
#'   as described below.
#'
#' @details
#' \strong{control list (every accepted entry, with its default)}:
#' \itemize{
#'   \item \code{train_ratio = 0.75} -- training proportion of the stratified
#'     split; must be strictly between 0 and 1.
#'   \item \code{sample_size = NULL} -- optional whole number. When supplied,
#'     the data is downsampled to approximately this many rows \emph{before}
#'     the split (proportionally within each target class for classification,
#'     uniformly at random for regression). Values above the available row
#'     count are capped. \code{NULL} keeps every row.
#'   \item \code{ntree = 500} -- number of trees to grow; whole number >= 1.
#'   \item \code{importance = TRUE} -- compute permutation importance. When
#'     \code{FALSE}, \code{scores} and \code{details$importance} are
#'     \code{NULL} and \code{selected} keeps plain column order.
#'   \item \code{scale_importance = TRUE} -- passed as \code{scale} to
#'     \code{randomForest::importance()}, which divides each mean decrease in
#'     accuracy by its permutation standard error. Set to \code{FALSE} for the
#'     raw, unscaled permutation importance.
#'   \item \code{mtry = NULL} -- predictors sampled at each split. \code{NULL}
#'     means \code{floor(sqrt(p))} for classification and \code{floor(p / 3)}
#'     for regression; any value, supplied or derived, is clamped to
#'     \code{[1, p]}, where \code{p} counts the predictors that survive
#'     selection and near-zero-variance removal.
#'   \item \code{nodesize = NULL} -- minimum size of terminal nodes; whole
#'     number >= 1. \code{NULL} leaves \code{randomForest}'s own default (1 for
#'     classification, 5 for regression).
#'   \item \code{maxnodes = NULL} -- maximum number of terminal nodes per tree;
#'     whole number >= 2, or \code{NULL} for no limit.
#'   \item \code{sampsize = NULL} -- rows drawn to grow each tree, passed to
#'     \code{randomForest}. A single number for regression (a vector is an
#'     error); one number per class is allowed for classification. Values are
#'     clamped to the training row count, and \code{NA} entries are replaced by
#'     the full training size when \code{replace = TRUE} and by
#'     \code{ceiling(0.632 * n)} otherwise.
#'   \item \code{classwt = NULL} -- class priors for classification, forwarded
#'     to \code{randomForest} unchanged and unvalidated.
#'   \item \code{strata = NULL} -- stratification variable for the per-tree
#'     sampling, forwarded to \code{randomForest} unchanged and unvalidated.
#'   \item \code{replace = TRUE} -- sample rows with replacement when growing
#'     each tree.
#'   \item \code{preprocess = NULL} -- function \code{dt -> dt}, applied to the
#'     full data before the split. It must return a data.frame/data.table that
#'     still contains the target. See the leakage note below.
#'   \item \code{feature_select = NULL} -- function \code{dt -> dt}; runs on the
#'     training split only, and must select existing columns while retaining
#'     the target -- see the note below.
#'   \item \code{impute = TRUE} -- median (numeric) or modal
#'     (factor/character) values learned on the training data for every
#'     predictor and applied to NAs in train and test.
#'   \item \code{drop_zerovar = TRUE} -- near-zero variance removal with
#'     \code{caret::nearZeroVar()}, using training data only.
#'   \item \code{oob = TRUE} -- include out-of-bag metrics in
#'     \code{details$oob} (\code{accuracy} for classification, \code{RMSE} for
#'     regression); see the note below about parallel training.
#'   \item \code{return_test_data = FALSE} -- when \code{TRUE}, the held-out
#'     rows are returned in \code{details$test_data}.
#'   \item \code{positive_class = NULL} -- optional level name treated as the
#'     positive class for binary AUC. Falls back to the second factor level
#'     both by default and, silently, when the string does not name a level of
#'     the target.
#' }
#' Unknown `control` entries are rejected, so typos and arguments that moved
#' out of `control` (`seed`, `n_cores`, the former `split_ratio`) fail loudly.
#'
#' \strong{Selection is train-only}: \code{control$feature_select} runs
#' \emph{after} the train/test split and sees the training rows only, so a
#' selection rule that looks at the target no longer leaks the held-out rows
#' into the reported test metrics. Because the same columns must be applied to
#' the test rows, the hook may only subset and reorder existing columns; put
#' any feature engineering in \code{control$preprocess}, which runs on the full
#' data before the split -- subject to the caveat in the next paragraph.
#' Near-zero-variance removal happens after the hook, so
#' \code{details$feature_names} (the predictors the forest actually used) can be
#' a subset of `selected`.
#'
#' \strong{What still sees the full data}: \code{control$preprocess} runs
#' before the split by design, so any quantity it learns -- an imputation
#' value, a scaling constant, anything that consults the target -- is computed
#' from the held-out rows too and will flatter \code{details$metrics}. Keep it
#' to row-wise transformations that do not depend on other rows. Optional
#' downsampling (\code{control$sample_size}) also happens before the split,
#' which is why \code{details$train_index} indexes the cleaned and
#' down-sampled table rather than the rows of the original \code{data}.
#'
#' \strong{OOB metrics and parallel training}: when more than one worker is
#' actually used (\code{n_cores} after the caps described above) the forest is
#' assembled with \code{randomForest::combine()}, which drops the out-of-bag
#' error structures (\code{err.rate}, \code{mse}, \code{confusion}). In that
#' case \code{details$oob} is \code{NULL} and a warning is emitted if
#' \code{control$oob = TRUE}. The held-out metrics in \code{details$metrics}
#' are unaffected; use \code{n_cores = 1} when you want the OOB numbers.
#'
#' Character predictors are converted to factors, with the levels learned on
#' the training split. Test values unseen in training become NA and are imputed
#' when \code{control$impute = TRUE}; with \code{control$impute = FALSE} those
#' NAs survive into \code{predict()}, which \code{randomForest} cannot handle.
#' \code{details$metrics$auc} is filled in only when the classification target
#' has exactly two levels \emph{and} the suggested package 'pROC' is installed;
#' it is \code{NA} otherwise, which includes every multi-class fit. Regression
#' fits report RMSE, MAE, and R2 instead, and R2 is \code{NA} when the held-out
#' target has no variance.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector. The predictors kept by
#'         `control$feature_select` when a hook is supplied; otherwise every
#'         predictor that reached the forest, since a plain random forest
#'         ranks rather than selects. Ordered by decreasing importance when
#'         importance was computed.}
#'   \item{scores}{Named numeric vector of permutation importance from
#'         `randomForest::importance(type = 1)` -- mean decrease in accuracy
#'         for classification, percent increase in MSE for regression -- scaled
#'         or not according to `control$scale_importance`. `NULL` when
#'         `control$importance = FALSE`.}
#'   \item{method}{"randomforest".}
#'   \item{task}{"classification" or "regression", echoing the `task`
#'         argument.}
#'   \item{model}{The fitted `randomForest` object; the result of
#'         `randomForest::combine()` when more than one worker was used.}
#'   \item{details}{A list with `metrics` (a named list: `accuracy`, `kappa`,
#'         `auc` for classification, or `RMSE`, `MAE`, `R2` for regression),
#'         `predictions` (test-set predictions), `probabilities` (test-set
#'         class probability matrix, classification only, `NULL` if
#'         `predict()` cannot produce one), `importance` (data.frame with
#'         columns `feature` and `importance`, sorted descending, `NULL` when
#'         `control$importance = FALSE`), `confusion` (Observed x Predicted
#'         table, classification only), `oob` (`accuracy` for classification or
#'         `RMSE` for regression; `NULL` when trained in parallel or switched
#'         off), `feature_names` (predictors the forest actually used),
#'         `train_index` (integer training rows of the cleaned and optionally
#'         down-sampled data), `test_data` (the held-out rows, only when
#'         `control$return_test_data = TRUE`, `NULL` otherwise), `control` (the
#'         merged control list) and `n_features` (candidate predictors after
#'         cleaning and before selection).}
#'   \item{call}{The matched call.}
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForest", quietly = TRUE) &&
#'     requireNamespace("caret", quietly = TRUE)) {
#'   res <- fs_randomforest(
#'     iris,
#'     target = "Species",
#'     task = "classification",
#'     control = list(ntree = 100),
#'     seed = 42
#'   )
#'   # every predictor, ranked: a forest ranks rather than selects
#'   print(res$selected)
#'   print(res$scores)
#'   print(res$details$metrics)
#' }
#' }
#' @export
fs_randomforest <- function(data,
                            target,
                            task = c("classification", "regression"),
                            control = list(),
                            seed = NULL,
                            verbose = FALSE,
                            n_cores = 1L) {
  cl_call <- match.call()

  task <- match.arg(task)
  assert_data_frame(data, "data")
  assert_target(data, target, arg = "target")
  if (!is.list(control)) {
    stop("'control' must be a list.", call. = FALSE)
  }
  assert_flag(verbose, "verbose")
  if (!is.null(seed)) {
    assert_number(seed, "seed")
  }

  # ---- Merge control with defaults -------------------------------------------
  ctrl_defaults <- list(
    train_ratio = 0.75,
    sample_size = NULL,
    ntree = 500,
    importance = TRUE,
    scale_importance = TRUE,
    mtry = NULL,
    nodesize = NULL,
    maxnodes = NULL,
    sampsize = NULL,
    classwt = NULL,
    strata = NULL,
    replace = TRUE,
    preprocess = NULL,
    feature_select = NULL,
    impute = TRUE,
    drop_zerovar = TRUE,
    oob = TRUE,
    return_test_data = FALSE,
    positive_class = NULL
  )
  unknown <- setdiff(names(control), names(ctrl_defaults))
  if (length(unknown) > 0L) {
    stop(sprintf(
      "Unknown 'control' entries: %s. Valid entries are: %s. Note that 'seed' and 'n_cores' are arguments of fs_randomforest(), not control entries, and 'split_ratio' is now 'train_ratio'.",
      paste(unknown, collapse = ", "),
      paste(names(ctrl_defaults), collapse = ", ")
    ), call. = FALSE)
  }
  ctrl <- utils::modifyList(ctrl_defaults, control, keep.null = TRUE)

  # ---- Validate control values ------------------------------------------------
  assert_number(ctrl$train_ratio, "control$train_ratio")
  if (ctrl$train_ratio <= 0 || ctrl$train_ratio >= 1) {
    stop("'control$train_ratio' must be strictly between 0 and 1.",
         call. = FALSE)
  }
  ntree <- assert_count(ctrl$ntree, "control$ntree", lower = 1L)
  if (!is.null(ctrl$sample_size)) {
    ctrl$sample_size <- assert_count(ctrl$sample_size, "control$sample_size",
                                     lower = 1L)
  }
  if (!is.null(ctrl$mtry)) {
    ctrl$mtry <- assert_count(ctrl$mtry, "control$mtry", lower = 1L)
  }
  if (!is.null(ctrl$nodesize)) {
    ctrl$nodesize <- assert_count(ctrl$nodesize, "control$nodesize",
                                  lower = 1L)
  }
  if (!is.null(ctrl$maxnodes)) {
    ctrl$maxnodes <- assert_count(ctrl$maxnodes, "control$maxnodes",
                                  lower = 2L)
  }
  assert_flag(ctrl$importance, "control$importance")
  assert_flag(ctrl$scale_importance, "control$scale_importance")
  assert_flag(ctrl$replace, "control$replace")
  assert_flag(ctrl$impute, "control$impute")
  assert_flag(ctrl$drop_zerovar, "control$drop_zerovar")
  assert_flag(ctrl$oob, "control$oob")
  assert_flag(ctrl$return_test_data, "control$return_test_data")
  if (!is.null(ctrl$positive_class)) {
    assert_string(ctrl$positive_class, "control$positive_class")
  }

  # ---- Worker count (sequential by default; capped at cores and ntree) -------
  n_cores <- resolve_cores(n_cores, arg = "n_cores")
  n_cores <- min(n_cores, ntree)

  # caret is used for splitting, near-zero variance filtering, and Kappa.
  fs_require(c("randomForest", "caret"), "random forest modeling")

  # ---- Seed (restored when this function exits) ------------------------------
  local_seed(seed)

  # ---- Convert to data.table (always a copy) ---------------------------------
  dt <- as_dt(data)

  # ---- Custom preprocess (full data, before the split) -----------------------
  if (!is.null(ctrl$preprocess)) {
    if (!is.function(ctrl$preprocess)) {
      stop("'control$preprocess' must be a function(dt) -> dt.", call. = FALSE)
    }
    dt <- ctrl$preprocess(dt)
    if (!is.data.frame(dt)) {
      stop("'control$preprocess' must return a data.frame/data.table.",
           call. = FALSE)
    }
    dt <- as_dt(dt)
    if (!target %in% names(dt)) {
      stop("Preprocessing removed the target column.", call. = FALSE)
    }
  }

  # ---- Date -> numeric --------------------------------------------------------
  date_cols <- vapply(dt, inherits, logical(1L), what = "Date")
  if (any(date_cols)) {
    idx <- which(date_cols)
    dt[, (idx) := lapply(.SD, as.numeric), .SDcols = idx]
  }

  # ---- Target type and NA handling -------------------------------------------
  if (task == "classification") {
    dt[[target]] <- as.factor(dt[[target]])

    if (anyNA(dt[[target]])) {
      warning("Rows with NA in the classification target were removed before training.",
              call. = FALSE)
      # Single-symbol i is evaluated in calling scope (no column shadowing).
      keep_rows <- !is.na(dt[[target]])
      dt <- dt[keep_rows]
    }

    dt[[target]] <- droplevels(dt[[target]])
    if (nlevels(dt[[target]]) < 2L) {
      stop("Classification target must have at least 2 classes after preprocessing and NA removal.",
           call. = FALSE)
    }
  } else {
    dt[[target]] <- suppressWarnings(as.numeric(dt[[target]]))

    if (anyNA(dt[[target]])) {
      warning("Rows with NA in the regression target (including from coercion) were removed before training.",
              call. = FALSE)
      # Single-symbol i is evaluated in calling scope (no column shadowing).
      keep_rows <- !is.na(dt[[target]])
      dt <- dt[keep_rows]
    }
  }

  if (nrow(dt) == 0L) {
    stop("No rows left after cleaning the target variable.", call. = FALSE)
  }

  # ---- Optional downsampling BEFORE split ------------------------------------
  if (!is.null(ctrl$sample_size)) {
    total_n <- nrow(dt)
    desired_total <- min(ctrl$sample_size, total_n)

    if (task == "classification") {
      # by = c(target) forces evaluation of `target` to the user's column
      # name; a bare `by = target` would grab a column literally named
      # "target" whenever one exists.
      dt <- dt[
        , {
          desired_class <- floor((.N / total_n) * desired_total)
          size_class <- max(1L, min(.N, desired_class))
          .SD[sample(.N, size = size_class)]
        },
        by = c(target)
      ]
    } else {
      dt <- dt[sample(.N, size = desired_total)]
    }
  }

  n_candidates <- length(setdiff(names(dt), target))

  # ---- Train/test split (plain integer index) --------------------------------
  index <- fs_split_index(dt[[target]], p = ctrl$train_ratio)
  # Subset with data.frame semantics so no data.table NSE lookup is involved.
  df_all <- as.data.frame(dt)
  train_df <- df_all[index, , drop = FALSE]
  test_df <- df_all[-index, , drop = FALSE]

  if (nrow(test_df) == 0L) {
    stop("Test split is empty; adjust 'control$train_ratio'.", call. = FALSE)
  }
  rf_message(sprintf("Training on %d rows; holding out %d rows.",
                     nrow(train_df), nrow(test_df)), verbose)

  # ---- Custom feature selection (TRAINING SPLIT ONLY: no test-set leakage) ---
  hook_selected <- NULL
  if (!is.null(ctrl$feature_select)) {
    hook_selected <- rf_apply_feature_select(train_df, target,
                                             ctrl$feature_select)
    keep_cols <- c(target, hook_selected)
    train_df <- train_df[, keep_cols, drop = FALSE]
    test_df <- test_df[, keep_cols, drop = FALSE]
    rf_message(sprintf("Feature selection kept %d of %d predictors.",
                       length(hook_selected), n_candidates), verbose)
  }

  # ---- Character predictors -> factors (randomForest rejects characters) -----
  predictor_cols <- setdiff(names(train_df), target)
  for (nm in predictor_cols) {
    if (is.character(train_df[[nm]])) {
      train_df[[nm]] <- factor(train_df[[nm]])
    }
  }

  # ---- Align factor levels between train and test -----------------------------
  aligned <- rf_align_levels(train_df, test_df)
  train_df <- aligned$train
  test_df <- aligned$test

  # ---- Zero-variance removal (train-only stats) ------------------------------
  if (isTRUE(ctrl$drop_zerovar)) {
    predictor_cols_train <- setdiff(names(train_df), target)
    if (length(predictor_cols_train) > 0L) {
      nzv <- caret::nearZeroVar(
        train_df[, predictor_cols_train, drop = FALSE],
        saveMetrics = TRUE
      )
      drop_cols <- rownames(nzv)[nzv$zeroVar | nzv$nzv]
      for (dc in drop_cols) {
        train_df[[dc]] <- NULL
        if (dc %in% names(test_df)) {
          test_df[[dc]] <- NULL
        }
      }
    }
  }

  # ---- Imputation (train-only stats, applied to both) ------------------------
  if (isTRUE(ctrl$impute)) {
    feat_cols <- setdiff(names(train_df), target)
    if (length(feat_cols) > 0L) {
      # Learn imputation values for EVERY predictor from the training data,
      # not only for columns with NAs in train: the test set can contain NAs
      # of its own, including NAs introduced by unseen factor levels during
      # level alignment.
      impute_values <- vector("list", length(feat_cols))
      names(impute_values) <- feat_cols

      for (i in seq_along(feat_cols)) {
        nm <- feat_cols[i]
        x <- train_df[[nm]]
        if (is.numeric(x)) {
          impute_values[[i]] <- stats::median(x, na.rm = TRUE)
        } else if (is.factor(x) || is.character(x)) {
          tab <- table(x, useNA = "no")
          impute_values[[i]] <- if (length(tab) > 0L) names(which.max(tab)) else NA
        } else {
          impute_values[[i]] <- NA
        }
      }

      # Apply to any NA in train or test.
      for (i in seq_along(feat_cols)) {
        nm <- feat_cols[i]
        val <- impute_values[[i]]
        if (length(val) != 1L || is.na(val)) next

        x_tr <- train_df[[nm]]
        idx_na_tr <- is.na(x_tr)
        if (any(idx_na_tr)) {
          x_tr[idx_na_tr] <- val
          train_df[[nm]] <- x_tr
        }

        if (nm %in% names(test_df)) {
          x_te <- test_df[[nm]]
          idx_na_te <- is.na(x_te)
          if (any(idx_na_te)) {
            x_te[idx_na_te] <- val
            test_df[[nm]] <- x_te
          }
        }
      }
    }
  }

  # ---- Prepare X/Y for training ----------------------------------------------
  if (!target %in% names(train_df)) {
    stop("Target column missing from training data after preprocessing.",
         call. = FALSE)
  }

  target_idx <- match(target, names(train_df))
  x_train <- train_df[, -target_idx, drop = FALSE]
  y_train <- train_df[[target]]

  n_pred <- ncol(x_train)
  if (is.na(n_pred) || n_pred < 1L) {
    stop("No predictor columns left after preprocessing/feature selection/NZV removal.",
         call. = FALSE)
  }

  # ---- Compute mtry safely ----------------------------------------------------
  mtry_eff <- ctrl$mtry
  if (is.null(mtry_eff)) {
    mtry_eff <- if (task == "classification") {
      max(1L, floor(sqrt(n_pred)))
    } else {
      max(1L, floor(n_pred / 3))
    }
  }
  mtry_eff <- min(max(1L, as.integer(mtry_eff)), n_pred)

  # ---- Compute sampsize safely ------------------------------------------------
  sampsize_eff <- ctrl$sampsize
  if (!is.null(sampsize_eff)) {
    if (task == "regression" && length(sampsize_eff) > 1L) {
      stop("'control$sampsize' must be a single scalar for regression.",
           call. = FALSE)
    }
    if (anyNA(sampsize_eff)) {
      n_obs <- nrow(x_train)
      sampsize_eff <- if (isTRUE(ctrl$replace)) n_obs else ceiling(0.632 * n_obs)
    }
    sampsize_eff <- as.integer(sampsize_eff)
    if (any(!is.finite(sampsize_eff)) || any(sampsize_eff < 1L)) {
      sampsize_eff <- 1L
    }
    if (length(sampsize_eff) == 1L) {
      sampsize_eff <- min(sampsize_eff, nrow(x_train))
    } else {
      sampsize_eff <- pmin(sampsize_eff, nrow(x_train))
    }
  }

  ntree_list <- if (n_cores > 1L) {
    base <- ntree %/% n_cores
    remainder <- ntree %% n_cores
    out <- rep(base, n_cores)
    if (remainder > 0L) {
      out[seq_len(remainder)] <- out[seq_len(remainder)] + 1L
    }
    out[out > 0L]
  } else {
    ntree
  }

  if (n_cores > 1L && length(ntree_list) < 1L) {
    # n_cores is already capped at ntree, so every worker should get at least
    # one tree and this should be unreachable. Fall back to sequential rather
    # than hand foreach an empty iteration set.
    n_cores <- 1L
    ntree_list <- ntree
  }

  # ---- randomForest args ------------------------------------------------------
  rf_args <- list(
    x = x_train,
    y = y_train,
    ntree = NULL, # set per-worker or serially below
    importance = isTRUE(ctrl$importance),
    mtry = mtry_eff,
    nodesize = ctrl$nodesize,
    maxnodes = ctrl$maxnodes,
    sampsize = sampsize_eff,
    strata = ctrl$strata,
    classwt = ctrl$classwt,
    replace = isTRUE(ctrl$replace),
    do.trace = FALSE,
    keep.forest = TRUE
  )
  rf_args <- Filter(function(z) !is.null(z), rf_args) # drop NULLs

  # ---- Train model ------------------------------------------------------------
  rf_message(sprintf("Growing %d trees on %d worker(s).", ntree, n_cores),
             verbose)
  if (n_cores > 1L) {
    fs_require(c("foreach", "doParallel"), "parallel random forest training")

    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit({
      try(foreach::registerDoSEQ(), silent = TRUE)
      try(parallel::stopCluster(cl), silent = TRUE)
    }, add = TRUE)

    if (!is.null(seed)) {
      parallel::clusterSetRNGStream(cl, as.integer(seed))
    }

    ntree_part <- NULL # foreach iteration variable; quiets R CMD check
    dopar_op <- get("%dopar%", asNamespace("foreach"))
    rf_model <- dopar_op(
      foreach::foreach(
        ntree_part = ntree_list,
        .combine = randomForest::combine,
        .packages = "randomForest"
      ),
      {
        args_i <- rf_args
        args_i$ntree <- ntree_part
        do.call(randomForest::randomForest, args_i)
      }
    )
  } else {
    args_serial <- rf_args
    args_serial$ntree <- ntree
    rf_model <- do.call(randomForest::randomForest, args_serial)
  }

  # ---- Evaluate on test set ---------------------------------------------------
  target_idx_test <- match(target, names(test_df))
  x_test <- as.data.frame(test_df[, -target_idx_test, drop = FALSE])
  y_test <- test_df[[target]]

  metrics <- list()
  preds <- NULL
  probs <- NULL
  confusion <- NULL

  if (task == "classification") {
    preds <- stats::predict(rf_model, newdata = x_test, type = "class")

    suppressWarnings({
      probs <- try(stats::predict(rf_model, newdata = x_test, type = "prob"),
                   silent = TRUE)
      if (inherits(probs, "try-error")) probs <- NULL
    })

    acc <- mean(preds == y_test)

    kappa <- try(caret::confusionMatrix(preds, y_test)$overall[["Kappa"]],
                 silent = TRUE)
    if (inherits(kappa, "try-error") || is.null(kappa)) kappa <- NA_real_

    confusion <- table(Observed = y_test, Predicted = preds)

    auc <- NA_real_
    if (nlevels(y_test) == 2L && !is.null(probs)) {
      if (requireNamespace("pROC", quietly = TRUE)) {
        levels_y <- levels(y_test)

        # Determine positive class and level ordering for pROC.
        if (!is.null(ctrl$positive_class) &&
            ctrl$positive_class %in% levels_y) {
          positive <- ctrl$positive_class
          negative <- setdiff(levels_y, positive)
          roc_levels <- c(negative, positive)
        } else {
          positive <- levels_y[2L]
          roc_levels <- levels_y
        }

        # Ensure we have a matching probability column.
        if (!positive %in% colnames(probs)) {
          positive <- colnames(probs)[min(2L, ncol(probs))]
        }

        roc_obj <- pROC::roc(
          response = y_test,
          predictor = probs[, positive],
          quiet = TRUE,
          levels = roc_levels,
          direction = "<"
        )
        auc <- as.numeric(pROC::auc(roc_obj))
      } else {
        rf_message("Package 'pROC' is not installed; AUC was not computed.",
                   verbose)
      }
    }

    metrics <- list(accuracy = acc, kappa = unname(kappa), auc = auc)
  } else {
    preds <- as.numeric(stats::predict(rf_model, newdata = x_test))
    err <- preds - y_test
    rmse <- sqrt(mean(err^2))
    mae <- mean(abs(err))
    sst <- sum((y_test - mean(y_test))^2)
    r2 <- if (sst > 0) 1 - sum(err^2) / sst else NA_real_
    metrics <- list(RMSE = rmse, MAE = mae, R2 = r2)
  }

  # ---- Variable importance ----------------------------------------------------
  importance_df <- NULL
  scores <- NULL
  if (isTRUE(ctrl$importance)) {
    imp <- randomForest::importance(rf_model, type = 1,
                                    scale = isTRUE(ctrl$scale_importance))
    if (!is.null(imp)) {
      importance_scores <- imp[, ncol(imp)]
      importance_df <- data.frame(
        feature = rownames(imp),
        importance = importance_scores,
        row.names = NULL,
        check.names = FALSE
      )
      importance_df <- importance_df[order(-importance_df$importance), ,
                                     drop = FALSE]
      scores <- stats::setNames(as.numeric(importance_df$importance),
                                importance_df$feature)
    }
  }

  # ---- OOB metrics ------------------------------------------------------------
  oob <- NULL
  if (isTRUE(ctrl$oob)) {
    if (n_cores > 1L) {
      warning(paste(
        "OOB metrics are unavailable when the forest is trained in parallel:",
        "randomForest::combine() drops err.rate/mse/confusion.",
        "Returning NULL 'oob'; use n_cores = 1 to obtain OOB metrics."
      ), call. = FALSE)
    } else if (task == "classification" && !is.null(rf_model$err.rate)) {
      oob <- list(accuracy = 1 - rf_model$err.rate[rf_model$ntree, "OOB"])
    } else if (task == "regression" && !is.null(rf_model$mse)) {
      oob <- list(RMSE = sqrt(utils::tail(rf_model$mse, 1L)))
    }
  }

  # ---- Assemble result --------------------------------------------------------
  feature_names <- setdiff(names(train_df), target)
  # A plain random forest ranks rather than selects, so without a hook every
  # predictor is "selected"; with a hook, the hook's decision is the selection.
  selected_features <- if (is.null(hook_selected)) feature_names else hook_selected
  selected_features <- rf_rank_features(selected_features, scores)

  new_fs_result(
    selected = selected_features,
    scores   = scores,
    method   = "randomforest",
    task     = task,
    model    = rf_model,
    details  = list(
      metrics       = metrics,
      predictions   = preds,
      probabilities = probs,
      importance    = importance_df,
      confusion     = confusion,
      oob           = oob,
      feature_names = feature_names,
      train_index   = as.integer(index),
      test_data     = if (isTRUE(ctrl$return_test_data)) test_df else NULL,
      control       = ctrl,
      n_features    = n_candidates
    ),
    call = cl_call
  )
}
