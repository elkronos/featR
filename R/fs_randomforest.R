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
#' NA (and are later imputed when `control$impute = TRUE`). The caller drops
#' unused levels from the training factors first, so a level that occurs only
#' in the test rows is unseen here even when the column was a factor in
#' `data`.
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

#' Cohen's kappa from a square confusion table
#'
#' @param tab Observed x Predicted table with matching levels.
#' @return Kappa, or `NA` when it is undefined (chance agreement of 1).
#' @noRd
rf_cohen_kappa <- function(tab) {
  n <- sum(tab)
  if (n == 0L || nrow(tab) != ncol(tab)) return(NA_real_)
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / n^2
  if (!is.finite(pe) || pe >= 1) return(NA_real_)
  (po - pe) / (1 - pe)
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

#' Pool permutation-importance standard errors across sub-forests
#'
#' `randomForest` stores, for each permutation-importance column, the mean
#' over trees in `importance` and the standard error
#' `sqrt(popvar_over_trees / ntree)` in `importanceSD`. For forests grown
#' separately and merged with `randomForest::combine()`, this recovers each
#' forest's per-tree mean and population variance, pools them exactly, and
#' returns the standard error the merged forest would have had had it been
#' grown in one piece.
#'
#' @param forests List of `randomForest` objects fitted with
#'   `importance = TRUE` on the same predictors.
#' @return An object shaped like `forests[[1]]$importanceSD`.
#' @noRd
rf_pool_importance_sd <- function(forests) {
  template <- forests[[1L]]$importanceSD
  if (is.null(template)) {
    return(NULL)
  }
  n <- vapply(forests, function(f) as.numeric(f$ntree), numeric(1L))
  total <- sum(n)
  k <- NCOL(template)
  m <- lapply(forests, function(f) {
    as.matrix(f$importance)[, seq_len(k), drop = FALSE]
  })
  se <- lapply(forests, function(f) as.matrix(f$importanceSD))
  pooled_mean <- Reduce(`+`, Map(function(mi, ni) mi * ni, m, n)) / total
  # E[x^2] per forest = popvar + mean^2, with popvar = se^2 * ntree
  pooled_sq <- Reduce(`+`, Map(function(mi, si, ni) ni * (si^2 * ni + mi^2),
                               m, se, n)) / total
  out <- sqrt(pmax(pooled_sq - pooled_mean^2, 0) / total)
  if (is.matrix(template)) {
    dimnames(out) <- dimnames(template)
    out
  } else {
    stats::setNames(as.numeric(out), names(template))
  }
}

#' Random forest importance with a held-out evaluation
#'
#' Fits a random forest on a stratified training split, scores each predictor
#' by permutation importance, and evaluates the forest on the held-out rows.
#' Unless you supply `control$feature_select`, `selected` holds every
#' predictor the forest used, ordered by importance, and you choose where to
#' cut the list.
#'
#' @details
#' A random forest averages many decision trees, each grown on a bootstrap
#' sample of the training rows with a random subset of predictors tried at
#' each split. Forests take any mix of numeric, factor, character, and `Date`
#' predictors and need no scaling, so `fs_randomforest()` is a reasonable
#' default when the feature types are messy. Permutation importance is the
#' drop in out-of-bag accuracy (or rise in out-of-bag MSE) when one
#' predictor's values are shuffled. It tends to favor predictors with many
#' distinct values and to split credit between correlated predictors, so
#' treat close scores as ties.
#'
#' The honest performance estimate is `details$metrics`, computed on rows the
#' forest and the selection hook never saw (`control$preprocess` does see
#' them; see "What sees the held-out rows"). The
#' [Validation and leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.html)
#' article lists which numbers in each featR result are held out and which
#' are optimistic.
#'
#' ## Order of operations
#'
#' \enumerate{
#'   \item `control$preprocess` runs on the full table.
#'   \item `Date` columns become numeric, and the target is coerced and
#'     cleaned (see `target`).
#'   \item If `control$sample_size` is set, the table is downsampled.
#'   \item The rows are split into training and test sets, stratified on the
#'     target, with `control$train_ratio` of them used for training.
#'   \item `control$feature_select` runs on the training rows.
#'   \item Character predictors become factors, unused factor levels are
#'     dropped, and the test factors are re-leveled to the training levels.
#'   \item Near-zero-variance predictors are removed, judged on the training
#'     rows.
#'   \item Missing predictor values are imputed with values learned on the
#'     training rows.
#'   \item The forest is grown, on `n_cores` workers.
#'   \item The forest predicts the test rows, and permutation importance is
#'     read from it.
#' }
#'
#' Because near-zero-variance removal comes after the hook,
#' `details$feature_names` (the predictors the forest used) can be a subset
#' of `selected`.
#'
#' ## The control list
#'
#' Every accepted entry, with its default:
#' \itemize{
#'   \item `train_ratio = 0.75`: training proportion of the stratified split,
#'     strictly between 0 and 1.
#'   \item `sample_size = NULL`: optional whole number. When set, the data is
#'     downsampled to about this many rows *before* the split, proportionally
#'     within each class for classification and uniformly at random for
#'     regression. Values above the available row count are capped. `NULL`
#'     keeps every row.
#'   \item `ntree = 500`: number of trees, a whole number >= 1.
#'   \item `importance = TRUE`: compute permutation importance. When `FALSE`,
#'     `scores` and `details$importance` are `NULL` and `selected` keeps
#'     column order.
#'   \item `scale_importance = TRUE`: passed as `scale` to
#'     `randomForest::importance()`, which divides each permutation importance
#'     by its standard error over trees. Set `FALSE` for the raw importance.
#'     With more than one worker the standard error is pooled across the
#'     sub-forests, so scaled scores do not depend on `n_cores`.
#'   \item `mtry = NULL`: predictors tried at each split. `NULL` means
#'     `floor(sqrt(p))` for classification and `floor(p / 3)` for regression.
#'     Any value, supplied or derived, is clamped to `[1, p]`, where `p` counts
#'     the predictors left after the hook and near-zero-variance removal.
#'   \item `nodesize = NULL`: minimum size of terminal nodes, a whole
#'     number >= 1. `NULL` keeps the `randomForest` default (1 for
#'     classification, 5 for regression).
#'   \item `maxnodes = NULL`: maximum number of terminal nodes per tree, a
#'     whole number >= 2, or `NULL` for no limit.
#'   \item `sampsize = NULL`: rows drawn to grow each tree, passed to
#'     `randomForest`. Regression takes a single number (a vector is an
#'     error); classification also accepts one number per class. Entries must
#'     be whole numbers >= 1. A single value is clamped to the training row
#'     count. A per-class (or, with `strata`, per-stratum) vector has each
#'     entry clamped to that class's (stratum's) count in the training rows,
#'     with a warning, because `randomForest` refuses larger entries and you
#'     cannot know the training split in advance. An `NA` entry becomes the
#'     full training size when `replace = TRUE` and `ceiling(0.632 * n)`
#'     otherwise, and is then clamped the same way.
#'   \item `classwt = NULL`: class priors for classification, passed to
#'     `randomForest` unchanged and unvalidated.
#'   \item `strata = NULL`: name of a column of `data` to stratify the
#'     per-tree sampling on (used with `sampsize`). Its values are read from
#'     the training rows so they line up with the rows the forest is grown on.
#'     Passing the values as a vector is an error, because the training rows
#'     are chosen inside the function. The column must not contain
#'     `NA`, and it stays in the model as a predictor unless you remove it.
#'   \item `replace = TRUE`: sample rows with replacement when growing each
#'     tree.
#'   \item `preprocess = NULL`: a function `dt -> dt` applied to the full data
#'     before the split. It must return a data.frame or data.table that still
#'     contains the target. See "What sees the held-out rows" below.
#'   \item `feature_select = NULL`: a function `dt -> dt` that runs on the
#'     training rows only. It must keep the target and may only subset and
#'     reorder existing columns, because the same columns are then taken from
#'     the test rows. Put feature engineering in `preprocess`.
#'   \item `impute = TRUE`: replace missing predictor values with the median
#'     (numeric) or the most frequent value (factor, character, logical),
#'     learned on the training rows and applied to both splits.
#'   \item `drop_zerovar = TRUE`: remove near-zero-variance predictors with
#'     `caret::nearZeroVar()`, using the training rows only. With `FALSE`,
#'     training rows in which every predictor is constant are an error, since
#'     the trees have nothing to split on.
#'   \item `oob = TRUE`: report out-of-bag metrics in `details$oob`
#'     (`accuracy` for classification, `RMSE` for regression). They are
#'     optimistic after a supervised hook and unavailable after parallel
#'     training; see below.
#'   \item `return_test_data = FALSE`: when `TRUE`, the held-out rows are
#'     returned in `details$test_data`.
#'   \item `positive_class = NULL`: level treated as the positive class for
#'     binary AUC. `NULL` means the second factor level. A string that does
#'     not name a level of the cleaned target is an error.
#' }
#' Unknown entries are an error, so typos and arguments that moved out of
#' `control` (`seed`, `n_cores`, and `split_ratio`, now `train_ratio`) fail
#' loudly.
#'
#' ## What sees the held-out rows
#'
#' `control$feature_select` runs after the split on the training rows only,
#' so a hook that looks at the target does not leak the test rows into
#' `details$metrics`.
#'
#' The out-of-bag metrics in `details$oob` do not get that protection. The
#' forest is grown on the same training rows the hook selected on, so an
#' out-of-bag row was left out of some trees but was still used for the
#' selection. With a hook that consults the target, `details$oob` is
#' optimistic, and on pure noise it can read well above chance.
#'
#' `control$preprocess` runs on the full data by design. Any quantity it
#' learns from other rows, such as an imputation value, a scaling constant,
#' or anything computed from the target, includes the test rows and will
#' flatter `details$metrics`. Keep it to row-wise transformations.
#' Downsampling with `control$sample_size` also happens before the split,
#' which is why `details$train_index` indexes the cleaned and downsampled
#' table rather than the rows of `data`.
#'
#' When more than one worker is used (`n_cores` after the caps), the
#' sub-forests are merged with `randomForest::combine()`, which drops the
#' out-of-bag error structures (`err.rate`, `mse`, `confusion`).
#' `details$oob` is then `NULL`, with a warning if `control$oob = TRUE`.
#' `details$metrics` is unaffected; use `n_cores = 1` to get the out-of-bag
#' numbers.
#'
#' ## Factor levels and missing values
#'
#' Factor predictors carry only the levels seen in the training rows. A test
#' value unseen in training becomes `NA` and is imputed when
#' `control$impute = TRUE`. With `control$impute = FALSE`, any `NA` left in a
#' predictor reaches `randomForest`, which fails on it.
#'
#' ## Metrics
#'
#' Classification reports `accuracy`, `kappa`, and `auc` on the held-out
#' rows. `kappa` is Cohen's kappa from the held-out confusion table, `NA` when
#' chance agreement is 1. `auc` is filled in only when the target has exactly
#' two levels and the suggested package 'pROC' is installed. It is `NA` for
#' every multi-class fit, and also (with a warning) when the held-out rows
#' contain only one of the two classes.
#'
#' For a binary target, the positive class (whose predicted probability is
#' scored) is `control$positive_class` or, by default, the *second* factor
#' level after unused levels are dropped. `fs_mars()` uses the *first* level.
#' ROC AUC is the same whichever class is called positive, so `auc` does not
#' depend on this choice, but class-specific quantities such as
#' `fs_mars()`'s PR AUC or sensitivity are not directly comparable with
#' anything computed here.
#'
#' Regression reports `RMSE`, `MAE`, and `R2` on the held-out rows. `R2` is
#' `1 - SSE/SST`, so it can be negative; it is not the squared correlation
#' that [fs_mars()] reports. It is `NA` when the held-out target has no
#' variance, including variance that is only floating-point round-off
#' relative to the target's magnitude.
#'
#' @param data A data.frame or data.table with at least one row and one
#'   column, holding the target and the candidate predictors (every other
#'   column). Column names must be unique and non-empty, and predictors may
#'   not contain infinite values. `Date` columns are converted to numeric
#'   before modeling.
#' @param target Name of the target column of `data`, as a single string (a
#'   column index is not accepted). For classification the target is coerced
#'   with `as.factor()`, an ordered factor is treated as unordered, and at
#'   least two levels must remain. For regression it is coerced with
#'   `as.numeric()`, a factor through its labels
#'   (`as.numeric(as.character(y))`) and never its internal level codes, and
#'   an infinite value is an error. Either way, rows whose target is `NA`
#'   after coercion are dropped with a warning.
#' @param task `"classification"` (the default) or `"regression"`, matched
#'   with `match.arg()`. The task is not inferred from the target. If you
#'   leave `task` at its default and the target is numeric, a warning says
#'   that every distinct value will become a class.
#' @param control List of method-specific options. Every accepted entry and
#'   its default is listed under "The control list" in Details. Default
#'   `list()`, meaning all defaults.
#' @param seed Optional single whole number within the range of an R integer;
#'   fractional or out-of-range values are an error. The seed applies for the
#'   duration of the call and the previous RNG state is restored on exit.
#'   With more than one worker it is also passed to
#'   `parallel::clusterSetRNGStream()`, and it is then the only way to make
#'   the run reproducible. Default `NULL`: the RNG is never seeded unless you
#'   ask.
#' @param verbose Logical. If `TRUE`, print the split sizes, how many
#'   predictors the hook kept, the tree and worker counts, and a note when AUC
#'   is skipped because 'pROC' is missing. Default `FALSE`.
#' @param n_cores Number of workers used to grow the forest, a whole
#'   number >= 1. Default `1L` (sequential; no cluster is created). Capped at
#'   the detected core count and at `control$ntree`. An effective value above 1
#'   needs the suggested packages 'foreach' and 'doParallel', and leaves
#'   `details$oob` empty (see Details).
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector. With `control$feature_select`, the
#'     predictors the hook kept; otherwise every predictor that reached the
#'     forest. Ordered by decreasing importance when importance was computed.}
#'   \item{scores}{Named numeric vector of permutation importance from
#'     `randomForest::importance(type = 1)`: mean decrease in accuracy for
#'     classification, mean increase in MSE for regression (randomForest
#'     names that column with a percent sign, but it is not a percentage).
#'     Scaled or not according to `control$scale_importance`. `NULL` when
#'     `control$importance = FALSE`.}
#'   \item{method}{`"randomforest"`.}
#'   \item{task}{`"classification"` or `"regression"`, echoing the `task`
#'     argument.}
#'   \item{model}{The fitted `randomForest` object, or the result of
#'     `randomForest::combine()` when more than one worker was used.}
#'   \item{details}{A list with:
#'     \itemize{
#'       \item `metrics`: held-out metrics, a named list with `accuracy`,
#'         `kappa`, and `auc` for classification, or `RMSE`, `MAE`, and `R2`
#'         for regression.
#'       \item `predictions`: test-set predictions.
#'       \item `probabilities`: test-set class probability matrix
#'         (classification only; `NULL` if `predict()` cannot produce one).
#'       \item `importance`: data.frame with columns `feature` and
#'         `importance`, sorted descending; `NULL` when
#'         `control$importance = FALSE`.
#'       \item `confusion`: Observed x Predicted table (classification only).
#'       \item `oob`: out-of-bag `accuracy` or `RMSE`; `NULL` when trained in
#'         parallel or switched off.
#'       \item `feature_names`: the predictors the forest used.
#'       \item `train_index`: integer training rows of the cleaned and
#'         optionally downsampled data.
#'       \item `test_data`: the held-out rows when
#'         `control$return_test_data = TRUE`, `NULL` otherwise.
#'       \item `control`: the merged control list.
#'       \item `n_features`: candidate predictors after cleaning and before
#'         selection.
#'     }}
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
#'   # every predictor, in decreasing order of importance
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

  task_defaulted <- missing(task)
  task <- match.arg(task)
  assert_data_frame(data, "data")
  assert_target(data, target, arg = "target")
  assert_column_names(data, "fs_randomforest")
  if (task_defaulted && is.numeric(data[[target]])) {
    warning(sprintf(paste(
      "Target '%s' is numeric but 'task' defaulted to \"classification\",",
      "so every distinct value is treated as a class. Pass",
      "task = \"regression\" for a numeric outcome, or",
      "task = \"classification\" to silence this warning."
    ), target), call. = FALSE)
  }
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
  # randomForest() reads `strata` positionally against the rows it is given
  # and does not check its length. Those rows are the training split (after
  # NA removal and downsampling), which the caller cannot know in advance, so
  # a vector supplied for `data` would be silently misaligned. Take a column
  # name instead and read it from the training rows.
  if (!is.null(ctrl$strata) &&
      !(is.character(ctrl$strata) && length(ctrl$strata) == 1L &&
        !is.na(ctrl$strata))) {
    stop("'control$strata' must be the name of a column of 'data' (a single ",
         "string). A vector cannot be aligned with the training rows, which ",
         "are only chosen inside fs_randomforest().", call. = FALSE)
  }

  # ---- Worker count (sequential by default; capped at cores and ntree) -------
  n_cores <- resolve_cores(n_cores, arg = "n_cores")
  n_cores <- min(n_cores, ntree)

  # caret is used for splitting and near-zero variance filtering.
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
    # Unordered: predict() returns a plain factor, and comparing it with an
    # ordered test target fails ("comparison of these types is not
    # implemented").
    dt[[target]] <- factor(dt[[target]], ordered = FALSE)
    assert_class_labels(dt[[target]], target)

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
    if (!is.null(ctrl$positive_class) &&
        !ctrl$positive_class %in% levels(dt[[target]])) {
      stop(sprintf(
        "'control$positive_class' (\"%s\") is not a level of target '%s'; levels are: %s.",
        ctrl$positive_class, target,
        paste(utils::head(levels(dt[[target]]), 10L), collapse = ", ")
      ), call. = FALSE)
    }
  } else {
    # A factor goes through its labels: as.numeric() on a factor returns the
    # internal level codes 1..k, which have nothing to do with the values.
    y_raw <- dt[[target]]
    if (is.factor(y_raw)) y_raw <- as.character(y_raw)
    dt[[target]] <- suppressWarnings(as.numeric(y_raw))
    assert_finite_columns(dt, target, "fs_randomforest")

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
      # Done in base R, outside data.table's j scope: inside j a predictor
      # column named `total_n` or `desired_total` would shadow the locals.
      # Groups are visited in order of first appearance and the target is
      # moved first, matching the former grouped data.table call.
      y_ds <- dt[[target]]
      grp_rows <- split(seq_len(total_n),
                        factor(as.character(y_ds),
                               levels = unique(as.character(y_ds))))
      keep_ds <- unlist(lapply(grp_rows, function(rows) {
        n_g <- length(rows)
        size_g <- max(1L, min(n_g, floor((n_g / total_n) * desired_total)))
        rows[sample.int(n_g, size = size_g)]
      }), use.names = FALSE)
      dt <- dt[keep_ds]
      data.table::setcolorder(dt, c(target, setdiff(names(dt), target)))
    } else {
      keep_ds <- sample.int(total_n, size = desired_total)
      dt <- dt[keep_ds]
    }
  }

  n_candidates <- length(setdiff(names(dt), target))

  # ---- Train/test split (plain integer index) --------------------------------
  index <- fs_split_index(dt[[target]], p = ctrl$train_ratio)
  # Subset with data.frame semantics so no data.table NSE lookup is involved.
  df_all <- as.data.frame(dt)
  train_df <- df_all[index, , drop = FALSE]
  test_df <- df_all[-index, , drop = FALSE]

  # Per-tree sampling strata, read from the training rows themselves (before
  # any hook or filter can drop the column).
  strata_train <- NULL
  if (!is.null(ctrl$strata)) {
    if (!ctrl$strata %in% names(train_df)) {
      stop(sprintf("'control$strata' names column '%s', which is not in the data%s.",
                   ctrl$strata,
                   if (is.null(ctrl$preprocess)) "" else " after control$preprocess"),
           call. = FALSE)
    }
    if (anyNA(df_all[[ctrl$strata]])) {
      stop(sprintf(paste(
        "'control$strata' column '%s' contains NA; randomForest cannot",
        "sample from an unknown stratum. Remove or recode those rows first."
      ), ctrl$strata), call. = FALSE)
    }
    strata_train <- droplevels(as.factor(train_df[[ctrl$strata]]))
  }

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
    } else if (is.factor(train_df[[nm]])) {
      # Row subsetting keeps the full-data levels; drop those with no training
      # rows so a test-only level becomes NA (and is imputed) exactly as it
      # does for a character predictor.
      train_df[[nm]] <- droplevels(train_df[[nm]])
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
        } else if (is.logical(x)) {
          # Modal value, kept logical. Without this branch an NA in a logical
          # predictor survived imputation and randomForest() refused to fit.
          tab <- table(x, useNA = "no")
          impute_values[[i]] <- if (length(tab) > 0L) {
            as.logical(names(which.max(tab)))
          } else {
            NA
          }
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

  # ---- Infinite values (imputation only replaces NA/NaN) ----------------------
  inf_cols <- names(train_df)[vapply(names(train_df), function(nm) {
    nm != target && is.numeric(train_df[[nm]]) &&
      (any(is.infinite(train_df[[nm]])) ||
         (nm %in% names(test_df) && any(is.infinite(test_df[[nm]]))))
  }, logical(1L))]
  if (length(inf_cols) > 0L) {
    stop("Predictors contain infinite values (Inf/-Inf) in: ",
         paste(utils::head(inf_cols, 5L), collapse = ", "),
         if (length(inf_cols) > 5L) ", ..." else "",
         ". Remove or replace them before calling fs_randomforest().",
         call. = FALSE)
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
  # randomForest never terminates on a classification fit in which no
  # predictor can be split (reachable with control$drop_zerovar = FALSE).
  all_constant <- all(vapply(x_train, function(x) {
    length(unique(x[!is.na(x)])) <= 1L
  }, logical(1L)))
  if (all_constant) {
    stop("Every predictor is constant in the training rows, so the forest ",
         "has nothing to split on.", call. = FALSE)
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
      repl <- if (isTRUE(ctrl$replace)) n_obs else ceiling(0.632 * n_obs)
      # Replace only the NA entries. Overwriting the whole vector silently
      # discarded the per-class sizes the user did supply, e.g. c(10, NA).
      sampsize_eff[is.na(sampsize_eff)] <- repl
    }
    # Reject invalid sizes instead of silently replacing the whole vector with
    # 1L, which grew every tree on a single row (regression R2 ~ 0) or failed
    # deep inside randomForest ("fewer than two classes in the in-bag sample").
    if (!is.numeric(sampsize_eff) || any(!is.finite(sampsize_eff)) ||
        any(sampsize_eff < 1) || any(sampsize_eff != trunc(sampsize_eff))) {
      stop("'control$sampsize' must contain whole numbers >= 1 (or NA).",
           call. = FALSE)
    }
    sampsize_eff <- as.integer(sampsize_eff)
    if (length(sampsize_eff) == 1L) {
      sampsize_eff <- min(sampsize_eff, nrow(x_train))
    } else {
      # randomForest checks each entry against the frequency of its class (or
      # stratum) in the rows it is given, i.e. the internal training split,
      # which the caller cannot know in advance.
      group_freq <- if (!is.null(strata_train)) {
        table(strata_train)
      } else {
        table(y_train)
      }
      if (length(sampsize_eff) == length(group_freq)) {
        freq <- as.integer(group_freq)
        over <- sampsize_eff > freq
        if (any(over)) {
          warning(sprintf(paste(
            "'control$sampsize' clamped to the %s frequencies in the training",
            "split: %s."
          ),
          if (is.null(strata_train)) "class" else "stratum",
          paste(sprintf("%s %d -> %d", names(group_freq)[over],
                        sampsize_eff[over], freq[over]), collapse = ", ")),
          call. = FALSE)
          sampsize_eff <- pmin(sampsize_eff, freq)
        }
      } else {
        sampsize_eff <- pmin(sampsize_eff, nrow(x_train))
      }
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
    strata = strata_train,
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
    # Teardown (stop the cluster, restore the caller's foreach backend) is
    # scheduled before registration, so nothing leaks if registration fails.
    local_parallel_cluster(n_cores, seed = seed)

    ntree_part <- NULL # foreach iteration variable; quiets R CMD check
    dopar_op <- get("%dopar%", asNamespace("foreach"))
    forests <- dopar_op(
      foreach::foreach(
        ntree_part = ntree_list,
        .packages = "randomForest"
      ),
      {
        args_i <- rf_args
        args_i$ntree <- ntree_part
        do.call(randomForest::randomForest, args_i)
      }
    )
    rf_model <- do.call(randomForest::combine, forests)
    # combine() pools the per-forest standard errors as if each were a
    # per-tree SD, which inflates importanceSD by about sqrt(n_cores) and so
    # shrinks the scaled importance by the same factor. Recompute it from the
    # pieces so scale_importance = TRUE means the same thing at any n_cores.
    if (isTRUE(ctrl$importance)) {
      rf_model$importanceSD <- rf_pool_importance_sd(forests)
    }
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

    confusion <- table(Observed = y_test, Predicted = preds)
    # Cohen's kappa from the confusion table directly (the same quantity
    # caret::confusionMatrix() reports, without its hidden e1071 dependency).
    kappa <- rf_cohen_kappa(confusion)

    auc <- NA_real_
    if (nlevels(y_test) == 2L && !is.null(probs) &&
        any(table(y_test) == 0L)) {
      warning("The held-out rows contain only one of the two classes, so ",
              "AUC is undefined and reported as NA.", call. = FALSE)
    } else if (nlevels(y_test) == 2L && !is.null(probs)) {
      if (requireNamespace("pROC", quietly = TRUE)) {
        levels_y <- levels(y_test)

        # Determine positive class and level ordering for pROC.
        if (!is.null(ctrl$positive_class)) {
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
    # Relative test: a target that is constant up to round-off has a tiny
    # positive SST, and 1 - SSE/SST would then be a ratio of round-off.
    r2 <- if (sst > 0 && !is_numerically_constant(y_test, tol = 1e-12)) {
      1 - sum(err^2) / sst
    } else {
      NA_real_
    }
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
