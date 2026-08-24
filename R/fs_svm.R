# SVM training and evaluation pipeline built on caret, with optional dummy
# encoding, SVM-RFE (or random-forest RFE) feature selection, class-imbalance
# handling inside resampling, and cross-validated hyperparameter tuning.

#' Validate inputs for the SVM workflow
#'
#' `task`, `kernel`, and `select_method` are validated in fs_svm() before this
#' runs, so a typo fails immediately rather than after expensive work.
#'
#' @param data Data frame with predictors and the target.
#' @param target String naming the target column.
#' @param task "classification" or "regression" (already validated).
#' @param train_ratio Single number strictly between 0 and 1.
#' @param nfolds Single whole number > 1.
#' @return Invisibly TRUE when validation passes.
#' @noRd
svm_validate <- function(data, target, task, train_ratio, nfolds) {
  assert_data_frame(data, arg = "data")
  assert_target(data, target)
  assert_number(train_ratio, "train_ratio")
  if (train_ratio <= 0 || train_ratio >= 1) {
    stop("'train_ratio' must be a single numeric value in (0, 1).",
         call. = FALSE)
  }
  assert_count(nfolds, "nfolds", lower = 2L)

  if (nrow(data) < 2L) {
    stop("'data' must contain at least 2 rows.", call. = FALSE)
  }

  predictor_names <- setdiff(names(data), target)
  if (length(predictor_names) == 0L) {
    stop("'data' must contain at least one predictor column besides the target.",
         call. = FALSE)
  }

  if (anyNA(data[[target]])) {
    stop("'target' contains missing values; please handle them before modeling.",
         call. = FALSE)
  }
  if (anyNA(data[predictor_names])) {
    stop("Predictor columns contain missing values; please handle them before modeling.",
         call. = FALSE)
  }

  if (task == "regression" && !is.numeric(data[[target]])) {
    stop("For regression, the target must be numeric.", call. = FALSE)
  }
  if (task == "classification" && length(unique(data[[target]])) < 2L) {
    stop("For classification, the target must have at least two classes.",
         call. = FALSE)
  }

  invisible(TRUE)
}

#' Coerce the target column to the type required by the task
#'
#' For classification, a numeric target with more than 10 unique values is
#' rejected (it is almost certainly a regression target); with 10 or fewer it
#' is coerced to a factor with a message.
#'
#' @param data Data frame.
#' @param target Target column name.
#' @param task "classification" or "regression".
#' @return The data frame with the target coerced.
#' @noRd
svm_coerce_target <- function(data, target, task) {
  y <- data[[target]]
  if (task == "classification") {
    if (is.numeric(y)) {
      n_unique <- length(unique(y[!is.na(y)]))
      if (n_unique > 10L) {
        stop(
          sprintf(
            paste0(
              "'target' is numeric with %d unique values; ",
              "did you mean task = 'regression'?"
            ),
            n_unique
          ),
          call. = FALSE
        )
      }
      message(sprintf(
        "Coercing numeric target '%s' (%d unique values) to a factor for classification.",
        target, n_unique
      ))
    }
    data[[target]] <- as.factor(y)
  } else {
    data[[target]] <- as.numeric(y)
  }
  data
}

#' Align factor/character predictor levels between train and test sets
#'
#' Each factor or character predictor in the test set is re-leveled to the
#' training levels. Test rows containing levels unseen in training are
#' dropped with a warning; without this, caret's dummyVars prediction fails
#' on the first unseen level.
#'
#' @param train_set,test_set Data frames from the same split.
#' @param target Target column name (excluded from alignment).
#' @return List with the aligned `train_set` and `test_set`.
#' @noRd
svm_align_levels <- function(train_set, test_set, target) {
  unseen <- rep(FALSE, nrow(test_set))

  for (nm in setdiff(names(train_set), target)) {
    col_train <- train_set[[nm]]
    if (!is.factor(col_train) && !is.character(col_train)) {
      next
    }
    train_levels <- if (is.factor(col_train)) {
      levels(col_train)
    } else {
      sort(unique(as.character(col_train)))
    }
    test_chr <- as.character(test_set[[nm]])
    unseen <- unseen | (!is.na(test_chr) & !(test_chr %in% train_levels))
    train_set[[nm]] <- factor(as.character(col_train), levels = train_levels)
    test_set[[nm]] <- factor(test_chr, levels = train_levels)
  }

  n_unseen <- sum(unseen)
  if (n_unseen > 0L) {
    warning(
      sprintf(
        "Dropped %d test row(s) containing predictor factor levels unseen in the training set.",
        n_unseen
      ),
      call. = FALSE
    )
    test_set <- test_set[!unseen, , drop = FALSE]
    if (nrow(test_set) == 0L) {
      stop(
        "All test rows contained predictor factor levels unseen in the training set; adjust 'train_ratio' or check the data.",
        call. = FALSE
      )
    }
  }

  list(train_set = train_set, test_set = test_set)
}

#' Fit a dummy encoder on the predictors
#'
#' @param data Data frame.
#' @param target Target column name.
#' @param full_rank Logical; if TRUE, full-rank parameterization.
#' @return A caret::dummyVars object.
#' @noRd
svm_dummy_encoder <- function(data, target, full_rank = TRUE) {
  predictors <- setdiff(names(data), target)
  if (length(predictors) == 0L) {
    stop("No predictor columns available for dummy encoding.", call. = FALSE)
  }
  caret::dummyVars(
    formula = stats::reformulate(backtick(predictors)),
    data = data,
    fullRank = full_rank
  )
}

#' Transform data with a fitted dummy encoder
#'
#' @param dv A caret::dummyVars object.
#' @param data Data frame to transform.
#' @return Numeric data frame of encoded predictors.
#' @noRd
svm_encode <- function(dv, data) {
  as.data.frame(stats::predict(dv, newdata = data))
}

###############################################################################
# SVM-RFE (Guyon, Weston, Barnhill and Vapnik, 2002)
###############################################################################

#' Center and scale an encoded predictor matrix
#'
#' SVM-RFE compares squared weights across features, which is only meaningful
#' when the features share a scale. Columns with zero (or undefined) standard
#' deviation are centered but not rescaled, so they collapse to zeros instead
#' of producing NaNs.
#'
#' @param x Numeric matrix or data frame of encoded predictors.
#' @return A numeric matrix with the same dimnames as `x`.
#' @noRd
svm_center_scale <- function(x) {
  x <- as.matrix(x)
  if (!is.numeric(x)) {
    stop("SVM-RFE requires a numeric (dummy-encoded) predictor matrix.",
         call. = FALSE)
  }
  centers <- colMeans(x)
  sds <- apply(x, 2L, stats::sd)
  sds[!is.finite(sds) | sds == 0] <- 1
  out <- scale(x, center = centers, scale = sds)
  attr(out, "scaled:center") <- NULL
  attr(out, "scaled:scale") <- NULL
  out
}

#' Fit the linear SVM used by every SVM-RFE step
#'
#' `kpar = list()` is passed explicitly so that kernlab does not print its
#' "Setting default kernel parameters" notice, and `scaled = FALSE` because
#' `svm_center_scale()` has already standardized the matrix.
#'
#' @param x Numeric matrix of (already standardized) predictors.
#' @param y Factor (classification) or numeric (regression) outcome.
#' @param task "classification" or "regression".
#' @param C Cost parameter (fixed at 1 during elimination).
#' @return A `kernlab::ksvm` fit.
#' @noRd
svm_rfe_ksvm <- function(x, y, task, C = 1) {
  kernlab::ksvm(
    x = x,
    y = y,
    type = if (task == "classification") "C-svc" else "eps-svr",
    kernel = "vanilladot",
    kpar = list(),
    C = C,
    scaled = FALSE
  )
}

#' Squared primal weights of a linear kernlab fit
#'
#' For a linear kernel the primal weight vector is
#' `w = t(X_sv) %*% (alpha * y)`, i.e. `crossprod(xmatrix(fit)[[i]],
#' coef(fit)[[i]])`. kernlab stores support vectors and coefficients as lists:
#' one block for a binary problem or a regression, one block per class (with
#' one column per pairwise problem) for multi-class one-against-one fits. The
#' ranking criterion is `w^2` summed over every column of every block, which
#' reduces exactly to `w^2` in the binary/regression case.
#'
#' @param fit A `kernlab::ksvm` fit built with a linear kernel.
#' @param features Character vector naming the columns of the matrix that was
#'   passed to `fit`, in order.
#' @return Named numeric vector of squared weights, one entry per feature.
#' @noRd
svm_rfe_weights <- function(fit, features) {
  xm <- kernlab::xmatrix(fit)
  cf <- kernlab::coef(fit)
  if (!is.list(xm)) {
    xm <- list(xm)
  }
  if (!is.list(cf)) {
    cf <- list(cf)
  }

  crit <- stats::setNames(numeric(length(features)), features)

  add_block <- function(total, xi, ci) {
    xi <- as.matrix(xi)
    ci <- as.matrix(ci)
    if (nrow(xi) != nrow(ci) || ncol(xi) != length(features)) {
      return(NULL)
    }
    # (features x support vectors) %*% (support vectors x binary problems)
    w <- crossprod(xi, ci)
    contrib <- rowSums(w * w)
    if (!is.null(names(contrib)) && all(features %in% names(contrib))) {
      contrib <- contrib[features]
    }
    total + as.numeric(contrib)
  }

  used <- 0L
  for (i in seq_len(min(length(xm), length(cf)))) {
    updated <- add_block(crit, xm[[i]], cf[[i]])
    if (!is.null(updated)) {
      crit <- updated
      used <- used + 1L
    }
  }

  if (used == 0L) {
    # Fall back to the pooled blocks: some kernlab versions split the support
    # vectors and the coefficients differently for multi-class problems.
    pooled_x <- tryCatch(do.call(rbind, lapply(xm, as.matrix)),
                         error = function(e) NULL)
    pooled_c <- tryCatch(do.call(rbind, lapply(cf, as.matrix)),
                         error = function(e) NULL)
    if (!is.null(pooled_x) && !is.null(pooled_c)) {
      updated <- add_block(crit, pooled_x, pooled_c)
      if (!is.null(updated)) {
        crit <- updated
        # The pooled matrices carry every block's support vectors, so this
        # path covers all of them; count it as complete so the
        # partial-recovery check below does not misread it as a gap.
        used <- min(length(xm), length(cf))
      }
    }
  }

  if (used == 0L) {
    stop(
      paste0("Could not recover the primal weight vector from the kernlab ",
             "fit; SVM-RFE needs a linear ('vanilladot') kernel."),
      call. = FALSE
    )
  }

  # Every block must contribute. A partially summed criterion omits whole
  # pairwise problems, which yields a ranking that is wrong rather than
  # merely imprecise -- the one failure mode a selection function must never
  # have silently.
  n_blocks <- min(length(xm), length(cf))
  if (used < n_blocks) {
    stop(sprintf(
      paste0(
        "Recovered the weight vector from only %d of %d kernlab blocks, so ",
        "the SVM-RFE criterion would omit %d pairwise problem(s) and rank ",
        "features incorrectly. This indicates an unexpected kernlab fit ",
        "layout; use select_method = \"rf_rfe\" as an alternative."
      ),
      used, n_blocks, n_blocks - used
    ), call. = FALSE)
  }

  crit[!is.finite(crit)] <- 0
  crit
}

#' Candidate subset sizes for the SVM-RFE size search
#'
#' The ladder is the powers of two up to the number of features plus the full
#' size, trimmed to at most `max_sizes` entries (always keeping 1 and the
#' full size) so that the cross-validated search stays cheap.
#'
#' @param p Number of ranked features.
#' @param max_sizes Maximum number of candidate sizes.
#' @return Increasing integer vector of candidate sizes.
#' @noRd
svm_rfe_size_ladder <- function(p, max_sizes = 6L) {
  p <- as.integer(p)
  if (is.na(p) || p <= 1L) {
    return(1L)
  }
  sizes <- as.integer(2^(0:floor(log2(p))))
  sizes <- sort(unique(c(sizes[sizes <= p], p)))
  if (length(sizes) > max_sizes) {
    sizes <- sort(unique(c(1L, utils::tail(sizes, max_sizes - 1L))))
  }
  as.integer(sizes)
}

#' Cross-validated score of one candidate feature subset
#'
#' Uses the same linear SVM as the elimination loop, on folds that are shared
#' by every candidate size so the comparison is paired.
#'
#' @param x Standardized predictor matrix restricted to the candidate subset.
#' @param y Outcome vector.
#' @param task "classification" or "regression".
#' @param folds List of integer vectors of held-out row indices.
#' @return Mean accuracy (classification) or mean RMSE (regression); `NA_real_`
#'   when no fold could be scored.
#' @noRd
svm_rfe_cv_score <- function(x, y, task, folds) {
  n <- nrow(x)
  vals <- vapply(folds, function(idx) {
    idx <- as.integer(idx)
    train_idx <- setdiff(seq_len(n), idx)
    if (length(train_idx) < 2L || length(idx) < 1L) {
      return(NA_real_)
    }
    y_train <- y[train_idx]
    y_test <- y[idx]
    if (task == "classification") {
      y_train <- droplevels(as.factor(y_train))
      if (nlevels(y_train) < 2L) {
        return(NA_real_)
      }
    }
    fit <- tryCatch(
      svm_rfe_ksvm(x[train_idx, , drop = FALSE], y_train, task),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(NA_real_)
    }
    pred <- tryCatch(
      kernlab::predict(fit, x[idx, , drop = FALSE]),
      error = function(e) NULL
    )
    if (is.null(pred)) {
      return(NA_real_)
    }
    if (task == "classification") {
      mean(as.character(pred) == as.character(y_test))
    } else {
      sqrt(mean((as.numeric(pred) - as.numeric(y_test))^2))
    }
  }, numeric(1L))

  if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
}

#' SVM-RFE ranking and subset-size selection
#'
#' Implements recursive feature elimination for a linear SVM (Guyon et al.,
#' 2002): fit a linear SVM on the surviving features, rank them by the squared
#' primal weight `w^2`, drop the lowest-ranked feature (the lowest 10% while
#' more than 50 features remain, to keep wide problems tractable), and refit on
#' the reduced set until a single feature is left. Reversing the elimination
#' order gives the full ranking, so rank 1 is the feature eliminated last.
#'
#' When `n_features` is `NULL` the subset size is chosen by scoring a small
#' ladder of candidate sizes with `nfolds`-fold cross-validation on the same
#' linear SVM, keeping the size with the highest mean accuracy
#' (classification) or the lowest mean RMSE (regression); ties go to the
#' smaller size. The folds are drawn once and shared by every candidate size,
#' so the comparison is paired, and drawing them consumes the RNG.
#'
#' Two honest caveats about that size search. The ranking it scores was
#' derived from all the training rows, including each fold's held-out rows, so
#' the cross-validated scores are optimistic and should not be read as
#' estimates of out-of-sample performance -- they are only used to compare
#' sizes against each other. Fully nested RFE would re-rank inside every fold,
#' at a cost of one full elimination run per fold. The predictor matrix is
#' also centred and scaled once up front rather than per fold; that transform
#' is unsupervised, but it does see every row. The test-set metrics that
#' `fs_svm()` reports are unaffected: they come from rows held out before any
#' of this runs. If no candidate size could be scored at all (every fold failed), the full feature
#' set is kept.
#'
#' @param x Data frame or matrix of encoded (numeric) predictors. It is
#'   centered and scaled internally.
#' @param y Factor (classification) or numeric (regression) outcome.
#' @param task "classification" or "regression".
#' @param nfolds Number of folds for the subset-size search, clamped to at
#'   least 2 and at most `length(y)`.
#' @param n_features Optional whole number; when supplied the top
#'   `n_features` ranked features are kept (capped at the number available)
#'   and no size search is run.
#' @param verbose Logical; report each elimination step.
#' @return A list with `selected` (the retained features, most important
#'   first), `ranking` (every feature, most to least important), `scores` (the
#'   `w^2` criterion from the first, full-feature fit, in the original column
#'   order), `sizes` and `size_scores` (the candidate sizes and their CV
#'   scores) and `size_metric` ("accuracy" or "RMSE"). The last three are
#'   `NULL`, `NULL`, and `NA_character_` when `n_features` was supplied and no
#'   size search ran.
#' @noRd
svm_rfe_rank <- function(x, y, task, nfolds = 5L, n_features = NULL,
                         verbose = FALSE) {
  x <- svm_center_scale(x)
  features <- colnames(x)
  if (is.null(features) || length(features) == 0L) {
    stop("No predictors available for SVM-RFE.", call. = FALSE)
  }
  p <- length(features)

  if (!is.null(n_features)) {
    n_features <- assert_count(n_features, "n_features")
    n_features <- min(n_features, p)
  }

  remaining <- features
  eliminated <- character(0)
  criterion <- NULL

  while (length(remaining) > 1L) {
    # The CV scorer already guards this same call. Without a guard here, a
    # degenerate matrix (svm_center_scale() collapses constant columns to
    # zeros, so an all-constant encoding arrives as zeros) aborts the whole
    # call with kernlab's "No Support Vectors found" after the split, the
    # encoding and possibly a cluster are already set up.
    fit <- tryCatch(
      svm_rfe_ksvm(x[, remaining, drop = FALSE], y, task),
      error = function(e) {
        stop(sprintf(
          paste0(
            "SVM-RFE could not fit a linear SVM on %d surviving predictor(s): ",
            "%s. This usually means the encoded predictors are constant or ",
            "near-constant, which carries no signal for a linear SVM; check ",
            "for zero-variance columns or use select_method = \"rf_rfe\"."
          ),
          length(remaining), conditionMessage(e)
        ), call. = FALSE)
      }
    )
    crit <- svm_rfe_weights(fit, remaining)
    if (is.null(criterion)) {
      criterion <- crit
    }
    n_remaining <- length(remaining)
    n_drop <- if (n_remaining > 50L) {
      max(1L, as.integer(floor(n_remaining * 0.1)))
    } else {
      1L
    }
    n_drop <- min(n_drop, n_remaining - 1L)
    ord <- order(crit, seq_along(crit))
    drop_now <- names(crit)[ord[seq_len(n_drop)]]
    eliminated <- c(eliminated, drop_now)
    remaining <- setdiff(remaining, drop_now)
    if (isTRUE(verbose)) {
      message(sprintf(
        "SVM-RFE: eliminated %s (%d feature(s) remaining).",
        paste(drop_now, collapse = ", "), length(remaining)
      ))
    }
  }

  if (is.null(criterion)) {
    # p == 1: score the single feature so that `scores` is still populated
    criterion <- svm_rfe_weights(
      svm_rfe_ksvm(x[, remaining, drop = FALSE], y, task),
      remaining
    )
  }
  criterion <- criterion[features]
  names(criterion) <- features

  # Least important eliminated first, so reversing gives rank 1 = best.
  eliminated <- c(eliminated, remaining)
  ranking <- rev(eliminated)

  sizes <- NULL
  size_scores <- NULL
  size_metric <- NA_character_

  if (!is.null(n_features)) {
    selected <- utils::head(ranking, n_features)
  } else {
    sizes <- svm_rfe_size_ladder(p)
    k_folds <- max(2L, min(as.integer(nfolds), length(y)))
    folds <- caret::createFolds(y, k = k_folds, list = TRUE,
                                returnTrain = FALSE)
    size_scores <- vapply(
      sizes,
      function(k) {
        svm_rfe_cv_score(x[, utils::head(ranking, k), drop = FALSE], y, task,
                         folds)
      },
      numeric(1L)
    )
    names(size_scores) <- as.character(sizes)
    size_metric <- if (task == "classification") "accuracy" else "RMSE"

    if (all(is.na(size_scores))) {
      best_size <- p
    } else if (task == "classification") {
      best_size <- sizes[order(-size_scores, sizes, na.last = TRUE)][1L]
    } else {
      best_size <- sizes[order(size_scores, sizes, na.last = TRUE)][1L]
    }
    if (isTRUE(verbose)) {
      message(sprintf("SVM-RFE: keeping %d feature(s) by %s.",
                      best_size, size_metric))
    }
    selected <- utils::head(ranking, best_size)
  }

  list(
    selected    = selected,
    ranking     = ranking,
    scores      = criterion,
    sizes       = sizes,
    size_scores = size_scores,
    size_metric = size_metric
  )
}

###############################################################################
# Random-forest RFE screening (the alternative selector)
###############################################################################

#' Mean random-forest importance recorded by caret::rfe()
#'
#' `rfe()` stores the per-resample importance of every variable it considered
#' in `$variables`; averaging over resamples and subset sizes gives one
#' comparable score per encoded predictor.
#'
#' @param rfe_obj An object returned by `caret::rfe()`.
#' @param features Character vector of encoded predictor names.
#' @return Named numeric vector aligned to `features` (`NA` where unknown).
#' @noRd
svm_rf_importance <- function(rfe_obj, features) {
  out <- stats::setNames(rep(NA_real_, length(features)), features)
  vars <- rfe_obj$variables
  if (is.data.frame(vars) && all(c("Overall", "var") %in% names(vars))) {
    agg <- tapply(vars$Overall, as.character(vars$var), mean, na.rm = TRUE)
    common <- intersect(features, names(agg))
    if (length(common) > 0L) {
      out[common] <- as.numeric(agg[common])
    }
  }
  out
}

#' Random-forest recursive feature elimination on encoded predictors
#'
#' Note: this is random-forest RFE (`caret::rfFuncs`) used as a screening step
#' for the SVM; it is NOT SVM-RFE (see `svm_rfe_rank()`). Every subset size
#' from 1 to `ncol(x_enc)` is offered to `rfe()`, and the reported scores are
#' the per-resample importances it recorded, averaged per predictor.
#'
#' When `rfe()` errors or selects nothing, it falls back with a warning to a
#' plain `randomForest` fit and keeps the `min_keep` most important predictors
#' by mean decrease in node impurity (`randomForest::importance(type = 2)`);
#' the scores reported in that case are those impurity values, not the
#' resampled ones.
#'
#' @param x_enc Data frame of encoded (numeric) predictors.
#' @param y Outcome vector.
#' @param rfe_folds Number of CV folds for RFE. Default 10; `fs_svm()` does
#'   not pass its own `nfolds` here.
#' @param min_keep Number of predictors the fallback keeps: the top `min_keep`
#'   by importance, capped at the number available. It is a FLOOR, not a quota:
#'   the fallback keeps every predictor with positive impurity importance, and
#'   only drops back to `min_keep` top-ranked predictors when fewer than that
#'   many qualify. It does not constrain the `rfe()` path, which ignores it.
#' @param allow_parallel Logical; let RFE use a registered foreach backend.
#' @return A list with `selected` (character), `scores` (named numeric aligned
#'   to the encoded predictors, `NA` where no importance was recorded) and
#'   `fallback` (logical).
#' @noRd
svm_feature_selection <- function(x_enc,
                                  y,
                                  rfe_folds = 10,
                                  min_keep = 1L,
                                  allow_parallel = FALSE) {
  if (ncol(x_enc) == 0L) {
    stop("No predictors available for feature selection.", call. = FALSE)
  }
  feature_names <- colnames(x_enc)

  sizes <- seq_len(ncol(x_enc))
  ctrl <- caret::rfeControl(
    functions = caret::rfFuncs,
    method = "cv",
    number = rfe_folds,
    verbose = FALSE,
    allowParallel = allow_parallel
  )

  rfe_obj <- tryCatch(
    caret::rfe(x = x_enc, y = y, sizes = sizes, rfeControl = ctrl),
    error = function(e) e
  )

  reason <- "rfe selected no features"
  if (!inherits(rfe_obj, "condition")) {
    rfe_vars <- tryCatch(caret::predictors(rfe_obj),
                         error = function(e) character(0))
    if (length(rfe_vars) > 0L) {
      return(list(
        selected = rfe_vars,
        scores   = svm_rf_importance(rfe_obj, feature_names),
        fallback = FALSE
      ))
    }
  } else {
    reason <- conditionMessage(rfe_obj)
  }

  rf_fit <- randomForest::randomForest(x = x_enc, y = y, importance = TRUE)
  imp <- as.data.frame(randomForest::importance(rf_fit, type = 2))
  imp$Feature <- rownames(imp)
  score_col <- utils::tail(names(imp)[vapply(imp, is.numeric, logical(1L))], 1L)
  ord <- order(imp[[score_col]], decreasing = TRUE, na.last = NA)

  # `min_keep` is a FLOOR, not a quota. With rfe() failed there is no
  # cross-validated size estimate, so reducing to a fixed count (formerly 1)
  # threw away usable predictors for no stated reason. Keep everything that
  # earned positive impurity importance, and only fall back to a fixed count
  # if that leaves too few.
  keep_n <- max(as.integer(min_keep), 1L)
  ranked <- imp$Feature[ord]
  ranked_imp <- imp[[score_col]][ord]
  selected <- ranked[is.finite(ranked_imp) & ranked_imp > 0]
  if (length(selected) < keep_n) {
    selected <- utils::head(ranked, min(keep_n, length(ranked)))
  }
  if (length(selected) == 0L) {
    selected <- feature_names[seq_len(min(keep_n, length(feature_names)))]
  }

  scores <- stats::setNames(as.numeric(imp[[score_col]]), imp$Feature)
  scores <- scores[feature_names]
  names(scores) <- feature_names

  warning(
    sprintf(
      "feature selection failed (%s); falling back to top random-forest importance features (n = %d)",
      reason, length(selected)
    ),
    call. = FALSE
  )

  list(selected = selected, scores = scores, fallback = TRUE)
}

#' Default hyperparameter grids for the SVM kernels
#'
#' @param kernel One of "linear", "radial", "polynomial".
#' @return Data frame suitable for caret::train(tuneGrid = ...).
#' @noRd
svm_tune_grid <- function(kernel = c("linear", "radial", "polynomial")) {
  kernel <- match.arg(kernel)
  if (kernel == "linear") {
    expand.grid(C = 2^seq(-5, 10, by = 1))
  } else if (kernel == "radial") {
    expand.grid(
      sigma = 2^seq(-15, -5, by = 1),
      C = 2^seq(-1, 10, by = 1)
    )
  } else {
    expand.grid(
      degree = 2:5,
      scale = c(0.1, 0.5, 1),
      C = 2^seq(-3, 8, by = 1)
    )
  }
}

#' Compute performance metrics on the test set
#'
#' Prediction/actual pairs containing NA are dropped with a warning, and all
#' metrics (including MAE) are computed from the same NA-filtered vectors so
#' they are internally consistent.
#'
#' @param predictions Predicted values.
#' @param actuals True target values.
#' @param task "classification" or "regression".
#' @return For classification, a caret::confusionMatrix object; for
#'   regression, a named numeric vector with RMSE, Rsquared, and MAE.
#' @noRd
svm_performance <- function(predictions, actuals, task) {
  valid_idx <- !is.na(predictions) & !is.na(actuals)
  n_dropped <- sum(!valid_idx)
  if (n_dropped > 0L) {
    warning(
      sprintf(
        "%d prediction/actual pair(s) contained NA and were dropped before computing performance metrics.",
        n_dropped
      ),
      call. = FALSE
    )
    predictions <- predictions[valid_idx]
    actuals <- actuals[valid_idx]
  }

  if (task == "classification") {
    predictions <- factor(predictions, levels = levels(actuals))
    caret::confusionMatrix(predictions, actuals)
  } else {
    pr <- caret::postResample(pred = predictions, obs = actuals)
    mae <- mean(abs(predictions - actuals))
    c(
      RMSE = unname(pr["RMSE"]),
      Rsquared = unname(pr["Rsquared"]),
      MAE = mae
    )
  }
}

#' Start a parallel backend with an explicit worker count
#'
#' @param n_cores Integer >= 2 (already resolved via resolve_cores()).
#' @return The cluster object.
#' @noRd
svm_setup_parallel <- function(n_cores) {
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  cl
}

#' Stop a parallel backend started by svm_setup_parallel()
#'
#' @param cl Cluster object, or NULL for a no-op.
#' @return Invisibly NULL.
#' @noRd
svm_stop_parallel <- function(cl) {
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
    doParallel::stopImplicitCluster()
  }
  invisible(NULL)
}

#' Train and evaluate an SVM, with optional SVM-RFE feature selection
#'
#' Trains an SVM classifier or regressor using \pkg{caret} (with the
#' \pkg{kernlab} engines), with options for dummy encoding of predictors,
#' feature selection, class-imbalance handling, and hyperparameter tuning
#' via cross-validation. Optional parallel training uses an explicit worker
#' count.
#'
#' This is the wrapper to reach for when the selector and the final model
#' should belong to the same family: SVM-RFE ranks features by the weights of
#' a linear SVM rather than by an external proxy criterion, and the returned
#' object carries the tuned model and its held-out performance alongside the
#' chosen features. That comes at a price -- a full SVM fit at every
#' elimination step, plus a cross-validated size search -- so on wide data
#' screen first with a filter such as \code{\link{fs_supervised}}.
#'
#' @details
#' \itemize{
#'   \item Feature selection (\code{feature_select = TRUE}) defaults to
#'     \strong{SVM-RFE} (Guyon, Weston, Barnhill and Vapnik, 2002). A linear
#'     SVM is fitted on the centered and scaled encoded training matrix, the
#'     features are ranked by the squared primal weight \code{w^2} (recovered
#'     from the fit's support vectors and coefficients, summed over the
#'     pairwise problems of a multi-class fit), the lowest-ranked feature is
#'     dropped (the lowest 10\% while more than 50 features remain), and the
#'     SVM is \emph{refitted} on the reduced set until one feature is left.
#'     Reversing the elimination order gives the ranking, so rank 1 is the
#'     feature eliminated last. SVM-RFE requires \code{kernel = "linear"},
#'     because the ranking criterion is the primal weight vector, which only
#'     exists for a linear kernel; combining it with another kernel is an
#'     error that points at \code{select_method = "rf_rfe"}. The elimination
#'     and size-search fits use a fixed cost of \code{C = 1} and are separate
#'     from the final model, which is tuned over \code{tune_grid}.
#'   \item How many features SVM-RFE keeps: with \code{n_features} supplied,
#'     exactly that many (the top of the ranking). Otherwise a short ladder of
#'     candidate sizes -- the powers of two up to the number of features, plus
#'     the full size, trimmed to at most six entries but always including 1
#'     and the full size -- is scored by \code{nfolds}-fold cross-validation
#'     with the same linear SVM, on folds shared by every candidate size. The
#'     winner is the size with the highest mean accuracy (classification) or
#'     the lowest mean RMSE (regression), ties going to the smaller size; if
#'     no size could be scored, all features are kept.
#'   \item \code{select_method = "rf_rfe"} keeps the older random-forest
#'     screening (\code{caret::rfFuncs}) and works with every kernel. It runs
#'     its own 10-fold cross-validation over subset sizes 1 to p, independent
#'     of \code{nfolds}. If \code{rfe()} fails or selects nothing, a plain
#'     random forest is fitted and its most important features are used
#'     instead, with a warning -- and because the fallback keeps exactly
#'     \code{n_features} of them, a failure with \code{n_features = NULL}
#'     leaves a single feature.
#'   \item Selection always runs on the training split only, so the test set
#'     never informs which features survive.
#'   \item Class-imbalance handling (\code{class_imbalance = TRUE},
#'     classification only) up-samples \emph{within} each cross-validation
#'     resample via \code{caret::trainControl(sampling = "up")}, so no
#'     resampled rows leak across folds.
#'   \item After the train/test split, factor (and character) predictor
#'     levels in the test set are aligned to the training levels; test rows
#'     containing levels unseen in training are dropped with a warning.
#'   \item A numeric target with \code{task = "classification"} is coerced
#'     to a factor (with a message) only when it has at most 10 unique
#'     values; more than 10 unique values is an error suggesting
#'     \code{task = "regression"}.
#' }
#'
#' Suggested packages required at runtime: \pkg{caret} and \pkg{kernlab}
#' always, \pkg{e1071} for classification metrics, \pkg{randomForest} when
#' \code{feature_select = TRUE} and \code{select_method = "rf_rfe"}, and
#' \pkg{doParallel}/\pkg{foreach} when \code{n_cores > 1}.
#'
#' @param data A data frame containing predictors and the target.
#' @param target A string naming the target variable.
#' @param task Either \code{"classification"} or \code{"regression"}.
#'   Required; there is no default, because guessing it from the target is
#'   exactly the mistake this argument exists to prevent.
#' @param train_ratio Training set proportion, strictly between 0 and 1
#'   (default \code{0.7}).
#' @param nfolds Number of CV folds for hyperparameter tuning, a whole number
#'   greater than 1 (default \code{5}). Also the number of folds used by the
#'   SVM-RFE subset-size search (clamped there to at most the number of
#'   training rows); \code{select_method = "rf_rfe"} ignores it and uses 10
#'   folds.
#' @param kernel One of \code{"linear"} (default), \code{"radial"}, or
#'   \code{"polynomial"}.
#' @param tune_grid Optional tuning grid data frame. If \code{NULL}, a
#'   default grid for the chosen kernel is used.
#' @param feature_select Logical; if \code{TRUE}, run feature selection on the
#'   dummy-encoded training predictors (default \code{FALSE}).
#' @param select_method Which selector to run when
#'   \code{feature_select = TRUE}: \code{"svm_rfe"} (default, true SVM-RFE,
#'   linear kernel only) or \code{"rf_rfe"} (random-forest screening, any
#'   kernel). Ignored when \code{feature_select = FALSE}, and so is the
#'   linear-kernel requirement, which is only enforced when SVM-RFE will
#'   actually run. An unrecognized value is always an error.
#' @param n_features Optional whole number of features to keep, capped at the
#'   number of encoded predictors. For \code{"svm_rfe"} the top
#'   \code{n_features} ranked features are kept and the cross-validated size
#'   search is skipped; for \code{"rf_rfe"} it truncates the selection to its
#'   first \code{n_features} entries and sets how many features the
#'   random-forest fallback keeps. Ignored when
#'   \code{feature_select = FALSE}. Default \code{NULL} (the size is chosen
#'   automatically).
#' @param class_imbalance Logical; if \code{TRUE} and the task is
#'   classification, up-samples classes within CV resampling (default
#'   \code{FALSE}).
#' @param seed Optional seed, applied locally for the duration of the call
#'   and restored afterwards; also used to set reproducible RNG streams on
#'   parallel workers when \code{n_cores > 1}. Default \code{NULL} (never
#'   seeds by default).
#' @param verbose Logical; if \code{TRUE}, report progress (including each
#'   SVM-RFE elimination step). Default \code{FALSE}.
#' @param n_cores Number of parallel workers (default \code{1}, sequential).
#'   Requests are capped at the detected core count; when greater than 1 a
#'   cluster is created for the duration of the call and stopped on exit.
#'
#' @return An object of class \code{fs_result} with:
#' \describe{
#'   \item{selected}{Character vector of selected encoded feature names. When
#'     \code{feature_select = FALSE} this is every encoded predictor. With
#'     \code{"svm_rfe"} it is the surviving subset, ordered from most to least
#'     important; with \code{"rf_rfe"} it is the subset in the order
#'     \code{caret::rfe()} reports it.}
#'   \item{scores}{Named numeric vector covering every encoded predictor, not
#'     just the survivors: the SVM-RFE criterion (the squared primal weights
#'     \code{w^2} of the first, full-feature fit) or, for
#'     \code{select_method = "rf_rfe"}, the mean random-forest importance
#'     recorded across resamples (\code{NA} for predictors \code{rfe()} never
#'     scored, or the mean decrease in node impurity when the fallback ran).
#'     \code{NULL} when \code{feature_select = FALSE}, because no selector
#'     produced comparable scores.}
#'   \item{method}{\code{"svm_"} followed by the kernel, for example
#'     \code{"svm_linear"}. The selector that ran, if any, is reported in
#'     \code{details$selection$method}.}
#'   \item{task}{\code{"classification"} or \code{"regression"}.}
#'   \item{model}{The fitted \code{caret::train} object.}
#'   \item{details}{A list with \code{test_set} (the test split with its
#'     coerced target and aligned factor levels), \code{predictions} (test-set
#'     predictions), \code{performance} (a \code{caret::confusionMatrix} for
#'     classification, or a named RMSE/Rsquared/MAE vector for regression),
#'     \code{selection} (\code{NULL} when no selection ran, otherwise a list
#'     with \code{method}, \code{ranking} from most to least important,
#'     \code{scores}, and the size search's \code{sizes}, \code{size_scores}
#'     and \code{size_metric} -- those last three being \code{NULL},
#'     \code{NULL} and \code{NA} whenever no size search ran, which is always
#'     the case for \code{"rf_rfe"} and for \code{"svm_rfe"} with an explicit
#'     \code{n_features}), \code{encoder} (the fitted
#'     \code{caret::dummyVars} object) and \code{n_features} (the number of
#'     encoded predictors considered, counted before any were dropped).}
#'   \item{call}{The matched call.}
#' }
#' @examples
#' \donttest{
#' if (requireNamespace("caret", quietly = TRUE) &&
#'     requireNamespace("kernlab", quietly = TRUE) &&
#'     requireNamespace("e1071", quietly = TRUE)) {
#'   res <- fs_svm(
#'     data = iris,
#'     target = "Species",
#'     task = "classification",
#'     nfolds = 3,
#'     kernel = "linear",
#'     tune_grid = data.frame(C = 1),
#'     seed = 42
#'   )
#'   res$details$performance
#'
#'   # SVM-RFE keeps the two most useful measurements
#'   sel <- fs_svm(
#'     data = iris,
#'     target = "Species",
#'     task = "classification",
#'     nfolds = 3,
#'     kernel = "linear",
#'     tune_grid = data.frame(C = 1),
#'     feature_select = TRUE,
#'     select_method = "svm_rfe",
#'     n_features = 2,
#'     seed = 42
#'   )
#'   sel$selected
#'   sel$scores
#' }
#' }
#' @export
fs_svm <- function(data,
                   target,
                   task,
                   train_ratio = 0.7,
                   nfolds = 5,
                   kernel = c("linear", "radial", "polynomial"),
                   tune_grid = NULL,
                   feature_select = FALSE,
                   select_method = c("svm_rfe", "rf_rfe"),
                   n_features = NULL,
                   class_imbalance = FALSE,
                   seed = NULL,
                   verbose = FALSE,
                   n_cores = 1L) {
  cl_call <- match.call()

  # Validate the cheap, typo-prone arguments first: a bad kernel, selector or
  # task must fail immediately, not after minutes of feature selection.
  kernel <- match.arg(kernel)
  select_method <- match.arg(select_method)
  assert_string(task, "task")
  if (!task %in% c("classification", "regression")) {
    stop("'task' must be either 'classification' or 'regression'.",
         call. = FALSE)
  }

  assert_data_frame(data, arg = "data")
  data <- as.data.frame(data)
  svm_validate(data, target, task, train_ratio, nfolds)
  assert_flag(feature_select, "feature_select")
  assert_flag(class_imbalance, "class_imbalance")
  assert_flag(verbose, "verbose")
  if (!is.null(n_features)) {
    n_features <- assert_count(n_features, "n_features")
  }

  # SVM-RFE ranks features by the primal weight vector, which only exists for
  # a linear kernel.
  if (feature_select && select_method == "svm_rfe" && kernel != "linear") {
    stop(
      sprintf(
        paste0(
          "SVM-RFE requires a linear kernel, but kernel = \"%s\". ",
          "Either set kernel = \"linear\" to run SVM-RFE, or set ",
          "select_method = \"rf_rfe\" to screen features with a random ",
          "forest before fitting the %s-kernel SVM."
        ),
        kernel, kernel
      ),
      call. = FALSE
    )
  }

  if (!is.null(tune_grid)) {
    assert_data_frame(tune_grid, arg = "tune_grid")
  }
  n_cores <- resolve_cores(n_cores)
  use_parallel <- n_cores > 1L

  pkgs <- c("caret", "kernlab")
  if (task == "classification") {
    pkgs <- c(pkgs, "e1071")
  }
  if (feature_select && select_method == "rf_rfe") {
    pkgs <- c(pkgs, "randomForest")
  }
  fs_require(pkgs, "SVM training and evaluation")

  # One local seed for the whole call; restored when fs_svm() exits.
  local_seed(seed)

  data <- svm_coerce_target(data, target, task)

  # Stratified split (classification strata; quantile groups for numeric
  # targets). The seed above already governs this draw.
  idx <- fs_split_index(data[[target]], p = train_ratio)
  train_set <- data[idx, , drop = FALSE]
  test_set <- data[-idx, , drop = FALSE]

  if (nrow(train_set) == 0L || nrow(test_set) == 0L) {
    stop("Training or test set is empty; adjust 'train_ratio' or check data size.",
         call. = FALSE)
  }

  if (task == "classification") {
    train_set[[target]] <- droplevels(train_set[[target]])
    if (length(unique(train_set[[target]])) < 2L) {
      stop(
        "Training set contains fewer than 2 classes after splitting; adjust 'train_ratio' or handle class imbalance.",
        call. = FALSE
      )
    }
    test_set[[target]] <- factor(
      test_set[[target]],
      levels = levels(train_set[[target]])
    )
    keep_rows <- !is.na(test_set[[target]])
    if (!all(keep_rows)) {
      warning(
        sprintf(
          "Dropped %d test row(s) whose target class was not present in the training set.",
          sum(!keep_rows)
        ),
        call. = FALSE
      )
      test_set <- test_set[keep_rows, , drop = FALSE]
    }
    if (nrow(test_set) == 0L) {
      stop(
        "All test set rows had classes not present in training; adjust 'train_ratio' or check class distribution.",
        call. = FALSE
      )
    }
  }

  # Align factor predictor levels; drops (with a warning) test rows carrying
  # levels unseen in training, which would otherwise crash dummy encoding.
  aligned <- svm_align_levels(train_set, test_set, target)
  train_set <- aligned$train_set
  test_set <- aligned$test_set

  # Optional parallel backend, created early so feature selection and
  # training can share it, and always cleaned up on exit.
  cl <- NULL
  if (use_parallel) {
    fs_require(c("doParallel", "foreach"), "parallel SVM training")
    cl <- svm_setup_parallel(n_cores)
    on.exit(svm_stop_parallel(cl), add = TRUE)
    if (!is.null(seed)) {
      parallel::clusterSetRNGStream(cl, iseed = as.integer(seed))
    }
  }

  # Dummy-encode predictors (fitted on the training split only).
  dv <- svm_dummy_encoder(train_set, target, full_rank = TRUE)
  train_x <- svm_encode(dv, train_set)
  test_x <- svm_encode(dv, test_set)
  n_encoded <- ncol(train_x)

  if (verbose) {
    message(sprintf(
      "Training on %d row(s) and %d encoded predictor(s); testing on %d row(s).",
      nrow(train_set), n_encoded, nrow(test_set)
    ))
  }

  selected_features <- colnames(train_x)
  scores <- NULL
  selection <- NULL

  if (feature_select) {
    if (select_method == "svm_rfe") {
      rfe_res <- svm_rfe_rank(
        x = train_x,
        y = train_set[[target]],
        task = task,
        nfolds = nfolds,
        n_features = n_features,
        verbose = verbose
      )
      selected_features <- rfe_res$selected
      scores <- rfe_res$scores
      selection <- list(
        method      = "svm_rfe",
        ranking     = rfe_res$ranking,
        scores      = rfe_res$scores,
        sizes       = rfe_res$sizes,
        size_scores = rfe_res$size_scores,
        size_metric = rfe_res$size_metric
      )
    } else {
      rf_res <- svm_feature_selection(
        x_enc = train_x,
        y = train_set[[target]],
        min_keep = if (is.null(n_features)) 1L else n_features,
        allow_parallel = use_parallel
      )
      selected_features <- rf_res$selected
      if (!is.null(n_features) && length(selected_features) > n_features) {
        selected_features <- utils::head(selected_features, n_features)
      }
      scores <- rf_res$scores
      selection <- list(
        method      = "rf_rfe",
        ranking     = names(scores)[order(scores, decreasing = TRUE,
                                          na.last = TRUE)],
        scores      = scores,
        sizes       = NULL,
        size_scores = NULL,
        size_metric = NA_character_
      )
    }

    keep <- selected_features[selected_features %in% colnames(train_x)]
    if (length(keep) == 0L) {
      stop("No features selected by feature selection.", call. = FALSE)
    }
    selected_features <- keep
    train_x <- train_x[, keep, drop = FALSE]
    test_x <- test_x[, keep, drop = FALSE]
  }

  # Modeling frames: encoded predictors + target.
  train_df <- cbind(train_x, stats::setNames(list(train_set[[target]]), target))
  test_df <- cbind(test_x, stats::setNames(list(test_set[[target]]), target))

  caret_method <- switch(
    kernel,
    linear = "svmLinear",
    radial = "svmRadial",
    polynomial = "svmPoly"
  )

  if (is.null(tune_grid)) {
    tune_grid <- svm_tune_grid(kernel)
  }

  # Class-imbalance handling happens inside each CV resample (no leakage of
  # duplicated rows across folds), via caret's sampling hook.
  sampling <- if (task == "classification" && class_imbalance) "up" else NULL
  tr_ctrl <- caret::trainControl(
    method = "cv",
    number = nfolds,
    allowParallel = use_parallel,
    sampling = sampling
  )

  svm_fit <- caret::train(
    stats::as.formula(paste(backtick(target), "~ .")),
    data = train_df,
    method = caret_method,
    trControl = tr_ctrl,
    preProcess = c("center", "scale"),
    tuneGrid = tune_grid
  )

  preds <- stats::predict(svm_fit, newdata = test_df)
  perf <- svm_performance(preds, test_df[[target]], task)

  new_fs_result(
    selected = selected_features,
    scores   = scores,
    method   = paste0("svm_", kernel),
    task     = task,
    model    = svm_fit,
    details  = list(
      test_set    = test_set,
      predictions = preds,
      performance = perf,
      selection   = selection,
      encoder     = dv,
      n_features  = n_encoded
    ),
    call = cl_call
  )
}
