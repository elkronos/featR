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
  assert_column_names(data, "fs_svm")
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
  # Inf reaches caret/kernlab as cryptic engine errors otherwise.
  assert_finite_columns(data, c(predictor_names, target), "fs_svm")

  if (task == "regression" && !is.numeric(data[[target]])) {
    stop("For regression, the target must be numeric.", call. = FALSE)
  }
  if (task == "classification" && length(unique(data[[target]])) < 2L) {
    stop("For classification, the target must have at least two classes.",
         call. = FALSE)
  }
  if (task == "classification") {
    assert_class_labels(data[[target]], target)
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
    # Unused levels would make createDataPartition() warn on every call.
    data[[target]] <- droplevels(as.factor(y))
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
    # Levels OBSERVED in the training rows: subsetting keeps every level of a
    # factor, so levels(col_train) would let unseen test levels through and
    # produce all-zero training dummies for unused or test-only levels.
    train_levels <- if (is.factor(col_train)) {
      levels(droplevels(col_train))
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
  out <- as.data.frame(stats::predict(dv, newdata = data))
  # svm_dummy_encoder() backticks non-syntactic names so the formula parses,
  # and dummyVars() copies those backticks into its column names ("`a b`",
  # "`f g`v"). Unquote them so encoded names match the user's column names.
  # backtick() escapes literal backticks and backslashes, so deleting every
  # backtick would misname "a`b" as "a\b" (the name of a different column).
  names(out) <- unbacktick_names(names(out))
  out
}

###############################################################################
# SVM-RFE (Guyon, Weston, Barnhill and Vapnik, 2002)
###############################################################################

#' Center and scale an encoded predictor matrix
#'
#' SVM-RFE compares squared weights across features, which is only meaningful
#' when the features share a scale. Columns of `ref` that are numerically
#' constant (`is_numerically_constant()`: a spread negligible relative to the
#' magnitude, so round-off such as `0.1 + 0.2` vs `0.3` counts as constant)
#' are centered but not rescaled (so they collapse to round-off-sized values)
#' instead of being divided by a round-off-sized standard deviation, which
#' would blow them up to full weight. The standard
#' deviation of the other columns is computed after dividing by the column's
#' largest absolute value, so columns in tiny units (e.g. `1e-170`) do not
#' underflow to `sd == 0` and are still standardized.
#'
#' When `ref` is supplied, the centers and standard deviations are estimated
#' on `ref` and applied to `x`; this is how a cross-validation fold's held-out
#' rows are standardized with statistics from that fold's training rows only.
#'
#' @param x Numeric matrix or data frame of encoded predictors.
#' @param ref Optional matrix (same columns as `x`) supplying the centering
#'   and scaling statistics. Defaults to `x` itself.
#' @return A numeric matrix with the same dimnames as `x`.
#' @noRd
svm_center_scale <- function(x, ref = x) {
  x <- as.matrix(x)
  ref <- as.matrix(ref)
  if (!is.numeric(x) || !is.numeric(ref)) {
    stop("SVM-RFE requires a numeric (dummy-encoded) predictor matrix.",
         call. = FALSE)
  }
  const <- apply(ref, 2L, is_numerically_constant)
  centers <- colMeans(ref)
  sds <- apply(ref, 2L, function(v) {
    m <- max(abs(v))
    if (!is.finite(m) || m == 0) return(NA_real_)
    stats::sd(v / m) * m
  })
  bad <- const | !is.finite(sds) | sds == 0
  sds[bad] <- 1
  centers[!is.finite(centers)] <- 0
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
#' coef(fit)[[i]])`. kernlab stores support vectors and coefficients as lists
#' with one block per binary machine: a single block for a binary problem (a
#' bare matrix/vector for a regression), and one block per one-against-one
#' pairwise problem for a multi-class fit. For each block,
#' `x %*% w - b(fit)[i]` reproduces kernlab's decision values exactly. The
#' ranking criterion is `w^2` summed over every block (the usual multi-class
#' extension of Guyon et al.), which reduces exactly to `w^2` in the
#' binary/regression case.
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

#' Recursive elimination on an already standardized matrix
#'
#' Fits a linear SVM on the surviving features, ranks them by `w^2`, drops the
#' lowest-ranked feature (the lowest 10 percent while more than 50 remain) and
#' refits on the reduced set until one feature is left.
#'
#' @param x Standardized numeric matrix with column names.
#' @param y Outcome vector.
#' @param task "classification" or "regression".
#' @param verbose Logical; report each elimination step.
#' @return A list with `ranking` (most to least important) and `criterion`
#'   (the `w^2` of the first, full-feature fit, in column order).
#' @noRd
svm_rfe_eliminate <- function(x, y, task, verbose = FALSE) {
  features <- colnames(x)
  remaining <- features
  eliminated <- character(0)
  criterion <- NULL

  while (length(remaining) > 1L) {
    # Without a guard here, a degenerate matrix (svm_center_scale() collapses
    # constant columns to zeros, so an all-constant encoding arrives as zeros)
    # aborts with kernlab's opaque "No Support Vectors found".
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
  list(ranking = rev(c(eliminated, remaining)), criterion = criterion)
}

#' Score every candidate size on one cross-validation fold
#'
#' Everything that is learned from data is learned on the fold's training rows
#' only: the centering/scaling statistics, the SVM-RFE ranking and the SVMs
#' fitted on each candidate subset. The held-out rows are used solely for
#' scoring. Re-ranking inside the fold is what keeps the size search free of
#' the selection bias of scoring a ranking that has already seen the held-out
#' rows (Ambroise and McLachlan, 2002).
#'
#' @param x Numeric matrix of encoded (unscaled) predictors.
#' @param y Outcome vector.
#' @param task "classification" or "regression".
#' @param sizes Candidate subset sizes.
#' @param idx Integer vector of held-out row indices.
#' @return Numeric vector, one accuracy (classification) or RMSE (regression)
#'   per entry of `sizes`; `NA` where the fold could not be scored.
#' @noRd
svm_rfe_fold_scores <- function(x, y, task, sizes, idx) {
  out <- rep(NA_real_, length(sizes))
  idx <- as.integer(idx)
  train_idx <- setdiff(seq_len(nrow(x)), idx)
  if (length(train_idx) < 2L || length(idx) < 1L) {
    return(out)
  }
  y_train <- y[train_idx]
  y_test <- y[idx]
  if (task == "classification") {
    y_train <- droplevels(as.factor(y_train))
    if (nlevels(y_train) < 2L) {
      return(out)
    }
  }
  x_train <- svm_center_scale(x[train_idx, , drop = FALSE])
  x_test <- svm_center_scale(x[idx, , drop = FALSE],
                             ref = x[train_idx, , drop = FALSE])

  ranking <- tryCatch(
    svm_rfe_eliminate(x_train, y_train, task)$ranking,
    error = function(e) NULL
  )
  if (is.null(ranking)) {
    return(out)
  }

  for (j in seq_along(sizes)) {
    keep <- utils::head(ranking, sizes[j])
    fit <- tryCatch(
      svm_rfe_ksvm(x_train[, keep, drop = FALSE], y_train, task),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      next
    }
    pred <- tryCatch(
      kernlab::predict(fit, x_test[, keep, drop = FALSE]),
      error = function(e) NULL
    )
    if (is.null(pred)) {
      next
    }
    out[j] <- if (task == "classification") {
      mean(as.character(pred) == as.character(y_test))
    } else {
      sqrt(mean((as.numeric(pred) - as.numeric(y_test))^2))
    }
  }
  out
}

#' SVM-RFE ranking and subset-size selection
#'
#' Implements recursive feature elimination for a linear SVM (Guyon et al.,
#' 2002): fit a linear SVM on the surviving features, rank them by the squared
#' primal weight `w^2`, drop the lowest-ranked feature (the lowest 10 percent
#' while more than 50 features remain, to keep wide problems tractable), and
#' refit on the reduced set until a single feature is left. Reversing the
#' elimination order gives the full ranking, so rank 1 is the feature
#' eliminated last.
#'
#' When `n_features` is `NULL` the subset size is chosen by a nested
#' `nfolds`-fold cross-validation over a small ladder of candidate sizes.
#' Inside every fold the predictors are standardized with that fold's training
#' statistics, the whole elimination is re-run on the fold's training rows,
#' and the top-`k` features of that fold-specific ranking are scored on the
#' held-out rows for each candidate `k`. The size with the highest mean
#' accuracy (classification) or lowest mean RMSE (regression) wins, ties going
#' to the smaller size, and the final subset is the top of the ranking computed
#' on all rows. The folds are drawn once and shared by every candidate size,
#' so the comparison is paired, and drawing them consumes the RNG. Because the
#' held-out rows never inform the ranking they score, each size score is an
#' honest cross-validated estimate for the RFE procedure at that size; the
#' score of the WINNING size is not, because it is the maximum (or minimum
#' RMSE) over the ladder and so is optimistically biased. If no candidate
#' size could be scored at all (every fold failed), the full feature set is
#' kept with a warning.
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
#'   order), `sizes` and `size_scores` (the candidate sizes and their nested CV
#'   scores) and `size_metric` ("accuracy" or "RMSE"). The last three are
#'   `NULL`, `NULL`, and `NA_character_` when `n_features` was supplied and no
#'   size search ran.
#' @noRd
svm_rfe_rank <- function(x, y, task, nfolds = 5L, n_features = NULL,
                         verbose = FALSE) {
  x_raw <- as.matrix(x)
  x <- svm_center_scale(x_raw)
  features <- colnames(x)
  if (is.null(features) || length(features) == 0L) {
    stop("No predictors available for SVM-RFE.", call. = FALSE)
  }
  p <- length(features)

  if (!is.null(n_features)) {
    n_features <- assert_count(n_features, "n_features")
    n_features <- min(n_features, p)
  }

  full <- svm_rfe_eliminate(x, y, task, verbose = verbose)
  ranking <- full$ranking
  criterion <- full$criterion

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
    fold_scores <- vapply(
      folds,
      function(idx) svm_rfe_fold_scores(x_raw, y, task, sizes, idx),
      numeric(length(sizes))
    )
    fold_scores <- matrix(fold_scores, nrow = length(sizes))
    size_scores <- apply(fold_scores, 1L, function(v) {
      if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
    })
    names(size_scores) <- as.character(sizes)
    size_metric <- if (task == "classification") "accuracy" else "RMSE"

    if (all(is.na(size_scores))) {
      warning(sprintf(
        paste0(
          "SVM-RFE size search could not score any candidate size (every ",
          "fold failed, e.g. too few rows or a single class per fold); ",
          "keeping all %d features. Supply 'n_features' to choose the size ",
          "explicitly."
        ),
        p
      ), call. = FALSE)
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
#' in `$variables`, once per subset size the variable survived to. Averaging
#' all of those rows would weight each resample by how far the variable
#' survived there -- that is, by how important it looked in that resample --
#' which inflates the scores of marginal predictors. Only the full-size rows
#' (every predictor present, once per resample) are averaged, giving one
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
    if ("Variables" %in% names(vars) && nrow(vars) > 0L) {
      vars <- vars[vars$Variables == max(vars$Variables), , drop = FALSE]
    }
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
#' Numerically constant columns are removed first (randomForest never returns
#' on a constant size-1 subset); they get an `NA` score and are never
#' selected.
#'
#' When `rfe()` errors or selects nothing, it falls back with a warning to a
#' plain `randomForest` fit and keeps every predictor with positive
#' permutation importance (`randomForest::importance(type = 1)`), or the
#' `min_keep` top-ranked predictors if fewer than `min_keep` qualify. The
#' scores reported in that case are those permutation importances, not the
#' resampled ones. Positive permutation importance is a weak screen (about
#' half of pure-noise predictors pass it).
#'
#' @param x_enc Data frame of encoded (numeric) predictors.
#' @param y Outcome vector.
#' @param rfe_folds Number of CV folds for RFE. Default 10; `fs_svm()` does
#'   not pass its own `nfolds` here.
#' @param min_keep Minimum number of predictors the fallback keeps (a FLOOR,
#'   not a quota), capped at the number available: the fallback keeps every
#'   predictor with positive permutation importance, and only drops back to
#'   the `min_keep` top-ranked predictors when fewer than that many qualify.
#'   It does not constrain the `rfe()` path, which ignores it.
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

  # randomForest never returns when a size-1 subset (or the whole matrix) is
  # a constant column, and no error is raised for tryCatch to catch. Constant
  # columns carry no information, so they are screened out before rfe() and
  # the fallback; their score is NA and they are never selected.
  x_enc <- as.data.frame(x_enc, check.names = FALSE)
  is_const <- vapply(x_enc, is_numerically_constant, logical(1L))
  if (all(is_const)) {
    stop(
      "Every encoded predictor is constant on the training split; there is nothing for feature selection to rank.",
      call. = FALSE
    )
  }
  x_enc <- x_enc[, !is_const, drop = FALSE]

  sizes <- seq_len(ncol(x_enc))
  ctrl <- caret::rfeControl(
    # Guarded: a column constant inside one CV fold would hang randomForest.
    functions = rfe_guard_rf_funcs(caret::rfFuncs),
    method = "cv",
    number = rfe_folds,
    verbose = FALSE,
    allowParallel = allow_parallel,
    # In parallel, resamples go to whichever worker is free, so per-worker RNG
    # streams alone are not reproducible; seeds = NULL (not the default NA)
    # makes caret draw a seed per resample from the master RNG.
    seeds = if (allow_parallel) NULL else NA
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
  # Permutation importance (type = 1: mean decrease in accuracy, or %IncMSE
  # for regression). Impurity importance (type = 2) is positive for every
  # predictor used in any split, noise included, so "positive" would keep
  # practically everything; permutation importance is centred near zero for
  # an uninformative predictor.
  imp <- as.data.frame(randomForest::importance(rf_fit, type = 1))
  imp$Feature <- rownames(imp)
  score_col <- utils::tail(names(imp)[vapply(imp, is.numeric, logical(1L))], 1L)
  ord <- order(imp[[score_col]], decreasing = TRUE, na.last = NA)

  # `min_keep` is a FLOOR, not a quota. With rfe() failed there is no
  # cross-validated size estimate, so reducing to a fixed count (formerly 1)
  # threw away usable predictors for no stated reason. Keep everything that
  # earned positive permutation importance, and only fall back to a fixed
  # count if that leaves too few. This is a weak screen: roughly half of
  # pure-noise predictors still clear zero.
  keep_n <- max(as.integer(min_keep), 1L)
  ranked <- imp$Feature[ord]
  ranked_imp <- imp[[score_col]][ord]
  selected <- ranked[is.finite(ranked_imp) & ranked_imp > 0]
  n_pos <- length(selected)
  if (length(selected) < keep_n) {
    selected <- utils::head(ranked, min(keep_n, length(ranked)))
  }
  if (length(selected) == 0L) {
    selected <- colnames(x_enc)[seq_len(min(keep_n, ncol(x_enc)))]
  }

  scores <- stats::setNames(as.numeric(imp[[score_col]]), imp$Feature)
  scores <- scores[feature_names]
  names(scores) <- feature_names

  warning(
    sprintf(
      paste0(
        "feature selection failed (%s); falling back to %d of %d ",
        "predictor(s): %s ",
        "(a weak screen, not a cross-validated selection)"
      ),
      reason, length(selected), length(feature_names),
      if (length(selected) > n_pos) {
        sprintf(paste0("the %d with positive random-forest permutation ",
                       "importance plus the next top-ranked to reach ",
                       "min_keep = %d"), n_pos, keep_n)
      } else {
        "those with positive random-forest permutation importance"
      }
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

#' Train and evaluate an SVM, with optional SVM-RFE feature selection
#'
#' Trains a support vector machine (SVM) classifier or regressor with caret's
#' \pkg{kernlab} engines, tunes it by cross-validation, and reports its
#' performance on held-out rows. With `feature_select = TRUE` it first
#' selects features on the training rows, by SVM-RFE (the default) or by
#' random-forest RFE. It can also up-sample minority classes and train on
#' several workers.
#'
#' @details
#' For classification, an SVM fits the boundary between classes that stays
#' furthest from the nearest training points while tolerating some points on
#' the wrong side. For regression, it fits a function that keeps most points
#' within a margin around it. The cost `C` sets how heavily points that break
#' these rules are penalized. The kernel sets the shape of the boundary or
#' function: a hyperplane for `"linear"`, curved surfaces for `"radial"` and
#' `"polynomial"`.
#'
#' Use `fs_svm()` when the selector and the final model should come from the
#' same family. SVM-RFE ranks features by the weights of a linear SVM, and
#' the result carries the tuned model and its held-out performance next to
#' the chosen features. The cost is a full SVM fit at every elimination step
#' plus a cross-validated size search, so on wide data screen first with a
#' filter such as [fs_supervised()].
#'
#' The honest performance estimate is `details$performance`. The
#' [Validation and leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.html)
#' article lists which numbers in each featR result are held out and which
#' are optimistic.
#'
#' ## Order of operations
#'
#' \enumerate{
#'   \item The target is coerced for `task` (see "Data requirements").
#'   \item The rows are split into training and test sets, stratified on the
#'     target, with `train_ratio` of them used for training. For
#'     classification, test rows whose class does not occur in the training
#'     rows are dropped with a warning.
#'   \item Factor and character predictors in the test rows are re-leveled to
#'     the levels observed in the training rows. Test rows with a level the
#'     training rows never saw are dropped with a warning.
#'   \item Predictors are dummy-encoded with full-rank dummies, fitted on the
#'     training rows.
#'   \item With `feature_select = TRUE`, the selector runs on the encoded
#'     training predictors, and the training and test matrices are cut down
#'     to the selected columns.
#'   \item The final SVM is fitted with `caret::train()`: predictors are
#'     centered and scaled, and `tune_grid` is searched by `nfolds`-fold
#'     cross-validation on the training rows.
#'   \item The tuned model predicts the test rows, which gives
#'     `details$performance`.
#' }
#'
#' ## SVM-RFE
#'
#' SVM-RFE (Guyon, Weston, Barnhill and Vapnik, 2002) fits a linear SVM on
#' the centered and scaled encoded training matrix and ranks the features by
#' their squared primal weight `w^2`. For a multi-class fit, `w^2` is summed
#' over the pairwise problems. The lowest-ranked feature is dropped (the
#' lowest 10 percent while more than 50 features remain), and the SVM is
#' refitted on the rest until one feature is left. Reversing the elimination order
#' gives the ranking, so rank 1 is the feature eliminated last. These fits
#' use a fixed cost of `C = 1` and are separate from the final model, which
#' is tuned over `tune_grid`.
#'
#' SVM-RFE requires `kernel = "linear"`, because the primal weight vector
#' only exists for a linear kernel. Combining it with another kernel is an
#' error that suggests `select_method = "rf_rfe"`.
#'
#' With `n_features` supplied, SVM-RFE keeps exactly that many features from
#' the top of the ranking. Otherwise it scores a short ladder of candidate
#' sizes: the powers of two up to the number of features, plus the full
#' size, trimmed to at most six entries but always including 1 and the full
#' size. Each size is scored by `nfolds`-fold cross-validation with the same
#' linear SVM, on folds shared by every size. Inside each fold, the
#' standardization and the whole elimination are re-run on that fold's
#' training rows, and the top features of that fold's ranking are scored on
#' its held-out rows, so no size is scored by a ranking that has seen the
#' rows it is scored on. The size with the highest mean accuracy
#' (classification) or the lowest mean RMSE (regression) wins, ties going to
#' the smaller size. The kept features are the top of the ranking computed
#' on the whole training split. If no size could be scored, all features are
#' kept with a warning.
#'
#' Each entry of `details$selection$size_scores` is an honest estimate for
#' its own size. The entry for the chosen size, though, is the best of up to
#' six noisy estimates and so is optimistic; on pure noise it typically sits
#' a few points above chance.
#'
#' ## Random-forest screening
#'
#' `select_method = "rf_rfe"` screens features with random-forest RFE
#' (`caret::rfFuncs`) and works with every kernel. It runs its own 10-fold
#' cross-validation over subset sizes 1 to `p`, independent of `nfolds`.
#' Encoded predictors that are numerically constant are removed first; their
#' score is `NA` and they are never selected.
#'
#' If `caret::rfe()` fails or selects nothing, a plain random forest is
#' fitted instead, with a warning. Every predictor with positive permutation
#' importance (mean decrease in accuracy, or \code{\%IncMSE} for
#' regression) is kept, or, if fewer than `n_features` qualify (1 when
#' `n_features = NULL`), the top `n_features` by that importance. This
#' fallback is a weak screen: roughly half of pure-noise predictors have
#' positive permutation importance. With `n_features` supplied, the
#' selection is then truncated to the `n_features` selected predictors with
#' the highest `scores`, so the fallback keeps exactly that many (capped at
#' the number available).
#'
#' ## Which numbers are optimistic
#'
#' Selection runs on the training split only, so the test rows never inform
#' which features survive. The final model's tuning cross-validation reuses
#' the training rows the selector has already seen, though. With
#' `feature_select = TRUE`, the resampled metrics in `model$results` and
#' `model$resample` (what printing `model` shows) are therefore biased
#' upward, and on pure noise they can approach perfect accuracy. Taking the
#' best point of `tune_grid` adds further optimism. Only
#' `details$performance`, computed on the untouched test split, is an honest
#' estimate.
#'
#' Class-imbalance handling (`class_imbalance = TRUE`, classification only)
#' up-samples within each cross-validation resample via
#' `caret::trainControl(sampling = "up")`, so duplicated rows do not leak
#' across folds.
#'
#' ## Reading the performance
#'
#' For regression, `details$performance` holds `RMSE`, `MAE` and `Rsquared`
#' from `caret::postResample()`. `Rsquared` is the squared Pearson
#' correlation between test predictions and observations, not
#' `1 - SSE/SST`. It is never negative and ignores bias and scale errors
#' (predictions of `2 * y` score 1), so it cannot be compared with an `R2`
#' defined as `1 - SSE/SST`, such as the one from [fs_randomforest()]. For
#' classification it is a `caret::confusionMatrix`, whose `overall` element
#' holds `Accuracy` and Cohen's `Kappa`. Test rows with a missing prediction
#' are dropped from the metrics with a warning.
#'
#' ## Data requirements
#'
#' \itemize{
#'   \item Missing values in the predictors or the target are an error, and
#'     so are infinite values.
#'   \item For classification, no class label may be the empty string. A
#'     numeric target is coerced to a factor (with a message) only when it
#'     has at most 10 unique values; more than 10 is an error suggesting
#'     `task = "regression"`. For regression the target must be numeric.
#'   \item A factor or character predictor with fewer than two distinct
#'     non-missing values is an error naming the column. So is one left with
#'     a single level in the training rows because its other levels fell
#'     only into the test rows.
#'   \item Unused factor levels are dropped before encoding, so they produce
#'     no all-zero dummy columns.
#'   \item If a dummy column name (`<factor><level>`) matches another column
#'     or the target, for example factor `g` with level `"1"` next to a
#'     numeric `g1`, the call stops and asks for a rename.
#' }
#'
#' ## Packages, seeds and parallel runs
#'
#' `fs_svm()` always needs the suggested packages \pkg{caret} and
#' \pkg{kernlab}. It also needs \pkg{e1071} for classification,
#' \pkg{randomForest} for `select_method = "rf_rfe"` with
#' `feature_select = TRUE`, and \pkg{doParallel} and \pkg{foreach} when
#' `n_cores > 1`.
#'
#' `seed` is applied for the duration of the call and the previous RNG state
#' is restored afterwards. When `n_cores > 1`, caret's resampling controls
#' get `seeds = NULL`, so caret draws one seed per resample from the locally
#' seeded RNG. That is what makes two seeded parallel runs agree. The
#' workers also get L'Ecuyer-CMRG streams, but those alone would not be
#' enough, because foreach hands each resample to whichever worker is free.
#' A seeded parallel run need not match a seeded sequential one. See
#' [Reproducibility and parallelism](https://elkronos.github.io/featR/articles/reproducibility.html).
#'
#' @param data A data frame (or data.table) containing the predictors and
#'   the target, with unique, non-empty column names.
#' @param target A string naming the target column.
#' @param task Either `"classification"` or `"regression"`. Required; there
#'   is no default, because guessing it from the target is exactly the
#'   mistake this argument exists to prevent.
#' @param train_ratio Training proportion of the stratified split, strictly
#'   between 0 and 1. Default `0.7`.
#' @param nfolds Number of cross-validation folds for tuning the final model,
#'   a whole number greater than 1. Default `5`. SVM-RFE also uses it for its
#'   size search (capped there at the number of training rows);
#'   `select_method = "rf_rfe"` ignores it and uses 10 folds.
#' @param kernel One of `"linear"` (default), `"radial"`, or `"polynomial"`.
#' @param tune_grid Optional data frame of tuning values for
#'   `caret::train()`. If `NULL`, the default grid for the kernel is used:
#'   `C = 2^(-5:10)` for `"linear"`; `sigma = 2^(-15:-5)` crossed with
#'   `C = 2^(-1:10)` for `"radial"`; `degree = 2:5`,
#'   `scale = c(0.1, 0.5, 1)` and `C = 2^(-3:8)` crossed for
#'   `"polynomial"`.
#' @param feature_select Logical; if `TRUE`, run feature selection on the
#'   dummy-encoded training predictors. Default `FALSE`.
#' @param select_method Which selector to run when `feature_select = TRUE`:
#'   `"svm_rfe"` (default; SVM-RFE, linear kernel only) or `"rf_rfe"`
#'   (random-forest screening, any kernel). Ignored when
#'   `feature_select = FALSE`, and so is the linear-kernel requirement, which
#'   is only enforced when SVM-RFE will run. An unrecognized value is always
#'   an error.
#' @param n_features Optional whole number of features to keep, capped at
#'   the number of encoded predictors. For `"svm_rfe"`, the top `n_features`
#'   ranked features are kept and the cross-validated size search is
#'   skipped. For `"rf_rfe"`, the selection is truncated to its `n_features`
#'   highest-scoring entries, and `n_features` is also the minimum the
#'   random-forest fallback keeps (see Details). Ignored when
#'   `feature_select = FALSE`. Default `NULL` (the size is chosen
#'   automatically).
#' @param class_imbalance Logical; if `TRUE` and the task is
#'   classification, up-sample classes within each cross-validation resample.
#'   Default `FALSE`.
#' @param seed Optional whole-number seed within the R integer range.
#'   Fractional values are an error, not truncated. Default `NULL`, which
#'   never seeds. See "Packages, seeds and parallel runs" in Details.
#' @param verbose Logical; if `TRUE`, report progress, including each
#'   SVM-RFE elimination step. Default `FALSE`.
#' @param n_cores Number of parallel workers. Default `1` (sequential).
#'   Requests are capped at the detected core count. Above 1, a cluster is
#'   created for the duration of the call and stopped on exit.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector of selected encoded feature names.
#'     With `feature_select = FALSE` this is every encoded predictor. With
#'     `"svm_rfe"` it is the surviving subset, from most to least important.
#'     With `"rf_rfe"` it is the subset in the order `caret::rfe()` reports
#'     it, or, when `n_features` truncated it, ordered by decreasing
#'     `scores`.}
#'   \item{scores}{Named numeric vector covering every encoded predictor, not
#'     only the survivors. For `"svm_rfe"` it is the squared primal weight
#'     `w^2` from the first, full-feature fit. For `"rf_rfe"` it is the mean
#'     random-forest importance recorded across resamples at the full subset
#'     size, `NA` for predictors `caret::rfe()` never scored; when the
#'     fallback ran, it is the permutation importance of the fallback
#'     forest. `NULL` when `feature_select = FALSE`, because no selector
#'     produced scores.}
#'   \item{method}{`"svm_"` followed by the kernel, for example
#'     `"svm_linear"`. The selector that ran, if any, is in
#'     `details$selection$method`.}
#'   \item{task}{`"classification"` or `"regression"`.}
#'   \item{model}{The fitted `caret::train` object. With
#'     `feature_select = TRUE` its resampled metrics are optimistic (see
#'     Details); use `details$performance`.}
#'   \item{details}{A list with:
#'     \itemize{
#'       \item `test_set`: the test split, with its coerced target and
#'         aligned factor levels.
#'       \item `predictions`: test-set predictions in the row order of
#'         `test_set`; a factor for classification, an unnamed numeric
#'         vector for regression.
#'       \item `performance`: a `caret::confusionMatrix` for
#'         classification, or a named `RMSE`/`Rsquared`/`MAE` vector for
#'         regression, where `Rsquared` is the squared correlation (see
#'         Details).
#'       \item `selection`: `NULL` when no selection ran, otherwise a list
#'         with `method`, `ranking` (most to least important), `scores`, and
#'         the size search's `sizes`, `size_scores` and `size_metric`. Those
#'         last three are `NULL`, `NULL` and `NA` whenever no size search
#'         ran, which is always the case for `"rf_rfe"` and for `"svm_rfe"`
#'         with an explicit `n_features`.
#'       \item `encoder`: the fitted `caret::dummyVars` object.
#'       \item `n_features`: the number of encoded predictors considered,
#'         counted before any were dropped.
#'     }}
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
  if (!is.null(seed)) {
    assert_count(seed, "seed", lower = -.Machine$integer.max)
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

  # A single-level factor/character predictor cannot be dummy-coded; without
  # this check it failed inside model.matrix() with "contrasts can be applied
  # only to factors with 2 or more levels".
  lasso_check_categorical_levels(
    data[, setdiff(names(data), target), drop = FALSE], "fs_svm"
  )

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
  # A factor can have two levels in 'data' but only one in the training rows
  # (the other level fell entirely into the held-out rows, which were dropped
  # above). Dummy coding then fails the same way, so say so here.
  one_level <- names(train_set)[vapply(names(train_set), function(nm) {
    nm != target && is.factor(train_set[[nm]]) &&
      length(unique(train_set[[nm]][!is.na(train_set[[nm]])])) < 2L
  }, logical(1L))]
  if (length(one_level) > 0L) {
    stop(sprintf(paste0(
      "fs_svm(): categorical predictor(s) with fewer than two levels in the ",
      "training rows: %s. The other level(s) occur only in held-out rows, so ",
      "the column cannot be dummy-coded; merge rare levels, remove the ",
      "column, or change 'train_ratio' or 'seed'."),
      paste0("'", utils::head(one_level, 5L), "'", collapse = ", ")),
      call. = FALSE)
  }

  # Optional parallel backend, created early so feature selection and
  # training can share it, and always cleaned up on exit.
  # local_parallel_cluster() schedules teardown on this frame before it
  # registers the backend, and restores the caller's previous backend.
  if (use_parallel) {
    local_parallel_cluster(n_cores, seed = seed)
  }

  # Dummy-encode predictors (fitted on the training split only).
  dv <- svm_dummy_encoder(train_set, target, full_rank = TRUE)
  train_x <- svm_encode(dv, train_set)
  test_x <- svm_encode(dv, test_set)
  # A dummy named <factor><level> can coincide with another column (factor g
  # with level 1 -> "g1" next to a numeric "g1"); name-based selection would
  # then silently keep the wrong copy.
  assert_unique_encoded_names(colnames(train_x), target, "fs_svm")
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
      scores <- rf_res$scores
      if (!is.null(n_features) && length(selected_features) > n_features) {
        # Truncate by the reported (full-size, once-per-resample) scores, not
        # by caret's optVariables order, which weights each resample by how
        # far the variable survived; otherwise a lower-scored feature could
        # be kept over a higher-scored one.
        sel_scores <- scores[selected_features]
        ord <- order(-sel_scores, seq_along(sel_scores), na.last = TRUE)
        selected_features <- utils::head(selected_features[ord], n_features)
      }
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
    sampling = sampling,
    # See svm_feature_selection(): per-resample seeds from the master RNG
    # make a seeded parallel run reproducible.
    seeds = if (use_parallel) NULL else NA
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
  if (task == "regression") {
    # kernlab names regression predictions by test row; return a plain
    # numeric aligned with details$test_set, like the other wrappers.
    preds <- unname(as.numeric(preds))
  }
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
