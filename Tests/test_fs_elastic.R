###############################################################################
# Testing Infrastructure - Elastic Net (fs_elastic)
###############################################################################

# Optional: if fs_elastic and helpers are in a separate script/package, load here:
# source("fs_elastic.R")
# library(yourpackage)

###############################################################################
# Global container for test results
###############################################################################

test_results <- data.frame(
  Test   = character(),
  Result = character(),
  stringsAsFactors = FALSE
)

#' Helper: Print and Store Test Result
#'
#' @param test_name Character. Name of the test.
#' @param passed Logical. Whether the test passed.
#' @param note Optional character. Additional notes.
print_and_store_result <- function(test_name, passed, note = NULL) {
  result <- if (isTRUE(passed)) "PASS" else "FAIL"
  cat(sprintf("%-70s [%s]\n", test_name, result))
  if (!is.null(note) && nzchar(note)) cat("  Note: ", note, "\n")
  test_results <<- rbind(
    test_results,
    data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)
  )
}

###############################################################################
# Tests: Existence and Basic Helpers
###############################################################################

test_fs_elastic_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_elastic") && is.function(fs_elastic)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: function exists and is callable", passed, note)
}

test_fs_elastic_helpers_existence <- function() {
  helper_names <- c(
    "extract_variables",
    "handle_missing_values",
    "perform_pca",
    "train_models",
    "select_best_model",
    "safe_summary",
    "detect_cores_safe",
    "with_parallel",
    "verbose_message",
    "infer_task_and_coerce_response"
  )
  missing <- character(0)
  err <- NULL
  passed <- tryCatch({
    for (nm in helper_names) {
      if (!exists(nm) || !is.function(get(nm, mode = "function"))) {
        missing <- c(missing, nm)
      }
    }
    length(missing) == 0L
  }, error = function(e) {
    err <<- e
    FALSE
  })
  if (length(missing) > 0L) {
    err <- paste("Missing or non-function helpers:", paste(missing, collapse = ", "))
  }
  note <- if (!is.null(err)) conditionMessage(simpleError(err)) else NULL
  print_and_store_result("fs_elastic: core helper functions exist", passed, note)
}

###############################################################################
# Tests: safe_summary and detect_cores_safe
###############################################################################

test_safe_summary_regression_na_handling <- function() {
  set.seed(123)
  # All obs are NA → defaultSummary should produce NA/NaN metrics
  dat <- data.frame(
    obs  = rep(NA_real_, 20),
    pred = rnorm(20)
  )
  err <- NULL
  passed <- tryCatch({
    res <- safe_summary(dat)
    all(c("RMSE", "Rsquared") %in% names(res)) &&
      is.infinite(res[["RMSE"]]) && res[["RMSE"]] > 0 &&
      is.infinite(res[["Rsquared"]]) && res[["Rsquared"]] < 0
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("safe_summary: regression NA metrics replaced with +/-Inf", passed, note)
}

test_safe_summary_classification_na_handling <- function() {
  set.seed(456)
  # Force classification metrics to be NA by having obs all NA
  obs <- factor(rep(NA, 30), levels = c("A", "B"))
  pred <- factor(sample(c("A", "B"), 30, TRUE), levels = c("A", "B"))
  dat <- data.frame(obs = obs, pred = pred)
  
  err <- NULL
  passed <- tryCatch({
    res <- safe_summary(dat)
    all(c("Accuracy", "Kappa") %in% names(res)) &&
      is.infinite(res[["Accuracy"]]) && res[["Accuracy"]] < 0 &&
      is.infinite(res[["Kappa"]]) && res[["Kappa"]] < 0
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("safe_summary: classification NA metrics replaced with -Inf", passed, note)
}

test_detect_cores_safe_minimum <- function() {
  err <- NULL
  passed <- tryCatch({
    cores <- detect_cores_safe()
    length(cores) == 1L && is.numeric(cores) && cores >= 1
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("detect_cores_safe: returns >= 1 core", passed, note)
}

###############################################################################
# Tests: extract_variables
###############################################################################

test_extract_variables_basic <- function() {
  set.seed(123)
  df <- data.frame(
    y  = rnorm(50),
    x1 = rnorm(50),
    x2 = rnorm(50)
  )
  err <- NULL
  passed <- tryCatch({
    vars <- extract_variables(df, y ~ x1 + x2)
    is.numeric(vars$y) &&
      is.matrix(vars$x) &&
      nrow(vars$x) == length(vars$y) &&
      ncol(vars$x) == 2L &&
      all(colnames(vars$x) %in% c("x1", "x2"))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("extract_variables: basic extraction with intercept", passed, note)
}

test_extract_variables_no_intercept <- function() {
  set.seed(123)
  df <- data.frame(
    y  = rnorm(40),
    x1 = rnorm(40),
    x2 = rnorm(40)
  )
  err <- NULL
  passed <- tryCatch({
    vars <- extract_variables(df, y ~ 0 + x1 + x2)
    is.numeric(vars$y) &&
      is.matrix(vars$x) &&
      nrow(vars$x) == length(vars$y) &&
      ncol(vars$x) == 2L &&
      identical(sort(colnames(vars$x)), sort(c("x1", "x2")))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("extract_variables: no-intercept formula retains all predictors", passed, note)
}

###############################################################################
# Tests: handle_missing_values
###############################################################################

test_handle_missing_values_dense <- function() {
  set.seed(123)
  y <- rnorm(10)
  x <- matrix(rnorm(30), ncol = 3)
  x[1, 1] <- NA
  x[5, 3] <- NA
  y[2]    <- NA
  
  err <- NULL
  passed <- tryCatch({
    res <- handle_missing_values(x, y)
    !anyNA(res$y) &&
      is.matrix(res$x) &&
      nrow(res$x) == length(res$y) &&
      !anyNA(res$x)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("handle_missing_values: NA removal (dense matrix)", passed, note)
}

test_handle_missing_values_sparse <- function() {
  set.seed(123)
  y <- rnorm(100)
  x <- Matrix::Matrix(rnorm(200), ncol = 2, sparse = TRUE)
  x[1, 1] <- NA
  x[5, 2] <- NA
  y[10]   <- NA
  
  err <- NULL
  passed <- tryCatch({
    res <- handle_missing_values(x, y)
    !anyNA(res$y) &&
      (inherits(res$x, "sparseMatrix") || is.matrix(res$x)) &&
      nrow(res$x) == length(res$y) &&
      (!inherits(res$x, "sparseMatrix") || !anyNA(res$x@x))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("handle_missing_values: NA removal (sparse-safe)", passed, note)
}

test_handle_missing_values_all_rows_removed <- function() {
  y <- c(NA_real_, NA_real_)
  x <- matrix(NA_real_, nrow = 2, ncol = 2)
  err <- NULL
  passed <- tryCatch({
    handle_missing_values(x, y)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("All rows were removed", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("handle_missing_values: errors if all rows removed", passed, note)
}

###############################################################################
# Tests: perform_pca
###############################################################################

test_perform_pca_no_pca_passthrough <- function() {
  set.seed(123)
  x <- matrix(rnorm(40), ncol = 4)
  err <- NULL
  passed <- tryCatch({
    out <- perform_pca(x, use_pca = FALSE, nPCs = NULL)
    identical(dim(out$x), dim(x)) &&
      isTRUE(all.equal(out$x, x)) &&
      is.null(out$pca)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("perform_pca: no-pca path returns original x and NULL pca", passed, note)
}

test_perform_pca_basic <- function() {
  set.seed(123)
  x <- Matrix::Matrix(rnorm(400), ncol = 4, sparse = TRUE)
  pcs <- 2L
  err <- NULL
  passed <- tryCatch({
    out <- perform_pca(x, use_pca = TRUE, nPCs = pcs)
    is.matrix(out$x) &&
      ncol(out$x) == pcs &&
      nrow(out$x) == nrow(x) &&
      !is.null(out$pca) &&
      inherits(out$pca, "prcomp")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("perform_pca: correct dimensionality and pca object", passed, note)
}

test_perform_pca_invalid_npcs_null <- function() {
  set.seed(123)
  x <- Matrix::Matrix(rnorm(100), ncol = 5, sparse = TRUE)
  err <- NULL
  passed <- tryCatch({
    perform_pca(x, use_pca = TRUE, nPCs = NULL)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Please set a positive nPCs", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("perform_pca: errors when nPCs is NULL and use_pca=TRUE", passed, note)
}

test_perform_pca_invalid_npcs_too_large <- function() {
  set.seed(123)
  x <- Matrix::Matrix(rnorm(60), ncol = 3, sparse = TRUE)
  err <- NULL
  passed <- tryCatch({
    perform_pca(x, use_pca = TRUE, nPCs = 10L)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("nPCs must be <= min", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("perform_pca: errors when nPCs > min(dim(x))", passed, note)
}

###############################################################################
# Tests: infer_task_and_coerce_response
###############################################################################

test_infer_task_regression_numeric <- function() {
  y <- rnorm(10)
  err <- NULL
  passed <- tryCatch({
    info <- infer_task_and_coerce_response(y)
    is.numeric(info$y) &&
      identical(info$task, "regression") &&
      identical(info$metric, "RMSE")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("infer_task_and_coerce_response: numeric -> regression", passed, note)
}

test_infer_task_classification_factor <- function() {
  y <- factor(c("A", "B", "A", "B"))
  err <- NULL
  passed <- tryCatch({
    info <- infer_task_and_coerce_response(y)
    is.factor(info$y) &&
      identical(info$task, "classification") &&
      identical(info$metric, "Accuracy")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("infer_task_and_coerce_response: factor -> classification", passed, note)
}

test_infer_task_classification_character <- function() {
  y <- c("A", "B", "A", "B")
  err <- NULL
  passed <- tryCatch({
    info <- infer_task_and_coerce_response(y)
    is.factor(info$y) &&
      identical(info$task, "classification") &&
      identical(info$metric, "Accuracy")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("infer_task_and_coerce_response: character -> classification", passed, note)
}

test_infer_task_unsupported_type <- function() {
  y <- as.Date("2020-01-01") + 0:3
  err <- NULL
  passed <- tryCatch({
    infer_task_and_coerce_response(y)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Unsupported response type", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("infer_task_and_coerce_response: unsupported type errors clearly", passed, note)
}

###############################################################################
# Tests: train_models and select_best_model
###############################################################################

test_train_models_glmnet_regression <- function() {
  set.seed(123)
  y <- rnorm(80)
  x <- Matrix::Matrix(rnorm(160), ncol = 2, sparse = TRUE)
  colnames(x) <- c("p1", "p2")
  
  tuneGrid  <- expand.grid(alpha = c(0, 1), lambda = c(0.01, 0.1))
  trControl <- caret::trainControl(
    method          = "cv",
    number          = 3,
    summaryFunction = safe_summary
  )
  
  err <- NULL
  passed <- tryCatch({
    fit <- train_models(x, y, tuneGrid, trControl, metric = "RMSE", cores = 1)
    inherits(fit, "train") &&
      identical(fit$method, "glmnet") &&
      all(c("alpha", "lambda") %in% names(fit$bestTune))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("train_models: regression (glmnet) training completes", passed, note)
}

test_train_models_glmnet_classification <- function() {
  set.seed(456)
  n <- 100
  x <- Matrix::Matrix(rnorm(2 * n), ncol = 2, sparse = TRUE)
  colnames(x) <- c("p1", "p2")
  lin <- x[, 1] - 0.5 * x[, 2]
  prob <- 1 / (1 + exp(-lin))
  y <- factor(ifelse(runif(n) < prob, "Yes", "No"))
  
  tuneGrid  <- expand.grid(alpha = c(0, 1), lambda = c(0.01, 0.1))
  trControl <- caret::trainControl(
    method          = "cv",
    number          = 3,
    summaryFunction = safe_summary
  )
  
  err <- NULL
  passed <- tryCatch({
    fit <- train_models(x, y, tuneGrid, trControl, metric = "Accuracy", cores = 1)
    inherits(fit, "train") &&
      identical(fit$method, "glmnet") &&
      all(c("alpha", "lambda") %in% names(fit$bestTune))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("train_models: classification (glmnet) training completes", passed, note)
}

test_select_best_model_regression <- function() {
  fit_mock <- list(
    finalModel = list(dummy = TRUE),
    bestTune   = data.frame(alpha = 1, lambda = 0.1),
    results    = data.frame(
      alpha = c(1, 1, 0.5),
      lambda = c(0.1, 0.2, 0.1),
      RMSE  = c(0.72, 0.51, 0.66)
    )
  )
  class(fit_mock) <- "train"
  
  err <- NULL
  passed <- tryCatch({
    res <- select_best_model(fit_mock, metric = "RMSE")
    is.list(res) &&
      isTRUE(res$model$dummy) &&
      identical(res$alpha, 1) &&
      identical(res$lambda, 0.1) &&
      identical(res$metric_name, "RMSE") &&
      isTRUE(all.equal(res$metric_value, 0.72))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("select_best_model: regression metric extraction", passed, note)
}

test_select_best_model_classification <- function() {
  fit_mock <- list(
    finalModel = list(dummy = TRUE),
    bestTune   = data.frame(alpha = 0.5, lambda = 0.2),
    results    = data.frame(
      alpha    = c(0.5, 0.5, 1.0),
      lambda   = c(0.2, 0.4, 0.2),
      Accuracy = c(0.70, 0.60, 0.65)
    )
  )
  class(fit_mock) <- "train"
  
  err <- NULL
  passed <- tryCatch({
    res <- select_best_model(fit_mock, metric = "Accuracy")
    is.list(res) &&
      isTRUE(res$model$dummy) &&
      identical(res$alpha, 0.5) &&
      identical(res$lambda, 0.2) &&
      identical(res$metric_name, "Accuracy") &&
      isTRUE(all.equal(res$metric_value, 0.70))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("select_best_model: classification metric extraction", passed, note)
}

###############################################################################
# Tests: fs_elastic - Input Validation
###############################################################################

test_fs_elastic_data_not_dataframe <- function() {
  err <- NULL
  passed <- tryCatch({
    fs_elastic(
      data    = "not a data frame",
      formula = y ~ x1,
      alpha_seq  = c(0, 1),
      lambda_seq = c(0.01, 0.1),
      cores      = 1,
      verbose    = FALSE
    )
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("data.frame", conditionMessage(e), ignore.case = TRUE) || TRUE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: errors when data is not a data.frame", passed, note)
}

test_fs_elastic_unsupported_response_type <- function() {
  set.seed(999)
  df <- data.frame(
    y  = as.Date("2020-01-01") + 0:9,
    x1 = rnorm(10),
    x2 = rnorm(10)
  )
  err <- NULL
  passed <- tryCatch({
    fs_elastic(
      data    = df,
      formula = y ~ x1 + x2,
      alpha_seq  = c(0, 1),
      lambda_seq = c(0.01, 0.1),
      cores      = 1,
      verbose    = FALSE
    )
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Unsupported response type", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: unsupported response type errors clearly", passed, note)
}

test_fs_elastic_pca_requires_npcs <- function() {
  set.seed(123)
  df <- data.frame(
    y  = rnorm(20),
    x1 = rnorm(20),
    x2 = rnorm(20)
  )
  err <- NULL
  passed <- tryCatch({
    fs_elastic(
      data    = df,
      formula = y ~ x1 + x2,
      alpha_seq  = c(0, 1),
      lambda_seq = c(0.01, 0.1),
      use_pca    = TRUE,
      nPCs       = NULL,
      cores      = 1,
      verbose    = FALSE
    )
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Please set a positive nPCs", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: errors when use_pca=TRUE and nPCs is NULL", passed, note)
}

###############################################################################
# Tests: fs_elastic - End-to-end Regression (no PCA / with PCA)
###############################################################################

test_fs_elastic_end_to_end_regression_no_pca <- function() {
  set.seed(123)
  n <- 120
  dat <- data.frame(
    response   = rnorm(n),
    predictor1 = rnorm(n),
    predictor2 = rnorm(n)
  )
  form <- response ~ predictor1 + predictor2
  
  err <- NULL
  passed <- tryCatch({
    ans <- fs_elastic(
      data      = dat,
      formula   = form,
      alpha_seq = c(0, 0.5, 1),
      lambda_seq = 10^seq(-2, 0, length = 4),
      use_pca    = FALSE,
      cores      = 1,
      verbose    = FALSE
    )
    all(c("coef", "best_alpha", "best_lambda", "metric_name", "metric_value",
          "task", "full_model", "pca_model", "use_pca", "formula") %in% names(ans)) &&
      inherits(ans$full_model, "train") &&
      identical(ans$task, "regression") &&
      identical(ans$metric_name, "RMSE") &&
      is.numeric(ans$metric_value) &&
      isTRUE(ans$use_pca == FALSE) &&
      is.null(ans$pca_model) &&
      identical(ans$formula, form) &&
      inherits(ans$coef, "dgCMatrix") &&
      length(ans$best_alpha) == 1L &&
      length(ans$best_lambda) == 1L
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: regression end-to-end training without PCA", passed, note)
}

test_fs_elastic_end_to_end_regression_with_pca <- function() {
  set.seed(123)
  n <- 100
  dat <- data.frame(
    response   = rnorm(n),
    predictor1 = rnorm(n),
    predictor2 = rnorm(n),
    predictor3 = rnorm(n)
  )
  form <- response ~ predictor1 + predictor2 + predictor3
  
  err <- NULL
  passed <- tryCatch({
    ans <- fs_elastic(
      data      = dat,
      formula   = form,
      alpha_seq = c(0, 1),
      lambda_seq = c(0.01, 0.1),
      use_pca    = TRUE,
      nPCs       = 2L,
      cores      = 1,
      verbose    = FALSE
    )
    all(c("coef", "best_alpha", "best_lambda", "metric_name", "metric_value",
          "task", "full_model", "pca_model", "use_pca", "formula") %in% names(ans)) &&
      inherits(ans$full_model, "train") &&
      identical(ans$task, "regression") &&
      identical(ans$metric_name, "RMSE") &&
      is.numeric(ans$metric_value) &&
      isTRUE(ans$use_pca == TRUE) &&
      !is.null(ans$pca_model) &&
      inherits(ans$pca_model, "prcomp") &&
      identical(ans$formula, form) &&
      inherits(ans$coef, "dgCMatrix")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: regression end-to-end training with PCA", passed, note)
}

test_fs_elastic_prediction_regression_no_pca <- function() {
  set.seed(321)
  n <- 80
  dat <- data.frame(
    response   = rnorm(n),
    predictor1 = rnorm(n),
    predictor2 = rnorm(n)
  )
  form <- response ~ predictor1 + predictor2
  
  err <- NULL
  passed <- tryCatch({
    ans <- fs_elastic(
      data      = dat,
      formula   = form,
      alpha_seq = c(0, 1),
      lambda_seq = c(0.01, 0.1),
      use_pca    = FALSE,
      cores      = 1,
      verbose    = FALSE
    )
    x_new <- as.matrix(dat[, c("predictor1", "predictor2")])
    preds <- predict(ans$full_model, newdata = x_new)
    length(preds) == n && is.numeric(preds)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: regression prediction works on training data (no PCA)", passed, note)
}

###############################################################################
# Tests: fs_elastic - End-to-end Classification (binary / multiclass)
###############################################################################

test_fs_elastic_end_to_end_classification_binary_no_pca <- function() {
  set.seed(101)
  n <- 150
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  lin <- x1 - 0.5 * x2
  prob <- 1 / (1 + exp(-lin))
  y <- factor(ifelse(runif(n) < prob, "Yes", "No"))
  dat <- data.frame(
    response   = y,
    predictor1 = x1,
    predictor2 = x2
  )
  form <- response ~ predictor1 + predictor2
  
  err <- NULL
  passed <- tryCatch({
    ans <- fs_elastic(
      data      = dat,
      formula   = form,
      alpha_seq = c(0, 1),
      lambda_seq = c(0.01, 0.1),
      use_pca    = FALSE,
      cores      = 1,
      verbose    = FALSE
    )
    all(c("coef", "best_alpha", "best_lambda", "metric_name", "metric_value",
          "task", "full_model", "pca_model", "use_pca", "formula") %in% names(ans)) &&
      inherits(ans$full_model, "train") &&
      identical(ans$task, "classification") &&
      identical(ans$metric_name, "Accuracy") &&
      is.numeric(ans$metric_value) &&
      isTRUE(ans$use_pca == FALSE) &&
      is.null(ans$pca_model) &&
      identical(ans$formula, form) &&
      !is.null(ans$coef)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: binary classification end-to-end (no PCA)", passed, note)
}

test_fs_elastic_end_to_end_classification_multiclass_with_pca <- function() {
  set.seed(202)
  n <- 180
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  z <- x1 + 0.5 * x2 - 0.3 * x3
  y <- cut(
    z,
    breaks = quantile(z, probs = c(0, 1/3, 2/3, 1)),
    include.lowest = TRUE,
    labels = c("C1", "C2", "C3")
  )
  dat <- data.frame(
    response   = factor(y),
    predictor1 = x1,
    predictor2 = x2,
    predictor3 = x3
  )
  form <- response ~ predictor1 + predictor2 + predictor3
  
  err <- NULL
  passed <- tryCatch({
    ans <- fs_elastic(
      data      = dat,
      formula   = form,
      alpha_seq = c(0, 1),
      lambda_seq = c(0.01, 0.1),
      use_pca    = TRUE,
      nPCs       = 2L,
      cores      = 1,
      verbose    = FALSE
    )
    all(c("coef", "best_alpha", "best_lambda", "metric_name", "metric_value",
          "task", "full_model", "pca_model", "use_pca", "formula") %in% names(ans)) &&
      inherits(ans$full_model, "train") &&
      identical(ans$task, "classification") &&
      identical(ans$metric_name, "Accuracy") &&
      is.numeric(ans$metric_value) &&
      isTRUE(ans$use_pca == TRUE) &&
      !is.null(ans$pca_model) &&
      inherits(ans$pca_model, "prcomp") &&
      identical(ans$formula, form) &&
      !is.null(ans$coef)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: multiclass classification end-to-end (with PCA)", passed, note)
}

test_fs_elastic_character_response_classification <- function() {
  set.seed(303)
  n <- 60
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  lin <- x1 + x2
  prob <- 1 / (1 + exp(-lin))
  y_char <- ifelse(runif(n) < prob, "Pos", "Neg")
  dat <- data.frame(
    response   = y_char,
    predictor1 = x1,
    predictor2 = x2,
    stringsAsFactors = FALSE
  )
  form <- response ~ predictor1 + predictor2
  
  err <- NULL
  passed <- tryCatch({
    ans <- fs_elastic(
      data      = dat,
      formula   = form,
      alpha_seq = c(0, 1),
      lambda_seq = c(0.01, 0.1),
      use_pca    = FALSE,
      cores      = 1,
      verbose    = FALSE
    )
    identical(ans$task, "classification") &&
      identical(ans$metric_name, "Accuracy") &&
      is.numeric(ans$metric_value)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: character response coerced to classification", passed, note)
}

test_fs_elastic_prediction_classification_no_pca <- function() {
  set.seed(404)
  n <- 90
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  lin <- x1 - x2
  prob <- 1 / (1 + exp(-lin))
  y <- factor(ifelse(runif(n) < prob, "One", "Zero"))
  dat <- data.frame(
    response   = y,
    predictor1 = x1,
    predictor2 = x2
  )
  form <- response ~ predictor1 + predictor2
  
  err <- NULL
  passed <- tryCatch({
    ans <- fs_elastic(
      data      = dat,
      formula   = form,
      alpha_seq = c(0, 1),
      lambda_seq = c(0.01, 0.1),
      use_pca    = FALSE,
      cores      = 1,
      verbose    = FALSE
    )
    x_new <- as.matrix(dat[, c("predictor1", "predictor2")])
    preds <- predict(ans$full_model, newdata = x_new)
    length(preds) == n &&
      is.factor(preds) &&
      all(levels(preds) %in% levels(y))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_elastic: classification prediction works on training data (no PCA)", passed, note)
}

###############################################################################
# Run All fs_elastic Tests
###############################################################################

run_fs_elastic_tests <- function() {
  cat("========== Running fs_elastic Tests ==========\n")
  # Existence / helpers
  test_fs_elastic_existence()
  test_fs_elastic_helpers_existence()
  
  # Core helpers: summary and cores
  test_safe_summary_regression_na_handling()
  test_safe_summary_classification_na_handling()
  test_detect_cores_safe_minimum()
  
  # extract_variables
  test_extract_variables_basic()
  test_extract_variables_no_intercept()
  
  # handle_missing_values
  test_handle_missing_values_dense()
  test_handle_missing_values_sparse()
  test_handle_missing_values_all_rows_removed()
  
  # perform_pca
  test_perform_pca_no_pca_passthrough()
  test_perform_pca_basic()
  test_perform_pca_invalid_npcs_null()
  test_perform_pca_invalid_npcs_too_large()
  
  # infer task
  test_infer_task_regression_numeric()
  test_infer_task_classification_factor()
  test_infer_task_classification_character()
  test_infer_task_unsupported_type()
  
  # train_models / select_best_model
  test_train_models_glmnet_regression()
  test_train_models_glmnet_classification()
  test_select_best_model_regression()
  test_select_best_model_classification()
  
  # fs_elastic input validation
  test_fs_elastic_data_not_dataframe()
  test_fs_elastic_unsupported_response_type()
  test_fs_elastic_pca_requires_npcs()
  
  # fs_elastic regression end-to-end
  test_fs_elastic_end_to_end_regression_no_pca()
  test_fs_elastic_end_to_end_regression_with_pca()
  test_fs_elastic_prediction_regression_no_pca()
  
  # fs_elastic classification end-to-end
  test_fs_elastic_end_to_end_classification_binary_no_pca()
  test_fs_elastic_end_to_end_classification_multiclass_with_pca()
  test_fs_elastic_character_response_classification()
  test_fs_elastic_prediction_classification_no_pca()
  
  cat("========== fs_elastic Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment the following line to run all tests when this script is executed:
# run_fs_elastic_tests()
