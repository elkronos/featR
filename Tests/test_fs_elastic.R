###############################################################################
# Testing Infrastructure - Elastic Net (for the refactored code)
###############################################################################

# Dependencies are assumed loaded by your main script:
# caret, glmnet, Matrix, doParallel, foreach, irlba

##########################################
# Minimal Test Harness Utilities
##########################################
test_results <- data.frame(
  Test   = character(),
  Result = character(),
  Note   = character(),
  stringsAsFactors = FALSE
)

print_and_store_result <- function(name, valid, note = "") {
  status <- if (isTRUE(valid)) "PASS" else "FAIL"
  cat(sprintf("[%s] %s%s\n",
              status,
              name,
              if (nzchar(note)) paste0(" — ", note) else ""))
  assign(
    "test_results",
    rbind(test_results, data.frame(Test = name, Result = status, Note = note, stringsAsFactors = FALSE)),
    envir = .GlobalEnv
  )
}

##########################################
# Unit Test: extract_variables
##########################################
#' Unit Test for extract_variables
#'
#' Ensures response and predictors are extracted with correct dimensions.
#' @return None.
test_extract_variables <- function() {
  set.seed(123)
  df <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50),
    x2 = rnorm(50)
  )
  vars <- extract_variables(df, y ~ x1 + x2)
  ok <- is.numeric(vars$y) &&
    is.matrix(vars$x) &&
    nrow(vars$x) == length(vars$y) &&
    ncol(vars$x) == 2
  print_and_store_result("extract_variables: basic extraction", ok)
}

##########################################
# Unit Test: handle_missing_values
##########################################
#' Unit Test for handle_missing_values
#'
#' Tests that missing values are removed in y and (sparse) x.
#' @return None.
test_handle_missing_values <- function() {
  set.seed(123)
  y <- rnorm(100)
  x <- Matrix::Matrix(rnorm(200), ncol = 2, sparse = TRUE)
  x[1, 1] <- NA
  x[5, 2] <- NA
  y[10]   <- NA
  
  result <- handle_missing_values(x, y)
  ok <- !anyNA(result$y) &&
    (inherits(result$x, "sparseMatrix") || is.matrix(result$x)) &&
    nrow(result$x) == length(result$y) &&
    !anyNA(result$x@x)
  print_and_store_result("handle_missing_values: NA removal (sparse-safe)", ok)
}

##########################################
# Unit Test: perform_pca
##########################################
#' Unit Test for perform_pca
#'
#' Ensures PCA runs and returns the requested number of components.
#' @return None.
test_perform_pca <- function() {
  set.seed(123)
  x <- Matrix::Matrix(rnorm(400), ncol = 4, sparse = TRUE)
  pcs <- 2L
  z <- perform_pca(x, use_pca = TRUE, nPCs = pcs)
  ok <- is.matrix(z) && ncol(z) == pcs && nrow(z) == nrow(x)
  print_and_store_result("perform_pca: correct dimensionality", ok)
}

##########################################
# Unit Test: train_models
##########################################
#' Unit Test for train_models
#'
#' Verifies caret training completes without error (glmnet).
#' @return None.
test_train_models <- function() {
  set.seed(123)
  y <- rnorm(120)
  x <- Matrix::Matrix(rnorm(240), ncol = 2, sparse = TRUE)
  colnames(x) <- c("predictor1", "predictor2")
  
  tuneGrid  <- expand.grid(alpha = c(0, 1), lambda = c(0.05, 0.5))
  trControl <- caret::trainControl(method = "cv", number = 3, summaryFunction = safe_summary)
  
  ok <- FALSE
  note <- ""
  tryCatch({
    fit <- train_models(x, y, tuneGrid, trControl, cores = 1) # keep tests light
    ok <- inherits(fit, "train") &&
      identical(fit$method, "glmnet") &&
      all(c("alpha", "lambda") %in% names(fit$bestTune))
  }, error = function(e) {
    note <<- e$message
  })
  print_and_store_result("train_models: caret+glmnet training", ok, note)
}

##########################################
# Unit Test: select_best_model
##########################################
#' Unit Test for select_best_model
#'
#' Uses a mocked caret-like object to ensure best params and RMSE extraction.
#' @return None.
test_select_best_model <- function() {
  # mock caret::train-like object
  fit_mock <- list(
    finalModel = list(dummy = TRUE),
    bestTune   = data.frame(alpha = 1, lambda = 0.1),
    results    = data.frame(RMSE = c(0.72, 0.51, 0.66))
  )
  class(fit_mock) <- "train"
  
  res <- select_best_model(fit_mock)
  ok <- is.list(res) &&
    isTRUE(res$model$dummy) &&
    identical(res$alpha, 1) &&
    identical(res$lambda, 0.1) &&
    isTRUE(all.equal(res$RMSE, 0.51))
  print_and_store_result("select_best_model: best extraction from train", ok)
}

##########################################
# Unit Test: fs_elastic (end-to-end)
##########################################
#' Unit Test for fs_elastic
#'
#' Trains an elastic net model end-to-end on synthetic data.
#' @return None.
test_fs_elastic <- function() {
  set.seed(123)
  n <- 150
  dat <- data.frame(
    response   = rnorm(n),
    predictor1 = rnorm(n),
    predictor2 = rnorm(n)
  )
  form <- response ~ predictor1 + predictor2
  
  ok <- FALSE
  note <- ""
  tryCatch({
    ans <- fs_elastic(
      data     = dat,
      formula  = form,
      alpha_seq  = c(0, 0.5, 1),
      lambda_seq = 10^seq(-2, 0, length = 5),
      use_pca    = FALSE,
      cores      = 1,
      verbose    = FALSE
    )
    ok <- is.list(ans) &&
      all(c("coef", "best_alpha", "best_lambda", "RMSE", "full_model") %in% names(ans)) &&
      inherits(ans$full_model, "train")
  }, error = function(e) {
    note <<- e$message
  })
  print_and_store_result("fs_elastic: end-to-end training", ok, note)
}

##########################################
# Run All Tests
##########################################
#' Run All Unit Tests
#'
#' Executes all defined unit tests and prints a summary.
#' @return None.
run_all_tests <- function() {
  cat("Running Comprehensive Unit Tests\n")
  cat("==================================\n")
  test_extract_variables()
  test_handle_missing_values()
  test_perform_pca()
  test_train_models()
  test_select_best_model()
  test_fs_elastic()
  cat("==================================\n")
  cat("Unit Testing completed\n\n")
  
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

##########################################
# Execute Unit Tests (Do Not Run by Default)
##########################################
## To run the tests immediately, uncomment:
## if (sys.nframe() == 0) {
##   run_all_tests()
## }
