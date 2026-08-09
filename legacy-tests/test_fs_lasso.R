###############################################################################
# Testing Infrastructure - Lasso (fs_lasso)
###############################################################################

# Optional: if fs_lasso is in a separate script/package, load it here, e.g.:
# source("fs_lasso.R")
# library(yourpackage)

# Global container for test results
test_results <- data.frame(
  Test   = character(),
  Result = character(),
  stringsAsFactors = FALSE
)

#' Helper: Print and Store Test Result
#'
#' @param test_name Character. Name of the test.
#' @param passed Logical. Whether the test passed.
#' @param note Optional character. Additional notes (e.g., error message).
print_and_store_result <- function(test_name, passed, note = NULL) {
  result <- if (isTRUE(passed)) "PASS" else "FAIL"
  cat(sprintf("%-80s [%s]\n", test_name, result))
  if (!is.null(note)) cat("  Note: ", note, "\n")
  test_results <<- rbind(
    test_results,
    data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)
  )
}

###############################################################################
# Tests: Existence and Basic Behavior
###############################################################################

test_fs_lasso_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_lasso") && is.function(fs_lasso)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_lasso: function exists and is callable", passed, note)
}

test_fs_lasso_basic_matrix <- function() {
  set.seed(123)
  n <- 160
  p <- 8
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(Xm) <- paste0("X", seq_len(p))
  # true model: X1, X2, X3 active
  y  <- 2 * Xm[, 1] - 3 * Xm[, 2] + 1.5 * Xm[, 3] + rnorm(n, sd = 0.8)
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_lasso(x = Xm, y = y, verbose = FALSE, seed = 42, parallel = FALSE)
    is.list(res) &&
      all(c("importance", "lambda_min", "lambda_1se") %in% names(res)) &&
      is.data.frame(res$importance) &&
      nrow(res$importance) == ncol(Xm) &&
      identical(colnames(res$importance),
                c("Variable", "Coefficient", "AbsCoefficient")) &&
      # importance sorted by |coef| desc
      all(diff(res$importance$AbsCoefficient) <= 0) &&
      # top features should include X1, X2, X3
      all(c("X1", "X2", "X3") %in% head(res$importance$Variable, 5L))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: basic functionality with matrix input (structure, sorting, top signals)",
    passed, note
  )
}

test_fs_lasso_basic_dataframe <- function() {
  set.seed(123)
  n <- 160
  p <- 8
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(Xm) <- paste0("X", seq_len(p))
  y  <- 2 * Xm[, 1] - 3 * Xm[, 2] + 1.5 * Xm[, 3] + rnorm(n, sd = 0.8)
  
  Xdf <- as.data.frame(Xm)
  Xdf$cat <- factor(sample(letters[1:3], n, TRUE))
  Xdf$grp <- sample(c("A", "B"), n, TRUE)
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_lasso(x = Xdf, y = y, verbose = FALSE, seed = 42, parallel = FALSE)
    is.list(res) &&
      is.data.frame(res$importance) &&
      nrow(res$importance) >= ncol(Xm) &&
      all(c("Variable", "Coefficient", "AbsCoefficient") %in% colnames(res$importance))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: handles data.frame input with factor/character columns via model.matrix",
    passed, note
  )
}

test_fs_lasso_return_model <- function() {
  set.seed(123)
  n <- 80
  p <- 5
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y  <- Xm[, 1] - Xm[, 2] + rnorm(n)
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_lasso(x = Xm, y = y, return_model = TRUE, seed = 202, parallel = FALSE)
    is.list(res) &&
      "model" %in% names(res) &&
      inherits(res$model, "cv.glmnet") &&
      is.numeric(res$lambda_min) &&
      is.numeric(res$lambda_1se) &&
      length(res$lambda_min) == 1L &&
      length(res$lambda_1se) == 1L
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: returns cv.glmnet model object with lambda_min and lambda_1se",
    passed, note
  )
}

###############################################################################
# Tests: Reproducibility and Imputation
###############################################################################

test_fs_lasso_seed_reproducibility <- function() {
  set.seed(123)
  n <- 120
  p <- 6
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y  <- 1.5 * Xm[, 1] - 0.5 * Xm[, 2] + rnorm(n)
  
  err <- NULL
  passed <- tryCatch({
    r1 <- fs_lasso(x = Xm, y = y, seed = 999, parallel = FALSE)
    r2 <- fs_lasso(x = Xm, y = y, seed = 999, parallel = FALSE)
    same_lambda <- isTRUE(all.equal(r1$lambda_min, r2$lambda_min, tolerance = 1e-12))
    same_order  <- identical(r1$importance$Variable, r2$importance$Variable)
    same_lambda && same_order
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else "Sequential path used for reproducibility."
  print_and_store_result(
    "fs_lasso: deterministic lambda_min and importance ordering given same seed (sequential)",
    passed, note
  )
}

test_fs_lasso_missing_values_imputation <- function() {
  set.seed(321)
  n <- 150
  p <- 7
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(Xm) <- paste0("X", seq_len(p))
  y  <- Xm[, 1] - 2 * Xm[, 2] + rnorm(n)
  
  # Inject NAs into X
  idx <- sample(length(Xm), size = floor(0.05 * length(Xm)))
  Xm[idx] <- NA_real_
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_lasso(x = Xm, y = y, seed = 7, parallel = FALSE)
    is.list(res) &&
      is.data.frame(res$importance) &&
      nrow(res$importance) == ncol(Xm) &&
      all(is.finite(res$importance$Coefficient))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: fits when predictors contain missing values (mean imputation)",
    passed, note
  )
}

test_fs_lasso_constant_and_all_na_predictors <- function() {
  set.seed(88)
  n <- 120
  p <- 5
  
  Xm_core <- matrix(rnorm(n * (p - 2L)), nrow = n, ncol = p - 2L)
  colnames(Xm_core) <- paste0("X", seq_len(p - 2L))
  const_col <- rep(1, n)
  all_na_col <- rep(NA_real_, n)
  
  Xm <- cbind(Xm_core, const = const_col, all_na = all_na_col)
  y  <- Xm_core[, 1] + rnorm(n)
  
  err <- NULL
  passed <- tryCatch({
    res  <- fs_lasso(x = Xm, y = y, seed = 4, parallel = FALSE)
    vars <- res$importance$Variable
    all(c("const", "all_na") %in% vars)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: handles constant and all-NA predictors via imputation",
    passed, note
  )
}

###############################################################################
# Tests: Custom Folds - Valid and Invalid
###############################################################################

test_fs_lasso_custom_folds_valid <- function() {
  set.seed(1)
  n <- 160
  p <- 8
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y  <- 2 * Xm[, 1] - 3 * Xm[, 2] + rnorm(n)
  
  kfold  <- 5L
  foldid <- sample(seq_len(kfold), n, replace = TRUE)
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_lasso(
      x = Xm, y = y,
      custom_folds = foldid,
      nfolds = kfold,
      seed = 11,
      parallel = FALSE
    )
    is.list(res) &&
      is.data.frame(res$importance) &&
      nrow(res$importance) == ncol(Xm)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: accepts custom fold IDs and fits model",
    passed, note
  )
}

test_fs_lasso_custom_folds_validation <- function() {
  set.seed(200)
  n <- 80
  p <- 5
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y  <- rnorm(n)
  
  # Case 1: custom_folds length mismatch
  err1 <- NULL
  cf1 <- rep(1L, n - 1L)
  passed1 <- tryCatch({
    fs_lasso(Xm, y, custom_folds = cf1, nfolds = 3)
    FALSE
  }, error = function(e) {
    err1 <<- e
    grepl("custom_folds' must be the same length as 'y'", conditionMessage(e), fixed = TRUE)
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_lasso: errors if custom_folds length does not match y",
    passed1, note1
  )
  
  # Case 2: non-integer custom_folds
  err2 <- NULL
  cf2 <- rep(1.5, n)
  passed2 <- tryCatch({
    fs_lasso(Xm, y, custom_folds = cf2, nfolds = 3)
    FALSE
  }, error = function(e) {
    err2 <<- e
    grepl("custom_folds' must be an integer vector", conditionMessage(e), fixed = TRUE)
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_lasso: errors if custom_folds is not an integer vector",
    passed2, note2
  )
  
  # Case 3: custom_folds < 1
  err3 <- NULL
  cf3 <- rep(0L, n)
  passed3 <- tryCatch({
    fs_lasso(Xm, y, custom_folds = cf3, nfolds = 3)
    FALSE
  }, error = function(e) {
    err3 <<- e
    grepl("custom_folds' contains invalid IDs (must be >= 1)", conditionMessage(e), fixed = TRUE)
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "fs_lasso: errors if custom_folds contains IDs < 1",
    passed3, note3
  )
  
  # Case 4: too many unique folds vs nfolds
  err4 <- NULL
  cf4 <- rep(1:4, length.out = n)
  passed4 <- tryCatch({
    fs_lasso(Xm, y, custom_folds = cf4, nfolds = 3)
    FALSE
  }, error = function(e) {
    err4 <<- e
    grepl("custom_folds' defines more unique folds than 'nfolds'", conditionMessage(e), fixed = TRUE)
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "fs_lasso: errors if custom_folds defines more unique folds than nfolds",
    passed4, note4
  )
  
  # Case 5: fold IDs greater than nfolds
  err5 <- NULL
  cf5 <- rep(c(1L, 4L), length.out = n)
  passed5 <- tryCatch({
    fs_lasso(Xm, y, custom_folds = cf5, nfolds = 3)
    FALSE
  }, error = function(e) {
    err5 <<- e
    grepl("custom_folds' contains fold IDs greater than 'nfolds'", conditionMessage(e), fixed = TRUE)
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    "fs_lasso: errors if custom_folds contains IDs greater than nfolds",
    passed5, note5
  )
  
  # Case 6: non-finite custom_folds
  err6 <- NULL
  cf6 <- rep(1L, n)
  cf6[1] <- NA_integer_
  passed6 <- tryCatch({
    fs_lasso(Xm, y, custom_folds = cf6, nfolds = 3)
    FALSE
  }, error = function(e) {
    err6 <<- e
    grepl("custom_folds' contains non-finite values", conditionMessage(e), fixed = TRUE)
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    "fs_lasso: errors if custom_folds contains non-finite values",
    passed6, note6
  )
}

###############################################################################
# Tests: Input Validation (General Arguments)
###############################################################################

test_fs_lasso_input_validation <- function() {
  set.seed(100)
  n <- 50
  p <- 4
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y  <- rnorm(n)
  
  # Case 1: x not data.frame/matrix
  err1 <- NULL
  passed1 <- tryCatch({
    fs_lasso("not a matrix", y)
    FALSE
  }, error = function(e) {
    err1 <<- e
    grepl("x' should be a data frame or matrix", conditionMessage(e), fixed = TRUE)
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_lasso: errors if x is not a data.frame or matrix",
    passed1, note1
  )
  
  # Case 2: x has zero columns
  err2 <- NULL
  x0 <- matrix(numeric(0), nrow = length(y), ncol = 0)
  passed2 <- tryCatch({
    fs_lasso(x0, y)
    FALSE
  }, error = function(e) {
    err2 <<- e
    grepl("must have at least one predictor", conditionMessage(e), fixed = TRUE)
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_lasso: errors if x has zero columns",
    passed2, note2
  )
  
  # Case 3: y non-numeric
  err3 <- NULL
  y_char <- as.character(y)
  passed3 <- tryCatch({
    fs_lasso(Xm, y_char)
    FALSE
  }, error = function(e) {
    err3 <<- e
    grepl("y' should be a numeric vector", conditionMessage(e), fixed = TRUE)
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "fs_lasso: errors if y is not numeric",
    passed3, note3
  )
  
  # Case 4: x/y length mismatch
  err4 <- NULL
  passed4 <- tryCatch({
    fs_lasso(Xm, c(y, 0))
    FALSE
  }, error = function(e) {
    err4 <<- e
    grepl("same number of rows/observations", conditionMessage(e), fixed = TRUE)
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "fs_lasso: errors if x and y have different number of rows",
    passed4, note4
  )
  
  # Case 5: invalid alpha
  err5 <- NULL
  passed5 <- tryCatch({
    fs_lasso(Xm, y, alpha = -0.1)
    FALSE
  }, error = function(e) {
    err5 <<- e
    grepl("alpha' must be a numeric value in (0, 1]", conditionMessage(e), fixed = TRUE)
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    "fs_lasso: errors if alpha is outside (0, 1]",
    passed5, note5
  )
  
  # Case 6: invalid nfolds
  err6 <- NULL
  passed6 <- tryCatch({
    fs_lasso(Xm, y, nfolds = 1)
    FALSE
  }, error = function(e) {
    err6 <<- e
    grepl("nfolds' must be a single integer greater than 1", conditionMessage(e), fixed = TRUE)
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    "fs_lasso: errors if nfolds <= 1",
    passed6, note6
  )
  
  # Case 7: invalid seed (non-integer numeric)
  err7 <- NULL
  passed7 <- tryCatch({
    fs_lasso(Xm, y, seed = 3.5)
    FALSE
  }, error = function(e) {
    err7 <<- e
    grepl("seed' must be a single integer value or NULL", conditionMessage(e), fixed = TRUE)
  })
  note7 <- if (!is.null(err7)) conditionMessage(err7) else NULL
  print_and_store_result(
    "fs_lasso: errors if seed is non-integer numeric",
    passed7, note7
  )
  
  # Case 8: invalid logical flags
  err8 <- NULL
  passed8 <- tryCatch({
    fs_lasso(Xm, y, return_model = "yes")
    FALSE
  }, error = function(e) {
    err8 <<- e
    grepl(
      "standardize', 'parallel', 'verbose', and 'return_model' must be single logical values",
      conditionMessage(e),
      fixed = TRUE
    )
  })
  note8 <- if (!is.null(err8)) conditionMessage(err8) else NULL
  print_and_store_result(
    "fs_lasso: errors if logical flags are not single logical values",
    passed8, note8
  )
}

###############################################################################
# Tests: Error Handling for y and Matrix Types
###############################################################################

test_fs_lasso_y_with_na <- function() {
  set.seed(55)
  n <- 60
  p <- 4
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y  <- Xm[, 1] + rnorm(n)
  y[5] <- NA_real_
  
  err <- NULL
  passed <- tryCatch({
    fs_lasso(Xm, y)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("y' contains non-finite values", conditionMessage(e), fixed = TRUE)
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: y with NA triggers clear non-finite error",
    passed, note
  )
}

test_fs_lasso_non_numeric_matrix <- function() {
  set.seed(10)
  n <- 20
  p <- 3
  Xm <- matrix(sample(letters[1:3], n * p, replace = TRUE), nrow = n, ncol = p)
  y  <- rnorm(n)
  
  err <- NULL
  passed <- tryCatch({
    fs_lasso(Xm, y)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Non-numeric matrices are not supported", conditionMessage(e), fixed = TRUE)
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: non-numeric matrix input triggers clear error",
    passed, note
  )
}

###############################################################################
# Tests: Model Variants (Elastic Net, High-Dimensional)
###############################################################################

test_fs_lasso_elastic_net <- function() {
  set.seed(222)
  n <- 140
  p <- 10
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y  <- 1.2 * Xm[, 1] - 0.8 * Xm[, 2] + rnorm(n)
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_lasso(x = Xm, y = y, alpha = 0.5, seed = 21, parallel = FALSE)
    is.list(res) &&
      is.data.frame(res$importance) &&
      nrow(res$importance) == ncol(Xm) &&
      any(res$importance$AbsCoefficient > 0)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: elastic-net path (alpha = 0.5) fits and returns non-zero coefficients",
    passed, note
  )
}

test_fs_lasso_small_n_large_p <- function() {
  set.seed(77)
  n <- 40
  p <- 100
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(Xm) <- paste0("X", seq_len(p))
  y  <- 0.5 * Xm[, 1] + rnorm(n)
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_lasso(x = Xm, y = y, seed = 10, parallel = FALSE)
    is.list(res) &&
      is.data.frame(res$importance) &&
      nrow(res$importance) == p
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: handles high-dimensional case (p >> n) without error",
    passed, note
  )
}

###############################################################################
# Tests: Parallel Path
###############################################################################

test_fs_lasso_parallel_path <- function() {
  have_parallel <- requireNamespace("parallel",   quietly = TRUE)
  have_doPar    <- requireNamespace("doParallel", quietly = TRUE)
  have_foreach  <- requireNamespace("foreach",    quietly = TRUE)
  
  if (!have_parallel || !have_doPar || !have_foreach) {
    print_and_store_result(
      "fs_lasso: parallel CV path (skipped - parallel/doParallel/foreach not available)",
      TRUE,
      "Required parallel packages are not available; test skipped."
    )
    return(invisible(NULL))
  }
  
  set.seed(123)
  n <- 120
  p <- 6
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y  <- Xm[, 1] - Xm[, 2] + rnorm(n)
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_lasso(x = Xm, y = y, parallel = TRUE, seed = 5, verbose = FALSE)
    is.list(res) &&
      is.data.frame(res$importance) &&
      nrow(res$importance) == ncol(Xm)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_lasso: parallel CV path executes successfully when backend available",
    passed, note
  )
}

###############################################################################
# Run All fs_lasso Tests
###############################################################################

run_fs_lasso_tests <- function() {
  cat("========== Running fs_lasso Tests ==========\n")
  
  # Reset global test_results for a fresh run
  test_results <<- data.frame(
    Test   = character(),
    Result = character(),
    stringsAsFactors = FALSE
  )
  
  test_fs_lasso_existence()
  test_fs_lasso_basic_matrix()
  test_fs_lasso_basic_dataframe()
  test_fs_lasso_return_model()
  test_fs_lasso_seed_reproducibility()
  test_fs_lasso_missing_values_imputation()
  test_fs_lasso_constant_and_all_na_predictors()
  test_fs_lasso_custom_folds_valid()
  test_fs_lasso_custom_folds_validation()
  test_fs_lasso_input_validation()
  test_fs_lasso_y_with_na()
  test_fs_lasso_non_numeric_matrix()
  test_fs_lasso_elastic_net()
  test_fs_lasso_small_n_large_p()
  test_fs_lasso_parallel_path()
  
  cat("========== fs_lasso Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment the following line to run all tests when this script is executed:
# run_fs_lasso_tests()
