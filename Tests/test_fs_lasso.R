###############################################################################
# Testing Infrastructure - Lasso
###############################################################################

#' Run Unit Tests for the fs_lasso Function
#'
#' Runs a suite of checks to validate correctness, robustness, and usability of fs_lasso.
#' Assumes fs_lasso and its helpers are already defined in the current session / package.
#'
#' @examples
#' \dontrun{
#'   test_fs_lasso()
#' }
#' @export
test_fs_lasso <- function() {
  cat("Running unit tests for fs_lasso...\n\n")
  
  # --- Minimal assertion helpers ------------------------------------------------
  pass <- 0L; fail <- 0L
  check <- function(cond, msg_ok = "OK", msg_fail = "FAILED") {
    if (isTRUE(cond)) {
      pass <<- pass + 1L
      cat("  ✓", msg_ok, "\n")
      TRUE
    } else {
      fail <<- fail + 1L
      cat("  ✗", msg_fail, "\n")
      FALSE
    }
  }
  expect_error_like <- function(expr, pattern, label) {
    got <- tryCatch({ force(expr); NULL }, error = function(e) e$message)
    if (is.null(got)) {
      check(FALSE, msg_ok = label, msg_fail = paste0(label, " (expected error not thrown)"))
    } else {
      if (grepl(pattern, got, fixed = TRUE)) {
        check(TRUE,  msg_ok = label)
      } else {
        check(FALSE, msg_fail = paste0(label, " (unexpected error): ", got))
      }
    }
  }
  
  # --- Synthetic data -----------------------------------------------------------
  set.seed(123)
  n <- 160
  p <- 8
  Xm <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(Xm) <- paste0("X", seq_len(p))
  # true model: x1, x2, x3 active
  y  <- 2 * Xm[, 1] - 3 * Xm[, 2] + 1.5 * Xm[, 3] + rnorm(n, sd = 0.8)
  
  # A data.frame version with a factor + char column to exercise model.matrix path
  Xdf <- as.data.frame(Xm)
  Xdf$cat <- factor(sample(letters[1:3], n, TRUE))
  Xdf$grp <- sample(c("A", "B"), n, TRUE)
  
  # --- 1) Basic functionality (matrix) -----------------------------------------
  cat("Test 1: Basic functionality (matrix input)\n")
  res1 <- fs_lasso(x = Xm, y = y, verbose = FALSE, seed = 42)
  ok1 <- is.list(res1) &&
    all(c("importance", "lambda_min", "lambda_1se") %in% names(res1)) &&
    is.data.frame(res1$importance) &&
    nrow(res1$importance) == ncol(Xm)
  check(ok1, "Returned structure looks correct")
  
  # importance is sorted by |coef|
  imp_sorted <- all(diff(res1$importance$AbsCoefficient) <= 0)
  check(imp_sorted, "Importance is sorted by absolute coefficient (desc)")
  
  # top features should include X1, X2, X3
  top5 <- head(res1$importance$Variable, 5)
  check(all(c("X1", "X2", "X3") %in% top5),
        "Top-5 importance contains true signals (X1, X2, X3)")
  
  # --- 2) Basic functionality (data.frame with non-numeric columns) ------------
  cat("\nTest 2: Data.frame input with factor/character (model.matrix path)\n")
  res2 <- fs_lasso(x = Xdf, y = y, verbose = FALSE, seed = 42)
  ok2 <- is.data.frame(res2$importance) && nrow(res2$importance) >= ncol(Xm)
  check(ok2, "Successfully handled factors/characters via model.matrix")
  
  # --- 3) Return model object ---------------------------------------------------
  cat("\nTest 3: Return model object\n")
  res3 <- fs_lasso(x = Xm, y = y, return_model = TRUE, seed = 202)
  ok3 <- "model" %in% names(res3) &&
    inherits(res3$model, "cv.glmnet") &&
    is.numeric(res3$lambda_min) &&
    is.numeric(res3$lambda_1se)
  check(ok3, "cv.glmnet model returned with lambda_min / lambda_1se")
  
  # --- 4) Seed reproducibility --------------------------------------------------
  cat("\nTest 4: Seed reproducibility\n")
  rA <- fs_lasso(x = Xm, y = y, seed = 999)
  rB <- fs_lasso(x = Xm, y = y, seed = 999)
  # compare lambda.min and the importance ordering (not necessarily identical numeric values across platforms)
  same_lambda <- isTRUE(all.equal(rA$lambda_min, rB$lambda_min, tolerance = 1e-12))
  same_order  <- identical(rA$importance$Variable, rB$importance$Variable)
  check(same_lambda && same_order, "Deterministic lambda_min and importance ordering with same seed")
  
  # --- 5) Missing values in X ---------------------------------------------------
  cat("\nTest 5: Missing values imputation\n")
  X_na <- Xm
  X_na[sample(length(X_na), size = floor(0.05 * length(X_na)))] <- NA
  res5 <- fs_lasso(x = X_na, y = y, seed = 7)
  ok5 <- is.data.frame(res5$importance) && nrow(res5$importance) == ncol(Xm)
  check(ok5, "Model fits with missing values in X (mean imputation)")
  
  # --- 6) Custom folds ----------------------------------------------------------
  cat("\nTest 6: Custom CV folds\n")
  kfold <- 5L
  set.seed(1)
  foldid <- as.integer(sample(seq_len(kfold), n, replace = TRUE))
  res6 <- fs_lasso(x = Xm, y = y, custom_folds = foldid, nfolds = kfold, seed = 11)
  ok6 <- is.data.frame(res6$importance) && nrow(res6$importance) == ncol(Xm)
  check(ok6, "Custom fold IDs accepted and model fits")
  
  # --- 7) Parallel path (conditional) ------------------------------------------
  cat("\nTest 7: Parallel CV (if doParallel + parallel present)\n")
  have_doPar   <- requireNamespace("doParallel", quietly = TRUE)
  have_parallel <- requireNamespace("parallel", quietly = TRUE)
  if (have_doPar && have_parallel) {
    res7 <- fs_lasso(x = Xm, y = y, parallel = TRUE, verbose = TRUE, seed = 5)
    ok7 <- is.data.frame(res7$importance) && nrow(res7$importance) == ncol(Xm)
    check(ok7, "Parallel CV executed successfully")
  } else {
    check(TRUE, "Parallel deps not available; skipped (treated as pass)")
  }
  
  # --- 8) Elastic net (alpha in (0,1)) -----------------------------------------
  cat("\nTest 8: Elastic net (alpha = 0.5)\n")
  res8 <- fs_lasso(x = Xm, y = y, alpha = 0.5, seed = 21)
  ok8 <- is.data.frame(res8$importance) && nrow(res8$importance) == ncol(Xm)
  check(ok8, "Elastic net path fits and returns importance")
  
  # --- 9) Error handling --------------------------------------------------------
  cat("\nTest 9: Error handling\n")
  expect_error_like(fs_lasso("bad_x", y), "x' should be a data frame or matrix", "Reject non-matrix/data.frame x")
  expect_error_like(fs_lasso(Xm, "bad_y"), "y' should be a numeric vector", "Reject non-numeric y")
  expect_error_like(fs_lasso(Xm, c(y, 0)), "same number of rows", "Reject length mismatch x/y")
  expect_error_like(fs_lasso(Xm, y, alpha = -0.1), "alpha' must be a numeric value in (0, 1]", "Reject invalid alpha")
  expect_error_like(fs_lasso(Xm, y, nfolds = 1), "nfolds' must be a single integer greater than 1", "Reject invalid nfolds")
  bad_folds <- rep(99L, length(y))
  expect_error_like(fs_lasso(Xm, y, custom_folds = bad_folds, nfolds = 5),
                    "invalid IDs (must be in 1..nfolds)", "Reject out-of-range custom folds")
  
  # --- 10) y with NA should error ----------------------------------------------
  cat("\nTest 10: y with NA should error\n")
  y_bad <- y; y_bad[5] <- NA_real_
  expect_error_like(fs_lasso(Xm, y_bad), "y' contains non-finite values", "Reject non-finite y")
  
  # --- 11) Names and shapes -----------------------------------------------------
  cat("\nTest 11: Output shapes and columns\n")
  cols_ok <- identical(colnames(res1$importance), c("Variable", "Coefficient", "AbsCoefficient"))
  nrows_ok <- (nrow(res1$importance) == p)
  check(cols_ok && nrows_ok, "Importance has expected columns and row count")
  
  # --- Summary -----------------------------------------------------------------
  cat("\n----------------------------------------\n")
  cat("Tests passed :", pass, "\n")
  cat("Tests failed :", fail, "\n")
  cat("----------------------------------------\n")
  if (fail > 0L) {
    stop(sprintf("test_fs_lasso: %d test(s) failed.", fail))
  } else {
    cat("All tests completed successfully.\n")
  }
}
