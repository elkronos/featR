###############################################################################
# Testing Infrastructure - fs_svd
###############################################################################

# Assumes fs_svd() and its helpers are already defined in the environment.

# Global container for test results
fs_svd_test_results <- data.frame(
  Test   = character(),
  Result = character(),
  stringsAsFactors = FALSE
)

# Helper: Print and Store Test Result -----------------------------------------

#' Helper: Print and Store Test Result for fs_svd
#'
#' @param test_name Character. Name of the test.
#' @param passed Logical. Whether the test passed.
#' @param note Optional character. Additional notes (e.g., error message).
print_and_store_fs_svd_result <- function(test_name, passed, note = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-80s [%s]\n", test_name, result))
  if (!is.null(note)) cat("  Note: ", note, "\n")
  fs_svd_test_results <<- rbind(
    fs_svd_test_results,
    data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)
  )
}

# Small Helpers for Numeric Checks -------------------------------------------

fs_svd_fnorm <- function(M) sqrt(sum(M * M))

fs_svd_reconstruct_from_svd <- function(U, d, V) {
  U %*% (d * t(V))
}

###############################################################################
# Tests: Existence and Basic Input Validation
###############################################################################

test_fs_svd_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_svd") && is.function(fs_svd)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result("fs_svd: Function exists and is callable", passed, note)
}

test_fs_svd_input_validation_type <- function() {
  err <- NULL
  passed <- tryCatch({
    fs_svd("not a matrix or data.frame", scale_input = FALSE)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Input must be a matrix or a data.frame", conditionMessage(e), fixed = TRUE)
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: errors if input is not a matrix or data.frame",
    passed, note
  )
}

test_fs_svd_input_validation_non_numeric_df <- function() {
  df_bad <- data.frame(
    a = rnorm(5),
    b = letters[1:5]
  )
  err <- NULL
  passed <- tryCatch({
    fs_svd(df_bad, scale_input = FALSE)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("must be numeric", conditionMessage(e), fixed = TRUE)
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: errors if data.frame has non-numeric columns",
    passed, note
  )
}

test_fs_svd_input_validation_non_finite_values <- function() {
  X <- matrix(c(1, 2, NA, 4, Inf, 6), nrow = 3)
  err <- NULL
  passed <- tryCatch({
    fs_svd(X, scale_input = FALSE)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("non-finite values", conditionMessage(e), fixed = TRUE)
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: errors if input contains NA/NaN/Inf",
    passed, note
  )
}

test_fs_svd_input_validation_zero_dim_matrix <- function() {
  X0 <- matrix(numeric(0), nrow = 0, ncol = 5)
  err <- NULL
  passed <- tryCatch({
    fs_svd(X0, scale_input = FALSE)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("at least one row and one column", conditionMessage(e), fixed = TRUE)
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: errors if matrix has zero rows or columns",
    passed, note
  )
}

###############################################################################
# Tests: scale_input Variants and Validation
###############################################################################

test_fs_svd_scale_input_variants <- function() {
  set.seed(1)
  X <- matrix(rnorm(6 * 5), nrow = 6)
  k <- 3L
  err <- NULL
  passed <- tryCatch({
    res_true   <- fs_svd(X, scale_input = TRUE,   n_singular_values = k, memoise_result = FALSE)
    res_center <- fs_svd(X, scale_input = "center", n_singular_values = k, memoise_result = FALSE)
    res_scale  <- fs_svd(X, scale_input = "scale",  n_singular_values = k, memoise_result = FALSE)
    res_false  <- fs_svd(X, scale_input = FALSE, n_singular_values = k, memoise_result = FALSE)
    
    stopifnot(length(res_true$singular_values)   == k)
    stopifnot(length(res_center$singular_values) == k)
    stopifnot(length(res_scale$singular_values)  == k)
    stopifnot(length(res_false$singular_values)  == k)
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: scale_input supports TRUE, FALSE, 'center', and 'scale'",
    passed, note
  )
}

test_fs_svd_invalid_scale_input <- function() {
  set.seed(2)
  X <- matrix(rnorm(4 * 5), nrow = 4)
  err <- NULL
  passed <- tryCatch({
    fs_svd(X, scale_input = "invalid", memoise_result = FALSE)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Invalid 'scale_input'", conditionMessage(e), fixed = TRUE)
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: invalid scale_input throws clear error",
    passed, note
  )
}

test_fs_svd_scaling_non_finite_error <- function() {
  # Constant column leads to zero variance; scaling should produce NaNs and be caught.
  X <- matrix(1, nrow = 10, ncol = 3)
  err <- NULL
  passed <- tryCatch({
    fs_svd(X, scale_input = TRUE, memoise_result = FALSE)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Scaling produced non-finite values", conditionMessage(e), fixed = TRUE)
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: scaling that produces non-finite values is rejected",
    passed, note
  )
}

###############################################################################
# Tests: Core Exact SVD Behaviour and Truncation
###############################################################################

test_fs_svd_exact_svd_reconstruction <- function() {
  set.seed(3)
  X <- matrix(rnorm(6 * 4), nrow = 6)  # 6 x 4
  err <- NULL
  passed <- tryCatch({
    res <- fs_svd(X, scale_input = FALSE, svd_method = "auto",
                  memoise_result = FALSE, verbose = FALSE)
    k <- min(dim(X))
    stopifnot(length(res$singular_values) == k)
    stopifnot(nrow(res$left_singular_vectors)  == nrow(X))
    stopifnot(ncol(res$left_singular_vectors)  == k)
    stopifnot(nrow(res$right_singular_vectors) == ncol(X))
    stopifnot(ncol(res$right_singular_vectors) == k)
    
    # Reconstruct and check Frobenius error relative to original (scale_input = FALSE)
    U <- res$left_singular_vectors
    d <- res$singular_values
    V <- res$right_singular_vectors
    Xhat <- fs_svd_reconstruct_from_svd(U, d, V)
    stopifnot(all(dim(Xhat) == dim(X)))
    rel_err <- fs_svd_fnorm(X - Xhat) / (fs_svd_fnorm(X) + 1e-12)
    stopifnot(rel_err < 1e-10)
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: exact SVD reconstructs matrix (scale_input = FALSE)",
    passed, note
  )
}

test_fs_svd_truncation_center_only <- function() {
  set.seed(4)
  X <- matrix(rnorm(5 * 8), nrow = 5)  # 5 x 8
  k <- 2L
  err <- NULL
  passed <- tryCatch({
    res <- fs_svd(X, scale_input = "center", n_singular_values = k,
                  memoise_result = FALSE)
    stopifnot(length(res$singular_values) == k)
    stopifnot(ncol(res$left_singular_vectors)  == k)
    stopifnot(ncol(res$right_singular_vectors) == k)
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: truncation works with scale_input = 'center'",
    passed, note
  )
}

test_fs_svd_truncation_scale_only <- function() {
  set.seed(5)
  X <- matrix(rnorm(9 * 5), nrow = 9)  # 9 x 5
  k <- 3L
  err <- NULL
  passed <- tryCatch({
    res <- fs_svd(X, scale_input = "scale", n_singular_values = k,
                  memoise_result = FALSE)
    stopifnot(length(res$singular_values) == k)
    stopifnot(ncol(res$left_singular_vectors)  == k)
    stopifnot(ncol(res$right_singular_vectors) == k)
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: truncation works with scale_input = 'scale'",
    passed, note
  )
}

test_fs_svd_no_scaling_truncation <- function() {
  set.seed(6)
  X <- matrix(rnorm(6 * 5), nrow = 6)  # 6 x 5
  k <- 4L
  err <- NULL
  passed <- tryCatch({
    res <- fs_svd(X, scale_input = FALSE, n_singular_values = k,
                  memoise_result = FALSE)
    stopifnot(length(res$singular_values) == k)
    stopifnot(ncol(res$left_singular_vectors)  == k)
    stopifnot(ncol(res$right_singular_vectors) == k)
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: truncation works with scale_input = FALSE",
    passed, note
  )
}

test_fs_svd_overspecified_n_singular_values <- function() {
  set.seed(7)
  X <- matrix(rnorm(4 * 5), nrow = 4)  # min(dim)=4
  err <- NULL
  passed <- tryCatch({
    res <- fs_svd(X, n_singular_values = 10L, svd_method = "exact",
                  scale_input = FALSE, memoise_result = FALSE)
    stopifnot(length(res$singular_values) == min(dim(X)))
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: overspecified n_singular_values truncated to min(dim)",
    passed, note
  )
}

test_fs_svd_invalid_n_singular_values_repaired <- function() {
  set.seed(8)
  X <- matrix(rnorm(6 * 5), nrow = 6)  # min(dim) = 5
  bad_values <- list(
    0,
    -1,
    1.5,
    c(1, 2),
    NA_real_
  )
  err <- NULL
  passed <- tryCatch({
    for (val in bad_values) {
      res <- fs_svd(X, n_singular_values = val, svd_method = "exact",
                    scale_input = FALSE, memoise_result = FALSE)
      if (length(res$singular_values) != min(dim(X))) {
        stop(sprintf("Unexpected # of singular values for n_singular_values = %s",
                     paste(val, collapse = ",")))
      }
    }
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else "Invalid n_singular_values repaired to full rank"
  print_and_store_fs_svd_result(
    "fs_svd: invalid n_singular_values fall back to min(dim(X))",
    passed, note
  )
}

###############################################################################
# Tests: Approximate SVD and Auto Method Selection
###############################################################################

test_fs_svd_approx_svd_large_matrix <- function() {
  set.seed(9)
  X <- matrix(rnorm(150 * 120), nrow = 150)
  k <- 5L
  err <- NULL
  passed <- tryCatch({
    res <- fs_svd(X, n_singular_values = k, svd_method = "approx",
                  scale_input = FALSE, memoise_result = FALSE)
    stopifnot(length(res$singular_values) == k)
    stopifnot(nrow(res$left_singular_vectors)  == nrow(X))
    stopifnot(ncol(res$left_singular_vectors)  == k)
    stopifnot(nrow(res$right_singular_vectors) == ncol(X))
    stopifnot(ncol(res$right_singular_vectors) == k)
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: svd_method = 'approx' works on large matrix",
    passed, note
  )
}

test_fs_svd_auto_threshold_triggers_approx_path <- function() {
  set.seed(10)
  X <- matrix(rnorm(150 * 120), nrow = 150)
  k <- 6L
  err <- NULL
  passed <- tryCatch({
    res <- fs_svd(X, n_singular_values = k, svd_threshold = 50,
                  svd_method = "auto", scale_input = FALSE,
                  memoise_result = FALSE)
    stopifnot(length(res$singular_values) == k)
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else
    "Auto method selection used on large matrix with low threshold"
  print_and_store_fs_svd_result(
    "fs_svd: svd_method = 'auto' runs on large matrix with low threshold",
    passed, note
  )
}

test_fs_svd_approx_args_passthrough <- function() {
  set.seed(11)
  X <- matrix(rnorm(160 * 100), nrow = 160)
  k <- 7L
  err <- NULL
  passed <- tryCatch({
    res <- fs_svd(
      X,
      n_singular_values = k,
      svd_method = "approx",
      approx_args = list(tol = 1e-3),
      scale_input = FALSE,
      memoise_result = FALSE
    )
    stopifnot(length(res$singular_values) == k)
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else "approx_args (e.g., tol) accepted without error"
  print_and_store_fs_svd_result(
    "fs_svd: approx_args are forwarded to RSpectra::svds",
    passed, note
  )
}

test_fs_svd_invalid_approx_args <- function() {
  set.seed(12)
  X <- matrix(rnorm(60 * 40), nrow = 60)
  err <- NULL
  passed <- tryCatch({
    fs_svd(X, n_singular_values = 5L, svd_method = "approx",
           approx_args = "not a list", scale_input = FALSE)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("'approx_args' must be a list", conditionMessage(e), fixed = TRUE)
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: invalid approx_args (non-list) throws clear error",
    passed, note
  )
}

test_fs_svd_invalid_svd_method <- function() {
  set.seed(13)
  X <- matrix(rnorm(5 * 4), nrow = 5)
  err <- NULL
  passed <- tryCatch({
    fs_svd(X, svd_method = "not_a_method", scale_input = FALSE)
    FALSE
  }, error = function(e) {
    err <<- e
    # match.arg error message will contain 'should be one of'
    grepl("should be one of", conditionMessage(e), fixed = TRUE)
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: invalid svd_method throws informative error",
    passed, note
  )
}

test_fs_svd_invalid_svd_threshold_repaired <- function() {
  set.seed(14)
  X <- matrix(rnorm(10 * 10), nrow = 10)
  err <- NULL
  passed <- tryCatch({
    res <- suppressWarnings(
      fs_svd(X, svd_threshold = "not numeric", svd_method = "auto",
             scale_input = FALSE, memoise_result = FALSE)
    )
    stopifnot(length(res$singular_values) == min(dim(X)))
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else
    "Invalid svd_threshold repaired (default used) without error"
  print_and_store_fs_svd_result(
    "fs_svd: invalid svd_threshold is repaired (no crash)",
    passed, note
  )
}

###############################################################################
# Tests: Data.frame Inputs and Memoisation Behaviour
###############################################################################

test_fs_svd_numeric_dataframe_input <- function() {
  set.seed(15)
  df <- data.frame(a = rnorm(12), b = rnorm(12), c = rnorm(12))
  err <- NULL
  passed <- tryCatch({
    res <- fs_svd(df, n_singular_values = 2L, scale_input = TRUE,
                  memoise_result = FALSE)
    stopifnot(length(res$singular_values) == 2L)
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_fs_svd_result(
    "fs_svd: accepts numeric data.frame and coerces to matrix",
    passed, note
  )
}

test_fs_svd_memoisation_consistency <- function() {
  set.seed(16)
  X <- matrix(rnorm(10 * 6), nrow = 10)
  k <- 3L
  err <- NULL
  passed <- tryCatch({
    r1 <- fs_svd(X, n_singular_values = k, memoise_result = TRUE,
                 scale_input = TRUE, verbose = FALSE)
    r2 <- fs_svd(X, n_singular_values = k, memoise_result = FALSE,
                 scale_input = TRUE, verbose = FALSE)
    
    if (!isTRUE(all.equal(r1$singular_values, r2$singular_values))) {
      stop("singular_values differ between memoised and non-memoised runs.")
    }
    lv_diff <- max(abs(abs(r1$left_singular_vectors)  - abs(r2$left_singular_vectors)))
    rv_diff <- max(abs(abs(r1$right_singular_vectors) - abs(r2$right_singular_vectors)))
    if (lv_diff >= 1e-6) stop("left singular vectors differ (up to sign).")
    if (rv_diff >= 1e-6) stop("right singular vectors differ (up to sign).")
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else "Memoised and non-memoised results are consistent"
  print_and_store_fs_svd_result(
    "fs_svd: memoise_result=TRUE vs FALSE yields consistent results",
    passed, note
  )
}

test_fs_svd_verbose_disables_memoisation_path <- function() {
  set.seed(17)
  X <- matrix(rnorm(10 * 6), nrow = 10)
  k <- 3L
  err <- NULL
  passed <- tryCatch({
    r1 <- fs_svd(X, n_singular_values = k, memoise_result = TRUE,
                 scale_input = TRUE, verbose = TRUE)
    r2 <- fs_svd(X, n_singular_values = k, memoise_result = FALSE,
                 scale_input = TRUE, verbose = TRUE)
    if (!isTRUE(all.equal(r1$singular_values, r2$singular_values))) {
      stop("singular_values differ between verbose TRUE runs.")
    }
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else
    "Verbose=TRUE path produces same results regardless of memoise_result"
  print_and_store_fs_svd_result(
    "fs_svd: verbose=TRUE path runs without memoisation side-effects",
    passed, note
  )
}

###############################################################################
# Tests: Approx vs Exact Numerical Consistency
###############################################################################

test_fs_svd_approx_vs_exact_close <- function() {
  set.seed(18)
  X <- matrix(rnorm(200 * 60), nrow = 200)
  k <- 4L
  err <- NULL
  passed <- tryCatch({
    ex <- fs_svd(X, n_singular_values = k, svd_method = "exact",
                 scale_input = FALSE, memoise_result = FALSE)
    ap <- fs_svd(X, n_singular_values = k, svd_method = "approx",
                 scale_input = FALSE, memoise_result = FALSE)
    rel_err <- max(abs(ex$singular_values - ap$singular_values) /
                     (abs(ex$singular_values) + 1e-12))
    if (rel_err >= 1e-2) {
      stop(sprintf("Relative error too large: %g", rel_err))
    }
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else "Top-k singular values reasonably close between exact and approx"
  print_and_store_fs_svd_result(
    "fs_svd: svd_method='approx' yields singular values close to 'exact' for top-k",
    passed, note
  )
}

###############################################################################
# Run All fs_svd Tests
###############################################################################

run_fs_svd_tests <- function() {
  cat("========== Running fs_svd Tests ==========\n")
  
  test_fs_svd_existence()
  test_fs_svd_input_validation_type()
  test_fs_svd_input_validation_non_numeric_df()
  test_fs_svd_input_validation_non_finite_values()
  test_fs_svd_input_validation_zero_dim_matrix()
  
  test_fs_svd_scale_input_variants()
  test_fs_svd_invalid_scale_input()
  test_fs_svd_scaling_non_finite_error()
  
  test_fs_svd_exact_svd_reconstruction()
  test_fs_svd_truncation_center_only()
  test_fs_svd_truncation_scale_only()
  test_fs_svd_no_scaling_truncation()
  test_fs_svd_overspecified_n_singular_values()
  test_fs_svd_invalid_n_singular_values_repaired()
  
  test_fs_svd_approx_svd_large_matrix()
  test_fs_svd_auto_threshold_triggers_approx_path()
  test_fs_svd_approx_args_passthrough()
  test_fs_svd_invalid_approx_args()
  test_fs_svd_invalid_svd_method()
  test_fs_svd_invalid_svd_threshold_repaired()
  
  test_fs_svd_numeric_dataframe_input()
  test_fs_svd_memoisation_consistency()
  test_fs_svd_verbose_disables_memoisation_path()
  
  test_fs_svd_approx_vs_exact_close()
  
  cat("========== fs_svd Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(fs_svd_test_results$Result))
  cat("\nDetailed Results:\n")
  print(fs_svd_test_results)
}

# Uncomment to run:
# run_fs_svd_tests()
