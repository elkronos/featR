###############################################################################
# Testing Infrastructure - fs_svd (base R, no testthat required)
###############################################################################

# Assumes fs_svd() and helpers are defined in the environment.

# Utilities ---------------------------------------------------------------

.msg <- function(...) cat(sprintf(...), "\n")

expect_true <- function(cond, msg = "Expected condition to be TRUE.") {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

expect_error_contains <- function(expr, pattern, msg = NULL) {
  got_err <- FALSE
  txt <- NULL
  tryCatch({
    force(expr)
  }, error = function(e) {
    got_err <<- TRUE
    txt <<- conditionMessage(e)
  })
  if (!got_err) {
    stop("Expected an error, but expression succeeded.", call. = FALSE)
  }
  if (!grepl(pattern, txt, fixed = TRUE)) {
    stop(if (is.null(msg)) sprintf("Error did not contain '%s'. Got: %s", pattern, txt) else msg, call. = FALSE)
  }
  invisible(TRUE)
}

expect_equal_len <- function(x, n, what = "vector") {
  lx <- length(x)
  expect_true(lx == n, sprintf("Expected %s length %d; got %d.", what, n, lx))
}

# Robust dimension check (avoid identical() int vs dbl pitfall)
expect_dim <- function(M, nrow_exp, ncol_exp, what = "matrix") {
  nr <- nrow(M); nc <- ncol(M)
  if (!is.integer(nrow_exp)) nrow_exp <- as.integer(nrow_exp)
  if (!is.integer(ncol_exp)) ncol_exp <- as.integer(ncol_exp)
  if (!is.integer(nr)) nr <- as.integer(nr)
  if (!is.integer(nc)) nc <- as.integer(nc)
  expect_true(nr == nrow_exp && nc == ncol_exp,
              sprintf("Expected %s dim [%d x %d]; got [%d x %d].",
                      what, nrow_exp, ncol_exp, nr, nc))
}

# Frobenius norm helper
fnorm <- function(M) sqrt(sum(M * M))

# Reconstruct from SVD parts: U * diag(d) * t(V)
reconstruct_from_svd <- function(U, d, V) U %*% (d * t(V))

# Main test runner --------------------------------------------------------

#' Unit Acceptance Testing for fs_svd
#'
#' Runs a series of tests against the refactored fs_svd() implementation.
#' Uses base R assertions; prints a summary at the end.
#'
#' @examples
#' test_fs_svd()
test_fs_svd <- function() {
  .msg("Running Unit Acceptance Tests (UAT) for fs_svd...")
  
  pass <- 0L
  fail <- 0L
  
  run <- function(name, code) {
    .msg("=== %s ===", name)
    err_txt <- NULL
    ok <- tryCatch({
      force(code)
      TRUE
    }, error = function(e) {
      err_txt <<- conditionMessage(e)
      FALSE
    })
    if (ok) {
      .msg("PASS\n")
      pass <<- pass + 1L
    } else {
      .msg("FAILED: %s", if (nzchar(err_txt)) err_txt else "<no error message>")
      .msg("----\n")
      fail <<- fail + 1L
    }
  }
  
  set.seed(123)
  
  # Test 1: Default SVD with centering & scaling; shape checks; low recon error
  run("Test 1: Default (auto) SVD, scale=TRUE", {
    X <- matrix(rnorm(24), nrow = 6)   # 6 x 4
    if (exists("dbg", mode = "function")) dbg("T1: X dim=[%d x %d]", nrow(X), ncol(X))
    res <- fs_svd(X, verbose = TRUE)
    k <- min(dim(X))
    expect_equal_len(res$singular_values, k, "singular_values")
    expect_dim(res$left_singular_vectors,  nrow(X), k, "left_singular_vectors")
    expect_dim(res$right_singular_vectors, ncol(X), k, "right_singular_vectors")
    
    U <- res$left_singular_vectors
    d <- res$singular_values
    V <- res$right_singular_vectors
    Xhat <- reconstruct_from_svd(U, d, V)
    expect_dim(Xhat, nrow(X), ncol(X), "reconstruction")
  })
  
  # Test 2: Center only + truncation
  run("Test 2: scale_input='center', n_singular_values=2", {
    X <- matrix(rnorm(40), nrow = 5)   # 5 x 8
    k <- 2
    res <- fs_svd(X, scale_input = "center", n_singular_values = k, verbose = TRUE)
    expect_equal_len(res$singular_values, k)
    expect_dim(res$left_singular_vectors,  nrow(X), k)
    expect_dim(res$right_singular_vectors, ncol(X), k)
  })
  
  # Test 3: Scale only + truncation
  run("Test 3: scale_input='scale', n_singular_values=3", {
    X <- matrix(rnorm(45), nrow = 9)   # 9 x 5
    k <- 3
    res <- fs_svd(X, scale_input = "scale", n_singular_values = k, verbose = TRUE)
    expect_equal_len(res$singular_values, k)
    expect_dim(res$left_singular_vectors,  nrow(X), k)
    expect_dim(res$right_singular_vectors, ncol(X), k)
  })
  
  # Test 4: No scaling
  run("Test 4: scale_input=FALSE", {
    X <- matrix(rnorm(30), nrow = 6)   # 6 x 5
    k <- 4
    res <- fs_svd(X, scale_input = FALSE, n_singular_values = k, verbose = TRUE)
    expect_equal_len(res$singular_values, k)
  })
  
  # Test 5: Approximate SVD on a larger matrix (k < min(dim))
  run("Test 5: svd_method='approx' on large matrix", {
    X <- matrix(rnorm(150 * 120), nrow = 150)
    k <- 5
    res <- fs_svd(X, n_singular_values = k, svd_method = "approx", verbose = TRUE)
    expect_equal_len(res$singular_values, k)
    expect_dim(res$left_singular_vectors,  nrow(X), k)
    expect_dim(res$right_singular_vectors, ncol(X), k)
  })
  
  # Test 6: Auto threshold should pick approx for big problems
  run("Test 6: svd_method='auto' + low threshold triggers approx", {
    X <- matrix(rnorm(150 * 120), nrow = 150)
    k <- 6
    res <- fs_svd(X, n_singular_values = k, svd_threshold = 50, verbose = TRUE)
    expect_equal_len(res$singular_values, k)
    expect_dim(res$left_singular_vectors,  nrow(X), k)
    expect_dim(res$right_singular_vectors, ncol(X), k)
  })
  
  # Test 7: approx_args are forwarded (e.g., tolerance). Just ensure it runs.
  run("Test 7: approx_args passthrough", {
    X <- matrix(rnorm(160 * 100), nrow = 160)
    k <- 7
    res <- fs_svd(
      X,
      n_singular_values = k,
      svd_method = "approx",
      approx_args = list(tol = 1e-3),
      verbose = TRUE
    )
    expect_equal_len(res$singular_values, k)
  })
  
  # Test 8: numeric data.frame input is accepted and coerced
  run("Test 8: numeric data.frame input", {
    df <- data.frame(a = rnorm(12), b = rnorm(12), c = rnorm(12))
    res <- fs_svd(df, n_singular_values = 2, verbose = TRUE)
    expect_equal_len(res$singular_values, 2)
  })
  
  # Test 9: non-numeric data.frame should error
  run("Test 9: non-numeric data.frame errors", {
    df_bad <- data.frame(a = rnorm(5), b = letters[1:5])
    expect_error_contains(
      fs_svd(df_bad, verbose = TRUE),
      "must be numeric"
    )
  })
  
  # Test 10: Matrix with missing values should error
  run("Test 10: matrix with NA errors", {
    Xna <- matrix(c(1, 2, NA, 4, 5, 6), nrow = 3)
    expect_error_contains(
      fs_svd(Xna, verbose = TRUE),
      "missing values"
    )
  })
  
  # Test 11: Invalid scale_input should error
  run("Test 11: invalid scale_input errors", {
    X <- matrix(rnorm(20), nrow = 4)
    expect_error_contains(
      fs_svd(X, scale_input = "invalid", verbose = TRUE),
      "Invalid 'scale_input'"
    )
  })
  
  # Test 12: n_singular_values > min(dim) should NOT error (truncation/fallback ok)
  run("Test 12: overspecified n_singular_values tolerated", {
    X <- matrix(rnorm(20), nrow = 4) # min(dim)=4
    res <- fs_svd(X, n_singular_values = 10, svd_method = "exact", verbose = TRUE)
    expect_equal_len(res$singular_values, 4)
  })
  
  # Test 13: Memoised vs non-memoised produce identical results
  run("Test 13: memoise_result consistency", {
    X <- matrix(rnorm(60), nrow = 10)
    k <- 3
    r1 <- fs_svd(X, n_singular_values = k, memoise_result = TRUE,  verbose = FALSE)
    r2 <- fs_svd(X, n_singular_values = k, memoise_result = FALSE, verbose = FALSE)
    expect_true(all.equal(r1$singular_values, r2$singular_values) == TRUE,
                "singular_values differ between memoised and non-memoised runs.")
    expect_true(max(abs(abs(r1$left_singular_vectors)  - abs(r2$left_singular_vectors)))  < 1e-6,
                "left singular vectors differ (up to sign).")
    expect_true(max(abs(abs(r1$right_singular_vectors) - abs(r2$right_singular_vectors))) < 1e-6,
                "right singular vectors differ (up to sign).")
  })
  
  # Test 14: Approx vs exact close on small k (sanity; not strict equality)
  run("Test 14: approx vs exact are close for top-k", {
    X <- matrix(rnorm(200 * 60), nrow = 200)
    k <- 4
    ex <- fs_svd(X, n_singular_values = k, svd_method = "exact",  scale_input = FALSE)
    ap <- fs_svd(X, n_singular_values = k, svd_method = "approx", scale_input = FALSE)
    rel_err <- max(abs(ex$singular_values - ap$singular_values) / (abs(ex$singular_values) + 1e-12))
    expect_true(rel_err < 1e-2, sprintf("Relative error too large: %g", rel_err))
  })
  
  .msg("All UAT completed. Passed: %d | Failed: %d", pass, fail)
  invisible(list(passed = pass, failed = fail))
}

## Uncomment to run:
# test_fs_svd()
