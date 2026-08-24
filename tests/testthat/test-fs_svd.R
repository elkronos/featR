# Tests for fs_svd(). The exact path is pure base R (base::svd()), and every
# argument check runs before fs_require("RSpectra"), so everything below runs
# unconditionally except the two blocks that actually reach RSpectra::svds().

# Deterministic 6 x 4 matrix (no RNG). Full column rank and every column has
# non-zero variance, so it is safe for scale_input = TRUE.
svd_toy <- function() {
  matrix(
    c(4, 0, 0, 3,
      0, 5, 1, 2,
      2, 1, 6, 0,
      1, 3, 2, 7,
      5, 2, 0, 1,
      0, 4, 3, 2),
    nrow = 6L, ncol = 4L, byrow = TRUE
  )
}

# Deterministic 40 x 25 matrix for the approximate solver: min(dim) is large
# enough that k < min(dim) leaves RSpectra::svds() room to work, and the
# modular pattern keeps it well conditioned without touching the RNG.
svd_grid <- function(n_row = 40L, n_col = 25L) {
  m <- outer(
    seq_len(n_row), seq_len(n_col),
    function(i, j) ((i * j) %% 13L) + ((i + 2L * j) %% 7L) + 1
  )
  storage.mode(m) <- "double"
  m
}

# U diag(d) t(V), written without forming diag(d)
svd_reconstruct <- function(res) {
  res$left_singular_vectors %*%
    (res$singular_values * t(res$right_singular_vectors))
}

# Strip the scaled:center / scaled:scale attributes that scale() attaches
svd_bare <- function(m) {
  attributes(m) <- list(dim = dim(m))
  m
}

test_that("fs_svd returns the documented components and reconstructs the raw input", {
  X <- svd_toy()
  res <- fs_svd(X, scale_input = FALSE)

  expect_type(res, "list")
  expect_named(res, c("singular_values", "left_singular_vectors",
                      "right_singular_vectors"))
  expect_length(res$singular_values, 4L)
  expect_identical(dim(res$left_singular_vectors), c(6L, 4L))
  expect_identical(dim(res$right_singular_vectors), c(4L, 4L))
  expect_equal(svd_reconstruct(res), X, tolerance = 1e-10)
})

test_that("the full decomposition reconstructs the scaled input for every scale_input", {
  X <- svd_toy()

  res_both <- fs_svd(X, scale_input = TRUE)
  expect_equal(svd_reconstruct(res_both), svd_bare(scale(X)), tolerance = 1e-10)

  res_center <- fs_svd(X, scale_input = "center")
  expect_equal(svd_reconstruct(res_center),
               svd_bare(scale(X, center = TRUE, scale = FALSE)),
               tolerance = 1e-10)

  # NB: with center = FALSE, scale() divides by the root mean square, not sd
  res_scale <- fs_svd(X, scale_input = "scale")
  expect_equal(svd_reconstruct(res_scale),
               svd_bare(scale(X, center = FALSE, scale = TRUE)),
               tolerance = 1e-10)

  for (res in list(res_both, res_center, res_scale)) {
    expect_length(res$singular_values, 4L)
  }
})

test_that("a diagonal matrix has known singular values and vectors", {
  X <- diag(c(5, 3, 1))
  res <- fs_svd(X, scale_input = FALSE)

  expect_equal(res$singular_values, c(5, 3, 1), tolerance = 1e-10)
  expect_equal(abs(res$left_singular_vectors), diag(3), tolerance = 1e-10)
  expect_equal(abs(res$right_singular_vectors), diag(3), tolerance = 1e-10)
})

test_that("a rank-one matrix has exactly one non-zero singular value", {
  # X = outer(a, b) has sigma_1 = ||a|| * ||b|| and sigma_2 = 0
  X <- outer(c(1, 2, 3), c(1, 2))
  res <- fs_svd(X, scale_input = FALSE)

  expect_length(res$singular_values, 2L)
  expect_equal(res$singular_values[1L], sqrt(14 * 5), tolerance = 1e-10)
  expect_lt(res$singular_values[2L], 1e-10)
})

test_that("a 1 x 1 matrix decomposes to its absolute value", {
  res <- fs_svd(matrix(-3), scale_input = FALSE)
  expect_length(res$singular_values, 1L)
  expect_equal(res$singular_values, 3, tolerance = 1e-12)
})

test_that("singular values are non-increasing and truncation honors n_singular_values", {
  X <- svd_toy()
  full <- fs_svd(X, scale_input = FALSE)

  expect_identical(full$singular_values,
                   sort(full$singular_values, decreasing = TRUE))
  expect_true(all(diff(full$singular_values) <= 0))
  expect_true(all(full$singular_values >= 0))

  for (k in 1:4) {
    res <- fs_svd(X, scale_input = FALSE, n_singular_values = k)
    expect_length(res$singular_values, k)
    expect_identical(dim(res$left_singular_vectors), c(6L, as.integer(k)))
    expect_identical(dim(res$right_singular_vectors), c(4L, as.integer(k)))
    expect_equal(res$singular_values, full$singular_values[seq_len(k)],
                 tolerance = 1e-10)
    expect_equal(abs(res$left_singular_vectors),
                 abs(full$left_singular_vectors[, seq_len(k), drop = FALSE]),
                 tolerance = 1e-10)
  }
})

test_that("results agree with base::svd() up to column sign", {
  X <- svd_toy()

  ref <- svd(X)
  res <- fs_svd(X, scale_input = FALSE)
  expect_equal(res$singular_values, ref$d, tolerance = 1e-10)
  expect_equal(abs(res$left_singular_vectors), abs(ref$u), tolerance = 1e-10)
  expect_equal(abs(res$right_singular_vectors), abs(ref$v), tolerance = 1e-10)

  # the same holds on the scaled matrix, truncated
  ref_s <- svd(svd_bare(scale(X)))
  res_s <- fs_svd(X, scale_input = TRUE, n_singular_values = 3)
  expect_equal(res_s$singular_values, ref_s$d[1:3], tolerance = 1e-8)
  expect_equal(abs(res_s$left_singular_vectors),
               abs(ref_s$u[, 1:3, drop = FALSE]), tolerance = 1e-8)
  expect_equal(abs(res_s$right_singular_vectors),
               abs(ref_s$v[, 1:3, drop = FALSE]), tolerance = 1e-8)
})

test_that("numeric data.frames are accepted and coerced", {
  df <- data.frame(a = c(1, 2, 3, 4, 5, 6),
                   b = c(2, 4, 1, 3, 5, 0),
                   c = c(0, 1, 4, 2, 3, 5))
  res <- fs_svd(df, scale_input = TRUE, n_singular_values = 2)

  expect_named(res, c("singular_values", "left_singular_vectors",
                      "right_singular_vectors"))
  expect_length(res$singular_values, 2L)
  expect_identical(dim(res$left_singular_vectors), c(6L, 2L))
  expect_identical(dim(res$right_singular_vectors), c(3L, 2L))
})

# --- validation regressions: silent "repair" was replaced by real errors -----

test_that("invalid n_singular_values errors instead of being silently repaired", {
  X <- svd_toy() # min(dim) == 4

  expect_error(fs_svd(X, n_singular_values = 0),
               "'n_singular_values' must be between 1 and Inf")
  expect_error(fs_svd(X, n_singular_values = -1),
               "'n_singular_values' must be between 1 and Inf")
  expect_error(fs_svd(X, n_singular_values = 1.5),
               "'n_singular_values' must be a whole number")
  expect_error(fs_svd(X, n_singular_values = c(1, 2)),
               "'n_singular_values' must be a single finite number")
  expect_error(fs_svd(X, n_singular_values = NA_real_),
               "'n_singular_values' must be a single finite number")
  expect_error(fs_svd(X, n_singular_values = "2"),
               "'n_singular_values' must be a single finite number")

  # over-specification is an error now, not a silent truncation to min(dim)
  expect_error(
    fs_svd(X, n_singular_values = 5),
    "'n_singular_values' \\(5\\) exceeds min\\(dim\\(matrix_data\\)\\) \\(4\\)"
  )
  expect_error(
    fs_svd(X, n_singular_values = 10, svd_method = "exact", scale_input = FALSE),
    "'n_singular_values' \\(10\\) exceeds min\\(dim\\(matrix_data\\)\\) \\(4\\)"
  )
})

test_that("invalid svd_threshold errors instead of falling back to the default", {
  X <- svd_toy()

  expect_error(fs_svd(X, svd_threshold = "big"),
               "'svd_threshold' must be a single finite number")
  expect_error(fs_svd(X, svd_threshold = c(10, 20)),
               "'svd_threshold' must be a single finite number")
  expect_error(fs_svd(X, svd_threshold = NA_real_),
               "'svd_threshold' must be a single finite number")
  expect_error(fs_svd(X, svd_threshold = Inf),
               "'svd_threshold' must be a single finite number")
  expect_error(fs_svd(X, svd_threshold = 0), "'svd_threshold' must be positive")
  expect_error(fs_svd(X, svd_threshold = -1), "'svd_threshold' must be positive")
})

test_that("verbose, svd_method, approx_args and scale_input are validated", {
  X <- svd_toy()

  expect_error(fs_svd(X, verbose = "yes"), "'verbose' must be TRUE or FALSE")
  expect_error(fs_svd(X, verbose = NA), "'verbose' must be TRUE or FALSE")
  expect_error(fs_svd(X, svd_method = "not_a_method"), "should be one of")
  expect_error(fs_svd(X, approx_args = "tol = 1e-6"),
               "'approx_args' must be a list of additional arguments")
  expect_error(fs_svd(X, scale_input = "middle"),
               "Invalid 'scale_input'\\. Use TRUE, FALSE, 'center', or 'scale'\\.")
  expect_error(fs_svd(X, scale_input = NA), "Invalid 'scale_input'")
  expect_error(fs_svd(X, scale_input = c(TRUE, TRUE)), "Invalid 'scale_input'")
})

test_that("approx_args must not smuggle in A or k", {
  # this check happens before fs_require("RSpectra"), so it needs no Suggests
  X <- svd_toy()

  expect_error(
    fs_svd(X, n_singular_values = 2, svd_method = "approx",
           approx_args = list(k = 3)),
    "'approx_args' must not include 'A' or 'k'",
    fixed = TRUE
  )
  expect_error(
    fs_svd(X, svd_method = "approx", approx_args = list(A = X)),
    "these are set internally from the input matrix and 'n_singular_values'",
    fixed = TRUE
  )
  expect_error(
    fs_svd(X, approx_args = list(A = X, tol = 1e-6)),
    "must not include 'A' or 'k'",
    fixed = TRUE
  )
})

test_that("zero-variance columns are rejected by name when scaling", {
  df <- data.frame(a = c(1, 2, 3, 4, 5, 6),
                   b = c(2, 4, 1, 3, 5, 0),
                   const = rep(1, 6))

  expect_error(
    fs_svd(df, scale_input = TRUE),
    "Cannot scale: 1 column\\(s\\) have zero \\(or undefined\\) variance: const"
  )
  expect_error(fs_svd(df, scale_input = "scale"),
               "zero \\(or undefined\\) variance: const")
  expect_error(fs_svd(df, scale_input = TRUE),
               "use scale_input = 'center' or scale_input = FALSE",
               fixed = TRUE)

  # without column names the offender is reported by position
  m <- as.matrix(df)
  dimnames(m) <- NULL
  expect_error(fs_svd(m, scale_input = TRUE),
               "zero \\(or undefined\\) variance: column 3")

  # centering only is unaffected by the constant column
  expect_silent(res <- fs_svd(df, scale_input = "center"))
  expect_length(res$singular_values, 3L)
})

test_that("input coercion errors are informative", {
  expect_error(fs_svd("nope"), "Input must be a matrix or a data\\.frame\\.")
  expect_error(fs_svd(list(1, 2)), "Input must be a matrix or a data\\.frame\\.")
  expect_error(
    fs_svd(data.frame(a = 1:3, b = letters[1:3])),
    "All columns in the data\\.frame must be numeric.*Non-numeric: b"
  )
  expect_error(fs_svd(matrix(c(1, 2, NA, 4), nrow = 2), scale_input = FALSE),
               "Input contains non-finite values \\(NA/NaN/Inf\\)")
  expect_error(fs_svd(matrix(c(1, 2, Inf, 4), nrow = 2), scale_input = FALSE),
               "Input contains non-finite values")
  expect_error(fs_svd(matrix(numeric(0), nrow = 0, ncol = 5)),
               "Input matrix must have at least one row and one column")
})

test_that("the removed memoise_result argument is gone", {
  X <- svd_toy()

  expect_false("memoise_result" %in% names(formals(fs_svd)))
  expect_error(fs_svd(X, scale_input = FALSE, memoise_result = FALSE),
               "unused argument")
  expect_error(fs_svd(X, memoise_result = TRUE), "unused argument")
})

test_that("fs_svd signature and defaults are stable", {
  fx <- formals(fs_svd)

  expect_identical(
    names(fx),
    c("matrix_data", "scale_input", "n_singular_values", "svd_method",
      "svd_threshold", "approx_args", "verbose")
  )
  expect_identical(fx$scale_input, TRUE)
  expect_null(fx$n_singular_values)
  expect_identical(eval(fx$svd_method), c("auto", "exact", "approx"))
  expect_identical(fx$svd_threshold, 100)
  expect_identical(eval(fx$approx_args), list())
  expect_identical(fx$verbose, FALSE)
})

# --- solver selection -------------------------------------------------------

test_that("svd_method = 'auto' quietly picks the exact solver for small matrices", {
  X <- svd_toy() # min(dim) = 4 <= svd_threshold = 100
  expect_silent(res <- fs_svd(X, scale_input = FALSE))
  expect_length(res$singular_values, 4L)
})

test_that("'auto' explains why a large matrix still used the exact solver", {
  # min(dim) = 4 > svd_threshold = 3, but the default k == min(dim) rules the
  # approximate solver out, so the exact solver is used with an explanation.
  X <- svd_toy()

  msgs <- capture_messages(
    res <- fs_svd(X, scale_input = FALSE, svd_threshold = 3)
  )
  expect_true(any(grepl(
    paste0("svd_method = 'auto' selected the exact solver despite a large ",
           "matrix because all min(dim(matrix_data)) singular values were ",
           "requested; set n_singular_values < min(dim(matrix_data)) to ",
           "enable the approximate solver."),
    msgs, fixed = TRUE
  )))
  expect_length(res$singular_values, 4L)
  expect_equal(res$singular_values, svd(X)$d, tolerance = 1e-10)
})

test_that("svd_method = 'approx' falls back to exact when all values are requested", {
  # k == min(dim) is unusable for RSpectra::svds(); the fallback happens before
  # fs_require("RSpectra"), so this path needs no Suggests either.
  X <- svd_toy()

  msgs <- capture_messages(
    res <- fs_svd(X, scale_input = FALSE, svd_method = "approx")
  )
  expect_true(any(grepl(
    paste0("Requested n_singular_values equals min(dim(matrix_data)), but ",
           "RSpectra::svds() requires fewer; falling back to exact SVD."),
    msgs, fixed = TRUE
  )))
  expect_length(res$singular_values, 4L)
  expect_equal(res$singular_values, svd(X)$d, tolerance = 1e-10)
})

test_that("verbose narrates the scaling and solver steps", {
  X <- svd_toy()

  msgs <- capture_messages(fs_svd(X, scale_input = TRUE, verbose = TRUE))
  expect_true(any(grepl("Applying centering and scaling...", msgs, fixed = TRUE)))
  expect_true(any(grepl("Auto-selected SVD method: exact", msgs, fixed = TRUE)))
  expect_true(any(grepl("Computing exact SVD via base::svd()...", msgs,
                        fixed = TRUE)))

  expect_true(any(grepl(
    "Applying centering only...",
    capture_messages(fs_svd(X, scale_input = "center", verbose = TRUE)),
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Applying scaling only...",
    capture_messages(fs_svd(X, scale_input = "scale", verbose = TRUE)),
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "No scaling applied.",
    capture_messages(fs_svd(X, scale_input = FALSE, verbose = TRUE)),
    fixed = TRUE
  )))
})

test_that("fs_svd leaves the caller's RNG state untouched", {
  set.seed(20260823)
  invisible(stats::runif(1))
  before <- .Random.seed

  invisible(fs_svd(svd_toy(), scale_input = TRUE))
  expect_identical(.Random.seed, before)
})

# --- approximate solver (RSpectra) ------------------------------------------

test_that("the approximate solver reproduces the top-k singular values", {
  skip_if_not_installed("RSpectra")

  X <- svd_grid()
  k <- 4L
  res <- fs_svd(X, scale_input = FALSE, n_singular_values = k,
                svd_method = "approx")

  expect_named(res, c("singular_values", "left_singular_vectors",
                      "right_singular_vectors"))
  expect_length(res$singular_values, k)
  expect_identical(dim(res$left_singular_vectors), c(40L, k))
  expect_identical(dim(res$right_singular_vectors), c(25L, k))
  expect_identical(res$singular_values,
                   sort(res$singular_values, decreasing = TRUE))

  # only the values are compared: singular vectors are only unique up to sign
  # (and up to rotation within any tied block)
  expect_equal(res$singular_values, svd(X)$d[seq_len(k)], tolerance = 1e-6)
})

test_that("approx_args reach RSpectra::svds() and 'auto' takes the approximate path", {
  skip_if_not_installed("RSpectra")

  X <- svd_grid()

  res <- fs_svd(X, scale_input = FALSE, n_singular_values = 3,
                svd_method = "approx", approx_args = list(tol = 1e-9))
  expect_length(res$singular_values, 3L)
  expect_equal(res$singular_values, svd(X)$d[1:3], tolerance = 1e-5)

  # min(dim) = 25 > svd_threshold = 10 and k = 3 < 25, so 'auto' goes approximate
  msgs <- capture_messages(
    auto <- fs_svd(X, scale_input = FALSE, n_singular_values = 3,
                   svd_threshold = 10, verbose = TRUE)
  )
  expect_true(any(grepl("Auto-selected SVD method: approx", msgs, fixed = TRUE)))
  expect_true(any(grepl("Computing approximate SVD via RSpectra::svds()...",
                        msgs, fixed = TRUE)))
  expect_equal(auto$singular_values, svd(X)$d[1:3], tolerance = 1e-6)
})
