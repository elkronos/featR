# Tests for fs_correlation().
#
# pearson/spearman/kendall and the sequential pointbiserial path rely only
# on base stats, so most tests run everywhere. Only the polychoric method
# needs the suggested polycor package; validation (including the ordered-
# factor type check) runs before fs_require("polycor"), so it never skips.

test_that("fs_correlation validates its inputs", {
  d <- data.frame(a = c(1, 2, 3, 4, 5), b = c(2, 4, 6, 8, 10))

  expect_error(fs_correlation(d, 0.5, method = "banana"), "Invalid `method`")
  expect_error(fs_correlation(d, threshold = 1.5),
               "'threshold' must be between 0 and 1")
  expect_error(fs_correlation(d, threshold = -0.2),
               "'threshold' must be between 0 and 1")
  expect_error(fs_correlation(d["a"], 0.5), "at least 2 columns")
  expect_error(fs_correlation(data.frame(a = 1:5, f = letters[1:5]), 0.5),
               "All columns must be numeric for method 'pearson'")
  expect_error(fs_correlation(d, 0.5, output_format = "tibble"),
               "`output_format` must be 'matrix' or 'data.frame'")
  expect_error(fs_correlation(d, 0.5, sample_frac = 0),
               "`sample_frac` must be greater than 0")
  expect_error(fs_correlation(d, 0.5, sample_frac = 1.5),
               "'sample_frac' must be between 0 and 1")
  expect_error(fs_correlation(d, 0.5, diag_value = c(1, 2)),
               "single numeric value or NA")
  expect_error(fs_correlation(d, 0.5, n_cores = 0),
               "'n_cores' must be between 1 and Inf")
  # column-type checks run before any suggested package is required
  expect_error(fs_correlation(d, 0.5, method = "polychoric"),
               "must be ordered factors")
  expect_error(
    fs_correlation(
      data.frame(x = c(1.5, 2.5, 3.5), f = factor(c("u", "v", "w"))),
      0.5, method = "pointbiserial"
    ),
    "must be numeric or dichotomous"
  )
})

test_that("fs_correlation accepts the inclusive threshold bounds", {
  d <- data.frame(a = c(1, 2, 3, 4), b = c(1.5, 0.5, 2.5, 1.0))
  # threshold = 1 is valid; |r| > 1 can never happen, so nothing is selected
  expect_message(res <- fs_correlation(d, threshold = 1), "No variables meet")
  expect_identical(res$selected_vars, character(0))
})

test_that("pointbiserial sign matches the direction of association (ltm regression)", {
  # x increases with the SECOND factor level of y, so the point-biserial
  # correlation must come out positive.
  y01 <- rep(c(0, 1), each = 30)
  x   <- y01 * 2 + rep(c(-0.10, 0.05, 0.12, -0.03, 0.08, -0.06), 10)
  d_fac <- data.frame(x = x, y = factor(ifelse(y01 == 1, "b", "a")))

  res <- fs_correlation(d_fac, threshold = 0.5, method = "pointbiserial")
  cm  <- res$corr_matrix

  expect_true(is.matrix(cm))
  expect_identical(dimnames(cm), list(c("x", "y"), c("x", "y")))
  expect_gt(cm["x", "y"], 0)
  expect_identical(cm["x", "y"], cm["y", "x"])
  # known answer: point-biserial r IS the Pearson r against the 0/1 indicator
  expect_equal(unname(cm["x", "y"]), stats::cor(x, y01))
  expect_setequal(res$selected_vars, c("x", "y"))

  # same positive sign when the dichotomous variable is numeric 0/1
  d_num <- data.frame(x = x, y = y01)
  res_num <- fs_correlation(d_num, threshold = 0.5, method = "pointbiserial")
  expect_gt(res_num$corr_matrix["x", "y"], 0)
  expect_equal(unname(res_num$corr_matrix["x", "y"]), stats::cor(x, y01))
})

test_that("pearson correlations equal stats::cor() and pairs report both members", {
  d <- data.frame(
    a = as.numeric(1:20),
    b = as.numeric(1:20) * 2 + 1, # perfectly correlated with a
    c = rep(c(5, -5), 10)         # essentially uncorrelated with either
  )

  res <- fs_correlation(d, threshold = 0.9)
  cm  <- res$corr_matrix

  expected <- stats::cor(d)
  diag(expected) <- 0 # fs_correlation sets the diagonal to diag_value (0)
  expect_equal(cm, expected)

  expect_equal(unname(cm["a", "b"]), 1)
  # BOTH members of the high-correlation pair are reported
  expect_setequal(res$selected_vars, c("a", "b"))
})

test_that("spearman and kendall run and respect monotone association", {
  d <- data.frame(a = c(1, 3, 2, 5, 4, 6), b = c(2, 6, 4, 10, 8, 12))
  res_s <- fs_correlation(d, threshold = 0.9, method = "spearman")
  expect_equal(unname(res_s$corr_matrix["a", "b"]), 1)
  expect_setequal(res_s$selected_vars, c("a", "b"))
  res_k <- fs_correlation(d, threshold = 0.9, method = "kendall")
  expect_equal(unname(res_k$corr_matrix["a", "b"]), 1)
})

test_that("output_format, diag_value and the no-match message behave as documented", {
  d <- data.frame(a = as.numeric(1:10), b = rep(c(5, -5), 5))

  expect_message(
    res <- fs_correlation(d, threshold = 0.99),
    "No variables meet the correlation threshold"
  )
  expect_identical(res$selected_vars, character(0))

  expect_message(
    fs_correlation(d, threshold = 0.99, no_vars_message = "nothing above cutoff"),
    "nothing above cutoff"
  )

  d2 <- data.frame(a = as.numeric(1:10), b = as.numeric(1:10) * 3)
  res_df <- fs_correlation(d2, threshold = 0.9, output_format = "data.frame")
  expect_s3_class(res_df$corr_matrix, "data.frame")
  expect_identical(names(res_df$corr_matrix), c("Var1", "Var2", "Correlation"))
  expect_identical(nrow(res_df$corr_matrix), 4L) # 2 x 2 entries, long format
  expect_setequal(res_df$selected_vars, c("a", "b"))

  res_diag <- fs_correlation(d2, threshold = 0.9, diag_value = NA_real_)
  expect_true(all(is.na(diag(res_diag$corr_matrix))))
})

test_that("NA correlations warn under na.rm = FALSE and resolve under na.rm = TRUE", {
  d <- data.frame(
    a = c(NA, 2:10),
    b = as.numeric(1:10),
    c = as.numeric(1:10) * 2
  )

  expect_warning(
    res <- fs_correlation(d, threshold = 0.9),
    "can never be selected"
  )
  # the complete pair is still found; pairs with NA correlations are not
  expect_setequal(res$selected_vars, c("b", "c"))
  expect_true(anyNA(res$corr_matrix))

  expect_silent(res2 <- fs_correlation(d, threshold = 0.9, na.rm = TRUE))
  expect_setequal(res2$selected_vars, c("a", "b", "c"))
  expect_false(anyNA(res2$corr_matrix))
})

test_that("pointbiserial warns in ASCII when no continuous-dichotomous pairs exist", {
  d <- data.frame(a = as.numeric(1:8), b = as.numeric((1:8)^2))

  warns <- capture_warnings(
    suppressMessages(
      res <- fs_correlation(d, threshold = 0.3, method = "pointbiserial")
    )
  )
  # exact ASCII hyphen wording (the legacy message used a non-ASCII dash)
  expect_true(any(grepl("No valid continuous-dichotomous pairs", warns,
                        fixed = TRUE)))

  off_diag <- res$corr_matrix[row(res$corr_matrix) != col(res$corr_matrix)]
  expect_true(all(is.na(off_diag)))
  expect_identical(res$selected_vars, character(0))
})

test_that("sampling with a seed is reproducible and leaves the RNG state alone", {
  d <- data.frame(
    a = as.numeric(1:40),
    b = as.numeric(1:40) * 2,
    c = rep(c(1.25, -0.75, 0.5, 2.0), 10)
  )

  set.seed(1)
  invisible(stats::runif(1))
  state_before <- .Random.seed

  res1 <- fs_correlation(d, threshold = 0.9, sample_frac = 0.5, seed = 42)
  expect_identical(.Random.seed, state_before)

  res2 <- fs_correlation(d, threshold = 0.9, sample_frac = 0.5, seed = 42)
  expect_identical(res1$corr_matrix, res2$corr_matrix)
  expect_identical(res1$selected_vars, res2$selected_vars)
  expect_identical(dim(res1$corr_matrix), c(3L, 3L))
})

test_that("polychoric with NAs and na.rm = FALSE errors informatively", {
  skip_if_not_installed("polycor")

  d <- data.frame(
    o1 = factor(c("l", "m", "h", NA, "m", "l"),
                levels = c("l", "m", "h"), ordered = TRUE),
    o2 = factor(c("l", "l", "h", "m", "h", "m"),
                levels = c("l", "m", "h"), ordered = TRUE)
  )
  expect_error(
    fs_correlation(d, threshold = 0.5, method = "polychoric"),
    "cannot propagate NAs"
  )
})

test_that("polychoric runs on complete ordered factors", {
  skip_if_not_installed("polycor")
  skip_on_cran()

  # Deterministic contingency table with a strong positive association and
  # every cell populated (polychor's ML estimate fails to converge, returning
  # NA, when cells are empty).
  counts <- matrix(c(20, 5, 1,
                     5, 20, 5,
                     1, 5, 20), nrow = 3, byrow = TRUE)
  cell_n <- as.vector(t(counts))                     # row-major cell counts
  o1v <- rep(rep(1:3, each = 3), times = cell_n)     # row index
  o2v <- rep(rep(1:3, times = 3), times = cell_n)    # column index
  d <- data.frame(
    o1 = factor(o1v, levels = 1:3, ordered = TRUE),
    o2 = factor(o2v, levels = 1:3, ordered = TRUE)
  )

  res <- suppressWarnings(
    fs_correlation(d, threshold = 0.5, method = "polychoric")
  )
  cm <- res$corr_matrix
  expect_identical(dim(cm), c(2L, 2L))
  expect_identical(rownames(cm), c("o1", "o2"))
  expect_gt(cm["o1", "o2"], 0.5)
  expect_setequal(res$selected_vars, c("o1", "o2"))
})
