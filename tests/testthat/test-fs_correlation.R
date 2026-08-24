# Tests for fs_correlation().
#
# pearson/spearman/kendall and the sequential pointbiserial path rely only
# on base stats, so most tests run everywhere. Only the polychoric method
# needs the suggested polycor package; validation (including the ordered-
# factor type check) runs before fs_require("polycor"), so it never skips.
# Pruning is implemented with base stats, so it needs no caret.

corr_detail_names <- c("corr_matrix", "pairs", "dropped", "redundant",
                       "n_features")

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
  expect_error(fs_correlation(d, 0.5, prune = "yes"),
               "'prune' must be TRUE or FALSE")
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

test_that("fs_correlation signature and defaults are stable", {
  fx <- formals(fs_correlation)
  expect_identical(
    names(fx),
    c("data", "threshold", "method", "prune", "na.rm", "sample_frac",
      "output_format", "diag_value", "seed", "verbose", "parallel", "n_cores")
  )
  expect_identical(fx$method, "pearson")
  expect_identical(fx$prune, TRUE)
  expect_identical(fx$na.rm, FALSE)
  expect_identical(fx$sample_frac, 1)
  expect_identical(fx$output_format, "matrix")
  expect_identical(fx$diag_value, 0)
  expect_null(fx$seed)
  expect_identical(fx$verbose, FALSE)
  expect_identical(fx$parallel, FALSE)
  expect_identical(fx$n_cores, 2L)
})

test_that("fs_correlation returns an fs_result with the documented details", {
  d <- data.frame(
    a = as.numeric(1:20),
    b = as.numeric(1:20) * 2 + 1, # perfectly correlated with a
    c = rep(c(5, -5), 10)         # essentially uncorrelated with either
  )

  res <- fs_correlation(d, threshold = 0.9)

  expect_s3_class(res, "fs_result")
  expect_identical(res$method, "correlation_pearson")
  expect_true(is.na(res$task))
  expect_null(res$model)
  expect_false(is.null(res$call))
  expect_named(res$details, corr_detail_names)
  expect_identical(res$details$n_features, 3L)

  # scores: strongest absolute correlation with any other variable
  expect_true(is.numeric(res$scores))
  expect_setequal(names(res$scores), c("a", "b", "c"))
  expect_equal(unname(res$scores[["a"]]), 1)
  expect_equal(unname(res$scores[["b"]]), 1)
  expect_lt(res$scores[["c"]], 0.9)

  expect_output(print(res), "correlation_pearson")

  # method name tracks the correlation method
  res_s <- fs_correlation(d, threshold = 0.9, method = "spearman")
  expect_identical(res_s$method, "correlation_spearman")
})

test_that("prune keeps one member of a perfectly correlated pair", {
  d <- data.frame(
    a = as.numeric(1:20),
    b = as.numeric(1:20) * 2 + 1, # perfectly correlated with a
    c = rep(c(5, -5), 10)         # essentially uncorrelated with either
  )

  res_prune <- fs_correlation(d, threshold = 0.9)                 # prune = TRUE
  res_keep  <- fs_correlation(d, threshold = 0.9, prune = FALSE)

  # exactly one member of the redundant pair survives pruning ...
  expect_identical(sum(c("a", "b") %in% res_prune$selected), 1L)
  expect_length(res_prune$details$dropped, 1L)
  expect_true(res_prune$details$dropped %in% c("a", "b"))
  # ... and a variable that was never flagged is always retained
  expect_true("c" %in% res_prune$selected)
  expect_false(res_prune$details$dropped %in% res_prune$selected)

  # prune = FALSE is the legacy behaviour: both members, and nothing dropped
  expect_setequal(res_keep$selected, c("a", "b"))
  expect_identical(res_keep$details$dropped, character(0))

  # the unpruned pair-member set is reported either way
  expect_setequal(res_prune$details$redundant, c("a", "b"))
  expect_setequal(res_keep$details$redundant, c("a", "b"))
})

test_that("prune keeps the member with the lowest mean absolute correlation", {
  # x1 and x2 are a redundant pair (|r| = 0.94). x2 is the more connected
  # member: it correlates more strongly with x3 than x1 does, so x2 carries
  # the higher mean absolute correlation and is the one dropped.
  x1 <- as.numeric(1:12)
  x3 <- rep(c(1, -1), 6)
  d  <- data.frame(x1 = x1, x2 = x1 + 1.2 * x3, x3 = x3)

  cm <- abs(stats::cor(d))
  expect_gt(cm["x1", "x2"], 0.9)            # the pair is flagged
  expect_lt(cm["x1", "x3"], 0.9)            # nothing else is
  expect_lt(cm["x2", "x3"], 0.9)
  expect_gt(cm["x2", "x3"], cm["x1", "x3"]) # x2 is the more connected member

  res <- fs_correlation(d, threshold = 0.9)
  expect_identical(res$details$dropped, "x2")
  expect_setequal(res$selected, c("x1", "x3"))
  expect_setequal(res$details$redundant, c("x1", "x2"))
})

test_that("fs_correlation accepts the inclusive threshold bounds", {
  d <- data.frame(a = c(1, 2, 3, 4), b = c(1.5, 0.5, 2.5, 1.0))

  # threshold = 1 is valid; |r| > 1 can never happen, so nothing is flagged
  expect_message(res <- fs_correlation(d, threshold = 1), "No variables meet")
  expect_identical(res$details$redundant, character(0))
  expect_identical(res$details$dropped, character(0))
  expect_identical(nrow(res$details$pairs), 0L)
  # nothing is redundant, so pruning keeps every variable
  expect_setequal(res$selected, c("a", "b"))

  # without pruning the selected set is the (empty) flagged set
  res_keep <- suppressMessages(fs_correlation(d, threshold = 1, prune = FALSE))
  expect_identical(res_keep$selected, character(0))
})

test_that("pointbiserial sign matches the direction of association", {
  # x increases with the SECOND factor level of y, so the point-biserial
  # correlation must come out positive.
  y01 <- rep(c(0, 1), each = 30)
  x   <- y01 * 2 + rep(c(-0.10, 0.05, 0.12, -0.03, 0.08, -0.06), 10)
  d_fac <- data.frame(x = x, y = factor(ifelse(y01 == 1, "b", "a")))

  res <- fs_correlation(d_fac, threshold = 0.5, method = "pointbiserial",
                        prune = FALSE)
  cm  <- res$details$corr_matrix

  expect_identical(res$method, "correlation_pointbiserial")
  expect_true(is.matrix(cm))
  expect_identical(dimnames(cm), list(c("x", "y"), c("x", "y")))
  expect_gt(cm["x", "y"], 0)
  expect_identical(cm["x", "y"], cm["y", "x"])
  # known answer: point-biserial r IS the Pearson r against the 0/1 indicator
  expect_equal(unname(cm["x", "y"]), stats::cor(x, y01))
  expect_setequal(res$selected, c("x", "y"))

  # same positive sign when the dichotomous variable is numeric 0/1
  d_num <- data.frame(x = x, y = y01)
  res_num <- fs_correlation(d_num, threshold = 0.5, method = "pointbiserial",
                            prune = FALSE)
  expect_gt(res_num$details$corr_matrix["x", "y"], 0)
  expect_equal(unname(res_num$details$corr_matrix["x", "y"]),
               stats::cor(x, y01))
})

test_that("pearson correlations equal stats::cor() and pairs report both members", {
  d <- data.frame(
    a = as.numeric(1:20),
    b = as.numeric(1:20) * 2 + 1, # perfectly correlated with a
    c = rep(c(5, -5), 10)         # essentially uncorrelated with either
  )

  res <- fs_correlation(d, threshold = 0.9, prune = FALSE)
  cm  <- res$details$corr_matrix

  expected <- stats::cor(d)
  diag(expected) <- 0 # fs_correlation sets the diagonal to diag_value (0)
  expect_equal(cm, expected)

  expect_equal(unname(cm["a", "b"]), 1)
  # with prune = FALSE BOTH members of the high-correlation pair are reported
  expect_setequal(res$selected, c("a", "b"))

  # the flagged pair table names both members and their correlation
  pairs <- res$details$pairs
  expect_s3_class(pairs, "data.frame")
  expect_identical(names(pairs), c("Var1", "Var2", "Correlation"))
  expect_identical(nrow(pairs), 1L)
  expect_setequal(c(pairs$Var1, pairs$Var2), c("a", "b"))
  expect_equal(pairs$Correlation, 1)
})

test_that("spearman and kendall run and respect monotone association", {
  d <- data.frame(a = c(1, 3, 2, 5, 4, 6), b = c(2, 6, 4, 10, 8, 12))
  res_s <- fs_correlation(d, threshold = 0.9, method = "spearman",
                          prune = FALSE)
  expect_equal(unname(res_s$details$corr_matrix["a", "b"]), 1)
  expect_setequal(res_s$selected, c("a", "b"))
  res_k <- fs_correlation(d, threshold = 0.9, method = "kendall",
                          prune = FALSE)
  expect_equal(unname(res_k$details$corr_matrix["a", "b"]), 1)
})

test_that("output_format, diag_value and the no-match message behave as documented", {
  d <- data.frame(a = as.numeric(1:10), b = rep(c(5, -5), 5))

  expect_message(
    res <- fs_correlation(d, threshold = 0.99),
    "No variables meet the correlation threshold"
  )
  expect_identical(res$details$redundant, character(0))

  res_keep <- suppressMessages(
    fs_correlation(d, threshold = 0.99, prune = FALSE)
  )
  expect_identical(res_keep$selected, character(0))

  d2 <- data.frame(a = as.numeric(1:10), b = as.numeric(1:10) * 3)
  res_df <- fs_correlation(d2, threshold = 0.9, output_format = "data.frame")
  cm_df <- res_df$details$corr_matrix
  expect_s3_class(cm_df, "data.frame")
  expect_identical(names(cm_df), c("Var1", "Var2", "Correlation"))
  expect_identical(nrow(cm_df), 4L) # 2 x 2 entries, long format
  expect_setequal(res_df$details$redundant, c("a", "b"))
  # the pair table is unaffected by output_format
  expect_identical(nrow(res_df$details$pairs), 1L)

  res_diag <- fs_correlation(d2, threshold = 0.9, diag_value = NA_real_)
  expect_true(all(is.na(diag(res_diag$details$corr_matrix))))
})

test_that("NA correlations warn under na.rm = FALSE and resolve under na.rm = TRUE", {
  d <- data.frame(
    a = c(NA, 2:10),
    b = as.numeric(1:10),
    c = as.numeric(1:10) * 2
  )

  expect_warning(
    res <- fs_correlation(d, threshold = 0.9, prune = FALSE),
    "can never be selected"
  )
  # the complete pair is still found; pairs with NA correlations are not
  expect_setequal(res$selected, c("b", "c"))
  expect_true(anyNA(res$details$corr_matrix))
  # an incomputable correlation scores NA rather than 0
  expect_equal(unname(res$scores[["b"]]), 1)

  expect_silent(res2 <- fs_correlation(d, threshold = 0.9, na.rm = TRUE,
                                       prune = FALSE))
  expect_setequal(res2$selected, c("a", "b", "c"))
  expect_false(anyNA(res2$details$corr_matrix))
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

  cm <- res$details$corr_matrix
  off_diag <- cm[row(cm) != col(cm)]
  expect_true(all(is.na(off_diag)))
  expect_identical(res$details$redundant, character(0))
  # an all-NA matrix can never make anything redundant, so nothing is dropped
  expect_identical(res$details$dropped, character(0))
  expect_true(all(is.na(res$scores)))
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
  expect_identical(res1$details$corr_matrix, res2$details$corr_matrix)
  expect_identical(res1$selected, res2$selected)
  expect_identical(dim(res1$details$corr_matrix), c(3L, 3L))
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
    fs_correlation(d, threshold = 0.5, method = "polychoric", prune = FALSE)
  )
  cm <- res$details$corr_matrix
  expect_identical(res$method, "correlation_polychoric")
  expect_identical(dim(cm), c(2L, 2L))
  expect_identical(rownames(cm), c("o1", "o2"))
  expect_gt(cm["o1", "o2"], 0.5)
  expect_setequal(res$selected, c("o1", "o2"))
})
