# Tests for fs_pca(). The decomposition runs through stats::prcomp() for
# anything under 1e7 cells, and the tidying only needs data.table (an Import),
# so every test below runs unconditionally except the blocks that build a
# ggplot, which are gated on the 'ggplot2' Suggest. The bigstatsr branch is
# unreachable with fixtures this small.
#
# Post-unification conventions exercised here: verbose defaults to FALSE and is
# the last argument, and plot defaults to FALSE unconditionally (it no longer
# keys off label_col). plot now governs printing only; the ggplot object is
# attached to $plot whenever one can be built.

# Deterministic 12-row toy set (no RNG). All four columns are numeric with
# non-zero variance and the matrix has full rank 4: 'x2' is a doubled 'x1'
# nudged by an alternating +/-1, 'x3' runs backwards with a period-4 wobble,
# and 'x4' is quadratic, so none of them is an exact linear combination of the
# others and every eigenvalue of the correlation matrix is positive.
pca_toy <- function() {
  i <- seq_len(12)
  data.frame(
    x1 = as.numeric(i),
    x2 = 2 * i + c(1, -1),
    x3 = rev(i) + c(0, 2, -2, 1),
    x4 = (i - 6)^2
  )
}

test_that("fs_pca returns the documented structure with named loadings", {
  res <- fs_pca(pca_toy(), num_pc = 2, verbose = FALSE)

  expect_type(res, "list")
  expect_identical(
    names(res),
    c("pc_loadings", "pc_scores", "var_explained", "pca_df", "meta")
  )
  expect_identical(
    names(res$meta),
    c("numeric_cols", "rows_kept", "n_rows_used", "n_cols_used")
  )

  expect_true(is.matrix(res$pc_loadings))
  expect_true(is.matrix(res$pc_scores))
  expect_identical(dim(res$pc_loadings), c(4L, 2L))
  expect_identical(dim(res$pc_scores), c(12L, 2L))

  # loadings are indexed by the numeric feature names, scores by observation
  expect_identical(rownames(res$pc_loadings), c("x1", "x2", "x3", "x4"))
  expect_identical(colnames(res$pc_loadings), c("PC1", "PC2"))
  expect_identical(colnames(res$pc_scores), c("PC1", "PC2"))
  expect_identical(res$meta$numeric_cols, c("x1", "x2", "x3", "x4"))
  expect_identical(res$meta$n_cols_used, 4L)
  expect_identical(res$meta$n_rows_used, 12L)
  expect_identical(res$meta$rows_kept, rep(TRUE, 12))

  # no labels to carry, so pca_df is just the scores
  expect_s3_class(res$pca_df, "data.table")
  expect_identical(names(res$pca_df), c("PC1", "PC2"))
  expect_identical(nrow(res$pca_df), 12L)
})

test_that("var_explained has one entry per retained PC, each in (0, 1]", {
  res <- fs_pca(pca_toy(), num_pc = 3, verbose = FALSE)

  expect_length(res$var_explained, 3L)
  expect_true(is.numeric(res$var_explained))
  expect_true(all(res$var_explained > 0))
  expect_true(all(res$var_explained <= 1))
  # a subset of the total variance can never exceed the whole
  expect_lte(sum(res$var_explained), 1 + 1e-8)
  # components come back in decreasing order of variance
  expect_false(is.unsorted(rev(res$var_explained)))
})

test_that("loadings and variance shares match prcomp/eigen on the same data", {
  d <- pca_toy()
  res <- fs_pca(d, num_pc = 2, verbose = FALSE)

  ref <- stats::prcomp(as.matrix(d), center = TRUE, scale. = TRUE)

  # eigenvector signs are arbitrary, so magnitudes are the invariant part
  expect_equal(unname(abs(res$pc_loadings)),
               unname(abs(ref$rotation[, 1:2, drop = FALSE])))
  expect_equal(unname(abs(res$pc_scores)),
               unname(abs(ref$x[, 1:2, drop = FALSE])))
  expect_equal(res$var_explained, (ref$sdev^2 / sum(ref$sdev^2))[1:2])

  # independent known answer: on centered/scaled data the PCA variances are
  # the eigenvalues of the correlation matrix, which sum to the column count
  eig <- eigen(stats::cor(as.matrix(d)), symmetric = TRUE,
               only.values = TRUE)$values
  expect_equal(sum(eig), 4)
  expect_equal(res$var_explained, eig[1:2] / 4)
})

test_that("scale_data = FALSE is passed through to the decomposition", {
  d <- pca_toy()
  res <- fs_pca(d, num_pc = 2, scale_data = FALSE, verbose = FALSE)
  ref <- stats::prcomp(as.matrix(d), center = TRUE, scale. = FALSE)

  expect_equal(res$var_explained, (ref$sdev^2 / sum(ref$sdev^2))[1:2])
  expect_equal(unname(abs(res$pc_loadings)),
               unname(abs(ref$rotation[, 1:2, drop = FALSE])))
  # unscaled, the quadratic column dominates, so PC1 leans on x4
  expect_identical(rownames(res$pc_loadings)[which.max(abs(res$pc_loadings[, 1]))],
                   "x4")
})

test_that("two perfectly correlated columns put all variance on PC1", {
  a <- c(1, 2, 3, 4, 5, 6, 7, 8)
  d <- data.frame(a = a, b = 2 * a + 1)

  res <- fs_pca(d, verbose = FALSE)

  # scaled, the two columns are the same vector: eigenvalues are 2 and 0
  expect_length(res$var_explained, 2L)
  expect_equal(res$var_explained[1L], 1, tolerance = 1e-8)
  expect_lt(res$var_explained[2L], 1e-8)
  # so PC1 must be (1, 1) / sqrt(2) up to sign
  expect_equal(unname(abs(res$pc_loadings[, "PC1"])),
               rep(1 / sqrt(2), 2), tolerance = 1e-8)
})

test_that("num_pc = NULL resolves to min(2, max_possible)", {
  # four usable columns and twelve rows: the default keeps two
  res <- fs_pca(pca_toy(), verbose = FALSE)
  expect_length(res$var_explained, 2L)
  expect_identical(ncol(res$pc_scores), 2L)
  expect_identical(ncol(res$pc_loadings), 2L)

  # a single numeric column caps max_possible at one; that lone component
  # trivially carries all of the variance
  one <- fs_pca(data.frame(x = c(1, 3, 2, 7, 5, 4)), verbose = FALSE)
  expect_length(one$var_explained, 1L)
  expect_equal(one$var_explained, 1)
  expect_identical(dim(one$pc_loadings), c(1L, 1L))
  expect_identical(rownames(one$pc_loadings), "x")
  expect_identical(colnames(one$pc_scores), "PC1")
  expect_identical(nrow(one$pc_scores), 6L)
  expect_equal(unname(abs(one$pc_loadings[1L, 1L])), 1)

  # with only two rows, nrow - 1 is the binding constraint
  two_rows <- fs_pca(data.frame(a = c(1, 2), b = c(3, 5), c = c(2, 9)),
                     verbose = FALSE)
  expect_length(two_rows$var_explained, 1L)
  expect_identical(nrow(two_rows$pc_scores), 2L)
})

test_that("character and factor columns are excluded but kept as labels", {
  d <- pca_toy()
  d$grp <- rep(c("a", "b", "c"), 4)
  d$tier <- factor(rep(c("lo", "hi"), 6))

  # verbose defaults to FALSE now, so the narration has to be asked for
  msgs <- capture_messages(res <- fs_pca(d, num_pc = 2, verbose = TRUE))
  all_msgs <- paste(msgs, collapse = "")
  expect_match(
    all_msgs,
    paste("Excluding 2 non-numeric column(s) from the PCA features",
          "(kept as label candidates): grp, tier"),
    fixed = TRUE
  )
  expect_match(all_msgs, "Using prcomp for PCA computation.", fixed = TRUE)

  expect_identical(res$meta$numeric_cols, c("x1", "x2", "x3", "x4"))
  expect_identical(rownames(res$pc_loadings), c("x1", "x2", "x3", "x4"))
  # the excluded columns come back attached to the scores
  expect_identical(names(res$pca_df), c("PC1", "PC2", "grp", "tier"))
  expect_identical(res$pca_df$grp, d$grp)
  expect_identical(res$pca_df$tier, d$tier)

  # the same call is silent at the default verbose = FALSE
  quiet <- expect_silent(fs_pca(d, num_pc = 2))
  expect_identical(quiet$meta$numeric_cols, res$meta$numeric_cols)
})

test_that("an explicit numeric label_col is excluded from the features", {
  d <- pca_toy()
  # a numeric grouping code, kept under 10 distinct values so that building the
  # (unprinted) ggplot does not trip pca_plot()'s "more than 9 labels" warning
  d$id <- as.numeric(rep(101:104, 3))

  res <- fs_pca(d, num_pc = 2, label_col = "id", plot = FALSE, verbose = FALSE)

  expect_false("id" %in% res$meta$numeric_cols)
  expect_identical(res$meta$numeric_cols, c("x1", "x2", "x3", "x4"))
  expect_true("id" %in% names(res$pca_df))
  expect_identical(res$pca_df$id, d$id)
  # identical to the run without the extra column, which is the whole point
  expect_equal(res$var_explained,
               fs_pca(pca_toy(), num_pc = 2, verbose = FALSE)$var_explained)
})

test_that("rows with missing numeric values are dropped and stay aligned", {
  d <- pca_toy()
  d$grp <- rep(c("a", "b", "c"), 4)
  d$x2[c(2, 5)] <- NA

  res <- fs_pca(d, num_pc = 2, verbose = FALSE)

  expect_identical(sum(res$meta$rows_kept), 10L)
  expect_identical(res$meta$n_rows_used, 10L)
  expect_false(res$meta$rows_kept[2L])
  expect_false(res$meta$rows_kept[5L])
  expect_identical(nrow(res$pc_scores), 10L)
  expect_identical(nrow(res$pca_df), 10L)
  # labels follow the surviving rows, not the original ones
  expect_identical(res$pca_df$grp, d$grp[res$meta$rows_kept])
})

test_that("zero-variance columns are dropped with a counted warning", {
  d <- pca_toy()
  d$const <- 5

  expect_warning(res <- fs_pca(d, num_pc = 2, verbose = FALSE),
                 "Removed 1 zero-variance column\\(s\\): const")
  expect_identical(res$meta$numeric_cols, c("x1", "x2", "x3", "x4"))
  expect_identical(res$meta$n_cols_used, 4L)
  expect_false("const" %in% rownames(res$pc_loadings))
})

test_that("fs_pca validates data, num_pc and label_col", {
  d <- pca_toy()

  expect_error(fs_pca("nope"), "'data' must be a data\\.frame")
  expect_error(fs_pca(data.frame()), "at least one row and one column")
  expect_error(fs_pca(data.frame(x = 1, y = 2)),
               "Invalid data for PCA: need at least 2 rows")
  expect_error(fs_pca(data.frame(a = c("x", "y"), b = c("p", "q"))),
               "Invalid data for PCA: need at least 1 numeric column")

  expect_error(fs_pca(d, num_pc = 0), "'num_pc' must be between 1")
  expect_error(fs_pca(d, num_pc = 2.5), "'num_pc' must be a whole number")
  expect_error(fs_pca(d, num_pc = "two"), "'num_pc' must be a single finite number")
  expect_error(fs_pca(d, num_pc = 5, verbose = FALSE),
               "Requested 5 PCs, but at most 4 can be computed from the data")

  expect_error(fs_pca(d, scale_data = NA), "'scale_data' must be TRUE or FALSE")
  expect_error(fs_pca(d, center_data = "yes"), "'center_data' must be TRUE or FALSE")
  expect_error(fs_pca(d, verbose = NA), "'verbose' must be TRUE or FALSE")

  # Scaling without centering is rejected because stats::prcomp() and
  # bigstatsr::big_scale() disagree about it: the latter silently ignores the
  # request, so the decomposition would depend on the size of the input.
  expect_error(fs_pca(d, scale_data = TRUE, center_data = FALSE),
               "not supported")
  # Either half alone is fine.
  expect_silent(fs_pca(d, scale_data = FALSE, center_data = FALSE))
  expect_silent(fs_pca(d, scale_data = TRUE, center_data = TRUE))
  expect_error(fs_pca(d, label_col = "absent"),
               "Column 'absent' not found in 'data'")
  expect_error(fs_pca(d, label_col = 1), "'label_col' must be a single non-empty")

  # the only numeric column doubles as the label, leaving nothing to decompose
  expect_error(
    fs_pca(data.frame(x = c(1, 2, 3, 4), g = c("a", "b", "a", "b")),
           label_col = "x", plot = FALSE, verbose = FALSE),
    "No numeric columns found for PCA after excluding label columns"
  )
})

test_that("fs_pca signature and defaults are stable", {
  fx <- formals(fs_pca)
  expect_identical(
    names(fx),
    c("data", "num_pc", "scale_data", "center_data", "label_col", "plot",
      "verbose")
  )
  expect_null(fx$num_pc)
  expect_null(fx$label_col)
  expect_identical(fx$scale_data, TRUE)
  expect_identical(fx$center_data, TRUE)
  # plot is now an unconditional FALSE, not the old !is.null(label_col)
  expect_identical(fx$plot, FALSE)
  expect_false(is.language(fx$plot))
  # verbose defaults to FALSE and is the last argument
  expect_identical(fx$verbose, FALSE)
  expect_identical(names(fx)[length(fx)], "verbose")
})

test_that("plot defaults to FALSE and the default call prints nothing", {
  # no messages, no warnings, and nothing sent to the console
  res <- expect_silent(fs_pca(pca_toy(), num_pc = 2))
  expect_false("plot" %in% names(res))

  # The default no longer keys off label_col, so supplying one does not turn
  # printing on. Whether $plot is attached depends on 'ggplot2' being
  # installed, so only the five always-present components are asserted here;
  # the ggplot2-gated block below covers $plot itself.
  d <- pca_toy()
  d$grp <- rep(c("a", "b", "c"), 4)
  res_lab <- fs_pca(d, num_pc = 2, label_col = "grp")
  expect_identical(
    names(res_lab)[1:5],
    c("pc_loadings", "pc_scores", "var_explained", "pca_df", "meta")
  )
  expect_identical(res_lab$meta$numeric_cols, c("x1", "x2", "x3", "x4"))
})

test_that("plot = TRUE warns and skips when it cannot draw", {
  # both branches happen before pca_plot(), so neither needs ggplot2
  expect_warning(res <- fs_pca(pca_toy(), num_pc = 2, plot = TRUE, verbose = FALSE),
                 "plot = TRUE but no 'label_col' provided")
  expect_false("plot" %in% names(res))

  one_pc <- data.frame(x = c(1, 3, 2, 7, 5, 4), g = rep(c("a", "b"), 3))
  expect_warning(res1 <- fs_pca(one_pc, label_col = "g", plot = TRUE,
                                verbose = FALSE),
                 "plot = TRUE requires at least 2 principal components")
  expect_false("plot" %in% names(res1))
  expect_length(res1$var_explained, 1L)

  # and with the new default that same call is quiet and draws nothing
  res2 <- expect_silent(fs_pca(one_pc, label_col = "g"))
  expect_false("plot" %in% names(res2))
})

test_that("the ggplot object is returned in $plot whether or not plot = TRUE", {
  skip_if_not_installed("ggplot2")

  d <- pca_toy()
  d$grp <- rep(c("a", "b", "c"), 4)

  # plot = FALSE (the default) still builds and returns the object
  res <- fs_pca(d, num_pc = 2, label_col = "grp")
  expect_s3_class(res$plot, "ggplot")
  expect_identical(
    names(res),
    c("pc_loadings", "pc_scores", "var_explained", "pca_df", "meta", "plot")
  )
  expect_true("grp" %in% names(res$pca_df))

  # plot = TRUE additionally prints it, so send that to a throwaway device
  pdf_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_file)
  withr::defer({
    grDevices::dev.off()
    unlink(pdf_file)
  })

  printed <- fs_pca(d, num_pc = 2, label_col = "grp", plot = TRUE,
                    verbose = FALSE)
  expect_s3_class(printed$plot, "ggplot")
  expect_identical(names(printed), names(res))
  expect_equal(printed$var_explained, res$var_explained)
})

test_that("fs_pca leaves the caller's RNG state untouched", {
  set.seed(20260809)
  rng_before <- .Random.seed
  invisible(fs_pca(pca_toy(), num_pc = 2, verbose = FALSE))
  expect_identical(.Random.seed, rng_before)
})
