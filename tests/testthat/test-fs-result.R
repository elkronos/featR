# summary.fs_result(): ranking and selected marks.

summary_table <- function(res, n = 20L) {
  out <- utils::capture.output(summary(res, n = n))
  start <- grep("^Scores \\(", out)
  rows <- out[(start + 2L):length(out)]
  rows <- rows[!grepl("^\\s*\\.\\.\\.|^\\(\\* = selected\\)", rows)]
  parts <- strsplit(trimws(rows), "\\s+")
  data.frame(
    feature = vapply(parts, `[`, "", 1L),
    selected = vapply(parts, function(p) length(p) == 3L && p[3L] == "*",
                      logical(1L)),
    stringsAsFactors = FALSE
  )
}

test_that("selected marks follow the keep-mask when feature names repeat", {
  # REGRESSION: rows were marked with names %in% selected, so when one of two
  # same-named features was kept, both were starred.
  x <- data.table::data.table(first = c(0, 1, 0, 1), second = c(1, 2, 3, 4),
                              y = c(1, 2, 3, 4))
  data.table::setnames(x, c("dup", "dup", "y"))
  res <- fs_supervised(x, "y", method = "correlation", threshold = 0.5)
  tab <- summary_table(res)
  expect_identical(tab$feature, c("dup", "dup"))
  expect_identical(tab$selected, c(TRUE, FALSE))
})

test_that("fs_correlation(prune = TRUE) lists the kept, least redundant variables first", {
  # REGRESSION: scores are max |r| (larger = more redundant) but were sorted
  # decreasing, so the independent kept variables were truncated out.
  set.seed(1)
  n <- 200
  z <- matrix(rnorm(n * 4), n)
  w <- z + matrix(rnorm(n * 4, sd = 0.1), n)
  u <- matrix(rnorm(n * 4), n)
  d <- data.frame(z, w, u)
  names(d) <- c(paste0("p", 1:4), paste0("q", 1:4), paste0("ind", 1:4))
  res <- fs_correlation(d, threshold = 0.9)
  tab <- summary_table(res, n = 8L)
  expect_true(all(tab$selected))
  expect_identical(sort(tab$feature[1:4]), paste0("ind", 1:4))
  # prune = FALSE selects the redundant set, which still comes first.
  res2 <- fs_correlation(d, threshold = 0.9, prune = FALSE)
  tab2 <- summary_table(res2, n = 8L)
  expect_true(all(tab2$selected))
  expect_false(any(grepl("^ind", tab2$feature)))
})

test_that("a selected fs_mars predictor with negative importance is not ranked below pruned ones", {
  # REGRESSION: fs_mars() selects every predictor in the final model, which
  # can carry a negative GCV importance while pruned ones score 0; sorting by
  # score alone listed it last.
  res <- new_fs_result(
    selected = c("v1", "v5", "v8"),
    scores = c(v1 = 100, v2 = 0, v3 = 0, v5 = 38.17, v8 = -4.049),
    method = "mars", task = "regression"
  )
  tab <- summary_table(res)
  expect_identical(tab$feature, c("v1", "v5", "v8", "v2", "v3"))
  expect_identical(tab$selected, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  # Truncation keeps the selected predictor.
  expect_true("v8" %in% summary_table(res, n = 3L)$feature)
})

test_that("ordinary rankings are unchanged", {
  res <- new_fs_result(selected = c("a", "c"),
                       scores = c(a = 2, b = -1, c = 3, d = NA),
                       method = "randomforest", task = "regression")
  expect_identical(summary_table(res)$feature, c("c", "a", "b", "d"))
  lasso <- new_fs_result(selected = c("a", "b"),
                         scores = c(a = 0.5, b = -2, c = 0),
                         method = "lasso", task = "regression")
  expect_identical(summary_table(lasso)$feature, c("b", "a", "c"))
  chi <- new_fs_result(selected = "b", scores = c(a = 0.2, b = 0.001),
                       method = "chi", task = "classification")
  expect_identical(summary_table(chi)$feature, c("b", "a"))
})
