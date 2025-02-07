###############################################################################
# Testing Infrastructure - Infogain
###############################################################################

test_fs_infogain_wrapper <- function() {
  test_that("fs_infogain handles a single data.frame with numeric predictors", {
    df1 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(1:5, 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    result1 <- fs_infogain(df1, "target")
    expect_true(all(c("Variable", "InfoGain") %in% names(result1)))
    expect_equal(nrow(result1), 2)
  })
  
  test_that("fs_infogain handles a single data.frame with categorical predictors", {
    df2 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    result2 <- fs_infogain(df2, "target")
    expect_true(all(c("Variable", "InfoGain") %in% names(result2)))
    expect_equal(nrow(result2), 2)
  })
  
  test_that("fs_infogain expands date columns in a single data.frame", {
    df3 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE),
      C = seq(as.Date("2001-01-01"), by = "month", length.out = 100),
      target = sample(1:2, 100, replace = TRUE)
    )
    result3 <- fs_infogain(df3, "target")
    expect_true(nrow(result3) >= 2)
  })
  
  test_that("fs_infogain errors when target is missing in a single data.frame", {
    df4 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE)
    )
    expect_error(fs_infogain(df4, "target"), "The target variable is not found")
  })
  
  test_that("fs_infogain handles a list of data.frames", {
    df1 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    df2 <- data.frame(
      B = sample(c("yes", "no"), 100, replace = TRUE),
      target = sample(1:3, 100, replace = TRUE)
    )
    dfs_list <- list(df1 = df1, df2 = df2)
    result <- fs_infogain(dfs_list, "target")
    expect_true(all(c("Variable", "InfoGain", "Origin") %in% names(result)))
  })
  
  test_that("fs_infogain errors when non-data.frame is included in list", {
    expect_error(fs_infogain(list(1, 2), "target"),
                 "All elements in the list must be data.frames.")
  })
  
  test_that("fs_infogain errors when target is missing in one data.frame of a list", {
    df1 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    df3 <- data.frame(
      A = sample(1:10, 100, replace = TRUE)
    )
    dfs_list_with_error <- list(df1 = df1, df3 = df3)
    expect_error(fs_infogain(dfs_list_with_error, "target"),
                 "The target variable is not found in data frame at position 2.")
  })
}

# Run the tests
# test_fs_infogain_wrapper()
