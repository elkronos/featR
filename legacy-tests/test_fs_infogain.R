###############################################################################
# Testing Infrastructure - Information Gain (fs_infogain)
###############################################################################

# Optional: if fs_infogain is in a separate script/package, load it here, e.g.:
# source("fs_infogain.R")
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
#' @param note Optional character. Additional notes.
print_and_store_result <- function(test_name, passed, note = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-70s [%s]\n", test_name, result))
  if (!is.null(note)) cat("  Note: ", note, "\n")
  test_results <<- rbind(
    test_results,
    data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)
  )
}

###############################################################################
# Tests: Existence and Input Validation
###############################################################################

test_fs_infogain_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_infogain") && is.function(fs_infogain)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_infogain: Function exists and is callable", passed, note)
}

test_fs_infogain_input_validation <- function() {
  # Case 1: data not a data.frame or list
  err1 <- NULL
  passed1 <- tryCatch({
    fs_infogain("not a data frame", "target")
    FALSE
  }, error = function(e) {
    err1 <<- e
    grepl("must be a data.frame or a list of data.frames", conditionMessage(e))
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_infogain: errors if data is not a data.frame or list",
    passed1, note1
  )
  
  # Case 2: target column missing in single data.frame
  df2 <- data.frame(A = 1:3, B = 4:6)
  err2 <- NULL
  passed2 <- tryCatch({
    fs_infogain(df2, "target")
    FALSE
  }, error = function(e) {
    err2 <<- e
    grepl("specified target.*is not found", conditionMessage(e), ignore.case = TRUE)
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_infogain: errors if target column missing in single data.frame",
    passed2, note2
  )
  
  # Case 3: non-data.frame element in list
  df3 <- data.frame(A = 1:3, target = c(0, 1, 0))
  err3 <- NULL
  passed3 <- tryCatch({
    fs_infogain(list(df3, 1), "target")
    FALSE
  }, error = function(e) {
    err3 <<- e
    grepl("All elements in 'data' must be data\\.frames\\.", conditionMessage(e))
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "fs_infogain: errors if any list element is not a data.frame",
    passed3, note3
  )
  
  # Case 4: target column missing in one data.frame of a list
  df4_ok <- data.frame(A = 1:3, target = c(1, 2, 1))
  df4_bad <- data.frame(A = 1:3)
  dfs_list <- list(df1 = df4_ok, df2 = df4_bad)
  err4 <- NULL
  passed4 <- tryCatch({
    fs_infogain(dfs_list, "target")
    FALSE
  }, error = function(e) {
    err4 <<- e
    grepl("Target 'target' not found.*position 2", conditionMessage(e))
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "fs_infogain: errors if target missing in one data.frame of a list",
    passed4, note4
  )
}

###############################################################################
# Tests: Single Data Frame - Core Behavior
###############################################################################

test_fs_infogain_single_numeric <- function() {
  set.seed(1)
  df1 <- data.frame(
    A      = sample(1:10, 100, replace = TRUE),
    B      = sample(1:5,  100, replace = TRUE),
    target = sample(1:2,  100, replace = TRUE)
  )
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_infogain(df1, "target")
    
    is_df <- is.data.frame(res)
    has_cols <- all(c("Variable", "InfoGain") %in% names(res))
    vars_ok <- identical(sort(res$Variable), sort(c("A", "B")))
    ig_num  <- is.numeric(res$InfoGain)
    ig_finite <- all(is.finite(res$InfoGain))
    ig_nonneg <- all(res$InfoGain >= 0)
    
    # IG bounded by H(Y)
    y <- factor(df1$target)
    freq <- as.numeric(table(y))
    p <- freq / sum(freq)
    H_y <- -sum(p * log2(p))
    ig_bounded <- all(res$InfoGain <= H_y + 1e-12)
    
    is_df && has_cols && vars_ok && ig_num && ig_finite && ig_nonneg && ig_bounded
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_infogain: single data.frame with numeric predictors",
    passed, note
  )
}

test_fs_infogain_single_mixed <- function() {
  set.seed(2)
  df2 <- data.frame(
    A      = sample(1:10, 100, replace = TRUE),
    B      = sample(c("yes", "no"), 100, replace = TRUE),
    target = sample(1:2,  100, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_infogain(df2, "target")
    
    is_df    <- is.data.frame(res)
    has_cols <- all(c("Variable", "InfoGain") %in% names(res))
    vars_ok  <- identical(sort(res$Variable), sort(c("A", "B")))
    ig_num   <- is.numeric(res$InfoGain)
    ig_finite <- all(is.finite(res$InfoGain))
    ig_nonneg <- all(res$InfoGain >= 0)
    
    is_df && has_cols && vars_ok && ig_num && ig_finite && ig_nonneg
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_infogain: single data.frame with mixed numeric + categorical predictors",
    passed, note
  )
}

test_fs_infogain_date_predictors <- function() {
  set.seed(3)
  df3 <- data.frame(
    A      = sample(1:10, 100, replace = TRUE),
    B      = sample(c("yes", "no"), 100, replace = TRUE),
    C      = seq(as.Date("2001-01-01"), by = "month", length.out = 100),
    target = sample(1:2,  100, replace = TRUE)
  )
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_infogain(df3, "target")
    vars <- res$Variable
    
    is_df    <- is.data.frame(res)
    has_cols <- all(c("Variable", "InfoGain") %in% names(res))
    has_C_expanded <- any(grepl("^C_", vars))
    no_C_original  <- !("C" %in% vars)
    ig_finite <- all(is.finite(res$InfoGain))
    
    is_df && has_cols && has_C_expanded && no_C_original && ig_finite
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_infogain: expands date predictors (C_year/C_month/C_day) and drops original",
    passed, note
  )
}

test_fs_infogain_constant_predictor <- function() {
  df <- data.frame(
    A      = rep(5, 100),
    B      = sample(1:3, 100, replace = TRUE),
    target = sample(0:1, 100, replace = TRUE)
  )
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_infogain(df, "target")
    row_A <- res[res$Variable == "A", , drop = FALSE]
    row_B <- res[res$Variable == "B", , drop = FALSE]
    nrow(row_A) == 1L &&
      nrow(row_B) == 1L &&
      isTRUE(all.equal(row_A$InfoGain, 0)) &&
      is.finite(row_B$InfoGain)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_infogain: constant predictor yields IG = 0",
    passed, note
  )
}

###############################################################################
# Tests: List of Data Frames
###############################################################################

test_fs_infogain_list_origin <- function() {
  set.seed(4)
  df1 <- data.frame(
    A      = sample(1:10, 100, replace = TRUE),
    target = sample(1:2,  100, replace = TRUE)
  )
  df2 <- data.frame(
    B      = sample(c("yes", "no"), 100, replace = TRUE),
    target = sample(1:3,  100, replace = TRUE)
  )
  dfs_list <- list(df1 = df1, df2 = df2)
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_infogain(dfs_list, "target")
    
    is_df    <- is.data.frame(res)
    has_cols <- all(c("Variable", "InfoGain", "Origin") %in% names(res))
    origins_ok <- all(res$Origin %in% c("df1", "df2"))
    ig_finite  <- all(is.finite(res$InfoGain))
    ig_nonneg  <- all(res$InfoGain >= 0)
    
    is_df && has_cols && origins_ok && ig_finite && ig_nonneg
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_infogain: list of data.frames returns Origin column",
    passed, note
  )
}

test_fs_infogain_list_na_names_origin <- function() {
  set.seed(5)
  df1 <- data.frame(
    A      = sample(1:10, 50, replace = TRUE),
    target = sample(0:1,  50, replace = TRUE)
  )
  df2 <- data.frame(
    B      = sample(letters[1:3], 50, replace = TRUE),
    target = sample(0:1,        50, replace = TRUE)
  )
  dfs_list <- list(df1, df2)
  names(dfs_list) <- c("df1", NA)
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_infogain(dfs_list, "target")
    origins <- unique(res$Origin)
    "df1" %in% origins && "Data_Frame_2" %in% origins
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_infogain: handles NA names in dfs_list (uses Data_Frame_i)",
    passed, note
  )
}

###############################################################################
# Tests: Discretization and NA Handling
###############################################################################

test_fs_infogain_numeric_bins_override <- function() {
  set.seed(6)
  df <- data.frame(
    X      = rnorm(200),
    Y      = runif(200, min = -5, max = 5),
    target = sample(0:1, 200, replace = TRUE)
  )
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_infogain(df, "target", numeric_bins = 3L)
    
    is_df    <- is.data.frame(res)
    has_cols <- all(c("Variable", "InfoGain") %in% names(res))
    vars_ok  <- identical(sort(res$Variable), sort(c("X", "Y")))
    ig_finite <- all(is.finite(res$InfoGain))
    ig_nonneg <- all(res$InfoGain >= 0)
    
    is_df && has_cols && vars_ok && ig_finite && ig_nonneg
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_infogain: numeric_bins override accepted and IG finite",
    passed, note
  )
}

test_fs_infogain_na_handling <- function() {
  set.seed(7)
  n <- 150
  df <- data.frame(
    A      = sample(1:10, n, replace = TRUE),
    B      = sample(c("yes", "no", NA), n, replace = TRUE, prob = c(0.45, 0.45, 0.10)),
    target = sample(c(0, 1, NA), n, replace = TRUE, prob = c(0.45, 0.45, 0.10))
  )
  
  err <- NULL
  passed <- tryCatch({
    res_keep <- fs_infogain(df, "target", remove_na = FALSE)
    res_drop <- fs_infogain(df, "target", remove_na = TRUE)
    
    has_cols_keep <- all(c("Variable", "InfoGain") %in% names(res_keep))
    has_cols_drop <- all(c("Variable", "InfoGain") %in% names(res_drop))
    
    vars_keep_ok <- identical(sort(res_keep$Variable), sort(c("A", "B")))
    vars_drop_ok <- identical(sort(res_drop$Variable), sort(c("A", "B")))
    
    ig_keep_ok <- all(is.finite(res_keep$InfoGain) | is.na(res_keep$InfoGain))
    ig_drop_ok <- all(is.finite(res_drop$InfoGain) | is.na(res_drop$InfoGain))
    
    nonneg_keep <- all(res_keep$InfoGain[!is.na(res_keep$InfoGain)] >= 0)
    nonneg_drop <- all(res_drop$InfoGain[!is.na(res_drop$InfoGain)] >= 0)
    
    has_cols_keep && has_cols_drop &&
      vars_keep_ok && vars_drop_ok &&
      ig_keep_ok && ig_drop_ok &&
      nonneg_keep && nonneg_drop
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_infogain: per-predictor NA handling (remove_na TRUE vs FALSE)",
    passed, note
  )
}

###############################################################################
# Tests: Target Types (numeric and date-like)
###############################################################################

test_fs_infogain_numeric_target <- function() {
  set.seed(8)
  df <- data.frame(
    A      = rnorm(250),
    B      = sample(letters[1:3], 250, replace = TRUE),
    target = rnorm(250)
  )
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_infogain(df, "target")
    
    vars_ok  <- identical(sort(res$Variable), sort(c("A", "B")))
    ig_finite <- all(is.finite(res$InfoGain))
    ig_nonneg <- all(res$InfoGain >= 0)
    
    vars_ok && ig_finite && ig_nonneg
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_infogain: numeric target discretized automatically, IG finite",
    passed, note
  )
}

test_fs_infogain_date_target <- function() {
  set.seed(9)
  dates <- as.Date("2020-01-01") + sample(0:60, 200, TRUE)
  df <- data.frame(
    A      = rnorm(200),
    B      = sample(c("x", "y"), 200, TRUE),
    target = dates
  )
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_infogain(df, "target")
    
    vars_ok  <- identical(sort(res$Variable), sort(c("A", "B")))
    ig_finite <- all(is.finite(res$InfoGain))
    ig_nonneg <- all(res$InfoGain >= 0)
    
    vars_ok && ig_finite && ig_nonneg
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_infogain: date-like target coerced to categorical and handled",
    passed, note
  )
}

###############################################################################
# Run All fs_infogain Tests
###############################################################################

run_fs_infogain_tests <- function() {
  cat("========== Running fs_infogain Tests ==========\n")
  test_fs_infogain_existence()
  test_fs_infogain_input_validation()
  test_fs_infogain_single_numeric()
  test_fs_infogain_single_mixed()
  test_fs_infogain_date_predictors()
  test_fs_infogain_constant_predictor()
  test_fs_infogain_list_origin()
  test_fs_infogain_list_na_names_origin()
  test_fs_infogain_numeric_bins_override()
  test_fs_infogain_na_handling()
  test_fs_infogain_numeric_target()
  test_fs_infogain_date_target()
  cat("========== fs_infogain Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment the following line to run all tests when this script is executed:
# run_fs_infogain_tests()
