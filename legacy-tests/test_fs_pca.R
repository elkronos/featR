###############################################################################
# Testing Infrastructure - PCA (fs_pca)
###############################################################################

# Optional: if fs_pca is in a separate script/package, load it here, e.g.:
# source("fs_pca.R")
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
# Tests: Existence and Basic Input Validation
###############################################################################

test_fs_pca_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_pca") && is.function(fs_pca)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_pca: Function exists and is callable", passed, note)
}

test_plot_pca_results_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("plot_pca_results") && is.function(plot_pca_results)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("plot_pca_results: Function exists and is callable", passed, note)
}

test_fs_pca_null_and_empty_data <- function() {
  err1 <- NULL
  err2 <- NULL
  
  passed1 <- tryCatch({
    fs_pca(NULL)
    FALSE
  }, error = function(e) {
    err1 <<- e
    TRUE
  })
  
  passed2 <- tryCatch({
    fs_pca(data.frame())
    FALSE
  }, error = function(e) {
    err2 <<- e
    TRUE
  })
  
  passed <- passed1 && passed2
  note <- paste(
    if (!is.null(err1)) paste("NULL:", conditionMessage(err1)) else "",
    if (!is.null(err2)) paste("| empty:", conditionMessage(err2)) else ""
  )
  print_and_store_result("fs_pca: errors on NULL and empty data", passed, note)
}

test_fs_pca_no_numeric_columns <- function() {
  df <- data.frame(
    Char1 = c("a", "b"),
    Char2 = c("c", "d"),
    stringsAsFactors = FALSE
  )
  err <- NULL
  passed <- tryCatch({
    fs_pca(df)
    FALSE
  }, error = function(e) {
    err <<- e
    TRUE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_pca: errors when data contains no numeric columns",
    passed, note
  )
}

test_fs_pca_invalid_num_pc <- function() {
  set.seed(123)
  n <- 20
  X <- matrix(rnorm(n * 2), ncol = 2)
  df <- data.frame(X)
  names(df) <- c("X1", "X2")
  
  err0    <- NULL
  errneg  <- NULL
  errchar <- NULL
  
  passed0 <- tryCatch({
    fs_pca(df, num_pc = 0)
    FALSE
  }, error = function(e) {
    err0 <<- e
    grepl("num_pc", conditionMessage(e))
  })
  
  passedneg <- tryCatch({
    fs_pca(df, num_pc = -1)
    FALSE
  }, error = function(e) {
    errneg <<- e
    grepl("num_pc", conditionMessage(e))
  })
  
  passedchar <- tryCatch({
    fs_pca(df, num_pc = "two")
    FALSE
  }, error = function(e) {
    errchar <<- e
    grepl("num_pc", conditionMessage(e))
  })
  
  passed <- passed0 && passedneg && passedchar
  note <- paste(
    if (!is.null(err0))    paste("0:",    conditionMessage(err0))    else "",
    if (!is.null(errneg))  paste("| -1:", conditionMessage(errneg))  else "",
    if (!is.null(errchar)) paste("| 'two':", conditionMessage(errchar)) else ""
  )
  print_and_store_result(
    "fs_pca: errors when num_pc < 1 or non-numeric",
    passed, note
  )
}

test_fs_pca_invalid_label_col <- function() {
  set.seed(123)
  n <- 30
  X <- matrix(rnorm(n * 3), ncol = 3)
  df <- data.frame(X)
  names(df) <- c("X1", "X2", "X3")
  
  err <- NULL
  passed <- tryCatch({
    fs_pca(df, label_col = "does_not_exist")
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("label_col", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_pca: errors when label_col is not found in data",
    passed, note
  )
}

###############################################################################
# Tests: Core Functionality and PCA Behavior
###############################################################################

test_fs_pca_basic_functionality <- function() {
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_pca(fakeData)
    required_names <- c("pc_loadings", "pc_scores", "var_explained", "pca_df", "meta")
    all(required_names %in% names(out)) &&
      length(out$var_explained) == 2L &&
      ncol(out$pc_scores) == 2L &&
      ncol(out$pc_loadings) == 2L &&
      is.numeric(out$var_explained) &&
      all(out$var_explained > 0) &&
      sum(out$var_explained) > 0 &&
      sum(out$var_explained) <= 1
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_pca: basic functionality and output structure (default num_pc = 2)",
    passed, note
  )
}

test_fs_pca_missing_values_alignment <- function() {
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  
  fakeData_na <- fakeData
  fakeData_na[1:10, 2] <- NA  # introduce NAs in one numeric column
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_pca(fakeData_na)
    rows_kept <- out$meta$rows_kept
    sum(rows_kept) < nrow(fakeData_na) &&
      !all(rows_kept[1:10]) &&
      nrow(out$pca_df) == sum(rows_kept)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_pca: rows with NA in numeric columns dropped; alignment preserved",
    passed, note
  )
}

test_fs_pca_custom_num_pc <- function() {
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_pca(fakeData, num_pc = 3)
    length(out$var_explained) == 3L &&
      ncol(out$pc_scores) == 3L &&
      ncol(out$pc_loadings) == 3L
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_pca: custom number of PCs (num_pc = 3) returns correctly sized outputs",
    passed, note
  )
}

test_fs_pca_too_many_pcs <- function() {
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  
  err <- NULL
  passed <- tryCatch({
    fs_pca(fakeData, num_pc = 10)
    FALSE
  }, error = function(e) {
    err <<- e
    TRUE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_pca: errors when requesting more PCs than data supports",
    passed, note
  )
}

test_fs_pca_zero_variance_column <- function() {
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  fakeData$const <- 1  # zero variance column
  
  err <- NULL
  warn_msg <- NULL
  passed <- tryCatch({
    out <- withCallingHandlers(
      fs_pca(fakeData, num_pc = 2),
      warning = function(w) {
        warn_msg <<- c(warn_msg, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    is.list(out) &&
      length(out$var_explained) == 2L &&
      (!is.null(warn_msg) && any(grepl("zero-variance", warn_msg)))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) {
    conditionMessage(err)
  } else if (!is.null(warn_msg)) {
    paste("Warnings:", paste(unique(warn_msg), collapse = " | "))
  } else {
    "No warnings captured."
  }
  print_and_store_result(
    "fs_pca: zero-variance numeric column removed with warning; PCA completes",
    passed, note
  )
}

test_fs_pca_insufficient_rows_after_filtering <- function() {
  # Construct data with 2 rows but only 1 row complete on numeric columns
  df <- data.frame(
    x = c(1, NA),
    y = c(2, 3)
  )
  err <- NULL
  passed <- tryCatch({
    fs_pca(df)
    FALSE
  }, error = function(e) {
    err <<- e
    TRUE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_pca: errors when fewer than 2 complete numeric rows remain after filtering",
    passed, note
  )
}

###############################################################################
# Tests: Label Handling and Plotting
###############################################################################

test_fs_pca_label_plotting_character <- function() {
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  fakeData$group <- sample(c("A", "B", "C"), n, replace = TRUE)
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_pca(fakeData, label_col = "group")
    "group" %in% names(out$pca_df)
    invisible(plot_pca_results(out, label_col = "group"))
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_pca: plotting with valid character/factor label column works",
    passed, note
  )
}

test_fs_pca_plot_invalid_label <- function() {
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  fakeData$group <- sample(c("A", "B", "C"), n, replace = TRUE)
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_pca(fakeData, label_col = "group")
    plot_pca_results(out, label_col = "invalid")
    FALSE
  }, error = function(e) {
    err <<- e
    TRUE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "plot_pca_results: errors when label_col is not present in pca_df",
    passed, note
  )
}

test_fs_pca_many_labels_palette_switch <- function() {
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  fakeData$group <- as.character(seq_len(n))  # many unique labels
  
  err <- NULL
  warn_msg <- NULL
  passed <- tryCatch({
    out <- fs_pca(fakeData, num_pc = 2, label_col = "group")
    withCallingHandlers(
      invisible(plot_pca_results(out, label_col = "group")),
      warning = function(w) {
        warn_msg <<- c(warn_msg, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    !is.null(warn_msg) && any(grepl("More than 9 unique labels", warn_msg))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) {
    conditionMessage(err)
  } else if (!is.null(warn_msg)) {
    paste("Warnings:", paste(unique(warn_msg), collapse = " | "))
  } else {
    "No warnings captured."
  }
  print_and_store_result(
    "plot_pca_results: many unique labels trigger viridis palette warning",
    passed, note
  )
}

test_fs_pca_numeric_label_handling <- function() {
  err <- NULL
  passed <- tryCatch({
    out <- fs_pca(mtcars, num_pc = 2, label_col = "cyl")
    # Numeric label column should appear in pca_df but be excluded from PCA features
    "cyl" %in% names(out$pca_df) &&
      !("cyl" %in% out$meta$numeric_cols) &&
      {
        invisible(plot_pca_results(out, label_col = "cyl"))
        TRUE
      }
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_pca: numeric label_col excluded from PCA features but included for plotting",
    passed, note
  )
}

test_fs_pca_plot_true_without_label <- function() {
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 5), ncol = 5)
  colnames(X) <- paste0("X", 1:5)
  fakeData <- data.frame(X)
  
  err <- NULL
  warn_msg <- NULL
  passed <- tryCatch({
    withCallingHandlers(
      fs_pca(fakeData, num_pc = 2, plot = TRUE),
      warning = function(w) {
        warn_msg <<- c(warn_msg, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    !is.null(warn_msg) && any(grepl("plot = TRUE", warn_msg))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) {
    conditionMessage(err)
  } else if (!is.null(warn_msg)) {
    paste("Warnings:", paste(unique(warn_msg), collapse = " | "))
  } else {
    "No warnings captured."
  }
  print_and_store_result(
    "fs_pca: plot = TRUE without label_col issues warning and does not error",
    passed, note
  )
}

###############################################################################
# Run All fs_pca Tests
###############################################################################

run_fs_pca_tests <- function() {
  cat("========== Running fs_pca Tests ==========\n")
  test_fs_pca_existence()
  test_plot_pca_results_existence()
  test_fs_pca_null_and_empty_data()
  test_fs_pca_no_numeric_columns()
  test_fs_pca_invalid_num_pc()
  test_fs_pca_invalid_label_col()
  test_fs_pca_basic_functionality()
  test_fs_pca_missing_values_alignment()
  test_fs_pca_custom_num_pc()
  test_fs_pca_too_many_pcs()
  test_fs_pca_zero_variance_column()
  test_fs_pca_insufficient_rows_after_filtering()
  test_fs_pca_label_plotting_character()
  test_fs_pca_plot_invalid_label()
  test_fs_pca_many_labels_palette_switch()
  test_fs_pca_numeric_label_handling()
  test_fs_pca_plot_true_without_label()
  cat("========== fs_pca Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment the following line to run all tests when this script is executed:
# run_fs_pca_tests()
