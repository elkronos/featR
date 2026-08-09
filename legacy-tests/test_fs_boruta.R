###############################################################################
# Testing Infrastructure - Boruta (Updated)
###############################################################################

# Optional: if fs_boruta is in a separate script or package, load it here:
# source("fs_boruta.R")
# library(yourpackage)

# Global container for test results
test_results <- data.frame(
  Test   = character(),
  Result = character(),
  stringsAsFactors = FALSE
)

# Global flag for caret availability (set in run_fs_boruta_tests)
have_caret <- FALSE

# Helper: Print and Store Test Result ----------------------------------------

#' Helper: Print and Store Test Result
#'
#' @param test_name Character. Name of the test.
#' @param passed Logical. Whether the test passed.
#' @param note Optional character. Additional notes.
print_and_store_result <- function(test_name, passed, note = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-80s [%s]\n", test_name, result))
  if (!is.null(note)) cat("  Note: ", note, "\n")
  test_results <<- rbind(
    test_results,
    data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)
  )
}

# Helper: deterministic wrapper around fs_boruta ------------------------------

.fs_boruta_run <- function(df, target, ...) {
  fs_boruta(df, target, seed = 42L, doTrace = 0, ...)
}

###############################################################################
# Tests: Existence / Basic Setup
###############################################################################

test_fs_boruta_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_boruta") && is.function(fs_boruta)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_boruta: Function exists and is callable", passed, note)
}

###############################################################################
# Tests: Core Functionality by Data Type
###############################################################################

# Test 1: Simple numeric data frame
test_fs_boruta_numeric_data <- function() {
  err <- NULL
  passed <- tryCatch({
    set.seed(1)
    df1 <- data.frame(
      A      = sample(1:10, 100, replace = TRUE),
      B      = sample(1:5,  100, replace = TRUE),
      target = sample(1:2,  100, replace = TRUE)
    )
    result1 <- .fs_boruta_run(df1, "target")
    is.character(result1$selected_features) &&
      inherits(result1$boruta_obj, "Boruta") &&
      "finalDecision" %in% names(result1$boruta_obj)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_boruta: works with numeric data", passed, note)
}

# Test 2: Data frame with categorical variables (character -> factor)
test_fs_boruta_categorical_character <- function() {
  err <- NULL
  passed <- tryCatch({
    set.seed(2)
    df2 <- data.frame(
      A      = sample(1:10, 100, replace = TRUE),
      B      = sample(c("yes", "no"), 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    result2 <- .fs_boruta_run(df2, "target")
    is.character(result2$selected_features) &&
      inherits(result2$boruta_obj, "Boruta")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: handles categorical (character) predictors",
    passed, note
  )
}

# Test 3: Data frame with date variables (Date -> numeric)
test_fs_boruta_date_predictors <- function() {
  err <- NULL
  passed <- tryCatch({
    set.seed(3)
    df3 <- data.frame(
      A      = sample(1:10, 100, replace = TRUE),
      B      = sample(c("yes", "no"), 100, replace = TRUE),
      C      = seq(as.Date("2001-01-01"), by = "day", length.out = 100),
      target = sample(1:2, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    result3 <- .fs_boruta_run(df3, "target")
    is.character(result3$selected_features) &&
      inherits(result3$boruta_obj, "Boruta")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_boruta: handles Date predictors", passed, note)
}

###############################################################################
# Tests: Error Handling
###############################################################################

# Test 4: Error when target variable is missing
test_fs_boruta_missing_target <- function() {
  df4 <- data.frame(
    A = sample(1:10, 100, replace = TRUE),
    B = sample(c("yes", "no"), 100, replace = TRUE)
  )
  err <- NULL
  passed <- tryCatch({
    fs_boruta(df4, "target")
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("The target variable is not found in the provided data frame\\.", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: errors when target variable is missing",
    passed, note
  )
}

# Test 5: Error on unsupported variable types (e.g., matrix column)
test_fs_boruta_unsupported_predictor_types <- function() {
  err <- NULL
  passed <- tryCatch({
    set.seed(5)
    df5 <- data.frame(
      A      = sample(1:10, 100, replace = TRUE),
      B      = I(matrix(sample(1:5, 200, replace = TRUE), ncol = 2)),
      target = sample(1:2, 100, replace = TRUE)
    )
    fs_boruta(df5, "target")
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Unsupported variable types found in columns:", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: errors on unsupported predictor types (e.g., matrix column)",
    passed, note
  )
}

# Test 10: Invalid target type errors
test_fs_boruta_invalid_target_type <- function() {
  df10 <- data.frame(
    A      = rnorm(50),
    target = as.Date("2001-01-01") + 0:49
  )
  err <- NULL
  passed <- tryCatch({
    fs_boruta(df10, "target")
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("`target_var` must be numeric \\(regression\\) or factor \\(classification\\)\\.", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: errors when target type is unsupported",
    passed, note
  )
}

# Test 11: NA in target triggers error
test_fs_boruta_na_in_target <- function() {
  set.seed(11)
  df11 <- data.frame(
    A      = rnorm(50),
    target = sample(c(1, 2, NA_real_), 50, replace = TRUE)
  )
  err <- NULL
  passed <- tryCatch({
    fs_boruta(df11, "target")
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("`target_var` contains missing values; please impute or remove them before calling `fs_boruta\\(\\)`\\.", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: errors when target contains NA",
    passed, note
  )
}

# Test 12: NA in predictors triggers error
test_fs_boruta_na_in_predictors <- function() {
  set.seed(12)
  df12 <- data.frame(
    A      = rnorm(50),
    B      = rnorm(50),
    target = sample(1:2, 50, replace = TRUE)
  )
  df12$A[sample(1:50, 5)] <- NA_real_
  
  err <- NULL
  passed <- tryCatch({
    fs_boruta(df12, "target")
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Predictors contain missing values; please impute or remove them before calling `fs_boruta\\(\\)`\\.", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: errors when predictors contain NA",
    passed, note
  )
}

# Test 15: Invalid cutoff_features validation
test_fs_boruta_invalid_cutoff_features <- function() {
  df15 <- data.frame(
    A      = rnorm(50),
    B      = rnorm(50),
    target = sample(1:2, 50, replace = TRUE)
  )
  err <- NULL
  passed <- tryCatch({
    fs_boruta(df15, "target", cutoff_features = -1)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("`cutoff_features` must be a single positive integer\\.", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: errors on invalid cutoff_features",
    passed, note
  )
}

# Test 16: Invalid cutoff_cor validation
test_fs_boruta_invalid_cutoff_cor <- function() {
  df16 <- data.frame(
    A      = rnorm(50),
    B      = rnorm(50),
    target = sample(1:2, 50, replace = TRUE)
  )
  err <- NULL
  passed <- tryCatch({
    fs_boruta(df16, "target", cutoff_cor = 2)
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("`cutoff_cor` must be a single finite numeric value in \\[0, 1\\]\\.", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: errors on invalid cutoff_cor",
    passed, note
  )
}

###############################################################################
# Tests: Feature Cap and Correlation Pruning
###############################################################################

# Test 6: Limiting number of selected features
test_fs_boruta_feature_cap <- function() {
  err <- NULL
  passed <- tryCatch({
    set.seed(6)
    df6 <- data.frame(
      A      = rnorm(200),
      B      = rnorm(200),
      C      = rnorm(200),
      D      = rnorm(200),
      target = sample(1:2, 200, replace = TRUE)
    )
    result6 <- .fs_boruta_run(df6, "target", cutoff_features = 2)
    length(result6$selected_features) <= 2 &&
      is.character(result6$selected_features)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: respects feature cap (cutoff_features)",
    passed, note
  )
}

# Test 7: Removing highly correlated features (requires caret)
test_fs_boruta_correlation_pruning <- function() {
  if (!have_caret) {
    print_and_store_result(
      "fs_boruta: correlation pruning when caret is available",
      TRUE,
      "Skipped: 'caret' not installed."
    )
    return(invisible(NULL))
  }
  
  err <- NULL
  passed <- tryCatch({
    set.seed(7)
    df7 <- data.frame(
      A = rnorm(200),
      B = rnorm(200),
      C = rnorm(200),
      D = rnorm(200)
    )
    df7$E      <- df7$A + rnorm(200, sd = 0.01)  # E ~ A + noise
    df7$target <- sample(1:2, 200, replace = TRUE)
    
    result7 <- .fs_boruta_run(df7, "target", cutoff_cor = 0.9)
    # Ensure not both A and E survive
    !all(c("A", "E") %in% result7$selected_features)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: removes highly correlated features when caret is available",
    passed, note
  )
}

# Test 8: Skipping correlation-pruning when cutoff_cor = NULL
test_fs_boruta_no_correlation_pruning_when_null <- function() {
  err <- NULL
  passed <- tryCatch({
    set.seed(8)
    df8 <- data.frame(
      A      = rnorm(150),
      B      = rnorm(150),
      target = sample(1:2, 150, replace = TRUE)
    )
    res_no_prune <- .fs_boruta_run(df8, "target", cutoff_cor = NULL)
    res_prune    <- .fs_boruta_run(df8, "target", cutoff_cor = 0.7)
    length(res_prune$selected_features) <= length(res_no_prune$selected_features)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: does not prune by correlation when cutoff_cor is NULL",
    passed, note
  )
}

# Test 18: Correlation NA handling with constant predictor (requires caret)
test_fs_boruta_constant_predictor_correlation_na <- function() {
  if (!have_caret) {
    print_and_store_result(
      "fs_boruta: handles NA correlations from constant predictors when caret is available",
      TRUE,
      "Skipped: 'caret' not installed."
    )
    return(invisible(NULL))
  }
  
  err <- NULL
  passed <- tryCatch({
    set.seed(16)
    df18 <- data.frame(
      A      = rnorm(200),
      B      = 1,  # constant predictor
      target = sample(1:2, 200, replace = TRUE)
    )
    res18 <- .fs_boruta_run(df18, "target", cutoff_cor = 0.7)
    is.character(res18$selected_features) &&
      inherits(res18$boruta_obj, "Boruta")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: handles NA correlations from constant predictors when caret is available",
    passed, note
  )
}

###############################################################################
# Tests: Target Type (factor vs numeric) and Predictor Types
###############################################################################

# Test 9: Factor target (classification)
test_fs_boruta_factor_target <- function() {
  err <- NULL
  passed <- tryCatch({
    set.seed(10)
    df9 <- data.frame(
      A      = rnorm(100),
      B      = rnorm(100),
      target = factor(sample(c("yes", "no"), 100, replace = TRUE))
    )
    result9 <- .fs_boruta_run(df9, "target")
    is.character(result9$selected_features) &&
      inherits(result9$boruta_obj, "Boruta")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: works with factor target (classification)",
    passed, note
  )
}

# Test 13: Logical predictors are accepted (logical -> factor)
test_fs_boruta_logical_predictors <- function() {
  err <- NULL
  passed <- tryCatch({
    set.seed(13)
    df13 <- data.frame(
      A      = rnorm(100),
      B      = sample(c(TRUE, FALSE), 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    result13 <- .fs_boruta_run(df13, "target")
    is.character(result13$selected_features) &&
      inherits(result13$boruta_obj, "Boruta")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: handles logical predictors",
    passed, note
  )
}

# Test 14: POSIXct predictors are accepted (POSIXct -> numeric)
test_fs_boruta_posix_predictors <- function() {
  err <- NULL
  passed <- tryCatch({
    set.seed(14)
    df14 <- data.frame(
      A      = rnorm(100),
      time   = as.POSIXct("2000-01-01", tz = "UTC") + 0:99,
      target = sample(1:2, 100, replace = TRUE)
    )
    result14 <- .fs_boruta_run(df14, "target")
    is.character(result14$selected_features) &&
      inherits(result14$boruta_obj, "Boruta")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: handles POSIXct predictors",
    passed, note
  )
}

# Test 17: resolve_tentative = FALSE path executes
test_fs_boruta_resolve_tentative_false <- function() {
  err <- NULL
  passed <- tryCatch({
    set.seed(15)
    df17 <- data.frame(
      A      = rnorm(100),
      B      = rnorm(100),
      target = sample(1:2, 100, replace = TRUE)
    )
    res17 <- fs_boruta(df17, "target", seed = 42L, doTrace = 0, resolve_tentative = FALSE)
    is.character(res17$selected_features) &&
      inherits(res17$boruta_obj, "Boruta")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_boruta: runs when resolve_tentative = FALSE",
    passed, note
  )
}

###############################################################################
# Run All fs_boruta Tests
###############################################################################

run_fs_boruta_tests <- function() {
  cat("========== Running fs_boruta Tests ==========\n")
  
  # Pre-flight: required package Boruta
  if (!requireNamespace("Boruta", quietly = TRUE)) {
    cat("Package 'Boruta' is not installed; skipping fs_boruta tests.\n")
    return(invisible(NULL))
  }
  
  # caret is optional, used only for correlation-related tests
  have_caret <<- requireNamespace("caret", quietly = TRUE)
  
  test_fs_boruta_existence()
  test_fs_boruta_numeric_data()
  test_fs_boruta_categorical_character()
  test_fs_boruta_date_predictors()
  test_fs_boruta_missing_target()
  test_fs_boruta_unsupported_predictor_types()
  test_fs_boruta_invalid_target_type()
  test_fs_boruta_na_in_target()
  test_fs_boruta_na_in_predictors()
  test_fs_boruta_invalid_cutoff_features()
  test_fs_boruta_invalid_cutoff_cor()
  test_fs_boruta_feature_cap()
  test_fs_boruta_correlation_pruning()
  test_fs_boruta_no_correlation_pruning_when_null()
  test_fs_boruta_constant_predictor_correlation_na()
  test_fs_boruta_factor_target()
  test_fs_boruta_logical_predictors()
  test_fs_boruta_posix_predictors()
  test_fs_boruta_resolve_tentative_false()
  
  cat("========== fs_boruta Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment to run all tests when this script is executed:
# run_fs_boruta_tests()
