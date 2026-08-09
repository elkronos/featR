###############################################################################
# Testing Infrastructure - Chi-Square (fs_chi)
###############################################################################

# Optional: if fs_chi is in a separate script/package, load it here, e.g.:
# source("fs_chi.R")
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

test_fs_chi_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_chi") && is.function(fs_chi)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_chi: Function exists and is callable", passed, note)
}

test_fs_chi_input_validation <- function() {
  # Case 1: data not a data.frame
  err1 <- NULL
  passed1 <- tryCatch({
    fs_chi("not a data frame", "target")
    FALSE
  }, error = function(e) {
    err1 <<- e
    TRUE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result("fs_chi: errors if data is not a data.frame", passed1, note1)
  
  # Case 2: target column missing
  df2 <- data.frame(A = factor(c("a", "b")), B = factor(c("x", "y")))
  err2 <- NULL
  passed2 <- tryCatch({
    fs_chi(df2, "target")
    FALSE
  }, error = function(e) {
    err2 <<- e
    TRUE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result("fs_chi: errors if target column missing", passed2, note2)
  
  # Case 3: target has < 2 non-NA levels
  df3 <- data.frame(target = factor(c("Yes", NA, NA)))
  err3 <- NULL
  passed3 <- tryCatch({
    fs_chi(df3, "target")
    FALSE
  }, error = function(e) {
    err3 <<- e
    TRUE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "fs_chi: errors if target has < 2 non-NA levels",
    passed3, note3
  )
}

###############################################################################
# Tests: No Categorical Features
###############################################################################

test_fs_chi_no_categorical_features <- function() {
  df <- data.frame(
    target = factor(c("Yes", "No")),
    X      = 1:2,
    Y      = c(3.1, 4.2)
  )
  err <- NULL
  passed <- tryCatch({
    res <- fs_chi(df, "target")
    is.list(res) &&
      all(c("results", "significant_features") %in% names(res)) &&
      nrow(res$results) == 0L &&
      length(res$significant_features) == 0L
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_chi: returns empty results when no factor features", passed, note)
}

###############################################################################
# Tests: Continuity Correction Behavior (2x2 Tables)
###############################################################################

test_fs_chi_continuity_correction <- function() {
  set.seed(1)
  target  <- factor(rep(c("Yes", "No"), each = 100))
  feat    <- factor(rep(c("A", "B"), times = 100))
  df <- data.frame(target = target, feat = feat)
  
  # Default (NULL) -> correction applied for 2x2
  err1 <- NULL
  passed1 <- tryCatch({
    out <- fs_chi(df, "target", continuity_correction = NULL, p_adjust_method = "none")
    stopifnot("feat" %in% out$results$feature)
    row <- out$results[out$results$feature == "feat", ]
    nrow(row) == 1L &&
      isTRUE(row$correction_applied) &&
      identical(as.character(row$method), "asymptotic") &&
      is.finite(row$p_value)
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_chi: continuity correction applied by default for 2x2 (NULL)",
    passed1, note1
  )
  
  # continuity_correction = FALSE -> no correction
  err2 <- NULL
  passed2 <- tryCatch({
    out <- fs_chi(df, "target", continuity_correction = FALSE, p_adjust_method = "none")
    row <- out$results[out$results$feature == "feat", ]
    nrow(row) == 1L && !isTRUE(row$correction_applied)
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_chi: continuity correction can be disabled for 2x2",
    passed2, note2
  )
}

###############################################################################
# Tests: Simulation Fallback for Low Expected Counts
###############################################################################

test_fs_chi_simulation_fallback <- function() {
  set.seed(123)
  target <- factor(rep(c("Yes", "No"), each = 250))
  feature_low <- factor(c(rep("A", 495), rep("B", 5)))
  df <- data.frame(feature_low = feature_low, target = target)
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_chi(df, "target", p_adjust_method = "none")
    row <- out$results[out$results$feature == "feature_low", ]
    nrow(row) == 1L &&
      identical(as.character(row$method), "simulation") &&
      (is.finite(row$p_value) || is.na(row$p_value))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: falls back to simulation when expected counts are low",
    passed, note
  )
}

###############################################################################
# Tests: Significant Association Detection
###############################################################################

test_fs_chi_significant_association <- function() {
  set.seed(42)
  target <- factor(c(rep("Yes", 300), rep("No", 200)))
  feat <- factor(c(
    rep("A", 290), rep("B", 10),   # mostly A in Yes
    rep("A", 50),  rep("B", 150)   # mostly B in No
  ))
  df <- data.frame(target = target, feat = feat)
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_chi(df, "target", p_adjust_method = "bonferroni")
    row <- out$results[out$results$feature == "feat", ]
    nrow(row) == 1L &&
      isTRUE(row$significant) &&
      "feat" %in% out$significant_features
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: detects significant association when present",
    passed, note
  )
}

###############################################################################
# Tests: p.adjust Methods (bonferroni vs none, case-insensitivity, invalid)
###############################################################################

test_fs_chi_p_adjust_methods <- function() {
  set.seed(999)
  target <- factor(rep(c("Yes", "No"), each = 200))
  f1 <- factor(sample(c("A","B"), 400, TRUE))
  f2 <- factor(sample(c("X","Y"), 400, TRUE))
  df <- data.frame(target, f1, f2)
  
  err <- NULL
  passed <- tryCatch({
    out_bonf <- fs_chi(df, "target", p_adjust_method = "bonferroni")
    out_none <- fs_chi(df, "target", p_adjust_method = "none")
    
    rb <- out_bonf$results[, c("feature", "adj_p_value")]
    rn <- out_none$results[, c("feature", "adj_p_value")]
    m  <- merge(rb, rn, by = "feature", suffixes = c("_bonf", "_none"))
    
    all(m$adj_p_value_bonf >= m$adj_p_value_none, na.rm = TRUE)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: p.adjust('bonferroni') yields adj_p >= 'none'",
    passed, note
  )
}

test_fs_chi_p_adjust_case_insensitive <- function() {
  set.seed(101)
  target <- factor(rep(c("Yes","No"), each = 150))
  f1 <- factor(sample(c("A","B","C"), 300, TRUE))
  f2 <- factor(sample(c("X","Y"), 300, TRUE))
  df <- data.frame(target, f1, f2)
  
  err <- NULL
  passed <- tryCatch({
    set.seed(101)
    out_upper <- fs_chi(df, "target", p_adjust_method = "BH", parallel = FALSE)
    set.seed(101)
    out_lower <- fs_chi(df, "target", p_adjust_method = "bh", parallel = FALSE)
    
    ru <- out_upper$results[, c("feature", "adj_p_value")]
    rl <- out_lower$results[, c("feature", "adj_p_value")]
    m  <- merge(ru, rl, by = "feature", suffixes = c("_upper", "_lower"))
    
    all.equal(m$adj_p_value_upper, m$adj_p_value_lower) == TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: p_adjust_method is case-insensitive (BH vs bh)",
    passed, note
  )
}

test_fs_chi_p_adjust_invalid_method <- function() {
  df <- data.frame(
    target = factor(c("Y","N","Y","N")),
    f1     = factor(c("A","A","B","B"))
  )
  err <- NULL
  passed <- tryCatch({
    fs_chi(df, "target", p_adjust_method = "not_a_method")
    FALSE
  }, error = function(e) {
    err <<- e
    grepl("Invalid p_adjust_method", conditionMessage(e))
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: invalid p_adjust_method throws clear error",
    passed, note
  )
}

###############################################################################
# Tests: Character Coercion and Skipped Features
###############################################################################

test_fs_chi_character_coercion <- function() {
  df <- data.frame(
    target = c("Y","Y","N","N"),
    f1     = c("A","A","B","B"),
    f2     = factor(c("X","Y","X","Y")),
    stringsAsFactors = FALSE
  )
  err <- NULL
  passed <- tryCatch({
    out <- fs_chi(df, "target", p_adjust_method = "none")
    all(out$results$feature %in% c("f1", "f2"))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: coerces character columns (including target) to factor",
    passed, note
  )
}

test_fs_chi_skip_features_low_levels <- function() {
  df <- data.frame(
    target = factor(c("Y","N","Y","N")),
    f1     = factor(c(NA, NA, NA, "only_one_non_na")),
    f2     = factor(c("A","A","B","B"))
  )
  err <- NULL
  passed <- tryCatch({
    out  <- fs_chi(df, "target", p_adjust_method = "none")
    row1 <- out$results[out$results$feature == "f1", ]
    row2 <- out$results[out$results$feature == "f2", ]
    is.na(row1$p_value) && is.finite(row2$p_value)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: skips features with <2 levels after NA removal",
    passed, note
  )
}

###############################################################################
# Tests: n, df, Sequential and Parallel Paths
###############################################################################

test_fs_chi_n_counts <- function() {
  df <- data.frame(
    target = factor(c("Y","N",NA,"Y",NA,"N")),
    f1     = factor(c("A","A","A",NA,"B","B"))
  )
  err <- NULL
  passed <- tryCatch({
    out <- fs_chi(df, "target", p_adjust_method = "none")
    row <- out$results[out$results$feature == "f1", ]
    as.integer(row$n) == 3L && is.finite(row$p_value)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: n reflects number of non-NA feature/target pairs",
    passed, note
  )
}

test_fs_chi_multi_level_df <- function() {
  set.seed(456)
  target <- factor(sample(c("C1", "C2", "C3"), 600, TRUE))
  feat   <- factor(sample(c("L1", "L2", "L3", "L4"), 600, TRUE))
  df <- data.frame(target = target, feat = feat)
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_chi(df, "target", p_adjust_method = "none")
    row <- out$results[out$results$feature == "feat", ]
    expected_df <- (nlevels(target) - 1L) * (nlevels(feat) - 1L)
    as.integer(row$df) == expected_df &&
      is.finite(row$p_value) &&
      identical(as.character(row$method), "asymptotic")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: handles multi-level features/targets with correct df",
    passed, note
  )
}

test_fs_chi_sequential_path <- function() {
  df <- data.frame(
    target = factor(c("Y","N","Y","N")),
    f1     = factor(c("A","A","B","B")),
    f2     = factor(c("X","Y","X","Y"))
  )
  err <- NULL
  passed <- tryCatch({
    out <- fs_chi(df, "target", parallel = FALSE, p_adjust_method = "none")
    nrow(out$results) == 2L &&
      all(out$results$feature %in% c("f1","f2"))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: sequential (parallel = FALSE) path runs without error",
    passed, note
  )
}

test_fs_chi_parallel_smoke <- function() {
  set.seed(321)
  target <- factor(rep(c("Yes","No"), each = 150))
  f1 <- factor(sample(c("A","B","C"), 300, TRUE))
  f2 <- factor(sample(c("X","Y"), 300, TRUE))
  df <- data.frame(target, f1, f2)
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_chi(
      df, "target",
      parallel = TRUE,
      temp_multisession = TRUE,
      p_adjust_method = "none"
    )
    nrow(out$results) == 2L
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_chi: parallel (multisession) path smoke test",
    passed, note
  )
}

###############################################################################
# Run All fs_chi Tests
###############################################################################

run_fs_chi_tests <- function() {
  cat("========== Running fs_chi Tests ==========\n")
  test_fs_chi_existence()
  test_fs_chi_input_validation()
  test_fs_chi_no_categorical_features()
  test_fs_chi_continuity_correction()
  test_fs_chi_simulation_fallback()
  test_fs_chi_significant_association()
  test_fs_chi_p_adjust_methods()
  test_fs_chi_p_adjust_case_insensitive()
  test_fs_chi_p_adjust_invalid_method()
  test_fs_chi_character_coercion()
  test_fs_chi_skip_features_low_levels()
  test_fs_chi_n_counts()
  test_fs_chi_multi_level_df()
  test_fs_chi_sequential_path()
  test_fs_chi_parallel_smoke()
  cat("========== fs_chi Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment the following line to run all tests when this script is executed:
# run_fs_chi_tests()
