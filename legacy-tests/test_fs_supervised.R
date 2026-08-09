###############################################################################
# Testing Infrastructure - Supervised Filter-Based Feature Selection (fs_supervised)
###############################################################################

# Optional: if fs_supervised is in a separate script/package, load it here, e.g.:
# source("fs_supervised.R")
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
# Tests: Existence and Helper Presence
###############################################################################

test_fs_supervised_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_supervised") && is.function(fs_supervised)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_supervised: function exists and is callable", passed, note)
}

test_fs_supervised_helpers_existence <- function() {
  helpers <- c(
    "ensure_package",
    ".validate_threshold",
    ".validate_logical_flag",
    ".validate_action",
    ".validate_direction",
    "convert_to_datatable",
    "compute_supervised_scores",
    ".selection_mask_from_scores"
  )
  err <- NULL
  passed <- tryCatch({
    all(vapply(
      helpers,
      function(fn) {
        exists(fn) && is.function(get(fn, mode = "function"))
      },
      logical(1)
    ))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_supervised: helper functions exist and are callable", passed, note)
}

###############################################################################
# Tests: ensure_package
###############################################################################

test_ensure_package <- function() {
  # Case 1: valid base package
  err1 <- NULL
  passed1 <- tryCatch({
    res <- ensure_package("stats")
    isTRUE(res)
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "ensure_package: loads existing base package 'stats' without error",
    passed1, note1
  )
  
  # Case 2: non-character pkg argument
  err2 <- NULL
  passed2 <- tryCatch({
    ensure_package(123)
    FALSE
  }, error = function(e) {
    err2 <<- e
    grepl("must be a non-empty character scalar", conditionMessage(e))
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "ensure_package: errors for non-character pkg argument",
    passed2, note2
  )
  
  # Case 3: empty string pkg argument
  err3 <- NULL
  passed3 <- tryCatch({
    ensure_package("")
    FALSE
  }, error = function(e) {
    err3 <<- e
    grepl("must be a non-empty character scalar", conditionMessage(e))
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "ensure_package: errors for empty string pkg argument",
    passed3, note3
  )
}

###############################################################################
# Tests: Validation Helpers
###############################################################################

test_validation_helpers <- function() {
  # .validate_threshold: valid value
  err1 <- NULL
  passed1 <- tryCatch({
    res <- .validate_threshold(0.5, name = "threshold")
    isTRUE(res)
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    ".validate_threshold: accepts non-negative, finite scalar",
    passed1, note1
  )
  
  # .validate_threshold: negative value
  err2 <- NULL
  passed2 <- tryCatch({
    .validate_threshold(-0.1, name = "threshold")
    FALSE
  }, error = function(e) {
    err2 <<- e
    grepl("must be a single non-negative, finite numeric value", conditionMessage(e))
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    ".validate_threshold: errors for negative threshold",
    passed2, note2
  )
  
  # .validate_logical_flag: valid logical
  err3 <- NULL
  passed3 <- tryCatch({
    res <- .validate_logical_flag(TRUE, "flag")
    isTRUE(res)
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    ".validate_logical_flag: accepts single non-NA logical",
    passed3, note3
  )
  
  # .validate_logical_flag: NA logical
  err4 <- NULL
  passed4 <- tryCatch({
    .validate_logical_flag(NA, "flag")
    FALSE
  }, error = function(e) {
    err4 <<- e
    grepl("must be a single non-NA logical value", conditionMessage(e))
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    ".validate_logical_flag: errors for NA logical",
    passed4, note4
  )
  
  # .validate_action: default vector takes first element
  err5 <- NULL
  passed5 <- tryCatch({
    res <- .validate_action(c("keep", "remove"))
    identical(res, "keep")
  }, error = function(e) {
    err5 <<- e
    FALSE
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    ".validate_action: uses first element of vector and matches allowed values",
    passed5, note5
  )
  
  # .validate_action: invalid action
  err6 <- NULL
  passed6 <- tryCatch({
    .validate_action("not_an_action")
    FALSE
  }, error = function(e) {
    err6 <<- e
    grepl("should be one of", conditionMessage(e))
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    ".validate_action: errors for invalid action",
    passed6, note6
  )
  
  # .validate_direction: default vector takes first element
  err7 <- NULL
  passed7 <- tryCatch({
    res <- .validate_direction(c("above", "below"))
    identical(res, "above")
  }, error = function(e) {
    err7 <<- e
    FALSE
  })
  note7 <- if (!is.null(err7)) conditionMessage(err7) else NULL
  print_and_store_result(
    ".validate_direction: uses first element of vector and matches allowed values",
    passed7, note7
  )
  
  # .validate_direction: invalid direction
  err8 <- NULL
  passed8 <- tryCatch({
    .validate_direction("sideways")
    FALSE
  }, error = function(e) {
    err8 <<- e
    grepl("should be one of", conditionMessage(e))
  })
  note8 <- if (!is.null(err8)) conditionMessage(err8) else NULL
  print_and_store_result(
    ".validate_direction: errors for invalid direction",
    passed8, note8
  )
}

###############################################################################
# Tests: convert_to_datatable
###############################################################################

test_convert_to_datatable <- function() {
  # Case 1: numeric matrix with column names
  err1 <- NULL
  passed1 <- tryCatch({
    x_mat <- matrix(1:6, nrow = 3)
    colnames(x_mat) <- c("A", "B")
    dt <- convert_to_datatable(x_mat, log_progress = TRUE)
    ensure_package("data.table")
    data.table::is.data.table(dt) &&
      identical(names(dt), c("A", "B")) &&
      all(dt[[1]] == x_mat[, 1]) &&
      all(dt[[2]] == x_mat[, 2])
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "convert_to_datatable: converts numeric matrix with colnames to data.table",
    passed1, note1
  )
  
  # Case 2: numeric matrix without column names
  err2 <- NULL
  passed2 <- tryCatch({
    x_mat2 <- matrix(1:8, nrow = 4)
    colnames(x_mat2) <- NULL
    dt2 <- convert_to_datatable(x_mat2, log_progress = FALSE)
    ensure_package("data.table")
    data.table::is.data.table(dt2) &&
      identical(names(dt2), c("V1", "V2"))
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "convert_to_datatable: assigns default column names for unnamed matrix",
    passed2, note2
  )
  
  # Case 3: numeric data.frame
  err3 <- NULL
  passed3 <- tryCatch({
    df <- data.frame(
      X = c(1.1, 2.2, 3.3),
      Y = c(4.4, 5.5, 6.6)
    )
    dt3 <- convert_to_datatable(df, log_progress = TRUE)
    ensure_package("data.table")
    data.table::is.data.table(dt3) &&
      identical(names(dt3), names(df)) &&
      all.equal(as.data.frame(dt3), df) == TRUE
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "convert_to_datatable: converts numeric data.frame to data.table",
    passed3, note3
  )
  
  # Case 4: non-numeric data.frame errors
  err4 <- NULL
  passed4 <- tryCatch({
    df_bad <- data.frame(
      X = c(1, 2, 3),
      Y = c("a", "b", "c"),
      stringsAsFactors = FALSE
    )
    convert_to_datatable(df_bad)
    FALSE
  }, error = function(e) {
    err4 <<- e
    grepl("All columns of the data frame must be numeric", conditionMessage(e))
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "convert_to_datatable: errors for data.frame with non-numeric columns",
    passed4, note4
  )
  
  # Case 5: non-numeric matrix errors
  err5 <- NULL
  passed5 <- tryCatch({
    x_char <- matrix(letters[1:4], nrow = 2)
    convert_to_datatable(x_char)
    FALSE
  }, error = function(e) {
    err5 <<- e
    grepl("Matrix data must be numeric", conditionMessage(e))
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    "convert_to_datatable: errors for non-numeric matrix input",
    passed5, note5
  )
  
  # Case 6: data.table input returned as data.table and unchanged
  err6 <- NULL
  passed6 <- tryCatch({
    ensure_package("data.table")
    dt_in <- data.table::data.table(
      X = c(1, 2, 3),
      Y = c(4, 5, 6)
    )
    dt_out <- convert_to_datatable(dt_in, log_progress = TRUE)
    data.table::is.data.table(dt_out) &&
      all.equal(dt_in, dt_out) == TRUE
  }, error = function(e) {
    err6 <<- e
    FALSE
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    "convert_to_datatable: returns data.table input unchanged",
    passed6, note6
  )
  
  # Case 7: completely invalid x
  err7 <- NULL
  passed7 <- tryCatch({
    convert_to_datatable("not valid")
    FALSE
  }, error = function(e) {
    err7 <<- e
    grepl("`x` must be a numeric matrix, data frame, or data.table", conditionMessage(e))
  })
  note7 <- if (!is.null(err7)) conditionMessage(err7) else NULL
  print_and_store_result(
    "convert_to_datatable: errors for invalid x type",
    passed7, note7
  )
}

###############################################################################
# Tests: compute_supervised_scores
###############################################################################

test_compute_supervised_scores <- function() {
  # Case 1: correlation scores match abs(stats::cor) with na_rm = TRUE
  err1 <- NULL
  passed1 <- tryCatch({
    df <- data.frame(
      x1 = 1:5,
      x2 = 5:1,
      x3 = c(1, 2, 1, 2, 1)
    )
    dt <- convert_to_datatable(df)
    y  <- 1:5
    s_cor <- compute_supervised_scores(
      dt     = dt,
      y      = y,
      method = "correlation",
      na_rm  = TRUE
    )
    y_num <- as.numeric(y)
    exp_cor <- vapply(
      dt,
      function(col) {
        idx  <- !is.na(col) & !is.na(y_num)
        col2 <- col[idx]
        y2   <- y_num[idx]
        if (length(col2) < 2L || stats::var(col2) == 0 || stats::var(y2) == 0) {
          return(NA_real_)
        }
        abs(stats::cor(col2, y2))
      },
      numeric(1)
    )
    all.equal(unname(s_cor), unname(exp_cor)) == TRUE &&
      identical(names(s_cor), names(dt))
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "compute_supervised_scores: correlation scores match abs(stats::cor)",
    passed1, note1
  )
  
  # Case 2: method = 'auto' matches 'correlation' for numeric y
  err2 <- NULL
  passed2 <- tryCatch({
    df2 <- data.frame(
      x1 = 1:4,
      x2 = 4:1
    )
    dt2 <- convert_to_datatable(df2)
    y2  <- 1:4
    s_auto <- compute_supervised_scores(dt2, y = y2, method = "auto", na_rm = TRUE)
    s_corr <- compute_supervised_scores(dt2, y = y2, method = "correlation", na_rm = TRUE)
    all.equal(s_auto, s_corr) == TRUE
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "compute_supervised_scores: method='auto' equals 'correlation' for numeric y",
    passed2, note2
  )
  
  # Case 3: ANOVA returns positive F-stat for varying feature
  err3 <- NULL
  passed3 <- tryCatch({
    df3 <- data.frame(
      x1 = c(1, 2, 1, 2),  # varies by group
      x2 = c(10, 10, 10, 10) # constant
    )
    dt3   <- convert_to_datatable(df3)
    y_fac <- factor(c("A", "A", "B", "B"))
    s_anova <- compute_supervised_scores(
      dt     = dt3,
      y      = y_fac,
      method = "anova",
      na_rm  = TRUE
    )
    (!is.na(s_anova["x1"]) && s_anova["x1"] > 0) &&
      isTRUE(is.na(s_anova["x2"]) || s_anova["x2"] >= 0)
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "compute_supervised_scores: ANOVA F-statistic positive for varying feature",
    passed3, note3
  )
  
  # Case 4: method = 'auto' matches 'anova' for factor y
  err4 <- NULL
  passed4 <- tryCatch({
    df4 <- data.frame(
      x1 = c(1, 2, 1, 2),
      x2 = c(3, 4, 3, 4)
    )
    dt4   <- convert_to_datatable(df4)
    y_fac2 <- factor(c("A", "A", "B", "B"))
    s_auto2  <- compute_supervised_scores(dt4, y = y_fac2, method = "auto",  na_rm = TRUE)
    s_anova2 <- compute_supervised_scores(dt4, y = y_fac2, method = "anova", na_rm = TRUE)
    all.equal(s_auto2, s_anova2) == TRUE
  }, error = function(e) {
    err4 <<- e
    FALSE
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "compute_supervised_scores: method='auto' equals 'anova' for categorical y",
    passed4, note4
  )
  
  # Case 5: dt must be a data.table
  err5 <- NULL
  passed5 <- tryCatch({
    bad_dt <- data.frame(x1 = 1:4)
    compute_supervised_scores(bad_dt, y = 1:4, method = "correlation")
    FALSE
  }, error = function(e) {
    err5 <<- e
    grepl("`dt` must be a data.table", conditionMessage(e))
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    "compute_supervised_scores: errors if dt is not a data.table",
    passed5, note5
  )
  
  # Case 6: y length mismatch
  err6 <- NULL
  passed6 <- tryCatch({
    dt6 <- convert_to_datatable(data.frame(x1 = 1:4))
    compute_supervised_scores(dt6, y = 1:3, method = "correlation")
    FALSE
  }, error = function(e) {
    err6 <<- e
    grepl("Length of `y` must equal number of rows in `x`", conditionMessage(e))
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    "compute_supervised_scores: errors when length(y) != nrow(dt)",
    passed6, note6
  )
  
  # Case 7: invalid y type
  err7 <- NULL
  passed7 <- tryCatch({
    dt7 <- convert_to_datatable(data.frame(x1 = 1:4))
    compute_supervised_scores(dt7, y = as.list(1:4), method = "correlation")
    FALSE
  }, error = function(e) {
    err7 <<- e
    grepl("`y` must be numeric, factor, character, or logical", conditionMessage(e))
  })
  note7 <- if (!is.null(err7)) conditionMessage(err7) else NULL
  print_and_store_result(
    "compute_supervised_scores: errors for unsupported y type",
    passed7, note7
  )
  
  # Case 8: method = 'correlation' with non-numeric y errors
  err8 <- NULL
  passed8 <- tryCatch({
    dt8 <- convert_to_datatable(data.frame(x1 = 1:4))
    y_fac3 <- factor(c("A", "A", "B", "B"))
    compute_supervised_scores(dt8, y = y_fac3, method = "correlation")
    FALSE
  }, error = function(e) {
    err8 <<- e
    grepl("requires numeric `y`", conditionMessage(e))
  })
  note8 <- if (!is.null(err8)) conditionMessage(err8) else NULL
  print_and_store_result(
    "compute_supervised_scores: method='correlation' errors for non-numeric y",
    passed8, note8
  )
  
  # Case 9: method = 'anova' with numeric y errors
  err9 <- NULL
  passed9 <- tryCatch({
    dt9 <- convert_to_datatable(data.frame(x1 = 1:4))
    compute_supervised_scores(dt9, y = 1:4, method = "anova")
    FALSE
  }, error = function(e) {
    err9 <<- e
    grepl("requires categorical `y`", conditionMessage(e))
  })
  note9 <- if (!is.null(err9)) conditionMessage(err9) else NULL
  print_and_store_result(
    "compute_supervised_scores: method='anova' errors for numeric y",
    passed9, note9
  )
  
  # Case 10: NA handling in correlation with na_rm = FALSE
  err10 <- NULL
  passed10 <- tryCatch({
    df10 <- data.frame(
      x1 = c(1, 2, NA, 4),
      x2 = c(10, 10, 10, 10)
    )
    dt10 <- convert_to_datatable(df10)
    y10  <- c(1, 2, 3, NA)
    s10 <- compute_supervised_scores(
      dt     = dt10,
      y      = y10,
      method = "correlation",
      na_rm  = FALSE
    )
    is.numeric(s10) && length(s10) == ncol(dt10)
  }, error = function(e) {
    err10 <<- e
    FALSE
  })
  note10 <- if (!is.null(err10)) conditionMessage(err10) else NULL
  print_and_store_result(
    "compute_supervised_scores: na_rm = FALSE for correlation runs without error",
    passed10, note10
  )
  
  # Case 11: degenerate case - constant y or constant feature => NA scores
  err11 <- NULL
  passed11 <- tryCatch({
    df11 <- data.frame(
      x1 = c(1, 1, 1, 1),  # constant feature
      x2 = c(1, 2, 3, 4)   # varying
    )
    dt11 <- convert_to_datatable(df11)
    y11  <- c(5, 5, 5, 5)  # constant target
    s11  <- compute_supervised_scores(
      dt     = dt11,
      y      = y11,
      method = "correlation",
      na_rm  = TRUE
    )
    all(is.na(s11))
  }, error = function(e) {
    err11 <<- e
    FALSE
  })
  note11 <- if (!is.null(err11)) conditionMessage(err11) else NULL
  print_and_store_result(
    "compute_supervised_scores: constant y/features yield NA correlation scores",
    passed11, note11
  )
}

###############################################################################
# Tests: .selection_mask_from_scores
###############################################################################

test_selection_mask_from_scores <- function() {
  scores <- c(a = 0, b = 1, c = 2, d = NA)
  
  # Case 1: above, include_equal = FALSE, keep
  err1 <- NULL
  passed1 <- tryCatch({
    m1 <- .selection_mask_from_scores(
      scores        = scores,
      threshold     = 1,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE
    )
    expected1 <- setNames(c(FALSE, FALSE, TRUE, FALSE), names(scores))
    identical(m1, expected1)
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    ".selection_mask_from_scores: direction = 'above', include_equal = FALSE, keep",
    passed1, note1
  )
  
  # Case 2: above, include_equal = TRUE, keep
  err2 <- NULL
  passed2 <- tryCatch({
    m2 <- .selection_mask_from_scores(
      scores        = scores,
      threshold     = 1,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE
    )
    expected2 <- setNames(c(FALSE, TRUE, TRUE, FALSE), names(scores))
    identical(m2, expected2)
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    ".selection_mask_from_scores: direction = 'above', include_equal = TRUE, keep",
    passed2, note2
  )
  
  # Case 3: below, include_equal = FALSE, keep
  err3 <- NULL
  passed3 <- tryCatch({
    m3 <- .selection_mask_from_scores(
      scores        = scores,
      threshold     = 1,
      direction     = "below",
      action        = "keep",
      include_equal = FALSE
    )
    expected3 <- setNames(c(TRUE, FALSE, FALSE, FALSE), names(scores))
    identical(m3, expected3)
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    ".selection_mask_from_scores: direction = 'below', include_equal = FALSE, keep",
    passed3, note3
  )
  
  # Case 4: above, include_equal = FALSE, remove (negation)
  err4 <- NULL
  passed4 <- tryCatch({
    m4 <- .selection_mask_from_scores(
      scores        = scores,
      threshold     = 1,
      direction     = "above",
      action        = "remove",
      include_equal = FALSE
    )
    base_mask <- setNames(c(FALSE, FALSE, TRUE, FALSE), names(scores))
    expected4 <- !base_mask
    identical(m4, expected4)
  }, error = function(e) {
    err4 <<- e
    FALSE
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    ".selection_mask_from_scores: action = 'remove' inverts comparison mask",
    passed4, note4
  )
  
  # Case 5: NA scores are treated as FALSE before optional inversion
  err5 <- NULL
  passed5 <- tryCatch({
    m_keep <- .selection_mask_from_scores(
      scores        = scores,
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE
    )
    m_remove <- .selection_mask_from_scores(
      scores        = scores,
      threshold     = 0,
      direction     = "above",
      action        = "remove",
      include_equal = FALSE
    )
    (!m_keep["d"]) && m_remove["d"]
  }, error = function(e) {
    err5 <<- e
    FALSE
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    ".selection_mask_from_scores: NA scores handled as FALSE then inverted for action='remove'",
    passed5, note5
  )
}

###############################################################################
# Tests: fs_supervised Input Validation
###############################################################################

test_fs_supervised_input_validation <- function() {
  # Case 1: x not matrix/data.frame/data.table
  err1 <- NULL
  passed1 <- tryCatch({
    fs_supervised("not a valid x", y = 1:3)
    FALSE
  }, error = function(e) {
    err1 <<- e
    grepl("`x` must be a numeric matrix, data frame, or data.table", conditionMessage(e))
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_supervised: errors if x is not matrix/data.frame/data.table",
    passed1, note1
  )
  
  # Case 2: non-numeric column in data.frame
  err2 <- NULL
  passed2 <- tryCatch({
    df_bad <- data.frame(
      X = c(1, 2, 3),
      Y = c("a", "b", "c"),
      stringsAsFactors = FALSE
    )
    fs_supervised(df_bad, y = 1:3)
    FALSE
  }, error = function(e) {
    err2 <<- e
    grepl("All columns of the data frame must be numeric", conditionMessage(e))
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_supervised: errors if data.frame has non-numeric columns",
    passed2, note2
  )
  
  # Case 3: negative threshold
  err3 <- NULL
  passed3 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_supervised(X, y = 1:nrow(X), threshold = -0.01)
    FALSE
  }, error = function(e) {
    err3 <<- e
    grepl("must be a single non-negative, finite numeric value", conditionMessage(e))
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "fs_supervised: errors for negative threshold",
    passed3, note3
  )
  
  # Case 4: invalid direction
  err4 <- NULL
  passed4 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_supervised(X, y = 1:nrow(X), direction = "sideways")
    FALSE
  }, error = function(e) {
    err4 <<- e
    grepl("should be one of", conditionMessage(e))
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "fs_supervised: errors for invalid direction",
    passed4, note4
  )
  
  # Case 5: invalid action
  err5 <- NULL
  passed5 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_supervised(X, y = 1:nrow(X), action = "discard")
    FALSE
  }, error = function(e) {
    err5 <<- e
    grepl("should be one of", conditionMessage(e))
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    "fs_supervised: errors for invalid action",
    passed5, note5
  )
  
  # Case 6: non-logical include_equal
  err6 <- NULL
  passed6 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_supervised(X, y = 1:nrow(X), include_equal = 1L)
    FALSE
  }, error = function(e) {
    err6 <<- e
    grepl("must be a single non-NA logical value", conditionMessage(e))
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    "fs_supervised: errors when include_equal is not logical",
    passed6, note6
  )
  
  # Case 7: non-logical na_rm
  err7 <- NULL
  passed7 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_supervised(X, y = 1:nrow(X), na_rm = "TRUE")
    FALSE
  }, error = function(e) {
    err7 <<- e
    grepl("must be a single non-NA logical value", conditionMessage(e))
  })
  note7 <- if (!is.null(err7)) conditionMessage(err7) else NULL
  print_and_store_result(
    "fs_supervised: errors when na_rm is not logical",
    passed7, note7
  )
  
  # Case 8: invalid method
  err8 <- NULL
  passed8 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_supervised(X, y = 1:nrow(X), method = "not_a_method")
    FALSE
  }, error = function(e) {
    msg <- conditionMessage(e)
    err8 <<- e
    grepl("should be one of", msg, fixed = TRUE)
  })
  note8 <- if (!is.null(err8)) conditionMessage(err8) else NULL
  print_and_store_result(
    "fs_supervised: errors for invalid method",
    passed8, note8
  )
  
  # Case 9: invalid out
  err9 <- NULL
  passed9 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_supervised(X, y = 1:nrow(X), out = "not_an_output")
    FALSE
  }, error = function(e) {
    msg <- conditionMessage(e)
    err9 <<- e
    grepl("should be one of", msg, fixed = TRUE)
  })
  note9 <- if (!is.null(err9)) conditionMessage(err9) else NULL
  print_and_store_result(
    "fs_supervised: errors for invalid out type",
    passed9, note9
  )
  
  # Case 10: y length mismatch
  err10 <- NULL
  passed10 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_supervised(X, y = 1:(nrow(X) - 1))
    FALSE
  }, error = function(e) {
    err10 <<- e
    grepl("Length of `y` must equal number of rows in `x`", conditionMessage(e))
  })
  note10 <- if (!is.null(err10)) conditionMessage(err10) else NULL
  print_and_store_result(
    "fs_supervised: errors when y length does not match nrow(x)",
    passed10, note10
  )
  
  # Case 11: unsupported y type (list)
  err11 <- NULL
  passed11 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_supervised(X, y = as.list(1:nrow(X)))
    FALSE
  }, error = function(e) {
    err11 <<- e
    grepl("`y` must be numeric, factor, character, or logical", conditionMessage(e))
  })
  note11 <- if (!is.null(err11)) conditionMessage(err11) else NULL
  print_and_store_result(
    "fs_supervised: errors for unsupported y type",
    passed11, note11
  )
}

###############################################################################
# Tests: fs_supervised Basic Selection and Output Types
###############################################################################

test_fs_supervised_basic_selection <- function() {
  # Correlation-based selection
  err1 <- NULL
  passed1 <- tryCatch({
    X <- matrix(
      c(
        1, 2, 3, 4,  # f_pos
        4, 3, 2, 1,  # f_neg
        1, 1, 1, 1   # f_const
      ),
      nrow = 4,
      ncol = 3
    )
    colnames(X) <- c("f_pos", "f_neg", "f_const")
    y <- 1:4
    
    out <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "list"
    )
    
    expected_names <- c("f_neg", "f_pos")
    setequal(out$names, expected_names) &&
      is.matrix(out$filtered) &&
      ncol(out$filtered) == 2L &&
      nrow(out$filtered) == nrow(X) &&
      all(sort(colnames(out$filtered)) == sort(expected_names)) &&
      is.logical(out$mask) &&
      length(out$mask) == ncol(X) &&
      sum(out$mask) == 2L &&
      out$meta$method_arg  == "correlation" &&
      out$meta$method_used == "correlation" &&
      out$meta$n_input_cols == ncol(X) &&
      out$meta$n_kept_cols  == 2L
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_supervised: correlation-based selection keeps highly correlated features",
    passed1, note1
  )
  
  # ANOVA-based selection
  err2 <- NULL
  passed2 <- tryCatch({
    X2 <- matrix(
      c(
        1, 2, 1, 2,  # f_diff1
        10, 10, 10, 10, # f_const
        2, 2, 3, 3   # f_diff2
      ),
      nrow = 4,
      ncol = 3
    )
    colnames(X2) <- c("f_diff1", "f_const", "f_diff2")
    y_fac <- factor(c("A", "A", "B", "B"))
    
    out2 <- fs_supervised(
      x             = X2,
      y             = y_fac,
      method        = "anova",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "list"
    )
    expected2 <- c("f_diff1", "f_diff2")
    setequal(out2$names, expected2) &&
      out2$meta$method_arg  == "anova" &&
      out2$meta$method_used == "anova" &&
      out2$meta$n_input_cols == ncol(X2) &&
      out2$meta$n_kept_cols  == 2L
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_supervised: ANOVA-based selection keeps features differing across groups",
    passed2, note2
  )
}

test_fs_supervised_output_types <- function() {
  X <- matrix(
    c(
      1, 2, 3, 4,  # f_pos
      4, 3, 2, 1,  # f_neg
      1, 1, 1, 1   # f_const
    ),
    nrow = 4,
    ncol = 3
  )
  colnames(X) <- c("f_pos", "f_neg", "f_const")
  y <- 1:4
  
  err <- NULL
  passed <- tryCatch({
    mat <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "matrix"
    )
    dt  <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "dt"
    )
    df  <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "data.frame"
    )
    mask <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "mask"
    )
    idx  <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "indices"
    )
    nm   <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "names"
    )
    lst  <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "list"
    )
    
    ensure_package("data.table")
    
    nrow(mat) == nrow(X) &&
      ncol(mat) == length(nm) &&
      all(colnames(mat) %in% nm) &&
      is.logical(mask) &&
      length(mask) == ncol(X) &&
      sum(mask) == length(nm) &&
      identical(which(mask), idx) &&
      all(nm %in% names(lst$scores)[idx]) &&
      all.equal(mat, as.matrix(lst$filtered)) == TRUE &&
      data.table::is.data.table(dt) &&
      ncol(dt) == length(nm) &&
      all(names(dt) %in% nm) &&
      is.data.frame(df) &&
      ncol(df) == length(nm) &&
      all(names(df) %in% nm) &&
      lst$meta$n_kept_cols  == length(nm) &&
      lst$meta$n_input_cols == ncol(X)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_supervised: all out types consistent (matrix/dt/data.frame/mask/indices/names/list)",
    passed, note
  )
}

###############################################################################
# Tests: fs_supervised Input Types Equivalence
###############################################################################

test_fs_supervised_input_types_equivalence <- function() {
  set.seed(123)
  X_mat <- matrix(rnorm(40), ncol = 5)
  colnames(X_mat) <- paste0("V", 1:5)
  X_df  <- as.data.frame(X_mat)
  
  ensure_package("data.table")
  X_dt  <- data.table::as.data.table(X_df)
  
  y <- rnorm(nrow(X_mat))
  
  err <- NULL
  passed <- tryCatch({
    out_mat <- fs_supervised(
      x             = X_mat,
      y             = y,
      method        = "correlation",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      na_rm         = TRUE,
      out           = "matrix"
    )
    out_df <- fs_supervised(
      x             = X_df,
      y             = y,
      method        = "correlation",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      na_rm         = TRUE,
      out           = "matrix"
    )
    out_dt <- fs_supervised(
      x             = X_dt,
      y             = y,
      method        = "correlation",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      na_rm         = TRUE,
      out           = "matrix"
    )
    all.equal(out_mat, out_df) == TRUE &&
      all.equal(out_mat, out_dt) == TRUE &&
      identical(colnames(out_mat), colnames(X_mat))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_supervised: equivalent results for matrix, data.frame, and data.table inputs",
    passed, note
  )
}

###############################################################################
# Tests: fs_supervised Scores Alignment with compute_supervised_scores
###############################################################################

test_fs_supervised_scores_alignment <- function() {
  X <- matrix(
    c(
      1, 2, 3, 4,
      2, 3, 4, 5
    ),
    nrow = 4,
    ncol = 2
  )
  colnames(X) <- c("f1", "f2")
  y <- 1:4
  
  err <- NULL
  passed <- tryCatch({
    dt <- convert_to_datatable(as.data.frame(X))
    s_direct <- compute_supervised_scores(
      dt           = dt,
      y            = y,
      method       = "correlation",
      na_rm        = TRUE,
      log_progress = FALSE
    )
    out <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      na_rm         = TRUE,
      out           = "list",
      log_progress  = FALSE
    )
    all.equal(unname(out$scores), unname(s_direct)) == TRUE &&
      identical(names(out$scores), names(s_direct))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_supervised: scores in out$list match compute_supervised_scores",
    passed, note
  )
}

###############################################################################
# Tests: Auto Method Behavior in fs_supervised
###############################################################################

test_fs_supervised_auto_method_behavior <- function() {
  # Numeric y -> correlation
  err1 <- NULL
  passed1 <- tryCatch({
    X <- matrix(
      c(
        1, 2, 3, 4,
        4, 3, 2, 1
      ),
      nrow = 4,
      ncol = 2
    )
    colnames(X) <- c("f_pos", "f_neg")
    y <- 1:4
    out <- fs_supervised(
      x             = X,
      y             = y,
      method        = "auto",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      na_rm         = TRUE,
      out           = "list"
    )
    out$meta$method_arg == "auto" &&
      out$meta$method_used == "correlation"
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_supervised: method='auto' resolves to 'correlation' for numeric y",
    passed1, note1
  )
  
  # Factor y -> ANOVA
  err2 <- NULL
  passed2 <- tryCatch({
    X2 <- matrix(
      c(
        1, 2, 1, 2,
        3, 4, 3, 4
      ),
      nrow = 4,
      ncol = 2
    )
    colnames(X2) <- c("f1", "f2")
    y_fac <- factor(c("A", "A", "B", "B"))
    out2 <- fs_supervised(
      x             = X2,
      y             = y_fac,
      method        = "auto",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      na_rm         = TRUE,
      out           = "list"
    )
    out2$meta$method_arg == "auto" &&
      out2$meta$method_used == "anova"
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_supervised: method='auto' resolves to 'anova' for categorical y",
    passed2, note2
  )
}

###############################################################################
# Tests: Direction, Action, and include_equal in fs_supervised (correlation)
###############################################################################

test_fs_supervised_direction_action_include_equal <- function() {
  X <- matrix(
    c(
      1, 2, 3, 4,  # f_strong
      4, 3, 2, 1,  # f_strong_neg
      1, 0, 1, 0   # f_weaker
    ),
    nrow = 4,
    ncol = 3
  )
  colnames(X) <- c("f_strong", "f_strong_neg", "f_weaker")
  y <- 1:4
  
  # Case 1: keep features with |cor| > 0.9
  err1 <- NULL
  passed1 <- tryCatch({
    nm1 <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "names"
    )
    setequal(nm1, c("f_strong", "f_strong_neg"))
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_supervised (correlation): keep features with |cor| > threshold",
    passed1, note1
  )
  
  # Case 2: remove features with |cor| > 0.9 -> keep weaker
  err2 <- NULL
  passed2 <- tryCatch({
    nm2 <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "above",
      action        = "remove",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "names"
    )
    identical(nm2, "f_weaker")
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_supervised (correlation): action='remove' excludes features meeting condition",
    passed2, note2
  )
  
  # Case 3: keep features with |cor| < 0.9
  err3 <- NULL
  passed3 <- tryCatch({
    nm3 <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0.9,
      direction     = "below",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "names"
    )
    identical(nm3, "f_weaker")
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "fs_supervised (correlation): keep features with |cor| < threshold",
    passed3, note3
  )
}

###############################################################################
# Tests: NA Handling in fs_supervised (correlation & ANOVA)
###############################################################################

test_fs_supervised_na_handling <- function() {
  # Correlation: na_rm = TRUE vs FALSE
  X_cor <- data.frame(
    a = c(1, 2, NA, 4),   # has NA
    b = c(10, 10, 10, 10) # constant
  )
  y_cor <- c(1, 2, 3, 4)
  
  # Case 1: na_rm = TRUE -> keep only a
  err1 <- NULL
  passed1 <- tryCatch({
    nm1 <- fs_supervised(
      x             = X_cor,
      y             = y_cor,
      method        = "correlation",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "names"
    )
    identical(nm1, "a")
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_supervised (correlation): na_rm=TRUE retains feature with non-zero correlation ignoring NAs",
    passed1, note1
  )
  
  # Case 2: na_rm = FALSE -> no features kept (scores NA or 0)
  err2 <- NULL
  passed2 <- tryCatch({
    nm2 <- suppressWarnings(
      fs_supervised(
        x             = X_cor,
        y             = y_cor,
        method        = "correlation",
        threshold     = 0,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        na_rm         = FALSE,
        out           = "names"
      )
    )
    length(nm2) == 0L
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_supervised (correlation): na_rm=FALSE with NAs leads to no features kept",
    passed2, note2
  )
  
  # ANOVA: NA handling
  X_anova <- data.frame(
    a = c(1, 2, NA, 2),   # varies by group, one NA
    b = c(3, 3, 3, 3)     # constant
  )
  y_fac <- factor(c("A", "A", "B", "B"))
  
  # Case 3: na_rm = TRUE -> keep 'a'
  err3 <- NULL
  passed3 <- tryCatch({
    nm3 <- fs_supervised(
      x             = X_anova,
      y             = y_fac,
      method        = "anova",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "names"
    )
    identical(nm3, "a")
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "fs_supervised (anova): na_rm=TRUE keeps varying feature with NA ignored",
    passed3, note3
  )
  
  # Case 4: na_rm = FALSE -> 'lm' drops NAs internally; selection still keeps 'a'
  err4 <- NULL
  passed4 <- tryCatch({
    nm4 <- fs_supervised(
      x             = X_anova,
      y             = y_fac,
      method        = "anova",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = FALSE,
      out           = "names"
    )
    identical(nm4, "a")
  }, error = function(e) {
    err4 <<- e
    FALSE
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "fs_supervised (anova): na_rm=FALSE still retains varying feature",
    passed4, note4
  )
}

###############################################################################
# Tests: No Features Kept Branch (All out Types)
###############################################################################

test_fs_supervised_no_features_kept <- function() {
  set.seed(456)
  X <- matrix(rnorm(20), nrow = 5, ncol = 4)
  colnames(X) <- paste0("X", 1:4)
  y <- rnorm(nrow(X))
  
  err <- NULL
  passed <- tryCatch({
    # Choose a threshold > 1 so no correlation-based scores can pass
    mask <- suppressWarnings(
      fs_supervised(
        x             = X,
        y             = y,
        method        = "correlation",
        threshold     = 1.1,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        na_rm         = TRUE,
        out           = "mask"
      )
    )
    idx <- suppressWarnings(
      fs_supervised(
        x             = X,
        y             = y,
        method        = "correlation",
        threshold     = 1.1,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        na_rm         = TRUE,
        out           = "indices"
      )
    )
    nm <- suppressWarnings(
      fs_supervised(
        x             = X,
        y             = y,
        method        = "correlation",
        threshold     = 1.1,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        na_rm         = TRUE,
        out           = "names"
      )
    )
    mat <- suppressWarnings(
      fs_supervised(
        x             = X,
        y             = y,
        method        = "correlation",
        threshold     = 1.1,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        na_rm         = TRUE,
        out           = "matrix"
      )
    )
    dt  <- suppressWarnings(
      fs_supervised(
        x             = X,
        y             = y,
        method        = "correlation",
        threshold     = 1.1,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        na_rm         = TRUE,
        out           = "dt"
      )
    )
    df  <- suppressWarnings(
      fs_supervised(
        x             = X,
        y             = y,
        method        = "correlation",
        threshold     = 1.1,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        na_rm         = TRUE,
        out           = "data.frame"
      )
    )
    lst <- suppressWarnings(
      fs_supervised(
        x             = X,
        y             = y,
        method        = "correlation",
        threshold     = 1.1,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        na_rm         = TRUE,
        out           = "list"
      )
    )
    
    ensure_package("data.table")
    
    # Core invariants
    cond_mask_len        <- (length(mask) == ncol(X))
    cond_mask_all_false  <- all(mask == FALSE)
    cond_idx_len         <- (length(idx) == 0L)
    cond_nm_len          <- (length(nm) == 0L)
    
    # For the matrix output we require full row count and zero columns
    cond_mat_dim         <- (
      is.matrix(mat) &&
        nrow(mat) == nrow(X) &&
        ncol(mat) == 0L
    )
    
    # For the data.table and data.frame outputs we only require 0 columns.
    # Some data.table versions may return 0×0, others nrow(X)×0; both are acceptable.
    cond_dt_dim          <- (
      data.table::is.data.table(dt) &&
        ncol(dt) == 0L &&
        nrow(dt) %in% c(0L, nrow(X))
    )
    cond_df_dim          <- (
      is.data.frame(df) &&
        ncol(df) == 0L &&
        nrow(df) %in% c(0L, nrow(X))
    )
    
    # For the list we enforce the stronger "nrow(X) × 0" contract for filtered
    cond_lst_filtered    <- (
      is.matrix(lst$filtered) &&
        nrow(lst$filtered) == nrow(X) &&
        ncol(lst$filtered) == 0L
    )
    cond_meta_input_cols <- (lst$meta$n_input_cols == ncol(X))
    cond_meta_kept_cols  <- (lst$meta$n_kept_cols == 0L)
    
    cond_mask_len &&
      cond_mask_all_false &&
      cond_idx_len &&
      cond_nm_len &&
      cond_mat_dim &&
      cond_dt_dim &&
      cond_df_dim &&
      cond_lst_filtered &&
      cond_meta_input_cols &&
      cond_meta_kept_cols
  }, error = function(e) {
    err <<- e
    FALSE
  })
  
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_supervised: 'no features kept' path returns empty structures with correct dimensions and meta",
    passed, note
  )
}


###############################################################################
# Tests: All Features Kept Scenario
###############################################################################

test_fs_supervised_all_features_kept <- function() {
  X <- matrix(
    c(
      1, 2, 3, 4,
      2, 4, 6, 8,
      3, 6, 9, 12
    ),
    nrow = 4,
    ncol = 3
  )
  colnames(X) <- c("f1", "f2", "f3")
  y <- 1:4
  
  err <- NULL
  passed <- tryCatch({
    mask <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "mask"
    )
    all(mask) && length(mask) == ncol(X)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_supervised: all features kept when all scores exceed threshold",
    passed, note
  )
}

###############################################################################
# Tests: log_progress Behavior
###############################################################################

test_fs_supervised_log_progress <- function() {
  set.seed(789)
  X <- matrix(rnorm(30), ncol = 3)
  colnames(X) <- paste0("F", 1:3)
  y <- rnorm(nrow(X))
  
  err <- NULL
  passed <- tryCatch({
    out_quiet <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "list",
      log_progress  = FALSE
    )
    out_verbose <- fs_supervised(
      x             = X,
      y             = y,
      method        = "correlation",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      out           = "list",
      log_progress  = TRUE
    )
    all.equal(out_quiet, out_verbose) == TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_supervised: log_progress controls messaging only (outputs identical)",
    passed, note
  )
}

###############################################################################
# Run All fs_supervised Tests
###############################################################################

run_fs_supervised_tests <- function() {
  cat("========== Running fs_supervised Tests ==========\n")
  test_fs_supervised_existence()
  test_fs_supervised_helpers_existence()
  test_ensure_package()
  test_validation_helpers()
  test_convert_to_datatable()
  test_compute_supervised_scores()
  test_selection_mask_from_scores()
  test_fs_supervised_input_validation()
  test_fs_supervised_basic_selection()
  test_fs_supervised_output_types()
  test_fs_supervised_input_types_equivalence()
  test_fs_supervised_scores_alignment()
  test_fs_supervised_auto_method_behavior()
  test_fs_supervised_direction_action_include_equal()
  test_fs_supervised_na_handling()
  test_fs_supervised_no_features_kept()
  test_fs_supervised_all_features_kept()
  test_fs_supervised_log_progress()
  cat("========== fs_supervised Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment the following line to run all tests when this script is executed:
# run_fs_supervised_tests()
