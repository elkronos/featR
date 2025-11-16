###############################################################################
# Testing Infrastructure - Unsupervised Filter-Based Feature Selection (fs_unsupervised)
###############################################################################

# Optional: if fs_unsupervised is in a separate script/package, load it here, e.g.:
# source("fs_unsupervised.R")
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

test_fs_unsupervised_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    exists("fs_unsupervised") && is.function(fs_unsupervised)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_unsupervised: function exists and is callable", passed, note)
}

test_fs_unsupervised_helpers_existence <- function() {
  helpers <- c(
    "ensure_package",
    ".validate_threshold",
    ".validate_logical_flag",
    ".validate_action",
    ".validate_direction",
    "convert_to_datatable",
    "compute_unsupervised_scores",
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
  print_and_store_result("fs_unsupervised: helper functions exist and are callable", passed, note)
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
# Tests: compute_unsupervised_scores
###############################################################################

test_compute_unsupervised_scores <- function() {
  # Set up numeric data without NA for most methods
  df <- data.frame(
    a = c(1, 2, 3, 4),
    b = c(2, 4, 6, 8),
    c = c(5, 5, 5, 5)
  )
  dt <- convert_to_datatable(df)
  
  # variance
  err1 <- NULL
  passed1 <- tryCatch({
    s_var <- compute_unsupervised_scores(dt, method = "variance", na_rm = TRUE)
    exp_var <- vapply(dt, function(col) stats::var(col, na.rm = TRUE), numeric(1))
    all.equal(unname(s_var), unname(exp_var)) == TRUE
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "compute_unsupervised_scores: variance matches stats::var",
    passed1, note1
  )
  
  # MAD
  err2 <- NULL
  passed2 <- tryCatch({
    s_mad <- compute_unsupervised_scores(dt, method = "mad", na_rm = TRUE)
    exp_mad <- vapply(dt, function(col) stats::mad(col, na.rm = TRUE), numeric(1))
    all.equal(unname(s_mad), unname(exp_mad)) == TRUE
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "compute_unsupervised_scores: mad matches stats::mad",
    passed2, note2
  )
  
  # IQR
  err3 <- NULL
  passed3 <- tryCatch({
    s_iqr <- compute_unsupervised_scores(dt, method = "iqr", na_rm = TRUE)
    exp_iqr <- vapply(dt, function(col) stats::IQR(col, na.rm = TRUE), numeric(1))
    all.equal(unname(s_iqr), unname(exp_iqr)) == TRUE
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "compute_unsupervised_scores: iqr matches stats::IQR",
    passed3, note3
  )
  
  # range (max - min)
  err4 <- NULL
  passed4 <- tryCatch({
    s_range <- compute_unsupervised_scores(dt, method = "range", na_rm = TRUE)
    exp_range <- vapply(
      dt,
      function(col) {
        col2 <- col[!is.na(col)]
        if (length(col2) == 0L) return(NA_real_)
        diff(range(col2))
      },
      numeric(1)
    )
    all.equal(unname(s_range), unname(exp_range)) == TRUE
  }, error = function(e) {
    err4 <<- e
    FALSE
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "compute_unsupervised_scores: range matches max-min with na_rm = TRUE",
    passed4, note4
  )
  
  # Data with NAs for missing_prop and n_unique
  df_na <- data.frame(
    x = c(1, NA, 2, NA),
    y = c(NA_real_, NA_real_, NA_real_, NA_real_)  # ensure numeric column of all NAs
  )
  dt_na <- convert_to_datatable(df_na)
  
  # missing_prop
  err5 <- NULL
  passed5 <- tryCatch({
    s_missing <- compute_unsupervised_scores(dt_na, method = "missing_prop", na_rm = TRUE)
    exp_missing <- vapply(
      dt_na,
      function(col) {
        n <- length(col)
        if (n == 0L) return(NA_real_)
        sum(is.na(col)) / n
      },
      numeric(1)
    )
    all.equal(unname(s_missing), unname(exp_missing)) == TRUE
  }, error = function(e) {
    err5 <<- e
    FALSE
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    "compute_unsupervised_scores: missing_prop matches proportion of NAs",
    passed5, note5
  )
  
  # n_unique
  err6 <- NULL
  passed6 <- tryCatch({
    s_nuniq <- compute_unsupervised_scores(dt_na, method = "n_unique", na_rm = TRUE)
    exp_nuniq <- vapply(
      dt_na,
      function(col) length(unique(col[!is.na(col)])),
      numeric(1)
    )
    all.equal(unname(s_nuniq), unname(exp_nuniq)) == TRUE
  }, error = function(e) {
    err6 <<- e
    FALSE
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    "compute_unsupervised_scores: n_unique matches unique non-NA counts",
    passed6, note6
  )
  
  # dt must be a data.table
  err7 <- NULL
  passed7 <- tryCatch({
    bad_dt <- data.frame(a = 1:3)
    compute_unsupervised_scores(bad_dt, method = "variance")
    FALSE
  }, error = function(e) {
    err7 <<- e
    grepl("`dt` must be a data.table", conditionMessage(e))
  })
  note7 <- if (!is.null(err7)) conditionMessage(err7) else NULL
  print_and_store_result(
    "compute_unsupervised_scores: errors if dt is not a data.table",
    passed7, note7
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
    ".selection_mask_from_scores: NA scores handled as FALSE then inverted for action = 'remove'",
    passed5, note5
  )
}

###############################################################################
# Tests: fs_unsupervised Input Validation
###############################################################################

test_fs_unsupervised_input_validation <- function() {
  # Case 1: x not matrix/data.frame/data.table
  err1 <- NULL
  passed1 <- tryCatch({
    fs_unsupervised("not a valid x")
    FALSE
  }, error = function(e) {
    err1 <<- e
    grepl("`x` must be a numeric matrix, data frame, or data.table", conditionMessage(e))
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_unsupervised: errors if x is not matrix/data.frame/data.table",
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
    fs_unsupervised(df_bad)
    FALSE
  }, error = function(e) {
    err2 <<- e
    grepl("All columns of the data frame must be numeric", conditionMessage(e))
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_unsupervised: errors if data.frame has non-numeric columns",
    passed2, note2
  )
  
  # Case 3: negative threshold
  err3 <- NULL
  passed3 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_unsupervised(X, threshold = -0.01)
    FALSE
  }, error = function(e) {
    err3 <<- e
    grepl("must be a single non-negative, finite numeric value", conditionMessage(e))
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "fs_unsupervised: errors for negative threshold",
    passed3, note3
  )
  
  # Case 4: invalid direction
  err4 <- NULL
  passed4 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_unsupervised(X, direction = "sideways")
    FALSE
  }, error = function(e) {
    err4 <<- e
    grepl("should be one of", conditionMessage(e))
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "fs_unsupervised: errors for invalid direction",
    passed4, note4
  )
  
  # Case 5: invalid action
  err5 <- NULL
  passed5 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_unsupervised(X, action = "discard")
    FALSE
  }, error = function(e) {
    err5 <<- e
    grepl("should be one of", conditionMessage(e))
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    "fs_unsupervised: errors for invalid action",
    passed5, note5
  )
  
  # Case 6: non-logical include_equal
  err6 <- NULL
  passed6 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_unsupervised(X, include_equal = 1L)
    FALSE
  }, error = function(e) {
    err6 <<- e
    grepl("must be a single non-NA logical value", conditionMessage(e))
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    "fs_unsupervised: errors when include_equal is not logical",
    passed6, note6
  )
  
  # Case 7: non-logical na_rm
  err7 <- NULL
  passed7 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_unsupervised(X, na_rm = "TRUE")
    FALSE
  }, error = function(e) {
    err7 <<- e
    grepl("must be a single non-NA logical value", conditionMessage(e))
  })
  note7 <- if (!is.null(err7)) conditionMessage(err7) else NULL
  print_and_store_result(
    "fs_unsupervised: errors when na_rm is not logical",
    passed7, note7
  )
  
  # Case 8: invalid method
  err8 <- NULL
  passed8 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_unsupervised(X, method = "not_a_method")
    FALSE
  }, error = function(e) {
    msg <- conditionMessage(e)
    err8 <<- e
    grepl("should be one of", msg, fixed = TRUE)
  })
  note8 <- if (!is.null(err8)) conditionMessage(err8) else NULL
  print_and_store_result(
    "fs_unsupervised: errors for invalid method",
    passed8, note8
  )
  
  # Case 9: invalid output
  err9 <- NULL
  passed9 <- tryCatch({
    X <- matrix(rnorm(12), ncol = 3)
    fs_unsupervised(X, output = "not_an_output")
    FALSE
  }, error = function(e) {
    msg <- conditionMessage(e)
    err9 <<- e
    grepl("should be one of", msg, fixed = TRUE)
  })
  note9 <- if (!is.null(err9)) conditionMessage(err9) else NULL
  print_and_store_result(
    "fs_unsupervised: errors for invalid output type",
    passed9, note9
  )
}

###############################################################################
# Tests: fs_unsupervised Basic Selection and Output Types
###############################################################################

test_fs_unsupervised_basic_selection <- function() {
  X <- matrix(
    c(
      1, 1, 1, 1,  # const1
      0, 1, 2, 3,  # var1
      5, 5, 5, 5   # const2
    ),
    nrow = 4,
    ncol = 3
  )
  colnames(X) <- c("const1", "var1", "const2")
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "list"
    )
    identical(out$names, "var1") &&
      is.matrix(out$filtered) &&
      ncol(out$filtered) == 1L &&
      nrow(out$filtered) == nrow(X) &&
      identical(colnames(out$filtered), "var1") &&
      is.logical(out$mask) &&
      length(out$mask) == ncol(X) &&
      sum(out$mask) == 1L &&
      out$meta$method == "variance" &&
      out$meta$n_input_cols == ncol(X) &&
      out$meta$n_kept_cols == 1L
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_unsupervised: variance-based selection keeps non-constant feature",
    passed, note
  )
}

test_fs_unsupervised_output_types <- function() {
  X <- matrix(
    c(
      1, 1, 1, 1,  # const1
      0, 1, 2, 3,  # var1
      5, 5, 5, 5   # const2
    ),
    nrow = 4,
    ncol = 3
  )
  colnames(X) <- c("const1", "var1", "const2")
  
  err <- NULL
  passed <- tryCatch({
    # Use same selection parameters as basic selection test
    mat <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "matrix"
    )
    dt  <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "dt"
    )
    df  <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "data.frame"
    )
    mask <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "mask"
    )
    idx  <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "indices"
    )
    nm   <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "names"
    )
    lst  <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "list"
    )
    
    ensure_package("data.table")
    
    # Consistency checks
    nrow(mat) == nrow(X) &&
      ncol(mat) == 1L &&
      identical(colnames(mat), nm) &&
      is.logical(mask) &&
      length(mask) == ncol(X) &&
      sum(mask) == 1L &&
      identical(which(mask), idx) &&
      identical(nm, names(lst$scores)[idx]) &&
      all.equal(mat, as.matrix(lst$filtered)) == TRUE &&
      data.table::is.data.table(dt) &&
      ncol(dt) == 1L &&
      identical(names(dt), nm) &&
      is.data.frame(df) &&
      ncol(df) == 1L &&
      identical(names(df), nm) &&
      lst$meta$n_kept_cols == 1L &&
      lst$meta$n_input_cols == ncol(X)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_unsupervised: all output types consistent (matrix/dt/data.frame/mask/indices/names/list)",
    passed, note
  )
}

###############################################################################
# Tests: fs_unsupervised Input Types Equivalence
###############################################################################

test_fs_unsupervised_input_types_equivalence <- function() {
  set.seed(123)
  X_mat <- matrix(rnorm(40), ncol = 5)
  colnames(X_mat) <- paste0("V", 1:5)
  X_df  <- as.data.frame(X_mat)
  
  ensure_package("data.table")
  X_dt  <- data.table::as.data.table(X_df)
  
  err <- NULL
  passed <- tryCatch({
    out_mat <- fs_unsupervised(
      x             = X_mat,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      na_rm         = TRUE,
      output        = "matrix"
    )
    out_df <- fs_unsupervised(
      x             = X_df,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      na_rm         = TRUE,
      output        = "matrix"
    )
    out_dt <- fs_unsupervised(
      x             = X_dt,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      na_rm         = TRUE,
      output        = "matrix"
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
    "fs_unsupervised: equivalent results for matrix, data.frame, and data.table inputs",
    passed, note
  )
}

###############################################################################
# Tests: fs_unsupervised Scores Alignment with compute_unsupervised_scores
###############################################################################

test_fs_unsupervised_scores_alignment <- function() {
  X <- matrix(
    c(
      1, 2, 3, 4,
      2, 3, 4, 5
    ),
    nrow = 4,
    ncol = 2
  )
  colnames(X) <- c("f1", "f2")
  
  err <- NULL
  passed <- tryCatch({
    dt <- convert_to_datatable(X)
    s_direct <- compute_unsupervised_scores(
      dt,
      method       = "mad",
      na_rm        = TRUE,
      log_progress = FALSE
    )
    out <- fs_unsupervised(
      x             = X,
      method        = "mad",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      na_rm         = TRUE,
      output        = "list",
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
    "fs_unsupervised: scores in output$list match compute_unsupervised_scores",
    passed, note
  )
}

###############################################################################
# Tests: Direction, Action, and include_equal Combinations (n_unique method)
###############################################################################

test_fs_unsupervised_direction_action_include_equal <- function() {
  X <- data.frame(
    a = c(1, 1, 1, 1),   # n_unique = 1
    b = c(1, 2, 1, 2),   # n_unique = 2
    c = c(1, 2, 3, 4)    # n_unique = 4
  )
  
  # Case 1: above, include_equal = FALSE, keep -> only c
  err1 <- NULL
  passed1 <- tryCatch({
    nm1 <- fs_unsupervised(
      x             = X,
      method        = "n_unique",
      threshold     = 2,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      output        = "names"
    )
    identical(nm1, "c")
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_unsupervised (n_unique): keep features with n_unique > threshold",
    passed1, note1
  )
  
  # Case 2: above, include_equal = TRUE, keep -> b and c
  err2 <- NULL
  passed2 <- tryCatch({
    nm2 <- fs_unsupervised(
      x             = X,
      method        = "n_unique",
      threshold     = 2,
      direction     = "above",
      action        = "keep",
      include_equal = TRUE,
      output        = "names"
    )
    identical(sort(nm2), c("b", "c"))
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_unsupervised (n_unique): include_equal = TRUE includes features with n_unique == threshold",
    passed2, note2
  )
  
  # Case 3: below, include_equal = FALSE, keep -> only a
  err3 <- NULL
  passed3 <- tryCatch({
    nm3 <- fs_unsupervised(
      x             = X,
      method        = "n_unique",
      threshold     = 2,
      direction     = "below",
      action        = "keep",
      include_equal = FALSE,
      output        = "names"
    )
    identical(nm3, "a")
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "fs_unsupervised (n_unique): keep features with n_unique < threshold",
    passed3, note3
  )
  
  # Case 4: above, include_equal = FALSE, remove -> keep a and b
  err4 <- NULL
  passed4 <- tryCatch({
    nm4 <- fs_unsupervised(
      x             = X,
      method        = "n_unique",
      threshold     = 2,
      direction     = "above",
      action        = "remove",
      include_equal = FALSE,
      output        = "names"
    )
    identical(sort(nm4), c("a", "b"))
  }, error = function(e) {
    err4 <<- e
    FALSE
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "fs_unsupervised (n_unique): action = 'remove' excludes features meeting condition",
    passed4, note4
  )
}

###############################################################################
# Tests: NA Handling (variance method)
###############################################################################

test_fs_unsupervised_na_handling <- function() {
  X <- data.frame(
    a = c(1, 2, NA, 4),   # has NA, variance > 0 when na_rm = TRUE
    b = c(10, 10, 10, 10) # constant, variance = 0
  )
  
  # Case 1: na_rm = TRUE -> keep only a (variance > 0)
  err1 <- NULL
  passed1 <- tryCatch({
    nm1 <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "names"
    )
    identical(nm1, "a")
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_unsupervised (variance): na_rm = TRUE retains feature with non-zero variance ignoring NAs",
    passed1, note1
  )
  
  # Case 2: na_rm = FALSE -> due to NA, feature a has NA variance;
  # feature b has variance 0; with threshold = 0 and 'above', no features kept.
  err2 <- NULL
  passed2 <- tryCatch({
    nm2 <- suppressWarnings(
      fs_unsupervised(
        x             = X,
        method        = "variance",
        threshold     = 0,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        na_rm         = FALSE,
        output        = "names"
      )
    )
    length(nm2) == 0L
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_unsupervised (variance): na_rm = FALSE with NAs leads to no features kept",
    passed2, note2
  )
}

###############################################################################
# Tests: missing_prop and n_unique Behavior in fs_unsupervised
###############################################################################

test_fs_unsupervised_missing_prop_and_n_unique <- function() {
  # Data for missing_prop
  X_missing <- data.frame(
    f1 = c(1, NA, 2, NA),  # missing_prop = 0.5
    f2 = c(1, 2, 3, 4)     # missing_prop = 0
  )
  
  err1 <- NULL
  passed1 <- tryCatch({
    nm1 <- fs_unsupervised(
      x             = X_missing,
      method        = "missing_prop",
      threshold     = 0.25,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      output        = "names"
    )
    identical(nm1, "f1")
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "fs_unsupervised (missing_prop): keeps features with missing_prop > threshold",
    passed1, note1
  )
  
  # Data for n_unique
  X_uni <- data.frame(
    u1 = c(1, 1, 1, 1),   # n_unique = 1
    u2 = c(1, 2, 1, 2),   # n_unique = 2
    u3 = c(1, 2, 3, 4)    # n_unique = 4
  )
  
  err2 <- NULL
  passed2 <- tryCatch({
    nm2 <- fs_unsupervised(
      x             = X_uni,
      method        = "n_unique",
      threshold     = 2,
      direction     = "below",
      action        = "keep",
      include_equal = TRUE,
      output        = "names"
    )
    identical(sort(nm2), c("u1", "u2"))
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "fs_unsupervised (n_unique): keeps features with n_unique <= threshold when include_equal = TRUE",
    passed2, note2
  )
}

###############################################################################
# Tests: No Features Kept Branch (All Outputs) + Debug
###############################################################################

test_fs_unsupervised_no_features_kept <- function() {
  set.seed(456)
  X <- matrix(rnorm(20), nrow = 5, ncol = 4)
  colnames(X) <- paste0("X", 1:4)
  
  err <- NULL
  passed <- tryCatch({
    # Choose an extremely high threshold to ensure no features pass
    mask <- suppressWarnings(
      fs_unsupervised(
        x             = X,
        method        = "variance",
        threshold     = 1e6,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        output        = "mask"
      )
    )
    idx <- suppressWarnings(
      fs_unsupervised(
        x             = X,
        method        = "variance",
        threshold     = 1e6,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        output        = "indices"
      )
    )
    nm <- suppressWarnings(
      fs_unsupervised(
        x             = X,
        method        = "variance",
        threshold     = 1e6,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        output        = "names"
      )
    )
    mat <- suppressWarnings(
      fs_unsupervised(
        x             = X,
        method        = "variance",
        threshold     = 1e6,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        output        = "matrix"
      )
    )
    dt  <- suppressWarnings(
      fs_unsupervised(
        x             = X,
        method        = "variance",
        threshold     = 1e6,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        output        = "dt"
      )
    )
    df  <- suppressWarnings(
      fs_unsupervised(
        x             = X,
        method        = "variance",
        threshold     = 1e6,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        output        = "data.frame"
      )
    )
    lst <- suppressWarnings(
      fs_unsupervised(
        x             = X,
        method        = "variance",
        threshold     = 1e6,
        direction     = "above",
        action        = "keep",
        include_equal = FALSE,
        output        = "list"
      )
    )
    
    ensure_package("data.table")
    
    cond_mask_len        <- (length(mask) == ncol(X))
    cond_mask_all_false  <- all(mask == FALSE)
    cond_idx_len         <- (length(idx) == 0L)
    cond_nm_len          <- (length(nm) == 0L)
    cond_mat_dim         <- (is.matrix(mat) && nrow(mat) == nrow(X) && ncol(mat) == 0L)
    # data.table cannot meaningfully have rows with zero columns; expect 0x0
    cond_dt_dim          <- (data.table::is.data.table(dt) && nrow(dt) == 0L && ncol(dt) == 0L)
    cond_df_dim          <- (is.data.frame(df) && nrow(df) == nrow(X) && ncol(df) == 0L)
    cond_lst_filtered    <- (is.matrix(lst$filtered) &&
                               nrow(lst$filtered) == nrow(X) &&
                               ncol(lst$filtered) == 0L)
    cond_meta_input_cols <- (lst$meta$n_input_cols == ncol(X))
    cond_meta_kept_cols  <- (lst$meta$n_kept_cols == 0L)
    
    # You can keep or remove this debug block as you prefer
    cat(
      "DEBUG no_features_kept:\n",
      "  mask:", mask, "\n",
      "  idx:", idx, "\n",
      "  nm:", nm, "\n",
      "  dim(mat):", paste(dim(mat), collapse = "x"), "\n",
      "  dim(dt):", paste(dim(dt), collapse = "x"), "\n",
      "  dim(df):", paste(dim(df), collapse = "x"), "\n",
      "  dim(lst$filtered):", paste(dim(lst$filtered), collapse = "x"), "\n",
      "  meta$n_input_cols:", lst$meta$n_input_cols, "\n",
      "  meta$n_kept_cols:", lst$meta$n_kept_cols, "\n",
      "  cond_mask_len:", cond_mask_len, "\n",
      "  cond_mask_all_false:", cond_mask_all_false, "\n",
      "  cond_idx_len:", cond_idx_len, "\n",
      "  cond_nm_len:", cond_nm_len, "\n",
      "  cond_mat_dim:", cond_mat_dim, "\n",
      "  cond_dt_dim:", cond_dt_dim, "\n",
      "  cond_df_dim:", cond_df_dim, "\n",
      "  cond_lst_filtered:", cond_lst_filtered, "\n",
      "  cond_meta_input_cols:", cond_meta_input_cols, "\n",
      "  cond_meta_kept_cols:", cond_meta_kept_cols, "\n",
      sep = ""
    )
    
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
    "fs_unsupervised: 'no features kept' path returns empty structures with correct dimensions and meta",
    passed, note
  )
}

###############################################################################
# Tests: All Features Kept Scenario
###############################################################################

test_fs_unsupervised_all_features_kept <- function() {
  X <- matrix(
    c(
      1, 2, 3, 4,
      2, 3, 4, 5
    ),
    nrow = 4,
    ncol = 2
  )
  colnames(X) <- c("f1", "f2")
  
  err <- NULL
  passed <- tryCatch({
    mask <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "mask"
    )
    all(mask) &&
      length(mask) == ncol(X)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_unsupervised: all features kept when all scores exceed threshold",
    passed, note
  )
}

###############################################################################
# Tests: log_progress Behavior
###############################################################################

test_fs_unsupervised_log_progress <- function() {
  set.seed(789)
  X <- matrix(rnorm(30), ncol = 3)
  colnames(X) <- paste0("F", 1:3)
  
  err <- NULL
  passed <- tryCatch({
    out_quiet <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "list",
      log_progress  = FALSE
    )
    # log_progress = TRUE should not alter outputs
    out_verbose <- fs_unsupervised(
      x             = X,
      method        = "variance",
      threshold     = 0,
      direction     = "above",
      action        = "keep",
      include_equal = FALSE,
      na_rm         = TRUE,
      output        = "list",
      log_progress  = TRUE
    )
    all.equal(out_quiet, out_verbose) == TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_unsupervised: log_progress controls messaging only (outputs identical)",
    passed, note
  )
}

###############################################################################
# Run All fs_unsupervised Tests
###############################################################################

run_fs_unsupervised_tests <- function() {
  cat("========== Running fs_unsupervised Tests ==========\n")
  test_fs_unsupervised_existence()
  test_fs_unsupervised_helpers_existence()
  test_ensure_package()
  test_validation_helpers()
  test_convert_to_datatable()
  test_compute_unsupervised_scores()
  test_selection_mask_from_scores()
  test_fs_unsupervised_input_validation()
  test_fs_unsupervised_basic_selection()
  test_fs_unsupervised_output_types()
  test_fs_unsupervised_input_types_equivalence()
  test_fs_unsupervised_scores_alignment()
  test_fs_unsupervised_direction_action_include_equal()
  test_fs_unsupervised_na_handling()
  test_fs_unsupervised_missing_prop_and_n_unique()
  test_fs_unsupervised_no_features_kept()
  test_fs_unsupervised_all_features_kept()
  test_fs_unsupervised_log_progress()
  cat("========== fs_unsupervised Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment the following line to run all tests when this script is executed:
# run_fs_unsupervised_tests()
