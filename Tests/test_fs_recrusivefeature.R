###############################################################################
# Testing Infrastructure - Recursive Feature Elimination
###############################################################################

# Global object to store test results
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

#' Print and Store Test Result
#'
#' Prints and records the outcome of a test.
#'
#' @param test_name A character string naming the test.
#' @param passed Logical. Whether the test passed.
#' @param message Optional character scalar with additional information.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' print_and_store_result("Sample test", TRUE, "All good")
print_and_store_result <- function(test_name, passed, message = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-50s [%s]\n", test_name, result))
  if (!is.null(message)) {
    cat("  ", message, "\n")
  }
  assign(
    "test_results",
    rbind(test_results, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)),
    envir = .GlobalEnv
  )
  invisible(NULL)
}

#' Test validate_rfe_control
#'
#' Exercises `validate_rfe_control()` with valid and invalid inputs.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' test_validate_rfe_control()
test_validate_rfe_control <- function() {
  valid_input <- tryCatch({
    validate_rfe_control(list(method = "cv", number = 3))
    TRUE
  }, error = function(e) FALSE)
  
  not_list <- tryCatch({
    validate_rfe_control("nope")
    FALSE
  }, error = function(e) TRUE)
  
  missing_number <- tryCatch({
    validate_rfe_control(list(method = "cv"))
    FALSE
  }, error = function(e) TRUE)
  
  bad_method <- tryCatch({
    validate_rfe_control(list(method = "invalid", number = 3))
    FALSE
  }, error = function(e) TRUE)
  
  bad_number <- tryCatch({
    validate_rfe_control(list(method = "cv", number = 0))
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("validate_rfe_control: Valid input", valid_input)
  print_and_store_result("validate_rfe_control: Not a list", not_list)
  print_and_store_result("validate_rfe_control: Missing 'number'", missing_number)
  print_and_store_result("validate_rfe_control: Invalid method", bad_method)
  print_and_store_result("validate_rfe_control: Invalid number", bad_number)
  invisible(NULL)
}

#' Test validate_train_control
#'
#' Exercises `validate_train_control()` for accepted and rejected inputs.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' test_validate_train_control()
test_validate_train_control <- function() {
  valid_cv <- tryCatch({
    validate_train_control(list(method = "cv", number = 3))
    TRUE
  }, error = function(e) FALSE)
  
  valid_none <- tryCatch({
    validate_train_control(list(method = "none"))
    TRUE
  }, error = function(e) FALSE)
  
  missing_method <- tryCatch({
    validate_train_control(list(number = 3))
    FALSE
  }, error = function(e) TRUE)
  
  bad_number <- tryCatch({
    validate_train_control(list(method = "cv", number = -2))
    FALSE
  }, error = function(e) TRUE)
  
  bad_method <- tryCatch({
    validate_train_control(list(method = "xyz", number = 3))
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("validate_train_control: Valid cv", valid_cv)
  print_and_store_result("validate_train_control: Valid none", valid_none)
  print_and_store_result("validate_train_control: Missing 'method'", missing_method)
  print_and_store_result("validate_train_control: Invalid number", bad_number)
  print_and_store_result("validate_train_control: Invalid method", bad_method)
  invisible(NULL)
}

#' Test split_data
#'
#' Verifies that `split_data()` returns 80/20 partitions and preserves the response name.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' test_split_data_fn()
test_split_data_fn <- function() {
  set.seed(123)
  df <- data.frame(y = rnorm(120), x1 = rnorm(120))
  sp <- split_data(df, response_var = "y", seed = 42)
  correct_train_rows <- nrow(sp$train) == 96
  correct_response <- identical(sp$response_name, "y")
  print_and_store_result("split_data: 80/20 split size", correct_train_rows)
  print_and_store_result("split_data: response_name returned", correct_response)
  invisible(NULL)
}

#' Test fit/apply one-hot encoder
#'
#' Checks that `fit_one_hot_encoder()` and `apply_one_hot_encoder()` encode predictors and keep response as first column.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' test_one_hot_encoding()
test_one_hot_encoding <- function() {
  tr <- data.frame(y = 1:5,
                   a = factor(c("A","B","A","C","B")),
                   b = c(0,1,0,1,0))
  dv <- fit_one_hot_encoder(tr, response_name = "y")
  enc <- apply_one_hot_encoder(tr, response_name = "y", dv = dv)
  
  response_first <- identical(colnames(enc)[1], "y")
  all_numeric <- all(vapply(enc[-1], is.numeric, logical(1)))
  extra_cols <- ncol(enc) > ncol(tr)
  
  print_and_store_result("one-hot: response first column", response_first)
  print_and_store_result("one-hot: encoded predictors numeric", all_numeric)
  print_and_store_result("one-hot: produced additional columns", extra_cols)
  invisible(NULL)
}

#' Test perform_rfe
#'
#' Runs `perform_rfe()` on a simple regression problem using `caret::lmFuncs`.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' test_perform_rfe_fn()
test_perform_rfe_fn <- function() {
  set.seed(1)
  tr <- data.frame(y = rnorm(60), x1 = rnorm(60), x2 = rnorm(60), x3 = rnorm(60))
  # Ensure response first column for clarity
  tr <- tr[, c("y","x1","x2","x3")]
  ok <- tryCatch({
    rfe_obj <- perform_rfe(
      train_df = tr,
      response_name = "y",
      sizes = c(1,2,3),
      rfe_control_params = list(method = "cv", number = 3),
      feature_funcs = caret::lmFuncs,
      parallel = FALSE,
      early_stop = FALSE
    )
    inherits(rfe_obj, "rfe") && length(rfe_obj$optVariables) >= 1
  }, error = function(e) {
    FALSE
  })
  print_and_store_result("perform_rfe: basic regression RFE", ok)
  invisible(NULL)
}

#' Test train_final_model
#'
#' Trains a final model with selected variables using `caret::train`.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' test_train_final_model_fn()
test_train_final_model_fn <- function() {
  set.seed(2)
  df <- data.frame(y = rnorm(80), a = rnorm(80), b = rnorm(80))
  ok <- tryCatch({
    mdl <- train_final_model(
      data_df = df,
      response_name = "y",
      optimal_vars = c("a","b"),
      train_control_params = list(method = "cv", number = 3),
      model_method = "lm"
    )
    inherits(mdl, "train")
  }, error = function(e) FALSE)
  print_and_store_result("train_final_model: linear regression", ok)
  invisible(NULL)
}

#' Test fs_recursivefeature (integration)
#'
#' Full workflow test: split, optional encoding, RFE, and optional final model training.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' test_fs_recursivefeature_fn()
test_fs_recursivefeature_fn <- function() {
  set.seed(42)
  n <- 120
  dat <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    cat = factor(sample(LETTERS[1:3], n, TRUE))
  )
  
  ok <- tryCatch({
    res <- fs_recursivefeature(
      data = dat,
      response_var = "y",
      seed = 42,
      rfe_control = list(method = "cv", number = 3),
      train_control = list(method = "cv", number = 3),
      sizes = c(1,2,3,4),
      parallel = FALSE,
      feature_funcs = caret::lmFuncs,
      handle_categorical = TRUE,
      return_final_model = TRUE,
      model_method = "lm"
    )
    is.list(res) &&
      !is.null(res$RFE) &&
      length(res$OptimalVariables) >= 1 &&
      (is.null(res$FinalModel) || inherits(res$FinalModel, "train"))
  }, error = function(e) FALSE)
  
  print_and_store_result("fs_recursivefeature: end-to-end (regression + encoding)", ok)
  invisible(NULL)
}

#' Test classification path (optional)
#'
#' Verifies classification handling in split and RFE using a simple two-class target.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' test_classification_path_fn()
test_classification_path_fn <- function() {
  set.seed(101)
  n <- 100
  df <- data.frame(
    y = factor(sample(c("A","B"), n, TRUE)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  sp <- split_data(df, response_var = "y", seed = 101)
  ok_split_factor <- is.factor(sp$train$y)
  
  # Use rfFuncs if available; fall back to lmFuncs on numeric encoding of y (not recommended), so prefer rfFuncs
  ok_rfe <- tryCatch({
    rfe_obj <- perform_rfe(
      train_df = sp$train,
      response_name = "y",
      sizes = c(1,2),
      rfe_control_params = list(method = "cv", number = 3),
      feature_funcs = caret::rfFuncs,
      parallel = FALSE,
      early_stop = FALSE
    )
    inherits(rfe_obj, "rfe") && length(rfe_obj$optVariables) >= 1
  }, error = function(e) FALSE)
  
  print_and_store_result("classification: split preserves factor", ok_split_factor)
  print_and_store_result("classification: RFE runs (rfFuncs)", ok_rfe)
  invisible(NULL)
}

#' Run All Tests
#'
#' Executes the full test suite and prints a summary.
#'
#' @return Invisibly returns a `data.frame` of detailed results.
#'
#' @examples
#' \dontrun{
#' run_all_tests()
#' }
run_all_tests <- function() {
  assign("test_results", data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE), envir = .GlobalEnv)
  cat("Running Test Suite\n")
  cat("==================================================\n")
  
  test_validate_rfe_control()
  test_validate_train_control()
  test_split_data_fn()
  test_one_hot_encoding()
  test_perform_rfe_fn()
  test_train_final_model_fn()
  test_fs_recursivefeature_fn()
  test_classification_path_fn()
  
  cat("==================================================\n")
  cat("Completed\n\n")
  
  cat("Test Summary:\n")
  cat("==================================================\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
  
  invisible(test_results)
}

# Run tests (uncomment to execute)
# run_all_tests()
