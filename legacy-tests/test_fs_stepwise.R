###############################################################################
# Testing Infrastructure - Stepwise Regression (fs_stepwise)
###############################################################################

# Optional: if fs_stepwise and helpers are in a separate script/package, load it here, e.g.:
# source("fs_stepwise.R")
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
#' @param note Optional character. Additional notes (e.g., error messages).
print_and_store_result <- function(test_name, passed, note = NULL) {
  result <- if (isTRUE(passed)) "PASS" else "FAIL"
  cat(sprintf("%-70s [%s]\n", test_name, result))
  if (!is.null(note) && nzchar(note)) cat("  Note: ", note, "\n")
  test_results <<- rbind(
    test_results,
    data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)
  )
}

###############################################################################
# Helpers
###############################################################################

# Synthetic linear data generator used across tests
make_linear_data <- function(n = 200, seed = 123,
                             betas = c(p1 = 2.0, p2 = 3.0, p3 = 0.5),
                             noise_sd = 1.0, extra_noise = 0) {
  set.seed(seed)
  k <- length(betas)
  X <- as.data.frame(replicate(k, rnorm(n)))
  names(X) <- names(betas)
  if (extra_noise > 0) {
    for (j in seq_len(extra_noise)) {
      X[[paste0("junk", j)]] <- rnorm(n)
    }
  }
  y <- as.matrix(X[, names(betas), drop = FALSE]) %*% as.numeric(betas) +
    rnorm(n, sd = noise_sd)
  X$response <- as.numeric(y)
  X
}

###############################################################################
# Tests: Existence and Basic Input Validation
###############################################################################

test_fs_stepwise_existence <- function() {
  err <- NULL
  passed <- tryCatch({
    all(
      exists("fs_stepwise")             && is.function(fs_stepwise),
      exists("train_stepwise_model")    && is.function(train_stepwise_model),
      exists("check_inputs")            && is.function(check_inputs),
      exists("prepare_formula")         && is.function(prepare_formula),
      exists("build_models_for_direction") && is.function(build_models_for_direction),
      exists("variable_importance")     && is.function(variable_importance)
    )
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_stepwise: core functions exist and are callable", passed, note)
}

test_check_inputs_validation <- function() {
  # Valid input
  data_valid <- data.frame(response = rnorm(100), x = rnorm(100))
  err1 <- NULL
  passed1 <- tryCatch({
    identical(check_inputs(data_valid, "response", "both"), "response")
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result("check_inputs: valid input returns dependent var", passed1, note1)
  
  # Invalid data (not data.frame)
  err2 <- NULL
  passed2 <- tryCatch({
    check_inputs(list(a = 1), "response", "both")
    FALSE
  }, error = function(e) {
    err2 <<- e
    TRUE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result("check_inputs: errors if data is not a data.frame", passed2, note2)
  
  # Missing dependent variable
  err3 <- NULL
  passed3 <- tryCatch({
    check_inputs(data_valid, "nope", "both")
    FALSE
  }, error = function(e) {
    err3 <<- e
    TRUE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result("check_inputs: errors if dependent variable missing", passed3, note3)
  
  # Invalid step_type
  err4 <- NULL
  passed4 <- tryCatch({
    check_inputs(data_valid, "response", "sideways")
    FALSE
  }, error = function(e) {
    err4 <<- e
    grepl("Invalid 'step_type'", conditionMessage(e), fixed = TRUE)
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result("check_inputs: errors with clear message on invalid step_type", passed4, note4)
}

test_fs_stepwise_input_validation <- function() {
  # Non-data.frame input
  err1 <- NULL
  passed1 <- tryCatch({
    fs_stepwise(list(a = 1), "y", step_type = "both")
    FALSE
  }, error = function(e) {
    err1 <<- e
    TRUE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result("fs_stepwise: errors on non-data.frame input", passed1, note1)
  
  # Missing dependent variable
  df <- data.frame(y = rnorm(10), x = rnorm(10))
  err2 <- NULL
  passed2 <- tryCatch({
    fs_stepwise(df, "nope", step_type = "both")
    FALSE
  }, error = function(e) {
    err2 <<- e
    TRUE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result("fs_stepwise: errors when dependent var is missing", passed2, note2)
  
  # No predictors
  df2 <- data.frame(response = rnorm(10))
  err3 <- NULL
  passed3 <- tryCatch({
    fs_stepwise(df2, "response", step_type = "both")
    FALSE
  }, error = function(e) {
    err3 <<- e
    TRUE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result("fs_stepwise: errors when no predictors remain", passed3, note3)
  
  # Invalid step_type
  err4 <- NULL
  passed4 <- tryCatch({
    fs_stepwise(df, "y", step_type = "weird")
    FALSE
  }, error = function(e) {
    err4 <<- e
    grepl("Invalid 'step_type'", conditionMessage(e), fixed = TRUE)
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result("fs_stepwise: invalid step_type yields clear error", passed4, note4)
}

###############################################################################
# Tests: Formula Construction and Model Setup
###############################################################################

test_prepare_formula <- function() {
  dat <- data.frame(response = rnorm(20), a = rnorm(20), b = rnorm(20))
  f <- prepare_formula(dat, "response")
  expected <- reformulate(setdiff(names(dat), "response"), response = "response")
  err1 <- NULL
  passed1 <- tryCatch({
    isTRUE(all.equal(f, expected))
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result("prepare_formula: builds correct formula", passed1, note1)
  
  # Error when no predictors
  dat2 <- data.frame(response = rnorm(10))
  err2 <- NULL
  passed2 <- tryCatch({
    prepare_formula(dat2, "response")
    FALSE
  }, error = function(e) {
    err2 <<- e
    TRUE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result("prepare_formula: errors when no predictors", passed2, note2)
}

test_build_models_for_direction <- function() {
  dat <- data.frame(response = rnorm(50), a = rnorm(50), b = rnorm(50))
  fml <- reformulate(c("a", "b"), response = "response")
  
  # Backward
  parts_b <- build_models_for_direction(fml, dat, "backward")
  passed_b <- inherits(parts_b$start_model, "lm") &&
    is.null(parts_b$scope) &&
    identical(parts_b$direction, "backward")
  print_and_store_result("build_models_for_direction: backward uses full model with no scope", passed_b)
  
  # Forward
  parts_f <- build_models_for_direction(fml, dat, "forward")
  passed_f <- inherits(parts_f$start_model, "lm") &&
    !is.null(parts_f$scope) &&
    identical(parts_f$direction, "forward") &&
    identical(
      as.character(parts_f$scope$lower),
      as.character(as.formula("response ~ 1"))
    )
  print_and_store_result("build_models_for_direction: forward uses null start and scope", passed_f)
  
  # Both
  parts_bt <- build_models_for_direction(fml, dat, "both")
  passed_bt <- inherits(parts_bt$start_model, "lm") &&
    !is.null(parts_bt$scope) &&
    identical(parts_bt$direction, "both")
  print_and_store_result("build_models_for_direction: both uses full start and scope", passed_bt)
}

###############################################################################
# Tests: train_stepwise_model Behavior
###############################################################################

test_train_stepwise_model_runs <- function() {
  df  <- make_linear_data(
    n = 300,
    seed = 42,
    betas = c(p1 = 2, p2 = 3, p3 = 0.5),
    noise_sd = 1.0,
    extra_noise = 3
  )
  fml <- reformulate(setdiff(names(df), "response"), response = "response")
  
  # both
  err_both <- NULL
  m_both <- tryCatch(
    train_stepwise_model(fml, df, direction = "both", verbose = FALSE),
    error = function(e) { err_both <<- e; NULL }
  )
  passed_both <- !is.null(m_both) && inherits(m_both, "lm")
  note_both <- if (!is.null(err_both)) conditionMessage(err_both) else NULL
  print_and_store_result("train_stepwise_model: runs for direction = 'both'", passed_both, note_both)
  
  # forward
  err_fwd <- NULL
  m_fwd <- tryCatch(
    train_stepwise_model(fml, df, direction = "forward", verbose = FALSE),
    error = function(e) { err_fwd <<- e; NULL }
  )
  passed_fwd <- !is.null(m_fwd) && inherits(m_fwd, "lm")
  note_fwd <- if (!is.null(err_fwd)) conditionMessage(err_fwd) else NULL
  print_and_store_result("train_stepwise_model: runs for direction = 'forward'", passed_fwd, note_fwd)
  
  # backward
  err_bwd <- NULL
  m_bwd <- tryCatch(
    train_stepwise_model(fml, df, direction = "backward", verbose = FALSE),
    error = function(e) { err_bwd <<- e; NULL }
  )
  passed_bwd <- !is.null(m_bwd) && inherits(m_bwd, "lm")
  note_bwd <- if (!is.null(err_bwd)) conditionMessage(err_bwd) else NULL
  print_and_store_result("train_stepwise_model: runs for direction = 'backward'", passed_bwd, note_bwd)
  
  # strong signals retained in 'both'
  sel <- if (!is.null(m_both)) attr(terms(m_both), "term.labels") else character(0)
  passed_signal <- all(c("p1", "p2") %in% sel)
  print_and_store_result("train_stepwise_model: retains strong signal predictors", passed_signal)
}

test_train_stepwise_model_trace_ignored <- function() {
  df  <- make_linear_data(
    n = 150,
    seed = 21,
    betas = c(p1 = 2, p2 = 3, p3 = 0.5),
    noise_sd = 1.0,
    extra_noise = 1
  )
  fml <- reformulate(setdiff(names(df), "response"), response = "response")
  
  err <- NULL
  passed <- tryCatch({
    fit <- train_stepwise_model(fml, df, direction = "both", verbose = FALSE, trace = 2)
    inherits(fit, "lm")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("train_stepwise_model: accepts and ignores 'trace' in '...'", passed, note)
}

###############################################################################
# Tests: variable_importance
###############################################################################

test_variable_importance_output <- function() {
  df <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- lm(y ~ x, df)
  err <- NULL
  passed <- tryCatch({
    imp <- variable_importance(fit)
    is.matrix(imp) && nrow(imp) >= 1L && "Estimate" %in% colnames(imp)
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("variable_importance: returns coefficient matrix with estimates", passed, note)
}

###############################################################################
# Tests: fs_stepwise End-to-End Behavior
###############################################################################

test_fs_stepwise_core <- function() {
  df <- make_linear_data(
    n = 250,
    seed = 7,
    betas = c(p1 = 2.0, p2 = 3.0, p3 = 0.0),
    noise_sd = 1.0,
    extra_noise = 2
  )
  
  err <- NULL
  out_both <- tryCatch({
    fs_stepwise(df, "response", step_type = "both", seed = 99, verbose = FALSE, return_models = TRUE)
  }, error = function(e) {
    err <<- e
    NULL
  })
  passed <- !is.null(out_both) &&
    inherits(out_both$final_model, "lm") &&
    is.matrix(out_both$importance) &&
    is.character(out_both$selected_terms) &&
    "step_model" %in% names(out_both) &&
    inherits(out_both$step_model, "lm")
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_stepwise: structured output with final_model, importance, terms", passed, note)
}

test_fs_stepwise_directions <- function() {
  df <- make_linear_data(
    n = 220,
    seed = 8,
    betas = c(p1 = 2.0, p2 = 3.0, p3 = 0.5),
    noise_sd = 1.0,
    extra_noise = 1
  )
  
  # forward
  err_fwd <- NULL
  out_fwd <- tryCatch({
    fs_stepwise(df, "response", step_type = "forward", seed = 101, verbose = FALSE, return_models = TRUE)
  }, error = function(e) {
    err_fwd <<- e
    NULL
  })
  passed_fwd <- !is.null(out_fwd) && inherits(out_fwd$final_model, "lm")
  note_fwd <- if (!is.null(err_fwd)) conditionMessage(err_fwd) else NULL
  print_and_store_result("fs_stepwise: runs with step_type = 'forward'", passed_fwd, note_fwd)
  
  # backward
  err_bwd <- NULL
  out_bwd <- tryCatch({
    fs_stepwise(df, "response", step_type = "backward", seed = 102, verbose = FALSE, return_models = TRUE)
  }, error = function(e) {
    err_bwd <<- e
    NULL
  })
  passed_bwd <- !is.null(out_bwd) && inherits(out_bwd$final_model, "lm")
  note_bwd <- if (!is.null(err_bwd)) conditionMessage(err_bwd) else NULL
  print_and_store_result("fs_stepwise: runs with step_type = 'backward'", passed_bwd, note_bwd)
}

test_fs_stepwise_keeps_signals <- function() {
  df <- make_linear_data(
    n = 260,
    seed = 11,
    betas = c(p1 = 2.0, p2 = 3.0, p3 = 0.0),
    noise_sd = 1.0,
    extra_noise = 3
  )
  
  err <- NULL
  out <- tryCatch({
    fs_stepwise(df, "response", step_type = "both", seed = 55, verbose = FALSE, return_models = TRUE)
  }, error = function(e) {
    err <<- e
    NULL
  })
  sel <- if (!is.null(out)) out$selected_terms else character(0)
  passed <- all(c("p1", "p2") %in% sel)
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_stepwise: keeps known strong predictors (p1, p2)", passed, note)
}

test_fs_stepwise_return_models_flag <- function() {
  df <- make_linear_data(
    n = 180,
    seed = 15,
    betas = c(p1 = 2.0, p2 = 3.0, p3 = 0.5),
    noise_sd = 1.0,
    extra_noise = 1
  )
  
  # return_models = FALSE
  err1 <- NULL
  out1 <- tryCatch({
    fs_stepwise(df, "response", step_type = "both", verbose = FALSE, return_models = FALSE)
  }, error = function(e) {
    err1 <<- e
    NULL
  })
  passed1 <- !is.null(out1) &&
    is.list(out1) &&
    !("step_model" %in% names(out1)) &&
    inherits(out1$final_model, "lm")
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result("fs_stepwise: return_models = FALSE does not include step_model", passed1, note1)
  
  # return_models = TRUE
  err2 <- NULL
  out2 <- tryCatch({
    fs_stepwise(df, "response", step_type = "both", verbose = FALSE, return_models = TRUE)
  }, error = function(e) {
    err2 <<- e
    NULL
  })
  passed2 <- !is.null(out2) &&
    is.list(out2) &&
    "step_model" %in% names(out2) &&
    inherits(out2$step_model, "lm") &&
    inherits(out2$final_model, "lm")
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result("fs_stepwise: return_models = TRUE includes step_model", passed2, note2)
}

test_fs_stepwise_verbose_mode <- function() {
  df <- make_linear_data(
    n = 120,
    seed = 100,
    betas = c(p1 = 2.0, p2 = 3.0, p3 = 0.5),
    noise_sd = 1.0,
    extra_noise = 1
  )
  err <- NULL
  passed <- tryCatch({
    invisible(fs_stepwise(df, "response", step_type = "both", verbose = TRUE))
    TRUE
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_stepwise: verbose = TRUE runs without error", passed, note)
}

test_fs_stepwise_accepts_extra_args <- function() {
  df <- make_linear_data(
    n = 200,
    seed = 120,
    betas = c(p1 = 2.0, p2 = 3.0, p3 = 0.5),
    noise_sd = 1.0,
    extra_noise = 2
  )
  
  err <- NULL
  passed <- tryCatch({
    out <- fs_stepwise(
      df,
      "response",
      step_type = "both",
      verbose = FALSE,
      k = log(nrow(df))  # pass additional argument to stepAIC via ...
    )
    is.list(out) && inherits(out$final_model, "lm")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("fs_stepwise: accepts additional stepAIC args via '...'", passed, note)
}

###############################################################################
# Run All fs_stepwise Tests
###############################################################################

run_fs_stepwise_tests <- function() {
  cat("========== Running fs_stepwise Tests ==========\n")
  
  # Reset results for a fresh run
  test_results <<- test_results[0, ]
  
  test_fs_stepwise_existence()
  test_check_inputs_validation()
  test_fs_stepwise_input_validation()
  test_prepare_formula()
  test_build_models_for_direction()
  test_train_stepwise_model_runs()
  test_train_stepwise_model_trace_ignored()
  test_variable_importance_output()
  test_fs_stepwise_core()
  test_fs_stepwise_directions()
  test_fs_stepwise_keeps_signals()
  test_fs_stepwise_return_models_flag()
  test_fs_stepwise_verbose_mode()
  test_fs_stepwise_accepts_extra_args()
  
  cat("========== fs_stepwise Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment the following line to run all tests when this script is executed:
# run_fs_stepwise_tests()
