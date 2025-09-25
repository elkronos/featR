###############################################################################
# Testing Infrastructure - Stepwise Regression (FULLY FIXED)
# Assumes the implementation functions are already defined in your session:
#   check_inputs, prepare_formula, build_models_for_direction,
#   train_stepwise_model, variable_importance, fs_stepwise
###############################################################################

# --- Logger shims (in case 'logger' isn't attached) -------------------
if (!exists("log_info"))       log_info       <- function(...) {}
if (!exists("log_error"))      log_error      <- function(...) {}
if (!exists("log_debug"))      log_debug      <- function(...) {}
if (!exists("WARN"))           WARN           <- NULL
if (!exists("log_threshold"))  log_threshold  <- function(...) {}

# --- Utilities ---------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) && !identical(a, "")) a else b

make_linear_data <- function(n = 200, seed = 123,
                             betas = c(p1 = 2.0, p2 = 3.0, p3 = 0.5),
                             noise_sd = 1.0, extra_noise = 0) {
  set.seed(seed)
  k <- length(betas)
  X <- as.data.frame(replicate(k, rnorm(n)))
  names(X) <- names(betas)
  if (extra_noise > 0) {
    for (j in seq_len(extra_noise)) X[[paste0("junk", j)]] <- rnorm(n)
  }
  y <- as.matrix(X[, names(betas), drop = FALSE]) %*% as.numeric(betas) + rnorm(n, sd = noise_sd)
  X$response <- as.numeric(y)
  X
}

# --- Test results management (FIXED exists()/get()) -------------------
test_results_init <- function() {
  if (!exists("test_results", envir = .GlobalEnv, inherits = FALSE)) {
    assign(
      "test_results",
      data.frame(Test = character(), Result = character(), Message = character(),
                 stringsAsFactors = FALSE),
      envir = .GlobalEnv
    )
  }
  invisible(get("test_results", envir = .GlobalEnv, inherits = FALSE))
}

reset_test_results <- function() {
  assign(
    "test_results",
    data.frame(Test = character(), Result = character(), Message = character(),
               stringsAsFactors = FALSE),
    envir = .GlobalEnv
  )
}

print_and_store_result <- function(test_name, passed, message = "") {
  test_results_init()
  result <- if (isTRUE(passed)) "PASS" else "FAIL"
  cat(sprintf("%-60s [%s]\n", test_name, result))
  if (nzchar(message)) cat("   ", message, "\n")
  if (result == "PASS") log_info("{test_name} [{result}] {message}") else log_error("{test_name} [{result}] {message}")
  tr <- get("test_results", envir = .GlobalEnv, inherits = FALSE)
  tr <- rbind(tr, data.frame(Test = test_name, Result = result, Message = message, stringsAsFactors = FALSE))
  assign("test_results", tr, envir = .GlobalEnv)
  invisible(tr)
}

# --- Tests -------------------------------------------------------------
test_check_inputs <- function() {
  log_info("Running test_check_inputs.")
  data_valid <- data.frame(response = rnorm(100), x = rnorm(100))
  
  ok_valid <- tryCatch(identical(check_inputs(data_valid, "response", "both"), "response"),
                       error = function(e) FALSE)
  print_and_store_result("check_inputs: Valid Input", ok_valid)
  
  ok_bad_data <- tryCatch({ check_inputs(list(a = 1), "response", "both"); FALSE },
                          error = function(e) TRUE)
  print_and_store_result("check_inputs: Invalid Data (not data.frame)", ok_bad_data)
  
  ok_missing_dep <- tryCatch({ check_inputs(data_valid, "nope", "both"); FALSE },
                             error = function(e) TRUE)
  print_and_store_result("check_inputs: Missing Dependent Var", ok_missing_dep)
  
  ok_bad_step <- tryCatch({ check_inputs(data_valid, "response", "sideways"); FALSE },
                          error = function(e) TRUE)
  print_and_store_result("check_inputs: Invalid step_type", ok_bad_step)
}

test_prepare_formula <- function() {
  log_info("Running test_prepare_formula.")
  dat <- data.frame(response = rnorm(20), a = rnorm(20), b = rnorm(20))
  f <- prepare_formula(dat, "response")
  expected <- reformulate(setdiff(names(dat), "response"), response = "response")
  ok <- isTRUE(all.equal(f, expected))
  print_and_store_result("prepare_formula: Builds Correct Formula", ok)
  
  dat2 <- data.frame(response = rnorm(10))
  ok_err <- tryCatch({ prepare_formula(dat2, "response"); FALSE }, error = function(e) TRUE)
  print_and_store_result("prepare_formula: Errors w/ No Predictors", ok_err)
}

test_build_models_for_direction <- function() {
  log_info("Running test_build_models_for_direction.")
  dat <- data.frame(response = rnorm(50), a = rnorm(50), b = rnorm(50))
  fml <- reformulate(c("a", "b"), response = "response")
  
  parts_b <- build_models_for_direction(fml, dat, "backward")
  ok_b <- inherits(parts_b$start_model, "lm") && is.null(parts_b$scope) && parts_b$direction == "backward"
  print_and_store_result("build_models_for_direction: backward", ok_b)
  
  parts_f <- build_models_for_direction(fml, dat, "forward")
  ok_f <- inherits(parts_f$start_model, "lm") && !is.null(parts_f$scope) &&
    parts_f$direction == "forward" &&
    identical(as.character(parts_f$scope$lower), as.character(as.formula("response ~ 1")))
  print_and_store_result("build_models_for_direction: forward (has null & scope)", ok_f)
  
  parts_bt <- build_models_for_direction(fml, dat, "both")
  ok_bt <- inherits(parts_bt$start_model, "lm") && !is.null(parts_bt$scope) && parts_bt$direction == "both"
  print_and_store_result("build_models_for_direction: both (full start + scope)", ok_bt)
}

test_train_stepwise_model <- function() {
  log_info("Running test_train_stepwise_model.")
  df  <- make_linear_data(n = 300, seed = 42, betas = c(p1 = 2, p2 = 3, p3 = 0.5), noise_sd = 1.0, extra_noise = 3)
  fml <- reformulate(setdiff(names(df), "response"), response = "response")
  
  m_both <- tryCatch(train_stepwise_model(fml, df, direction = "both", verbose = FALSE), error = function(e) NULL)
  ok_both <- !is.null(m_both) && inherits(m_both, "lm")
  print_and_store_result("train_stepwise_model: runs (both)", ok_both)
  
  m_fwd <- tryCatch(train_stepwise_model(fml, df, direction = "forward", verbose = FALSE), error = function(e) NULL)
  ok_fwd <- !is.null(m_fwd) && inherits(m_fwd, "lm")
  print_and_store_result("train_stepwise_model: runs (forward)", ok_fwd)
  
  m_bwd <- tryCatch(train_stepwise_model(fml, df, direction = "backward", verbose = FALSE), error = function(e) NULL)
  ok_bwd <- !is.null(m_bwd) && inherits(m_bwd, "lm")
  print_and_store_result("train_stepwise_model: runs (backward)", ok_bwd)
  
  sel <- if (!is.null(m_both)) attr(terms(m_both), "term.labels") else character(0)
  ok_signal <- all(c("p1", "p2") %in% sel)
  print_and_store_result("train_stepwise_model: retains strong signals", ok_signal)
}

test_variable_importance <- function() {
  log_info("Running test_variable_importance.")
  df <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- lm(y ~ x, df)
  imp <- variable_importance(fit)
  ok <- is.matrix(imp) && nrow(imp) >= 1 && "Estimate" %in% colnames(imp)
  print_and_store_result("variable_importance: returns coefficient matrix", ok)
}

test_fs_stepwise_core <- function() {
  log_info("Running test_fs_stepwise_core.")
  df <- make_linear_data(n = 250, seed = 7, betas = c(p1 = 2.0, p2 = 3.0, p3 = 0.0),
                         noise_sd = 1.0, extra_noise = 2)
  
  out_both <- tryCatch(fs_stepwise(df, "response", step_type = "both", seed = 99, verbose = FALSE, return_models = TRUE),
                       error = function(e) NULL)
  ok_both <- !is.null(out_both) && inherits(out_both$final_model, "lm") &&
    is.matrix(out_both$importance) && is.character(out_both$selected_terms)
  print_and_store_result("fs_stepwise: returns structured output (both)", ok_both)
  
  out_fwd <- tryCatch(fs_stepwise(df, "response", step_type = "forward", seed = 99, verbose = FALSE, return_models = TRUE),
                      error = function(e) NULL)
  ok_fwd <- !is.null(out_fwd) && inherits(out_fwd$final_model, "lm")
  print_and_store_result("fs_stepwise: runs (forward)", ok_fwd)
  
  out_bwd <- tryCatch(fs_stepwise(df, "response", step_type = "backward", seed = 99, verbose = FALSE, return_models = TRUE),
                      error = function(e) NULL)
  ok_bwd <- !is.null(out_bwd) && inherits(out_bwd$final_model, "lm")
  print_and_store_result("fs_stepwise: runs (backward)", ok_bwd)
  
  ok_keep_signal <- if (!is.null(out_both)) all(c("p1", "p2") %in% out_both$selected_terms) else FALSE
  print_and_store_result("fs_stepwise: keeps true signals (both)", ok_keep_signal)
}

test_fs_stepwise_errors <- function() {
  log_info("Running test_fs_stepwise_errors.")
  
  err1 <- tryCatch({ fs_stepwise(list(a = 1), "y", step_type = "both"); FALSE }, error = function(e) TRUE)
  print_and_store_result("fs_stepwise: error on non-data.frame", err1)
  
  df <- data.frame(y = rnorm(10), x = rnorm(10))
  err2 <- tryCatch({ fs_stepwise(df, "nope", step_type = "both"); FALSE }, error = function(e) TRUE)
  print_and_store_result("fs_stepwise: error on missing dependent var", err2)
  
  df2 <- data.frame(response = rnorm(10))
  err3 <- tryCatch({ fs_stepwise(df2, "response", step_type = "both"); FALSE }, error = function(e) TRUE)
  print_and_store_result("fs_stepwise: error on no predictors", err3)
  
  err4 <- tryCatch({ fs_stepwise(df, "y", step_type = "weird"); FALSE }, error = function(e) TRUE)
  print_and_store_result("fs_stepwise: error on invalid step_type", err4)
}

test_verbose_mode <- function() {
  log_info("Running test_verbose_mode.")
  df <- make_linear_data(n = 120, seed = 100, extra_noise = 1)
  ok <- tryCatch({ invisible(fs_stepwise(df, "response", step_type = "both", verbose = TRUE)); TRUE },
                 error = function(e) FALSE)
  print_and_store_result("fs_stepwise: verbose mode runs", ok)
}

# --- Test Runner -------------------------------------------------------
run_all_tests <- function(quiet_log = TRUE) {
  old_thresh <- tryCatch(log_threshold(), error = function(e) NULL)
  on.exit(try(log_threshold(old_thresh), silent = TRUE), add = TRUE)
  if (isTRUE(quiet_log)) try(log_threshold(WARN), silent = TRUE)
  
  reset_test_results()
  cat("==== Starting Comprehensive Tests ====\n")
  
  test_check_inputs()
  test_prepare_formula()
  test_build_models_for_direction()
  test_train_stepwise_model()
  test_variable_importance()
  test_fs_stepwise_core()
  test_fs_stepwise_errors()
  test_verbose_mode()
  
  cat("\n===== Test Summary =====\n")
  tbl <- table(test_results$Result)
  print(tbl)
  cat("\nDetailed Test Results:\n")
  print(test_results)
  
  cat("==== Testing Completed ====\n")
  invisible(test_results)
}

# To run the suite:
# run_all_tests()
