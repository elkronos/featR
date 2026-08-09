###############################################################################
# Testing Infrastructure - Bayes (Updated)
###############################################################################

# Global container for test results
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

#' Helper: Print and Store Test Result
#'
#' @param test_name Character. Name of the test.
#' @param passed Logical. Whether the test passed.
#' @param note Optional character. Additional notes.
print_and_store_result <- function(test_name, passed, note = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-50s [%s]\n", test_name, result))
  if (!is.null(note)) cat("  Note: ", note, "\n")
  test_results <<- rbind(
    test_results,
    data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)
  )
}

###############################################################################
# Tests: validate_data
###############################################################################

test_validate_data <- function() {
  dt <- data.table(
    response   = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    date       = seq.Date(Sys.Date(), by = "day", length.out = 100)
  )
  
  valid_input <- tryCatch({
    validate_data(dt, "response", c("predictor1", "predictor2"), "date")  # gaussian()
    TRUE
  }, error = function(e) FALSE)
  
  invalid_response <- tryCatch({
    validate_data(dt, "non_existent", c("predictor1", "predictor2"), "date")
    FALSE
  }, error = function(e) TRUE)
  
  invalid_predictor <- tryCatch({
    validate_data(dt, "response", c("predictor1", "non_existent"), "date")
    FALSE
  }, error = function(e) TRUE)
  
  invalid_date <- tryCatch({
    validate_data(dt, "response", c("predictor1", "predictor2"), "non_existent")
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("validate_data: Valid input", valid_input)
  print_and_store_result("validate_data: Invalid response", invalid_response)
  print_and_store_result("validate_data: Invalid predictor", invalid_predictor)
  print_and_store_result("validate_data: Invalid date", invalid_date)
}

# Additional tests for family-specific behavior
test_validate_data_family_checks <- function() {
  # Gaussian with non-numeric response should fail
  dt_char <- data.table(
    response   = as.character(sample(letters[1:2], 50, TRUE)),
    predictor1 = rnorm(50)
  )
  gaussian_non_numeric <- tryCatch({
    validate_data(dt_char, "response", "predictor1", date_col = NULL, brm_family = gaussian())
    FALSE
  }, error = function(e) TRUE)
  
  # Bernoulli with valid numeric response
  dt_bern_num <- data.table(
    response   = sample(c(0, 1), 50, TRUE),
    predictor1 = rnorm(50)
  )
  bernoulli_valid_num <- tryCatch({
    validate_data(dt_bern_num, "response", "predictor1", date_col = NULL, brm_family = bernoulli())
    TRUE
  }, error = function(e) FALSE)
  
  # Bernoulli with valid logical response
  dt_bern_log <- data.table(
    response   = sample(c(TRUE, FALSE), 50, TRUE),
    predictor1 = rnorm(50)
  )
  bernoulli_valid_log <- tryCatch({
    validate_data(dt_bern_log, "response", "predictor1", date_col = NULL, brm_family = bernoulli())
    TRUE
  }, error = function(e) FALSE)
  
  # Bernoulli with invalid values
  dt_bern_bad <- data.table(
    response   = sample(c(0, 1, 2), 50, TRUE),
    predictor1 = rnorm(50)
  )
  bernoulli_invalid <- tryCatch({
    validate_data(dt_bern_bad, "response", "predictor1", date_col = NULL, brm_family = bernoulli())
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("validate_data: Gaussian non-numeric response", gaussian_non_numeric)
  print_and_store_result("validate_data: Bernoulli valid numeric", bernoulli_valid_num)
  print_and_store_result("validate_data: Bernoulli valid logical", bernoulli_valid_log)
  print_and_store_result("validate_data: Bernoulli invalid values", bernoulli_invalid)
}

###############################################################################
# Tests: ISO Week Feature Addition
###############################################################################

test_add_week_feature <- function() {
  dt <- data.table(
    response   = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    date       = seq.Date(Sys.Date(), by = "day", length.out = 100)
  )
  res <- add_week_feature(dt, "date", c("predictor1", "predictor2"))
  week_added <- "iso_week_id" %in% names(res$data) && "iso_week_id" %in% res$predictor_cols
  no_na_week <- all(!is.na(res$data$iso_week_id))
  is_integer_week <- is.integer(res$data$iso_week_id)
  print_and_store_result(
    "add_week_feature: ISO week column added and valid",
    week_added && no_na_week && is_integer_week
  )
}

###############################################################################
# Tests: Predictor Combination Generation
###############################################################################

test_generate_predictor_combinations <- function() {
  preds <- c("predictor1", "predictor2", "predictor3")
  combs <- generate_predictor_combinations(preds)
  correct_count <- length(combs) == (2^length(preds) - 1)
  print_and_store_result("generate_predictor_combinations: Correct count", correct_count)
}

# Additional tests for new behavior in generate_predictor_combinations
test_generate_predictor_combinations_options <- function() {
  preds <- c("p1", "p2", "p3")
  
  # max_comb_size truncation
  combs_max2 <- generate_predictor_combinations(preds, max_comb_size = 2)
  expected_count_max2 <- choose(3, 1) + choose(3, 2)
  max2_ok <- length(combs_max2) == expected_count_max2
  
  # sampling behavior
  combs_sample2 <- generate_predictor_combinations(preds, sample_combinations = 2, seed = 999)
  sample2_ok <- length(combs_sample2) == 2
  
  # invalid max_comb_size
  invalid_max <- tryCatch({
    generate_predictor_combinations(preds, max_comb_size = 0)
    FALSE
  }, error = function(e) TRUE)
  
  # invalid sample_combinations
  invalid_sample <- tryCatch({
    generate_predictor_combinations(preds, sample_combinations = 0)
    FALSE
  }, error = function(e) TRUE)
  
  print_and_store_result("generate_predictor_combinations: max_comb_size truncation", max2_ok)
  print_and_store_result("generate_predictor_combinations: sampling length", sample2_ok)
  print_and_store_result("generate_predictor_combinations: invalid max_comb_size errors", invalid_max)
  print_and_store_result("generate_predictor_combinations: invalid sample_combinations errors", invalid_sample)
}

###############################################################################
# Tests: Model Fitting Function
###############################################################################

test_fit_model <- function(fast_mode = TRUE) {
  set.seed(123)
  data_size <- if (fast_mode) 50 else 100
  iter_val   <- if (fast_mode) 100 else 500
  warmup_val <- if (fast_mode) 0   else 250
  
  dt <- data.table(
    response   = rnorm(data_size),
    predictor1 = rnorm(data_size),
    predictor2 = rnorm(data_size)
  )
  
  formula_str <- "response ~ predictor1"
  
  # Proper prior for b to allow fixed_param prior sampling
  prior_fast <- prior(normal(0, 10), class = "b")
  
  brm_args <- if (fast_mode) {
    list(
      iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0,
      algorithm = "fixed_param", sample_prior = "only", seed = 123
    )
  } else {
    list(iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0, seed = 123)
  }
  
  model <- fit_model(
    data        = dt,
    formula_str = formula_str,
    brm_family  = gaussian(),
    prior       = prior_fast,
    brm_args    = brm_args,
    verbose     = FALSE
  )
  
  model_ok <- !is.null(model) && inherits(model, "brmsfit")
  print_and_store_result("fit_model: Basic fitting", model_ok)
}

###############################################################################
# Tests: Adding Metrics to Data
###############################################################################

test_add_metrics_to_data <- function(fast_mode = TRUE) {
  set.seed(123)
  data_size <- if (fast_mode) 50 else 100
  iter_val   <- if (fast_mode) 100 else 500
  warmup_val <- if (fast_mode) 0   else 250
  
  dt <- data.table(
    response   = rnorm(data_size),
    predictor1 = rnorm(data_size)
  )
  
  # Proper prior for b to allow fixed_param prior sampling
  prior_fast <- prior(normal(0, 10), class = "b")
  
  brm_args <- if (fast_mode) {
    list(
      iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0,
      algorithm = "fixed_param", sample_prior = "only", seed = 123
    )
  } else {
    list(iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0, seed = 123)
  }
  
  # Fit a simple model directly
  model <- do.call(
    brm,
    c(
      list(
        formula = response ~ predictor1,
        data    = dt,
        family  = gaussian(),
        prior   = prior_fast
      ),
      brm_args
    )
  )
  
  dt <- add_metrics_to_data(dt, model)
  cols_present <- all(c("fitted_values", "residuals", "abs_residuals", "squared_residuals") %in% names(dt))
  no_na_lengths <- nrow(dt) == length(dt$fitted_values) &&
    nrow(dt) == length(dt$residuals)
  print_and_store_result("add_metrics_to_data: Metrics appended", cols_present && no_na_lengths)
}

###############################################################################
# Tests: Main fs_bayes Function
###############################################################################

test_fs_bayes <- function(fast_mode = TRUE) {
  set.seed(123)
  data_size <- if (fast_mode) 50 else 120
  iter_val   <- if (fast_mode) 100 else 500
  warmup_val <- if (fast_mode) 0   else 250
  
  dt <- data.table(
    response   = rnorm(data_size),
    predictor1 = rnorm(data_size),
    predictor2 = rnorm(data_size),
    date       = seq.Date(Sys.Date(), by = "day", length.out = data_size)
  )
  
  # Proper prior for b for fast (fixed_param) mode
  prior_fast <- prior(normal(0, 10), class = "b")
  
  brm_args <- if (fast_mode) {
    list(
      iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0,
      algorithm = "fixed_param", sample_prior = "only", seed = 123
    )
  } else {
    list(iter = iter_val, warmup = warmup_val, chains = 1, refresh = 0, seed = 123)
  }
  
  # Basic functionality test
  res <- tryCatch({
    fs_bayes(
      dt,
      response_col         = "response",
      predictor_cols       = c("predictor1", "predictor2"),
      date_col             = "date",
      brm_family           = gaussian(),
      prior                = prior_fast,
      brm_args             = brm_args,
      parallel_combinations = FALSE,
      show_progress        = TRUE,
      verbose              = FALSE
    )
  }, error = function(e) NULL)
  
  basic_ok <- !is.null(res) && all(c("Model", "Data", "MAE", "RMSE") %in% names(res))
  print_and_store_result("fs_bayes: Basic functionality", basic_ok)
  
  # Check new return fields if res is not NULL
  if (!is.null(res)) {
    fields_ok <- all(c("SelectedPredictors", "SelectedFormula", "BestELPD") %in% names(res))
    predictors_ok <- is.character(res$SelectedPredictors) && length(res$SelectedPredictors) >= 1
    formula_ok <- is.character(res$SelectedFormula) && grepl("response ~", res$SelectedFormula, fixed = TRUE)
    print_and_store_result("fs_bayes: Return fields present", fields_ok)
    print_and_store_result("fs_bayes: SelectedPredictors non-empty", predictors_ok)
    print_and_store_result("fs_bayes: SelectedFormula coherent", formula_ok)
  }
  
  # Test: Handling missing data (should still run after NA omission)
  dt_missing <- copy(dt)
  dt_missing[sample(.N, 5), response := NA]
  dt_missing[sample(.N, 5), predictor1 := NA]
  
  res_missing <- tryCatch({
    fs_bayes(
      dt_missing,
      response_col         = "response",
      predictor_cols       = c("predictor1", "predictor2"),
      date_col             = "date",
      brm_family           = gaussian(),
      prior                = prior_fast,
      brm_args             = brm_args,
      parallel_combinations = FALSE,
      verbose              = FALSE
    )
  }, error = function(e) e)
  
  print_and_store_result(
    "fs_bayes: Missing data handling",
    !inherits(res_missing, "error"),
    if (inherits(res_missing, "error")) conditionMessage(res_missing)
  )
  
  # Test: Custom prior does not break (also proper for b)
  custom_prior <- prior(normal(0, 10), class = "b")
  res_prior <- tryCatch({
    fs_bayes(
      dt,
      response_col         = "response",
      predictor_cols       = c("predictor1", "predictor2"),
      date_col             = "date",
      brm_family           = gaussian(),
      prior                = custom_prior,
      brm_args             = brm_args,
      parallel_combinations = FALSE,
      verbose              = FALSE
    )
  }, error = function(e) NULL)
  print_and_store_result("fs_bayes: Custom prior", !is.null(res_prior))
  
  # Test: Parallel combinations (outer level)
  res_parallel <- tryCatch({
    fs_bayes(
      dt,
      response_col         = "response",
      predictor_cols       = c("predictor1", "predictor2"),
      date_col             = "date",
      brm_family           = gaussian(),
      prior                = prior_fast,
      brm_args             = brm_args,
      parallel_combinations = TRUE,
      verbose              = FALSE
    )
  }, error = function(e) NULL)
  print_and_store_result("fs_bayes: Parallel evaluation", !is.null(res_parallel))
}

###############################################################################
# Run All Tests
###############################################################################

#' Run All Tests
#'
#' Executes a series of tests to validate the functionality of the module.
#'
#' @param fast_mode Logical. If TRUE, uses settings for quick testing.
run_all_tests <- function(fast_mode = TRUE) {
  cat("========== Running Comprehensive Tests ==========\n")
  test_validate_data()
  test_validate_data_family_checks()
  test_add_week_feature()
  test_generate_predictor_combinations()
  test_generate_predictor_combinations_options()
  test_fit_model(fast_mode)
  test_add_metrics_to_data(fast_mode)
  test_fs_bayes(fast_mode)
  cat("========== Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment to run all tests:
# run_all_tests(fast_mode = TRUE)
