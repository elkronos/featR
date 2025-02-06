# ===============================
# Setup: Load Required Libraries
# ===============================
if (!requireNamespace("logger", quietly = TRUE)) {
  install.packages("logger")
}
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}

library(logger)
library(MASS)

# ===============================
# Logger Initialization
# ===============================
log_appender(appender_file("script_log.log"))  # Write logs to file
log_layout(layout_glue_colors)                  # Use colored layout for console

# Log basic R environment info
log_info("===== R Environment Details =====")
log_info("R Version: {R.version.string}")
log_info("Operating System: {Sys.info()['sysname']} {Sys.info()['release']}")
installed_pkgs <- as.data.frame(installed.packages()[, c("Package", "Version")])
log_info("Installed Packages:\n{paste(apply(installed_pkgs, 1, paste, collapse = ' '), collapse = '\n')}")
log_info("===================================\n")

# ===============================
# Helper Functions
# ===============================

# Log errors along with an optional stack trace
log_error_with_trace <- function(e) {
  log_error("Error: {e$message}")
  if (!is.null(e$trace)) {
    log_error("Stack Trace:\n{paste(e$trace, collapse = '\n')}")
  }
}

# Print and store test results (global data frame 'test_results')
print_and_store_result <- function(test_name, passed, message = NULL) {
  result <- if (passed) "PASS" else "FAIL"
  cat(sprintf("%-50s [%s]\n", test_name, result))
  log_info("{test_name} [{result}]")
  
  if (!is.null(message)) {
    cat("  ", message, "\n")
    log_debug("Additional Message for {test_name}: {message}")
  }
  
  # Append to a global test results data frame
  test_results <<- rbind(test_results, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE))
}

# ===============================
# Input Checking and Formula Prep
# ===============================

# Check inputs for fs_stepwise
check_inputs <- function(data, dependent_var, step_type) {
  log_info("Validating inputs: dependent_var = {dependent_var}, step_type = {step_type}")
  
  if (!is.data.frame(data)) {
    log_error("The 'data' input must be a data frame.")
    stop("Input 'data' must be a data frame")
  }
  
  # Convert dependent_var to character if not already
  dep_var <- if (is.character(dependent_var)) {
    dependent_var
  } else {
    deparse(substitute(dependent_var))
  }
  
  if (!(dep_var %in% colnames(data))) {
    log_error("Dependent variable '{dep_var}' not found in data columns.")
    stop("Dependent variable not found in data")
  }
  
  if (!step_type %in% c("backward", "forward", "both")) {
    log_error("Invalid 'step_type' ({step_type}). Must be one of 'backward', 'forward', or 'both'.")
    stop("Invalid 'step_type'")
  }
  
  log_info("Input validation successful; using dependent variable: {dep_var}")
  return(dep_var)
}

# Prepare the formula for modeling
prepare_formula <- function(data, dep_var) {
  log_info("Preparing model formula with dependent variable: {dep_var}")
  formula <- reformulate(termlabels = setdiff(colnames(data), dep_var), response = dep_var)
  log_debug("Constructed formula: {formula}")
  return(formula)
}

# ===============================
# Model Training Function
# ===============================

# Train a linear model using stepwise selection with stepAIC
train_stepwise_model <- function(formula, data, direction = "both", verbose = FALSE, ...) {
  log_info("Training stepwise model with direction = {direction}")
  
  tryCatch({
    # Fit the full model
    log_info("Fitting the initial full model.")
    full_model <- lm(formula, data = data)
    
    # Perform stepwise selection using stepAIC
    log_info("Starting stepwise selection using stepAIC.")
    step_model <- stepAIC(full_model, direction = direction, trace = verbose, ...)
    
    log_info("Stepwise model training completed successfully.")
    return(step_model)
  }, error = function(e) {
    log_error("Error during stepwise model training: {e$message}")
    log_error_with_trace(e)
    stop("Model training failed.")
  })
}

# ===============================
# Main Function: fs_stepwise
# ===============================
#' Perform stepwise feature selection using linear regression
#'
#' @param data A data.frame containing the dataset.
#' @param dependent_var The name (as a character string) of the dependent variable.
#' @param step_type The direction for stepwise selection: "backward", "forward", or "both". Default is "both".
#' @param seed An optional seed for reproducibility.
#' @param verbose Logical. If TRUE, prints detailed output. Default is FALSE.
#' @param return_models Logical. If TRUE, includes intermediate model results in the returned list.
#' @param ... Additional parameters to pass to \code{stepAIC}.
#'
#' @return A list containing the final model and variable importance, and optionally the full stepwise model.
fs_stepwise <- function(data, 
                        dependent_var, 
                        step_type = "both", 
                        seed = NULL, 
                        verbose = FALSE, 
                        return_models = FALSE,
                        ...) {
  log_info("Starting fs_stepwise with dependent_var = {dependent_var}, step_type = {step_type}")
  
  if (!is.null(seed)) {
    set.seed(seed)
    log_info("Random seed set to: {seed}")
  }
  
  # Validate inputs and prepare formula
  dep_var <- check_inputs(data, dependent_var, step_type)
  formula <- prepare_formula(data, dep_var)
  
  # Train the model using stepwise selection
  step_model <- train_stepwise_model(formula, data, direction = step_type, verbose = verbose, ...)
  
  # Compute variable importance from the final model coefficients
  importance <- summary(step_model)$coefficients
  log_info("Final model trained successfully.")
  
  # Create output list
  output <- list(final_model = step_model,
                 importance = importance)
  
  if (return_models) {
    output$step_model <- step_model
    log_info("Returning full model details as requested.")
  }
  
  if (verbose) {
    cat("===== Final Model Summary =====\n")
    print(summary(step_model))
    cat("\n===== Variable Importance =====\n")
    print(importance)
  }
  
  return(output)
}

# ===============================
# Testing Functions
# ===============================

# Global data frame to store test results
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

test_check_inputs <- function() {
  log_info("Running test_check_inputs.")
  
  # Test 1: Valid input
  data_valid <- data.frame(response = rnorm(100), predictor1 = rnorm(100))
  valid_result <- tryCatch({
    dep_var <- check_inputs(data_valid, "response", "both")
    identical(dep_var, "response")
  }, error = function(e) {
    log_error("Test check_inputs valid input failed: {e$message}")
    FALSE
  })
  print_and_store_result("check_inputs: Valid Input", valid_result)
  
  # Test 2: Data not a data.frame
  invalid_data <- tryCatch({
    check_inputs(list(a = 1, b = 2), "response", "both")
    FALSE
  }, error = function(e) {
    TRUE
  })
  print_and_store_result("check_inputs: Invalid Data", invalid_data)
  
  # Test 3: Dependent variable not found
  invalid_dep <- tryCatch({
    check_inputs(data_valid, "nonexistent", "both")
    FALSE
  }, error = function(e) {
    TRUE
  })
  print_and_store_result("check_inputs: Invalid Dependent Variable", invalid_dep)
  
  # Test 4: Invalid step_type
  invalid_step <- tryCatch({
    check_inputs(data_valid, "response", "invalid")
    FALSE
  }, error = function(e) {
    TRUE
  })
  print_and_store_result("check_inputs: Invalid step_type", invalid_step)
}

test_prepare_formula <- function() {
  log_info("Running test_prepare_formula.")
  
  data_test <- data.frame(response = rnorm(100), predictor1 = rnorm(100))
  formula_generated <- prepare_formula(data_test, "response")
  expected_formula <- reformulate(setdiff(names(data_test), "response"), response = "response")
  formula_test <- isTRUE(all.equal(formula_generated, expected_formula))
  print_and_store_result("prepare_formula: Correct Formula", formula_test)
}

test_train_stepwise_model <- function() {
  log_info("Running test_train_stepwise_model.")
  
  set.seed(123)
  data_test <- data.frame(
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    predictor3 = rnorm(100)
  )
  # Create response with known relationships
  data_test$response <- 2 * data_test$predictor1 + 3 * data_test$predictor2 + 0.5 * data_test$predictor3 + rnorm(100)
  formula_test <- reformulate(setdiff(names(data_test), "response"), response = "response")
  
  test_success <- tryCatch({
    model <- train_stepwise_model(formula_test, data_test, direction = "both", verbose = FALSE)
    !is.null(model)
  }, error = function(e) {
    FALSE
  })
  print_and_store_result("train_stepwise_model: Basic Functionality", test_success)
}

test_fs_stepwise <- function() {
  log_info("Running test_fs_stepwise.")
  
  set.seed(123)
  data_test <- data.frame(
    predictor1 = rnorm(100),
    predictor2 = rnorm(100),
    predictor3 = rnorm(100)
  )
  data_test$response <- 2 * data_test$predictor1 + 3 * data_test$predictor2 + 0.5 * data_test$predictor3 + rnorm(100)
  
  # Test the fs_stepwise function with valid input
  test_success <- tryCatch({
    result <- fs_stepwise(data_test, "response", step_type = "both", seed = 123, verbose = FALSE, return_models = TRUE)
    !is.null(result$final_model)
  }, error = function(e) {
    FALSE
  })
  print_and_store_result("fs_stepwise: Basic Functionality", test_success)
  
  # Test with invalid data input
  invalid_test <- tryCatch({
    fs_stepwise(list(a = 1, b = 2), "response", step_type = "both", seed = 123, verbose = FALSE)
    FALSE
  }, error = function(e) {
    TRUE
  })
  print_and_store_result("fs_stepwise: Invalid Data", invalid_test)
}

# Run all tests and summarize
run_all_tests <- function() {
  log_info("==== Starting Comprehensive Tests ====")
  test_check_inputs()
  test_prepare_formula()
  test_train_stepwise_model()
  test_fs_stepwise()
  
  cat("\n===== Test Summary =====\n")
  print(table(test_results$Result))
  cat("\nDetailed Test Results:\n")
  print(test_results)
  
  log_info("Tests Summary: PASS = {sum(test_results$Result == 'PASS')}, FAIL = {sum(test_results$Result == 'FAIL')}")
  log_info("==== Testing Completed ====")
}

# Run tests
# run_all_tests()