###############################################################################
# Testing Infrastructure - MARS
###############################################################################

#' Test Functions for fs_mars and Utilities
#'
#' Runs unit tests to verify the fs_mars pipeline and its utility functions.
#'
#' @export
test_fs_mars_functions <- function() {
  message("Running unit tests for fs_mars and related functions...")
  
  # Test: check_libraries
  test_that("check_libraries verifies required libraries", {
    expect_error(check_libraries(), NA)
  })
  
  # Test: check_response_column
  test_that("check_response_column detects missing columns", {
    dt <- data.table(response = rnorm(100), predictor = rnorm(100))
    expect_error(check_response_column(dt, "invalid"), "does not exist")
    expect_error(check_response_column(dt, "response"), NA)
  })
  
  # Test: handle_missing_values
  test_that("handle_missing_values removes NA rows", {
    dt <- data.table(response = c(rnorm(95), rep(NA, 5)),
                     predictor = c(rnorm(90), rep(NA, 10)))
    dt_clean <- handle_missing_values(dt)
    expect_true(sum(is.na(dt_clean)) == 0)
    expect_true(nrow(dt_clean) <= nrow(dt))
  })
  
  # Test: balance_classes
  test_that("balance_classes properly oversamples minority classes", {
    dt <- data.table(response = factor(rep(c("A", "B"), c(70, 30))),
                     predictor = rnorm(100))
    dt_balanced <- balance_classes(dt, "response")
    counts <- table(dt_balanced$response)
    expect_equal(min(counts), max(counts))
  })
  
  # Test: sample_data
  test_that("sample_data reduces dataset size", {
    dt <- data.table(response = rnorm(20000), predictor = rnorm(20000))
    dt_sampled <- sample_data(dt, 10000, seed = 123)
    expect_equal(nrow(dt_sampled), 10000)
  })
  
  # Test: split_data
  test_that("split_data splits data correctly", {
    dt <- data.table(response = rnorm(100), predictor = rnorm(100))
    splits <- split_data(dt, "response", 0.8, seed = 123)
    expect_equal(nrow(splits$train), 80)
    expect_equal(nrow(splits$test), 20)
  })
  
  # Test: define_hyperparameter_grid
  test_that("define_hyperparameter_grid creates correct grid", {
    grid <- define_hyperparameter_grid(1:3, c(5, 10, 15))
    expect_equal(nrow(grid), 9)
  })
  
  # Test: define_train_control
  test_that("define_train_control returns a trainControl object", {
    dt <- data.table(response = factor(rep(c("A", "B"), each = 50)), predictor = rnorm(100))
    ctrl <- define_train_control(5, 3, "grid", dt, "response")
    expect_true(inherits(ctrl, "trainControl"))
  })
  
  # Test: train_model and evaluate_model for regression
  test_that("train_model and evaluate_model work for regression", {
    dt <- data.table(response = rnorm(100), predictor = rnorm(100))
    grid <- define_hyperparameter_grid(1:3, c(5, 10, 15))
    ctrl <- define_train_control(5, 3, "grid", dt, "response")
    model <- train_model(dt, "response", "earth", ctrl, grid, seed = 123)
    splits <- split_data(dt, "response", 0.8, seed = 123)
    eval_out <- evaluate_model(model, splits$test, "response")
    expect_true(!is.null(eval_out$rmse))
  })
  
  # Test: fs_mars integration for regression
  test_that("fs_mars works for regression", {
    dt <- data.table(response = rnorm(100), predictor = rnorm(100))
    result <- fs_mars(dt, "response", seed = 123)
    expect_true(!is.null(result$model))
    expect_true(!is.null(result$rmse))
  })
  
  message("All unit tests passed.")
}

# Run tests
# test_fs_mars_functions()
