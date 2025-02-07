###############################################################################
# Testing Infrastructure - Elastic Net
###############################################################################

#' Unit Test for handle_missing_values
#'
#' Tests the \code{handle_missing_values} function to ensure missing values are removed.
#'
#' @return None.
test_handle_missing_values <- function() {
  set.seed(123)
  y <- rnorm(100)
  x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
  x[1, 1] <- NA
  x[5, 2] <- NA
  
  result <- handle_missing_values(x, y)
  valid <- !anyNA(result$x) && length(result$y) == nrow(result$x)
  print_and_store_result("handle_missing_values: Missing values handled", valid)
}

##########################################
# Unit Test: Perform PCA
##########################################
#' Unit Test for perform_pca
#'
#' Tests the \code{perform_pca} function to ensure correct dimensionality.
#'
#' @return None.
test_perform_pca <- function() {
  set.seed(123)
  x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
  
  result <- perform_pca(x, use_pca = TRUE, nPCs = 1)
  valid <- ncol(result) == 1
  print_and_store_result("perform_pca: PCA dimensionality", valid)
}

##########################################
# Unit Test: Train Models
##########################################
#' Unit Test for train_models
#'
#' Tests the \code{train_models} function to ensure parallel training runs without errors.
#'
#' @return None.
test_train_models <- function() {
  set.seed(123)
  y <- rnorm(100)
  x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
  colnames(x) <- c("predictor1", "predictor2")
  
  tuneGrid <- expand.grid(alpha = c(0, 1), lambda = c(0.1, 1))
  trControl <- trainControl(method = "cv", number = 3, summaryFunction = safe_summary)
  
  result <- tryCatch({
    train_models(x, y, tuneGrid, trControl)
    TRUE
  }, error = function(e) {
    print_and_store_result("train_models: Parallel training", FALSE, e$message)
    FALSE
  })
  
  if (result) print_and_store_result("train_models: Parallel training", TRUE)
}

##########################################
# Unit Test: Select Best Model
##########################################
#' Unit Test for select_best_model
#'
#' Tests the \code{select_best_model} function to ensure the best model is selected.
#'
#' @return None.
test_select_best_model <- function() {
  # Create dummy model objects with dummy bestTune parameters
  model1 <- list(finalModel = list(), bestTune = data.frame(alpha = 0, lambda = 0.1))
  model2 <- list(finalModel = list(), bestTune = data.frame(alpha = 1, lambda = 1))
  
  cv_results <- data.frame(
    RMSE = c(1.0, 0.5),
    alpha = c(0, 1),
    lambda = c(0.1, 1),
    model = I(list(model1, model2))
  )
  
  result <- select_best_model(cv_results)
  valid <- !is.null(result$model) && (result$RMSE == 0.5)
  print_and_store_result("select_best_model: Best model selection", valid)
}

##########################################
# Unit Test: Full Elastic Net Function
##########################################
#' Unit Test for fs_elastic
#'
#' Tests the full \code{fs_elastic} function.
#'
#' @return None.
test_fs_elastic <- function() {
  set.seed(123)
  data <- data.frame(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  formula <- response ~ predictor1 + predictor2
  
  result <- tryCatch({
    fs_elastic(data, formula, verbose = FALSE)
    TRUE
  }, error = function(e) {
    print_and_store_result("fs_elastic: Full function test", FALSE, e$message)
    FALSE
  })
  
  if (result) print_and_store_result("fs_elastic: Full function test", TRUE)
}

##########################################
# Run All Tests
##########################################
#' Run All Unit Tests
#'
#' Executes all defined unit tests and prints a summary of the results.
#'
#' @return None.
run_all_tests <- function() {
  cat("Running Comprehensive Unit Tests\n")
  cat("==================================\n")
  test_extract_variables()
  test_handle_missing_values()
  test_perform_pca()
  test_train_models()
  test_select_best_model()
  test_fs_elastic()
  cat("==================================\n")
  cat("Unit Testing completed\n\n")
  
  # Print summary of test results
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

##########################################
# Execute Unit Tests (Do Not Run by Default)
##########################################
## To run the tests, uncomment the block below.
## if (sys.nframe() == 0) {
##   run_all_tests()
## }
