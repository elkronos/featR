# Load necessary libraries
library(caret)
library(glmnet)
library(Matrix)
library(doParallel)
library(foreach)

#' Extract Response and Predictor Variables
#'
#' This function extracts the response and predictor variables from a given data frame and formula.
#'
#' @param data A data frame containing the variables in the model.
#' @param formula A formula specifying the model.
#' @return A list containing the response variable \code{y} and the predictor variables \code{x}.
#' @importFrom stats model.response model.frame model.matrix
#' @examples
#' \dontrun{
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
#' formula <- response ~ predictor1 + predictor2
#' extract_variables(data, formula)
#' }
extract_variables <- function(data, formula) {
  y <- model.response(model.frame(formula, data))
  x <- model.matrix(formula, data)[, -1]
  list(y = y, x = x)
}

#' Handle Missing Values
#'
#' This function handles missing values by removing rows with any missing values.
#'
#' @param x A sparse matrix of predictor variables.
#' @param y A vector of response variable values.
#' @return A list containing the predictor variables \code{x} and response variable \code{y} with missing values removed.
#' @importFrom Matrix as.matrix
#' @importFrom stats complete.cases
#' @examples
#' \dontrun{
#' y <- rnorm(100)
#' x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
#' x[1, 1] <- NA
#' x[5, 2] <- NA
#' handle_missing_values(x, y)
#' }
handle_missing_values <- function(x, y) {
  x_dense <- as.matrix(x)
  complete_cases <- complete.cases(x_dense, y)
  if (any(!complete_cases)) {
    y <- y[complete_cases]
    x <- x[complete_cases, ]
  }
  list(x = x, y = y)
}

#' Perform Principal Component Analysis (PCA)
#'
#' This function performs PCA on the predictor variables if specified.
#'
#' @param x A sparse matrix of predictor variables.
#' @param use_pca A logical value indicating whether to perform PCA.
#' @param nPCs An integer specifying the number of principal components to retain. If NULL, all components are retained.
#' @return A sparse matrix of predictor variables after PCA.
#' @importFrom stats prcomp
#' @examples
#' \dontrun{
#' x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
#' perform_pca(x, TRUE, 1)
#' }
perform_pca <- function(x, use_pca, nPCs) {
  if (use_pca) {
    pca <- prcomp(as.matrix(x), scale. = TRUE, retx = TRUE)
    if (is.null(nPCs)) {
      x <- pca$x
    } else {
      x <- pca$x[, 1:min(nPCs, ncol(pca$x))]
    }
    x <- as(x, "CsparseMatrix")
  }
  x
}

#' Train Models in Parallel
#'
#' This function trains models in parallel using the specified tuning grid and control parameters.
#'
#' @param x A sparse matrix of predictor variables.
#' @param y A vector of response variable values.
#' @param tuneGrid A data frame specifying the grid of hyperparameters for tuning.
#' @param trControl A list specifying the training control parameters.
#' @return A data frame containing the RMSE, alpha, lambda, and the trained models.
#' @importFrom doParallel registerDoParallel makeCluster stopCluster registerDoSEQ
#' @importFrom caret train
#' @importFrom foreach foreach %dopar% clusterEvalQ
#' @importFrom utils head
#' @examples
#' \dontrun{
#' y <- rnorm(100)
#' x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
#' tuneGrid <- expand.grid(alpha = c(0, 1), lambda = c(0.1, 1))
#' trControl <- trainControl(method = "cv", number = 3)
#' train_models(x, y, tuneGrid, trControl)
#' }
train_models <- function(x, y, tuneGrid, trControl) {
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  on.exit({
    stopCluster(cl)
    registerDoSEQ()
  })
  
  clusterEvalQ(cl, {
    library(caret)
    library(glmnet)
    library(Matrix)
    library(foreach)
  })
  
  cv_fit <- foreach(i = 1:nrow(tuneGrid), .combine = rbind, .packages = c("caret", "glmnet", "Matrix")) %dopar% {
    a <- tuneGrid$alpha[i]
    l <- tuneGrid$lambda[i]
    
    fit <- train(x, y,
                 method = "glmnet",
                 tuneGrid = expand.grid(alpha = a, lambda = l),
                 trControl = trControl)
    
    rmse <- min(fit$results$RMSE, na.rm = TRUE)
    if (is.na(rmse)) {
      rmse <- Inf
    }
    
    data.frame(RMSE = rmse, alpha = a, lambda = l, model = I(list(fit)))
  }
  
  cv_fit <- cv_fit[cv_fit$RMSE != Inf, ]
  
  cv_fit
}

#' Select the Best Model
#'
#' This function selects the best model based on the RMSE values from cross-validation.
#'
#' @param cv_fit A data frame containing the results from cross-validation.
#' @return A list containing the best model, alpha, lambda, and RMSE value.
#' @examples
#' \dontrun{
#' cv_fit <- data.frame(
#'   RMSE = c(1.0, 0.5),
#'   alpha = c(0, 1),
#'   lambda = c(0.1, 1),
#'   model = I(list(model1, model2))
#' )
#' select_best_model(cv_fit)
#' }
select_best_model <- function(cv_fit) {
  rmse_values <- cv_fit$RMSE
  best_index <- which.min(rmse_values)
  best_model <- cv_fit$model[[best_index]]
  list(model = best_model$finalModel, alpha = best_model$bestTune$alpha, lambda = best_model$bestTune$lambda, RMSE = rmse_values[best_index])
}

#' Elastic Net Feature Selection and Model Training
#'
#' This function performs feature selection and model training using elastic net regularization.
#'
#' @param data A data frame containing the variables in the model.
#' @param formula A formula specifying the model.
#' @param alpha A sequence of alpha values for tuning. Default is \code{seq(0, 1, by = 0.1)}.
#' @param trControl A list specifying the training control parameters. Default is 5-fold cross-validation.
#' @param use_pca A logical value indicating whether to perform PCA. Default is \code{FALSE}.
#' @param nPCs An integer specifying the number of principal components to retain. Default is \code{NULL}.
#' @param lambda_seq A sequence of lambda values for tuning. Default is \code{NULL}.
#' @return A list containing the coefficients of the best model, alpha, lambda, and RMSE value.
#' @importFrom caret trainControl
#' @importFrom stats model.response model.frame model.matrix
#' @importFrom utils head
#' @examples
#' \dontrun{
#' data <- data.frame(response = rnorm(100), predictor1 = rnorm(100), predictor2 = rnorm(100))
#' formula <- response ~ predictor1 + predictor2
#' fs_elastic(data, formula)
#' }
fs_elastic <- function(data, formula, alpha = seq(0, 1, by = 0.1), 
                       trControl = trainControl(method = "cv", number = 5), 
                       use_pca = FALSE, nPCs = NULL, lambda_seq = NULL) {
  
  cat("Extracting response and predictor variables...\n")
  vars <- extract_variables(data, formula)
  y <- vars$y
  x <- vars$x
  
  cat("Response variable (y):\n")
  print(head(y))
  cat("Predictor variables (x):\n")
  print(head(x))
  
  cat("Converting to sparse matrix for memory efficiency...\n")
  x <- as(x, "CsparseMatrix")
  
  cat("Handling missing values...\n")
  vars <- handle_missing_values(x, y)
  x <- vars$x
  y <- vars$y
  
  cat("Response variable (y) after handling missing values:\n")
  print(head(y))
  cat("Predictor variables (x) after handling missing values:\n")
  print(head(x))
  
  if (is.null(lambda_seq)) {
    lambda_seq <- 10^seq(-3, 3, length = 100)
  }
  
  cat("Performing PCA...\n")
  x <- perform_pca(x, use_pca, nPCs)
  if (use_pca) {
    cat("Predictor variables (x) after PCA:\n")
    print(head(x))
  }
  
  tuneGrid <- expand.grid(alpha = alpha, lambda = lambda_seq)
  
  cat("Starting parallel processing...\n")
  cv_fit <- train_models(x, y, tuneGrid, trControl)
  
  cat("Results from cross-validation (cv_fit):\n")
  print(cv_fit)
  
  best_model <- select_best_model(cv_fit)
  
  cat("Best model details:\n")
  print(best_model$model)
  
  list(coef = coef(best_model$model$finalModel, s = best_model$lambda),
       alpha = best_model$alpha,
       lambda = best_model$lambda,
       RMSE = best_model$RMSE)
}

# Initialize results data frame
test_results <- data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE)

#' Print and Store Test Result
#'
#' This function prints and stores the result of a unit test.
#'
#' @param test_name A character string specifying the name of the test.
#' @param passed A logical value indicating whether the test passed.
#' @param message An optional character string specifying additional information about the test result.
#' @examples
#' \dontrun{
#' print_and_store_result("example_test", TRUE)
#' }
print_and_store_result <- function(test_name, passed, message = NULL) {
  result <- if(passed) "PASS" else "FAIL"
  cat(sprintf("%-40s [%s]\n", test_name, result))
  if (!is.null(message)) cat("  ", message, "\n")
  test_results <<- rbind(test_results, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE))
}

#' Test Extract Variables Function
#'
#' This function tests the \code{extract_variables} function.
#'
#' @examples
#' \dontrun{
#' test_extract_variables()
#' }
test_extract_variables <- function() {
  data <- data.frame(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  formula <- response ~ predictor1 + predictor2
  
  result <- extract_variables(data, formula)
  
  valid_result <- all(c("y", "x") %in% names(result)) && length(result$y) == 100 && ncol(result$x) == 2
  print_and_store_result("extract_variables: Valid input", valid_result)
}

#' Test Handle Missing Values Function
#'
#' This function tests the \code{handle_missing_values} function.
#'
#' @examples
#' \dontrun{
#' test_handle_missing_values()
#' }
test_handle_missing_values <- function() {
  set.seed(123)
  y <- rnorm(100)
  x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
  x[1, 1] <- NA
  x[5, 2] <- NA
  
  result <- handle_missing_values(x, y)
  
  valid_result <- !anyNA(result$x) && length(result$y) == nrow(result$x)
  print_and_store_result("handle_missing_values: Handle missing values", valid_result)
}

#' Test Perform PCA Function
#'
#' This function tests the \code{perform_pca} function.
#'
#' @examples
#' \dontrun{
#' test_perform_pca()
#' }
test_perform_pca <- function() {
  set.seed(123)
  x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
  
  result <- perform_pca(x, TRUE, 1)
  
  valid_result <- ncol(result) == 1
  print_and_store_result("perform_pca: Perform PCA", valid_result)
}

#' Test Train Models Function
#'
#' This function tests the \code{train_models} function.
#'
#' @examples
#' \dontrun{
#' test_train_models()
#' }
test_train_models <- function() {
  set.seed(123)
  y <- rnorm(100)
  x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
  colnames(x) <- c("predictor1", "predictor2")
  
  tuneGrid <- expand.grid(alpha = c(0, 1), lambda = c(0.1, 1))
  trControl <- trainControl(method = "cv", number = 3)
  
  result <- tryCatch({
    train_models(x, y, tuneGrid, trControl)
    TRUE
  }, error = function(e) {
    print_and_store_result("train_models: Error", FALSE, e$message)
    FALSE
  })
  
  print_and_store_result("train_models: Train models in parallel", result)
}

#' Test Select Best Model Function
#'
#' This function tests the \code{select_best_model} function.
#'
#' @examples
#' \dontrun{
#' test_select_best_model()
#' }
test_select_best_model <- function() {
  cv_fit <- data.frame(
    RMSE = c(1.0, 0.5),
    alpha = c(0, 1),
    lambda = c(0.1, 1),
    model = I(list(
      {
        x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
        colnames(x) <- c("predictor1", "predictor2")
        train(
          x = x,
          y = rnorm(100),
          method = "glmnet",
          tuneGrid = expand.grid(alpha = 0, lambda = 0.1),
          trControl = trainControl(method = "cv", number = 3)
        )
      },
      {
        x <- Matrix(rnorm(200), ncol = 2, sparse = TRUE)
        colnames(x) <- c("predictor1", "predictor2")
        train(
          x = x,
          y = rnorm(100),
          method = "glmnet",
          tuneGrid = expand.grid(alpha = 1, lambda = 1),
          trControl = trainControl(method = "cv", number = 3)
        )
      }
    ))
  )
  
  result <- select_best_model(cv_fit)
  
  valid_result <- !is.null(result$model) && result$RMSE == 0.5
  print_and_store_result("select_best_model: Select best model", valid_result)
}

#' Test FS Elastic Function
#'
#' This function tests the \code{fs_elastic} function.
#'
#' @examples
#' \dontrun{
#' test_fs_elastic()
#' }
test_fs_elastic <- function() {
  set.seed(123)
  data <- data.frame(
    response = rnorm(100),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  formula <- response ~ predictor1 + predictor2
  
  result <- tryCatch({
    fs_elastic(data, formula)
    TRUE
  }, error = function(e) FALSE)
  
  print_and_store_result("fs_elastic: Full function test", result)
}

#' Run All Tests
#'
#' This function runs all unit tests.
#'
#' @examples
#' \dontrun{
#' run_all_tests()
#' }
run_all_tests <- function() {
  cat("Running Comprehensive UAT\n")
  cat("==================================\n")
  test_extract_variables()
  test_handle_missing_values()
  test_perform_pca()
  test_train_models()
  test_select_best_model()
  test_fs_elastic()
  cat("==================================\n")
  cat("UAT completed\n\n")
  
  # Print summary
  cat("Test Summary:\n")
  cat("==================================\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Execute all tests
run_all_tests()