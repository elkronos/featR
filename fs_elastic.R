# Load necessary packages
if (!requireNamespace("glmnet", quietly = TRUE)) install.packages("glmnet")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("doParallel", quietly = TRUE)) install.packages("doParallel")
if (!requireNamespace("Matrix", quietly = TRUE)) install.packages("Matrix")

library(glmnet)
library(caret)
library(doParallel)
library(Matrix)

#' Elastic Net Regression with Optional PCA and Cross-Validation
#'
#' This function performs elastic net regression with optional PCA and cross-validation, 
#' using the `glmnet` and `caret` packages. Parallel processing is utilized with the `doParallel` package.
#'
#' @param data A data frame containing the response variable and predictor variables.
#' @param formula A formula specifying the model, with the response variable on the left-hand side 
#' and the predictor variables on the right-hand side.
#' @param alpha A numeric vector of values between 0 and 1 that specifies the mixing parameter 
#' between the L1 and L2 penalties in the elastic net model. Default is a sequence from 0 to 1 in increments of 0.1.
#' @param lambda_seq A numeric vector of lambda values for regularization. If NULL (default), 
#' it uses `10^seq(-3, 3, length = 100)`.
#' @param trControl A `trainControl` object specifying the cross-validation method and number of folds. 
#' Default is 5-fold cross-validation.
#' @param use_pca A logical value indicating whether principal component analysis (PCA) should be 
#' performed on the predictor variables before fitting the model. Default is FALSE.
#' @param nPCs Numeric. Number of principal components to retain if PCA is used. If NULL and PCA is used, 
#' all components are retained.
#'
#' @return A list with the following elements:
#'   \itemize{
#'     \item \code{coef}: Coefficients of the best model.
#'     \item \code{alpha}: The mixing parameter between the L1 and L2 penalties for the best model.
#'     \item \code{lambda}: The regularization parameter for the best model.
#'     \item \code{RMSE}: The root mean square error (RMSE) of the best model.
#'   }
#'
#' @examples
#' # Set the seed for reproducibility
#' set.seed(123)
#' # Generate a dataset
#' n <- 100
#' p <- 10
#' x <- matrix(rnorm(n * p), nrow = n)
#' colnames(x) <- paste0("x", 1:p)
#' beta <- c(1.5, -2, 0, 0, 0.8, 0, -1.2, 0.5, 0.3, -1)
#' y <- x %*% beta + rnorm(n)
#' data <- data.frame(y, x)
#'
#' # Apply the function
#' result <- fs_elastic(data, y ~ .)
#'
#' @importFrom glmnet glmnet
#' @importFrom caret trainControl train
#' @importFrom doParallel makeCluster registerDoParallel stopCluster detectCores
#' @importFrom stats prcomp model.matrix model.response complete.cases
#' @export
fs_elastic <- function(data, formula, alpha = seq(0, 1, by = 0.1), 
                       trControl = trainControl(method = "cv", number = 5), 
                       use_pca = FALSE, nPCs = NULL, lambda_seq = NULL) {
  
  cat("Extracting response and predictor variables...\n")
  y <- model.response(model.frame(formula, data))
  x <- model.matrix(formula, data)[,-1]
  cat("Response variable (y):\n")
  print(head(y))
  cat("Predictor variables (x):\n")
  print(head(x))
  
  cat("Converting to sparse matrix for memory efficiency...\n")
  x <- as(x, "CsparseMatrix")
  
  cat("Handling missing values...\n")
  x_dense <- as.matrix(x)
  complete_cases <- complete.cases(x_dense, y)
  if(any(!complete_cases)){
    y <- y[complete_cases]
    x <- x[complete_cases, ]
  }
  cat("Response variable (y) after handling missing values:\n")
  print(head(y))
  cat("Predictor variables (x) after handling missing values:\n")
  print(head(x))
  
  if(is.null(lambda_seq)){
    lambda_seq <- 10^seq(-3, 3, length = 100)
  }
  
  if (use_pca) {
    cat("Performing PCA...\n")
    pca <- prcomp(as.matrix(x), scale. = TRUE, retx = TRUE)
    if(is.null(nPCs)){
      x <- pca$x
    } else {
      x <- pca$x[, 1:min(nPCs, ncol(pca$x))]
    }
    x <- as(x, "CsparseMatrix") # Convert PCA result to sparse matrix
    cat("Predictor variables (x) after PCA:\n")
    print(head(x))
  }
  
  tuneGrid <- expand.grid(alpha = alpha, lambda = lambda_seq)
  
  cat("Starting parallel processing...\n")
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  clusterEvalQ(cl, {
    library(caret)
    library(glmnet)
    library(Matrix)
  })
  
  cv_fit <- foreach(i = 1:nrow(tuneGrid), .combine = rbind, .packages = c("caret", "glmnet", "Matrix")) %dopar% {
    a <- tuneGrid$alpha[i]
    l <- tuneGrid$lambda[i]
    
    fit <- train(x, y,
                 method = "glmnet",
                 tuneGrid = expand.grid(alpha = a, lambda = l),
                 trControl = trControl)
    data.frame(RMSE = min(fit$results$RMSE, na.rm = TRUE), alpha = a, lambda = l, model = I(list(fit)))
  }
  
  stopCluster(cl)
  
  cat("Results from cross-validation (cv_fit):\n")
  print(cv_fit)
  
  rmse_values <- cv_fit$RMSE
  
  cat("Extracted RMSE values:\n")
  print(rmse_values)
  
  best_index <- which.min(rmse_values)
  best_model <- cv_fit$model[[best_index]]
  
  cat("Best model details:\n")
  print(best_model)
  
  list(coef = coef(best_model$finalModel, s = best_model$bestTune$lambda),
       alpha = best_model$bestTune$alpha,
       lambda = best_model$bestTune$lambda,
       RMSE = min(best_model$results$RMSE, na.rm = TRUE))
}

# Load necessary libraries for testing
if (!requireNamespace("testthat", quietly = TRUE)) install.packages("testthat")
library(testthat)

# Define UAT for fs_elastic function
test_fs_elastic <- function() {
  cat("Running UAT for fs_elastic...\n")
  
  # Test 1: Simple numeric data frame without PCA
  set.seed(123)
  df1 <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  result1 <- fs_elastic(df1, y ~ .)
  print(result1)
  expect_s4_class(result1$coef, "dgCMatrix")
  expect_type(result1$alpha, "double")
  expect_type(result1$lambda, "double")
  expect_type(result1$RMSE, "double")
  
  # Test 2: Simple numeric data frame with PCA
  set.seed(123)
  df2 <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  result2 <- fs_elastic(df2, y ~ ., use_pca = TRUE)
  print(result2)
  expect_s4_class(result2$coef, "dgCMatrix")
  expect_type(result2$alpha, "double")
  expect_type(result2$lambda, "double")
  expect_type(result2$RMSE, "double")
  
  # Test 3: Data frame with missing values
  set.seed(123)
  df3 <- data.frame(
    y = c(rnorm(95), rep(NA, 5)),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  result3 <- fs_elastic(df3, y ~ .)
  print(result3)
  expect_s4_class(result3$coef, "dgCMatrix")
  expect_type(result3$alpha, "double")
  expect_type(result3$lambda, "double")
  expect_type(result3$RMSE, "double")
  
  # Test 4: Data frame with categorical variables
  set.seed(123)
  df4 <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = sample(c("A", "B", "C"), 100, replace = TRUE)
  )
  result4 <- fs_elastic(df4, y ~ .)
  print(result4)
  expect_s4_class(result4$coef, "dgCMatrix")
  expect_type(result4$alpha, "double")
  expect_type(result4$lambda, "double")
  expect_type(result4$RMSE, "double")
  
  # Test 5: Data frame with date variables
  set.seed(123)
  df5 <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = seq(as.Date("2001/1/1"), by = "day", length.out = 100)
  )
  result5 <- fs_elastic(df5, y ~ .)
  print(result5)
  expect_s4_class(result5$coef, "dgCMatrix")
  expect_type(result5$alpha, "double")
  expect_type(result5$lambda, "double")
  expect_type(result5$RMSE, "double")
  
  # Test 6: Error handling when target is missing
  set.seed(123)
  df6 <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  expect_error(fs_elastic(df6, y ~ .), "object 'y' not found")
  
  # Test 7: Error handling for invalid formula
  set.seed(123)
  df7 <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  expect_error(fs_elastic(df7, y ~ z), "object 'z' not found")
  
  cat("UAT for fs_elastic completed.\n")
}

# Run the UAT function
test_fs_elastic()
