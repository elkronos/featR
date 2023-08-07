# Load the necessary packages
library(glmnet)
library(caret)
library(doParallel)

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
#' @importFrom stats prcomp model.matrix model.response
#' @export
fs_elastic <- function(data, formula, alpha = seq(0, 1, by = 0.1), 
                       trControl = trainControl(method = "cv", number = 5), 
                       use_pca = FALSE, nPCs = NULL, lambda_seq = NULL) {
  
  # Extract the response and predictor variables from the formula
  y <- model.response(model.frame(formula, data))
  x <- model.matrix(formula, data)[,-1]
  
  # Handle missing values
  complete_cases <- complete.cases(x, y)
  if(any(!complete_cases)){
    y <- y[complete_cases]
    x <- x[complete_cases, ]
  }
  
  if(is.null(lambda_seq)){
    lambda_seq <- 10^seq(-3, 3, length = 100)
  }
  
  tuneGrid <- expand.grid(alpha = alpha, lambda = lambda_seq)
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  clusterEvalQ(cl, {
    library(caret)
    library(glmnet)
  })
  
  cv_fit <- foreach(i = 1:nrow(tuneGrid), .combine = "list", .packages = c("caret", "glmnet")) %dopar% {
    a <- tuneGrid$alpha[i]
    l <- tuneGrid$lambda[i]
    x_train <- x
    if (use_pca) {
      pca <- prcomp(x, scale. = TRUE)
      if(is.null(nPCs)){
        x_train <- pca$x
      } else {
        x_train <- pca$x[, 1:min(nPCs, ncol(pca$x))]
      }
    }
    fit <- train(x_train, y,
                 method = "glmnet",
                 tuneGrid = expand.grid(alpha = a, lambda = l),
                 trControl = trControl)
    return(list(RMSE = min(fit$results$RMSE, na.rm = TRUE), model = fit))
  }
  
  stopCluster(cl)
  
  rmse_values <- sapply(cv_fit, function(fit) {
    return(fit$RMSE)
  })
  
  best_index <- which.min(rmse_values)
  best_model <- cv_fit[[best_index]]$model
  
  list(coef = coef(best_model$finalModel, s = best_model$bestTune$lambda),
       alpha = best_model$bestTune$alpha,
       lambda = best_model$bestTune$lambda,
       RMSE = min(best_model$results$RMSE, na.rm = TRUE))
}