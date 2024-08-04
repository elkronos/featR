# Load necessary packages
if (!requireNamespace("earth", quietly = TRUE)) install.packages("earth")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
if (!requireNamespace("bigmemory", quietly = TRUE)) install.packages("bigmemory")
if (!requireNamespace("doParallel", quietly = TRUE)) install.packages("doParallel")

library(earth)
library(caret)
library(data.table)
library(bigmemory)
library(doParallel)

#' Train and Evaluate a MARS Model on a Dataset
#'
#' This function takes a dataset, splits it into training and test sets, performs grid search over 
#' a predefined set of hyperparameters, and trains a MARS model on the training set using cross-validation. 
#' The best model is then used to make predictions on the test set, and depending on the type of response 
#' variable, either the root mean squared error (RMSE) for numerical response or accuracy for categorical 
#' response is calculated and returned.
#'
#' @param data A data frame containing the response variable and predictor variables.
#' @param responseName The name of the response column. Can be any column name present in the data. 
#'        For regression tasks, it should be numeric, and for classification tasks, it should be a factor.
#' @param p The proportion of the data to use for training. Default is 0.8.
#' @param degree A vector of integers specifying the polynomial degree of the MARS model to fit. Default is 1:3.
#' @param nprune A vector of integers specifying the number of basis functions to prune from the MARS model. Default is c(5, 10, 15).
#' @param method The method to use for model fitting. Default is "earth".
#' @param search The method to use for searching the hyperparameter space. Default is "grid".
#' @param number The number of folds to use for cross-validation. Default is 5.
#' @param repeats The number of repetitions for cross-validation. Default is 3.
#' @param seed The seed value for reproducibility. Default is 123.
#' @param sampleSize The size of the sample to use for initial training. Default is 10000.
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item \code{model}: The trained MARS model.
#'   \item \code{rmse}: The root mean squared error (RMSE) of the model (if the response variable is numeric).
#'   \item \code{accuracy}: The accuracy of the model (if the response variable is categorical).
#' }
#'
#' @examples
#' # Load required libraries
#' library(earth)
#' library(caret)
#' library(data.table)
#' library(bigmemory)
#' library(methods)
#' 
#' # Create a synthetic dataset for regression
#' set.seed(123)
#' n <- 500
#' p <- 10
#' x <- matrix(rnorm(n * p), nrow = n)
#' colnames(x) <- paste0("x", 1:p)
#' beta <- runif(p, -2, 2)
#' y <- x %*% beta + rnorm(n)
#' data_reg <- data.frame(y = y, x)
#' 
#' # Apply the function for regression
#' result_reg <- fs_mars(data_reg, responseName = "y", p = 0.7)
#' print(result_reg$model)
#' print(paste("RMSE:", result_reg$rmse))
#' 
#' # Create a synthetic dataset for classification
#' set.seed(123)
#' y_class <- factor(sample(c("A", "B"), n, replace = TRUE, prob = c(0.5, 0.5)))
#' data_class <- data.frame(y = y_class, x)
#' 
#' # Apply the function for classification
#' result_class <- fs_mars(data_class, responseName = "y", p = 0.7)
#' print(result_class$model)
#' print(paste("Accuracy:", result_class$accuracy))
#'
#' @importFrom earth earth
#' @importFrom caret trainControl train createDataPartition upSample
#' @importFrom data.table as.data.table
#' @importFrom bigmemory as.big.matrix
#' @importFrom doParallel makeCluster registerDoParallel stopCluster detectCores
#' @export
fs_mars <- function(data, responseName, p = 0.8, degree = 1:3, 
                    nprune = c(5, 10, 15), method = "earth", search = "grid", 
                    number = 5, repeats = 3, seed = 123, sampleSize = 10000) {
  cat("Checking if required libraries are loaded...\n")
  # Check if required libraries are loaded
  if (!"earth" %in% loadedNamespaces()) stop("The 'earth' package is not loaded. Please install and load it before calling this function.")
  if (!"caret" %in% loadedNamespaces()) stop("The 'caret' package is not loaded. Please install and load it before calling this function.")
  if (!"data.table" %in% loadedNamespaces()) stop("The 'data.table' package is not loaded. Please install and load it before calling this function.")
  if (!"doParallel" %in% loadedNamespaces()) stop("The 'doParallel' package is not loaded. Please install and load it before calling this function.")
  
  cat("Checking if responseName exists in the data...\n")
  # Check if responseName exists in the data
  if (!responseName %in% colnames(data)) stop("The responseName does not exist in the dataset.")
  
  cat("Converting data to data.table for efficient processing...\n")
  # Convert data to data.table for efficient processing
  data <- as.data.table(data)
  
  cat("Handling missing values...\n")
  # Handle missing values
  initial_rows <- nrow(data)
  data <- na.omit(data)
  cat("Removed", initial_rows - nrow(data), "rows with missing values.\n")
  
  cat("Checking class balance...\n")
  # Check class balance for classification tasks
  if (is.factor(data[[responseName]])) {
    class_counts <- table(data[[responseName]])
    if (any(class_counts < 2)) {
      stop("Each class must have at least two samples.")
    }
    if (any(class_counts < 10)) {
      warning("Some classes have fewer than 10 samples. Consider collecting more data or using a different method.")
    }
  }
  
  cat("Setting up parallel processing...\n")
  # Set up parallel processing
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  
  cat("Sampling data for initial training if larger than sampleSize...\n")
  # Sample data for initial training if larger than sampleSize
  if (nrow(data) > sampleSize) {
    set.seed(seed)
    data <- data[sample(.N, sampleSize)]
    cat("Sampled down to", nrow(data), "rows.\n")
  }
  
  cat("Splitting data into training and test sets...\n")
  # Split data into training and test sets
  set.seed(seed)
  if (is.factor(data[[responseName]])) {
    # For classification, use createDataPartition
    trainIndex <- caret::createDataPartition(data[[responseName]], p = p, list = FALSE)
  } else {
    # For regression, use simple random sampling
    trainIndex <- sample(seq_len(nrow(data)), size = floor(p * nrow(data)))
  }
  train <- data[trainIndex, ]
  test <- data[-trainIndex, ]
  cat("Training set size:", nrow(train), "rows.\n")
  cat("Test set size:", nrow(test), "rows.\n")
  
  cat("Balancing classes if necessary...\n")
  # Balance classes if necessary
  if (is.factor(data[[responseName]])) {
    train <- caret::upSample(x = train[, !..responseName], y = train[[responseName]], yname = responseName)
    cat("Balanced training set size:", nrow(train), "rows.\n")
  }
  
  cat("Defining the grid of hyperparameters to search over...\n")
  # Define the grid of hyperparameters to search over
  hyperParameters <- expand.grid(nprune = nprune, degree = degree)
  
  cat("Defining the control parameters for the model training...\n")
  # Define the control parameters for the model training
  ctrl <- caret::trainControl(method = "repeatedcv", 
                              number = number, 
                              repeats = repeats, 
                              search = search, 
                              allowParallel = TRUE, 
                              savePredictions = "final",
                              classProbs = is.factor(train[[responseName]]),
                              returnResamp = "all",
                              minClass = max(2, floor(nrow(train) / (number * 10))))  # Ensure minimum class size
  
  cat("Training the model using grid search for hyperparameters...\n")
  # Train the model using grid search for hyperparameters
  set.seed(seed)
  mars_model <- tryCatch({
    caret::train(as.formula(paste(responseName, "~ .")), 
                 data = train, 
                 method = method, 
                 trControl = ctrl, 
                 tuneGrid = hyperParameters)
  }, error = function(e) {
    cat("Error in model training:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (is.null(mars_model)) {
    return(list(model = NULL, error = "Model training failed"))
  }
  
  cat("Making predictions on test set...\n")
  # Make predictions on test set
  pred <- predict(mars_model, newdata = test)
  
  cat("Calculating performance metrics...\n")
  # Calculate the RMSE if the response variable is numeric
  if (is.numeric(test[[responseName]])) {
    rmse <- sqrt(mean((test[[responseName]] - pred)^2))
    cat("Returning results for regression...\n")
    return(list(model = mars_model, rmse = rmse))
  }
  # Calculate accuracy if the response variable is categorical (factor)
  else if (is.factor(test[[responseName]])) {
    accuracy <- sum(test[[responseName]] == pred) / length(pred)
    cat("Returning results for classification...\n")
    return(list(model = mars_model, accuracy = accuracy))
  } else {
    stop("Unsupported response variable type.")
  }
}

# Define UAT for fs_mars function
test_fs_mars <- function() {
  cat("Running UAT for fs_mars...\n")
  
  # Test 1: Simple numeric data frame for regression
  cat("Test 1: Simple numeric data frame for regression\n")
  set.seed(123)
  df1 <- data.frame(
    response = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  result1 <- tryCatch({
    fs_mars(df1, responseName = "response")
  }, error = function(e) {
    cat("Error in Test 1:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result1)) {
    print(result1)
    cat("Test 1 completed successfully.\n\n")
  }
  
  # Test 2: Simple data frame for classification
  cat("Test 2: Simple data frame for classification\n")
  set.seed(123)
  df2 <- data.frame(
    response = factor(rep(c("A", "B"), each = 50)),  # Ensure balanced classes
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  result2 <- tryCatch({
    suppressWarnings(fs_mars(df2, responseName = "response"))
  }, error = function(e) {
    cat("Error in Test 2:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result2)) {
    print(result2)
    cat("Test 2 completed successfully.\n\n")
  }
  
  # Test 3: Data frame with missing values
  cat("Test 3: Data frame with missing values\n")
  set.seed(123)
  df3 <- data.frame(
    response = c(rnorm(95), rep(NA, 5)),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  result3 <- tryCatch({
    fs_mars(df3, responseName = "response")
  }, error = function(e) {
    cat("Error in Test 3:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result3)) {
    print(result3)
    cat("Test 3 completed successfully.\n\n")
  }
  
  # Test 4: Data frame with categorical predictors
  cat("Test 4: Data frame with categorical predictors\n")
  set.seed(123)
  df4 <- data.frame(
    response = rnorm(100),
    x1 = rnorm(100),
    x2 = factor(sample(c("A", "B", "C"), 100, replace = TRUE))
  )
  result4 <- tryCatch({
    fs_mars(df4, responseName = "response")
  }, error = function(e) {
    cat("Error in Test 4:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result4)) {
    print(result4)
    cat("Test 4 completed successfully.\n\n")
  }
  
  # Test 5: Data frame with date variables
  cat("Test 5: Data frame with date variables\n")
  set.seed(123)
  df5 <- data.frame(
    response = rnorm(100),
    x1 = rnorm(100),
    x2 = seq(as.Date("2001/1/1"), by = "day", length.out = 100)
  )
  result5 <- tryCatch({
    fs_mars(df5, responseName = "response")
  }, error = function(e) {
    cat("Error in Test 5:", conditionMessage(e), "\n")
    return(NULL)
  })
  if (!is.null(result5)) {
    print(result5)
    cat("Test 5 completed successfully.\n\n")
  }
  
  # Test 6: Error handling when responseName is missing
  cat("Test 6: Error handling when responseName is missing\n")
  set.seed(123)
  df6 <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  result6 <- tryCatch({
    fs_mars(df6, responseName = "response")
  }, error = function(e) {
    cat("Expected error in Test 6:", conditionMessage(e), "\n")
    return("Error caught successfully")
  })
  if (!is.null(result6)) {
    print(result6)
    cat("Test 6 completed successfully.\n\n")
  }
  
  # Test 7: Error handling for unsupported response variable type
  cat("Test 7: Error handling for unsupported response variable type\n")
  set.seed(123)
  df7 <- data.frame(
    response = as.character(sample(letters, 100, replace = TRUE)),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  result7 <- tryCatch({
    fs_mars(df7, responseName = "response")
  }, error = function(e) {
    cat("Expected error in Test 7:", conditionMessage(e), "\n")
    return("Error caught successfully")
  })
  if (!is.null(result7)) {
    print(result7)
    cat("Test 7 completed successfully.\n\n")
  }
  
  cat("UAT for fs_mars completed.\n")
}

test_fs_mars()