###############################################################################
# Testing Infrastructure - Random Forest
###############################################################################

test_fs_randomforest <- function() {
  cat("Running unit tests for fs_randomforest...\n")
  
  # Test 1: Regression on a simple numeric data frame
  df1 <- data.frame(
    A = sample(1:100, 1000, replace = TRUE),
    B = sample(1:50, 1000, replace = TRUE),
    target = rnorm(1000)
  )
  result1 <- fs_randomforest(df1, "target", type = "regression")
  cat("Test 1 RMSE:", result1$RMSE, "\n")
  expect_s3_class(result1$model, "randomForest")
  expect_type(result1$predictions, "double")
  expect_type(result1$RMSE, "double")
  
  # Test 2: Classification on a simple numeric data frame
  df2 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = sample(1:5, 1000, replace = TRUE),
    target = factor(sample(1:2, 1000, replace = TRUE))
  )
  result2 <- fs_randomforest(df2, "target", type = "classification")
  cat("Test 2 Accuracy:", result2$accuracy, "\n")
  expect_s3_class(result2$model, "randomForest")
  expect_true(is.factor(result2$predictions))
  expect_type(result2$accuracy, "double")
  
  # Test 3: Classification with a categorical predictor
  df3 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = factor(sample(c("yes", "no"), 1000, replace = TRUE)),
    target = factor(sample(1:2, 1000, replace = TRUE))
  )
  result3 <- fs_randomforest(df3, "target", type = "classification")
  cat("Test 3 Accuracy:", result3$accuracy, "\n")
  expect_s3_class(result3$model, "randomForest")
  expect_true(is.factor(result3$predictions))
  expect_type(result3$accuracy, "double")
  
  # Test 4: Classification with date variables
  df4 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = factor(sample(c("yes", "no"), 1000, replace = TRUE)),
    C = seq(as.Date("2001-01-01"), by = "day", length.out = 1000),
    target = factor(sample(1:2, 1000, replace = TRUE))
  )
  result4 <- fs_randomforest(df4, "target", type = "classification")
  cat("Test 4 Accuracy:", result4$accuracy, "\n")
  expect_s3_class(result4$model, "randomForest")
  expect_true(is.factor(result4$predictions))
  expect_type(result4$accuracy, "double")
  
  # Test 5: Error when target is missing
  df5 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = factor(sample(c("yes", "no"), 1000, replace = TRUE))
  )
  expect_error(fs_randomforest(df5, "target", type = "classification"),
               "The target variable does not exist in the dataset.")
  
  # Test 6: Error for invalid type parameter
  df6 <- data.frame(
    A = sample(1:10, 1000, replace = TRUE),
    B = sample(1:5, 1000, replace = TRUE),
    target = sample(1:2, 1000, replace = TRUE)
  )
  expect_error(fs_randomforest(df6, "target", type = "invalid_type"),
               "Invalid type parameter. Choose 'classification' or 'regression'.")
  
  # Test 7: Regression with a preprocessing function
  preprocess_function <- function(data) {
    data$A <- data$A / max(data$A)
    return(data)
  }
  df7 <- data.frame(
    A = sample(1:100, 1000, replace = TRUE),
    B = sample(1:50, 1000, replace = TRUE),
    target = rnorm(1000)
  )
  result7 <- fs_randomforest(df7, "target", type = "regression", preprocess = preprocess_function)
  cat("Test 7 RMSE:", result7$RMSE, "\n")
  expect_s3_class(result7$model, "randomForest")
  expect_type(result7$predictions, "double")
  expect_type(result7$RMSE, "double")
  
  cat("All unit tests for fs_randomforest completed.\n")
}

# Run tests
# test_fs_randomforest()
