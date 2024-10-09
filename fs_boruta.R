# Load necessary packages
library(Boruta)
library(caret)
library(doParallel)
library(testthat)

#' @title Perform Feature Selection Using Boruta Algorithm
#'
#' @description
#' This function performs feature selection from a given dataset using the Boruta algorithm. Optionally, you can limit 
#' the number of features and remove highly correlated variables. The function handles parallel processing to speed up 
#' computation when multiple cores are available.
#'
#' @details
#' The function expects the predictor variables to be appropriately formatted:
#' - **Categorical variables** should be factors.
#' - **Date variables** should be converted to numeric representations if they carry meaningful information.
#' - **Non-numeric variables** are handled properly to ensure accurate modeling.
#'
#' @param data A data frame containing the dataset.
#' @param target_var The name of the target variable as a string.
#' @param seed An optional integer for random seed initialization.
#' @param doTrace Verbosity level of the Boruta algorithm (0 = silent, 1 = confirmed attributes, 2 = tentative attributes).
#' @param maxRuns Maximum number of iterations for the Boruta algorithm.
#' @param num_cores Optional integer to specify the number of cores for parallel processing.
#' @param cutoff_features Optional integer to limit the number of selected features.
#' @param cutoff_cor Numeric threshold for correlation coefficient. Features with higher correlation than this are removed.
#'
#' @return A list containing:
#' * `selected_features`: A character vector of the selected feature names.
#' * `boruta_obj`: The Boruta result object.
#'
#' @examples
#' \dontrun{
#' # Load iris dataset
#' data(iris)
#' 
#' # Apply the fs_boruta function
#' result <- fs_boruta(iris, "Species", doTrace = 0, num_cores = 2, cutoff_features = 10, cutoff_cor = 0.7)
#' 
#' # Display the selected features
#' print(result$selected_features)
#' }
#' @seealso 
#' \code{\link[Boruta]{Boruta}} for details on the Boruta algorithm.
#'
#' @importFrom Boruta Boruta getSelectedAttributes
#' @importFrom caret findCorrelation
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @export
fs_boruta <- function(data, target_var, seed = NULL, doTrace = 1, maxRuns = 250,
                      num_cores = NULL, cutoff_features = NULL, cutoff_cor = 0.7) {
  # Check if the target variable exists in the data
  if (!(target_var %in% names(data))) {
    stop("The target variable is not found in the provided data frame.")
  }
  
  # Create a copy of the data without the target variable
  predictors <- data[, setdiff(names(data), target_var), drop = FALSE]
  
  # Create a vector of the dependent variable
  y <- data[[target_var]]
  
  # Check for unsupported variable types
  unsupported_vars <- names(predictors)[sapply(predictors, function(x) {
    is.matrix(x) || is.array(x) || (!is.numeric(x) && !is.factor(x) && !is.character(x) && !inherits(x, "Date"))
  })]
  if (length(unsupported_vars) > 0) {
    stop(paste("Unsupported variable types found in the following columns:", paste(unsupported_vars, collapse = ", ")))
  }
  
  # Ensure character variables are converted to factors
  categorical_vars <- names(predictors)[sapply(predictors, is.character)]
  if (length(categorical_vars) > 0) {
    predictors[categorical_vars] <- lapply(predictors[categorical_vars], as.factor)
  }
  
  # Convert Date variables to numeric
  date_vars <- names(predictors)[sapply(predictors, inherits, "Date")]
  if (length(date_vars) > 0) {
    predictors[date_vars] <- lapply(predictors[date_vars], as.numeric)
  }
  
  # Double-check for any remaining unsupported variable types
  unsupported_vars <- names(predictors)[sapply(predictors, function(x) {
    !is.numeric(x) && !is.factor(x)
  })]
  if (length(unsupported_vars) > 0) {
    stop(paste("After processing, unsupported variable types found in the following columns:", paste(unsupported_vars, collapse = ", ")))
  }
  
  # Run the Boruta algorithm
  if (!is.null(num_cores)) {
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)
    on.exit({
      stopCluster(cl)
      registerDoSEQ()
    }, add = TRUE)
  }
  
  boruta_obj <- Boruta(x = predictors, y = y, doTrace = doTrace, maxRuns = maxRuns, seed = seed)
  
  # Get the selected features
  selected_features <- getSelectedAttributes(boruta_obj, withTentative = FALSE)
  
  if (length(selected_features) == 0) {
    return(list(selected_features = character(0), boruta_obj = boruta_obj))
  }
  
  # If only one feature is selected, skip correlation step
  if (length(selected_features) > 1) {
    # Get the selected data
    selected_data <- predictors[, selected_features, drop = FALSE]
    # Filter numeric variables
    numeric_vars <- names(selected_data)[sapply(selected_data, is.numeric)]
    if (length(numeric_vars) > 1) {
      numeric_data <- selected_data[, numeric_vars, drop = FALSE]
      # Get the correlations among selected numeric features
      correlation_matrix <- cor(numeric_data)
      # Get the indices of high correlations
      to_drop <- findCorrelation(correlation_matrix, cutoff = cutoff_cor)
      # Map indices back to the original selected_features
      to_drop_features <- numeric_vars[to_drop]
      # Drop the variables
      selected_features <- setdiff(selected_features, to_drop_features)
    }
  }
  
  # Optionally limit the number of selected features
  if (!is.null(cutoff_features) && length(selected_features) > cutoff_features) {
    selected_features <- head(selected_features, cutoff_features)
  }
  
  return(list(selected_features = selected_features, boruta_obj = boruta_obj))
}

# Define UAT for fs_boruta function
test_fs_boruta <- function() {
  cat("Running UAT for fs_boruta...\n")
  
  # Test 1: Simple numeric data frame
  test_that("fs_boruta works with numeric data", {
    df1 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(1:5, 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE)
    )
    result1 <- fs_boruta(df1, "target")
    expect_type(result1$selected_features, "character")
    expect_true(is.list(result1$boruta_obj))
    expect_true("finalDecision" %in% names(result1$boruta_obj))
  })
  
  # Test 2: Data frame with categorical variables
  test_that("fs_boruta handles categorical variables", {
    df2 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE),
      target = sample(1:2, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    result2 <- fs_boruta(df2, "target")
    expect_type(result2$selected_features, "character")
    expect_true(is.list(result2$boruta_obj))
    expect_true("finalDecision" %in% names(result2$boruta_obj))
  })
  
  # Test 3: Data frame with date variables
  test_that("fs_boruta handles date variables", {
    df3 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE),
      C = seq(as.Date("2001/1/1"), by = "day", length.out = 100),
      target = sample(1:2, 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    result3 <- fs_boruta(df3, "target")
    expect_type(result3$selected_features, "character")
    expect_true(is.list(result3$boruta_obj))
    expect_true("finalDecision" %in% names(result3$boruta_obj))
  })
  
  # Test 4: Error handling when target is missing
  test_that("fs_boruta stops when target variable is missing", {
    df4 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = sample(c("yes", "no"), 100, replace = TRUE)
    )
    expect_error(fs_boruta(df4, "target"), "The target variable is not found in the provided data frame.")
  })
  
  # Test 5: Data frame with non-numeric and non-factor variables
  test_that("fs_boruta handles non-numeric, non-factor variables", {
    df5 <- data.frame(
      A = sample(1:10, 100, replace = TRUE),
      B = I(matrix(sample(1:5, 200, replace = TRUE), ncol = 2)),
      target = sample(1:2, 100, replace = TRUE)
    )
    expect_error(fs_boruta(df5, "target"), "Unsupported variable types found")
  })
  
  # Test 6: Limiting the number of selected features
  test_that("fs_boruta limits the number of features", {
    df6 <- data.frame(
      A = rnorm(100),
      B = rnorm(100),
      C = rnorm(100),
      D = rnorm(100),
      target = sample(1:2, 100, replace = TRUE)
    )
    result6 <- fs_boruta(df6, "target", cutoff_features = 2)
    expect_lte(length(result6$selected_features), 2)
  })
  
  # Test 7: Correlation cutoff
  test_that("fs_boruta removes highly correlated features", {
    df7 <- data.frame(
      A = rnorm(100),
      B = rnorm(100),
      C = rnorm(100),
      D = rnorm(100)
    )
    df7$E <- df7$A + rnorm(100, sd = 0.01)  # Highly correlated with A
    df7$target <- sample(1:2, 100, replace = TRUE)
    result7 <- fs_boruta(df7, "target", cutoff_cor = 0.9)
    expect_false(all(c("A", "E") %in% result7$selected_features))
  })
  
  cat("UAT for fs_boruta completed.\n")
}

# Run the UAT functions
test_fs_boruta()
