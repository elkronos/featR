# Required Packages
library(earth)
library(caret)
library(data.table)
library(testthat)

# ---------------------------
# Utility Functions
# ---------------------------

#' Check Required Libraries
#'
#' Verifies that all required libraries are loaded.
#'
#' @return invisible(NULL) if all required libraries are loaded.
#' @export
check_libraries <- function() {
  required_libs <- c("earth", "caret", "data.table")
  for (lib in required_libs) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      stop(sprintf("Package '%s' is not installed. Please install it before proceeding.", lib))
    }
  }
  invisible(NULL)
}

#' Check Response Column
#'
#' Validates that the specified response column exists in the dataset.
#'
#' @param data A data.table or data.frame.
#' @param responseName Character. The name of the response column.
#'
#' @return invisible(NULL) if the response column is found.
#' @export
check_response_column <- function(data, responseName) {
  if (!(responseName %in% colnames(data))) {
    stop("The specified response column does not exist in the dataset.")
  }
  invisible(NULL)
}

#' Handle Missing Values
#'
#' Removes rows with missing values.
#'
#' @param data A data.table.
#'
#' @return data.table. The dataset with missing rows removed.
#' @export
handle_missing_values <- function(data) {
  initial_rows <- nrow(data)
  data_clean <- na.omit(data)
  message(sprintf("Removed %d rows with missing values.", initial_rows - nrow(data_clean)))
  return(data_clean)
}

#' Check Class Balance
#'
#' Checks whether each class in a factor response variable meets minimum counts.
#'
#' @param data A data.table.
#' @param responseName Character. The name of the response column.
#' @param show_warnings Logical. If TRUE, issues warnings for classes with low counts.
#'
#' @return invisible(NULL) if class balance conditions are met.
#' @export
check_class_balance <- function(data, responseName, show_warnings = TRUE) {
  if (is.factor(data[[responseName]])) {
    counts <- table(data[[responseName]])
    if (any(counts < 2)) {
      stop("Each class must have at least two samples.")
    }
    if (any(counts < 10) && show_warnings) {
      warning("Some classes have fewer than 10 samples. Oversampling will be used to balance classes.")
    }
  }
  invisible(NULL)
}

#' Sample Data
#'
#' Samples the data down to a specified sample size.
#'
#' @param data A data.table.
#' @param sampleSize Integer. Maximum number of rows to retain.
#' @param seed Integer. Random seed.
#'
#' @return data.table. The sampled dataset.
#' @export
sample_data <- function(data, sampleSize, seed) {
  if (nrow(data) > sampleSize) {
    set.seed(seed)
    data <- data[sample(.N, sampleSize)]
    message(sprintf("Data sampled down to %d rows.", sampleSize))
  }
  return(data)
}

#' Split Data
#'
#' Splits data into training and test sets.
#'
#' @param data A data.table.
#' @param responseName Character. The name of the response column.
#' @param train_prop Numeric. Proportion of data for training.
#' @param seed Integer. Random seed.
#'
#' @return A list with components \code{train} and \code{test}.
#' @export
split_data <- function(data, responseName, train_prop, seed) {
  set.seed(seed)
  if (is.factor(data[[responseName]])) {
    train_idx <- caret::createDataPartition(data[[responseName]], p = train_prop, list = FALSE)
  } else {
    train_idx <- sample(seq_len(nrow(data)), size = floor(train_prop * nrow(data)))
  }
  list(train = data[train_idx, ], test = data[-train_idx, ])
}

#' Balance Classes
#'
#' Balances classes by oversampling minority classes.
#'
#' @param train A data.table representing the training set.
#' @param responseName Character. The name of the response column.
#'
#' @return data.table. The balanced training set.
#' @export
balance_classes <- function(train, responseName) {
  if (is.factor(train[[responseName]])) {
    counts <- table(train[[responseName]])
    max_count <- max(counts)
    balanced_list <- lapply(names(counts), function(cl) {
      subset_data <- train[train[[responseName]] == cl]
      if (nrow(subset_data) < max_count) {
        subset_data[sample(1:.N, max_count, replace = TRUE)]
      } else {
        subset_data
      }
    })
    train_balanced <- rbindlist(balanced_list)
    # Shuffle the rows
    train_balanced <- train_balanced[sample(.N)]
    message(sprintf("Balanced training set size: %d rows.", nrow(train_balanced)))
    return(train_balanced)
  }
  return(train)
}

#' Define Hyperparameter Grid
#'
#' Creates a grid of hyperparameters for tuning.
#'
#' @param degree Integer vector. Degrees of interaction.
#' @param nprune Integer vector. Number of terms to prune.
#'
#' @return data.frame. The hyperparameter grid.
#' @export
define_hyperparameter_grid <- function(degree, nprune) {
  expand.grid(nprune = nprune, degree = degree)
}

#' Define Training Control
#'
#' Sets up the caret training control parameters.
#'
#' @param number Integer. Number of cross-validation folds.
#' @param repeats Integer. Number of repeats for cross-validation.
#' @param search Character. Hyperparameter search method (e.g., "grid").
#' @param train A data.table. The training dataset.
#' @param responseName Character. The response column.
#'
#' @return A \code{trainControl} object.
#' @export
define_train_control <- function(number, repeats, search, train, responseName) {
  custom_summary <- caret::defaultSummary
  function_with_na_rm <- function(data, lev = NULL, model = NULL) {
    # Remove NA values in performance measures if any
    metrics <- custom_summary(data, lev, model)
    metrics <- lapply(metrics, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else x)
    unlist(metrics)
  }
  
  ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = number,
    repeats = repeats,
    search = search,
    allowParallel = TRUE,
    savePredictions = "final",
    classProbs = is.factor(train[[responseName]]),
    returnResamp = "all",
    summaryFunction = function_with_na_rm,
    verboseIter = TRUE
  )
  
  if (!inherits(ctrl, "trainControl")) {
    class(ctrl) <- c("trainControl", class(ctrl))
  }
  
  return(ctrl)
}

#' Train Model
#'
#' Trains a model using caret with the specified parameters.
#'
#' @param train A data.table. The training dataset.
#' @param responseName Character. The name of the response column.
#' @param method Character. The model method (e.g., "earth").
#' @param ctrl A trainControl object.
#' @param hyperParameters data.frame. The hyperparameter grid.
#' @param seed Integer. Random seed.
#'
#' @return A trained model object.
#' @export
train_model <- function(train, responseName, method, ctrl, hyperParameters, seed) {
  set.seed(seed)
  model <- tryCatch({
    caret::train(
      as.formula(sprintf("%s ~ .", responseName)),
      data = train,
      method = method,
      trControl = ctrl,
      tuneGrid = hyperParameters
    )
  }, error = function(e) {
    stop(sprintf("Error during model training: %s", conditionMessage(e)))
  })
  model
}

#' Evaluate Model
#'
#' Evaluates a trained model on the test set.
#'
#' @param model The trained model.
#' @param test A data.table. The test dataset.
#' @param responseName Character. The response column.
#'
#' @return A list of performance metrics and the model.
#' @export
evaluate_model <- function(model, test, responseName) {
  pred <- predict(model, newdata = test)
  if (is.numeric(test[[responseName]])) {
    rmse_val <- sqrt(mean((test[[responseName]] - pred)^2))
    list(model = model, rmse = rmse_val)
  } else if (is.factor(test[[responseName]])) {
    pred <- factor(pred, levels = levels(test[[responseName]]))
    cm <- caret::confusionMatrix(pred, test[[responseName]])
    list(model = model, accuracy = cm$overall["Accuracy"], confusion_matrix = cm)
  } else {
    stop("Unsupported response variable type.")
  }
}

# ---------------------------
# Main Function: fs_mars
# ---------------------------

#' MARS Feature Selection and Model Training
#'
#' Orchestrates feature selection and model training using MARS (Multivariate Adaptive Regression Splines).
#'
#' @param data A data.frame or data.table.
#' @param responseName Character. The name of the response column.
#' @param p Numeric. Proportion of data for training (default 0.8).
#' @param degree Integer vector. Degrees of interaction (default 1:3).
#' @param nprune Integer vector. Number of terms to prune (default c(5, 10, 15)).
#' @param method Character. Modeling method (default "earth").
#' @param search Character. Hyperparameter search method (default "grid").
#' @param number Integer. Number of CV folds (default 5).
#' @param repeats Integer. Number of CV repeats (default 3).
#' @param seed Integer. Random seed (default 123).
#' @param sampleSize Integer. Maximum number of samples to use (default 10000).
#' @param show_warnings Logical. Whether to show warnings for class imbalance (default TRUE).
#' @param verbose Logical. If TRUE, prints progress messages (default TRUE).
#'
#' @return A list containing the trained model and evaluation metrics.
#' @export
fs_mars <- function(data, responseName, 
                    p = 0.8, 
                    degree = 1:3, 
                    nprune = c(5, 10, 15), 
                    method = "earth", 
                    search = "grid", 
                    number = 5, 
                    repeats = 3, 
                    seed = 123, 
                    sampleSize = 10000,
                    show_warnings = TRUE,
                    verbose = TRUE) {
  
  # Check required libraries and response column
  if (verbose) message("Verifying required libraries...")
  check_libraries()
  
  if (verbose) message("Checking response column existence...")
  check_response_column(data, responseName)
  
  # Convert data to data.table if not already
  if (!is.data.table(data)) {
    data <- as.data.table(data)
    if (verbose) message("Converted input data to data.table.")
  }
  
  # Handle missing values
  data <- handle_missing_values(data)
  
  # Check class balance (only applies for factor responses)
  check_class_balance(data, responseName, show_warnings)
  
  # Sample data if necessary (useful for very large datasets)
  data <- sample_data(data, sampleSize, seed)
  
  # Split data into training and test sets
  if (verbose) message("Splitting data into training and test sets...")
  splits <- split_data(data, responseName, p, seed)
  train <- splits$train
  test <- splits$test
  if (verbose) {
    message(sprintf("Training set: %d rows; Test set: %d rows.", nrow(train), nrow(test)))
  }
  
  # Balance classes in training data if necessary
  train <- balance_classes(train, responseName)
  
  # Define hyperparameter grid and training control
  hyperParameters <- define_hyperparameter_grid(degree, nprune)
  ctrl <- define_train_control(number, repeats, search, train, responseName)
  
  # Train the model
  if (verbose) message("Training the model...")
  model <- train_model(train, responseName, method, ctrl, hyperParameters, seed)
  
  # Ensure factor levels are consistent between train and test
  if (is.factor(train[[responseName]])) {
    test[[responseName]] <- factor(test[[responseName]], levels = levels(train[[responseName]]))
  }
  
  # Evaluate the model
  if (verbose) message("Evaluating model performance...")
  eval_metrics <- evaluate_model(model, test, responseName)
  
  if (verbose) message("Model training and evaluation complete.")
  return(eval_metrics)
}

