###############################################################################
# Testing Infrastructure - MARS
###############################################################################

# NOTE: These tests assume you've sourced the functions first.
# We keep CV small for speed. Use skip_on_cran() for heavier bits.

library(testthat)
library(data.table)
library(caret)

SEED <- 123

# --------------------------------------------------------------------
# Helper: tiny tuning grid & CV control for quick tests
# --------------------------------------------------------------------
.quick_grid <- function() define_hyperparameter_grid(degree = 1:2, nprune = c(3, 5))
.quick_ctrl <- function(train, responseName) {
  define_train_control(
    number = 3, repeats = 1, search = "grid",
    train = train, responseName = responseName, seed = SEED
  )
}

test_fs_mars_functions <- function() {
  message("Running unit tests for fs_mars and utilities...")
  
  # ------------------------------------------------------------------
  # check_libraries
  # ------------------------------------------------------------------
  test_that("check_libraries verifies required and notes optional", {
    expect_error(check_libraries(), NA)
  })
  
  # ------------------------------------------------------------------
  # check_response_column + coerce_response
  # ------------------------------------------------------------------
  test_that("check_response_column detects missing columns; coerce_response handles character", {
    dt <- data.table(y = rnorm(20), x = rnorm(20))
    expect_error(check_response_column(dt, "missing"), "does not exist")
    expect_error(check_response_column(dt, "y"), NA)
    
    # coerce character -> factor
    dtc <- data.table(y = sample(c("A","B"), 20, TRUE), x = rnorm(20))
    dtc2 <- coerce_response(dtc, "y")
    expect_true(is.factor(dtc2$y))
  })
  
  # ------------------------------------------------------------------
  # handle_missing_values
  # ------------------------------------------------------------------
  test_that("handle_missing_values removes NA rows", {
    dt <- data.table(y = c(rnorm(18), NA, NA),
                     x = c(rnorm(17), NA, NA, NA))
    dt_clean <- handle_missing_values(dt)
    expect_true(sum(is.na(dt_clean)) == 0)
    expect_true(nrow(dt_clean) <= nrow(dt))
  })
  
  # ------------------------------------------------------------------
  # check_class_balance + balance_classes
  # ------------------------------------------------------------------
  test_that("balance_classes oversamples minority classes to max count", {
    dt <- data.table(y = factor(rep(c("A","B"), c(35, 15))),
                     x = rnorm(50))
    expect_error(check_class_balance(dt, "y"), NA)
    dt_bal <- balance_classes(dt, "y")
    cnt <- table(dt_bal$y)
    expect_equal(min(cnt), max(cnt))
  })
  
  # ------------------------------------------------------------------
  # sample_data
  # ------------------------------------------------------------------
  test_that("sample_data reduces dataset size deterministically", {
    dt <- data.table(y = rnorm(2000), x = rnorm(2000))
    d1 <- sample_data(dt, 1000, seed = SEED)
    d2 <- sample_data(dt, 1000, seed = SEED)
    expect_equal(nrow(d1), 1000)
    expect_equal(nrow(d2), 1000)
    expect_equal(d1, d2)  # same rows with same seed
  })
  
  # ------------------------------------------------------------------
  # split_data (stratified via createDataPartition for both tasks)
  # ------------------------------------------------------------------
  test_that("split_data creates requested split sizes", {
    set.seed(SEED)
    dt <- data.table(y = rnorm(100), x = rnorm(100))
    sp <- split_data(dt, "y", 0.8, seed = SEED)
    expect_equal(nrow(sp$train), 80)
    expect_equal(nrow(sp$test), 20)
  })
  
  # ------------------------------------------------------------------
  # define_hyperparameter_grid
  # ------------------------------------------------------------------
  test_that("define_hyperparameter_grid creates sorted unique grid", {
    grid <- define_hyperparameter_grid(1:2, c(5, 5, 3))
    expect_equal(nrow(grid), 4)  # (2 degrees) x (unique nprune {3,5})
    expect_true(all(sort(unique(grid$degree)) == c(1,2)))
    expect_true(all(sort(unique(grid$nprune)) == c(3,5)))
  })
  
  # ------------------------------------------------------------------
  # preprocess_predictors (NZV + correlation)
  # ------------------------------------------------------------------
  test_that("preprocess_predictors removes NZV and correlated predictors", {
    set.seed(SEED)
    # Construct NZV & correlated features
    y <- rnorm(100)
    x1 <- rnorm(100)
    x2 <- x1 + rnorm(100, sd = 0.01)  # highly correlated with x1
    nzv <- rep(1, 100)                # constant (zero variance)
    dt <- data.table(y = y, x1 = x1, x2 = x2, nzv = nzv)
    
    sp <- split_data(dt, "y", 0.8, seed = SEED)
    pp <- preprocess_predictors(sp$train, sp$test, "y", corr_cut = 0.90, remove_nzv = TRUE)
    removed <- unlist(pp$removed, use.names = FALSE)
    # Expect "nzv" removed and either x1 or x2 dropped for correlation
    expect_true("nzv" %in% removed)
    expect_true(any(c("x1","x2") %in% removed))
  })
  
  # ------------------------------------------------------------------
  # define_train_control
  # ------------------------------------------------------------------
  test_that("define_train_control returns a valid trainControl object", {
    dt <- data.table(y = factor(rep(c("A","B"), each = 30)), x = rnorm(60))
    ctrl <- define_train_control(3, 1, "grid", dt, "y", seed = SEED)
    expect_true(inherits(ctrl, "trainControl"))
    expect_true(ctrl$repeats == 1)
    expect_true(ctrl$number == 3)
  })
  
  # ------------------------------------------------------------------
  # Regression: train_model + evaluate_model
  # ------------------------------------------------------------------
  test_that("train_model and evaluate_model work for regression (earth)", {
    set.seed(SEED)
    dt <- data.table(y = rnorm(120), x1 = rnorm(120), x2 = rnorm(120))
    sp <- split_data(dt, "y", 0.8, seed = SEED)
    
    grid <- .quick_grid()
    ctrl <- .quick_ctrl(sp$train, "y")
    model <- train_model(sp$train, "y", "earth", ctrl, grid, seed = SEED)
    
    expect_s3_class(model, "train")
    ev <- evaluate_model(model, sp$test, "y")
    expect_true(all(c("RMSE","MAE","R2") %in% names(ev$metrics)))
    expect_true(is.numeric(ev$metrics$RMSE))
  })
  
  # ------------------------------------------------------------------
  # Binary Classification: train_model + evaluate_model
  # ------------------------------------------------------------------
  test_that("train_model and evaluate_model work for binary classification (earth)", {
    set.seed(SEED)
    # make a separable-ish dataset
    x1 <- rnorm(150)
    x2 <- rnorm(150)
    lin <- 1.5*x1 - 1.0*x2 + rnorm(150, sd = 0.5)
    y <- factor(ifelse(lin > 0, "neg", "pos"))  # two classes
    dt <- data.table(y = y, x1 = x1, x2 = x2)
    
    sp <- split_data(dt, "y", 0.8, seed = SEED)
    sp$train <- balance_classes(sp$train, "y")
    
    grid <- .quick_grid()
    ctrl <- .quick_ctrl(sp$train, "y")
    model <- train_model(sp$train, "y", "earth", ctrl, grid, seed = SEED)
    
    ev <- evaluate_model(model, sp$test, "y")
    expect_true(all(c("Accuracy","Kappa") %in% names(ev$metrics)))
    expect_true(is.matrix(ev$confusion_matrix) || is.table(ev$confusion_matrix))
  })
  
  # ------------------------------------------------------------------
  # fs_mars integration (regression)
  # ------------------------------------------------------------------
  test_that("fs_mars runs end-to-end for regression and returns expected fields", {
    set.seed(SEED)
    dt <- data.table(y = rnorm(140), x1 = rnorm(140), x2 = rnorm(140))
    res <- fs_mars(
      data = dt, responseName = "y",
      degree = 1:2, nprune = c(3,5),
      number = 3, repeats = 1,
      seed = SEED, sampleSize = 2000,
      verbose = FALSE, corr_cut = 0.9
    )
    expect_true(!is.null(res$model))
    expect_true(all(c("RMSE","MAE","R2") %in% names(res$metrics)))
    expect_true("removed_predictors" %in% names(res$preprocessing))
  })
  
  # ------------------------------------------------------------------
  # fs_mars integration (binary classification)
  # ------------------------------------------------------------------
  test_that("fs_mars runs end-to-end for binary classification", {
    set.seed(SEED)
    x1 <- rnorm(160)
    x2 <- rnorm(160)
    lin <- 1.2*x1 - 0.8*x2 + rnorm(160, sd = 0.6)
    y <- factor(ifelse(lin > 0, "no", "yes"))
    dt <- data.table(y = y, x1 = x1, x2 = x2)
    
    res <- fs_mars(
      data = dt, responseName = "y",
      degree = 1:2, nprune = c(3,5),
      number = 3, repeats = 1,
      seed = SEED, sampleSize = 2000,
      verbose = FALSE, corr_cut = 0.9
    )
    expect_true(!is.null(res$model))
    expect_true(all(c("Accuracy","Kappa") %in% names(res$metrics)))
  })
  
  message("All unit tests for fs_mars passed.")
}

# Run tests (uncomment to execute directly)
# test_fs_mars_functions()
