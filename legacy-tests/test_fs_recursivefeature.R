###############################################################################
# Testing Infrastructure - Recursive Feature Elimination (fs_recursivefeature)
###############################################################################

# Optional: if fs_recursivefeature utilities are in a separate script/package:
# source("fs_recursivefeature.R")
# library(yourpackage)

# Global container for test results
test_results <- data.frame(
  Test   = character(),
  Result = character(),
  stringsAsFactors = FALSE
)

#' Helper: Print and Store Test Result
#'
#' @param test_name Character. Name of the test.
#' @param passed Logical. Whether the test passed.
#' @param note Optional character. Additional notes.
print_and_store_result <- function(test_name, passed, note = NULL) {
  # Be robust to NA: anything not strictly TRUE is treated as FAIL
  result <- if (isTRUE(passed)) "PASS" else "FAIL"
  cat(sprintf("%-70s [%s]\n", test_name, result))
  if (!is.null(note)) cat("  Note: ", note, "\n")
  test_results <<- rbind(
    test_results,
    data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE)
  )
}

###############################################################################
# Tests: Existence and Core Helpers
###############################################################################

test_core_function_existence <- function() {
  fns <- c(
    ".resolve_response_name",
    ".task_type",
    "validate_rfe_control",
    "validate_train_control",
    "split_data",
    "fit_one_hot_encoder",
    "apply_one_hot_encoder",
    "perform_rfe",
    "train_final_model",
    "fs_recursivefeature"
  )
  for (fn in fns) {
    err <- NULL
    passed <- tryCatch({
      exists(fn, mode = "function")
    }, error = function(e) {
      err <<- e
      FALSE
    })
    note <- if (!is.null(err)) conditionMessage(err) else NULL
    print_and_store_result(
      sprintf("%s: function exists and is callable", fn),
      passed,
      note
    )
  }
}

test_resolve_response_name <- function() {
  df <- data.frame(y = 1:3, x = 4:6)
  
  # Valid numeric index
  err1 <- NULL
  passed1 <- tryCatch({
    nm <- .resolve_response_name(df, 1L)
    identical(nm, "y")
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    ".resolve_response_name: valid numeric index",
    passed1, note1
  )
  
  # Invalid numeric index (out of bounds)
  err2 <- NULL
  passed2 <- tryCatch({
    .resolve_response_name(df, 3L)
    FALSE
  }, error = function(e) {
    err2 <<- e
    TRUE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    ".resolve_response_name: errors on out-of-bounds index",
    passed2, note2
  )
  
  # Non-integer numeric
  err3 <- NULL
  passed3 <- tryCatch({
    .resolve_response_name(df, 1.5)
    FALSE
  }, error = function(e) {
    err3 <<- e
    TRUE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    ".resolve_response_name: errors on non-integer numeric index",
    passed3, note3
  )
  
  # Numeric vector length > 1
  err4 <- NULL
  passed4 <- tryCatch({
    .resolve_response_name(df, c(1L, 2L))
    FALSE
  }, error = function(e) {
    err4 <<- e
    TRUE
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    ".resolve_response_name: errors on numeric vector length > 1",
    passed4, note4
  )
  
  # Valid character name
  err5 <- NULL
  passed5 <- tryCatch({
    nm <- .resolve_response_name(df, "x")
    identical(nm, "x")
  }, error = function(e) {
    err5 <<- e
    FALSE
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    ".resolve_response_name: valid character name",
    passed5, note5
  )
  
  # Missing character name
  err6 <- NULL
  passed6 <- tryCatch({
    .resolve_response_name(df, "z")
    FALSE
  }, error = function(e) {
    err6 <<- e
    TRUE
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    ".resolve_response_name: errors on unknown column name",
    passed6, note6
  )
}

test_task_type_fn <- function() {
  err <- NULL
  passed <- tryCatch({
    c1 <- .task_type(factor(c("A","B")))
    c2 <- .task_type(c("a","b"))
    c3 <- .task_type(c(TRUE, FALSE))
    c4 <- .task_type(rnorm(5))
    identical(c1, "classification") &&
      identical(c2, "classification") &&
      identical(c3, "classification") &&
      identical(c4, "regression")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(".task_type: classification vs regression behavior", passed, note)
}

###############################################################################
# Tests: validate_rfe_control and validate_train_control
###############################################################################

test_validate_rfe_control <- function() {
  # Valid basic input
  err1 <- NULL
  passed1 <- tryCatch({
    validate_rfe_control(list(method = "cv", number = 3))
    TRUE
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result("validate_rfe_control: valid input (cv, number=3)", passed1, note1)
  
  # Not a list
  err2 <- NULL
  passed2 <- tryCatch({
    validate_rfe_control("nope")
    FALSE
  }, error = function(e) {
    err2 <<- e
    TRUE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result("validate_rfe_control: errors when not a list", passed2, note2)
  
  # Missing 'number'
  err3 <- NULL
  passed3 <- tryCatch({
    validate_rfe_control(list(method = "cv"))
    FALSE
  }, error = function(e) {
    err3 <<- e
    TRUE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result("validate_rfe_control: errors when 'number' missing", passed3, note3)
  
  # Invalid 'number'
  err4 <- NULL
  passed4 <- tryCatch({
    validate_rfe_control(list(method = "cv", number = 0))
    FALSE
  }, error = function(e) {
    err4 <<- e
    TRUE
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result("validate_rfe_control: errors when 'number' <= 0", passed4, note4)
  
  # repeatedcv with invalid repeats
  err5 <- NULL
  passed5 <- tryCatch({
    validate_rfe_control(list(method = "repeatedcv", number = 3, repeats = 0))
    FALSE
  }, error = function(e) {
    err5 <<- e
    TRUE
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    "validate_rfe_control: errors when method='repeatedcv' and repeats <= 0",
    passed5, note5
  )
  
  # repeatedcv with valid repeats
  err6 <- NULL
  passed6 <- tryCatch({
    validate_rfe_control(list(method = "repeatedcv", number = 3, repeats = 2))
    TRUE
  }, error = function(e) {
    err6 <<- e
    FALSE
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    "validate_rfe_control: accepts method='repeatedcv' with valid repeats",
    passed6, note6
  )
}

test_validate_train_control <- function() {
  # Valid cv as list
  err1 <- NULL
  passed1 <- tryCatch({
    validate_train_control(list(method = "cv", number = 3))
    TRUE
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result("validate_train_control: valid cv as list", passed1, note1)
  
  # Valid none as list
  err2 <- NULL
  passed2 <- tryCatch({
    validate_train_control(list(method = "none"))
    TRUE
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result("validate_train_control: valid none as list", passed2, note2)
  
  # Valid trainControl object
  err3 <- NULL
  passed3 <- tryCatch({
    ctrl <- caret::trainControl(method = "cv", number = 3)
    validate_train_control(ctrl)
    TRUE
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result(
    "validate_train_control: accepts trainControl object",
    passed3, note3
  )
  
  # Missing method
  err4 <- NULL
  passed4 <- tryCatch({
    validate_train_control(list(number = 3))
    FALSE
  }, error = function(e) {
    err4 <<- e
    TRUE
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result(
    "validate_train_control: errors when 'method' missing",
    passed4, note4
  )
  
  # Invalid number
  err5 <- NULL
  passed5 <- tryCatch({
    validate_train_control(list(method = "cv", number = -2))
    FALSE
  }, error = function(e) {
    err5 <<- e
    TRUE
  })
  note5 <- if (!is.null(err5)) conditionMessage(err5) else NULL
  print_and_store_result(
    "validate_train_control: errors when 'number' <= 0",
    passed5, note5
  )
  
  # repeatedcv with invalid repeats
  err6 <- NULL
  passed6 <- tryCatch({
    validate_train_control(list(method = "repeatedcv", number = 3, repeats = 0))
    FALSE
  }, error = function(e) {
    err6 <<- e
    TRUE
  })
  note6 <- if (!is.null(err6)) conditionMessage(err6) else NULL
  print_and_store_result(
    "validate_train_control: errors when method='repeatedcv' and repeats <= 0",
    passed6, note6
  )
}

###############################################################################
# Tests: split_data
###############################################################################

test_split_data_fn <- function() {
  set.seed(123)
  df <- data.frame(y = rnorm(120), x1 = rnorm(120))
  
  # Basic 80/20 split
  err1 <- NULL
  sp <- NULL
  passed1 <- tryCatch({
    # Use local assignment
    sp <- split_data(df, response_var = "y", seed = 42, p = 0.8)
    nrow(sp$train) == 96L && nrow(sp$test) == 24L
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result("split_data: 80/20 split sizes", passed1, note1)
  
  # Response name preservation
  passed2 <- !is.null(sp) && identical(sp$response_name, "y")
  print_and_store_result("split_data: response_name returned correctly", passed2)
  
  # TrainIndex/TestIndex alignment with partitions
  err3 <- NULL
  passed3 <- tryCatch({
    identical(df[sp$TrainIndex, , drop = FALSE], sp$train) &&
      identical(df[sp$TestIndex,  , drop = FALSE], sp$test)
  }, error = function(e) {
    err3 <<- e
    FALSE
  })
  note3 <- if (!is.null(err3)) conditionMessage(err3) else NULL
  print_and_store_result("split_data: TrainIndex/TestIndex match partitions", passed3, note3)
  
  # Invalid p throws error
  err4 <- NULL
  passed4 <- tryCatch({
    split_data(df, response_var = "y", p = 1)
    FALSE
  }, error = function(e) {
    err4 <<- e
    TRUE
  })
  note4 <- if (!is.null(err4)) conditionMessage(err4) else NULL
  print_and_store_result("split_data: errors when p is not in (0,1)", passed4, note4)
}

###############################################################################
# Tests: fit_one_hot_encoder / apply_one_hot_encoder
###############################################################################

test_one_hot_encoding <- function() {
  tr <- data.frame(
    y = 1:5,
    a = factor(c("A","B","A","C","B")),
    b = c(0,1,0,1,0)
  )
  
  err <- NULL
  passed <- tryCatch({
    dv  <- fit_one_hot_encoder(tr, response_name = "y")
    enc <- apply_one_hot_encoder(tr, response_name = "y", dv = dv)
    
    response_first <- identical(colnames(enc)[1], "y")
    all_numeric    <- all(vapply(enc[-1], is.numeric, logical(1)))
    extra_cols     <- ncol(enc) > ncol(tr)
    
    response_first && all_numeric && extra_cols
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "one-hot: response first, predictors numeric, additional columns present",
    passed, note
  )
}

###############################################################################
# Tests: perform_rfe
###############################################################################

test_perform_rfe_fn <- function() {
  set.seed(1)
  tr <- data.frame(
    y  = rnorm(60),
    x1 = rnorm(60),
    x2 = rnorm(60),
    x3 = rnorm(60)
  )
  tr <- tr[, c("y","x1","x2","x3")]
  
  err <- NULL
  passed <- tryCatch({
    rfe_obj <- perform_rfe(
      train_df = tr,
      response_name = "y",
      sizes = c(1,2,3),
      rfe_control_params = list(method = "cv", number = 3),
      feature_funcs = caret::lmFuncs,
      parallel = FALSE,
      early_stop = FALSE
    )
    inherits(rfe_obj, "rfe") && length(rfe_obj$optVariables) >= 1L
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result("perform_rfe: basic regression RFE", passed, note)
}

###############################################################################
# Tests: train_final_model
###############################################################################

test_train_final_model_linear <- function() {
  set.seed(2)
  df <- data.frame(y = rnorm(80), a = rnorm(80), b = rnorm(80))
  
  err <- NULL
  passed <- tryCatch({
    mdl <- train_final_model(
      data_df = df,
      response_name = "y",
      optimal_vars = c("a","b"),
      train_control_params = list(method = "cv", number = 3),
      model_method = "lm"
    )
    pu <- attr(mdl, "predictors_used")
    inherits(mdl, "train") &&
      is.character(pu) &&
      length(pu) >= 1L &&
      all(pu %in% c("a","b"))
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "train_final_model: linear regression + predictors_used attribute",
    passed, note
  )
}

test_train_final_model_trainControl <- function() {
  set.seed(3)
  df <- data.frame(y = rnorm(50), a = rnorm(50), b = rnorm(50))
  
  err <- NULL
  passed <- tryCatch({
    ctrl <- caret::trainControl(method = "cv", number = 3)
    mdl <- train_final_model(
      data_df = df,
      response_name = "y",
      optimal_vars = c("a","b"),
      train_control_params = ctrl,
      model_method = "lm"
    )
    inherits(mdl, "train")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "train_final_model: accepts trainControl object",
    passed, note
  )
}

###############################################################################
# Tests: fs_recursivefeature Integration (Regression & Encoding)
###############################################################################

test_fs_recursivefeature_regression <- function() {
  set.seed(42)
  n <- 120
  dat <- data.frame(
    y   = rnorm(n),
    x1  = rnorm(n),
    x2  = rnorm(n),
    cat = factor(sample(LETTERS[1:3], n, TRUE))
  )
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_recursivefeature(
      data = dat,
      response_var = "y",
      seed = 42,
      rfe_control = list(method = "cv", number = 3),
      train_control = list(method = "cv", number = 3),
      sizes = c(1,2,3,4),
      parallel = FALSE,
      feature_funcs = caret::lmFuncs,
      handle_categorical = TRUE,
      return_final_model = TRUE,
      model_method = "lm"
    )
    
    # Basic structure
    ok_list    <- is.list(res)
    ok_rfe     <- !is.null(res$RFE) && inherits(res$RFE, "rfe")
    ok_optvars <- is.character(res$OptimalVariables) && length(res$OptimalVariables) >= 1L
    ok_model   <- !is.null(res$FinalModel) && inherits(res$FinalModel, "train")
    ok_task    <- identical(res$TaskType, "regression")
    
    # Indices
    idx_all <- sort(c(res$TrainIndex, res$TestIndex))
    ok_idx_len   <- length(idx_all) == nrow(dat)
    ok_idx_cover <- identical(idx_all, seq_len(nrow(dat)))
    ok_idx_disj  <- length(intersect(res$TrainIndex, res$TestIndex)) == 0L
    
    # Final model variables coherence
    fm_vars <- res$FinalModelVariables
    pu      <- attr(res$FinalModel, "predictors_used")
    ok_fm   <- is.character(fm_vars) &&
      length(fm_vars) >= 1L &&
      all(fm_vars %in% res$OptimalVariables) &&
      identical(sort(fm_vars), sort(pu))
    
    ok_list && ok_rfe && ok_optvars && ok_model && ok_task &&
      ok_idx_len && ok_idx_cover && ok_idx_disj && ok_fm
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_recursivefeature: end-to-end regression + encoding + final model",
    passed, note
  )
}

###############################################################################
# Tests: Classification Path
###############################################################################

test_classification_path_fn <- function() {
  set.seed(101)
  n <- 100
  df <- data.frame(
    y  = factor(sample(c("A","B"), n, TRUE)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  
  # split_data preserves factor type
  err1 <- NULL
  sp <- NULL
  passed1 <- tryCatch({
    # Use local assignment
    sp <- split_data(df, response_var = "y", seed = 101)
    is.factor(sp$train$y)
  }, error = function(e) {
    err1 <<- e
    FALSE
  })
  note1 <- if (!is.null(err1)) conditionMessage(err1) else NULL
  print_and_store_result(
    "classification: split_data preserves factor response",
    passed1, note1
  )
  
  # RFE with rfFuncs
  err2 <- NULL
  passed2 <- tryCatch({
    rfe_obj <- perform_rfe(
      train_df = sp$train,
      response_name = "y",
      sizes = c(1,2),
      rfe_control_params = list(method = "cv", number = 3),
      feature_funcs = caret::rfFuncs,
      parallel = FALSE,
      early_stop = FALSE
    )
    inherits(rfe_obj, "rfe") && length(rfe_obj$optVariables) >= 1L
  }, error = function(e) {
    err2 <<- e
    FALSE
  })
  note2 <- if (!is.null(err2)) conditionMessage(err2) else NULL
  print_and_store_result(
    "classification: perform_rfe with rfFuncs runs",
    passed2, note2
  )
}

###############################################################################
# Tests: Parallel Path (Smoke Test)
###############################################################################

test_fs_recursivefeature_parallel_smoke <- function() {
  set.seed(55)
  n <- 80
  dat <- data.frame(
    y  = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  
  err <- NULL
  passed <- tryCatch({
    res <- fs_recursivefeature(
      data = dat,
      response_var = "y",
      seed = 55,
      rfe_control = list(method = "cv", number = 3),
      train_control = list(method = "cv", number = 3),
      sizes = c(1,2),
      parallel = TRUE,
      feature_funcs = caret::lmFuncs,
      handle_categorical = FALSE,
      return_final_model = TRUE,
      model_method = "lm"
    )
    inherits(res$RFE, "rfe") &&
      !is.null(res$FinalModel) &&
      inherits(res$FinalModel, "train")
  }, error = function(e) {
    err <<- e
    FALSE
  })
  note <- if (!is.null(err)) conditionMessage(err) else NULL
  print_and_store_result(
    "fs_recursivefeature: parallel (doParallel) path smoke test",
    passed, note
  )
}

###############################################################################
# Run All fs_recursivefeature Tests
###############################################################################

run_fs_recursivefeature_tests <- function() {
  cat("========== Running fs_recursivefeature Tests ==========\n")
  
  # Reset results each run
  test_results <<- data.frame(
    Test   = character(),
    Result = character(),
    stringsAsFactors = FALSE
  )
  
  test_core_function_existence()
  test_resolve_response_name()
  test_task_type_fn()
  test_validate_rfe_control()
  test_validate_train_control()
  test_split_data_fn()
  test_one_hot_encoding()
  test_perform_rfe_fn()
  test_train_final_model_linear()
  test_train_final_model_trainControl()
  test_fs_recursivefeature_regression()
  test_classification_path_fn()
  test_fs_recursivefeature_parallel_smoke()
  
  cat("========== fs_recursivefeature Tests Completed ==========\n\n")
  cat("Test Summary:\n")
  print(table(test_results$Result))
  cat("\nDetailed Results:\n")
  print(test_results)
}

# Uncomment to run directly:
# run_fs_recursivefeature_tests()
