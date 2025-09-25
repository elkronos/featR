# ============================================
# Stepwise Feature Selection Utilities (R)
# ============================================
# - Modular, maintainable, and correct
# - Uses MASS::stepAIC with proper handling of forward/backward/both
# - Robust logging to both console and file
# - Roxygen docs included
# ============================================

# ---- Setup: Load/Install Required Libraries ----
.ensure_packages <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p)
    }
  }
}
.ensure_packages(c("logger", "MASS"))

library(logger)
library(MASS)

# ---- Logger Initialization ----

#' Initialize logging (console + file) with a colored console layout
#'
#' @param logfile Path to the logfile (default: "script_log.log").
#' @param threshold Logging threshold; one of \code{TRACE, DEBUG, INFO, WARN, ERROR, FATAL}.
#' @return Invisibly returns \code{TRUE} on success.
#' @examples
#' init_logging()  # default logfile and INFO threshold
init_logging <- function(logfile = "script_log.log", threshold = INFO) {
  # Log to both console and file
  log_appender(appender_tee(logfile))
  log_layout(layout_glue_colors)
  log_threshold(threshold)
  log_info("===== Logger initialized =====")
  invisible(TRUE)
}

# Default init (you can comment this out in package context)
init_logging()

# ---- Environment & Session Info Logging (Optional) ----

#' Log basic R environment details
#'
#' @return Invisibly returns \code{TRUE}.
#' @examples
#' log_environment_details()
log_environment_details <- function() {
  log_info("===== R Environment Details =====")
  log_info("R Version: {R.version.string}")
  os <- tryCatch(Sys.info(), error = function(e) list(sysname = NA, release = NA))
  log_info("Operating System: {os[['sysname']]} {os[['release']]}")
  installed_pkgs <- as.data.frame(installed.packages()[, c("Package", "Version")])
  pkg_lines <- paste(apply(installed_pkgs, 1, paste, collapse = " "), collapse = "\n")
  log_debug("Installed Packages:\n{pkg_lines}")
  log_info("=================================")
  invisible(TRUE)
}

# ---- Utility: Safe Error Logging with Traceback ----

#' Log an error with a captured traceback
#'
#' @param e The condition (error) object from \code{tryCatch}.
#' @return Invisibly returns \code{TRUE}.
log_error_with_trace <- function(e) {
  log_error("Error: {e$message}")
  tb <- NULL
  # Attempt to capture the most recent traceback
  tb <- tryCatch({
    # generate a traceback
    utils::capture.output(traceback(x = sys.calls(), max.lines = 50))
  }, error = function(...) character(0))
  if (length(tb) > 0) {
    log_error("Stack Trace:\n{paste(tb, collapse = '\n')}")
  }
  invisible(TRUE)
}

# ---- Results Table Helper (Optional) ----

#' Create (if missing) and append a test result row to a global results data frame
#'
#' @param test_name Name of the test or check.
#' @param passed Logical; whether the test passed.
#' @param message Optional message to print/log.
#' @return Invisibly returns the updated \code{test_results} data frame.
#' @examples
#' test_results_init()
#' print_and_store_result("Example test", TRUE, "All good")
test_results_init <- function() {
  if (!exists("test_results", inherits = .GlobalEnv)) {
    assign("test_results",
           data.frame(Test = character(), Result = character(), stringsAsFactors = FALSE),
           envir = .GlobalEnv)
  }
  invisible(get("test_results", envir = .GlobalEnv))
}

#' @rdname test_results_init
print_and_store_result <- function(test_name, passed, message = NULL) {
  test_results_init()
  result <- if (isTRUE(passed)) "PASS" else "FAIL"
  cat(sprintf("%-50s [%s]\n", test_name, result))
  log_info("{test_name} [{result}]")
  if (!is.null(message)) {
    cat("  ", message, "\n")
    log_debug("Additional Message for {test_name}: {message}")
  }
  tr <- get("test_results", envir = .GlobalEnv)
  tr <- rbind(tr, data.frame(Test = test_name, Result = result, stringsAsFactors = FALSE))
  assign("test_results", tr, envir = .GlobalEnv)
  invisible(tr)
}

# ---- Input Checking & Formula Prep ----

#' Validate inputs for \code{fs_stepwise}
#'
#' @param data A data.frame.
#' @param dependent_var Character name of the dependent variable, or unquoted symbol.
#' @param step_type One of \code{"backward"}, \code{"forward"}, or \code{"both"}.
#' @return The dependent variable name as a character string.
check_inputs <- function(data, dependent_var, step_type) {
  log_info("Validating inputs: dependent_var = {deparse(substitute(dependent_var))}, step_type = {step_type}")
  
  if (!is.data.frame(data)) {
    log_error("The 'data' input must be a data frame.")
    stop("Input 'data' must be a data frame")
  }
  
  dep_var <- if (is.character(dependent_var)) {
    dependent_var
  } else {
    deparse(substitute(dependent_var))
  }
  
  if (!(dep_var %in% colnames(data))) {
    log_error("Dependent variable '{dep_var}' not found in data columns.")
    stop("Dependent variable not found in data")
  }
  
  if (!step_type %in% c("backward", "forward", "both")) {
    log_error("Invalid 'step_type' ({step_type}). Must be one of 'backward', 'forward', or 'both'.")
    stop("Invalid 'step_type'")
  }
  
  log_info("Input validation successful; using dependent variable: {dep_var}")
  dep_var
}

#' Prepare a modeling formula of the form dep_var ~ all_other_columns
#'
#' @param data A data.frame.
#' @param dep_var Character name of the dependent variable.
#' @return An object of class \code{formula}.
#' @examples
#' prepare_formula(mtcars, "mpg")
prepare_formula <- function(data, dep_var) {
  log_info("Preparing model formula with dependent variable: {dep_var}")
  rhs <- setdiff(colnames(data), dep_var)
  if (length(rhs) == 0) stop("No predictors found in 'data' besides the dependent variable")
  formula <- reformulate(termlabels = rhs, response = dep_var)
  log_debug("Constructed formula: {format(formula)}")
  formula
}

# ---- Model Builders for Step Directions ----

#' Build initial models and scope objects for stepwise selection
#'
#' @param formula A model formula.
#' @param data A data.frame.
#' @param direction One of \code{"backward"}, \code{"forward"}, or \code{"both"}.
#' @return A list with \code{start_model} (lm), \code{scope} (list or NULL), and \code{direction}.
build_models_for_direction <- function(formula, data, direction) {
  full_model <- lm(formula, data = data)
  
  if (direction == "backward") {
    return(list(start_model = full_model, scope = NULL, direction = "backward"))
  }
  
  dep_var <- all.vars(formula)[1]
  null_formula <- as.formula(paste(dep_var, "~ 1"))
  null_model <- lm(null_formula, data = data)
  scope <- list(lower = formula(null_model), upper = formula(full_model))
  
  if (direction == "forward") {
    return(list(start_model = null_model, scope = scope, direction = "forward"))
  }
  
  list(start_model = full_model, scope = scope, direction = "both")
}

# ---- Training via stepAIC ----

#' Train a model via stepwise selection using \code{MASS::stepAIC}
#'
#' @param formula A model formula.
#' @param data A data.frame.
#' @param direction One of \code{"backward"}, \code{"forward"}, or \code{"both"}.
#' @param verbose Logical; whether to print stepAIC tracing output.
#' @param ... Additional arguments passed to \code{MASS::stepAIC}.
#' @return The final model object returned by \code{stepAIC}.
#' @examples
#' final <- train_stepwise_model(mpg ~ ., mtcars, direction = "both")
train_stepwise_model <- function(formula, data, direction = "both", verbose = FALSE, ...) {
  log_info("Training stepwise model with direction = {direction}")
  tryCatch({
    parts <- build_models_for_direction(formula, data, direction)
    start_model <- parts$start_model
    scope <- parts$scope
    dir <- parts$direction
    
    # Create a unique, hidden global binding for the data so update() can always see it
    df_token <- paste0(".fs_data_", sprintf("%08d", sample.int(1e8, 1)))
    assign(df_token, data, envir = .GlobalEnv)
    on.exit({
      # best effort cleanup
      if (exists(df_token, envir = .GlobalEnv, inherits = FALSE)) {
        rm(list = df_token, envir = .GlobalEnv)
      }
    }, add = TRUE)
    
    # Rewrite the model call to use the global data symbol
    start_model$call$data <- as.name(df_token)
    
    # Also harden the terms/formula environments to the global env (defensive)
    if (!is.null(start_model$terms)) {
      environment(start_model$terms) <- .GlobalEnv
    }
    if (!is.null(attr(start_model$terms, "formula"))) {
      environment(attr(start_model$terms, "formula")) <- .GlobalEnv
    }
    
    log_info("Starting stepwise selection using stepAIC.")
    step_model <- if (is.null(scope)) {
      MASS::stepAIC(start_model, direction = dir, trace = verbose, ...)
    } else {
      MASS::stepAIC(start_model, scope = scope, direction = dir, trace = verbose, ...)
    }
    
    log_info("Stepwise model training completed successfully.")
    step_model
  }, error = function(e) {
    log_error("Error during stepwise model training: {e$message}")
    log_error_with_trace(e)
    stop("Model training failed.")
  })
}

# ---- Variable Importance Helper ----

#' Compute a simple variable importance table from a fitted model
#'
#' Currently returns the coefficient summary table from \code{summary(model)$coefficients}.
#' You may customize this to use standardized coefficients or other metrics.
#'
#' @param model A fitted \code{lm} or compatible model.
#' @return A matrix (like \code{summary(model)$coefficients}) with rows per term.
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' importance <- variable_importance(model)
variable_importance <- function(model) {
  sm <- summary(model)
  sm$coefficients
}

# ---- Public API: fs_stepwise ----

#' Perform stepwise feature selection using linear regression
#'
#' This function uses \code{MASS::stepAIC} to perform forward, backward, or both-direction
#' stepwise selection. For \code{direction = "forward"} and \code{"both"}, it sets up a
#' proper null model and \code{scope} to ensure forward moves are possible.
#'
#' @param data A data.frame containing the dataset.
#' @param dependent_var The name (as a character string or unquoted symbol) of the dependent variable.
#' @param step_type The direction for stepwise selection: \code{"backward"}, \code{"forward"}, or \code{"both"}. Default is \code{"both"}.
#' @param seed An optional seed for reproducibility.
#' @param verbose Logical. If \code{TRUE}, prints detailed \code{stepAIC} output. Default is \code{FALSE}.
#' @param return_models Logical. If \code{TRUE}, includes the final stepwise model in the returned list.
#' @param ... Additional parameters to pass to \code{MASS::stepAIC} (e.g., \code{k} for AIC penalty).
#'
#' @return A list with:
#' \itemize{
#'   \item \code{final_model}: the fitted model returned by \code{stepAIC}.
#'   \item \code{importance}: a coefficient summary matrix for the final model.
#'   \item \code{selected_terms}: character vector of selected predictors (excluding intercept).
#'   \item \code{call}: a list describing inputs used.
#'   \item \code{step_model}: (optional) same as \code{final_model} if \code{return_models = TRUE}.
#' }
#'
#' @examples
#' # Basic usage with mtcars
#' out <- fs_stepwise(mtcars, dependent_var = "mpg", step_type = "both", seed = 123)
#' out$final_model
#' out$importance
#' out$selected_terms
fs_stepwise <- function(data,
                        dependent_var,
                        step_type = "both",
                        seed = NULL,
                        verbose = FALSE,
                        return_models = FALSE,
                        ...) {
  log_info("Starting fs_stepwise with dependent_var = {deparse(substitute(dependent_var))}, step_type = {step_type}")
  
  if (!is.null(seed)) {
    set.seed(seed)
    log_info("Random seed set to: {seed}")
  }
  
  dep_var <- check_inputs(data, dependent_var, step_type)
  formula <- prepare_formula(data, dep_var)
  
  step_model <- train_stepwise_model(formula, data, direction = step_type, verbose = verbose, ...)
  
  imp <- variable_importance(step_model)
  terms_selected <- attr(terms(step_model), "term.labels")
  
  out <- list(
    final_model    = step_model,
    importance     = imp,
    selected_terms = terms_selected,
    call           = list(
      dependent_var = dep_var,
      step_type     = step_type,
      seed          = seed,
      verbose       = verbose
    )
  )
  if (isTRUE(return_models)) {
    out$step_model <- step_model
    log_info("Returning full model details as requested.")
  }
  
  if (isTRUE(verbose)) {
    cat("===== Final Model Summary =====\n")
    print(summary(step_model))
    cat("\n===== Variable Importance =====\n")
    print(imp)
    cat("\n===== Selected Terms =====\n")
    print(terms_selected)
  }
  
  log_info("fs_stepwise completed successfully.")
  out
}

# ---- (Optional) Quick Self-Test ----
# Uncomment to run a quick check
# test_results_init()
# res <- try(fs_stepwise(mtcars, "mpg", step_type = "both", seed = 42, verbose = TRUE), silent = TRUE)
# print_and_store_result("fs_stepwise runs on mtcars", !inherits(res, "try-error"))
