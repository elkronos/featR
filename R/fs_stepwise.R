# Stepwise linear-model selection (MASS::stepAIC) for featR.

#' Fit a linear model inside the persistent fit environment
#'
#' Builds and evaluates `stats::lm(<fml>, data = .fs_stepwise_data)` inside
#' `fit_env`, where `.fs_stepwise_data` has been assigned. Because the formula
#' (and therefore the model's terms) carries `fit_env`, the data stays
#' reachable for `stepAIC()` refits and for later generics that re-evaluate
#' through the terms environment (`add1()`, `drop1()`, `model.frame()`, ...),
#' without ever touching the global environment.
#'
#' @param fml A formula whose environment is `fit_env`.
#' @param fit_env Environment holding `.fs_stepwise_data`.
#' @return A fitted `lm` object.
#' @noRd
step_fit_lm <- function(fml, fit_env) {
  fit_call <- as.call(list(
    quote(stats::lm),
    formula = fml,
    data = as.name(".fs_stepwise_data")
  ))
  eval(fit_call, fit_env)
}

#' Build the start model and scope for a stepwise search
#'
#' Reproduces the classic setup: backward starts from the full model with no
#' scope; forward starts from the intercept-only model with
#' `scope = list(lower = null, upper = full)`; both starts from the full model
#' with the same scope.
#'
#' @param dep_var Dependent variable name (character).
#' @param direction One of "backward", "forward", "both".
#' @param fit_env Environment holding `.fs_stepwise_data`.
#' @return A list with `start_model`, `scope` (list or NULL), and `direction`.
#' @noRd
step_build_models <- function(dep_var, direction, fit_env) {
  fml <- stats::as.formula(paste(backtick(dep_var), "~ ."), env = fit_env)
  full_model <- step_fit_lm(fml, fit_env)

  if (direction == "backward") {
    return(list(start_model = full_model, scope = NULL,
                direction = "backward"))
  }

  null_fml <- stats::as.formula(paste(backtick(dep_var), "~ 1"),
                                env = fit_env)
  null_model <- step_fit_lm(null_fml, fit_env)
  scope <- list(lower = stats::formula(null_model),
                upper = stats::formula(full_model))

  if (direction == "forward") {
    return(list(start_model = null_model, scope = scope,
                direction = "forward"))
  }

  list(start_model = full_model, scope = scope, direction = "both")
}

#' Run MASS::stepAIC inside the fit environment
#'
#' The `stepAIC()` call is evaluated inside `fit_env` so that its internal
#' refits (which use `eval.parent()`) resolve `.fs_stepwise_data` there.
#'
#' @param start_model Starting `lm` model.
#' @param scope Scope list or NULL.
#' @param direction One of "backward", "forward", "both".
#' @param verbose Logical; passed as `trace` to `stepAIC()`.
#' @param dots List of extra arguments for `stepAIC()`; a user-supplied
#'   `trace` is dropped with a warning.
#' @param fit_env Environment holding `.fs_stepwise_data`.
#' @return The model selected by `stepAIC()`.
#' @noRd
step_run_stepwise <- function(start_model, scope, direction, verbose, dots,
                              fit_env) {
  if ("trace" %in% names(dots)) {
    warning("Argument 'trace' supplied via '...' is ignored; use 'verbose' instead.",
            call. = FALSE)
    dots$trace <- NULL
  }

  args <- c(
    list(object = start_model, direction = direction, trace = verbose),
    if (!is.null(scope)) list(scope = scope),
    dots
  )

  assign(".fs_stepwise_args", args, envir = fit_env)
  on.exit(rm(list = ".fs_stepwise_args", envir = fit_env), add = TRUE)

  step_call <- as.call(list(
    quote(do.call),
    quote(MASS::stepAIC),
    as.name(".fs_stepwise_args")
  ))
  eval(step_call, fit_env)
}

#' Coefficient summary of the selected model
#'
#' Returns `summary(model)$coefficients`. Note that estimates and p-values
#' computed on the same data that drove the stepwise selection are subject to
#' selection bias and are not valid for formal inference.
#'
#' @param model A fitted `lm`.
#' @return The coefficient matrix from `summary()`.
#' @noRd
step_coef_summary <- function(model) {
  summary(model)$coefficients
}

#' Stepwise linear-regression feature selection via AIC
#'
#' Uses `MASS::stepAIC()` to perform forward, backward, or both-direction
#' stepwise selection on a linear regression of `dependent_var` against all
#' other columns of `data`. For `step_type = "forward"` and `"both"` a proper
#' null model and scope are set up so that forward moves are possible.
#'
#' @details
#' Requires the suggested package 'MASS'. The function is
#' linear-regression-only: the dependent variable must be numeric.
#'
#' The fitted models reference the data through a small private environment
#' attached to the model formula, so `predict()`, `summary()`, `anova()`,
#' `add1()`/`drop1()`, and similar generics keep working on the returned
#' model; nothing is assigned to the global environment and nothing is
#' written to disk. To refit the returned model with `update()` from another
#' environment, pass the data explicitly, e.g.
#' `update(model, . ~ . - x, data = my_data)` (plain `update(model)`
#' re-evaluates the call in the caller's environment, where the private data
#' object is not visible).
#'
#' Rows containing missing values in any column are dropped (with a warning)
#' before the search, because `stepAIC()` cannot compare models fitted on
#' differing row sets.
#'
#' @param data A data.frame containing the dependent variable and the
#'   candidate predictors (all other columns).
#' @param dependent_var Character string naming the numeric dependent
#'   variable. (Unquoted symbols are not accepted.)
#' @param step_type Direction of the search: `"backward"`, `"forward"`, or
#'   `"both"` (default).
#' @param verbose Logical. If `TRUE`, emits progress messages and enables the
#'   `stepAIC()` trace output. Default `FALSE`.
#' @param ... Additional arguments passed to `MASS::stepAIC()` (e.g. `k`,
#'   `steps`), excluding `trace`, which is controlled by `verbose` (a
#'   user-supplied `trace` is dropped with a warning).
#'
#' @return A list with:
#' \itemize{
#'   \item \code{final_model}: the model selected by \code{stepAIC()}.
#'   \item \code{importance}: the coefficient summary matrix of the final
#'     model. \strong{Caveat}: p-values obtained after stepwise selection on
#'     the same data are optimistically biased (the selective-inference
#'     problem) and must not be used for formal inference.
#'   \item \code{selected_terms}: character vector of selected predictors
#'     (excluding the intercept).
#'   \item \code{call}: a list describing the inputs used.
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   out <- fs_stepwise(mtcars, dependent_var = "mpg", step_type = "both")
#'   out$selected_terms
#'   out$importance
#' }
#' }
#' @export
fs_stepwise <- function(data,
                        dependent_var,
                        step_type = "both",
                        verbose = FALSE,
                        ...) {
  fs_require("MASS", "stepwise selection")

  assert_data_frame(data, "data")
  assert_target(data, dependent_var, arg = "dependent_var")
  assert_string(step_type, "step_type")
  if (!step_type %in% c("backward", "forward", "both")) {
    stop("'step_type' must be one of 'backward', 'forward', or 'both'.",
         call. = FALSE)
  }
  assert_flag(verbose, "verbose")

  dep_var <- dependent_var

  if (!is.numeric(data[[dep_var]])) {
    stop("fs_stepwise() fits linear regressions only; the dependent variable must be numeric.",
         call. = FALSE)
  }
  if (ncol(data) < 2L) {
    stop("'data' must contain at least one predictor besides the dependent variable.",
         call. = FALSE)
  }

  data <- as.data.frame(data)

  # stepAIC() fails mid-search when the number of usable rows changes between
  # candidate models, so drop incomplete rows up front.
  if (anyNA(data)) {
    n_before <- nrow(data)
    data <- stats::na.omit(data)
    warning(sprintf("Dropped %d row(s) with missing values before stepwise selection.",
                    n_before - nrow(data)), call. = FALSE)
    if (nrow(data) == 0L) {
      stop("No rows remain after removing missing values.", call. = FALSE)
    }
  }

  if (verbose) {
    message(sprintf("Starting stepwise selection ('%s') for dependent variable '%s'.",
                    step_type, dep_var))
  }

  # Private environment that carries the data for model fitting; it persists
  # through the returned model's formula/terms environment (never the global
  # environment).
  fit_env <- new.env(parent = parent.frame())
  assign(".fs_stepwise_data", data, envir = fit_env)

  parts <- step_build_models(dep_var, direction = step_type,
                             fit_env = fit_env)

  step_model <- step_run_stepwise(
    start_model = parts$start_model,
    scope = parts$scope,
    direction = parts$direction,
    verbose = verbose,
    dots = list(...),
    fit_env = fit_env
  )

  imp <- step_coef_summary(step_model)
  terms_selected <- attr(stats::terms(step_model), "term.labels")

  if (verbose) {
    message(sprintf(
      "Stepwise selection complete: %d term(s) selected%s.",
      length(terms_selected),
      if (length(terms_selected) > 0L) {
        paste0(" (", paste(terms_selected, collapse = ", "), ")")
      } else {
        ""
      }
    ))
  }

  list(
    final_model = step_model,
    importance = imp,
    selected_terms = terms_selected,
    call = list(
      dependent_var = dep_var,
      step_type = step_type,
      verbose = verbose
    )
  )
}
