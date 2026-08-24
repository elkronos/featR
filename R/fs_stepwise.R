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

#' Absolute t statistics of the retained coefficients
#'
#' The intercept is dropped: it is never a selected feature. The result is the
#' per-feature score reported by `fs_stepwise()`; see the caveat about
#' post-selection inference in the function documentation.
#'
#' @param coefs A coefficient matrix from `summary.lm()`.
#' @return Named numeric vector of `|t|`, empty when no non-intercept
#'   coefficient survived.
#' @noRd
step_abs_t <- function(coefs) {
  empty <- stats::setNames(numeric(0), character(0))
  if (!is.matrix(coefs) || nrow(coefs) == 0L ||
      !"t value" %in% colnames(coefs)) {
    return(empty)
  }
  keep <- rownames(coefs) != "(Intercept)"
  if (!any(keep)) {
    return(empty)
  }
  out <- abs(as.numeric(coefs[keep, "t value"]))
  names(out) <- rownames(coefs)[keep]
  out
}

#' Stepwise linear-regression feature selection via AIC
#'
#' Uses `MASS::stepAIC()` to perform forward, backward, or both-direction
#' stepwise selection on a linear regression of `target` against all other
#' columns of `data`. For `direction = "forward"` and `"both"` a proper null
#' model and scope are set up so that forward moves are possible.
#'
#' @details
#' Requires the suggested package 'MASS'. The function is
#' linear-regression-only: the target must be numeric.
#'
#' \strong{Post-selection inference caveat.} The reported `scores` (absolute
#' t statistics) and the p-values in `details$coefficients` are computed on
#' the same data that drove the search. They are optimistically biased -- the
#' selective-inference problem -- and must not be used for formal inference,
#' only as a rough ordering of the retained terms.
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
#' @param data A data.frame containing the target and the candidate
#'   predictors (all other columns).
#' @param target Character string naming the numeric target column.
#'   (Unquoted symbols are not accepted.)
#' @param direction Direction of the search: `"both"` (default),
#'   `"backward"`, or `"forward"`.
#' @param verbose Logical. If `TRUE`, emits progress messages and enables the
#'   `stepAIC()` trace output. Default `FALSE`.
#' @param ... Additional arguments passed to `MASS::stepAIC()` (e.g. `k`,
#'   `steps`), excluding `trace`, which is controlled by `verbose` (a
#'   user-supplied `trace` is dropped with a warning).
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector of the selected predictor terms
#'     (excluding the intercept).}
#'   \item{scores}{Named numeric vector of absolute t statistics from the
#'     final model's coefficient table, excluding the intercept.
#'     \strong{Caveat}: these statistics (and the p-values in
#'     `details$coefficients`) are computed after selection on the same data,
#'     so they are optimistically biased and are not valid for inference.}
#'   \item{method}{"stepwise".}
#'   \item{task}{"regression"; `fs_stepwise()` fits linear models only.}
#'   \item{model}{The `lm` selected by `stepAIC()`.}
#'   \item{details}{A list with `final_model` (the same `lm`), `coefficients`
#'     (the `summary()` coefficient matrix, same caveat as `scores`),
#'     `selected_terms` (the selected term labels), `direction` (the search
#'     direction actually used), `n_features` (the number of candidate
#'     predictors offered to the search) and `dropped_na_rows` (how many rows
#'     were removed for missing values).}
#'   \item{call}{The matched call.}
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   res <- fs_stepwise(mtcars, target = "mpg", direction = "both")
#'   res$selected
#'   res$scores
#' }
#' }
#' @export
fs_stepwise <- function(data,
                        target,
                        direction = c("both", "backward", "forward"),
                        verbose = FALSE,
                        ...) {
  cl_call <- match.call()

  fs_require("MASS", "stepwise selection")

  direction <- match.arg(direction)
  assert_data_frame(data, "data")
  assert_target(data, target, arg = "target")
  assert_flag(verbose, "verbose")

  dep_var <- target

  if (!is.numeric(data[[dep_var]])) {
    stop("fs_stepwise() fits linear regressions only; the target must be numeric.",
         call. = FALSE)
  }
  if (ncol(data) < 2L) {
    stop("'data' must contain at least one predictor besides the target.",
         call. = FALSE)
  }

  data <- as.data.frame(data)
  n_candidates <- length(setdiff(names(data), dep_var))

  # stepAIC() fails mid-search when the number of usable rows changes between
  # candidate models, so drop incomplete rows up front.
  dropped_na_rows <- 0L
  if (anyNA(data)) {
    n_before <- nrow(data)
    data <- stats::na.omit(data)
    dropped_na_rows <- n_before - nrow(data)
    warning(sprintf("Dropped %d row(s) with missing values before stepwise selection.",
                    dropped_na_rows), call. = FALSE)
    if (nrow(data) == 0L) {
      stop("No rows remain after removing missing values.", call. = FALSE)
    }
  }

  if (verbose) {
    message(sprintf("Starting stepwise selection ('%s') for target '%s'.",
                    direction, dep_var))
  }

  # Private environment that carries the data for model fitting; it persists
  # through the returned model's formula/terms environment (never the global
  # environment).
  fit_env <- new.env(parent = parent.frame())
  assign(".fs_stepwise_data", data, envir = fit_env)

  parts <- step_build_models(dep_var, direction = direction,
                             fit_env = fit_env)

  step_model <- step_run_stepwise(
    start_model = parts$start_model,
    scope = parts$scope,
    direction = parts$direction,
    verbose = verbose,
    dots = list(...),
    fit_env = fit_env
  )

  coefficients <- step_coef_summary(step_model)
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

  new_fs_result(
    selected = terms_selected,
    scores   = step_abs_t(coefficients),
    method   = "stepwise",
    task     = "regression",
    model    = step_model,
    details  = list(
      final_model     = step_model,
      coefficients    = coefficients,
      selected_terms  = terms_selected,
      direction       = direction,
      n_features      = n_candidates,
      dropped_na_rows = dropped_na_rows
    ),
    call = cl_call
  )
}
