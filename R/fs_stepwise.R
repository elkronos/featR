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
#' @param check_saturated Logical; when TRUE, a full model with no residual
#'   degrees of freedom is an error (its AIC is -Inf, so the search cannot
#'   proceed or is meaningless).
#' @return A list with `start_model`, `scope` (list or NULL), and `direction`.
#' @noRd
step_build_models <- function(dep_var, direction, fit_env,
                              check_saturated = TRUE) {
  fml <- stats::as.formula(paste(backtick(dep_var), "~ ."), env = fit_env)
  full_model <- step_fit_lm(fml, fit_env)

  # Design-matrix names must be unique: a factor dummy (g, level b -> "gb")
  # that collides with another column (or the target) makes the coefficient
  # names in `scores` and `details$coefficients` ambiguous.
  mm_names <- unbacktick_names(colnames(stats::model.matrix(full_model)))
  assert_unique_encoded_names(mm_names[mm_names != "(Intercept)"], dep_var,
                              "fs_stepwise")

  if (check_saturated && stats::df.residual(full_model) <= 0L) {
    stop(sprintf(paste0(
      "fs_stepwise(): the full model has no residual degrees of freedom ",
      "(%d complete row(s) for %d model coefficient(s), intercept and factor ",
      "dummies included), so it fits the data perfectly and its AIC is -Inf; ",
      "stepwise selection is meaningless. Use more rows or fewer predictors."),
      nrow(stats::model.frame(full_model)), length(stats::coef(full_model))),
      call. = FALSE)
  }

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

  # A user-supplied scope replaces the default one instead of colliding with
  # it ("formal argument 'scope' matched by multiple actual arguments").
  if ("scope" %in% names(dots)) {
    scope <- NULL
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
#' Returns `summary(model)$coefficients`. Estimates and p-values
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
#' @return Named numeric vector of `|t|`, empty when the coefficient table is
#'   unusable (not a matrix, no rows, or no "t value" column) or when no
#'   non-intercept coefficient survived.
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
  names(out) <- unbacktick_names(rownames(coefs)[keep])
  out
}

#' Per-term scores of the selected model
#'
#' Maps each coefficient's `|t|` to the model term it belongs to (via the
#' design matrix's `assign` attribute) and scores each term by the largest
#' `|t|` among its coefficients, so `scores` is keyed by the same term labels
#' as `selected`. A term whose coefficients are all aliased (`NA`) scores
#' `NA`.
#'
#' @param model The selected `lm`.
#' @param coefs Its `summary()` coefficient matrix.
#' @param term_labels Unbackticked term labels of `model`.
#' @return Named numeric vector, one entry per term label.
#' @noRd
step_term_scores <- function(model, coefs, term_labels) {
  if (length(term_labels) == 0L) {
    return(stats::setNames(numeric(0), character(0)))
  }
  abs_t <- step_abs_t(coefs)
  mm <- stats::model.matrix(model)
  asg <- attr(mm, "assign")
  coef_term <- stats::setNames(asg, unbacktick_names(colnames(mm)))
  term_of <- coef_term[names(abs_t)]
  out <- vapply(seq_along(term_labels), function(k) {
    v <- abs_t[!is.na(term_of) & term_of == k]
    if (length(v) == 0L || all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
  }, numeric(1L))
  stats::setNames(out, term_labels)
}

#' Stepwise linear-regression feature selection by AIC
#'
#' Finds the subset of columns that AIC keeps in a linear model. Uses
#' `MASS::stepAIC()` to add or drop one term at a time from a linear
#' regression of `target` on all other columns of `data`, stopping when no
#' single move lowers AIC. Requires the suggested package 'MASS'.
#'
#' @details
#' This is the cheapest wrapper method in featR and the easiest to read: the
#' result is an ordinary `lm` that the usual generics work on. The search is
#' greedy, so it can walk past the AIC-best subset, and unstable, so small
#' changes in the data can change the retained set.
#'
#' The reported `scores` (absolute t statistics) and the p-values in
#' `details$coefficients` are computed on the same data that drove the search.
#' They are optimistically biased (the selective-inference problem) and are
#' not valid for formal inference; use them only as a rough ordering of the
#' retained terms.
#'
#' Only linear regression is supported: the target must be numeric, and every
#' other column of `data` is a candidate predictor. `direction = "backward"`
#' starts from the full model; `"forward"` starts from the intercept-only
#' model with the full model as the upper scope; `"both"` starts from the full
#' model with the same scope, so terms can be added back. There is no `seed`
#' argument: `stepAIC()` is deterministic, so the same call on the same data
#' returns the same model, and the RNG is never touched.
#'
#' Rows with a missing value in any column are dropped, with a warning,
#' before the search, because `stepAIC()` cannot compare models fitted on
#' different rows. Infinite values are an error.
#'
#' Backward and both-direction searches start from the full model, which
#' must leave residual degrees of freedom. With as many coefficients
#' (intercept and factor dummies included) as complete rows, the full model
#' fits perfectly, its AIC is -Inf, and the call stops with an error giving
#' the counts. A forward search may have more candidate predictors than rows;
#' it warns if the model it ends on has no residual degrees of freedom, since
#' that perfect fit makes the selection meaningless. A factor or character
#' predictor with fewer than two observed levels (after incomplete rows are
#' dropped) is an error, as is a factor dummy name that collides with another
#' column or the target (factor `g` with level `b` next to a column `gb`).
#'
#' The models reach their data through a private environment attached to the
#' model formula, so `predict()`, `summary()`, `anova()`, `add1()`, `drop1()`
#' and similar generics work on the returned model. Nothing is assigned to the
#' global environment or written to disk. That environment holds a copy of
#' the complete-case `data` and is kept alive by the returned model, so the
#' returned object is at least as large as that copy. Plain `update(model)`
#' fails with an object-not-found error, because `update()` re-evaluates the
#' stored call in the caller's environment, where the private data is not
#' visible. Pass the data explicitly instead:
#' `update(model, . ~ . - x, data = my_data)`.
#'
#' Non-syntactic column names are backticked in the formula and reported
#' without the backticks, so `selected` matches `names(data)`.
#'
#' `...` is forwarded to `MASS::stepAIC()` unchanged, so it also swallows
#' arguments this function does not have. The removed arguments `seed` and
#' `return_models` are passed through and ignored rather than rejected: a call
#' written for the older interface still runs, without the effect it asked
#' for. Check your argument names against the list below.
#'
#' @param data A data.frame (or data.table) with at least two columns, the
#'   numeric target and at least one candidate predictor, with unique,
#'   non-empty column names and no infinite values. Every column other than
#'   `target` is offered to the search. For backward and both-direction
#'   searches, the complete rows must outnumber the coefficients of the full
#'   model.
#' @param target Single string naming the numeric target column of `data`.
#'   Unquoted symbols and column indices are not accepted. Non-syntactic names
#'   work, because they are backticked in the formula.
#' @param direction Direction of the search: `"both"` (the default),
#'   `"backward"`, or `"forward"`.
#' @param verbose Logical. If `TRUE`, print progress messages and the
#'   `stepAIC()` trace. Default `FALSE`.
#' @param ... Further arguments passed to `MASS::stepAIC()`, for example `k`
#'   (`k = log(nrow(data))` gives BIC), `steps`, or `scope` (which then
#'   replaces the default scope). `trace` is controlled by `verbose`, and a
#'   `trace` supplied here is dropped with a warning. See Details for what
#'   else `...` absorbs.
#'
#' @return An object of class `fs_result` with:
#' \describe{
#'   \item{selected}{Character vector of the selected predictor terms,
#'     without the intercept.}
#'   \item{scores}{Named numeric vector with one entry per selected term,
#'     keyed like `selected`: the absolute t statistic of the term's
#'     coefficient or, for a term with several coefficients (a factor, an
#'     interaction), the largest `|t|` among them (`NA` when all are
#'     aliased). The per-coefficient values (for example `grpb` and `grpc`
#'     for the term `grp`) are in `details$coefficient_scores`. These
#'     statistics are computed after selection on the same data, so they are
#'     optimistically biased and not valid for inference.}
#'   \item{method}{`"stepwise"`.}
#'   \item{task}{`"regression"`; `fs_stepwise()` fits linear models only.}
#'   \item{model}{The `lm` chosen by `stepAIC()`. It carries a copy of the
#'     complete-case data in a private environment (see Details).}
#'   \item{details}{A list with `final_model` (the same `lm`), `coefficients`
#'     (the `summary()` coefficient matrix, biased like `scores`),
#'     `coefficient_scores` (named `|t|` per non-intercept coefficient),
#'     `selected_terms` (the same labels as `selected`), `direction` (the
#'     search direction used), `n_features` (the number of candidate
#'     predictors, `ncol(data) - 1`), and `dropped_na_rows` (the number of
#'     rows removed for missing values, `0L` when none were).}
#'   \item{call}{The matched call.}
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   res <- fs_stepwise(mtcars, target = "mpg", direction = "both")
#'   print(res$selected)
#'   # |t| of the retained terms: a rough ordering, not valid inference
#'   print(res$scores)
#'
#'   # the returned model is an ordinary lm, but update() needs 'data'
#'   refit <- stats::update(res$model, . ~ . - wt, data = mtcars)
#'   print(stats::formula(refit))
#' }
#' }
#' @seealso The [Wrapper methods](https://elkronos.github.io/featR/articles/wrappers.html)
#'   guide, which runs this function on a worked example.
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
  assert_column_names(data, "fs_stepwise")
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
  # na.omit() leaves Inf/-Inf in place, which lm() rejects as "NA/NaN/Inf in
  # 'x'" (or 'y').
  inf_cols <- names(data)[vapply(data, function(col) {
    is.numeric(col) && any(is.infinite(col))
  }, logical(1L))]
  if (length(inf_cols) > 0L) {
    stop("Columns contain infinite values (Inf/-Inf): ",
         paste(utils::head(inf_cols, 5L), collapse = ", "),
         if (length(inf_cols) > 5L) ", ..." else "",
         ". Remove or replace them before calling fs_stepwise().",
         call. = FALSE)
  }

  # model.matrix()/lm() die with an unnamed "contrasts can be applied only to
  # factors with 2 or more levels" otherwise. Checked after na.omit(), which
  # can remove a level.
  lasso_check_categorical_levels(data[setdiff(names(data), dep_var)], "fs_stepwise")

  if (verbose) {
    message(sprintf("Starting stepwise selection ('%s') for target '%s'.",
                    direction, dep_var))
  }

  # Private environment that carries the data for model fitting. It survives in
  # the returned model's formula/terms environment (never the global
  # environment), which is what keeps predict()/add1()/drop1() working, and it
  # holds a full copy of the complete-case data for as long as that model
  # lives. Its parent is the caller's frame, so ordinary lexical lookup still
  # reaches the user's own objects.
  fit_env <- new.env(parent = parent.frame())
  assign(".fs_stepwise_data", data, envir = fit_env)

  dots <- list(...)
  # Backward and both-direction searches start from the full `~ .` model, so
  # a saturated full model (AIC = -Inf) makes them meaningless. A forward
  # search starts from the intercept and only uses the full model as its upper
  # scope, so p >= n is a legitimate forward use case.
  parts <- step_build_models(dep_var, direction = direction,
                             fit_env = fit_env,
                             check_saturated = direction != "forward")

  step_model <- step_run_stepwise(
    start_model = parts$start_model,
    scope = parts$scope,
    direction = parts$direction,
    verbose = verbose,
    dots = dots,
    fit_env = fit_env
  )
  if (stats::df.residual(step_model) <= 0L) {
    warning("fs_stepwise(): the forward search ended on a model with no ",
            "residual degrees of freedom (a perfect fit); the selection is ",
            "not meaningful. Limit 'steps' or use fewer candidate predictors.",
            call. = FALSE)
  }

  coefficients <- step_coef_summary(step_model)
  # Term labels of non-syntactic columns come back backticked; report the
  # caller's column names instead.
  terms_selected <- unbacktick_names(
    attr(stats::terms(step_model), "term.labels")
  )

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

  coef_scores <- step_abs_t(coefficients)
  new_fs_result(
    selected = terms_selected,
    scores   = step_term_scores(step_model, coefficients, terms_selected),
    method   = "stepwise",
    task     = "regression",
    model    = step_model,
    details  = list(
      final_model     = step_model,
      coefficients    = coefficients,
      coefficient_scores = coef_scores,
      selected_terms  = terms_selected,
      direction       = direction,
      n_features      = n_candidates,
      dropped_na_rows = dropped_na_rows
    ),
    call = cl_call
  )
}
