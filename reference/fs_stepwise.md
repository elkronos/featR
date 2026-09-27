# Stepwise linear-regression feature selection via AIC

Answers "which subset of these columns does AIC keep in a linear model?"
Uses [`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html) to
perform forward, backward, or both-direction stepwise selection on a
linear regression of `target` against all other columns of `data`. For
`direction = "forward"` and `"both"` a proper null model and scope are
set up so that forward moves are possible.

## Usage

``` r
fs_stepwise(
  data,
  target,
  direction = c("both", "backward", "forward"),
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  A data.frame (or data.table) with at least two columns: the numeric
  target and at least one candidate predictor. Every column other than
  `target` is offered to the search.

- target:

  Single string naming the numeric target column of `data`. Unquoted
  symbols and column indices are not accepted. Non-syntactic names are
  backticked into the formula, so they work.

- direction:

  Direction of the search: `"both"` (the default), `"backward"`, or
  `"forward"`, matched with
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html).

- verbose:

  Logical. If `TRUE`, emits progress messages and enables the
  `stepAIC()` trace output on the console. Default `FALSE`.

- ...:

  Additional arguments passed to
  [`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html), for
  example `k` (use `k = log(nrow(data))` for BIC), `steps`, or `scope`
  (which then replaces the default scope described above). `trace` is
  the one exception: it is controlled by `verbose`, and a user-supplied
  `trace` is dropped with a warning. See Details for what else `...`
  quietly absorbs.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of the selected predictor terms (excluding the
  intercept).

- scores:

  Named numeric vector of absolute t statistics from the final model's
  coefficient table, excluding the intercept. Scores are per
  coefficient, so a selected factor term appears here under its dummy
  names (e.g. `grpb`, `grpc` for the term `grp`), not under the term
  label reported in `selected`. **Caveat**: these statistics (and the
  p-values in `details$coefficients`) are computed after selection on
  the same data, so they are optimistically biased and are not valid for
  inference.

- method:

  "stepwise".

- task:

  "regression"; `fs_stepwise()` fits linear models only.

- model:

  The `lm` selected by `stepAIC()`. It carries a copy of the
  complete-case data in a private environment (see Details).

- details:

  A list with `final_model` (the same `lm`), `coefficients` (the
  [`summary()`](https://rdrr.io/r/base/summary.html) coefficient matrix,
  same caveat as `scores`), `selected_terms` (the same term labels as
  `selected`), `direction` (the search direction actually used),
  `n_features` (the number of candidate predictors offered to the
  search, i.e. `ncol(data) - 1`) and `dropped_na_rows` (how many rows
  were removed for missing values, `0L` when none were).

- call:

  The matched call.

## Details

This is the cheapest wrapper method in featR and the easiest to read:
what comes back is an ordinary `lm` that the usual generics work on. The
price is that the search is greedy, so it can walk past the AIC-best
subset, and unstable, so small changes in the data can change the
retained set. On top of that, the statistics it reports about its own
answer are not valid; see the post-selection inference caveat below.

Requires the suggested package 'MASS'. The function is
linear-regression-only: the target must be numeric, and every other
column of `data` is offered to the search as a candidate predictor.
There is no `seed` argument, because `stepAIC()` is deterministic:
repeating a call on the same data returns the same model, and the RNG is
never touched.

**Post-selection inference caveat.** The reported `scores` (absolute t
statistics) and the p-values in `details$coefficients` are computed on
the same data that drove the search. They are optimistically biased –
the selective-inference problem – and must not be used for formal
inference, only as a rough ordering of the retained terms.

The fitted models reach their data through a private environment
attached to the model formula, so
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`anova()`](https://rdrr.io/r/stats/anova.html),
[`add1()`](https://rdrr.io/r/stats/add1.html)/[`drop1()`](https://rdrr.io/r/stats/add1.html)
and similar generics keep working on the returned model; nothing is
assigned to the global environment and nothing is written to disk. That
environment holds a copy of the complete-case `data` and is kept alive
by the returned model, so the result carries the data with it and is
correspondingly large. To refit the returned model with
[`update()`](https://rdrr.io/r/stats/update.html) from another
environment, pass the data explicitly, e.g.
`update(model, . ~ . - x, data = my_data)`. Plain `update(model)` does
*not* work: [`update()`](https://rdrr.io/r/stats/update.html)
re-evaluates the stored call in the *caller's* environment, where the
private data object is not visible, so it fails with an object-not-found
error.

Non-syntactic column names are backticked into the formula and reported
back without the backticks, so `selected` matches `names(data)`.

`...` is forwarded to
[`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html) verbatim,
which means it also absorbs arguments this function does not have. The
removed `seed` and `return_models` are passed through and ignored rather
than rejected, so a call written against the older API still runs and
quietly does nothing with them; check your argument names against the
list below.

Rows containing missing values in any column are dropped (with a
warning) before the search, because `stepAIC()` cannot compare models
fitted on differing row sets.

## Examples

``` r
# \donttest{
if (requireNamespace("MASS", quietly = TRUE)) {
  res <- fs_stepwise(mtcars, target = "mpg", direction = "both")
  print(res$selected)
  # |t| of the retained terms: a rough ordering, not valid inference
  print(res$scores)

  # the returned model is an ordinary lm, but update() needs 'data'
  refit <- stats::update(res$model, . ~ . - wt, data = mtcars)
  print(stats::formula(refit))
}
#> [1] "wt"   "qsec" "am"  
#>       wt     qsec       am 
#> 5.506882 4.246676 2.080819 
#> mpg ~ qsec + am
#> <environment: 0x56468ccd7020>
# }
```
