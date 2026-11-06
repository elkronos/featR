# Stepwise linear-regression feature selection by AIC

Finds the subset of columns that AIC keeps in a linear model. Uses
[`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html) to add or
drop one term at a time from a linear regression of `target` on all
other columns of `data`, stopping when no single move lowers AIC.
Requires the suggested package 'MASS'.

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

  A data.frame (or data.table) with at least two columns, the numeric
  target and at least one candidate predictor, with unique, non-empty
  column names and no infinite values. Every column other than `target`
  is offered to the search. For backward and both-direction searches,
  the complete rows must outnumber the coefficients of the full model.

- target:

  Single string naming the numeric target column of `data`. Unquoted
  symbols and column indices are not accepted. Non-syntactic names work,
  because they are backticked in the formula.

- direction:

  Direction of the search: `"both"` (the default), `"backward"`, or
  `"forward"`.

- verbose:

  Logical. If `TRUE`, print progress messages and the `stepAIC()` trace.
  Default `FALSE`.

- ...:

  Further arguments passed to
  [`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html), for
  example `k` (`k = log(nrow(data))` gives BIC), `steps`, or `scope`
  (which then replaces the default scope). `trace` is controlled by
  `verbose`, and a `trace` supplied here is dropped with a warning. See
  Details for what else `...` absorbs.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of the selected predictor terms, without the
  intercept.

- scores:

  Named numeric vector with one entry per selected term, keyed like
  `selected`: the absolute t statistic of the term's coefficient or, for
  a term with several coefficients (a factor, an interaction), the
  largest `|t|` among them (`NA` when all are aliased). The
  per-coefficient values (for example `grpb` and `grpc` for the term
  `grp`) are in `details$coefficient_scores`. These statistics are
  computed after selection on the same data, so they are optimistically
  biased and not valid for inference.

- method:

  `"stepwise"`.

- task:

  `"regression"`; `fs_stepwise()` fits linear models only.

- model:

  The `lm` chosen by `stepAIC()`. It carries a copy of the complete-case
  data in a private environment (see Details).

- details:

  A list with `final_model` (the same `lm`), `coefficients` (the
  [`summary()`](https://rdrr.io/r/base/summary.html) coefficient matrix,
  biased like `scores`), `coefficient_scores` (named `|t|` per
  non-intercept coefficient), `selected_terms` (the same labels as
  `selected`), `direction` (the search direction used), `n_features`
  (the number of candidate predictors, `ncol(data) - 1`), and
  `dropped_na_rows` (the number of rows removed for missing values, `0L`
  when none were).

- call:

  The matched call.

## Details

This is the cheapest wrapper method in featR and the easiest to read:
the result is an ordinary `lm` that the usual generics work on. The
search is greedy, so it can walk past the AIC-best subset, and unstable,
so small changes in the data can change the retained set.

The reported `scores` (absolute t statistics) and the p-values in
`details$coefficients` are computed on the same data that drove the
search. They are optimistically biased (the selective-inference problem)
and are not valid for formal inference; use them only as a rough
ordering of the retained terms.

Only linear regression is supported: the target must be numeric, and
every other column of `data` is a candidate predictor.
`direction = "backward"` starts from the full model; `"forward"` starts
from the intercept-only model with the full model as the upper scope;
`"both"` starts from the full model with the same scope, so terms can be
added back. There is no `seed` argument: `stepAIC()` is deterministic,
so the same call on the same data returns the same model, and the RNG is
never touched.

Rows with a missing value in any column are dropped, with a warning,
before the search, because `stepAIC()` cannot compare models fitted on
different rows. Infinite values are an error.

Backward and both-direction searches start from the full model, which
must leave residual degrees of freedom. With as many coefficients
(intercept and factor dummies included) as complete rows, the full model
fits perfectly, its AIC is -Inf, and the call stops with an error giving
the counts. A forward search may have more candidate predictors than
rows; it warns if the model it ends on has no residual degrees of
freedom, since that perfect fit makes the selection meaningless. A
factor or character predictor with fewer than two observed levels (after
incomplete rows are dropped) is an error, as is a factor dummy name that
collides with another column or the target (factor `g` with level `b`
next to a column `gb`).

The models reach their data through a private environment attached to
the model formula, so
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`anova()`](https://rdrr.io/r/stats/anova.html),
[`add1()`](https://rdrr.io/r/stats/add1.html),
[`drop1()`](https://rdrr.io/r/stats/add1.html) and similar generics work
on the returned model. Nothing is assigned to the global environment or
written to disk. That environment holds a copy of the complete-case
`data` and is kept alive by the returned model, so the returned object
is at least as large as that copy. Plain `update(model)` fails with an
object-not-found error, because
[`update()`](https://rdrr.io/r/stats/update.html) re-evaluates the
stored call in the caller's environment, where the private data is not
visible. Pass the data explicitly instead:
`update(model, . ~ . - x, data = my_data)`.

Non-syntactic column names are backticked in the formula and reported
without the backticks, so `selected` matches `names(data)`.

`...` is forwarded to
[`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html)
unchanged, so it also swallows arguments this function does not have.
The removed arguments `seed` and `return_models` are passed through and
ignored rather than rejected: a call written for the older interface
still runs, without the effect it asked for. Check your argument names
against the list below.

## See also

The [Wrapper
methods](https://elkronos.github.io/featR/articles/wrappers.html) guide,
which runs this function on a worked example.

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
#> <environment: 0x55837c212878>
# }
```
