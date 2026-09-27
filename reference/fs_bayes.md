# Bayesian feature selection for model optimization

Fits a brms model for every candidate predictor combination and compares
them with
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html),
returning the selected combination as an `fs_result`.

## Usage

``` r
fs_bayes(
  data,
  target,
  predictors,
  date_col = NULL,
  brm_family = stats::gaussian(),
  prior = NULL,
  brm_args = list(),
  rule = c("1se", "best"),
  max_comb_size = NULL,
  sample_combinations = NULL,
  parallel_combinations = FALSE,
  seed = NULL,
  verbose = FALSE,
  n_cores = 1L
)
```

## Arguments

- data:

  A data.frame or data.table. It is copied, never modified. Only
  `target`, `predictors` and `date_col` are carried forward, and rows
  with a missing value in any of them are dropped before any model is
  fitted.

- target:

  Character. Name of the target (response) column, which must exist in
  `data` and suit `brm_family` (numeric for
  [`stats::gaussian()`](https://rdrr.io/r/stats/family.html), 0/1 or
  logical for
  [`brms::bernoulli()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)).

- predictors:

  Character vector. Names of the candidate predictor columns; at least
  one, all present in `data`.

- date_col:

  Character or NULL. Name of a date column. When provided, an
  `iso_week_id` feature (ISO year \* 100 + ISO week) is added to the
  predictors. Note: `iso_week_id` enters the models as a continuous
  covariate, which is a rough encoding of seasonality; a categorical or
  cyclic encoding is deferred to a future version. Default NULL (no date
  feature; the date column itself is never used as a predictor).

- brm_family:

  A model family accepted by brms::brm(), for example stats::gaussian()
  (default) or brms::bernoulli().

- prior:

  A brms prior specification (default NULL).

- brm_args:

  List. Extra arguments for brms::brm() (for example iter, warmup,
  seed), overriding featR's defaults of iter = 2000, adapt_delta = 0.99,
  max_treedepth = 15 and refresh = 0. `chains` and `cores` are respected
  when evaluating combinations sequentially (`cores` is capped at the
  detected core count; the defaults are 4 chains on 1 core), but both
  are forced to 1 when `parallel_combinations = TRUE`. Default
  [`list()`](https://rdrr.io/r/base/list.html).

- rule:

  Selection rule: `"1se"` (default) keeps the most parsimonious model
  within one standard error of the best elpd; `"best"` keeps the raw
  elpd maximum.

- max_comb_size:

  Whole number \>= 1, or NULL. Largest number of predictors allowed in a
  combination; values above the number of candidate predictors are
  capped rather than rejected. Default NULL (all sizes).

- sample_combinations:

  Whole number \>= 1, or NULL. Randomly sample this many combinations
  instead of evaluating all of them; ignored when fewer combinations
  exist. Pass `seed` to make the draw reproducible. Default NULL
  (evaluate every combination).

- parallel_combinations:

  Logical. Evaluate predictor combinations in parallel via
  parallel::mclapply(). Not available on Windows (falls back to
  sequential evaluation with a message), and falls back to sequential
  evaluation when `n_cores` resolves to 1. Because each model is then
  held to a single MCMC chain, cross-chain convergence diagnostics such
  as R-hat become unavailable; a warning says so. Default FALSE.

- seed:

  Optional whole number (within the R integer range; fractional values
  are an error rather than being truncated). When supplied, seeds the
  random sampling of combinations (see `sample_combinations`) locally;
  the previous RNG state is restored on exit. Default NULL: fs_bayes()
  never seeds the RNG unless asked. Note this does not seed the
  samplers; pass `seed` inside `brm_args` to control brms itself.

- verbose:

  Logical. Print progress information, and show a progress bar for
  sequential evaluation when the suggested pbapply package is installed.
  Default FALSE.

- n_cores:

  Whole number \>= 1. Worker count used when
  `parallel_combinations = TRUE`, and ignored otherwise. Default 1
  (sequential); requests are capped at the detected core count.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of the predictors in the chosen model.

- scores:

  `NULL`: no per-feature score is comparable across combination models.
  `details$n_features` records how many candidate predictors were
  offered.

- method:

  "bayes".

- task:

  "regression", or "classification" when `brm_family` is one of the
  categorical brms families.

- model:

  The selected `brmsfit`.

- details:

  A list with `data` (the complete-case modeling data.table: `target`,
  `predictors`, `date_col` and `iso_week_id` where applicable, plus
  appended `fitted_values`, `residuals`, `abs_residuals` and
  `squared_residuals` columns), `mae` and `rmse` (in-sample,
  post-selection errors computed on the same rows used to fit and
  select, so they are optimistic), `formula` (the selected model's
  formula string), `best_elpd` (the selected model's elpd_loo, possibly
  `NA`), `loo_comparison` (the
  [`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
  table – a matrix or data.frame depending on the loo version – or
  `NULL` when fewer than two models could be compared or the comparison
  failed), `n_failed_fits` (how many combinations failed to fit) and
  `n_features` (the number of candidate predictors, including
  `iso_week_id` when `date_col` is supplied).

- call:

  The matched call.

## Details

Use this when you want the predictor subset itself chosen by
out-of-sample predictive fit under a fully Bayesian model, and you can
afford to fit one model per subset. The search is exhaustive by default:
with `p` candidate predictors it fits every non-empty subset, `2^p - 1`
models, and each one compiles and samples its own Stan program. Use
`max_comb_size` or `sample_combinations` to bound the search.

Model selection uses
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
rather than the raw elpd maximum. With `rule = "1se"` (the default) the
chosen model is the most parsimonious one – fewest predictors, ties
broken by the higher elpd – among those whose elpd difference from the
best model is no larger in absolute value than one standard error of
that difference (the `se_diff` column of the comparison table). With
`rule = "best"` the raw elpd maximum wins, which is the older behavior
and is more prone to over-fitting the comparison. When no usable
comparison table is available, both rules fall back to the raw elpd
maximum, ties broken by fewer predictors.

Combinations whose model fails to fit are excluded from selection and
counted in `details$n_failed_fits`, with a warning. If no fit yields a
finite `elpd_loo` at all, the first successfully fitted model is
returned with a warning; that is an arbitrary fallback, not a selection.

Per-model sampler warnings (divergent transitions, R-hat, effective
sample size) and loo's Pareto-k warnings are muffled while the models
are fitted, so a search over many subsets does not flood the console,
but they are not discarded: after the search one warning summarizes how
many fits raised sampler warnings and another how many have observations
with Pareto k \> 0.7 (unreliable PSIS-LOO estimates), each stating
whether the selected model is among them. `verbose = TRUE` shows every
individual warning.

Two caveats are worth stating plainly. `details$mae` and `details$rmse`
are in-sample errors computed on the same rows used to fit and to
select, so they are optimistic. And anything read off the returned
`brmsfit` (posterior intervals, effect sizes) is post-selection
inference: the model was chosen by looking at the same data it is
reported on.

## Examples

``` r
# Fitting needs more than the brms package: brms compiles and samples a
# Stan program for each candidate model, which requires a working C++
# toolchain. The call is therefore guarded on brms being installed, limited
# to two single-predictor fits via max_comb_size = 1, and wrapped in try()
# so that a machine without a usable Stan toolchain skips the example
# instead of failing it.
# \donttest{
if (requireNamespace("brms", quietly = TRUE)) {
  x1 <- seq(-2, 2, length.out = 40)
  x2 <- rep(c(-1, 1), 20)
  d <- data.frame(
    y  = 1 + 2 * x1 + sin(seq_len(40)),
    x1 = x1,
    x2 = x2
  )
  res <- try(
    fs_bayes(
      d, target = "y", predictors = c("x1", "x2"),
      max_comb_size = 1,
      brm_args = list(chains = 1, iter = 500, refresh = 0),
      rule = "1se", verbose = FALSE
    ),
    silent = TRUE
  )
  if (!inherits(res, "try-error")) {
    print(res$selected)
    print(res$details$loo_comparison)
  }
}
#> Compiling Stan program...
#> Compiling Stan program...
#> Warning: 2 of 2 model fits failed and were excluded from selection.
# }
```
