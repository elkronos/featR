# Bayesian predictor-subset selection with brms and LOO

Fits one brms model for each candidate combination of predictors,
estimates each model's out-of-sample fit with PSIS-LOO, and picks a
combination from the
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
table. The selected `brmsfit` is returned as the `model` of an
`fs_result`.

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
  `target`, `predictors`, and `date_col` are kept, and rows with a
  missing value in any of them are dropped before any model is fitted.
  Infinite values in those columns are an error. None of those columns
  may be named `fitted_values`, `residuals`, `abs_residuals`, or
  `squared_residuals`, because those columns are appended to
  `details$data`.

- target:

  Name of the target (response) column. It must exist in `data` and suit
  `brm_family`: numeric for
  [`stats::gaussian()`](https://rdrr.io/r/stats/family.html), 0/1 or
  logical for
  [`brms::bernoulli()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).

- predictors:

  Character vector naming the candidate predictor columns: at least one,
  without duplicates, all present in `data`.

- date_col:

  Name of a date column, or `NULL`. When given, an `iso_week_id` feature
  (ISO year \* 100 + ISO week) is added to the predictors, so no target,
  predictor, or date column may itself be named `iso_week_id`.
  `iso_week_id` enters the models as a continuous covariate, a rough
  encoding of seasonality; a categorical or cyclic encoding may come in
  a future version. The date column itself is never used as a predictor.
  Default `NULL` (no date feature).

- brm_family:

  A model family accepted by
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html), for
  example [`stats::gaussian()`](https://rdrr.io/r/stats/family.html)
  (the default) or
  [`brms::bernoulli()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).

- prior:

  A brms prior specification. Default `NULL` (the brms default priors).

- brm_args:

  List of extra arguments for
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html),
  such as `iter`, `warmup`, or `seed`. They override featR's defaults of
  `iter = 2000`, `adapt_delta = 0.99`, `max_treedepth = 15`, and
  `refresh = 0`. `chains` and `cores` are respected when combinations
  are evaluated sequentially (the defaults are 4 chains on 1 core, and
  `cores` is capped at the detected core count), but both are forced to
  1 when `parallel_combinations = TRUE`. The PSIS-LOO step uses the same
  core count, never `getOption("mc.cores")`. Default
  [`list()`](https://rdrr.io/r/base/list.html).

- rule:

  Selection rule. `"1se"` (default) keeps the most parsimonious model
  within one standard error of the best elpd; `"best"` keeps the raw
  elpd maximum. See Details.

- max_comb_size:

  Largest number of predictors in a combination, a whole number \>= 1,
  or `NULL`. Values above the number of candidate predictors are capped.
  Default `NULL` (all sizes).

- sample_combinations:

  Number of combinations to draw at random instead of evaluating all of
  them, a whole number \>= 1, or `NULL`. Ignored when fewer combinations
  exist. Pass `seed` to make the draw reproducible. Default `NULL`
  (evaluate every combination).

- parallel_combinations:

  Logical. If `TRUE`, evaluate combinations in parallel with
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html).
  Each model is then limited to a single MCMC chain, so cross-chain
  convergence diagnostics such as R-hat are unavailable; a warning says
  so. On Windows, or when `n_cores` resolves to 1, evaluation falls back
  to sequential with a message. Default `FALSE`.

- seed:

  Optional whole number within the R integer range; fractional values
  are an error. When supplied, the whole call runs on a local RNG stream
  and your RNG state is restored on exit. The seed drives the random
  draw of combinations (see `sample_combinations`) and, unless
  `brm_args$seed` is given, one Stan seed per combination, so the MCMC
  fits are reproducible too, including with
  `parallel_combinations = TRUE`. An explicit `brm_args$seed` is used
  for every model instead. Default `NULL`: `fs_bayes()` never seeds the
  RNG unless you ask, and brms and rstan then draw their seeds from the
  global stream.

- verbose:

  Logical. If `TRUE`, print progress and every per-model warning, and
  show a progress bar for sequential evaluation when the suggested
  package 'pbapply' is installed. Default `FALSE`.

- n_cores:

  Number of workers when `parallel_combinations = TRUE`, a whole number
  \>= 1; ignored otherwise. Requests are capped at the detected core
  count. Default 1 (sequential).

## Value

An object of class `fs_result` with:

- selected:

  Character vector of the predictors in the chosen model.

- scores:

  `NULL`, because no per-feature score is comparable across models with
  different predictors. `details$n_features` records how many candidate
  predictors were offered.

- method:

  `"bayes"`.

- task:

  `"classification"` when `brm_family` is a binary, categorical, or
  ordinal family (`bernoulli`, `binomial`, `beta_binomial`,
  `categorical`, `multinomial`, `cumulative`, `sratio`, `cratio`,
  `acat`), otherwise `"regression"`.

- model:

  The selected `brmsfit`.

- details:

  A list with:

  - `data`: the complete-case modeling data.table (`target`,
    `predictors`, and `date_col` and `iso_week_id` where applicable),
    with `fitted_values`, `residuals`, `abs_residuals`, and
    `squared_residuals` columns appended. Families without a single
    fitted value per row (categorical, multinomial, and multivariate
    responses) get no appended columns.

  - `mae`, `rmse`: in-sample errors on the rows used to fit and select,
    so optimistic; `NA` for the families without a single fitted value
    per row.

  - `formula`: the selected model's formula, as a string.

  - `best_elpd`: the selected model's `elpd_loo`, possibly `NA`;
    optimistic because it was selected (see Details).

  - `loo_comparison`: the
    [`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
    table (a matrix or a data.frame, depending on the loo version), or
    `NULL` when fewer than two models could be compared or the
    comparison failed.

  - `n_failed_fits`: how many combinations failed to fit.

  - `n_loo_failed`: how many fitted models were left out because `loo()`
    failed.

  - `n_compared`: how many models with a finite `elpd_loo` competed in
    the selection.

  - `n_features`: the number of candidate predictors, including
    `iso_week_id` when `date_col` is given.

- call:

  The matched call.

## Details

PSIS-LOO (Pareto-smoothed importance sampling leave-one-out)
approximates leave-one-out cross-validation from a single posterior fit.
It yields `elpd_loo`, an estimate of the expected log predictive density
for new data; higher is better. Use `fs_bayes()` when you want the
predictor subset chosen by that criterion under a fully Bayesian model
and can afford one model per subset.

### Cost of the search

The search is exhaustive by default. With `p` candidate predictors it
fits every non-empty subset, `2^p - 1` models, and each one compiles and
samples its own Stan program: 4 predictors mean 15 models, 10 predictors
mean 1023. Use `max_comb_size` or `sample_combinations` to bound the
search, and `parallel_combinations` to spread it over cores.

### How a model is chosen

With `rule = "1se"` (the default), the chosen model is the one with the
fewest predictors, ties broken by the higher elpd, among those whose
elpd difference from the best model is at most one standard error of
that difference (the `se_diff` column of the comparison table) in
absolute value. With `rule = "best"`, the raw elpd maximum wins; that
was the older behavior and is more prone to over-fitting the comparison.
When no usable comparison table is available, both rules fall back to
the raw elpd maximum, ties broken by fewer predictors.

### Failures and diagnostics

Combinations whose model fails to fit are left out of the selection,
counted in `details$n_failed_fits`, and reported in a warning that
quotes the first brms error. Fitted models for which
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) fails or
gives no finite elpd are left out in the same way, counted in
`details$n_loo_failed`, and reported in a warning. If
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
itself fails, a warning says that the raw elpd maximum is used and
`rule = "1se"` is not applied. If no fit yields a finite `elpd_loo` at
all, the first successfully fitted model is returned with a warning.
That model is an arbitrary fallback that no criterion chose.

Sampler warnings (divergent transitions, R-hat, effective sample size)
and loo's Pareto-k warnings are muffled while the models are fitted, so
a search over many subsets does not flood the console. They are counted
instead: after the search, one warning reports how many fits raised
sampler warnings and another how many have observations with Pareto k \>
0.7 (unreliable PSIS-LOO estimates), and each says whether the selected
model is among them. `verbose = TRUE` shows every individual warning.

### What is optimistic

`details$mae` and `details$rmse` are in-sample errors, computed on the
same rows used to fit and to select, so they are optimistic.

`details$best_elpd` and the `loo_comparison` table are optimistic too.
PSIS-LOO is roughly unbiased for one fixed model, but the selected
model's `elpd_loo` is the maximum (or, under `rule = "1se"`, a
near-maximum) over `details$n_compared` candidates. It therefore
overstates the chosen model's out-of-sample performance, and more so the
larger the search (selection-induced bias; Piironen and Vehtari, 2017).
An honest estimate needs the whole search repeated inside an outer
cross-validation, or held-out data.

Anything read off the returned `brmsfit`, such as posterior intervals or
effect sizes, is post-selection inference: the model was chosen using
the same data it is reported on. The [Validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.html)
article covers both points.

## References

Piironen, J. and Vehtari, A. (2017). Comparison of Bayesian predictive
methods for model selection. *Statistics and Computing*, 27(3), 711-735.

## See also

The [Wrapper
methods](https://elkronos.github.io/featR/articles/wrappers.html) guide,
which runs this function on a worked example.

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
#> Warning: 2 of 2 model fits failed and were excluded from selection. First error (y ~ x1): Boost not found; call install.packages('BH')
# }
```
