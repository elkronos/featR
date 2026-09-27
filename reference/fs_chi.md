# Chi-square feature selection for categorical features

Tests association between each categorical feature and a (categorical)
target via the chi-square test of independence. Character columns are
automatically coerced to factors, and only factor features (excluding
the target) are tested. Handles missing values per-feature, switches to
simulation-based p-values when any expected cell count is \< 5, and
supports multiple-testing correction.

## Usage

``` r
fs_chi(
  data,
  target,
  sig_level = 0.05,
  continuity_correction = NULL,
  p_adjust_method = "bonferroni",
  simulation_B = 2000,
  seed = NULL,
  verbose = FALSE,
  parallel = FALSE,
  n_cores = 2L
)
```

## Arguments

- data:

  A data.frame or data.table with features and target, with unique
  column names. Character columns are coerced to factor. The input
  object is never modified.

- target:

  Character scalar: name of the target column. It is coerced to a factor
  if necessary and must have at least 2 non-NA levels.

- sig_level:

  Numeric threshold for significance, strictly between 0 and 1 (default
  0.05).

- continuity_correction:

  NULL/TRUE/FALSE: apply Yates correction to 2x2 tables tested
  asymptotically. If NULL (default), auto-apply to every such table;
  TRUE is equivalent, FALSE disables it. It has no effect on tables
  larger than 2x2 or on simulation-based p-values, neither of which is
  ever corrected.

- p_adjust_method:

  Character: one of
  [`stats::p.adjust.methods`](https://rdrr.io/r/stats/p.adjust.html)
  (default "bonferroni"). Set to "none" to disable multiple-testing
  correction. Matching is case-insensitive. The correction counts only
  the features that were actually tested: a skipped feature has an `NA`
  p-value, and
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) drops NAs
  before its default `n = length(p)` is evaluated, so skipped features
  keep an `NA` adjusted p-value and do not inflate the multiplier for
  the others. With `k` tested and `s` skipped features, "bonferroni"
  therefore multiplies by `k`, not by `k + s`.

- simulation_B:

  Whole number \>= 100: replicates for the simulation-based p-value used
  when any expected cell count is \< 5 (default 2000).

- seed:

  Optional integer. Seeds the RNG locally (the previous RNG state is
  restored on exit), which makes simulation-based p-values reproducible
  in the sequential path. The parallel path draws from furrr's own
  L'Ecuyer-CMRG parallel streams (`furrr_options(seed = TRUE)`), so for
  the same `seed` parallel results are internally reproducible but
  differ from sequential results. Default NULL (never seeds).

- verbose:

  Logical; if TRUE, emits informative messages (target coercion, skipped
  features, worker count). Default FALSE.

- parallel:

  Logical; if TRUE, run features in parallel using the suggested furrr
  and future packages. Default FALSE (sequential).

- n_cores:

  Whole number \>= 1. Number of workers used when `parallel = TRUE`;
  requests are capped at the detected core count. A
  [`future::multisession`](https://future.futureverse.org/reference/multisession.html)
  plan is set for the duration of the call and the previous plan is
  restored on exit. Default 2.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of features with adj_p_value \< sig_level.

- scores:

  Named numeric vector of adjusted p-values, one per candidate
  categorical feature and `NA` for any feature that had to be skipped
  (smaller is stronger evidence of association).

- method:

  "chi".

- task:

  "classification".

- model:

  NULL; the chi-square filter fits no model.

- details:

  A list with `results` (the full results data.frame, one row per
  candidate categorical feature, ordered by adj_p_value then p_value so
  that skipped features sort last, with columns: feature; n (for tested
  features, the number of complete feature-target pairs; for skipped
  features, the feature's non-NA row count); df (NA for simulation-based
  tests, where the asymptotic degrees of freedom do not apply, and for
  skipped features); p_value; adj_p_value; significant (TRUE only when
  adj_p_value \< sig_level, so FALSE for skipped features); method
  ("asymptotic" or "simulation", NA when skipped); correction_applied
  (TRUE/FALSE, NA when skipped); min_expected (minimum expected cell
  count, NA when skipped)), plus `sig_level`, `p_adjust_method` (the
  method string as supplied), and `n_features` (the number of candidate
  categorical features, including any that were skipped).

- call:

  The matched call.

## Details

The question this answers is, per feature: does the joint distribution
of that feature and the target differ from what independence would
predict? Features are examined one at a time, so the result describes
marginal association only. It says nothing about interactions, and two
features carrying the same information are both reported as significant.
Only factor features are tested: numeric, logical and date columns are
ignored entirely, so convert or discretize them first if you want them
included.

Each feature is tested on its own complete cases (rows where both the
feature and the target are observed), so `n` can differ between
features. Levels left empty after that filtering are dropped; if either
the feature or the target then has fewer than two levels, the feature is
skipped with an `NA` p-value rather than tested.

A feature is tested with the asymptotic chi-square statistic when every
expected cell count is at least 5. Otherwise the p-value comes from a
Monte-Carlo simulation with `simulation_B` replicates, which makes it
stochastic unless `seed` is set and leaves `df` as `NA`. Yates'
continuity correction applies only on the asymptotic path and only to
2x2 tables; it is never applied to a simulated p-value or to a larger
table, whatever `continuity_correction` says.

Finally, a p-value is evidence against independence, not an effect size:
with enough rows a negligible association still clears any `sig_level`.

## Examples

``` r
d <- data.frame(
  f1 = factor(rep(c("A", "B", "A"), times = c(40, 40, 20))),
  f2 = factor(rep(c("X", "Y"), times = 50)),
  target = factor(rep(c("Yes", "No"), each = 50))
)
out <- fs_chi(d, "target")
out$selected
#> [1] "f1"
out$details$results
#>   feature   n df      p_value  adj_p_value significant     method
#> 1      f1 100  1 0.0001051636 0.0002103271        TRUE asymptotic
#> 2      f2 100  1 1.0000000000 1.0000000000       FALSE asymptotic
#>   correction_applied min_expected
#> 1               TRUE           20
#> 2              FALSE           25
```
