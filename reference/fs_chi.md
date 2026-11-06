# Chi-squared tests for categorical features

Tests each categorical feature for association with a categorical target
using the chi-squared test of independence, adjusts the p-values for
multiple testing, and keeps the features whose adjusted p-value is below
`sig_level`.

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

  A data frame or data.table holding the features and the target. Column
  names must be unique, non-empty, and not `NA`. The input object is
  never modified.

- target:

  Name of the target column. It is converted to a factor if necessary
  and must have at least 2 levels among its non-missing values.

- sig_level:

  Significance threshold for the adjusted p-values, strictly between 0
  and 1. Default 0.05.

- continuity_correction:

  `NULL`, `TRUE`, or `FALSE`. Controls Yates' correction for 2x2 tables
  tested asymptotically. `NULL` (the default) and `TRUE` both apply it;
  `FALSE` turns it off. Tables larger than 2x2 and simulated p-values
  are never corrected.

- p_adjust_method:

  Multiple-testing correction, one of
  [`stats::p.adjust.methods`](https://rdrr.io/r/stats/p.adjust.html),
  matched case-insensitively. Default `"bonferroni"`; `"none"` turns
  correction off. An invalid name is an error before any feature is
  tested. Only tested features count toward the correction: a skipped
  feature keeps an `NA` adjusted p-value and does not raise the
  multiplier for the others. With `k` tested and `s` skipped features,
  `"bonferroni"` multiplies by `k`, not `k + s`.

- simulation_B:

  Number of Monte Carlo replicates for the simulated p-value used when
  any expected cell count is below 5. A whole number of at least 100;
  default 2000. A simulated p-value is never below
  `1 / (simulation_B + 1)`, so after adjustment it may be unable to
  reach `sig_level` at all. With Bonferroni this happens once the number
  of tested features reaches `sig_level * (simulation_B + 1)`, which is
  101 at the defaults. A warning then names the affected features and
  the `simulation_B` you would need.

- seed:

  Optional integer. When supplied, the RNG is seeded for this call and
  the previous RNG state is restored on exit, which makes simulated
  p-values reproducible. With `parallel = TRUE` the simulations draw
  from furrr's parallel-safe streams instead
  (`furrr_options(seed = TRUE)`), so a given `seed` gives reproducible
  parallel results that differ from the sequential ones. Default `NULL`
  (featR does not touch the RNG).

- verbose:

  If `TRUE`, report target conversion, skipped features, and the worker
  count. Default `FALSE`.

- parallel:

  If `TRUE`, test features in parallel using the suggested furrr and
  future packages. Default `FALSE`.

- n_cores:

  Number of workers when `parallel = TRUE`, a whole number of at least
  1, capped at the detected core count. Default 2. If the current future
  plan is sequential (the future default), featR sets a
  [`future::multisession`](https://future.futureverse.org/reference/multisession.html)
  plan with this many workers for the duration of the call and restores
  the previous plan on exit, even if worker start-up fails. If you have
  already set a non-sequential plan, for example
  `future::plan(future::multisession, workers = 4)`, featR uses it as
  is, leaves its workers running, and ignores `n_cores`.

## Value

An `fs_result` object with:

- `selected`:

  Features with an adjusted p-value below `sig_level`.

- `scores`:

  Named numeric vector of adjusted p-values, one per candidate factor
  feature, `NA` for skipped features. Smaller means stronger evidence of
  association, the opposite of most featR scores.

- `method`:

  `"chi"`.

- `task`:

  `"classification"`.

- `model`:

  `NULL`; no model is fitted.

- `details`:

  A list with `results`, `sig_level`, `p_adjust_method` (as supplied),
  and `n_features` (the number of candidate factor features, skipped
  ones included). `results` is a data frame with one row per candidate
  feature, ordered by adjusted then raw p-value with skipped features
  last. Its columns are:

  - `feature`: the column name.

  - `n`: for tested features, the number of complete feature-target
    pairs; for skipped features, the feature's count of non-missing
    values.

  - `df`: degrees of freedom; `NA` for simulated and skipped tests.

  - `p_value`, `adj_p_value`: raw and adjusted p-values.

  - `significant`: `TRUE` when `adj_p_value < sig_level`, so `FALSE` for
    skipped features.

  - `method`: `"asymptotic"` or `"simulation"`; `NA` when skipped.

  - `correction_applied`: whether Yates' correction was actually
    applied. A 2x2 table whose counts equal their expected values gets
    no correction even when one was requested. `NA` when skipped.

  - `min_expected`: the smallest expected cell count; `NA` when skipped.

- `call`:

  The matched call.

## Details

For each feature, the test asks whether the feature and the target are
distributed jointly the way independence would predict. Features are
tested one at a time, so the result describes marginal association only.
A feature that matters only in combination with another is missed, and
two features carrying the same information are both reported as
significant.

A p-value measures evidence against independence and says nothing about
the size of the effect. With enough rows a negligible association still
clears any `sig_level`.

## Which columns are tested

Character columns are converted to factors, and then every factor column
other than the target is tested. Numeric, logical, and date columns are
ignored, so convert or discretize them first if you want them tested. If
no factor features remain, the result has an empty selection and no
error is raised. A numeric target is converted with
[`as.factor()`](https://rdrr.io/r/base/factor.html), so each distinct
value becomes its own class.

## Missing values and sparse tables

Each feature is tested on its own complete cases (rows where both the
feature and the target are observed), so `n` can differ between
features. Levels left empty after that filtering are dropped. If the
feature or the target then has fewer than two levels, the feature is
skipped: it gets an `NA` p-value and is not selected.

A feature is tested with the asymptotic chi-squared statistic when every
expected cell count is at least 5. Otherwise the p-value comes from a
Monte Carlo simulation with `simulation_B` replicates. A simulated
p-value changes from run to run unless `seed` is set, and its `df` is
`NA`. Yates' continuity correction applies only to 2x2 tables on the
asymptotic path, whatever `continuity_correction` says.

## See also

The [Filter
methods](https://elkronos.github.io/featR/articles/filters.html) guide,
which runs this function on a worked example.

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
