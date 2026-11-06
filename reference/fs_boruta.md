# All-relevant feature selection with Boruta

Runs the Boruta algorithm to find every feature that carries information
about the target, then optionally prunes correlated features and caps
how many are returned. Boruta compares each feature's random-forest
importance with that of "shadow" copies of the predictors whose values
have been shuffled, and confirms the features that beat the best shadow
more often than chance allows.

## Usage

``` r
fs_boruta(
  data,
  target,
  maxRuns = 250,
  cutoff_features = NULL,
  cutoff_cor = 0.7,
  resolve_tentative = TRUE,
  seed = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame (or data-frame-like object, or matrix). Predictor columns
  may be numeric, factor, character, logical, Date, or POSIXt; character
  and logical columns become factors, Date and POSIXt columns become
  numeric, and any other type is an error. Neither the target nor the
  predictors may contain missing values, because Boruta's random forest
  cannot fit them. Column names must be unique and non-empty, and `data`
  must hold at least one predictor besides the target.

- target:

  Name of the target column in `data`. A factor target means
  classification and a numeric target means regression; any other type
  is an error, as is a target with `NA` or infinite values. Unused
  factor levels are dropped, and a factor target with fewer than two
  observed classes is an error.

- maxRuns:

  Maximum number of Boruta iterations: a whole number of at least 11,
  the minimum Boruta itself accepts. Default 250.

- cutoff_features:

  Optional whole number capping how many features are returned. The top
  features by median Boruta importance are kept, after any correlation
  pruning. Default `NULL` (no cap).

- cutoff_cor:

  Correlation cutoff between 0 and 1 for pruning redundant features from
  the selected set (see Details). Within a correlated group the feature
  with the highest median Boruta importance is kept. Only numeric
  predictors are compared; factor predictors are never pruned. Set
  `NULL` to skip pruning. Default 0.7.

- resolve_tentative:

  Logical. If `TRUE` (default), apply
  [`Boruta::TentativeRoughFix()`](https://rdrr.io/pkg/Boruta/man/TentativeRoughFix.html)
  and return only confirmed features. If `FALSE`, tentative features are
  included in the selected set.

- seed:

  Optional single whole number within the range of an R integer;
  fractional or out-of-range values are an error. The seed is applied
  locally and the previous RNG state is restored on exit. Default
  `NULL`: the RNG is never seeded unless you ask.

- verbose:

  Logical. If `TRUE`, report progress, which sets Boruta's `doTrace = 1`
  (decisions are reported as they are made), and name any features
  dropped by correlation pruning. `FALSE` (default) sets `doTrace = 0`.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of selected feature names, after any correlation
  pruning and `cutoff_features` cap.

- scores:

  Named numeric vector of median Boruta importance for every candidate
  feature (`NA` for a feature with no importance history).

- method:

  `"boruta"`.

- task:

  `"classification"` for a factor target, `"regression"` for a numeric
  one.

- model:

  The Boruta object, after
  [`Boruta::TentativeRoughFix()`](https://rdrr.io/pkg/Boruta/man/TentativeRoughFix.html)
  when `resolve_tentative = TRUE` and some feature was still tentative.

- details:

  A list with `boruta_obj` (the same Boruta object as `model`),
  `decisions` (the per-feature Confirmed, Tentative, or Rejected factor,
  after any tentative fix), `dropped_correlated` (features removed by
  correlation pruning, empty when `cutoff_cor` is `NULL`), and
  `n_features` (the number of candidate features).

- call:

  The matched call.

## Details

Boruta is an all-relevant selector: it looks for every feature that
carries information about the target, rather than the smallest subset
that predicts best. Redundant but informative features are therefore all
confirmed, which helps you understand a dataset more than it helps you
build a compact model. The search costs up to `maxRuns` random-forest
fits, and its outcome varies from run to run unless `seed` is supplied.
The forests are grown on a single thread (`num.threads = 1` is passed to
Boruta's importance source), in line with featR's sequential default.

Features still undecided when `maxRuns` is reached are Tentative.
`resolve_tentative = TRUE` settles them with the
[`Boruta::TentativeRoughFix()`](https://rdrr.io/pkg/Boruta/man/TentativeRoughFix.html)
heuristic, not with further evidence. The heuristic confirms a tentative
feature whenever its median importance beats the median importance of
the best shadow, a weaker test than the one that confirms features
during the run, so it trades false negatives for false positives. In a
simulation of 20 data sets with one real predictor and three pure-noise
predictors (150 rows, 100 runs), Boruta alone confirmed 5 percent of the
noise predictors and 8.3 percent after the rough fix. When false
positives are costly, use `resolve_tentative = FALSE` and inspect
`details$decisions`, or raise `maxRuns` so fewer features stay
tentative.

Two featR steps then run on Boruta's selection, in this order, and both
rank features by median Boruta importance:

1.  Correlation pruning (`cutoff_cor`, on by default at 0.7). Starting
    from the most important selected feature, every remaining numeric
    feature whose absolute Pearson correlation with it exceeds
    `cutoff_cor` is dropped; the walk then moves to the next most
    important survivor. Factor predictors are never pruned. A constant
    column has an undefined correlation, which is treated as 0 with a
    warning.

2.  The cap (`cutoff_features`), which keeps the top features by median
    importance.

## See also

The [Wrapper
methods](https://elkronos.github.io/featR/articles/wrappers.html) guide,
which runs this function on a worked example.

## Examples

``` r
# \donttest{
if (requireNamespace("Boruta", quietly = TRUE)) {
  d <- data.frame(
    y  = factor(rep(c("a", "b"), each = 20)),
    x1 = rep(c(0, 1), each = 20) + seq(0, 1, length.out = 40),
    x2 = seq_len(40) %% 3
  )
  res <- fs_boruta(d, "y", maxRuns = 25, cutoff_cor = NULL, seed = 42)
  res$selected
  res$scores
}
#>          x1          x2 
#>  1.60306444 -0.07368859 
# }
```
