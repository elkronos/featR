# Feature selection using Boruta

Runs the Boruta all-relevant feature selection algorithm on a dataset.
Preprocesses predictors, optionally seeds the RNG locally, optionally
resolves tentative features, and optionally prunes correlated features
from the confirmed set. Pruning is importance-aware: within a group of
correlated features the one with the highest median Boruta importance is
kept.

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
  may be numeric, factor, character, logical, Date or POSIXt; character
  and logical columns are converted to factors and Date/POSIXt columns
  to numeric, and any other type is an error. Neither the target nor the
  predictors may contain missing values, since Boruta's underlying
  random forest cannot fit them.

- target:

  Name of the target column in `data`. A factor target is treated as
  classification, a numeric target as regression; any other type is an
  error, as is a target containing NAs.

- maxRuns:

  Whole number of at least 11 (the minimum Boruta itself accepts).
  Maximum number of Boruta iterations. Default 250.

- cutoff_features:

  Optional whole number capping the number of returned features. When
  supplied, the top features by median Boruta importance are retained,
  applied after any correlation pruning. Default NULL (no cap).

- cutoff_cor:

  Numeric correlation cutoff between 0 and 1 used to drop redundant
  features from the selected set. Within each group of features
  correlated above the cutoff, the feature with the highest median
  Boruta importance is kept and the rest are dropped. Only numeric
  predictors are compared (absolute Pearson correlation); factor
  predictors are never pruned. Set NULL to skip this step. Default 0.7.

- resolve_tentative:

  Logical; if TRUE, apply
  [`Boruta::TentativeRoughFix()`](https://rdrr.io/pkg/Boruta/man/TentativeRoughFix.html)
  and return only confirmed attributes. If FALSE, tentative attributes
  are included in the selected set. Default TRUE.

- seed:

  Optional single whole number (within the range of an R integer) for
  reproducibility; fractional or out-of-range values are an error.
  Applied locally: the previous RNG state is restored when the function
  exits. Default NULL (the RNG is never seeded unless requested).

- verbose:

  Logical; if TRUE, report progress and name any features dropped by
  correlation pruning. This maps to Boruta's `doTrace = 1` (decisions
  are reported as they are made); FALSE maps to `doTrace = 0`. Default
  FALSE.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of selected feature names, after optional correlation
  pruning and the optional `cutoff_features` cap.

- scores:

  Named numeric vector of median Boruta importance for every candidate
  feature (`NA` for attributes with no importance history).

- method:

  "boruta".

- task:

  "classification" for a factor target, "regression" for a numeric one.

- model:

  The Boruta object, after
  [`Boruta::TentativeRoughFix()`](https://rdrr.io/pkg/Boruta/man/TentativeRoughFix.html)
  when `resolve_tentative = TRUE` and something was still tentative.

- details:

  A list with `boruta_obj` (the same Boruta object as `model`),
  `decisions` (the per-feature Confirmed/Tentative/Rejected factor, as
  it stands after any tentative fix), `dropped_correlated` (features
  removed by correlation pruning, empty when `cutoff_cor` is NULL), and
  `n_features` (the number of candidate features).

- call:

  The matched call.

## Details

Boruta answers "which features carry any information about the target?",
not "which minimal subset predicts best". It is an all-relevant
selector: across up to `maxRuns` random-forest fits it compares each
feature against "shadow" features built by permuting the predictors, and
confirms every feature that beats the best shadow often enough to be
unlikely by chance. Redundant-but-informative features are therefore all
confirmed. That makes it a good fit for understanding a dataset, and a
poor fit when you need a compact model. It also costs many forest fits,
and the outcome varies from run to run unless `seed` is supplied. The
forests are grown on a single thread (`num.threads = 1` is passed to
Boruta's importance source), in line with featR's sequential default.

The `cutoff_cor` pruning and the `cutoff_features` cap are featR
additions, applied to Boruta's output in that order; both rank features
by median Boruta importance. Features still undecided when `maxRuns` is
reached stay Tentative; `resolve_tentative = TRUE` settles them with the
[`Boruta::TentativeRoughFix()`](https://rdrr.io/pkg/Boruta/man/TentativeRoughFix.html)
heuristic rather than with further evidence. That heuristic confirms a
tentative feature whenever its median importance beats the median of the
best shadow, a weaker test than the one that confirms features during
the run, so it trades false negatives for false positives. In a
simulation of 20 data sets with one real predictor and three pure-noise
predictors (150 rows, 100 runs), Boruta alone confirmed 5 percent of the
noise predictors and 8.3 percent after the rough fix. Use
`resolve_tentative = FALSE` and inspect `details$decisions` when false
positives are costly, or raise `maxRuns` so fewer features stay
tentative.

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
