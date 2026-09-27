# Correlation-based feature selection

Flags variable pairs whose absolute correlation exceeds `threshold` and,
by default, reduces each correlated group to a single representative.

## Usage

``` r
fs_correlation(
  data,
  threshold,
  method = "pearson",
  prune = TRUE,
  na.rm = FALSE,
  sample_frac = 1,
  output_format = "matrix",
  diag_value = 0,
  seed = NULL,
  verbose = FALSE,
  parallel = FALSE,
  n_cores = 2L
)
```

## Arguments

- data:

  A data frame or matrix with at least 2 columns and unique column names
  (a correlation matrix with duplicated dimnames is ambiguous, so
  duplicates are rejected rather than silently resolved to the first
  match). For `"pearson"`, `"spearman"`, `"kendall"`: all columns must
  be numeric. For `"polychoric"`: all columns must be ordered factors.
  For `"pointbiserial"`: columns may be numeric (continuous) or
  dichotomous (exactly 2 unique non-NA values).

- threshold:

  Numeric between 0 and 1. Pairs with \|correlation\| \> threshold are
  flagged as redundant. Required; there is no default.

- method:

  One of `"pearson"` (default), `"spearman"`, `"kendall"`,
  `"polychoric"`, `"pointbiserial"`. Point-biserial correlations are
  computed as the Pearson correlation between the continuous variable
  and a 0/1 indicator of the dichotomous variable (1 for the second
  sorted unique value, e.g. the second factor level), which is the
  definition of the point-biserial coefficient. Only
  continuous-dichotomous pairs are defined under that method: cells for
  continuous-continuous and dichotomous-dichotomous pairs stay `NA`, so
  those pairs can never be flagged.

- prune:

  Logical. If `TRUE` (default), `selected` is the reduced non-redundant
  set: variables are dropped one at a time until no two retained
  variables correlate above `threshold`, each step dropping the member
  of the strongest remaining pair with the HIGHER mean absolute
  correlation to the other retained variables. This borrows the
  tie-break of
  [`caret::findCorrelation()`](https://rdrr.io/pkg/caret/man/findCorrelation.html)
  but not its scan order (caret visits columns in order of mean
  correlation rather than pairs in order of strength), so the two can
  drop different variables, and neither is guaranteed to drop the
  fewest. Variables that were never flagged are always retained. If
  `FALSE`, `selected` is every variable appearing in at least one
  flagged pair, i.e. the redundant set, and nothing is dropped.

- na.rm:

  Logical. If `TRUE`, missing values are removed pairwise for
  `"pearson"`/`"spearman"`/`"kendall"`, per pair (complete observations
  within each variable pair) for `"pointbiserial"`, and casewise
  (complete cases across all columns) for `"polychoric"`. If `FALSE`
  (default), missing values propagate NA into the affected correlations,
  except for `"polychoric"`, which stops with an error when missing
  values are present (silently deleting cases would contradict the
  behavior of the other methods). Default `FALSE`.

- sample_frac:

  Numeric in (0, 1\]. Fraction of rows sampled without replacement
  (rounded up, never below one row) before computing correlations.
  Default `1` (no sampling).

- output_format:

  `"matrix"` (default) or `"data.frame"` for the correlation matrix
  stored in `details$corr_matrix`.

- diag_value:

  Single numeric value, or `NA`, assigned to the diagonal of the
  reported correlation matrix. It is cosmetic: the diagonal never takes
  part in flagging, scoring, or pruning. Default `0`.

- seed:

  Optional integer seed for reproducible sampling, applied locally; the
  previous RNG state is restored afterwards. Default `NULL` (never
  seeds).

- verbose:

  Logical. Emit progress messages (row sampling, the correlation method
  used, and any variables dropped by pruning). Default `FALSE`.

- parallel:

  Logical. Use parallel processing (via the suggested foreach and
  doParallel packages) for point-biserial computations. Ignored by every
  other method, all of which are single-call. Default `FALSE`
  (sequential).

- n_cores:

  Whole number \>= 1. Number of workers if `parallel = TRUE` and
  `method = "pointbiserial"`; requests are capped at the detected core
  count. Default `2`.

## Value

An object of class `fs_result` with:

- selected:

  Character vector. With `prune = TRUE`, every variable that survives
  pruning (all columns except `details$dropped`), no two of which have a
  computable absolute correlation above `threshold`. With
  `prune = FALSE`, every variable appearing in at least one flagged pair
  (both members of each pair), which is the redundant set rather than
  the set to keep.

- scores:

  Named numeric vector giving each variable's maximum absolute
  correlation with any other variable (NA when no correlation with that
  variable could be computed).

- method:

  `paste0("correlation_", method)`, e.g. "correlation_pearson".

- task:

  `NA_character_`; correlation filtering is unsupervised.

- model:

  NULL; no model is fitted.

- details:

  A list with `corr_matrix` (the correlation matrix, with the diagonal
  set to `diag_value`, reshaped to long form with columns Var1, Var2,
  Correlation when `output_format = "data.frame"`), `pairs` (a
  data.frame of the flagged pairs with columns Var1, Var2, Correlation,
  ordered by decreasing absolute correlation and unaffected by
  `output_format`), `dropped` (variables removed by pruning, in the
  order they were dropped; empty when `prune = FALSE`), `redundant`
  (every variable in at least one flagged pair, i.e. `selected` as it
  would be under `prune = FALSE`), and `n_features` (the number of
  variables considered).

- call:

  The matched call.

## Details

This is an unsupervised redundancy filter: it looks only at how the
variables relate to one another and never at an outcome. When two
variables are near-interchangeable it therefore cannot prefer the one
that predicts better; it keeps whichever is least entangled with the
rest of the data. Use
[`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
when the choice inside a correlated group should be driven by importance
for a target.

Pruning is greedy rather than group-wise: while any retained pair still
exceeds `threshold`, the strongest such pair is taken and the member
with the larger mean absolute correlation to the other retained
variables is dropped. No two retained variables end up with a computable
correlation above `threshold`, but the surviving set is not guaranteed
to be the smallest one with that property, and it depends on the order
in which pairs are resolved.

Correlations that cannot be computed come back as `NA`. Those pairs are
never flagged and never pruned – an unknown correlation is treated as no
evidence of redundancy – and a warning reports how many there are. A
message (not a warning) is emitted when no pair at all exceeds
`threshold`, whatever `verbose` is set to.

## Examples

``` r
d <- data.frame(
  a = c(1, 2, 3, 4, 5, 6),
  b = c(2, 4, 6, 8, 10, 12),
  c = c(1.5, 0.9, 2.1, 0.4, 1.1, 0.8)
)
res <- fs_correlation(d, threshold = 0.9)
res$selected
#> [1] "a" "c"
res$details$pairs
#>   Var1 Var2 Correlation
#> 1    a    b           1

# keep the legacy view: both members of every flagged pair
fs_correlation(d, threshold = 0.9, prune = FALSE)$selected
#> [1] "a" "b"
```
