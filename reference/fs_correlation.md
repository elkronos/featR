# Find and prune highly correlated variables

Finds pairs of variables whose absolute correlation exceeds `threshold`
and, by default, drops variables until no such pair remains, keeping one
representative of each correlated group.

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

  A data frame or matrix with at least 2 columns. Column names must be
  unique, non-empty, and not `NA`; duplicated names are an error because
  a correlation matrix with duplicated names is ambiguous. The allowed
  column types depend on `method`:

  - `"pearson"`, `"spearman"`, `"kendall"`: all numeric.

  - `"polychoric"`: all ordered factors.

  - `"pointbiserial"`: numeric, or any type with exactly 2 distinct
    non-missing values. A numeric column with more than 2 distinct
    values counts as continuous, and a column with exactly 2 counts as
    dichotomous. At least one of each is needed, or every correlation is
    `NA` (with a warning).

- threshold:

  Absolute-correlation cutoff between 0 and 1. Pairs with an absolute
  correlation strictly greater than `threshold` are flagged. Required;
  there is no default.

- method:

  One of `"pearson"` (default), `"spearman"`, `"kendall"`,
  `"polychoric"` (needs the suggested polycor package), or
  `"pointbiserial"`. The point-biserial correlation is the Pearson
  correlation between a continuous variable and a 0/1 indicator of a
  dichotomous one, where 1 marks the second sorted value (for a factor,
  the second level). Only continuous-dichotomous pairs have one, so
  under this method every continuous-continuous and
  dichotomous-dichotomous cell stays `NA` and those pairs are never
  flagged.

- prune:

  If `TRUE` (default), `selected` is the reduced set left after pruning
  (see "How pruning works"). If `FALSE`, nothing is dropped and
  `selected` is every variable in at least one flagged pair: the
  redundant set, not the set to keep.

- na.rm:

  How to handle missing values. If `TRUE`, they are removed pairwise for
  `"pearson"`, `"spearman"`, and `"kendall"`; within each pair for
  `"pointbiserial"`; and casewise (rows complete across all columns) for
  `"polychoric"`. If `FALSE` (default), a missing value makes the
  affected correlations `NA`, except under `"polychoric"`, which stops
  with an error instead of silently dropping rows.

- sample_frac:

  Fraction of rows, in (0, 1\], to sample without replacement before
  computing correlations. The sample size is rounded up and is never
  below one row. Default 1 (no sampling).

- output_format:

  Shape of `details$corr_matrix`: `"matrix"` (default) or `"data.frame"`
  (long form).

- diag_value:

  Single number, or `NA`, written on the diagonal of
  `details$corr_matrix`. The diagonal never takes part in flagging,
  scoring, or pruning. Default 0.

- seed:

  Optional integer seed for the row sampling. It is applied for this
  call only and the previous RNG state is restored afterward. It has no
  effect when `sample_frac = 1`, and parallel point-biserial computation
  uses no random numbers. Default `NULL` (featR does not touch the RNG).

- verbose:

  If `TRUE`, report row sampling, the correlation method, and any
  variables dropped by pruning. Default `FALSE`.

- parallel:

  If `TRUE`, compute point-biserial correlations in parallel with the
  suggested foreach and doParallel packages. Every other method computes
  the matrix in one call and ignores this. Default `FALSE`.

- n_cores:

  Number of workers when `parallel = TRUE` and
  `method = "pointbiserial"`, a whole number of at least 1, capped at
  the detected core count. Default 2.

## Value

An `fs_result` object with:

- `selected`:

  With `prune = TRUE`, every variable not in `details$dropped`; no two
  of them have a computable absolute correlation above `threshold`. With
  `prune = FALSE`, both members of every flagged pair.

- `scores`:

  Named numeric vector giving each variable's largest absolute
  correlation with any other variable. Larger means more redundant. `NA`
  when no correlation with that variable could be computed.

- `method`:

  `paste0("correlation_", method)`, for example `"correlation_pearson"`.

- `task`:

  `NA`; the filter uses no outcome.

- `model`:

  `NULL`; no model is fitted.

- `details`:

  A list with:

  - `corr_matrix`: the correlation matrix with `diag_value` on the
    diagonal, or, when `output_format = "data.frame"`, the same values
    in long form with columns `Var1`, `Var2`, `Correlation`.

  - `pairs`: a data frame of the flagged pairs (`Var1`, `Var2`,
    `Correlation`), strongest absolute correlation first, whatever
    `output_format` is.

  - `dropped`: variables removed by pruning, in the order they were
    dropped; empty when `prune = FALSE`.

  - `redundant`: every variable in at least one flagged pair, which is
    `selected` as it would be under `prune = FALSE`.

  - `n_features`: the number of variables considered.

- `call`:

  The matched call.

## Details

This is an unsupervised redundancy filter. It looks only at how the
variables relate to one another, never at an outcome, so when two
variables are nearly interchangeable it cannot keep the one that
predicts better. It keeps whichever is less correlated with the rest of
the data. Use
[`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
when the choice within a correlated group should depend on importance
for a target.

## How pruning works

Pruning is greedy. While any pair of retained variables still has an
absolute correlation above `threshold`, the strongest such pair is
taken, and the member with the larger mean absolute correlation to the
other retained variables is dropped (on an exact tie, the later column).
Afterward no two retained variables have a computable correlation above
`threshold`. The surviving set is not guaranteed to be the smallest one
with that property, and it can depend on the order in which pairs are
resolved.

This borrows the tie-break of
[`caret::findCorrelation()`](https://rdrr.io/pkg/caret/man/findCorrelation.html)
but not its scan order: caret visits columns in order of mean
correlation rather than pairs in order of strength, so the two can drop
different variables. Variables that were never flagged are always kept.

## Correlations that cannot be computed

A correlation that cannot be computed is `NA`. Such pairs are never
flagged and never pruned, because an unknown correlation is no evidence
of redundancy, and a warning reports how many there are. When no pair
exceeds `threshold`, a message (not a warning) says so, whatever
`verbose` is set to.

## See also

The [Filter
methods](https://elkronos.github.io/featR/articles/filters.html) guide,
which runs this function on a worked example.

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

# Both members of every flagged pair, with nothing dropped
fs_correlation(d, threshold = 0.9, prune = FALSE)$selected
#> [1] "a" "b"
```
