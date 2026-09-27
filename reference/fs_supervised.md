# Supervised Filter-Based Feature Selection

Performs supervised, univariate, filter-based feature selection: every
column of `data` except `target` is scored against the target, and
features are selected or dropped by comparing that score to `threshold`.

## Usage

``` r
fs_supervised(
  data,
  target,
  method = c("auto", "correlation", "anova"),
  threshold = 0,
  direction = c("above", "below"),
  action = c("keep", "remove"),
  include_equal = FALSE,
  na_rm = TRUE,
  output = c("result", "matrix", "dt", "data.frame", "mask", "indices", "names", "list"),
  verbose = FALSE
)
```

## Arguments

- data:

  A data.frame, data.table, or matrix containing `target` and the
  candidate feature columns. Every column other than `target` must be
  numeric. The input is copied, never modified in place.

- target:

  Character. Name of the target column in `data`. It is removed from the
  candidate features and used as the scoring target. The target column
  itself may be numeric, factor, character, or logical; any other type
  is an error.

- method:

  One of `"auto"` (default), `"correlation"`, `"anova"`.

- threshold:

  Non-negative, finite numeric scalar threshold applied to the feature
  scores (not to the target directly). Default 0. Note that the two
  methods put scores on different scales: `[0, 1]` for `"correlation"`
  and `[0, Inf]` for `"anova"`.

- direction:

  One of `"above"` (default), `"below"`; compares scores to `threshold`.

- action:

  One of `"keep"` (default), `"remove"`; determines whether features
  meeting the condition are retained or dropped.

- include_equal:

  Logical; if TRUE, comparisons are inclusive (greater/less than or
  equal) instead of strict. Default FALSE.

- na_rm:

  Logical; if TRUE (the default), rows with NA in a feature or in the
  target are dropped when computing that feature's score. If FALSE, an
  NA anywhere in a feature or the target makes that feature's score
  undefined.

- output:

  One of `"result"` (default), `"matrix"`, `"dt"`, `"data.frame"`,
  `"mask"`, `"indices"`, `"names"`, `"list"`. Throughout, "candidate
  features" means the columns of `data` other than `target`, in their
  original order.

  - `"result"` (default): an `fs_result` object (see Value).

  - `"matrix"`: numeric matrix of the selected features.

  - `"dt"`: data.table of the selected features.

  - `"data.frame"`: data.frame of the selected features.

  - `"mask"`: logical vector, one element per candidate feature, named
    after the candidate features; TRUE marks a selected column.

  - `"indices"`: integer vector of the selected column indices, indexing
    the candidate features (that is, `data` without `target`), named
    after the selected columns.

  - `"names"`: character vector of the selected column names.

  - `"list"`: list with components `filtered` (matrix), `mask`,
    `indices`, `names`, `scores` (all as above), and `meta`, a list
    recording `method_arg` (the method as requested), `method_used` (the
    method after `"auto"` was resolved), `threshold`, `direction`,
    `action`, `include_equal`, `na_rm`, `n_input_cols`, and
    `n_kept_cols`.

- verbose:

  Logical; emit progress messages. Default FALSE.

## Value

With the default `output = "result"`, an object of class `fs_result`
with elements:

- `selected`: character vector of selected feature names.

- `scores`: named numeric vector of per-feature scores, in column order,
  `NA` where the score is undefined.

- `method`: `"supervised_"` followed by the resolved scoring method, for
  example `"supervised_correlation"`.

- `task`: `"regression"` for a numeric target, `"classification"` for a
  factor target, `NA` otherwise.

- `model`: `NULL`.

- `details`: a list holding, in this order, `mask` (the logical
  keep-mask over the candidate features), `indices` (the selected column
  indices, named after the selected columns), `filtered` (the selected
  columns as a data.table), `threshold`, `direction`, `action`, and
  `n_features` (the number of candidate features, that is
  `ncol(data) - 1`).

- `call`: the matched call.

Any other `output` returns that shape instead, exactly as documented
above. When no feature meets the selection criteria, a warning is issued
and the tabular shapes come back empty: `"matrix"` and `"data.frame"`
have zero columns and keep the input row count, while the `"dt"` shape
(and `details$filtered`) is only guaranteed to have zero columns –
data.table represents a zero-column table as having zero rows, so its
row count is not preserved.

## Details

This is one of the cheapest supervised screens in featR: no predictive
model is fitted, each feature is scored on its own, and the cost is
linear in the number of columns, so it scales to wide data. The flip
side is that scoring is strictly univariate: it cannot see interactions
between features, and it will happily keep a whole group of
near-duplicate columns that all correlate with the target. Use
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
to prune that redundancy afterwards, or a wrapper such as
[`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
or [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
when the joint contribution is what matters.

Supported methods:

- `"correlation"`: Absolute Pearson correlation (numeric target), so
  scores lie in `[0, 1]`. A constant feature (or target) has no defined
  correlation and scores `NA`; the check is relative to the column's
  magnitude, so features measured on a very small scale are scored
  normally.

- `"anova"`: One-way ANOVA F-statistic (categorical target), so scores
  lie in `[0, Inf]`: a feature that separates the classes perfectly (no
  spread within any class) scores `Inf`, and a constant feature scores
  `NA`.

- `"auto"`: Chooses `"correlation"` for a numeric target and `"anova"`
  for a categorical one. Because the two score scales differ, a message
  reports the resolved method when `verbose = TRUE`.

Features whose score is undefined (`NA`) are never selected, under both
`action = "keep"` and `action = "remove"`; a warning reports how many
such features were excluded.

Columns are subset by integer index, never by name, so duplicated column
names cannot select the wrong columns.

## Examples

``` r
df <- data.frame(
  strong = c(1, 2, 3, 4),
  mirror = c(4, 3, 2, 1),
  weak   = c(1, 0, 1, 0),
  y      = c(1, 2, 3, 4)
)

# Default: an fs_result
res <- fs_supervised(df, target = "y", method = "correlation",
                     threshold = 0.5)
res$selected
#> [1] "strong" "mirror"
res$scores
#>    strong    mirror      weak 
#> 1.0000000 1.0000000 0.4472136 
res$details$filtered
#>    strong mirror
#>     <num>  <num>
#> 1:      1      4
#> 2:      2      3
#> 3:      3      2
#> 4:      4      1

# The classic shapes are still available
fs_supervised(df, target = "y", method = "correlation", threshold = 0.5,
              output = "names")
#> [1] "strong" "mirror"

# ANOVA against a factor target
df_fac <- data.frame(
  wide = c(1, 2, 10, 11),
  mild = c(1, 3, 2, 4),
  grp  = factor(c("a", "a", "b", "b"))
)
fs_supervised(df_fac, target = "grp", method = "anova", threshold = 1,
              output = "matrix")
#>      wide
#> [1,]    1
#> [2,]    2
#> [3,]   10
#> [4,]   11
```
