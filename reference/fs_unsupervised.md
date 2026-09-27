# Unsupervised Filter-Based Feature Selection

Performs unsupervised, univariate, filter-based feature selection by
scoring each feature using a chosen unsupervised criterion and selecting
or dropping features based on a threshold on that score.

## Usage

``` r
fs_unsupervised(
  data,
  method = c("variance", "mad", "iqr", "range", "missing_prop", "n_unique"),
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

  A data.frame, data.table, or matrix; all columns must be numeric.
  Every column is a candidate feature. The input is copied, never
  modified in place.

- method:

  One of `"variance"` (default), `"mad"`, `"iqr"`, `"range"`,
  `"missing_prop"`, `"n_unique"`.

- threshold:

  Non-negative, finite numeric scalar threshold applied to the feature
  scores. Default 0. The scales differ by method (a variance is in
  squared units, `"missing_prop"` is in `[0, 1]`, `"n_unique"` is a
  count), so a threshold is not portable between methods.

- direction:

  One of `"above"` (default), `"below"`; compares scores to `threshold`.

- action:

  One of `"keep"` (default), `"remove"`; determines whether features
  meeting the condition are retained or dropped.

- include_equal:

  Logical; if TRUE, comparisons are inclusive (greater/less than or
  equal) instead of strict. Default FALSE.

- na_rm:

  Logical; if TRUE (the default), remove NAs when computing scores. Has
  no effect on `"missing_prop"` and `"n_unique"`, which always account
  for NAs the same way.

- output:

  One of `"result"` (default), `"matrix"`, `"dt"`, `"data.frame"`,
  `"mask"`, `"indices"`, `"names"`, `"list"`.

  - `"result"` (default): an `fs_result` object (see Value).

  - `"matrix"`: numeric matrix of the selected features.

  - `"dt"`: data.table of the selected features.

  - `"data.frame"`: data.frame of the selected features.

  - `"mask"`: logical vector of length `ncol(data)`, named after the
    columns of `data`; TRUE marks a selected column.

  - `"indices"`: integer vector of the selected column indices, named
    after the selected columns.

  - `"names"`: character vector of the selected column names.

  - `"list"`: list with components `filtered` (matrix), `mask`,
    `indices`, `names`, `scores` (all as above), and `meta`, a list
    recording `method`, `threshold`, `direction`, `action`,
    `include_equal`, `na_rm`, `n_input_cols`, and `n_kept_cols`.

- verbose:

  Logical; emit progress messages. Default FALSE.

## Value

With the default `output = "result"`, an object of class `fs_result`
with elements:

- `selected`: character vector of selected feature names.

- `scores`: named numeric vector of per-feature scores, in column order,
  `NA` where the score is undefined.

- `method`: `"unsupervised_"` followed by the scoring method, for
  example `"unsupervised_variance"`.

- `task`: `NA_character_` (unsupervised selection has no task).

- `model`: `NULL`.

- `details`: a list holding, in this order, `mask` (the logical
  keep-mask over the candidate features), `indices` (the selected column
  indices, named after the selected columns), `filtered` (the selected
  columns as a data.table), `threshold`, `direction`, `action`, and
  `n_features` (the number of candidate features, that is `ncol(data)`).

- `call`: the matched call.

Any other `output` returns that shape instead, exactly as documented
above. When no feature meets the selection criteria, a warning is issued
and the tabular shapes come back empty: `"matrix"` and `"data.frame"`
have zero columns and keep the input row count, while the `"dt"` shape
(and `details$filtered`) is only guaranteed to have zero columns –
data.table represents a zero-column table as having zero rows, so its
row count is not preserved.

## Details

No target is involved, so this is the right tool for the first cleaning
pass – dropping constant or near-constant columns, columns that are
mostly missing, or columns with too few distinct values – and it is safe
to run before a train/test split, since nothing about the outcome
informs it. It says nothing about whether a feature is *useful*: a
high-variance column can be pure noise, and a low-variance one can be
the best predictor you have. Use
[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
or a model-based method for that judgment.

Supported methods:

- `"variance"`: Sample variance (denominator n - 1).

- `"mad"`: Median absolute deviation, computed with
  [`stats::mad()`](https://rdrr.io/r/stats/mad.html)'s default
  consistency constant 1.4826 (normal-consistent). Scores are therefore
  on the standard-deviation (sigma) estimate scale, not the raw
  median-absolute-deviation scale, and thresholds should be chosen
  accordingly.

- `"iqr"`: Interquartile range.

- `"range"`: Max - Min.

- `"missing_prop"`: Proportion of missing values, in `[0, 1]`. Note that
  with the default `threshold = 0`, `direction = "above"`, and
  `action = "keep"`, this KEEPS the features with the most missing
  values, which is almost never the intent; a warning suggests
  `action = "remove"` or `direction = "below"`. The warning fires only
  when all three of `threshold`, `direction`, and `action` are left at
  their defaults, so passing any one of them explicitly opts out of the
  advisory.

- `"n_unique"`: Number of unique non-NA values.

`na_rm` affects only the methods that summarize the observed values
(`"variance"`, `"mad"`, `"iqr"`, `"range"`); `"missing_prop"` and
`"n_unique"` always look at the whole column and treat NA as NA.

Features whose score is undefined (`NA`) are never selected, under both
`action = "keep"` and `action = "remove"`; a warning reports how many
such features were excluded.

Columns are subset by integer index, never by name, so duplicated column
names cannot select the wrong columns.

## Examples

``` r
df <- data.frame(
  spread = c(1, 2, 3, 4, 100),
  flat   = c(2, 2, 2, 2, 2),
  gappy  = c(1, NA, 3, NA, 5)
)

# Default: an fs_result
res <- fs_unsupervised(df, method = "variance", threshold = 0.5)
res$selected
#> [1] "spread" "gappy" 
res$scores
#> spread   flat  gappy 
#> 1902.5    0.0    4.0 
res$details$filtered
#>    spread gappy
#>     <num> <num>
#> 1:      1     1
#> 2:      2    NA
#> 3:      3     3
#> 4:      4    NA
#> 5:    100     5

# The classic shapes are still available
fs_unsupervised(df, method = "variance", threshold = 0.5,
                output = "matrix")
#>      spread gappy
#> [1,]      1     1
#> [2,]      2    NA
#> [3,]      3     3
#> [4,]      4    NA
#> [5,]    100     5

# Remove features with missing proportion >= 0.2
fs_unsupervised(df, method = "missing_prop", threshold = 0.2,
                direction = "above", action = "remove",
                include_equal = TRUE, output = "names")
#> [1] "spread" "flat"  
```
