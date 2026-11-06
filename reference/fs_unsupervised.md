# Unsupervised filter that scores each feature without a target

Scores each column of `data` with a summary that ignores any outcome
(variance, spread, missingness, or number of distinct values) and keeps
or drops each column by comparing its score with `threshold`.

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

  A data.frame, data.table, or matrix whose columns are all numeric.
  Every column is a candidate feature. The input is copied, never
  modified in place.

- method:

  One of `"variance"` (default), `"mad"`, `"iqr"`, `"range"`,
  `"missing_prop"`, or `"n_unique"`. See Details.

- threshold:

  Non-negative, finite number compared with each feature's score.
  Default 0. Each method has its own scale (a variance is in squared
  units, `"missing_prop"` lies in `[0, 1]`, `"n_unique"` is a count), so
  a threshold chosen for one method does not carry over to another.

- direction:

  `"above"` (default) or `"below"`: which side of `threshold` a score
  must fall on to meet the condition.

- action:

  `"keep"` (default) retains the features that meet the condition;
  `"remove"` drops them.

- include_equal:

  Logical. If `TRUE`, a score equal to `threshold` meets the condition.
  Default `FALSE`.

- na_rm:

  Logical. If `TRUE` (the default), `NA`s are removed before computing
  `"variance"`, `"mad"`, `"iqr"`, and `"range"`; if `FALSE`, an `NA` in
  a column makes its score `NA` under those methods. `"missing_prop"`
  and `"n_unique"` ignore `na_rm`.

- output:

  Shape of the return value.

  - `"result"` (default): an `fs_result` object (see Value).

  - `"matrix"`, `"dt"`, `"data.frame"`: the selected features as a
    numeric matrix, a data.table, or a data.frame.

  - `"mask"`: logical vector of length `ncol(data)`, named after the
    columns of `data`; `TRUE` marks a selected column.

  - `"indices"`: integer positions of the selected columns, named after
    them.

  - `"names"`: character vector of the selected column names.

  - `"list"`: a list with `filtered` (matrix), `mask`, `indices`,
    `names`, `scores`, and `meta`. `meta` records `method`, `threshold`,
    `direction`, `action`, `include_equal`, `na_rm`, `n_input_cols`, and
    `n_kept_cols`.

- verbose:

  Logical; print progress messages. Default `FALSE`.

## Value

With `output = "result"` (the default), an `fs_result` object:

- `selected`: character vector of selected feature names.

- `scores`: named numeric vector with one score per column, in column
  order, `NA` where the score is undefined.

- `method`: `"unsupervised_"` followed by the method, for example
  `"unsupervised_variance"`.

- `task`: `NA_character_`, since there is no target.

- `model`: `NULL`.

- `details`: a list holding, in this order, `mask` (the logical
  keep-mask over the columns), `indices` (selected positions, named),
  `filtered` (the selected columns as a data.table), `threshold`,
  `direction`, `action`, and `n_features` (`ncol(data)`).

- `call`: the matched call.

Any other `output` returns the shape described under `output`.

When no feature meets the criteria, a warning is issued and the tabular
shapes come back empty. `"matrix"` and `"data.frame"` then have zero
columns and keep the input row count. The `"dt"` shape and
`details$filtered` have zero columns and also zero rows, because
data.table represents a zero-column table that way.

## Details

Use it for the first cleaning pass: dropping constant or near-constant
columns, columns that are mostly missing, or columns with too few
distinct values. No outcome informs the scores, so it is safe to run
before a train/test split. For the same reason it says nothing about
whether a feature is useful. A high-variance column can be pure noise
and a low-variance one can be your best predictor;
[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
or a model-based method answers that question.

The six values of `method`:

- `"variance"` (default): sample variance (denominator n - 1), in
  squared units. For values below about `1e-162` in magnitude the
  variance underflows to 0, so to find constant columns in extreme units
  use `"range"` with threshold 0 or `"n_unique"` with threshold 1.

- `"mad"`: median absolute deviation from
  [`stats::mad()`](https://rdrr.io/r/stats/mad.html), which multiplies
  by the consistency constant 1.4826. Scores are on the scale of a
  standard-deviation estimate, not the raw median absolute deviation;
  choose thresholds on that scale.

- `"iqr"`: interquartile range.

- `"range"`: maximum minus minimum.

- `"missing_prop"`: proportion of missing values, in `[0, 1]`. With the
  default `threshold = 0`, `direction = "above"`, and `action = "keep"`,
  this keeps the columns that have any missing values and drops the
  complete ones, which is almost never what you want. A warning then
  suggests `action = "remove"` or `direction = "below"`. The warning
  fires only when all three of `threshold`, `direction`, and `action`
  are left at their defaults; passing any one of them explicitly
  silences it.

- `"n_unique"`: number of distinct non-`NA` values.

A feature whose score is `NA` is never selected, whether `action` is
`"keep"` or `"remove"`, and a warning reports how many features were
excluded this way.

Columns are subset by position, never by name, so duplicated column
names cannot pull in the wrong column. Every column needs a non-empty,
non-`NA` name. A matrix keeps its own column names; `V1`, `V2`, ... are
used only when it has none.

## See also

The [Filter
methods](https://elkronos.github.io/featR/articles/filters.html) guide,
which runs this function on a worked example.

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
