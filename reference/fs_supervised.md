# Supervised filter that scores each feature against a target

Scores every column of `data` except `target` against the target, one
feature at a time, and keeps or drops each feature by comparing its
score with `threshold`. A numeric target is scored by absolute Pearson
correlation, a categorical target by the one-way ANOVA F statistic.

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

  A data.frame, data.table, or matrix holding `target` and the candidate
  features. Every column other than `target` must be numeric. The input
  is copied, never modified in place.

- target:

  Single string naming the target column of `data`. The target may be
  numeric, factor, character, or logical; any other type is an error. It
  is removed from the candidates and used only for scoring.

- method:

  One of `"auto"` (default), `"correlation"`, or `"anova"`. See Details.

- threshold:

  Non-negative, finite number compared with each feature's score.
  Default 0. The scale depends on the method: `[0, 1]` for
  `"correlation"`, `[0, Inf]` for `"anova"`. With the defaults
  (`threshold = 0`, `direction = "above"`, `action = "keep"`) nothing is
  filtered: any non-constant feature, pure noise included, has a sample
  \\\|r\| \> 0\\ or \\F \> 0\\, so every feature with a defined score is
  kept. Set a threshold to make a real selection.

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

  Logical. If `TRUE` (the default), rows with `NA` in a feature or in
  the target are left out when that feature is scored. If `FALSE`, an
  `NA` anywhere in a feature or in the target makes that feature's score
  `NA`.

- output:

  Shape of the return value. "Candidate features" means the columns of
  `data` other than `target`, in their original order.

  - `"result"` (default): an `fs_result` object (see Value).

  - `"matrix"`, `"dt"`, `"data.frame"`: the selected features as a
    numeric matrix, a data.table, or a data.frame.

  - `"mask"`: logical vector with one element per candidate feature,
    named after the candidates; `TRUE` marks a selected feature.

  - `"indices"`: integer positions of the selected features among the
    candidates (that is, in `data` without `target`), named after the
    selected features.

  - `"names"`: character vector of the selected feature names.

  - `"list"`: a list with `filtered` (matrix), `mask`, `indices`,
    `names`, `scores`, and `meta`. `meta` records `method_arg` (the
    method as requested), `method_used` (after `"auto"` is resolved),
    `threshold`, `direction`, `action`, `include_equal`, `na_rm`,
    `n_input_cols`, and `n_kept_cols`.

- verbose:

  Logical; print progress messages. Default `FALSE`.

## Value

With `output = "result"` (the default), an `fs_result` object:

- `selected`: character vector of selected feature names.

- `scores`: named numeric vector with one score per candidate feature,
  in column order, `NA` where the score is undefined.

- `method`: `"supervised_"` followed by the method actually used, for
  example `"supervised_correlation"`.

- `task`: `"regression"` for a numeric target, `"classification"` for a
  factor target, and `NA` for a character or logical target, whose
  intended task is ambiguous.

- `model`: `NULL`.

- `details`: a list holding, in this order, `mask` (the logical
  keep-mask over the candidates), `indices` (selected positions, named),
  `filtered` (the selected columns as a data.table), `threshold`,
  `direction`, `action`, and `n_features` (the number of candidates,
  `ncol(data) - 1`).

- `call`: the matched call.

Any other `output` returns the shape described under `output`.

When no feature meets the criteria, a warning is issued and the tabular
shapes come back empty. `"matrix"` and `"data.frame"` then have zero
columns and keep the input row count. The `"dt"` shape and
`details$filtered` have zero columns and also zero rows, because
data.table represents a zero-column table that way.

## Details

No model is fitted, so the cost grows linearly with the number of
columns and the function copes with wide data. Each feature is scored on
its own, which has two consequences: the filter cannot see interactions,
and it keeps every member of a group of near-duplicate columns that all
track the target. Prune that redundancy afterwards with
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md),
or use a wrapper such as
[`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
or [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
when the joint contribution of features is what you care about.

The three values of `method`:

- `"correlation"` requires a numeric target. The score is the absolute
  Pearson correlation, in `[0, 1]`. A constant feature or a constant
  target has no defined correlation and scores `NA`. The constancy check
  is relative to each column's magnitude, so a feature measured on a
  very small or very large scale is scored normally.

- `"anova"` requires a factor, character, or logical target. The score
  is the one-way ANOVA F statistic, in `[0, Inf]`. A feature that
  separates the classes perfectly (no spread within any class) scores
  `Inf`, and a constant feature scores `NA`.

- `"auto"` (the default) uses `"correlation"` for a numeric target and
  `"anova"` otherwise. The two scales differ, so a threshold that suits
  one is meaningless for the other; with `verbose = TRUE` a message
  reports which method was chosen.

A feature whose score is `NA` is never selected, whether `action` is
`"keep"` or `"remove"`, and a warning reports how many features were
excluded this way.

Columns are subset by position, never by name, so duplicated feature
names cannot pull in the wrong column. The target's name must be unique,
and every column needs a non-empty, non-`NA` name. A matrix keeps its
own column names; `V1`, `V2`, ... are used only when it has none.

## See also

The [Filter
methods](https://elkronos.github.io/featR/articles/filters.html) guide,
which runs this function on a worked example.

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
