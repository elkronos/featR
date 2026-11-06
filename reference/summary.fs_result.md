# Summarize a featR result

Prints the result, the call that produced it, and a table of scores
ranked strongest first.

## Usage

``` r
# S3 method for class 'fs_result'
summary(object, n = 20L, ...)
```

## Arguments

- object:

  An `fs_result` object.

- n:

  Maximum number of rows in the score table. Default 20.

- ...:

  Unused, for consistency with the generic.

## Value

`object`, invisibly. Called for the printed output.

## Details

The output starts with everything
[`print()`](https://rdrr.io/r/base/print.html) shows, then the recorded
call. For methods that produce per-feature scores it ends with a table
of every scored feature, with columns `feature`, `score` (rounded to
four significant digits), and `selected` (`*` marks the kept features).
The table is truncated to `n` rows with a "... (m more)" tail.

## How rows are ranked

For most methods a larger score means a stronger feature, so rows are in
decreasing order of score. A negative importance (a feature that did
worse than its permuted copy) therefore ranks below every positive one.
The other rules are:

- [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  scores are signed standardized coefficients. The sign gives the
  direction of the effect, not its strength, so rows are in decreasing
  order of absolute value.

- [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md)
  scores are adjusted p-values, where smaller is stronger, so rows are
  in increasing order and the most significant feature comes first.

- [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
  and
  [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
  also use increasing order when they kept the low side of the threshold
  (`direction = "below"` with `action = "keep"`, or `"above"` with
  `"remove"`), so the kept features come first.

Two methods select by something other than a cut on the score. For them
the selected features are listed first, each group ordered by score, so
a kept feature is never ranked below, or truncated out behind, a dropped
one:

- [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
  selects every predictor the final earth model uses. A used predictor
  can have a zero or negative GCV importance while pruned predictors
  score 0.

- [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
  with `prune = TRUE` scores each variable by its largest absolute
  correlation with any other (larger means more redundant) and keeps one
  member of each redundant pair. Rows are in increasing order within
  each group, least redundant first. With `prune = FALSE` the selection
  is the redundant set itself, and rows are in decreasing order of
  score.

When the result records a per-feature keep-mask (`details$mask`, as the
threshold filters do), the `selected` marks are matched by position, so
two features sharing a name are marked separately. Otherwise they are
matched by name.

## Examples

``` r
res <- fs_unsupervised(
  data.frame(a = c(1, 5, 2, 8), b = c(1, 1, 1, 1)),
  method = "variance", threshold = 0.5
)
summary(res)
#> <fs_result> unsupervised_variance
#> Selected 1 of 2 features
#>   a
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
#> 
#> Call:
#>   fs_unsupervised(data = data.frame(a = c(1, 5, 2, 8), b = c(1, 
#>     1, 1, 1)), method = "variance", threshold = 0.5)
#> 
#> Scores (2 features, ranked):
#>  feature score selected
#>        a    10        *
#>        b     0         
#> (* = selected)
```
