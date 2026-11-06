# Summarize a featR result

Prints the result, the call that produced it, and a ranked score table.

## Usage

``` r
# S3 method for class 'fs_result'
summary(object, n = 20L, ...)
```

## Arguments

- object:

  An `fs_result` object.

- n:

  Maximum number of scored features to show (default 20).

- ...:

  Unused, for consistency with the generic.

## Value

`object`, invisibly. Called for its printed output.

## Details

Everything [`print()`](https://rdrr.io/r/base/print.html) shows, then
the recorded call, then – for methods that produce per-feature scores –
a table of every scored feature, ranked strongest first, with columns
`feature`, `score` (four significant digits) and `selected` (`*` marks
the features that were kept). The table is truncated to `n` rows with a
"... (m more)" tail.

The ranking direction follows the method. For most methods a larger
score means a stronger feature, and rows are ordered by decreasing
score, so a negative importance (a feature that did worse than its
permuted copy) ranks below every positive one. The exception is
[`fs_lasso`](https://elkronos.github.io/featR/reference/fs_lasso.md),
whose scores are signed standardized coefficients: those are ordered by
decreasing absolute value, since the sign is the direction of the
effect, not its strength. For methods that score by p-value, where
smaller is better, rows are ordered ascending instead, so the most
significant feature is listed first. The same ascending order is used
for the threshold filters
([`fs_unsupervised`](https://elkronos.github.io/featR/reference/fs_unsupervised.md),
[`fs_supervised`](https://elkronos.github.io/featR/reference/fs_supervised.md))
when they kept the low side of the threshold (`direction = "below"` with
`action = "keep"`, or `"above"` with `"remove"`), so the kept features
come first.

Two methods select features by something other than a cut on the score,
and for them the selected features are listed first (each group ordered
by score), so a kept feature is never ranked below, or truncated out
behind, a dropped one.
[`fs_mars`](https://elkronos.github.io/featR/reference/fs_mars.md)
selects every predictor the final model uses, which can have a zero or
negative GCV importance while pruned predictors score 0.
[`fs_correlation`](https://elkronos.github.io/featR/reference/fs_correlation.md)
with `prune = TRUE` scores each variable by its largest absolute
correlation with any other (larger = more redundant) and keeps one
member of each redundant pair; its rows are ordered ascending within
each group, least redundant first. With `prune = FALSE` the selection is
the redundant set, ordered by decreasing score.

The `selected` marks are matched by position when the result records a
per-feature keep-mask (`details$mask`, as the threshold filters do), so
features sharing a name are marked individually; otherwise by name.

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
