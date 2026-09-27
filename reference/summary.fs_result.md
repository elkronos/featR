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
