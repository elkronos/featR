# Extract the selected features from a featR result

The generic accessor for the one thing every featR method produces: the
names of the features it kept. Equivalent to `x$selected`, but stable
against future changes in the internal layout, and safe to call on the
result of any `fs_*()` selection function.

## Usage

``` r
selected(x, ...)

# S3 method for class 'fs_result'
selected(x, ...)
```

## Arguments

- x:

  An `fs_result` object.

- ...:

  Unused, for future methods.

## Value

A character vector of selected feature names, possibly empty when
nothing met the selection criteria.

## Examples

``` r
res <- fs_unsupervised(
  data.frame(a = c(1, 5, 2, 8), b = c(1, 1, 1, 1)),
  method = "variance", threshold = 0.5
)
selected(res)
#> [1] "a"

# Empty selections are returned as character(0), not NULL
none <- suppressWarnings(
  fs_unsupervised(
    data.frame(a = c(1, 5, 2, 8), b = c(1, 1, 1, 1)),
    method = "variance", threshold = 1e6
  )
)
selected(none)
#> character(0)
```
