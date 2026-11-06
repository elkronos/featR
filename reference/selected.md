# Extract the selected features from a featR result

Returns the names of the features a selection function kept. It gives
the same value as `x$selected` and works on the result of any `fs_*()`
selection function. Code that calls `selected()` keeps working if the
internal layout of the result changes.

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

A character vector of selected feature names. When nothing met the
selection criteria it is `character(0)`, never `NULL`.

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
