# Print a featR result

Prints a compact summary of what a selection run kept.

## Usage

``` r
# S3 method for class 'fs_result'
print(x, n = 10L, ...)
```

## Arguments

- x:

  An `fs_result` object.

- n:

  Maximum number of features to list (default 10).

- ...:

  Unused, for consistency with the generic.

## Value

`x`, invisibly. Called for its printed output.

## Details

The output is at most five lines: a `<fs_result>` header naming the
method, followed by the task in parentheses when one is known; a count,
either "Selected k of N features" or, when the number of candidates
cannot be recovered, "Selected k features"; the selected names,
truncated to the first `n` with a "... (m more)" tail; a "Model:" line
giving the class of `x$model` when the method fitted one; and a
"Details:" line naming the elements of `x$details`. Nothing is printed
for the names when the selection is empty.

## Examples

``` r
fs_unsupervised(
  data.frame(a = c(1, 5, 2, 8), b = c(1, 1, 1, 1)),
  method = "variance", threshold = 0.5
)
#> <fs_result> unsupervised_variance
#> Selected 1 of 2 features
#>   a
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
```
