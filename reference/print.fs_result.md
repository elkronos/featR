# Print a featR result

Prints the method, how many features were kept, and their names.

## Usage

``` r
# S3 method for class 'fs_result'
print(x, n = 10L, ...)
```

## Arguments

- x:

  An `fs_result` object.

- n:

  Maximum number of selected names to list. Default 10.

- ...:

  Unused, for consistency with the generic.

## Value

`x`, invisibly. Called for the printed output.

## Details

The output has at most five lines:

1.  A `<fs_result>` header naming the method, followed by the task in
    parentheses when one is known.

2.  "Selected k of N features", or "Selected k features" when the number
    of candidates cannot be recovered from the result.

3.  The selected names, truncated to the first `n` with a "... (m more)"
    tail. This line is left out when the selection is empty.

4.  A "Model:" line giving the class of `x$model`, when the method
    fitted one.

5.  A "Details:" line naming the elements of `x$details`.

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
