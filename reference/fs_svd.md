# Singular value decomposition with optional scaling and truncation

Computes the singular value decomposition (SVD) of a numeric matrix,
optionally centering and scaling it first and keeping only the leading
singular values and vectors. The SVD factors a matrix into
`u %*% diag(d) %*% t(v)`; the leading terms give the best low-rank
approximation of the matrix in the least-squares sense.

## Usage

``` r
fs_svd(
  x,
  n_singular_values = NULL,
  scale_input = TRUE,
  svd_method = c("auto", "exact", "approx"),
  svd_threshold = 100,
  approx_args = list(),
  verbose = FALSE
)
```

## Arguments

- x:

  Numeric matrix, or a data.frame whose columns are all numeric (it is
  coerced to a matrix). It must contain no `NA`, `NaN`, or `Inf` values.
  The argument is called `x` rather than `data` because it is a matrix
  of values, not a table of observations and features.

- n_singular_values:

  Positive whole number of singular values and vectors to keep, or
  `NULL` (default) for `min(dim(x))`. A value above `min(dim(x))` is an
  error.

- scale_input:

  How to transform the columns before decomposing: `TRUE` (default)
  centers and scales, `FALSE` leaves the matrix alone, `"center"`
  subtracts column means only, and `"scale"` divides only. Any other
  value is an error. The two dividing forms (`TRUE` and `"scale"`) need
  every column to have non-zero variance, meaning a standard deviation
  above `1000 * .Machine$double.eps` (about 2e-13) times the column's
  largest absolute value, so the check does not depend on units.
  Offending columns are named in the error, or numbered when the matrix
  has no column names. `"scale"` follows
  [`base::scale()`](https://rdrr.io/r/base/scale.html): without
  centering it divides by the root mean square, not the standard
  deviation.

- svd_method:

  `"auto"` (default), `"exact"`, or `"approx"`. See Details for how
  `"auto"` decides and when `"approx"` falls back to the exact solver.

- svd_threshold:

  Positive number. With `svd_method = "auto"`, the approximate solver is
  considered only when `min(dim(x))` exceeds it. Default 100.

- approx_args:

  List of extra arguments passed to
  [`RSpectra::svds()`](https://rdrr.io/pkg/RSpectra/man/svds.html), for
  example `tol` or `opts`. It must not include `A` or `k`, which are set
  from `x` and `n_singular_values`.

- verbose:

  Logical; print progress messages. Default `FALSE`.

## Value

A plain list, not an `fs_result`: `fs_svd()` reduces dimensions rather
than selecting features. Writing `k` for the number kept
(`n_singular_values`, or `min(dim(x))` by default), the components are:

- `singular_values`: numeric vector of length `k`, in non-increasing
  order.

- `left_singular_vectors`: `nrow(x)` by `k` matrix (the `u` of
  [`base::svd()`](https://rdrr.io/r/base/svd.html)).

- `right_singular_vectors`: `ncol(x)` by `k` matrix (the `v` of
  [`base::svd()`](https://rdrr.io/r/base/svd.html)).

Singular vectors are unique only up to sign (and up to rotation within a
block of tied singular values), so column signs can differ between the
exact and approximate solvers and between platforms.

## Details

Use it for a low-rank summary of a numeric matrix: compression,
denoising, or a latent-factor representation. The components are linear
combinations of every input column, so nothing is dropped and the
components cannot be read as individual features; use one of featR's
selection functions when you need to name the features you keep.
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md)
covers the same ground with labeled output and variance-explained
figures; `fs_svd()` is the lower-level decomposition.

The exact solver is [`base::svd()`](https://rdrr.io/r/base/svd.html).
The approximate solver is
[`RSpectra::svds()`](https://rdrr.io/pkg/RSpectra/man/svds.html) (from
RSpectra, a Suggests dependency needed only when that solver runs), and
it can only return fewer than `min(dim(x))` singular values. The default
`n_singular_values = NULL` asks for all `min(dim(x))` of them and so
always takes the exact path; request fewer to make the approximate
solver available.

With `svd_method = "auto"`, the approximate solver runs only when
`n_singular_values < min(dim(x))` and `min(dim(x)) > svd_threshold`.
Otherwise the exact solver runs, and a message explains why when a large
matrix ends up on the exact path because all singular values were
requested. `svd_method = "approx"` with `n_singular_values` equal to
`min(dim(x))` is not an error: it falls back to the exact solver with a
message. If
[`RSpectra::svds()`](https://rdrr.io/pkg/RSpectra/man/svds.html)
converges on fewer singular values than requested, the call warns and
recomputes them with the exact solver.

Centering and scaling happen before the decomposition, so the returned
values and vectors factorize the transformed matrix. With the default
`scale_input = TRUE` they reconstruct `scale(x)`, not `x`. The column
means and standard deviations are not returned; keep them yourself if
you need to map results back to the original units.

## See also

The [Dimensionality
reduction](https://elkronos.github.io/featR/articles/dimensionality-reduction.html)
guide, which runs this function on a worked example.

## Examples

``` r
m <- matrix(
  c(4, 0, 0, 3,
    0, 5, 1, 2,
    2, 1, 6, 0,
    1, 3, 2, 7,
    5, 2, 0, 1,
    0, 4, 3, 2),
  nrow = 6, ncol = 4, byrow = TRUE
)

# Two leading components of the centered and scaled matrix
res <- fs_svd(m, n_singular_values = 2, scale_input = TRUE)
res$singular_values
#> [1] 3.075623 2.574852
res$left_singular_vectors
#>            [,1]       [,2]
#> [1,] -0.5433794 -0.2618208
#> [2,]  0.4467209 -0.1147094
#> [3,] -0.1104811  0.8100647
#> [4,]  0.2986167 -0.4626424
#> [5,] -0.4927382 -0.1397594
#> [6,]  0.4012610  0.1688673
res$right_singular_vectors
#>            [,1]       [,2]
#> [1,] -0.7067452 -0.1089774
#> [2,]  0.6323538 -0.1568284
#> [3,]  0.2259715  0.7369407
#> [4,]  0.2226583 -0.6484190

# Share of the total (scaled) variance captured by those two components
all_values <- fs_svd(m, scale_input = TRUE)$singular_values
sum(res$singular_values^2) / sum(all_values^2)
#> [1] 0.804466

# Untransformed, the full decomposition reconstructs the input exactly
full <- fs_svd(m, scale_input = FALSE)
recon <- full$left_singular_vectors %*%
  (full$singular_values * t(full$right_singular_vectors))
all.equal(recon, m)
#> [1] TRUE
```
