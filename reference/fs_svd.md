# Singular Value Decomposition with Optional Scaling and Truncation

Computes the SVD of a matrix with options for centering/scaling,
truncation to the leading singular triplets, and an approximate solver
for large matrices.

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
  coerced to a matrix). Must contain no NA/NaN/Inf values. The argument
  is called `x` rather than `data` because it is a matrix of values, not
  a table of observations and features.

- n_singular_values:

  Positive whole number of singular values and vectors to keep, or NULL
  (default) for `min(dim(x))`. Values exceeding `min(dim(x))` are an
  error.

- scale_input:

  TRUE (center and scale, the default), FALSE (leave the matrix alone),
  `"center"` (subtract column means only), or `"scale"` (divide only).
  Any other value is an error. The two dividing forms (`TRUE` and
  `"scale"`) require every column to have non-zero variance (a standard
  deviation above `1000 * .Machine$double.eps`, about 2e-13, times the
  column's largest absolute value, so the check is unit-free), and
  offending columns are reported by name (or by position when the matrix
  has no column names); `"center"` has no such requirement. Note that
  `"scale"` inherits
  [`base::scale()`](https://rdrr.io/r/base/scale.html)'s semantics: with
  no centering it divides by the root mean square, not by the standard
  deviation.

- svd_method:

  `"auto"` (default), `"exact"`, or `"approx"`. See Details for how
  `"auto"` decides and when `"approx"` falls back to the exact solver.

- svd_threshold:

  Positive number; with `svd_method = "auto"`, the approximate solver is
  considered only when `min(dim(x))` exceeds this value (default 100).

- approx_args:

  List of extra arguments passed to
  [`RSpectra::svds()`](https://rdrr.io/pkg/RSpectra/man/svds.html) (for
  example `tol` or `opts`). Must not include `A` or `k`, which are set
  internally.

- verbose:

  Logical; emit progress messages. Default FALSE.

## Value

A plain list. `fs_svd()` is dimensionality reduction rather than feature
selection, so it returns its own decomposition structure and not the
`fs_result` object produced by the package's selection functions.
Writing `k` for the number of triplets kept (`n_singular_values`, or
`min(dim(x))` by default), the components are:

- `singular_values`: numeric vector of length `k`, in non-increasing
  order.

- `left_singular_vectors`: `nrow(x)` by `k` matrix (the `u` of
  [`base::svd()`](https://rdrr.io/r/base/svd.html)).

- `right_singular_vectors`: `ncol(x)` by `k` matrix (the `v` of
  [`base::svd()`](https://rdrr.io/r/base/svd.html)).

Singular vectors are unique only up to sign (and up to rotation within a
tied block), so column signs may differ between the exact and
approximate solvers and between platforms.

## Details

Reach for this when you want a low-rank summary of a numeric matrix –
the leading components for compression, denoising, or a latent-factor
representation – rather than a subset of the original columns. Because
the components are linear combinations of every input column, nothing is
dropped and individual features stay uninterpretable; use one of the
package's selection functions when you need to name the features you
keep. [`fs_pca`](https://elkronos.github.io/featR/reference/fs_pca.md)
covers the same ground with tidy, labeled output and variance-explained
figures; `fs_svd()` is the lower-level decomposition.

The exact path uses [`base::svd()`](https://rdrr.io/r/base/svd.html).
The approximate path uses
[`RSpectra::svds()`](https://rdrr.io/pkg/RSpectra/man/svds.html)
(package RSpectra, a Suggests dependency, required only when that path
is actually taken) and is only applicable when fewer than `min(dim(x))`
singular values are requested. By default `n_singular_values = NULL`
resolves to `min(dim(x))`, which implies the exact path; to enable the
approximate solver on a large matrix, request
`n_singular_values < min(dim(x))`.

With `svd_method = "auto"`, the approximate solver is chosen only when
`n_singular_values < min(dim(x))` and `min(dim(x)) > svd_threshold`;
otherwise the exact solver is used, and a message explains why when a
large matrix still ends up on the exact path because all singular values
were requested. Asking for `svd_method = "approx"` outright when
`n_singular_values` equals `min(dim(x))` is not an error: it falls back
to the exact solver with a message, because
[`RSpectra::svds()`](https://rdrr.io/pkg/RSpectra/man/svds.html) cannot
return the full set. If
[`RSpectra::svds()`](https://rdrr.io/pkg/RSpectra/man/svds.html)
converges on fewer than the requested number of singular values, the
call warns and recomputes them with the exact solver.

Centering and scaling, when requested, happen *before* the
decomposition, so the returned triplets factorize the transformed matrix
rather than the raw input: with the default `scale_input = TRUE` they
reconstruct `scale(x)`, not `x`. The transformation itself is not
returned, so keep the column means and standard deviations yourself if
you need to map results back to the original units.

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
