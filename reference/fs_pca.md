# Principal component analysis with tidy results and optional plotting

Runs a principal component analysis (PCA) on the numeric columns of
`data` and returns loadings, scores, and the share of variance each
retained component explains. PCA finds the directions of greatest spread
in the data; use it to see how much of that spread a few components
carry and which columns drive them.

## Usage

``` r
fs_pca(
  data,
  num_pc = NULL,
  scale_data = TRUE,
  center_data = TRUE,
  label_col = NULL,
  plot = FALSE,
  verbose = FALSE
)
```

## Arguments

- data:

  A data.frame or data.table with at least two rows and at least one
  numeric column. Column names must be unique and non-empty. A
  non-numeric (label) column named like a retained score column (`PC1`,
  `PC2`, ...) would be hidden by the scores in `pca_df`, so that is an
  error. Columns in extreme units (beyond about `1e154` or below
  `1e-154`) are kept, because the zero-variance check is relative to
  each column's magnitude; with `scale_data = FALSE` their variance
  cannot be represented and they are an error.

- num_pc:

  Number of principal components to retain: a whole number `>= 1`, or
  `NULL` (the default) for `min(2, max_possible)`. `max_possible` is
  `min(nrow - 1, ncol)` of the usable numeric data, after incomplete
  rows and zero-variance columns are dropped. A larger explicit value is
  an error.

- scale_data:

  Logical; scale numeric columns to unit variance. Default `TRUE`.
  `scale_data = TRUE` with `center_data = FALSE` is an error, because
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) and
  [`bigstatsr::big_scale()`](https://privefl.github.io/bigstatsr/reference/big_scale.html)
  handle uncentered scaling differently (the latter ignores it), and the
  result would depend on which engine the data size selected.

- center_data:

  Logical; center numeric columns. Default `TRUE`.

- label_col:

  Optional single string naming a column of `data` used to label and
  color points. It is left out of the PCA features and carried into
  `pca_df`. Default `NULL`: no labels, and therefore no plot.

- plot:

  Logical; if `TRUE`, print the PC1 vs PC2 scatterplot. Default `FALSE`,
  whether or not `label_col` is supplied. The plot object is returned in
  `$plot` either way whenever one can be built (see Details).

- verbose:

  Logical; print progress messages naming the non-numeric columns held
  back as labels, the engine chosen, and any fallback from 'bigstatsr'.
  Default `FALSE`.

## Value

A plain list, not an `fs_result`: `fs_pca()` reduces dimensions rather
than selecting features, so it has no `selected` set to report. The
components are:

- `pc_loadings`: numeric matrix of variable loadings, one row per
  retained numeric feature (named) and one column per retained component
  (`PC1`, `PC2`, ...).

- `pc_scores`: numeric matrix of observation scores, one row per kept
  observation and one column per retained component.

- `var_explained`: numeric vector of length `num_pc`, the proportion of
  total variance carried by each retained component (see Details).

- `pca_df`: data.table of the scores with the label columns (every
  non-numeric column, plus `label_col`) bound alongside, restricted to
  the kept rows.

- `meta`: list with `numeric_cols` (the features actually decomposed),
  `rows_kept` (logical vector over the rows of `data`), `n_rows_used`,
  and `n_cols_used`.

- `plot`: the ggplot object, present only when a plot could be built (a
  `label_col` was supplied, at least two components were retained, and
  'ggplot2' is installed). Test for it with `"plot" %in% names(res)`.

## Details

PCA is unsupervised dimensionality reduction, not feature selection.
Every component is a linear combination of all the retained numeric
columns, chosen without reference to any outcome. A PCA therefore does
not shorten the list of variables you have to measure, and the leading
components are the highest-variance directions, which need not be the
ones related to a response.

Before the decomposition:

- Non-numeric columns (character, factor, logical, Date, ...) are left
  out of the features and carried along as labels. An explicit
  `label_col`, numeric or not, is also left out of the features.

- Rows with a missing value in any numeric feature column are dropped.
  An infinite value in a feature column is an error.

- Zero-variance columns are dropped with a warning. A column counts as
  zero-variance when its standard deviation is at most
  `1000 * .Machine$double.eps` (about 2e-13) times its largest absolute
  value. A column that is constant up to floating-point round-off is
  therefore dropped, while small-unit or large-offset columns
  (timestamps, say) are kept.

Data with fewer than 1e7 cells is decomposed with
[`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html). From 1e7 cells
up, if the suggested package 'bigstatsr' is installed,
[`bigstatsr::big_SVD()`](https://privefl.github.io/bigstatsr/reference/big_SVD.html)
runs on a temporary file-backed matrix (the backing file is deleted when
the call returns) and can compute at most one fewer component than there
are usable columns. Without 'bigstatsr' the call falls back to
[`prcomp()`](https://rdrr.io/r/stats/prcomp.html), and says so only when
`verbose = TRUE`.

`var_explained` is the proportion of total variance explained by each
retained component under both engines. It sums to 1 only when every
available component is retained; with the default two components it sums
to the fraction those two capture.
[`prcomp()`](https://rdrr.io/r/stats/prcomp.html) computes every
component, so the total is the sum of all eigenvalues. `big_SVD()`
computes only the top `num_pc` singular values, so the total is
recovered separately from the column statistics of the (implicitly
centered and scaled) matrix. Under either engine, read the entries as
shares of the whole, never as shares of the components that happened to
be computed.

`plot` controls printing only, never construction. Whenever a plot can
be built (a `label_col` was supplied, at least two components were
retained, and the suggested package 'ggplot2' is installed), the ggplot
object is returned in `$plot`, whatever the value of `plot`.
`plot = TRUE` also prints it and makes 'ggplot2' required: its absence
is then an error rather than a missing `$plot`. `plot = TRUE` without a
`label_col`, or with fewer than two retained components, warns and skips
the plot.

## See also

The [Dimensionality
reduction](https://elkronos.github.io/featR/articles/dimensionality-reduction.html)
guide, which runs this function on a worked example.

## Examples

``` r
res <- fs_pca(mtcars, num_pc = 2, label_col = "cyl")
res$var_explained
#> [1] 0.5697822 0.2644102
head(res$pca_df)
#>            PC1        PC2   cyl
#>          <num>      <num> <num>
#> 1: -0.68400200 -1.6918186     6
#> 2: -0.64964186 -1.5108421     6
#> 3: -2.45446503  0.1707165     4
#> 4: -0.25539550  2.3333568     6
#> 5:  1.68827810  0.7361403     8
#> 6:  0.02536833  2.7401361     6

if (requireNamespace("ggplot2", quietly = TRUE)) {
  # plot = FALSE (the default) draws nothing, but $plot is built anyway
  res <- fs_pca(iris, label_col = "Species", verbose = TRUE)
  print(inherits(res$plot, "ggplot"))

  # plot = TRUE draws it as well
  res <- fs_pca(iris, label_col = "Species", plot = TRUE)
}
#> Excluding 1 non-numeric column(s) from the PCA features (kept as label candidates): Species
#> Using prcomp for PCA computation.
#> [1] TRUE
```
