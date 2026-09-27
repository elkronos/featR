# Elastic Net Feature Selection and Model Training

Performs feature selection and model training using elastic net
regularization via `caret::train(method = "glmnet")`. Supports
regression (numeric outcomes) and classification (factor/character
outcomes).

## Usage

``` r
fs_elastic(
  data,
  target,
  alpha_seq = seq(0.1, 1, by = 0.1),
  lambda_seq = NULL,
  trControl = NULL,
  metric = NULL,
  use_pca = FALSE,
  nPCs = NULL,
  seed = NULL,
  verbose = FALSE,
  n_cores = 1L
)
```

## Arguments

- data:

  A data frame (or data.table) containing the target and the candidate
  predictors.

- target:

  Single string naming the outcome column in `data`.

- alpha_seq:

  Numeric vector of alpha values to tune over, each in `[0, 1]`. Default
  `seq(0.1, 1, by = 0.1)`; 0 (ridge) is left out because it cannot zero
  a coefficient (see Details).

- lambda_seq:

  Numeric vector of non-negative lambda values to tune over, or `NULL`
  (default) to use glmnet's own path per alpha.

- trControl:

  Optional
  [`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html)
  object. If `NULL` (default), 5-fold CV with an NA-safe summary
  function is used. When `use_pca = TRUE`, `pcaComp = nPCs` is injected
  into its `preProcOptions`.

- metric:

  Optional character. Performance metric to optimize. If `NULL`
  (default), `"RMSE"` is used for regression and `"Accuracy"` for
  classification.

- use_pca:

  Logical. Whether to project predictors onto principal components
  inside each resample. Default `FALSE`.

- nPCs:

  Integer \>= 1. Number of principal components to retain when
  `use_pca = TRUE`. Must be strictly less than `min(nrow, ncol)` of the
  predictor matrix. Default `NULL`, which is an error when
  `use_pca = TRUE` and ignored otherwise.

- seed:

  Optional integer seed applied locally (and restored on exit) before
  resampling and tuning. Default `NULL` (never seeds by default).

- verbose:

  Logical. Print progress messages. Default `FALSE`.

- n_cores:

  Integer \>= 1. Number of workers for parallel training. Default `1`
  (sequential; no cluster is created). Values above the detected core
  count are capped.

## Value

An `fs_result` object with:

- selected:

  Predictors (or components, when `use_pca = TRUE`) whose coefficient at
  the chosen alpha/lambda is non-zero; for multinomial fits, the union
  across classes.

- scores:

  Named numeric vector of absolute coefficients at the chosen
  alpha/lambda, one entry per column the model saw (predictors shrunk to
  zero are kept, with a score of 0); `NULL` for multinomial fits, where
  a predictor has one coefficient per class and no single score exists.

- method:

  `"elastic_net"`.

- task:

  `"regression"` or `"classification"`.

- model:

  The [`caret::train`](https://rdrr.io/pkg/caret/man/train.html) object.

- details:

  List of `coef` (coefficients at the best lambda, intercept included: a
  sparse matrix, or a list of them for multinomial fits), `best_alpha`
  and `best_lambda` (the winning tuning pair), `metric_name` (the metric
  optimized), `metric_value` (its resampled value for that pair, `NA`
  when the metric is absent from caret's results table), `use_pca`, and
  `n_features` (number of columns the model saw, i.e. `nPCs` when
  `use_pca = TRUE`).

- call:

  The matched call.

## Details

Use this when the question is "which predictors survive a jointly tuned
L1/L2 penalty?". Both alpha and lambda are chosen by resampling, and
every predictor with a non-zero coefficient at the winning pair is
reported. `alpha = 0` is pure ridge, which never sets a coefficient to
exactly zero, so it cannot select: the default `alpha_seq` therefore
starts at 0.1, and if you include 0 yourself and it wins, every
predictor is "selected" and a warning says so. When the winning alpha is
below 1 the ridge component spreads weight across correlated predictors,
so a group of collinear columns tends to survive together instead of
being reduced to a single representative.

The model formula is built internally from `data` and `target`: every
other column of `data` is a candidate predictor, and non-syntactic names
are backticked. Predictors then go through
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) and
the intercept column is removed, so a k-level factor or character column
contributes k - 1 dummy columns and `selected`, `scores` and
`details$coef` name design-matrix columns rather than the original
columns. Non-syntactic names are reported as the caller wrote them,
without the backticks
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) adds.
`glmnet` needs at least two design-matrix columns, so a single numeric
predictor is an error.

Rows with a missing response, or a missing value in any predictor, are
dropped before fitting (an error if that leaves nothing), and a constant
predictor column is a hard error rather than a silently degenerate fit.
A logical response is converted to a two-level factor with a message; a
numeric response with only two distinct values is still treated as
regression, with a warning telling you to convert it to a factor if you
meant classification.

`scores` are absolute coefficients on the scale of the columns the model
saw, not standardized ones, so they rank predictors fairly only when
those columns are on comparable scales – unlike
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md),
this function does not rescale them for you.

When `use_pca = TRUE` the PCA is **not** fitted up front. `caret` is
asked for `preProcess = c("center", "scale", "pca")` with
`pcaComp = nPCs` in `trControl$preProcOptions`, so centering, scaling
and the component loadings are refit on the training part of every
resample and the held-out fold never contributes to them. The model is
then fitted on components, so `selected`, `scores` and `details$coef`
are named `PC1`, `PC2`, ... rather than after the original columns.
`nPCs` must be smaller than the number of rows each resample trains on
as well as smaller than the number of predictors.

`lambda_seq = NULL` (the default) tunes over the lambda path
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
itself proposes at each alpha (up to 50 values per alpha), which is
scaled to the data, instead of a fixed sequence that spends most of its
fits on irrelevant lambdas. Only the candidate values come from the full
data, exactly as in caret's own default glmnet grid; which pair wins is
still decided by resampling. With `use_pca = TRUE` that path is computed
on the original predictors, so it is only an approximation of the scale
the components live on; pass `lambda_seq` explicitly if you need to
control it.

## Examples

``` r
# \donttest{
if (requireNamespace("caret", quietly = TRUE) &&
    requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("Matrix", quietly = TRUE)) {
  # x1 and x2 drive y; x3 is noise
  df <- data.frame(
    x1 = seq(-2, 2, length.out = 60),
    x2 = rep(c(-1, 0, 1), 20),
    x3 = cos(seq_len(60))
  )
  df$y <- 2 * df$x1 - df$x2 + 0.1 * cos(seq_len(60) * 3)
  res <- fs_elastic(df, "y", alpha_seq = c(0.5, 1), seed = 1)
  selected(res)
  res$scores
  res$details$best_alpha
}
#> Loading required package: ggplot2
#> Loading required package: lattice
#> [1] 1
# }
```
