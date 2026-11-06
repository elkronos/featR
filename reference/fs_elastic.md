# Elastic-net feature selection with tuned alpha and lambda

Tunes an elastic-net model over a grid of `alpha` and `lambda` values
with `caret::train(method = "glmnet")` and selects the predictors whose
coefficient is non-zero at the winning pair. Handles regression (numeric
outcome) and classification (factor, character or logical outcome).

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
  predictors, with unique, non-empty column names.

- target:

  Single string naming the outcome column in `data`.

- alpha_seq:

  Numeric vector of alpha values to tune over, each in `[0, 1]`. Default
  `seq(0.1, 1, by = 0.1)`, which leaves out 0 (ridge) because ridge
  cannot zero a coefficient.

- lambda_seq:

  Numeric vector of non-negative lambda values to tune over, or `NULL`
  (default) to use glmnet's own path at each alpha (see "Tuning grid and
  metrics").

- trControl:

  Optional
  [`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html)
  object; a partial list or a string is an error. If `NULL` (default),
  5-fold CV with an NA-safe summary function is used. When
  `use_pca = TRUE`, `pcaComp = nPCs` is added to its `preProcOptions`.
  Its `allowParallel` is always overwritten with `n_cores > 1`, so a
  registered foreach backend is used only when `n_cores > 1`.

- metric:

  Optional character. Performance metric to optimize. If `NULL`
  (default), `"RMSE"` for regression and `"Accuracy"` for
  classification. If caret does not find the name in the summary output,
  it warns and falls back to its default, and `details$metric_name`
  reports the metric actually used.

- use_pca:

  Logical; default `FALSE`. Whether to project the predictors onto
  principal components inside each resample.

- nPCs:

  Integer \>= 1. Number of principal components to retain when
  `use_pca = TRUE`; must be strictly less than `min(nrow, ncol)` of the
  predictor matrix. Default `NULL`, which is an error when
  `use_pca = TRUE` and ignored otherwise.

- seed:

  Optional whole number; default `NULL`, which leaves the RNG alone.
  When given, the seed is set for this call only and the previous RNG
  state is restored on exit. The resampling folds are drawn in the main
  R process and the glmnet fits are deterministic, so a seeded
  `n_cores > 1` run gives the same result as a seeded sequential one.
  When `trControl$seeds` is unset (`NA`, the `trainControl()` default),
  caret draws one seed per resample from this seed in the main process
  and sets it on each worker, so seeded runs that use worker-side
  randomness (for example `sampling = "up"`) are reproducible with any
  `n_cores`. A user-supplied `trControl$seeds` takes precedence.

- verbose:

  Logical; default `FALSE`. Print progress messages.

- n_cores:

  Integer \>= 1; default 1 (sequential, and no cluster is created).
  Number of workers for parallel training; values above the detected
  core count are capped. Values above 1 need the 'foreach' and
  'doParallel' packages.

## Value

An `fs_result` object with:

- selected:

  Columns (or components, when `use_pca = TRUE`) whose coefficient at
  the chosen alpha/lambda is non-zero; for multinomial fits, the union
  across classes.

- scores:

  Named numeric vector of absolute coefficients at the chosen
  alpha/lambda, one entry per column the model saw; columns shrunk to
  zero are kept with a score of 0. `NULL` for multinomial fits, where a
  predictor has one coefficient per class and no single score exists.

- method:

  `"elastic_net"`.

- task:

  `"regression"` or `"classification"`.

- model:

  The [`caret::train`](https://rdrr.io/pkg/caret/man/train.html) object.

- details:

  List of `coef` (coefficients at the winning lambda, intercept
  included: a sparse matrix, or a list of them for multinomial fits),
  `best_alpha` and `best_lambda` (the winning tuning pair),
  `metric_name` (the metric caret optimized, which differs from `metric`
  when caret fell back to its default), `metric_value` (its resampled
  value for the winning pair, `NA` when the metric is absent from
  caret's results table; optimistic, see Details), `use_pca`, and
  `n_features` (number of columns the model saw, which is `nPCs` when
  `use_pca = TRUE`).

- call:

  The matched call.

## Details

The elastic net penalizes coefficients with a mix of an L1 (lasso) and
an L2 (ridge) penalty: `alpha` sets the mix and `lambda` the overall
strength. Both are chosen by resampling, and every column with a
non-zero coefficient at the winning pair is reported. When the winning
`alpha` is below 1, the ridge part spreads weight across correlated
predictors, so a group of collinear columns tends to survive together
instead of being reduced to one representative as
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
tends to do.

`alpha = 0` is pure ridge, which never sets a coefficient to exactly
zero and so cannot select. The default `alpha_seq` starts at 0.1 for
that reason. If you include 0 and it wins, every column is selected and
a warning says so.

The winning pair is the one with the best resampled metric (caret's
default `selectionFunction = "best"`). With hundreds of alpha/lambda
candidates that choice is noisy and leans towards too little penalty: on
pure-noise data (100 rows, 50 predictors) it selected 8.5 predictors on
average and at least one in every run. A `trControl` with
`selectionFunction = "oneSE"` picks the most penalized pair within one
standard error of the best and was much sparser in the same simulation
(1.8 predictors on average).

For the same reason `details$metric_value`, the best value over the
whole grid, is an optimistic estimate of performance. `fs_elastic()`
holds out no test rows and reports no honest performance estimate; that
needs an outer resampling loop around the whole call.

## How the design matrix is built

Every column of `data` except `target` is a candidate predictor. The
formula is built for you (non-syntactic names are backticked), and the
predictors go through
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
with the intercept column removed, so a k-level factor or character
column contributes k - 1 dummy columns. `selected`, `scores` and
`details$coef` therefore name design-matrix columns rather than the
original columns. Non-syntactic names are reported as you wrote them,
without the backticks
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) adds.
Above 500,000 cells the matrix is converted to a sparse matrix unless
`use_pca = TRUE`.

These inputs are errors:

- fewer than two design-matrix columns (`glmnet` cannot fit a
  single-column `x`), so a single numeric predictor fails;

- a factor or character predictor with fewer than two distinct values;

- a dummy name that collides with another column (factor `g` with level
  `b` next to a column called `gb`);

- Inf or -Inf in the target or any predictor;

- a predictor column that is constant, or constant up to floating-point
  round-off (range at most `64 * .Machine$double.eps` times its largest
  absolute value), checked the same way for dense and sparse input.

## Missing values and the outcome type

Rows with a missing response or a missing value in any predictor are
dropped before fitting; if no rows remain, that is an error. The task is
then inferred from the remaining rows. A class that occurred only in
dropped rows is removed with a message, and a class with fewer than two
complete rows is an error. When rows are dropped, a user-supplied
`trControl` carrying `index`, `indexOut` or `indexFinal` is an error,
because those row numbers no longer match.

A logical response is converted to a two-level factor with a message. A
numeric response with only two distinct values is still treated as
regression, with a warning telling you to convert it to a factor if you
meant classification.

## Scores

`scores` are absolute coefficients on the scale of the columns the model
saw, not standardized ones. They rank predictors fairly only when those
columns are on comparable scales; unlike
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md),
this function does not rescale them for you. Coefficients are taken at
exactly the winning lambda (the path is refitted if that lambda is not
one of the final model's knots), not interpolated between knots.

## Principal components inside resampling

With `use_pca = TRUE`, caret is asked for
`preProcess = c("center", "scale", "pca")` with `pcaComp = nPCs` in
`trControl$preProcOptions`. Centering, scaling and the component
loadings are therefore refitted on the training part of every resample,
and the held-out fold never contributes to them. The model is fitted on
components, so `selected`, `scores` and `details$coef` are named `PC1`,
`PC2`, and so on, rather than after the original columns. `nPCs` must be
smaller than the number of predictors and than the number of rows each
resample trains on.

## Tuning grid and metrics

`lambda_seq = NULL` (the default) tunes over the lambda path that
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
itself proposes at each alpha (up to 50 values per alpha), which is
scaled to the data, instead of a fixed sequence that spends most of its
fits on irrelevant lambdas. Only the candidate values come from the full
data, as in caret's own default glmnet grid; which pair wins is still
decided by resampling. With `use_pca = TRUE` the path is computed on the
original predictors, so it only approximates the scale of the
components; pass `lambda_seq` if you need control over it.

The resampled metrics in `model$results` and `details$metric_value` are
caret's. For regression they are `RMSE`, `Rsquared` and `MAE`.
`Rsquared` is the squared Pearson correlation between held-out
predictions and observations
([`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html)),
not `1 - SSE/SST`, so it is not comparable with an `R2` defined as
`1 - SSE/SST` (for example
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)'s).
It lies in \[0, 1\], except that with the default `trControl = NULL`
featR's summary function records a fold where it is undefined (constant
predictions, typical at the large-lambda end of the path) as `-Inf`, so
such grid rows show `-Inf` in `model$results`. For classification the
metrics are `Accuracy` and Cohen's `Kappa`, which need the suggested
package 'e1071'. featR checks for it before fitting when the default
summary function is used; a custom `summaryFunction` in `trControl` is
responsible for its own dependencies.

## See also

The [Regularization and embedded
importance](https://elkronos.github.io/featR/articles/regularization.html)
guide, which runs this function on a worked example.

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
