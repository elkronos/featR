# Lasso Feature Selection with Cross-Validation

Fits a lasso (or elastic-net) model with
[`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
and reports which predictors survive at `lambda.min` (default) or
`lambda.1se`. Numeric outcomes only (gaussian family).

## Usage

``` r
fs_lasso(
  data,
  target,
  alpha = 1,
  nfolds = 5,
  standardize = TRUE,
  custom_folds = NULL,
  impute = c("none", "mean"),
  return_model = FALSE,
  seed = NULL,
  verbose = FALSE,
  parallel = FALSE,
  n_cores = 2L,
  lambda = c("min", "1se")
)
```

## Arguments

- data:

  A data.frame or data.table holding the target and the candidate
  predictors. A numeric matrix with column names is also accepted;
  non-numeric matrices are not (supply a data.frame so factors and
  characters can be expanded by
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)).

- target:

  Single string naming the outcome column in `data`. It must be numeric
  and free of NA/NaN/Inf.

- alpha:

  Numeric in (0, 1\]; default 1 (lasso). Use values in (0, 1) for
  elastic-net. `alpha = 0` (pure ridge) is rejected: it never sets a
  coefficient to exactly zero, so it cannot select.

- nfolds:

  Integer \>= 3; default 5. Number of cross-validation folds passed to
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html),
  which rejects anything smaller than 3. When `custom_folds` is supplied
  those fold IDs define the folds instead, and `nfolds` only bounds
  which IDs are valid (so `custom_folds` must also define at least 3
  folds).

- standardize:

  Logical; default TRUE. Passed to
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
  and also controls the scale `scores` is ranked on (see Details).

- custom_folds:

  Optional integer vector of fold IDs (one per row of `data`, covering
  1..`nfolds` with no empty folds); default NULL.

- impute:

  How to handle missing predictor values: `"none"` (default) errors and
  names the offending columns, `"mean"` imputes column means computed on
  the full data and warns about the leakage.

- return_model:

  Logical; keep the fitted cv.glmnet object in the result (and pass
  `keep = TRUE` to `cv.glmnet()`); default FALSE.

- seed:

  Optional whole number for reproducibility, applied locally and
  restored on exit; default NULL (never seeds by default).

- verbose:

  Logical; default FALSE. When TRUE, reports the parallel backend status
  and how many design-matrix columns were selected.

- parallel:

  Logical; default FALSE. When TRUE, cross-validation runs on `n_cores`
  workers (requires the 'doParallel' and 'foreach' packages); if
  `n_cores` resolves to one worker it stays sequential and says so under
  `verbose = TRUE`.

- n_cores:

  Integer \>= 1; number of workers used only when `parallel = TRUE`.
  Default 2. Values above the detected core count are capped.

- lambda:

  Which cross-validated penalty to select at: `"min"` (default,
  `lambda.min`) or `"1se"` (`lambda.1se`, sparser). See Details.

## Value

An `fs_result` object with:

- selected:

  Design-matrix columns whose raw coefficient at the chosen lambda
  (`lambda.min` by default) is non-zero, ordered by decreasing `scores`
  magnitude. (Selection reads the raw coefficients, so a constant column
  that glmnet nonetheless gave a non-zero coefficient is reported even
  though its standardized score is 0.)

- scores:

  data.frame with columns Variable, Coefficient and AbsCoefficient, one
  row per design-matrix column (including those shrunk to zero), ordered
  by decreasing AbsCoefficient, on the standardized scale when
  `standardize = TRUE` (see Details).

- method:

  `"lasso"`, regardless of `alpha`.

- task:

  `"regression"` (gaussian family only).

- model:

  The fitted cv.glmnet object when `return_model = TRUE`, else NULL.

- details:

  List of `lambda_min` (lambda minimizing CV error), `lambda_1se`
  (largest lambda within 1 SE of the minimum), `lambda_used`
  (`"lambda.min"` or `"lambda.1se"`, the one selection and all
  coefficients are read at), `coefficients` (the same three-column table
  as `scores` but always on the raw coefficient scale) and `n_features`
  (number of design-matrix columns considered).

- call:

  The matched call.

## Details

Use this when the question is "which predictors keep a non-zero
coefficient under a cross-validated L1 penalty (or, for `alpha < 1`, an
elastic-net penalty)?". By default selection happens at `lambda.min`,
the penalty that minimizes cross-validated error. `lambda.min` is tuned
for prediction and is known to over-select (it tends to keep noise
variables alongside the true ones); `lambda = "1se"` selects at
`lambda.1se`, the largest penalty whose CV error is within one standard
error of the minimum, which gives a sparser, more conservative set. Both
values are reported in `details` whichever one is used. The main caveat
is that lasso tends to keep one member of a group of strongly correlated
predictors and zero out the rest, so an absent feature is not evidence
that it is unrelated to the outcome.

The design matrix is built internally from every column of `data` except
`target`, via `stats::model.frame(na.action = stats::na.pass)` followed
by [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html):
factors and characters are expanded to dummies and rows carrying NAs are
preserved rather than silently dropped. Non-syntactic column names are
reported as the caller wrote them (without the backticks
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) adds). The
design matrix must have at least two columns, because `glmnet` cannot
fit a single-column `x`. The matrix carries no intercept column of its
own (`glmnet` fits its own intercept, which is dropped from the reported
coefficients), so `selected`, `scores` and `details$coefficients` name
design-matrix columns – for a factor predictor, its expanded dummy
columns rather than the original column.

`scores` ranks features on the STANDARDIZED coefficient scale when
`standardize = TRUE`: each coefficient is multiplied by the standard
deviation of its design-matrix column, which makes the ranking
independent of the units the predictors happen to be measured in. glmnet
standardizes internally for fitting but reports coefficients back on the
input scale, so those raw coefficients are kept in
`details$coefficients`. With `standardize = FALSE`, `scores` is the raw
table and the two are identical.

Raw coefficient magnitude is a scale-dependent notion of importance: a
predictor measured in small units earns a large coefficient for the same
effect. That applies to `details$coefficients` always, and to `scores`
when `standardize = FALSE`, so compare those numbers across predictors
only when the predictors share a scale.

Missing predictor values are an error under the default
`impute = "none"`, because imputing before cross-validation lets the
folds see each other. `impute = "mean"` fills them with column means
computed on the whole data set – not on the training part of each fold –
and warns that this leaks. Columns that are entirely NA are an error
either way.

## Examples

``` r
# \donttest{
if (requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("Matrix", quietly = TRUE)) {
  n <- 100
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    cat = sample(letters[1:3], n, TRUE)
  )
  df$y <- 2 * df$x1 - 3 * df$x2 + rnorm(n)
  res <- fs_lasso(df, "y", seed = 123)
  selected(res)
  head(res$scores)
}
#>   Variable Coefficient AbsCoefficient
#> 1       x2 -2.87232033     2.87232033
#> 2       x1  2.16440132     2.16440132
#> 3     catc -0.09940822     0.09940822
#> 4     cata  0.08988327     0.08988327
#> 5     catb  0.00000000     0.00000000
# }
```
