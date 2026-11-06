# Lasso feature selection with cross-validation

Fits a lasso (or elastic-net) regression with
[`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
and selects the predictors whose coefficient is non-zero at the
cross-validated penalty. The outcome must be numeric; only the gaussian
family is fitted.

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
  lambda = c("min", "1se"),
  seed = NULL,
  verbose = FALSE,
  parallel = FALSE,
  n_cores = 2L
)
```

## Arguments

- data:

  A data.frame or data.table holding the target and the candidate
  predictors, with unique, non-empty column names. A numeric matrix with
  column names is also accepted. A non-numeric matrix is not: supply a
  data.frame so factors and characters can be expanded by
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).

- target:

  Single string naming the outcome column in `data`. It must be numeric
  and free of NA, NaN and Inf.

- alpha:

  Numeric in (0, 1\]; default 1 (lasso). Values in (0, 1) give an
  elastic net. `alpha = 0` (pure ridge) is rejected because ridge never
  sets a coefficient to exactly zero, so it cannot select.

- nfolds:

  Integer \>= 3; default 5. Number of cross-validation folds passed to
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html),
  which rejects anything smaller than 3. When `custom_folds` is
  supplied, those fold IDs define the folds and `nfolds` only bounds
  which IDs are valid, so `custom_folds` must also define at least 3
  folds.

- standardize:

  Logical; default `TRUE`. Passed to
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html),
  and also sets the scale `scores` is ranked on (see "How scores are
  scaled").

- custom_folds:

  Optional integer vector of fold IDs, one per row of `data`, covering
  1..`nfolds` with no empty fold; default `NULL`.

- impute:

  How to handle missing predictor values: `"none"` (default) errors and
  names the affected columns; `"mean"` imputes column means computed on
  the full data and warns about the leak.

- return_model:

  Logical; default `FALSE`. When `TRUE`, the fitted `cv.glmnet` object
  is kept in `model` and `keep = TRUE` is passed to `cv.glmnet()` so the
  prevalidated fits are retained.

- lambda:

  Penalty to select at: `"min"` (default, `lambda.min`) or `"1se"`
  (`lambda.1se`, sparser). See Details.

- seed:

  Optional whole number; default `NULL`, which leaves the RNG alone.
  When given, the seed is set for this call only and the previous RNG
  state is restored on exit.

- verbose:

  Logical; default `FALSE`. When `TRUE`, reports the parallel backend
  status and how many design-matrix columns were selected.

- parallel:

  Logical; default `FALSE`. When `TRUE`, cross-validation runs on
  `n_cores` workers, which needs the 'doParallel' and 'foreach'
  packages. If `n_cores` resolves to one worker, the run stays
  sequential and says so when `verbose = TRUE`.

- n_cores:

  Integer \>= 1; default 2. Number of workers, used only when
  `parallel = TRUE`. Values above the detected core count are capped.

## Value

An `fs_result` object with:

- selected:

  Design-matrix columns whose coefficient at the chosen lambda is
  non-zero, ordered by decreasing `scores` magnitude. Constant and
  numerically constant columns always have coefficient 0, so they are
  never selected.

- scores:

  data.frame with columns Variable, Coefficient and AbsCoefficient, one
  row per design-matrix column (including those shrunk to zero), ordered
  by decreasing AbsCoefficient. On the standardized scale when
  `standardize = TRUE`.

- method:

  `"lasso"`, whatever the value of `alpha`.

- task:

  `"regression"` (gaussian family only).

- model:

  The fitted `cv.glmnet` object when `return_model = TRUE`, else `NULL`.

- details:

  List of `lambda_min` (lambda minimizing CV error), `lambda_1se`
  (largest lambda within one standard error of the minimum),
  `lambda_used` (`"lambda.min"` or `"lambda.1se"`, the one that
  selection and all coefficients are read at), `coefficients` (the same
  three-column table as `scores`, always on the raw coefficient scale)
  and `n_features` (number of design-matrix columns considered).

- call:

  The matched call.

## Details

The lasso adds an L1 penalty to least squares, which shrinks
coefficients and sets some of them to exactly zero. `fs_lasso()` chooses
the penalty strength (lambda) by `nfolds`-fold cross-validation and
reports the predictors that keep a non-zero coefficient. With
`alpha < 1` the penalty becomes an elastic net, a mix of L1 and L2
penalties;
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
tunes `alpha` as well and also handles classification.

By default selection happens at `lambda.min`, the penalty with the
lowest cross-validated error. `lambda.min` is tuned for prediction and
tends to over-select, keeping noise variables alongside the true ones.
`lambda = "1se"` selects at `lambda.1se`, the largest penalty whose CV
error is within one standard error of the minimum, which gives a
sparser, more conservative set. Both values are reported in `details`
whichever one is used.

The lasso tends to keep one member of a group of strongly correlated
predictors and zero out the rest, so an absent feature is not evidence
that it is unrelated to the outcome.

`fs_lasso()` reports no performance estimate. The lambdas are tuned on
all rows, so judging the selected set needs an outer resampling loop
around the whole call.

## How the design matrix is built

Every column of `data` except `target` is a candidate predictor. The
design matrix comes from
`stats::model.frame(na.action = stats::na.pass)` followed by
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
without an intercept column, so rows with NAs reach the missing-value
policy instead of being dropped silently. Factors and characters are
expanded to dummy columns: the first factor (in column order) gets one
dummy per level and each later factor gets one per level except the
first. `glmnet` fits its own intercept, which is left out of the
reported coefficients.

`selected`, `scores` and `details$coefficients` therefore name
design-matrix columns, so a factor predictor appears as its dummy
columns rather than under its own name. Non-syntactic column names are
reported as you wrote them, without the backticks
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) adds.

These inputs are errors:

- a design matrix with fewer than two columns (`glmnet` cannot fit a
  single-column `x`);

- a factor or character predictor with fewer than two observed levels;

- a dummy name that collides with another column or the target (factor
  `g` with level `b` next to a column `gb`), because `selected` and
  `scores` identify columns by name;

- Inf or -Inf in any predictor (NaN counts as a missing value);

- a column whose variance overflows double precision (values around
  1e154 or larger); rescale it first.

## How scores are scaled

With `standardize = TRUE` (the default), `scores` ranks features on the
standardized coefficient scale: each coefficient is multiplied by the
standard deviation of its design-matrix column, so the ranking does not
depend on the units the predictors are measured in. `glmnet`
standardizes internally for fitting but reports coefficients on the
input scale, and those raw coefficients are kept in
`details$coefficients`. With `standardize = FALSE`, `scores` holds the
raw coefficients and the two tables are identical.

A raw coefficient depends on scale: a predictor measured in small units
earns a large coefficient for the same effect. Compare
`details$coefficients` (and `scores` when `standardize = FALSE`) across
predictors only when the predictors share a scale.

Constant columns get a coefficient of exactly 0 and are never selected.
The same holds for columns that are constant up to floating-point
round-off: standardization would otherwise inflate that round-off into a
full-scale predictor with a raw coefficient around 1e15, so such columns
are held at 0 during the fit. A coefficient whose standardized value is
at most `1e-10` times the largest standardized coefficient is
floating-point residue and is set to exactly 0. Without that step, the
redundant dummy of the first factor, which is collinear with the
intercept, would keep a coefficient around 1e-13 and be selected.

## Missing values

Under the default `impute = "none"`, a missing predictor value is an
error that names the affected columns, because imputing before
cross-validation lets the folds see each other. `impute = "mean"` fills
missing values with column means computed on all rows, not on the
training part of each fold, and warns about that leak. A column that is
entirely NA is an error under either setting. The target must be free of
NA, NaN and Inf.

## See also

The [Regularization and embedded
importance](https://elkronos.github.io/featR/articles/regularization.html)
guide, which runs this function on a worked example.

## Examples

``` r
if (requireNamespace("glmnet", quietly = TRUE)) {
  n <- 100
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    cat = sample(letters[1:3], n, TRUE)
  )
  df$y <- 2 * df$x1 - 3 * df$x2 + rnorm(n)
  res <- fs_lasso(df, "y", seed = 123)
  print(selected(res))
  print(head(res$scores))
}
#> [1] "x2"   "x1"   "catc" "cata"
#>   Variable Coefficient AbsCoefficient
#> 1       x2 -2.87232033     2.87232033
#> 2       x1  2.16440132     2.16440132
#> 3     catc -0.09940822     0.09940822
#> 4     cata  0.08988327     0.08988327
#> 5     catb  0.00000000     0.00000000
```
