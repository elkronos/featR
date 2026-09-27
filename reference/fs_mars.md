# MARS (earth) feature selection

Trains a Multivariate Adaptive Regression Splines model with
`caret::train(method = "earth")` and repeated cross-validation,
evaluates it on a held-out test set, and reports the predictors earth
retained together with their variable importance.

## Usage

``` r
fs_mars(
  data,
  target,
  train_ratio = 0.8,
  degree = 1:3,
  nprune = c(5, 10, 15),
  tuneLength = 10L,
  search = "grid",
  number = 5,
  repeats = 3,
  sample_size = 10000,
  corr_cut = 0.95,
  remove_nzv = TRUE,
  seed = NULL,
  verbose = FALSE,
  n_cores = 1L
)
```

## Arguments

- data:

  data.frame or data.table with predictors and the target. A data.table
  is copied, never modified in place.

- target:

  Character. Name of the target column in `data`. A factor or character
  target is treated as classification, a numeric one as regression.

- train_ratio:

  Numeric in (0, 1). Training proportion (default 0.8).

- degree:

  Integer vector, each element \>= 1. Interaction degrees to tune
  (default 1:3). Used when `search = "grid"`.

- nprune:

  Integer vector, each element \>= 2. Numbers of retained terms to tune
  (default `c(5, 10, 15)`). Used when `search = "grid"`.

- tuneLength:

  Integer \>= 1. Number of random hyperparameter combinations evaluated
  when `search = "random"` (default 10; ignored for grid search).

- search:

  Character. "grid" (default) or "random"; see Details.

- number:

  Integer \>= 2. Cross-validation folds (default 5).

- repeats:

  Integer \>= 1. Cross-validation repeats (default 3).

- sample_size:

  Integer \>= 1. Maximum number of rows used; larger data sets are
  randomly down-sampled first, after missing rows are dropped and before
  the train/test split (default 10000).

- corr_cut:

  Numeric between 0 and 1. Correlation cutoff for dropping highly
  correlated numeric predictors (default 0.95; 0 disables).

- remove_nzv:

  Logical. Remove near-zero-variance predictors (default TRUE).

- seed:

  Optional whole number for reproducibility, applied locally and
  restored on exit. Default NULL (never seeds by default). When NULL, no
  deterministic resampling seed lists are constructed either.

- verbose:

  Logical. Print progress messages, caret's per-fold iteration log, and
  the small-class notice (default FALSE).

- n_cores:

  Integer \>= 1. Number of parallel workers for model training (default
  1 = sequential; no cluster is created and caret's `allowParallel`
  stays FALSE). Values above the detected core count are capped;
  requires 'doParallel' and 'foreach' when greater than 1.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of the predictors earth retained (strictly positive
  [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html)
  importance), ordered by decreasing importance. Empty when no
  importance is available.

- scores:

  Named numeric vector of unscaled variable importance, one entry per
  candidate predictor that entered training, floored at 0 (predictors
  earth pruned away score 0). `NULL` when caret cannot compute variable
  importance for the fitted model, in which case `selected` is empty.

- method:

  "mars".

- task:

  "classification" for a factor or character target (characters are
  coerced to factor), "regression" for a numeric one.

- model:

  The [`caret::train`](https://rdrr.io/pkg/caret/man/train.html) object.

- details:

  A list with `predictions` (test-set predictions; a factor carrying the
  training levels for classification), `metrics` (RMSE/MAE/R2 for
  regression; Accuracy/Kappa plus ROC_AUC/PR_AUC when the optional
  'pROC'/'PRROC' packages are installed for binary classification),
  `confusion_matrix` (classification only, else NULL), `varimp` (the
  [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html) object,
  or NULL when unsupported), `removed_predictors` (a list with `nzv` and
  `corr` naming the dropped predictors), `train_index` (integer row
  indices of the training rows, into the cleaned and optionally
  down-sampled data), `test_data` (the held-out rows after
  preprocessing) and `n_features` (number of candidate predictors that
  entered training).

- call:

  The matched call.

## Details

Use this when you want a model that discovers non-linear effects and
interactions on its own and then tells you which predictors it used:
earth fits piecewise-linear hinge terms, prunes them back, and the
surviving terms define the selected set. Selection is model-based rather
than a filter, so it reflects one particular fitted model and its
tuning.

The pipeline, in order: rows with any missing value are dropped; the
data are randomly down-sampled to at most `sample_size` rows; for
classification each class must have at least two rows; a stratified
[`caret::createDataPartition()`](https://rdrr.io/pkg/caret/man/createDataPartition.html)
split keeps `train_ratio` of the rows for training and holds the rest
out; near-zero-variance and strongly correlated predictors (see
`remove_nzv` and `corr_cut`) are identified on the training rows only
and dropped from both halves; then
[`caret::train()`](https://rdrr.io/pkg/caret/man/train.html) fits earth
under repeated cross-validation with
`preProcess = c("center", "scale")`.

Selection is read off the fitted model:
`caret::varImp(model, scale = FALSE)` is computed on the caret `train`
object and every predictor with a strictly positive importance is
reported in `selected`. Predictors earth pruned away score 0 and are not
selected. caret expands factors into dummy columns before fitting, so
each importance row is attributed back to the source predictor by name
(the longest candidate name it starts with) and a predictor keeps the
largest importance of its encoded columns; that name-based attribution
is ambiguous if one predictor's name happens to be a prefix of another
predictor's encoded column name. The backticks caret adds around
non-syntactic names are stripped first, so such columns are attributed
like any other.

For classification, class imbalance is handled by `sampling = "up"`
inside
[`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html),
i.e. upsampling happens within each resample; the data are never
upsampled before cross-validation (which would leak duplicated rows
across folds). The FIRST factor level is treated as the positive class
for ROC/PR AUC, and the test-set ROC AUC is computed with a fixed
direction, so a worse-than-chance model scores below 0.5 rather than
being silently flipped. Factor levels are sanitized with
`make.names(unique = TRUE)`, so distinct labels can never be merged.

For regression, the reported `R2` comes from
[`caret::R2()`](https://rdrr.io/pkg/caret/man/postResample.html), which
is the squared correlation between predictions and observations – not
`1 - SSE/SST` – and can be high even for a biased model.

Everything in `details$metrics` comes from the single held-out split, so
on small data sets these numbers are noisy; the per-resample tuning
results are in `model$resample`.

`search = "grid"` tunes over `expand.grid(nprune, degree)`;
`search = "random"` ignores that grid and evaluates `tuneLength` random
hyperparameter combinations instead.

## Examples

``` r
# \donttest{
if (requireNamespace("caret", quietly = TRUE) &&
    requireNamespace("earth", quietly = TRUE)) {
  # x1 and x2 drive y; x3 is noise
  df <- data.frame(
    x1 = rnorm(150),
    x2 = rnorm(150),
    x3 = rnorm(150)
  )
  df$y <- 2 * df$x1 - df$x2 + rnorm(150, sd = 0.5)
  res <- fs_mars(df, "y", degree = 1, nprune = c(5, 10),
                 number = 3, repeats = 1, seed = 42)
  res$selected
  res$scores
  res$details$metrics
}
#> Loading required package: earth
#> Loading required package: Formula
#> Loading required package: plotmo
#> Loading required package: plotrix
#> $RMSE
#> [1] 0.4538177
#> 
#> $MAE
#> [1] 0.3480216
#> 
#> $R2
#> [1] 0.9614264
#> 
# }
```
