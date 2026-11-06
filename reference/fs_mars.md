# MARS (earth) feature selection

Trains a multivariate adaptive regression splines (MARS) model with
`caret::train(method = "earth")` under repeated cross-validation,
evaluates it on a held-out test set, and selects the predictors that the
final model uses, together with their variable importance.

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

  data.frame or data.table with the predictors and the target, with
  unique, non-empty column names and no infinite values. A data.table is
  copied, never modified in place.

- target:

  Character. Name of the target column in `data`. A factor or character
  target is treated as classification, a numeric one as regression.
  Unused levels of a factor target are dropped.

- train_ratio:

  Numeric in (0, 1). Proportion of rows used for training (default 0.8).

- degree:

  Integer vector, each element \>= 1. Interaction degrees to tune
  (default `1:3`). Used when `search = "grid"`.

- nprune:

  Integer vector, each element \>= 2. Numbers of retained terms to tune
  (default `c(5, 10, 15)`). Used when `search = "grid"`.

- tuneLength:

  Integer \>= 1. Number of random hyperparameter combinations evaluated
  when `search = "random"` (default 10; ignored for grid search).

- search:

  Character. `"grid"` (default) or `"random"`; see Details.

- number:

  Integer \>= 2. Cross-validation folds (default 5).

- repeats:

  Integer \>= 1. Cross-validation repeats (default 3).

- sample_size:

  Integer \>= 1. Maximum number of rows used (default 10000). Larger
  data sets are randomly down-sampled after missing rows are dropped and
  before the train/test split.

- corr_cut:

  Numeric between 0 and 1. Correlation cutoff for dropping highly
  correlated numeric predictors (default 0.95; 0 disables).

- remove_nzv:

  Logical. Remove near-zero-variance predictors (default `TRUE`).

- seed:

  Optional whole number; default `NULL`, which leaves the RNG alone and
  builds no resampling seed lists. When given, the seed is set for this
  call only and the previous RNG state is restored on exit. It drives
  the down-sampling and the train/test split, and one seed per resample
  and tuning candidate is drawn from it and passed to
  `caret::trainControl(seeds = )`. That seed list is what keeps a seeded
  `n_cores > 1` run reproducible; the parallel workers themselves are
  not given seeded RNG streams.

- verbose:

  Logical. Print progress messages, caret's per-fold iteration log, and
  a warning when a class has fewer than 10 rows (default `FALSE`).

- n_cores:

  Integer \>= 1. Number of parallel workers for model training (default
  1: sequential, no cluster is created and caret's `allowParallel` stays
  `FALSE`). Values above the detected core count are capped. Values
  above 1 need 'doParallel' and 'foreach'; the workers need 'caret' and
  'earth' installed, not attached.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of the predictors that appear in a term of the final
  earth model, ordered by decreasing importance. If
  [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html) fails
  (with a warning), those predictors are still reported, in model order.

- scores:

  Named numeric vector of unscaled variable importance, one entry per
  candidate predictor that entered training. Predictors earth pruned
  away score 0; a predictor in the final model keeps its importance even
  when that is zero or negative. `NULL` when caret cannot compute
  variable importance for the fitted model, in which case a warning
  carries caret's error message.

- method:

  `"mars"`.

- task:

  `"classification"` for a factor or character target (characters are
  coerced to factor), `"regression"` for a numeric one.

- model:

  The [`caret::train`](https://rdrr.io/pkg/caret/man/train.html) object.

- details:

  A list with `predictions` (test-set predictions; for classification, a
  factor with the training levels), `metrics` (RMSE, MAE and R2 for
  regression; Accuracy and Kappa for classification; for binary
  classification also ROC_AUC when the optional 'pROC' package is
  installed and PR_AUC when 'PRROC' is installed as well; `ROC_AUC` is
  `NA` and `PR_AUC` absent when the held-out rows contain only one
  class), `confusion_matrix` (classification only, else `NULL`),
  `varimp` (the
  [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html) object,
  or `NULL` when unsupported), `removed_predictors` (a list with `nzv`
  and `corr` naming the dropped predictors), `train_index` (integer row
  indices of the training rows within the cleaned and possibly
  down-sampled data), `test_data` (the held-out rows after
  preprocessing) and `n_features` (number of candidate predictors that
  entered training).

- call:

  The matched call.

## Details

MARS builds a model from piecewise-linear hinge terms, optionally
multiplied together into interactions, and then prunes the terms back.
It finds non-linear effects and interactions without you specifying
them, and the predictors that appear in the surviving terms form the
selected set. That set comes from one fitted and tuned model, not from a
per-predictor test.

The steps run in this order:

1.  Rows with any missing value are dropped.

2.  The data are randomly down-sampled to at most `sample_size` rows.

3.  For classification, unused target levels are dropped and each class
    must have at least two rows.

4.  A stratified
    [`caret::createDataPartition()`](https://rdrr.io/pkg/caret/man/createDataPartition.html)
    split keeps `train_ratio` of the rows for training and holds the
    rest out.

5.  Near-zero-variance predictors (see `remove_nzv`) and highly
    correlated numeric predictors (see `corr_cut`) are identified on the
    training rows only and dropped from both halves.

6.  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html) tunes
    earth under repeated cross-validation with
    `preProcess = c("center", "scale")` applied inside each resample.

7.  The final model is evaluated on the held-out rows.

`search = "grid"` tunes over `expand.grid(nprune, degree)`.
`search = "random"` ignores that grid and evaluates `tuneLength` random
hyperparameter combinations instead. Tuning optimizes `RMSE` for
regression, `ROC` for binary classification when 'pROC' is installed,
and `Accuracy` otherwise.

Everything in `details$metrics` comes from the single held-out split.
The selection and preprocessing never saw those rows, so the numbers are
honest, but on small data sets they are noisy. The cross-validated
figures in `model$results` were used to pick the tuning values, so the
winning row is optimistic. The per-resample results for every candidate
are in `model$resample`.

## How predictors are selected and scored

`caret::varImp(model, scale = FALSE)` is computed on the caret `train`
object, and every predictor that appears in a term of the final (pruned)
earth model is reported in `selected`. Predictors earth pruned away
score 0 and are not selected. earth's GCV importance can be zero or
negative for a predictor the final model still uses; such a predictor is
selected and keeps its signed importance.

caret expands factors into dummy columns before fitting, so each
importance row is attributed back to its source predictor by name (the
longest candidate name it starts with), and a predictor keeps the
largest importance of its dummy columns. This attribution is ambiguous
when one predictor's name is a prefix of another predictor's dummy
column name. The backticks caret adds around non-syntactic names are
stripped first, so such columns are attributed like any other.

## Classification

Class imbalance is handled by `sampling = "up"` in
[`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html),
so upsampling happens inside each resample. The data are never upsampled
before cross-validation, which would leak duplicated rows across folds.

The first factor level, after unused levels are dropped and labels
sanitized, is the positive class for ROC and PR AUC and for the
sensitivity and specificity (`Sens`, `Spec`) that
[`caret::twoClassSummary()`](https://rdrr.io/pkg/caret/man/postResample.html)
reports in `model$results`. There is no override; reorder the target's
levels with [`stats::relevel()`](https://rdrr.io/r/stats/relevel.html)
to change it.
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
differs: its default positive class is the second level. ROC AUC does
not depend on which class is positive, but PR AUC, sensitivity and
specificity do. The test-set ROC AUC is computed with a fixed direction,
so a worse-than-chance model scores below 0.5 instead of being flipped.

Factor levels are sanitized with `make.names(unique = TRUE)`, so
distinct labels such as "class 1" and "class.1" are never merged.
Classification needs the 'e1071' package (used by
[`caret::confusionMatrix()`](https://rdrr.io/pkg/caret/man/confusionMatrix.html)
and caret's resampling summaries), which is checked before any training.

## Regression

The reported `R2` comes from
[`caret::R2()`](https://rdrr.io/pkg/caret/man/postResample.html), which
is the squared correlation between predictions and observations, not
`1 - SSE/SST`. It can be high even for a biased model.

## See also

The [Regularization and embedded
importance](https://elkronos.github.io/featR/articles/regularization.html)
guide, which runs this function on a worked example.

## Examples

``` r
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
  print(res$selected)
  print(res$scores)
  print(res$details$metrics)
}
#> Loading required package: earth
#> Loading required package: Formula
#> Loading required package: plotmo
#> Loading required package: plotrix
#> [1] "x1" "x2"
#>        x1        x2        x3 
#> 100.00000  43.91191   0.00000 
#> $RMSE
#> [1] 0.4538177
#> 
#> $MAE
#> [1] 0.3480216
#> 
#> $R2
#> [1] 0.9614264
#> 
```
