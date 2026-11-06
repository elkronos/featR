# Train and evaluate an SVM, with optional SVM-RFE feature selection

Trains a support vector machine (SVM) classifier or regressor with
caret's kernlab engines, tunes it by cross-validation, and reports its
performance on held-out rows. With `feature_select = TRUE` it first
selects features on the training rows, by SVM-RFE (the default) or by
random-forest RFE. It can also up-sample minority classes and train on
several workers.

## Usage

``` r
fs_svm(
  data,
  target,
  task,
  train_ratio = 0.7,
  nfolds = 5,
  kernel = c("linear", "radial", "polynomial"),
  tune_grid = NULL,
  feature_select = FALSE,
  select_method = c("svm_rfe", "rf_rfe"),
  n_features = NULL,
  class_imbalance = FALSE,
  seed = NULL,
  verbose = FALSE,
  n_cores = 1L
)
```

## Arguments

- data:

  A data frame (or data.table) containing the predictors and the target,
  with unique, non-empty column names.

- target:

  A string naming the target column.

- task:

  Either `"classification"` or `"regression"`. Required; there is no
  default, because guessing it from the target is exactly the mistake
  this argument exists to prevent.

- train_ratio:

  Training proportion of the stratified split, strictly between 0 and 1.
  Default `0.7`.

- nfolds:

  Number of cross-validation folds for tuning the final model, a whole
  number greater than 1. Default `5`. SVM-RFE also uses it for its size
  search (capped there at the number of training rows);
  `select_method = "rf_rfe"` ignores it and uses 10 folds.

- kernel:

  One of `"linear"` (default), `"radial"`, or `"polynomial"`.

- tune_grid:

  Optional data frame of tuning values for
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html). If
  `NULL`, the default grid for the kernel is used: `C = 2^(-5:10)` for
  `"linear"`; `sigma = 2^(-15:-5)` crossed with `C = 2^(-1:10)` for
  `"radial"`; `degree = 2:5`, `scale = c(0.1, 0.5, 1)` and
  `C = 2^(-3:8)` crossed for `"polynomial"`.

- feature_select:

  Logical; if `TRUE`, run feature selection on the dummy-encoded
  training predictors. Default `FALSE`.

- select_method:

  Which selector to run when `feature_select = TRUE`: `"svm_rfe"`
  (default; SVM-RFE, linear kernel only) or `"rf_rfe"` (random-forest
  screening, any kernel). Ignored when `feature_select = FALSE`, and so
  is the linear-kernel requirement, which is only enforced when SVM-RFE
  will run. An unrecognized value is always an error.

- n_features:

  Optional whole number of features to keep, capped at the number of
  encoded predictors. For `"svm_rfe"`, the top `n_features` ranked
  features are kept and the cross-validated size search is skipped. For
  `"rf_rfe"`, the selection is truncated to its `n_features`
  highest-scoring entries, and `n_features` is also the minimum the
  random-forest fallback keeps (see Details). Ignored when
  `feature_select = FALSE`. Default `NULL` (the size is chosen
  automatically).

- class_imbalance:

  Logical; if `TRUE` and the task is classification, up-sample classes
  within each cross-validation resample. Default `FALSE`.

- seed:

  Optional whole-number seed within the R integer range. Fractional
  values are an error, not truncated. Default `NULL`, which never seeds.
  See "Packages, seeds and parallel runs" in Details.

- verbose:

  Logical; if `TRUE`, report progress, including each SVM-RFE
  elimination step. Default `FALSE`.

- n_cores:

  Number of parallel workers. Default `1` (sequential). Requests are
  capped at the detected core count. Above 1, a cluster is created for
  the duration of the call and stopped on exit.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of selected encoded feature names. With
  `feature_select = FALSE` this is every encoded predictor. With
  `"svm_rfe"` it is the surviving subset, from most to least important.
  With `"rf_rfe"` it is the subset in the order
  [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html) reports it,
  or, when `n_features` truncated it, ordered by decreasing `scores`.

- scores:

  Named numeric vector covering every encoded predictor, not only the
  survivors. For `"svm_rfe"` it is the squared primal weight `w^2` from
  the first, full-feature fit. For `"rf_rfe"` it is the mean
  random-forest importance recorded across resamples at the full subset
  size, `NA` for predictors
  [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html) never scored;
  when the fallback ran, it is the permutation importance of the
  fallback forest. `NULL` when `feature_select = FALSE`, because no
  selector produced scores.

- method:

  `"svm_"` followed by the kernel, for example `"svm_linear"`. The
  selector that ran, if any, is in `details$selection$method`.

- task:

  `"classification"` or `"regression"`.

- model:

  The fitted [`caret::train`](https://rdrr.io/pkg/caret/man/train.html)
  object. With `feature_select = TRUE` its resampled metrics are
  optimistic (see Details); use `details$performance`.

- details:

  A list with:

  - `test_set`: the test split, with its coerced target and aligned
    factor levels.

  - `predictions`: test-set predictions in the row order of `test_set`;
    a factor for classification, an unnamed numeric vector for
    regression.

  - `performance`: a
    [`caret::confusionMatrix`](https://rdrr.io/pkg/caret/man/confusionMatrix.html)
    for classification, or a named `RMSE`/`Rsquared`/`MAE` vector for
    regression, where `Rsquared` is the squared correlation (see
    Details).

  - `selection`: `NULL` when no selection ran, otherwise a list with
    `method`, `ranking` (most to least important), `scores`, and the
    size search's `sizes`, `size_scores` and `size_metric`. Those last
    three are `NULL`, `NULL` and `NA` whenever no size search ran, which
    is always the case for `"rf_rfe"` and for `"svm_rfe"` with an
    explicit `n_features`.

  - `encoder`: the fitted
    [`caret::dummyVars`](https://rdrr.io/pkg/caret/man/dummyVars.html)
    object.

  - `n_features`: the number of encoded predictors considered, counted
    before any were dropped.

- call:

  The matched call.

## Details

For classification, an SVM fits the boundary between classes that stays
furthest from the nearest training points while tolerating some points
on the wrong side. For regression, it fits a function that keeps most
points within a margin around it. The cost `C` sets how heavily points
that break these rules are penalized. The kernel sets the shape of the
boundary or function: a hyperplane for `"linear"`, curved surfaces for
`"radial"` and `"polynomial"`.

Use `fs_svm()` when the selector and the final model should come from
the same family. SVM-RFE ranks features by the weights of a linear SVM,
and the result carries the tuned model and its held-out performance next
to the chosen features. The cost is a full SVM fit at every elimination
step plus a cross-validated size search, so on wide data screen first
with a filter such as
[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md).

The honest performance estimate is `details$performance`. The
[Validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.html)
article lists which numbers in each featR result are held out and which
are optimistic.

### Order of operations

1.  The target is coerced for `task` (see "Data requirements").

2.  The rows are split into training and test sets, stratified on the
    target, with `train_ratio` of them used for training. For
    classification, test rows whose class does not occur in the training
    rows are dropped with a warning.

3.  Factor and character predictors in the test rows are re-leveled to
    the levels observed in the training rows. Test rows with a level the
    training rows never saw are dropped with a warning.

4.  Predictors are dummy-encoded with full-rank dummies, fitted on the
    training rows.

5.  With `feature_select = TRUE`, the selector runs on the encoded
    training predictors, and the training and test matrices are cut down
    to the selected columns.

6.  The final SVM is fitted with
    [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html):
    predictors are centered and scaled, and `tune_grid` is searched by
    `nfolds`-fold cross-validation on the training rows.

7.  The tuned model predicts the test rows, which gives
    `details$performance`.

### SVM-RFE

SVM-RFE (Guyon, Weston, Barnhill and Vapnik, 2002) fits a linear SVM on
the centered and scaled encoded training matrix and ranks the features
by their squared primal weight `w^2`. For a multi-class fit, `w^2` is
summed over the pairwise problems. The lowest-ranked feature is dropped
(the lowest 10 percent while more than 50 features remain), and the SVM
is refitted on the rest until one feature is left. Reversing the
elimination order gives the ranking, so rank 1 is the feature eliminated
last. These fits use a fixed cost of `C = 1` and are separate from the
final model, which is tuned over `tune_grid`.

SVM-RFE requires `kernel = "linear"`, because the primal weight vector
only exists for a linear kernel. Combining it with another kernel is an
error that suggests `select_method = "rf_rfe"`.

With `n_features` supplied, SVM-RFE keeps exactly that many features
from the top of the ranking. Otherwise it scores a short ladder of
candidate sizes: the powers of two up to the number of features, plus
the full size, trimmed to at most six entries but always including 1 and
the full size. Each size is scored by `nfolds`-fold cross-validation
with the same linear SVM, on folds shared by every size. Inside each
fold, the standardization and the whole elimination are re-run on that
fold's training rows, and the top features of that fold's ranking are
scored on its held-out rows, so no size is scored by a ranking that has
seen the rows it is scored on. The size with the highest mean accuracy
(classification) or the lowest mean RMSE (regression) wins, ties going
to the smaller size. The kept features are the top of the ranking
computed on the whole training split. If no size could be scored, all
features are kept with a warning.

Each entry of `details$selection$size_scores` is an honest estimate for
its own size. The entry for the chosen size, though, is the best of up
to six noisy estimates and so is optimistic; on pure noise it typically
sits a few points above chance.

### Random-forest screening

`select_method = "rf_rfe"` screens features with random-forest RFE
([`caret::rfFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html)) and
works with every kernel. It runs its own 10-fold cross-validation over
subset sizes 1 to `p`, independent of `nfolds`. Encoded predictors that
are numerically constant are removed first; their score is `NA` and they
are never selected.

If [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html) fails or
selects nothing, a plain random forest is fitted instead, with a
warning. Every predictor with positive permutation importance (mean
decrease in accuracy, or `%IncMSE` for regression) is kept, or, if fewer
than `n_features` qualify (1 when `n_features = NULL`), the top
`n_features` by that importance. This fallback is a weak screen: roughly
half of pure-noise predictors have positive permutation importance. With
`n_features` supplied, the selection is then truncated to the
`n_features` selected predictors with the highest `scores`, so the
fallback keeps exactly that many (capped at the number available).

### Which numbers are optimistic

Selection runs on the training split only, so the test rows never inform
which features survive. The final model's tuning cross-validation reuses
the training rows the selector has already seen, though. With
`feature_select = TRUE`, the resampled metrics in `model$results` and
`model$resample` (what printing `model` shows) are therefore biased
upward, and on pure noise they can approach perfect accuracy. Taking the
best point of `tune_grid` adds further optimism. Only
`details$performance`, computed on the untouched test split, is an
honest estimate.

Class-imbalance handling (`class_imbalance = TRUE`, classification only)
up-samples within each cross-validation resample via
`caret::trainControl(sampling = "up")`, so duplicated rows do not leak
across folds.

### Reading the performance

For regression, `details$performance` holds `RMSE`, `MAE` and `Rsquared`
from
[`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html).
`Rsquared` is the squared Pearson correlation between test predictions
and observations, not `1 - SSE/SST`. It is never negative and ignores
bias and scale errors (predictions of `2 * y` score 1), so it cannot be
compared with an `R2` defined as `1 - SSE/SST`, such as the one from
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md).
For classification it is a
[`caret::confusionMatrix`](https://rdrr.io/pkg/caret/man/confusionMatrix.html),
whose `overall` element holds `Accuracy` and Cohen's `Kappa`. Test rows
with a missing prediction are dropped from the metrics with a warning.

### Data requirements

- Missing values in the predictors or the target are an error, and so
  are infinite values.

- For classification, no class label may be the empty string. A numeric
  target is coerced to a factor (with a message) only when it has at
  most 10 unique values; more than 10 is an error suggesting
  `task = "regression"`. For regression the target must be numeric.

- A factor or character predictor with fewer than two distinct
  non-missing values is an error naming the column. So is one left with
  a single level in the training rows because its other levels fell only
  into the test rows.

- Unused factor levels are dropped before encoding, so they produce no
  all-zero dummy columns.

- Dummy columns are named by caret's `dummyVars()`: `color.green` for a
  factor with three or more levels, `g1` for a two-level factor `g`. If
  a dummy name matches another column or the target (factor `g` with
  levels `"0"` and `"1"` next to a numeric `g1`), the call stops and
  asks for a rename.

### Packages, seeds and parallel runs

`fs_svm()` always needs the suggested packages caret and kernlab. It
also needs e1071 for classification, randomForest for
`select_method = "rf_rfe"` with `feature_select = TRUE`, and doParallel
and foreach when `n_cores > 1`.

`seed` is applied for the duration of the call and the previous RNG
state is restored afterwards. When `n_cores > 1`, caret's resampling
controls get `seeds = NULL`, so caret draws one seed per resample from
the locally seeded RNG. That is what makes two seeded parallel runs
agree. The workers also get L'Ecuyer-CMRG streams, but those alone would
not be enough, because foreach hands each resample to whichever worker
is free. A seeded parallel run need not match a seeded sequential one.
See [Reproducibility and
parallelism](https://elkronos.github.io/featR/articles/reproducibility.html).

## See also

The [Wrapper
methods](https://elkronos.github.io/featR/articles/wrappers.html) guide,
which runs this function on a worked example.

## Examples

``` r
# \donttest{
if (requireNamespace("caret", quietly = TRUE) &&
    requireNamespace("kernlab", quietly = TRUE) &&
    requireNamespace("e1071", quietly = TRUE)) {
  res <- fs_svm(
    data = iris,
    target = "Species",
    task = "classification",
    nfolds = 3,
    kernel = "linear",
    tune_grid = data.frame(C = 1),
    seed = 42
  )
  res$details$performance

  # SVM-RFE keeps the two most useful measurements
  sel <- fs_svm(
    data = iris,
    target = "Species",
    task = "classification",
    nfolds = 3,
    kernel = "linear",
    tune_grid = data.frame(C = 1),
    feature_select = TRUE,
    select_method = "svm_rfe",
    n_features = 2,
    seed = 42
  )
  sel$selected
  sel$scores
}
#> Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#>    0.1598883    0.3103668    4.5281035    5.1138497 
# }
```
