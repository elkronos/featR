# Train and evaluate an SVM, with optional SVM-RFE feature selection

Trains an SVM classifier or regressor using caret (with the kernlab
engines), with options for dummy encoding of predictors, feature
selection, class-imbalance handling, and hyperparameter tuning via
cross-validation. Optional parallel training uses an explicit worker
count.

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

  A data frame containing predictors and the target, with unique,
  non-empty column names.

- target:

  A string naming the target variable. For classification, no class
  label may be the empty string.

- task:

  Either `"classification"` or `"regression"`. Required; there is no
  default, because guessing it from the target is exactly the mistake
  this argument exists to prevent.

- train_ratio:

  Training set proportion, strictly between 0 and 1 (default `0.7`).

- nfolds:

  Number of CV folds for hyperparameter tuning, a whole number greater
  than 1 (default `5`). Also the number of folds used by the SVM-RFE
  subset-size search (clamped there to at most the number of training
  rows); `select_method = "rf_rfe"` ignores it and uses 10 folds.

- kernel:

  One of `"linear"` (default), `"radial"`, or `"polynomial"`.

- tune_grid:

  Optional tuning grid data frame. If `NULL`, a default grid for the
  chosen kernel is used.

- feature_select:

  Logical; if `TRUE`, run feature selection on the dummy-encoded
  training predictors (default `FALSE`).

- select_method:

  Which selector to run when `feature_select = TRUE`: `"svm_rfe"`
  (default, true SVM-RFE, linear kernel only) or `"rf_rfe"`
  (random-forest screening, any kernel). Ignored when
  `feature_select = FALSE`, and so is the linear-kernel requirement,
  which is only enforced when SVM-RFE will actually run. An unrecognized
  value is always an error.

- n_features:

  Optional whole number of features to keep, capped at the number of
  encoded predictors. For `"svm_rfe"` the top `n_features` ranked
  features are kept and the cross-validated size search is skipped; for
  `"rf_rfe"` it truncates the selection to its `n_features`
  highest-scoring entries and is the minimum number of features the
  random-forest fallback keeps (see Details). Ignored when
  `feature_select = FALSE`. Default `NULL` (the size is chosen
  automatically).

- class_imbalance:

  Logical; if `TRUE` and the task is classification, up-samples classes
  within CV resampling (default `FALSE`).

- seed:

  Optional whole-number seed (within the R integer range; fractional
  values are an error, not truncated), applied locally for the duration
  of the call and restored afterwards. When `n_cores > 1`, caret's
  resampling controls get `seeds = NULL`, so caret draws one seed per
  resample from the locally seeded RNG; that is what makes two seeded
  parallel runs agree. The workers also get L'Ecuyer-CMRG streams, but
  those alone would not, because foreach hands resamples to whichever
  worker is free. A seeded parallel run need not equal a seeded
  sequential one. Default `NULL` (never seeds by default).

- verbose:

  Logical; if `TRUE`, report progress (including each SVM-RFE
  elimination step). Default `FALSE`.

- n_cores:

  Number of parallel workers (default `1`, sequential). Requests are
  capped at the detected core count; when greater than 1 a cluster is
  created for the duration of the call and stopped on exit.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of selected encoded feature names. When
  `feature_select = FALSE` this is every encoded predictor. With
  `"svm_rfe"` it is the surviving subset, ordered from most to least
  important; with `"rf_rfe"` it is the subset in the order
  [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html) reports it,
  or, when `n_features` truncated it, ordered by decreasing `scores`.

- scores:

  Named numeric vector covering every encoded predictor, not just the
  survivors: the SVM-RFE criterion (the squared primal weights `w^2` of
  the first, full-feature fit) or, for `select_method = "rf_rfe"`, the
  mean random-forest importance recorded across resamples (`NA` for
  predictors `rfe()` never scored, or the mean decrease in node impurity
  when the fallback ran). `NULL` when `feature_select = FALSE`, because
  no selector produced comparable scores.

- method:

  `"svm_"` followed by the kernel, for example `"svm_linear"`. The
  selector that ran, if any, is reported in `details$selection$method`.

- task:

  `"classification"` or `"regression"`.

- model:

  The fitted [`caret::train`](https://rdrr.io/pkg/caret/man/train.html)
  object. With `feature_select = TRUE` its resampled metrics are
  optimistic (see Details); use `details$performance`.

- details:

  A list with `test_set` (the test split with its coerced target and
  aligned factor levels), `predictions` (test-set predictions in the row
  order of `test_set`: a factor for classification, an unnamed numeric
  vector for regression), `performance` (a
  [`caret::confusionMatrix`](https://rdrr.io/pkg/caret/man/confusionMatrix.html)
  for classification, or a named `RMSE`/`Rsquared`/`MAE` vector from
  [`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html)
  for regression, where `Rsquared` is the squared correlation; see
  Details), `selection` (`NULL` when no selection ran, otherwise a list
  with `method`, `ranking` from most to least important, `scores`, and
  the size search's `sizes`, `size_scores` and `size_metric` – those
  last three being `NULL`, `NULL` and `NA` whenever no size search ran,
  which is always the case for `"rf_rfe"` and for `"svm_rfe"` with an
  explicit `n_features`), `encoder` (the fitted
  [`caret::dummyVars`](https://rdrr.io/pkg/caret/man/dummyVars.html)
  object) and `n_features` (the number of encoded predictors considered,
  counted before any were dropped).

- call:

  The matched call.

## Details

This is the wrapper to reach for when the selector and the final model
should belong to the same family: SVM-RFE ranks features by the weights
of a linear SVM rather than by an external proxy criterion, and the
returned object carries the tuned model and its held-out performance
alongside the chosen features. That comes at a price – a full SVM fit at
every elimination step, plus a cross-validated size search – so on wide
data screen first with a filter such as
[`fs_supervised`](https://elkronos.github.io/featR/reference/fs_supervised.md).

- Feature selection (`feature_select = TRUE`) defaults to **SVM-RFE**
  (Guyon, Weston, Barnhill and Vapnik, 2002). A linear SVM is fitted on
  the centered and scaled encoded training matrix, the features are
  ranked by the squared primal weight `w^2` (recovered from the fit's
  support vectors and coefficients, summed over the pairwise problems of
  a multi-class fit), the lowest-ranked feature is dropped (the lowest
  10 percent while more than 50 features remain), and the SVM is
  *refitted* on the reduced set until one feature is left. Reversing the
  elimination order gives the ranking, so rank 1 is the feature
  eliminated last. SVM-RFE requires `kernel = "linear"`, because the
  ranking criterion is the primal weight vector, which only exists for a
  linear kernel; combining it with another kernel is an error that
  points at `select_method = "rf_rfe"`. The elimination and size-search
  fits use a fixed cost of `C = 1` and are separate from the final
  model, which is tuned over `tune_grid`.

- How many features SVM-RFE keeps: with `n_features` supplied, exactly
  that many (the top of the ranking). Otherwise a short ladder of
  candidate sizes – the powers of two up to the number of features, plus
  the full size, trimmed to at most six entries but always including 1
  and the full size – is scored by nested `nfolds`-fold cross-validation
  with the same linear SVM, on folds shared by every candidate size.
  Inside each fold the standardization and the whole elimination are
  re-run on that fold's training rows only, and the top features of that
  fold's own ranking are scored on its held-out rows, so no candidate
  size is scored by a ranking that has already seen its held-out rows.
  The winner is the size with the highest mean accuracy (classification)
  or the lowest mean RMSE (regression), ties going to the smaller size,
  and the kept features are the top of the ranking computed on the whole
  training split; if no size could be scored, all features are kept with
  a warning. Each entry of `details$selection$size_scores` is an honest
  estimate for its own size, but the entry of the *chosen* size is the
  best of up to six noisy estimates and is therefore optimistic (on pure
  noise it typically sits a few points above chance); use
  `details$performance` for an unbiased estimate.

- `select_method = "rf_rfe"` keeps the older random-forest screening
  ([`caret::rfFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html))
  and works with every kernel. It runs its own 10-fold cross-validation
  over subset sizes 1 to p, independent of `nfolds`. Numerically
  constant encoded predictors are removed before screening (score `NA`,
  never selected). If `rfe()` fails or selects nothing, a plain random
  forest is fitted instead, with a warning, and every predictor with a
  positive permutation importance (mean decrease in accuracy, or
  `%IncMSE` for regression) is kept – or, if fewer than `n_features` (1
  when `n_features = NULL`) qualify, the top `n_features` by that
  importance. This fallback is a weak screen: roughly half of pure-noise
  predictors have positive permutation importance. With `n_features`
  supplied the selection is truncated to the `n_features` selected
  predictors with the highest reported `scores`, so the fallback keeps
  exactly that many (capped at the number available).

- Selection always runs on the training split only, so the test set
  never informs which features survive. The final model's tuning
  cross-validation, however, reuses the training rows the selector has
  already seen, so with `feature_select = TRUE` the resampled metrics in
  `model$results` and `model$resample` (what printing `model` shows) are
  optimistically biased – on pure noise they can approach perfect
  accuracy – and the maximum over `tune_grid` adds further optimism.
  Only `details$performance`, computed on the untouched test split, is
  an honest estimate.

- Regression performance (`details$performance`) comes from
  [`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html):
  `RMSE`, `MAE`, and `Rsquared`, which is the *squared Pearson
  correlation* between test predictions and observations – not
  `1 - SSE/SST`. It is therefore never negative and ignores bias and
  scale errors (predictions of `2 * y` score 1), so it is not comparable
  with an `R2` defined as `1 - SSE/SST` (for example
  [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)'s).
  Classification performance is a
  [`caret::confusionMatrix`](https://rdrr.io/pkg/caret/man/confusionMatrix.html),
  whose `overall` element holds `Accuracy` and Cohen's `Kappa`.

- Class-imbalance handling (`class_imbalance = TRUE`, classification
  only) up-samples *within* each cross-validation resample via
  `caret::trainControl(sampling = "up")`, so no resampled rows leak
  across folds.

- After the train/test split, factor (and character) predictor levels in
  the test set are aligned to the levels observed in the training rows
  (unused factor levels are dropped, so they produce no all-zero dummy
  columns); test rows containing levels unseen in training are dropped
  with a warning. If a dummy column name (`<factor><level>`) coincides
  with another column or the target, for example factor `g` with level
  `"1"` next to a numeric `g1`, the call stops and asks for a rename. A
  factor or character predictor with fewer than two distinct non-missing
  values is an error naming the column, and so is one left with a single
  level in the training rows because its other levels fell only into the
  held-out rows. Non-finite (`Inf`) values in the predictors or target
  are an error.

- A numeric target with `task = "classification"` is coerced to a factor
  (with a message) only when it has at most 10 unique values; more than
  10 unique values is an error suggesting `task = "regression"`.

Suggested packages required at runtime: caret and kernlab always, e1071
for classification metrics, randomForest when `feature_select = TRUE`
and `select_method = "rf_rfe"`, and doParallel/foreach when
`n_cores > 1`.

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
