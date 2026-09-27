# Random forest importance and held-out evaluation

Answers "how much does each predictor contribute to a random forest, and
how well does that forest do on rows it has never seen?" The pipeline
runs in this order: optional preprocessing of the full table, target
cleaning, optional downsampling, a stratified train/test split, the
optional `control$feature_select` hook on the training rows,
character-to-factor coercion and level alignment, near-zero-variance
removal, imputation, training (optionally across several workers), and
evaluation on the held-out rows. Permutation importance is reported as
the per-feature score.

## Usage

``` r
fs_randomforest(
  data,
  target,
  task = c("classification", "regression"),
  control = list(),
  seed = NULL,
  verbose = FALSE,
  n_cores = 1L
)
```

## Arguments

- data:

  A data.frame or data.table with at least one row and one column,
  holding the target and the candidate predictors (every other column).
  `Date` columns are converted to numeric before modeling.

- target:

  Single string naming the target column of `data`; a column index is
  not accepted.

- task:

  One of `"classification"` (the default) or `"regression"`, matched
  with [`match.arg()`](https://rdrr.io/r/base/match.arg.html). The task
  is not inferred from the target: when `task` is left at its default
  and the target is numeric, a warning says so, since every distinct
  value would become a class. A classification target is coerced with
  [`as.factor()`](https://rdrr.io/r/base/factor.html) and must retain at
  least two levels; a regression target is coerced with
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html). Either way,
  rows whose target is `NA` after coercion are dropped with a warning.

- control:

  List of method-specific options; every accepted entry and its default
  is listed under Details. Unknown entries are an error, not a silent
  no-op. Default [`list()`](https://rdrr.io/r/base/list.html), i.e. all
  defaults.

- seed:

  Optional single whole number for reproducibility, within the range of
  an R integer; fractional or out-of-range values are an error rather
  than being truncated. Applied for the duration of the call only; the
  previous RNG state is restored on exit. With more than one worker it
  is also handed to
  [`parallel::clusterSetRNGStream()`](https://rdrr.io/r/parallel/RngStream.html),
  and is then the only way to make the run reproducible. Default `NULL`
  (the RNG is never seeded unless requested).

- verbose:

  Logical; emit progress messages (split sizes, how many predictors the
  hook kept, the tree/worker counts, and a note when AUC is skipped
  because 'pROC' is missing). Default `FALSE`.

- n_cores:

  Whole number \>= 1. Number of workers used to grow the forest. Default
  `1L` (sequential; no cluster is created). Capped at the detected core
  count and at `control$ntree`; an effective value above 1 requires the
  suggested packages 'foreach' and 'doParallel', and costs the OOB
  metrics as described below.

## Value

An object of class `fs_result` with:

- selected:

  Character vector. The predictors kept by `control$feature_select` when
  a hook is supplied; otherwise every predictor that reached the forest,
  since a plain random forest ranks rather than selects. Ordered by
  decreasing importance when importance was computed.

- scores:

  Named numeric vector of permutation importance from
  `randomForest::importance(type = 1)` – mean decrease in accuracy for
  classification, mean increase in MSE for regression (the column
  randomForest names with a percent sign, though it is not a percentage)
  – scaled or not according to `control$scale_importance`. `NULL` when
  `control$importance = FALSE`.

- method:

  "randomforest".

- task:

  "classification" or "regression", echoing the `task` argument.

- model:

  The fitted `randomForest` object; the result of
  [`randomForest::combine()`](https://rdrr.io/pkg/randomForest/man/combine.html)
  when more than one worker was used.

- details:

  A list with `metrics` (a named list: `accuracy`, `kappa`, `auc` for
  classification, or `RMSE`, `MAE`, `R2` for regression), `predictions`
  (test-set predictions), `probabilities` (test-set class probability
  matrix, classification only, `NULL` if
  [`predict()`](https://rdrr.io/r/stats/predict.html) cannot produce
  one), `importance` (data.frame with columns `feature` and
  `importance`, sorted descending, `NULL` when
  `control$importance = FALSE`), `confusion` (Observed x Predicted
  table, classification only), `oob` (`accuracy` for classification or
  `RMSE` for regression; `NULL` when trained in parallel or switched
  off), `feature_names` (predictors the forest actually used),
  `train_index` (integer training rows of the cleaned and optionally
  down-sampled data), `test_data` (the held-out rows, only when
  `control$return_test_data = TRUE`, `NULL` otherwise), `control` (the
  merged control list) and `n_features` (candidate predictors after
  cleaning and before selection).

- call:

  The matched call.

## Details

Random forests take any mix of numeric, factor, character, and `Date`
predictors and need no scaling, which makes this a good default when the
feature types are messy. The main caveat is that a forest *ranks* rather
than *selects*: unless you supply `control$feature_select`, `selected`
is every predictor that reached the forest, ordered by importance, and
choosing the cutoff is left to you. Permutation importance also tends to
favor predictors with many distinct values and to split credit between
correlated predictors, so treat close scores as ties.

**control list (every accepted entry, with its default)**:

- `train_ratio = 0.75` – training proportion of the stratified split;
  must be strictly between 0 and 1.

- `sample_size = NULL` – optional whole number. When supplied, the data
  is downsampled to approximately this many rows *before* the split
  (proportionally within each target class for classification, uniformly
  at random for regression). Values above the available row count are
  capped. `NULL` keeps every row.

- `ntree = 500` – number of trees to grow; whole number \>= 1.

- `importance = TRUE` – compute permutation importance. When `FALSE`,
  `scores` and `details$importance` are `NULL` and `selected` keeps
  plain column order.

- `scale_importance = TRUE` – passed as `scale` to
  [`randomForest::importance()`](https://rdrr.io/pkg/randomForest/man/importance.html),
  which divides each permutation importance (mean decrease in accuracy,
  or mean increase in MSE) by its standard error over trees. Set to
  `FALSE` for the raw, unscaled permutation importance. With more than
  one worker the standard error is pooled across the sub-forests, so
  scaled scores do not depend on `n_cores`.

- `mtry = NULL` – predictors sampled at each split. `NULL` means
  `floor(sqrt(p))` for classification and `floor(p / 3)` for regression;
  any value, supplied or derived, is clamped to `[1, p]`, where `p`
  counts the predictors that survive selection and near-zero-variance
  removal.

- `nodesize = NULL` – minimum size of terminal nodes; whole number
  \>= 1. `NULL` leaves `randomForest`'s own default (1 for
  classification, 5 for regression).

- `maxnodes = NULL` – maximum number of terminal nodes per tree; whole
  number \>= 2, or `NULL` for no limit.

- `sampsize = NULL` – rows drawn to grow each tree, passed to
  `randomForest`. A single number for regression (a vector is an error);
  one number per class is allowed for classification. Entries must be
  whole numbers \>= 1 (anything else is an error). Values are clamped to
  the training row count, and `NA` entries are replaced by the full
  training size when `replace = TRUE` and by `ceiling(0.632 * n)`
  otherwise.

- `classwt = NULL` – class priors for classification, forwarded to
  `randomForest` unchanged and unvalidated.

- `strata = NULL` – stratification variable for the per-tree sampling,
  forwarded to `randomForest` unchanged and unvalidated.

- `replace = TRUE` – sample rows with replacement when growing each
  tree.

- `preprocess = NULL` – function `dt -> dt`, applied to the full data
  before the split. It must return a data.frame/data.table that still
  contains the target. See the leakage note below.

- `feature_select = NULL` – function `dt -> dt`; runs on the training
  split only, and must select existing columns while retaining the
  target – see the note below.

- `impute = TRUE` – median (numeric) or modal (factor/character/logical)
  values learned on the training data for every predictor and applied to
  NAs in train and test.

- `drop_zerovar = TRUE` – near-zero variance removal with
  [`caret::nearZeroVar()`](https://rdrr.io/pkg/caret/man/nearZeroVar.html),
  using training data only.

- `oob = TRUE` – include out-of-bag metrics in `details$oob` (`accuracy`
  for classification, `RMSE` for regression); see the note below about
  parallel training.

- `return_test_data = FALSE` – when `TRUE`, the held-out rows are
  returned in `details$test_data`.

- `positive_class = NULL` – optional level name treated as the positive
  class for binary AUC. Falls back to the second factor level both by
  default and, silently, when the string does not name a level of the
  target.

Unknown `control` entries are rejected, so typos and arguments that
moved out of `control` (`seed`, `n_cores`, the former `split_ratio`)
fail loudly.

**Selection is train-only**: `control$feature_select` runs *after* the
train/test split and sees the training rows only, so a selection rule
that looks at the target no longer leaks the held-out rows into the
reported test metrics. Because the same columns must be applied to the
test rows, the hook may only subset and reorder existing columns; put
any feature engineering in `control$preprocess`, which runs on the full
data before the split – subject to the caveat in the next paragraph.
Near-zero-variance removal happens after the hook, so
`details$feature_names` (the predictors the forest actually used) can be
a subset of `selected`.

**What still sees the full data**: `control$preprocess` runs before the
split by design, so any quantity it learns – an imputation value, a
scaling constant, anything that consults the target – is computed from
the held-out rows too and will flatter `details$metrics`. Keep it to
row-wise transformations that do not depend on other rows. Optional
downsampling (`control$sample_size`) also happens before the split,
which is why `details$train_index` indexes the cleaned and down-sampled
table rather than the rows of the original `data`.

**OOB metrics and parallel training**: when more than one worker is
actually used (`n_cores` after the caps described above) the forest is
assembled with
[`randomForest::combine()`](https://rdrr.io/pkg/randomForest/man/combine.html),
which drops the out-of-bag error structures (`err.rate`, `mse`,
`confusion`). In that case `details$oob` is `NULL` and a warning is
emitted if `control$oob = TRUE`. The held-out metrics in
`details$metrics` are unaffected; use `n_cores = 1` when you want the
OOB numbers.

Character predictors are converted to factors, with the levels learned
on the training split. Test values unseen in training become NA and are
imputed when `control$impute = TRUE`; with `control$impute = FALSE`
those NAs survive into
[`predict()`](https://rdrr.io/r/stats/predict.html), which
`randomForest` cannot handle. `details$metrics$auc` is filled in only
when the classification target has exactly two levels *and* the
suggested package 'pROC' is installed; it is `NA` otherwise, which
includes every multi-class fit. Regression fits report RMSE, MAE, and R2
instead, and R2 is `NA` when the held-out target has no variance.

## Examples

``` r
# \donttest{
if (requireNamespace("randomForest", quietly = TRUE) &&
    requireNamespace("caret", quietly = TRUE)) {
  res <- fs_randomforest(
    iris,
    target = "Species",
    task = "classification",
    control = list(ntree = 100),
    seed = 42
  )
  # every predictor, ranked: a forest ranks rather than selects
  print(res$selected)
  print(res$scores)
  print(res$details$metrics)
}
#> [1] "Petal.Length" "Petal.Width"  "Sepal.Length" "Sepal.Width" 
#> Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
#>    15.409812    12.423520     3.745021     1.804016 
#> $accuracy
#> [1] 0.9444444
#> 
#> $kappa
#> [1] 0.9166667
#> 
#> $auc
#> [1] NA
#> 
# }
```
