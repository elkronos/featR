# Recursive feature elimination with held-out evaluation

Finds how few predictors a model needs before its resampled performance
starts to drop. `fs_recursivefeature()` splits the data into stratified
training and test rows, runs
[`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html) on the training
rows, and scores the chosen subset on the test rows. It can also one-hot
encode the predictors first and fit a final caret model on the selected
ones.

## Usage

``` r
fs_recursivefeature(
  data,
  target,
  sizes = NULL,
  train_ratio = 0.8,
  rfe_control = list(method = "cv", number = 5),
  train_control = list(method = "cv", number = 5),
  model_method = "rf",
  handle_categorical = FALSE,
  return_final_model = FALSE,
  seed = NULL,
  verbose = FALSE,
  parallel = FALSE
)
```

## Arguments

- data:

  A data.frame or data.table holding the target and the candidate
  predictors (every other column). It needs at least one row, unique
  non-empty column names, and at least two candidate predictors after
  any one-hot encoding. It is converted to a plain data.frame on entry.

- target:

  Single string naming the target column; a column index is not
  accepted. A factor, character, or logical target means classification
  (unused factor levels are dropped), anything else regression. The
  target may not contain missing or infinite values.

- sizes:

  Whole-number subset sizes to evaluate; fractional sizes are an error.
  Default `NULL` uses `1:p`, where `p` is the predictor count after any
  one-hot encoding. Values outside `[1, p]` are dropped with a warning,
  and the call stops if none are left.

- train_ratio:

  Training proportion of the stratified split, strictly between 0 and 1.
  Default 0.8.

- rfe_control:

  List of arguments for
  [`caret::rfeControl()`](https://rdrr.io/pkg/caret/man/rfeControl.html).
  Default `list(method = "cv", number = 5)`.

  - `method` and `number` are required. `method` is one of `"cv"`,
    `"repeatedcv"`, `"LOOCV"`, `"boot"`, `"boot632"`, `"LGOCV"` or
    `"test"` (matched ignoring case), and `number` must be at least 2
    for `"cv"` and `"repeatedcv"`. With `"repeatedcv"` and no `repeats`,
    caret's default of 1 is used and a warning says so.

  - `functions` is the caret RFE function set, default
    [`caret::rfFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html). A
    supplied set must contain the functions `summary`, `fit`, `pred`,
    `rank`, `selectSize` and `selectVar`.

  - `index`, `indexOut` and `indexFinal` are an error: resampling runs
    on the internal training split, so row numbers built for `data`
    would point at the wrong rows.

  - `allowParallel` is dropped with a warning; use `parallel` instead.
    `verbose`, if present, overrides the `verbose` argument for
    [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html).

- train_control:

  List of arguments for
  [`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html),
  used only when `return_final_model = TRUE` and then checked, together
  with `model_method`, before the search starts. It must contain
  `method`, and also `number` unless `method = "none"` (at least 2 for
  `"cv"` and `"repeatedcv"`). `index`, `indexOut` and `indexFinal` are
  an error, for the same reason as in `rfe_control`. Unless set here,
  `allowParallel` follows `parallel`, so a sequential call does not use
  a foreach backend you registered yourself. Default
  `list(method = "cv", number = 5)`.

- model_method:

  Single string: the caret model key for the final model, such as `"rf"`
  or `"lm"`. It must be in caret's built-in model library. Used only
  when `return_final_model = TRUE`. Default `"rf"`.

- handle_categorical:

  Logical; one-hot encode the predictors with full-rank dummies, fitted
  on the training rows and applied to the test rows. A dummy name
  (`<variable><level>`) that matches another column is an error.
  Function sets that work on a model matrix, such as
  [`caret::lmFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html),
  rank dummy columns rather than factors, so they need
  `handle_categorical = TRUE` when factor predictors are present;
  without it the call stops with an error that says so. Default `FALSE`.

- return_final_model:

  Logical; train a final caret model on the training rows using the
  selected predictors and return it as `model` (the `rfe` object stays
  in `details$rfe`). Near-zero-variance and linearly dependent
  predictors are dropped from the selected set first, each with a
  warning, and the survivors are recorded in
  `details$final_model_variables`. Default `FALSE`.

- seed:

  Optional whole number within the range of an R integer. Fractional or
  out-of-range values are an error, not truncated. Default `NULL`, which
  never seeds. See "Seeds and parallel runs" in Details.

- verbose:

  Logical; print progress messages and let
  [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html) report its
  own progress, unless `rfe_control$verbose` overrides the latter.
  Default `FALSE`.

- parallel:

  Logical. `TRUE` registers a two-worker PSOCK cluster (capped at the
  detected core count) for the duration of the call, then stops it and
  restores whichever foreach backend was registered before. Requires the
  suggested packages 'foreach' and 'doParallel'. The RFE resampling and
  the final model's resampling both follow this setting. A custom
  function set such as
  [`caret::caretFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html)
  builds its own inner
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html) control
  and is not covered. Default `FALSE`.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of the predictors RFE kept: caret's `optVariables` at
  the optimal subset size.

- scores:

  Named numeric vector with one entry per candidate predictor: its
  importance in the full-size fit (all candidates present), averaged
  over every resample.
  [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html) on the
  `rfe` object instead averages a predictor only over the resamples
  where it survived to the optimal size, which inflates marginal
  predictors and omits predictors that never survived. When the function
  set records no per-resample importance, the
  [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html) values
  are used instead. `NULL` when neither is available.

- method:

  `"rfe"`.

- task:

  `"classification"` or `"regression"`, inferred from the target.

- model:

  The final [`caret::train`](https://rdrr.io/pkg/caret/man/train.html)
  model when `return_final_model = TRUE`, otherwise the `rfe` object.

- details:

  A list with:

  - `rfe`: the caret `rfe` object, present even when `model` holds the
    final model.

  - `optimal_size`: the subset size RFE chose.

  - `test_metrics`:
    [`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html)
    on the test rows, the only held-out estimate on the result.

  - `resampling_results`: the RFE resampling summary over candidate
    sizes, computed inside the training rows.

  - `variable_importance`: the
    [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html)
    data.frame. It averages each predictor only over the resamples where
    it survived to the optimal size, and `selected` follows caret's
    `optVariables` rule, which is weighted the same way.

  - `preprocessor`: the `dummyVars` encoder, or `NULL` when
    `handle_categorical = FALSE`.

  - `train_index`, `test_index`: row indices into `data` for the two
    partitions.

  - `final_model_variables`: the predictors the final model used after
    near-zero-variance and linear-combination filtering, or `NULL` when
    no final model was requested.

  - `n_features`: the number of candidate predictors offered to RFE,
    counted after one-hot encoding and after constant predictors were
    removed.

- call:

  The matched call.

## Details

Recursive feature elimination (RFE) fits a model on all candidate
predictors and ranks them by importance, then refits on the top `k` for
each subset size `k` in `sizes`, inside every resample of `rfe_control`.
The size with the best resampled performance wins, so you do not have to
pick a cutoff yourself. The price is one model fit per candidate size
per resample, which makes RFE one of the slowest methods in featR. The
answer is also tied to the model family in `rfe_control$functions`
(random forests by default): it tells you which predictors that model
needs, not which predictors are informative in general.

The honest performance estimate is `details$test_metrics`. The
[Validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.html)
article lists which numbers in each featR result are held out and which
are optimistic.

### Required packages

'caret' is always required. The default function set,
[`caret::rfFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html), fits
random forests and needs 'randomForest'; so does the default final model
(`model_method = "rf"`) when `return_final_model = TRUE`. To avoid
random forests, pass another set, for example
`rfe_control = list(method = "cv", number = 5, functions = caret::lmFuncs)`.
Classification also needs 'e1071', because the metrics come from
[`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html).
`parallel = TRUE` needs 'foreach' and 'doParallel'. All of these are
checked before any model is fitted.

### What the held-out rows see

Everything learned from data is learned on the training rows: the
one-hot encoder, the factor levels the test columns are aligned to, the
elimination itself, and the final model. The test rows play no part in
choosing either the predictors or the subset size.

`details$test_metrics` applies
[`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html)
to the test-row predictions of the fitted `rfe` object, which is caret's
refit on all training rows restricted to the optimal subset. It is the
only held-out estimate on the result. Two other summaries reuse the
training rows and are optimistic:

- `details$resampling_results` is the resampled performance at each
  candidate size, computed inside the training rows. The size RFE picked
  is the best of these, so its entry is biased upward.

- With `return_final_model = TRUE`, `model$results` reports
  `train_control` resampling inside the same training rows, on
  predictors that were already chosen using those rows.

### Reading the metrics

The metrics are caret's. For regression they are `RMSE`, `Rsquared` and
`MAE`, where `Rsquared` is the squared Pearson correlation between
predictions and observations, not `1 - SSE/SST`. It is never negative
and ignores bias and scale errors, so it cannot be compared with an `R2`
defined as `1 - SSE/SST`, such as `details$metrics$R2` from
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md).
For classification they are `Accuracy` (the proportion of test rows
classified correctly) and `Kappa` (Cohen's kappa, accuracy corrected for
chance agreement).

### Data preparation and requirements

- Infinite values (`Inf`, `-Inf`) anywhere in `data` are an error.

- Missing values in the training predictors are an error; impute or drop
  incomplete rows before calling. Missing values in the test rows are
  not imputed. Depending on the function set,
  [`predict()`](https://rdrr.io/r/stats/predict.html) then either stops
  (random forests) or returns `NA` for those rows, which
  [`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html)
  leaves out of `details$test_metrics`. A warning reports how many rows
  were left out.

- Character and logical predictors become factors. Factor predictors
  keep only the levels observed in the training rows; a test value with
  a level the training rows never saw becomes missing, with a warning.

- Predictors that are constant on the training rows (a single value, or
  a factor with one observed level) are removed with a warning before
  RFE. They carry no information, and a random forest fitted on a single
  constant predictor never returns.

- Predictor names that are not syntactic R names, such as `"a b"`, break
  caret's function sets, so the `rfe` object and the final model use
  their [`make.names()`](https://rdrr.io/r/base/make.names.html)
  versions. `selected`, `scores`, `details$variable_importance` and
  `details$final_model_variables` report the original names.

- With `handle_categorical = TRUE`, the encoded test rows are checked
  against the input row count, so an encoding that loses rows is an
  error instead of a misaligned metric.

### Seeds and parallel runs

`seed` covers the split and the RFE resampling. It is applied for the
duration of the call only, and the previous RNG state is restored on
exit. With `seed = NULL` the call draws from your current RNG stream
without reseeding it. Under `parallel = TRUE`, a seed also sets
`rfe_control$seeds = NULL` unless you supplied `seeds`, so caret draws
one seed per resample from the locally seeded RNG. That is what makes
two seeded parallel runs agree. The workers also get L'Ecuyer-CMRG
streams, but those alone would not be enough, because foreach hands each
resample to whichever worker is free. A seeded parallel run need not
match a seeded sequential one, because the sequential run keeps caret's
default `seeds = NA`. See [Reproducibility and
parallelism](https://elkronos.github.io/featR/articles/reproducibility.html).

## See also

The [Wrapper
methods](https://elkronos.github.io/featR/articles/wrappers.html) guide,
which runs this function on a worked example.

## Examples

``` r
# \donttest{
if (requireNamespace("caret", quietly = TRUE) &&
    requireNamespace("randomForest", quietly = TRUE) &&
    requireNamespace("e1071", quietly = TRUE)) {
  res <- fs_recursivefeature(
    iris,
    target = "Species",
    sizes = 1:4,
    rfe_control = list(method = "cv", number = 3),
    seed = 42
  )
  print(res$selected)
  print(res$details$optimal_size)
  # the only held-out estimate on the object
  print(res$details$test_metrics)
}
#> [1] "Petal.Length" "Petal.Width"  "Sepal.Length"
#> [1] 3
#>  Accuracy     Kappa 
#> 0.9333333 0.9000000 
# }
```
