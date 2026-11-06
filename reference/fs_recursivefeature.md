# Recursive feature elimination with held-out evaluation

Answers "how few predictors can I keep before resampled performance
starts to fall off?" Splits the data into stratified train/test
partitions, optionally one-hot encodes the predictors (encoder fitted on
the training rows only), runs
[`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html) on the training
set, evaluates the fitted RFE model on the held-out test rows, and
optionally trains a final caret model on the training rows.

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

  A data.frame or data.table with at least one row and one column,
  holding the target and the candidate predictors (every other column),
  with unique, non-empty column names and at least two candidate
  predictors (after any one-hot encoding). It is converted to a plain
  data.frame on entry.

- target:

  Single string naming the target column of `data`; a column index is
  not accepted. A factor, character, or logical target means
  classification (unused factor levels are dropped), anything else
  regression. The target may not contain missing or infinite values.

- sizes:

  Vector of whole-number feature-subset sizes to evaluate (fractional
  sizes are an error). Default `NULL`, which uses `1:p`, where `p` is
  the predictor count after any one-hot encoding. Values outside
  `[1, p]` are dropped with a warning; if that leaves nothing, the call
  is an error rather than a silent empty run.

- train_ratio:

  Numeric, strictly between 0 and 1: the training proportion of the
  stratified split. Default 0.8.

- rfe_control:

  List of arguments for
  [`caret::rfeControl()`](https://rdrr.io/pkg/caret/man/rfeControl.html);
  must contain at least `method` and `number`. Default
  `list(method = "cv", number = 5)`. `method` must be one of `"cv"`,
  `"repeatedcv"`, `"LOOCV"`, `"boot"`, `"boot632"`, `"LGOCV"` or
  `"test"`, and `number` must be at least 2 for `"cv"`/`"repeatedcv"`.
  `index`, `indexOut` and `indexFinal` are not accepted (an error):
  resampling runs on the internal training split, so row numbers built
  for `data` would point at the wrong rows. `functions` selects the
  caret RFE function set and defaults to
  [`caret::rfFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html); a
  supplied set must contain the functions `summary`, `fit`, `pred`,
  `rank`, `selectSize` and `selectVar`. An `allowParallel` entry is
  dropped with a warning (use the `parallel` argument instead), while a
  `verbose` entry, if present, overrides the `verbose` argument for
  [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html). With
  `method = "repeatedcv"` and no `repeats`, caret's default of 1 is used
  and a warning says so.

- train_control:

  List of arguments for
  [`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html),
  used only when `return_final_model = TRUE`, and then validated
  (together with `model_method`) before the search starts. Must contain
  `method`, and also `number` unless `method = "none"` (at least 2 for
  `"cv"`/`"repeatedcv"`). `index`, `indexOut` and `indexFinal` are not
  accepted (an error), for the same reason as in `rfe_control`. Unless
  it is set here, `allowParallel` follows the `parallel` argument, so a
  foreach backend you registered yourself is not used by a sequential
  call. Default `list(method = "cv", number = 5)`.

- model_method:

  Single string; the caret model key used for the final model, for
  example `"rf"` or `"lm"`. Used only when `return_final_model = TRUE`.
  Default `"rf"`.

- handle_categorical:

  Logical; one-hot encode predictors with full-rank dummies (fitted on
  the training rows, applied to the test rows). A dummy name
  (`<variable><level>`) that collides with another column is an error.
  Function sets that work on a model matrix (for example
  [`caret::lmFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html))
  rank dummy columns rather than factors, so they need
  `handle_categorical = TRUE` when factor predictors are present;
  without it the call stops with an error saying so. Default `FALSE`.

- return_final_model:

  Logical; train a final caret model on the training rows using the
  selected features and return it as `model`, with the `rfe` object
  still available in `details$rfe`. Near-zero-variance and linearly
  dependent predictors are dropped from the selected set first, each
  with a warning, and what survives is recorded in
  `details$final_model_variables`. Default `FALSE`.

- seed:

  Optional single whole number within the range of an R integer;
  fractional or out-of-range values are an error rather than being
  truncated. It covers the split and the RFE resampling, is applied for
  the duration of the call only (the previous RNG state is restored on
  exit), and defaults to `NULL`, which never seeds. Under
  `parallel = TRUE` a seed also sets `rfe_control$seeds = NULL` (unless
  you supplied `seeds`), so caret draws one seed per resample from the
  locally seeded RNG; that is what makes two seeded parallel runs agree
  with each other (the workers additionally get L'Ecuyer-CMRG streams,
  but those alone would not, because foreach hands resamples to
  whichever worker is free). A seeded parallel run need not equal a
  seeded sequential one, because the sequential run keeps caret's
  default `seeds = NA`.

- verbose:

  Logical; print progress messages and let
  [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html) report its
  own progress, unless `rfe_control$verbose` overrides the latter.
  Default `FALSE`.

- parallel:

  Logical. If `TRUE`, registers a two-worker PSOCK cluster (capped at
  the detected core count) for the duration of the call, then stops it
  and restores whichever foreach backend was registered before the call;
  requires the suggested packages 'foreach' and 'doParallel'. The RFE
  resampling and the final model's resampling both follow this setting;
  a custom function set such as
  [`caret::caretFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html)
  builds its own inner
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html) control
  and is not covered. Default `FALSE`.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of the variables RFE kept (`optVariables` at the
  optimal subset size).

- scores:

  Named numeric vector with one entry per candidate predictor: its
  importance in the full-size fit (all candidates present), averaged
  over every resample. Unlike
  [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html) on the
  `rfe` object, this does not average a variable only over the resamples
  where it survived to the optimal size, so marginal variables are not
  inflated and no candidate is missing. When the function set records no
  per-resample importance, the
  [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html) values
  are used instead; `NULL` when there is nothing usable.

- method:

  "rfe".

- task:

  "classification" or "regression", inferred from the target.

- model:

  The final [`caret::train`](https://rdrr.io/pkg/caret/man/train.html)
  model when `return_final_model = TRUE`, otherwise the `rfe` object.

- details:

  A list, in snake_case, with `rfe` (the caret `rfe` object, always
  present even when `model` holds the final model), `optimal_size` (the
  subset size RFE chose), `test_metrics`
  ([`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html)
  on the held-out rows – the only held-out estimate on the object),
  `resampling_results` (the RFE resampling summary over candidate sizes,
  computed inside the training rows), `variable_importance` (the
  [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html)
  data.frame, which averages each variable only over the resamples where
  it survived to the optimal size; `selected` follows caret's
  `optVariables` rule, which is weighted the same way), `preprocessor`
  (the `dummyVars` encoder, or `NULL` when
  `handle_categorical = FALSE`), `train_index` and `test_index` (row
  indices into `data` for the two partitions), `final_model_variables`
  (predictors the final model actually used after NZV/linear-combination
  filtering, or `NULL` when no final model was requested) and
  `n_features` (candidate predictors offered to RFE, counted after
  one-hot encoding when `handle_categorical = TRUE` and after constant
  predictors were removed).

- call:

  The matched call.

## Details

RFE is a wrapper method: it refits the underlying model once per
candidate subset size per resample, so it is by far the most expensive
method here, and its answer is specific to the model family in
`rfe_control$functions` rather than being a general statement about the
features. In exchange, the subset it reports is tuned to the model you
actually intend to use, and the subset size is chosen by resampling
instead of by a threshold you invent.

Requires the suggested package 'caret'. The RFE function set comes from
`rfe_control$functions` and defaults to
[`caret::rfFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html), which
fits random forests, so the suggested package 'randomForest' must also
be installed unless you supply a different set (for example
`rfe_control = list(method = "cv", number = 5, functions = caret::lmFuncs)`);
it is also needed for the default final model (`model_method = "rf"`)
when `return_final_model = TRUE`. Both are checked before any fitting
starts. Parallel execution additionally requires 'foreach' and
'doParallel'; classification metrics use
[`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html),
which needs 'e1071' (also checked before any fitting starts).

Everything that could leak is fitted on the training rows: the one-hot
encoder, the factor levels the test columns are aligned to, the
elimination itself, and the final model. `details$test_metrics` is
therefore a genuine held-out estimate. It applies
[`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html)
to the predictions of the fitted `rfe` object (caret's own refit on all
the training rows, restricted to the optimal subset) on test rows that
took no part in choosing either the features or the subset size.

The metrics are caret's: for regression `RMSE`, `Rsquared` and `MAE`,
where `Rsquared` is the *squared Pearson correlation* between
predictions and observations, not `1 - SSE/SST`. It is never negative
and ignores bias and scale errors, so it is not comparable with an `R2`
defined as `1 - SSE/SST` (for example
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)'s
`details$metrics$R2`). For classification they are `Accuracy`
(proportion of held-out rows classified correctly) and `Kappa` (Cohen's
kappa, accuracy corrected for chance agreement).

Two other summaries on the result are *not* held-out estimates, by
construction. `details$resampling_results` summarizes resampling
performed inside the training rows across candidate subset sizes, and,
when `return_final_model = TRUE`, `model$results` reports
`train_control` resampling inside those same training rows on features
that were already selected. Only `details$test_metrics` is computed on
data the search never saw.

Predictor names that are not syntactic R names (for example `"a b"`) are
replaced by their
[`make.names()`](https://rdrr.io/r/base/make.names.html) versions inside
the `rfe` object and the final model, because caret's function sets
cannot handle them; `selected`, `scores`, `details$variable_importance`
and `details$final_model_variables` report the original names.

Infinite values (`Inf`/`-Inf`) anywhere in `data` are an error.
Predictors that are constant on the training rows (a single value, or a
factor with a single observed level) are removed before RFE with a
warning; they carry no information, and a random forest fitted on a
single constant predictor never returns. Factor predictors keep only the
levels observed in the training rows; a held-out value with a level the
training rows never saw is treated as missing, with a warning.

Missing values in the *training* predictors are rejected with an error;
impute or drop incomplete rows before calling. NAs in the held-out rows
are not imputed: depending on the function set,
[`predict()`](https://rdrr.io/r/stats/predict.html) either stops (random
forests) or returns `NA` for those rows, which
[`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html)
then leaves out of `details$test_metrics`; a warning reports how many
rows were left out. With `handle_categorical = TRUE` the encoded test
rows are row-count checked against their input, so an encoding that
quietly loses rows becomes an error rather than a silently misaligned
metric.

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
