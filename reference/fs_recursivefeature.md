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
  holding the target and the candidate predictors (every other column).
  It is converted to a plain data.frame on entry.

- target:

  Single string naming the target column of `data`; a column index is
  not accepted. A factor, character, or logical target means
  classification (unused factor levels are dropped), anything else
  regression. The target may not contain missing values.

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
  `list(method = "cv", number = 5)`. `functions` selects the caret RFE
  function set and defaults to
  [`caret::rfFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html). An
  `allowParallel` entry is dropped with a warning (use the `parallel`
  argument instead), while a `verbose` entry, if present, overrides the
  `verbose` argument for
  [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html). With
  `method = "repeatedcv"` and no `repeats`, caret's default of 1 is used
  and a warning says so.

- train_control:

  List of arguments for
  [`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html),
  used only when `return_final_model = TRUE`. Must contain `method`, and
  also `number` unless `method = "none"`. Default
  `list(method = "cv", number = 5)`.

- model_method:

  Single string; the caret model key used for the final model, for
  example `"rf"` or `"lm"`. Used only when `return_final_model = TRUE`.
  Default `"rf"`.

- handle_categorical:

  Logical; one-hot encode predictors with full-rank dummies (fitted on
  the training rows, applied to the test rows). Default `FALSE`.

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
  `parallel = TRUE` the seed is also used to set reproducible
  L'Ecuyer-CMRG streams on the workers, so two seeded parallel runs
  agree with each other; because the workers draw from their own
  streams, a seeded parallel run need not equal a seeded sequential one.

- verbose:

  Logical; print progress messages and let
  [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html) report its
  own progress, unless `rfe_control$verbose` overrides the latter.
  Default `FALSE`.

- parallel:

  Logical. If `TRUE`, registers a two-worker PSOCK cluster (capped at
  the detected core count) for the duration of the call, then stops it
  and restores whichever foreach backend was registered before the call;
  requires the suggested packages 'foreach' and 'doParallel'. Default
  `FALSE`.

## Value

An object of class `fs_result` with:

- selected:

  Character vector of the variables RFE kept (`optVariables` at the
  optimal subset size).

- scores:

  Named numeric vector of resample-averaged importance from
  [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html) on the
  `rfe` object: its "Overall" column when caret supplies one, otherwise
  the row means of whatever numeric columns it did supply. `NULL` when
  there is nothing usable.

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
  data.frame), `preprocessor` (the `dummyVars` encoder, or `NULL` when
  `handle_categorical = FALSE`), `train_index` and `test_index` (row
  indices into `data` for the two partitions), `final_model_variables`
  (predictors the final model actually used after NZV/linear-combination
  filtering, or `NULL` when no final model was requested) and
  `n_features` (candidate predictors offered to RFE, counted after
  one-hot encoding when `handle_categorical = TRUE`).

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
which needs 'e1071'.

Everything that could leak is fitted on the training rows: the one-hot
encoder, the factor levels the test columns are aligned to, the
elimination itself, and the final model. `details$test_metrics` is
therefore a genuine held-out estimate. It applies
[`caret::postResample()`](https://rdrr.io/pkg/caret/man/postResample.html)
to the predictions of the fitted `rfe` object (caret's own refit on all
the training rows, restricted to the optimal subset) on test rows that
took no part in choosing either the features or the subset size.

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
