# featR 0.1.0

First release. Unifies the `feature_selection` script collection into a package
with 16 exported `fs_*()` functions (plus the `selected()` accessor) sharing one
calling convention and one return type.

## Calling convention

All selection functions take the form:

```r
fs_<method>(data, target, <method options>, ...)
```

`data` is always first and `target` (a single column-name string) always second,
for the functions that have an outcome. Housekeeping arguments come last:
every function takes `verbose`, functions that use randomness take
`seed = NULL`, and functions that can run in parallel take `n_cores` and/or
`parallel`.
This replaces the previous mix of `response_col`, `target_var`, `target_col`,
`responseName`, `response_var`, `dependent_var`, and `x`/`y` argument pairs.
Other renames: `p` -> `train_ratio`, `predictor_cols` -> `predictors`,
`out` -> `output`, `log_progress`/`show_progress`/`doTrace` -> `verbose`,
`cores`/`temp_multisession` -> `n_cores`, and `control$split_ratio` ->
`control$train_ratio` in `fs_randomforest()`. `fs_randomforest()` also moves
`seed` and `n_cores` out of its `control` list into real arguments.
Removed arguments that never worked or had no effect: `early_stop_threshold`
(`fs_bayes()`), `early_stop` and `feature_funcs` (`fs_recursivefeature()`,
the latter now passed as `rfe_control$functions`), `seed` and `return_models`
(`fs_stepwise()`), `memoise_result` (`fs_svd()`), `auto_install`
(`fs_randomforest()`), and `method` (`fs_mars()`, which is earth-only).

## Return value

The 14 selection functions return an `fs_result` object with `selected`,
`scores`, `method`, `task`, `model`, `details`, and `call`, plus `print()`,
`summary()`, and `selected()` methods. Method-specific output moved into
`details`. `fs_pca()` and `fs_svd()` are dimensionality reduction and keep
their own decomposition structure.

## Statistical corrections

* `fs_svm()` gained a real SVM-RFE implementation (linear-kernel weight-vector
  ranking with a refit at each elimination step, plus cross-validated size
  selection). The previous behavior — random-forest RFE — is still available
  via `select_method = "rf_rfe"`.
* `fs_correlation()` gained `prune` (default `TRUE`): `selected` is now the
  reduced non-redundant set rather than both members of every correlated pair.
* `fs_boruta()` prunes correlated features by Boruta importance, keeping the
  stronger member of a correlated group instead of deferring to a blind
  correlation heuristic.
* `fs_bayes()` selects with `loo::loo_compare()` and a 1-SE parsimony rule
  (`rule = "1se"`, default) instead of the raw elpd maximum. The rule accepts
  either container `loo_compare()` may return (matrix or data.frame); an
  earlier `is.matrix()` guard would have silently disabled it under any loo
  release that returns a data.frame.
* `fs_infogain()` gained `normalize = "gain_ratio"` to correct information
  gain's bias toward high-cardinality predictors, and now discretizes the
  target once so scores are comparable across features.
* `fs_lasso()` no longer mean-imputes by default (`impute = "none"`); full-data
  imputation leaked across cross-validation folds. Scores are now standardized
  coefficients.
* `fs_elastic()` fits PCA inside each resample via caret's `preProcess`
  instead of once on the full data.
* `fs_randomforest()` runs the `control$feature_select` hook on the training
  split only.
* Class upsampling in `fs_mars()` and `fs_svm()` happens within resampling
  folds rather than before cross-validation.
* `fs_recursivefeature()` evaluates the selected subset on a held-out test
  split and trains its final model on training rows only.

## Bug fixes

* `fs_bayes()` reads the model labels `loo::loo_compare()` produces from its
  `model` column when present, falling back to row names. loo 2.10.0 moved
  those labels out of the row names, which would otherwise have made the
  selection rule map comparison rows to the wrong candidate models.
* `fs_recursivefeature()` coerces character and logical predictors to factors
  *before* fitting the one-hot encoder, so the encoder is not silently refitted
  on the test rows (`caret::dummyVars()` records levels only for columns that
  are already factors).
* `fs_supervised()` honors `na_rm = FALSE` on the ANOVA path, where
  `stats::lm()`'s own NA handling previously made it behave like
  `na_rm = TRUE`.
* `fs_unsupervised()` returns an undefined score rather than an error for
  `method = "iqr"` with `na_rm = FALSE`.
* `fs_lasso()` computes standardized scores by position rather than by column
  name, so a design matrix with duplicated names cannot pair a coefficient with
  the wrong standard deviation. Its `nfolds` minimum is now 3, matching glmnet.
* `fs_mars()` checks for MLmetrics before selecting caret's multi-class
  summary, instead of a test that could never fail; multi-class targets no
  longer fail after the resamples have been computed.
* `fs_correlation()` rejects duplicated column names instead of silently
  resolving every lookup to the first match, which could replace one column's
  correlations with another's and drop both members of the pair.
* `fs_boruta()` validates `maxRuns` against Boruta's own minimum of 11, so the
  error names the featR argument rather than surfacing from the dependency.
* `fs_lasso()` reports non-finite predictor values as a user-input problem,
  naming the offending columns, rather than as an internal error.
* `fs_elastic()` no longer errors with "missing value where TRUE/FALSE needed"
  when a tuning result column contains `NA`.
* `summary()` on an `fs_result` ranks p-value scores ascending, so `fs_chi()`
  lists its most significant features first instead of last.
* `print()` on an `fs_result` reports the recorded candidate count in
  preference to the number of scored features, so methods that score only a
  subset (such as `fs_recursivefeature()`) no longer report "Selected 2 of 2".
* The `fs_result` constructor rejects an unnamed numeric `scores` vector and a
  zero-length `task`, both of which `print()` and `summary()` could not
  display.
* `fs_bayes()` samples predictor combinations without enumerating them, so
  `sample_combinations` now works at the scale it exists for: previously every
  subset was materialized first, which exhausted memory past roughly 25
  predictors. An unbounded search over a very large subset space now errors
  with instructions instead of dying on allocation.
* `fs_bayes()` validates `brm_family`, so passing a family generator without
  parentheses reports the mistake instead of "object of type 'closure' is not
  subsettable".
* `fs_bayes()` supports families whose `fitted()` is a 3-D array (categorical,
  multinomial, multivariate). Selection proceeds; the in-sample MAE and RMSE,
  which are undefined for those responses, are reported as `NA` rather than
  causing an error that was previously mislabelled as a sampling failure.
* `fs_svm()` errors rather than ranking features from a partially recovered
  weight vector, and reports a degenerate elimination-step fit as a featR
  error naming the likely cause instead of surfacing kernlab's.
* `fs_recursivefeature()` sets reproducible RNG streams on its parallel
  workers, so two seeded parallel runs agree, and stops its cluster if
  backend registration fails.
* `fs_pca()` rejects `scale_data = TRUE` with `center_data = FALSE`, which the
  two engines handled differently, and validates `num_pc` against the
  large-data engine's own limit.
* `fs_randomforest()` replaces only the `NA` entries of a per-class
  `control$sampsize`, instead of discarding the sizes that were supplied.
* `fs_infogain()` refuses to expand a date column when the resulting
  `<col>_year`/`_month`/`_day` name already exists. Previously that column was
  overwritten in place, silently, including when it was the target.
* `fs_infogain()` caps the automatic bin count at the number of observations.
  A near-constant column beside one extreme outlier drove the
  Freedman-Diaconis count past the integer range, which became `NA` and made
  `cut()` fail with "invalid number of intervals".
* `fs_chi()` reports `correction_applied` from what `stats::chisq.test()`
  actually did. A 2x2 table sitting exactly at expectation receives no Yates
  correction even when one is requested, and the row previously claimed
  otherwise.
* `fs_correlation()` registers its cluster teardown before registering the
  parallel backend, so a failure there cannot leak the cluster, and restores
  the caller's own foreach backend instead of forcing sequential execution.
* `fs_elastic()` sets `allowParallel` from whether featR created a cluster.
  caret's default of `TRUE` meant a single-worker call would dispatch
  resamples to whatever backend the caller had registered elsewhere.
* `fs_svm()`'s `min_keep` is a floor rather than a quota: when random-forest
  RFE fails, the fallback keeps every predictor with positive impurity
  importance and only drops to `min_keep` top-ranked predictors if fewer
  qualify. It previously returned exactly one predictor.

* `fs_mars()` no longer fails on every call (a data.table was indexed with the
  matrix returned by `caret::createDataPartition()`).
* `fs_mars()` sanitizes factor levels with `make.names(unique = TRUE)`, so
  distinct classes such as `"class 1"` and `"class.1"` can no longer merge.
* `fs_lasso()` accepts data frames containing `NA` (the previous
  `model.matrix()` call silently dropped those rows and then aborted).
* `fs_pca()` computes variance explained against total variance in the
  large-data path, labels its loadings, and no longer overflows on very large
  inputs.
* `fs_correlation()` reports point-biserial correlations with the correct
  sign.
* `fs_randomforest()` stratifies on the user's target rather than any column
  literally named `"target"`, imputes test-only missing values, and accepts
  character predictors.
* `fs_svd()` errors on invalid arguments instead of silently repairing them.

## Pre-release adversarial review

A full statistical, methodological, and programmatic review, with every
finding reproduced in R before it was fixed and a regression test added for
each.

### Behavior changes

* `fs_elastic()`'s default `alpha_seq` is now `seq(0.1, 1, by = 0.1)`. The old
  default included `alpha = 0`, pure ridge. Ridge never zeroes a coefficient,
  so whenever it won the tuning (it won on every seed tried with correlated
  predictors) every predictor was reported as selected. A user-supplied
  `alpha = 0` that wins now warns.
* `fs_lasso()` gains `lambda = c("min", "1se")`. The default is unchanged;
  `"1se"` selects at `lambda.1se` for a sparser, more stable set, and
  `details$lambda_used` records which was used.
* `summary()` ranks scores by value rather than by absolute value, except for
  `fs_lasso()`'s signed coefficients. Negative permutation or Boruta
  importance (worse than noise) previously ranked above useful features. It
  also lists the kept side first when a threshold filter kept the low scores.
* `fs_boruta()` passes `num.threads = 1` to Boruta's importance engine, which
  otherwise used every core, contrary to featR's sequential default.
* `seed` must be a whole number within the integer range. Fractional seeds
  were silently truncated (`seed = 1.9` gave the same stream as `seed = 1`).
* Parallel paths restore the caller's own foreach backend on exit, through
  one shared helper. Previously they forced the session back to sequential,
  or left a user-registered backend pointing at featR's stopped cluster.

### Statistical corrections

* `fs_svm()`'s SVM-RFE subset-size search was optimistically biased. It
  scored the top of a ranking built on all training rows, including each
  fold's held-out rows, and scaled once on all rows. On pure noise it reported
  about 93% cross-validated accuracy where the truth is 50%. The elimination
  and the scaling are now re-run inside every fold.
* `fs_bayes()` no longer discards sampler and loo warnings. Convergence
  problems and high Pareto *k* values are collected and summarized after the
  search, saying whether the selected model is affected.

* `fs_randomforest()` pooled per-forest importance standard errors with
  `randomForest::combine()` when run in parallel, which inflates them by about
  `sqrt(n_cores)` and shrank scaled importance by the same factor. They are
  now pooled exactly across trees.
* `fs_recursivefeature()` over-reported held-out accuracy for character and
  logical targets. The test factor was rebuilt from the test rows alone, and
  `caret::postResample()` silently dropped predictions of classes absent from
  them.
* `fs_supervised()` computes the ANOVA *F* in closed form. Perfect separation
  scores `Inf` instead of a rounding-noise value near 1e31, only observed
  groups count towards the degrees of freedom, and the "essentially perfect
  fit" warning is gone. The constant-column check for correlation scores is
  relative to the data's scale, so features measured in very small units are
  no longer scored `NA`.
* `fs_infogain()` clamps floating-point residue to zero, so exactly
  independent predictors score 0 and are not selected, and bins columns
  containing infinite values instead of failing.
* `fs_pca()` and `fs_svd()` use a relative zero-variance tolerance. Columns
  that are constant up to round-off are dropped rather than scaled up to take
  a full share of a component, and columns in very small units are kept.
  `fs_svd()` falls back to the exact solver when RSpectra fails to converge.
* `fs_mars()` returns regression predictions and `R2` as plain numbers rather
  than 1 x 1 matrices.

### Bug fixes

* Non-syntactic column names: `fs_mars()` never selected such predictors, and
  `fs_lasso()`, `fs_elastic()`, and `fs_stepwise()` returned them backticked,
  so `data[selected(res)]` failed. `fs_svm()` carried the same backticks into
  its encoded feature names. `fs_recursivefeature()` crashed on them
  with `caret::lmFuncs`. Formula construction now escapes backslashes and
  backticks inside names.
* `fs_chi()` and `fs_infogain()` reject duplicated column names. The second
  copy was previously never tested.
* `fs_lasso()` and `fs_elastic()` give a clear error for a single
  design-matrix column instead of glmnet's.
* `fs_stepwise()` accepts a user-supplied `scope` instead of erroring.
* `fs_mars(search = "random")` with a factor outcome no longer fails on the
  first call in a fresh session.
* `fs_randomforest()` rejects invalid `sampsize` entries instead of silently
  growing every tree on one row, imputes logical predictors, and warns when a
  numeric target is modeled with the default `task = "classification"`.
* `fs_recursivefeature()` handles unused factor levels in the target, rejects
  fractional `sizes` and `NA` targets, reports `NA` test predictions, and
  requires randomForest when its defaults need it.
* `fs_correlation()` validates the type of `diag_value`. Its documentation
  now describes the pruning rule accurately: strongest pair first, not
  `caret::findCorrelation()`'s scan order.
* `fs_pca()` keeps logical and date columns as labels instead of dropping them
  silently.

## Package conventions

* Modeling engines are Suggests; each function checks for what it needs.
* Functions never seed the RNG unless `seed` is supplied, and restore the
  previous RNG state afterwards.
* Execution is sequential by default; worker counts are opt-in and capped.
* No `library()`, `install.packages()`, `.GlobalEnv` writes, or log files in
  package code.
