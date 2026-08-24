# featR 0.1.0

First release. Unifies the `feature_selection` script collection into a package
with 16 exported functions sharing one calling convention and one return type.

## Calling convention

All selection functions take the form:

```r
fs_<method>(data, target, ..., seed = NULL, verbose = FALSE, n_cores = 1L)
```

`data` is always first and `target` (a single column-name string) always second.
This replaces the previous mix of `response_col`, `target_var`, `target_col`,
`responseName`, `response_var`, `dependent_var`, and `x`/`y` argument pairs.
Other renames: `p` -> `train_ratio`, `predictor_cols` -> `predictors`,
`out` -> `output`, `log_progress`/`show_progress` -> `verbose`,
`cores`/`split_ratio`/`temp_multisession` -> `n_cores`.

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
  (`rule = "1se"`, default) instead of the raw elpd maximum.
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

## Package conventions

* Modeling engines are Suggests; each function checks for what it needs.
* Functions never seed the RNG unless `seed` is supplied, and restore the
  previous RNG state afterwards.
* Execution is sequential by default; worker counts are opt-in and capped.
* No `library()`, `install.packages()`, `.GlobalEnv` writes, or log files in
  package code.
