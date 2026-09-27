# Package index

## Working with results

Every selection function returns an `fs_result`. These are its accessor
and display methods.

- [`selected()`](https://elkronos.github.io/featR/reference/selected.md)
  : Extract the selected features from a featR result
- [`print(`*`<fs_result>`*`)`](https://elkronos.github.io/featR/reference/print.fs_result.md)
  : Print a featR result
- [`summary(`*`<fs_result>`*`)`](https://elkronos.github.io/featR/reference/summary.fs_result.md)
  : Summarize a featR result

## Filters

Score each feature without fitting a predictive model. Cheap, and
suitable for a first cut on wide data, but they judge features one at a
time.

- [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
  : Supervised Filter-Based Feature Selection
- [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
  : Unsupervised Filter-Based Feature Selection
- [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) :
  Chi-square feature selection for categorical features
- [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md)
  : Feature Selection via Information Gain
- [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
  : Correlation-based feature selection

## Regularization and embedded importance

One model fit whose own structure names the survivors.

- [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  : Lasso Feature Selection with Cross-Validation
- [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
  : Elastic Net Feature Selection and Model Training
- [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
  : Random forest importance and held-out evaluation
- [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) :
  MARS (earth) feature selection

## Wrappers

Refit a model over candidate subsets. The most expensive family, and the
one most closely tied to the model you intend to use.

- [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
  : Recursive feature elimination with held-out evaluation
- [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) :
  Train and evaluate an SVM, with optional SVM-RFE feature selection
- [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
  : Feature selection using Boruta
- [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md)
  : Stepwise linear-regression feature selection via AIC
- [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
  : Bayesian feature selection for model optimization

## Dimensionality reduction

Replace the original columns with new components instead of keeping a
subset. These return their own list rather than an `fs_result`.

- [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) :
  Principal component analysis with tidy results and optional plotting
- [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) :
  Singular Value Decomposition with Optional Scaling and Truncation

## Package

- [`featR`](https://elkronos.github.io/featR/reference/featR-package.md)
  [`featR-package`](https://elkronos.github.io/featR/reference/featR-package.md)
  : featR: A Unified Toolkit for Feature Selection
