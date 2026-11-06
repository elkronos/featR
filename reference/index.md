# Package index

## Working with results

Every selection function except
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) and
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md)
returns an `fs_result`. These functions print it, summarize it, and
extract the selected names.

- [`selected()`](https://elkronos.github.io/featR/reference/selected.md)
  : Extract the selected features from a featR result
- [`print(`*`<fs_result>`*`)`](https://elkronos.github.io/featR/reference/print.fs_result.md)
  : Print a featR result
- [`summary(`*`<fs_result>`*`)`](https://elkronos.github.io/featR/reference/summary.fs_result.md)
  : Summarize a featR result

## Filters

Score each feature without fitting a predictive model. They are fast and
work on wide data, but they judge one feature at a time, so they miss
features that only help in combination.

- [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
  : Supervised filter that scores each feature against a target
- [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
  : Unsupervised filter that scores each feature without a target
- [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) :
  Chi-squared tests for categorical features
- [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md)
  : Rank features by information gain
- [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
  : Find and prune highly correlated variables

## Regularization and embedded importance

Fit one model and keep the features it uses: nonzero coefficients for
the lasso and elastic net, variable importance for random forests, and
the terms of the final MARS model.

- [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  : Lasso feature selection with cross-validation
- [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
  : Elastic-net feature selection with tuned alpha and lambda
- [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
  : Random forest importance with a held-out evaluation
- [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) :
  MARS (earth) feature selection

## Wrappers

Refit a model on many candidate subsets and keep the best one. They are
the slowest family, and their choice is tied to the model they refit.

- [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
  : Recursive feature elimination with held-out evaluation
- [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) :
  Train and evaluate an SVM, with optional SVM-RFE feature selection
- [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
  : All-relevant feature selection with Boruta
- [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md)
  : Stepwise linear-regression feature selection by AIC
- [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
  : Bayesian predictor-subset selection with brms and LOO

## Dimensionality reduction

Replace the original columns with new components instead of keeping a
subset. These return a list of their own rather than an `fs_result`.

- [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) :
  Principal component analysis with tidy results and optional plotting
- [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) :
  Singular value decomposition with optional scaling and truncation

## Package

- [`featR`](https://elkronos.github.io/featR/reference/featR-package.md)
  [`featR-package`](https://elkronos.github.io/featR/reference/featR-package.md)
  : featR: A Unified Toolkit for Feature Selection
