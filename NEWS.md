# featR 0.1.0

Initial release. Unifies the feature_selection script collection into a single
package with 16 exported functions:

* Filters: `fs_chi()`, `fs_correlation()`, `fs_infogain()`, `fs_supervised()`,
  `fs_unsupervised()`.
* Regularization: `fs_elastic()`, `fs_lasso()`.
* Model-based / wrappers: `fs_bayes()`, `fs_boruta()`, `fs_mars()`,
  `fs_randomforest()`, `fs_recursivefeature()`, `fs_stepwise()`, `fs_svm()`.
* Dimensionality reduction: `fs_pca()`, `fs_svd()`.

Notable changes relative to the pre-package scripts:

* Heavy modeling engines are optional (Suggests); each function checks its own
  requirements and reports what to install.
* Functions never seed the RNG unless a `seed` argument is supplied, and then
  restore the previous RNG state.
* Sequential execution by default; parallelism is opt-in and capped.
* Numerous bug fixes, including: train/test splitting for data.table inputs
  (`fs_mars()`), NA handling for data.frame input (`fs_lasso()`),
  variance-explained computation for large data (`fs_pca()`), held-out test
  evaluation (`fs_recursivefeature()`), point-biserial correlation signs
  (`fs_correlation()`), and per-fold class upsampling (`fs_mars()`, `fs_svm()`).
