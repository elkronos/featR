# featR: A Unified Toolkit for Feature Selection

featR gathers filter, regularization, wrapper, and
dimensionality-reduction methods for choosing predictors and puts them
behind one calling convention and one return type, so that trying a
different method is a one-word change rather than a rewrite. Each method
is a single function call on a data frame; there is no pipeline object
to build first.

## Details

**Calling convention.** Selection functions are called as
`fs_<method>(data, target, ...)`. `data` holds the observations (a
data.frame, or a data.table or matrix where the method accepts one) and
`target` is the *name* of the outcome column inside `data`, never a
separate vector. Method-specific options follow, and housekeeping
arguments (`seed`, `verbose`, `n_cores`) come last wherever a method
supports them. The functions with no outcome to name take no `target`:
[`fs_unsupervised`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
and
[`fs_correlation`](https://elkronos.github.io/featR/reference/fs_correlation.md)
score features without one, and
[`fs_pca`](https://elkronos.github.io/featR/reference/fs_pca.md) and
[`fs_svd`](https://elkronos.github.io/featR/reference/fs_svd.md)
decompose a numeric table.

**Return value.** Every selection function returns an object of class
`fs_result`, a list with the elements:

- `selected`: character vector of the chosen feature names (possibly
  empty).

- `scores`: per-feature scores, usually a named numeric vector covering
  every candidate feature (a data.frame for a few methods), or `NULL`
  when the method produces no comparable per-feature score.

- `method`: single string naming what actually ran, for example
  `"supervised_correlation"`, `"lasso"`, or `"svm_linear"`.

- `task`: `"classification"`, `"regression"`, or `NA` when the method
  has no task.

- `model`: the fitted model when the method fits one, otherwise `NULL`.

- `details`: named list of method-specific extras, documented per
  function.

- `call`: the matched call.

Results have [`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods, and
[`selected`](https://elkronos.github.io/featR/reference/selected.md)
extracts the chosen names.
[`fs_pca`](https://elkronos.github.io/featR/reference/fs_pca.md) and
[`fs_svd`](https://elkronos.github.io/featR/reference/fs_svd.md) are
dimensionality reduction rather than selection, so they are the two
exceptions: each returns its own plain list, documented on its own help
page.

**Dependencies.** Only data.table, parallel, stats, utils, and withr are
hard dependencies. Every modeling engine (caret, glmnet, kernlab,
randomForest, Boruta, earth, MASS, brms, RSpectra, and the rest) is a
suggested package, loaded only on the code path that needs it and
checked at the point of use: if one is missing you get an error naming
the packages and the exact
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
call to run, rather than a cryptic failure deep in a model fit.

**Reproducibility and resources.** featR never seeds the RNG on its own.
Functions that use randomness take `seed = NULL`; supplying a seed sets
it for that call only and restores the caller's RNG state on exit. Every
function is sequential by default, parallelism is opt-in through
`n_cores` (or `parallel = TRUE`), worker requests are capped at the
detected core count, and any cluster featR starts is stopped when the
call returns.

**Available methods.**

- Filters:

  No model is fitted, so they are cheap and scale to wide data.
  [`fs_supervised`](https://elkronos.github.io/featR/reference/fs_supervised.md)
  (absolute Pearson correlation or one-way ANOVA F against the target),
  [`fs_unsupervised`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
  (variance, MAD, IQR, range, missingness, distinct values),
  [`fs_chi`](https://elkronos.github.io/featR/reference/fs_chi.md)
  (chi-squared tests for categorical features),
  [`fs_infogain`](https://elkronos.github.io/featR/reference/fs_infogain.md)
  (information gain and gain ratio), and
  [`fs_correlation`](https://elkronos.github.io/featR/reference/fs_correlation.md)
  (drops redundant members of highly correlated groups).

- Regularization and embedded importance:

  One model fit whose own structure names the survivors.
  [`fs_lasso`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  (L1 penalty),
  [`fs_elastic`](https://elkronos.github.io/featR/reference/fs_elastic.md)
  (L1/L2 mixture),
  [`fs_randomforest`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
  (permutation importance with held-out evaluation), and
  [`fs_mars`](https://elkronos.github.io/featR/reference/fs_mars.md)
  (the predictors an earth model retains).

- Wrappers:

  A model is refitted over candidate subsets: the most expensive family,
  and the most closely tailored to the model you intend to use.
  [`fs_recursivefeature`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
  (caret recursive feature elimination),
  [`fs_svm`](https://elkronos.github.io/featR/reference/fs_svm.md)
  (SVM-RFE, or random-forest screening, plus a tuned SVM),
  [`fs_boruta`](https://elkronos.github.io/featR/reference/fs_boruta.md)
  (all-relevant selection against shadow features),
  [`fs_stepwise`](https://elkronos.github.io/featR/reference/fs_stepwise.md)
  (AIC stepwise regression), and
  [`fs_bayes`](https://elkronos.github.io/featR/reference/fs_bayes.md)
  (brms models compared with loo).

- Dimensionality reduction:

  New components replace the original columns instead of a subset being
  kept. [`fs_pca`](https://elkronos.github.io/featR/reference/fs_pca.md)
  (principal components with tidy output and an optional plot) and
  [`fs_svd`](https://elkronos.github.io/featR/reference/fs_svd.md)
  (truncated singular value decomposition with an optional approximate
  solver).

## See also

Useful links:

- <https://github.com/elkronos/featR>

- <https://elkronos.github.io/featR/>

- Report bugs at <https://github.com/elkronos/featR/issues>

## Author

**Maintainer**: Justin Chase <jchase.msu@gmail.com> \[copyright holder\]
