# featR: A Unified Toolkit for Feature Selection

featR collects filter, regularization, wrapper, and
dimensionality-reduction methods for choosing predictors. Every
selection method is one function call on a data frame and returns the
same kind of object, so switching from
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
to
[`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
changes the function name and its method-specific options and leaves the
surrounding code alone.

The package website has longer guides. Start with [Getting
started](https://elkronos.github.io/featR/articles/featR.html),
[Choosing a
method](https://elkronos.github.io/featR/articles/choosing-a-method.html),
and [Validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.html),
which explains which reported numbers are held-out and which are
optimistic.

## Calling convention

Selection functions are called as `fs_<method>(data, target, ...)`.
`data` holds the observations (a data frame, or a data.table or matrix
where the method accepts one), and `target` is the name of the outcome
column inside `data`, never a separate vector. Method-specific options
follow, and housekeeping arguments (`seed`, `verbose`, `n_cores`) come
last wherever a method has them.

Four functions take no `target`.
[`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
and
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
score features without an outcome, and
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) and
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md)
decompose a numeric table.

## The result object

Every selection function returns an `fs_result`, a list of seven
elements:

- `selected`:

  Character vector of the chosen feature names, possibly empty.

- `scores`:

  Per-feature scores: usually a named numeric vector covering every
  candidate feature, a data frame for a few methods, or `NULL` when the
  method has no comparable per-feature score. What a score means depends
  on the method; see each function's Value section.

- `method`:

  A single string naming what ran, for example
  `"supervised_correlation"`, `"lasso"`, or `"svm_linear"`.

- `task`:

  `"classification"`, `"regression"`, or `NA` when the method has no
  task.

- `model`:

  The fitted model when the method fits one, otherwise `NULL`.

- `details`:

  A named list of method-specific extras, documented on each function's
  help page.

- `call`:

  The matched call.

Results have
[print()](https://elkronos.github.io/featR/reference/print.fs_result.md)
and
[summary()](https://elkronos.github.io/featR/reference/summary.fs_result.md)
methods, and
[`selected()`](https://elkronos.github.io/featR/reference/selected.md)
extracts the chosen names.
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) and
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) are
the exceptions: they build new components rather than choosing columns,
and each returns its own plain list. The [fs_result
guide](https://elkronos.github.io/featR/articles/fs-result.html)
tabulates what `scores` holds for each function.

## Methods

- Filters:

  No model is fitted, so they are cheap and scale to wide data.
  [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
  scores each feature by absolute Pearson correlation or one-way ANOVA F
  against the target;
  [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
  by variance, MAD, IQR, range, missingness, or number of distinct
  values;
  [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) by
  a chi-squared test for categorical features; and
  [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md)
  by information gain or gain ratio.
  [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
  drops redundant members of highly correlated groups.

- Regularization and embedded importance:

  One model fit whose own structure picks the survivors.
  [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  uses an L1 penalty,
  [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
  an L1/L2 mixture,
  [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
  permutation importance with held-out evaluation, and
  [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
  keeps the predictors an earth model retains.

- Wrappers:

  A model is refitted over candidate subsets. These are the most
  expensive methods and the most closely tied to the model you intend to
  use.
  [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
  runs caret recursive feature elimination;
  [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
  tunes an SVM, optionally after SVM-RFE or random-forest screening;
  [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
  tests each feature against shuffled shadow copies;
  [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md)
  runs AIC stepwise regression; and
  [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
  compares brms models with loo.

- Dimensionality reduction:

  New components replace the original columns.
  [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md)
  returns principal components with tidy output and an optional plot,
  and [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md)
  a truncated singular value decomposition with an optional approximate
  solver.

## Dependencies

The hard dependencies are data.table, methods, parallel, stats, utils,
and withr. Every modeling engine (caret, glmnet, kernlab, randomForest,
Boruta, earth, MASS, brms, RSpectra, and the rest) is a suggested
package, loaded only on the code path that needs it. If one is missing,
the call stops with an error that names the missing packages and gives
the
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
call to run.

## Reproducibility and parallelism

featR never seeds the random number generator on its own. Functions that
use randomness take `seed = NULL`; a supplied seed applies to that call
only, and the caller's RNG state is restored on exit. Every function
runs sequentially by default. Parallelism is opt-in through `n_cores`
(or `parallel = TRUE`), worker requests are capped at the detected core
count, and any cluster featR starts is stopped when the call returns.
See [Reproducibility and
parallelism](https://elkronos.github.io/featR/articles/reproducibility.html).

## See also

Useful links:

- <https://github.com/elkronos/featR>

- <https://elkronos.github.io/featR/>

- Report bugs at <https://github.com/elkronos/featR/issues>

## Author

**Maintainer**: Justin Chase <jchase.msu@gmail.com> \[copyright holder\]
