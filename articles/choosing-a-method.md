# Choosing a method

featR has fourteen selection functions and two dimensionality-reduction
functions. Pick one by the question you are asking, then check the
reference tables at the end for what it accepts, how it treats missing
values, and which optional packages it needs.

## Start with the question

| Question | Function |
|----|----|
| Which columns are constant, near-constant, or mostly missing? | [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md) |
| Which columns duplicate each other? | [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) |
| Which numeric columns are individually associated with the outcome? | [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md) |
| Which categorical columns are associated with a categorical outcome? | [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md), [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) |
| How much does each column reduce uncertainty about the outcome, whatever its type? | [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) |
| Which predictors survive in a sparse linear model? | [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md), [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md) |
| Which predictors carry *any* information about the outcome? | [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md); set `cutoff_cor = NULL` to keep redundant ones too |
| How does each predictor rank in a random forest, and how good is that forest on held-out rows? | [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md) |
| Which predictors does a model with automatic non-linearities and interactions use? | [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) |
| What is the smallest subset a particular model needs? | [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md), [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) |
| Which terms does AIC keep in a linear regression? | [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md) |
| Which predictor subset has the best out-of-sample predictive fit under a Bayesian model? | [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) |
| Can I summarize these columns in a few directions? | [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md), [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) |

The method guides cover each function in depth: [Filter
methods](https://elkronos.github.io/featR/articles/filters.md),
[Regularization and embedded
importance](https://elkronos.github.io/featR/articles/regularization.md),
[Wrapper
methods](https://elkronos.github.io/featR/articles/wrappers.md), and
[Dimensionality
reduction](https://elkronos.github.io/featR/articles/dimensionality-reduction.md).

## Filters, embedded methods, and wrappers

A **filter** scores each feature without fitting a predictive model. The
filters are
[`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md),
[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md),
[`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md),
[`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md),
and
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md).
They are fast and scale to thousands of columns. The cost is that they
look at one feature at a time (or, for
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md),
one pair at a time), so two copies of the same signal both score highly,
and a feature that matters only in combination with another scores low.
Use filters for a first cut, then refine with something else.

An **embedded method** fits one model and reads the selection off its
structure: non-zero coefficients, importance, or retained terms. The
embedded methods are
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md),
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md),
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md),
and
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md).
They account for the other predictors, but the answer belongs to that
model family.
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
ranks rather than selects: unless you pass a selection function in
`control$feature_select`, `selected` holds every predictor the forest
used, in importance order, and the cutoff is yours.

A **wrapper** refits a model over many candidate subsets. The wrappers
are
[`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md),
[`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md),
[`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md),
[`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md),
and
[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md).
They are the most expensive and the most tailored to one model. They are
also the most prone to overfitting the selection itself, so validate
their output on held-out data.

## A sensible default pipeline

For a new tabular problem:

1.  Drop constant or mostly missing numeric columns with
    [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md).
    It uses no outcome, so it is safe to run before any split.
2.  Hold out a test set now, before anything looks at the outcome.
3.  Prune near-duplicate numeric columns with
    [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
    on the training rows.
4.  On the training rows, run the selector that matches your final
    model:
    [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
    for a linear model,
    [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
    (random forests by default) for a forest,
    `fs_svm(feature_select = TRUE)` for an SVM. Use
    `fs_boruta(cutoff_cor = NULL)` instead when you want every
    informative feature rather than a compact set.
5.  Evaluate the final model on the test rows.

The [validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.md)
article runs this pipeline end to end and lists which numbers each
function reports on held-out data.

## Reference: what each function accepts

The [calling
convention](https://elkronos.github.io/featR/articles/featR.html#the-calling-convention)
is `data` first, then `target` as a string for the functions that use an
outcome. Four functions break the pattern:

- [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
  requires `task = "classification"` or `"regression"` and will not
  guess it.
- [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
  takes `task` too, defaulting to `"classification"`; it warns when that
  default meets a numeric target.
- [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
  uses only the columns you list in `predictors`, and
  [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md)
  takes a numeric matrix `x` instead of `data`.

### Outcome and predictor types

“Dummy-coded” means factor and character predictors are expanded into
one column per level, so `selected` names those columns (`colorred`)
rather than the original one (`color`). The [fs_result
object](https://elkronos.github.io/featR/articles/fs-result.md) article
lists which functions do this.

| Function | Outcome (`target`) | `task` reported | Predictor columns |
|----|----|----|----|
| [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md) | none | `NA` | numeric only; any other column is an error |
| [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) | none | `NA` | numeric for `"pearson"`, `"spearman"`, `"kendall"`; ordered factors for `"polychoric"`; numeric or two-valued columns for `"pointbiserial"` |
| [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md) | numeric, factor, character, or logical | `"regression"` for numeric, `"classification"` for factor, `NA` for character or logical | numeric only; any other column is an error |
| [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) | anything, coerced to a factor (a numeric outcome becomes one class per distinct value) | `"classification"` | factor and character only; numeric, logical, and date columns are skipped |
| [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) | anything; a numeric outcome is binned | always `"classification"` | any mix: numeric columns are binned, dates split into year, month, and day |
| [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md) | numeric only | `"regression"` | numeric, factor, character, logical; dummy-coded |
| [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md) | numeric, or factor, character, or logical for classification | either | numeric, factor, character, logical; dummy-coded |
| [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md) | set by `task` | either | numeric, factor, character, logical, `Date` |
| [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) | numeric, or factor or character for classification; logical is an error | either | numeric, factor, character, logical; importance is mapped back to the original column |
| [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md) | numeric, or factor, character, or logical for classification | either | numeric, factor, character, logical; dummy-coded only with `handle_categorical = TRUE` |
| [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) | set by `task`; a numeric outcome may be a class label only if it has at most 10 values | either | numeric, factor, character, logical; dummy-coded |
| [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md) | numeric, or factor for classification; character is an error | either | numeric, factor, character, logical, `Date`, `POSIXt` |
| [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md) | numeric only | `"regression"` | numeric, factor, character, logical; `scores` are per term, not per dummy |
| [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) | whatever `brm_family` accepts (numeric for [`gaussian()`](https://rdrr.io/r/stats/family.html), 0/1 or logical for `bernoulli()`) | `"classification"` for binary, categorical, and ordinal families, else `"regression"` | the columns named in `predictors`; `date_col` adds an `iso_week_id` predictor |
| [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) | none | no `fs_result` | numeric columns; other columns are kept as labels |
| [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) | none | no `fs_result` | a numeric matrix, or a data frame whose columns are all numeric |

### Missing values and optional packages

The packages in the last column are in featR’s Suggests, so they are not
installed with featR. Packages listed in parentheses are needed only for
the feature named. When a required package is missing, the function
stops before doing any work and prints the
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
call you need. A few are optional extras with a fallback instead of an
error: without bigstatsr,
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) uses
[`prcomp()`](https://rdrr.io/r/stats/prcomp.html) on large data; without
pROC,
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
reports no AUC and
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
tunes on accuracy instead of ROC and skips the test ROC AUC; without
PRROC,
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
skips the PR AUC; without pbapply,
[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
shows no progress bar. [Optional
dependencies](https://elkronos.github.io/featR/articles/reproducibility.html#optional-dependencies)
has one command that installs every suggested package.

| Function | Missing values | Suggests packages |
|----|----|----|
| [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md) | `na_rm = TRUE` (default) ignores them for `"variance"`, `"mad"`, `"iqr"`, `"range"`; `"missing_prop"` and `"n_unique"` always see them | none |
| [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) | `na.rm = FALSE` (default) leaves those correlations `NA`, and an `NA` pair is never flagged; `na.rm = TRUE` drops them pairwise. `"polychoric"` errors unless `na.rm = TRUE`, then drops incomplete rows | polycor (`"polychoric"`); foreach, doParallel (parallel point-biserial) |
| [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md) | `na_rm = TRUE` (default) drops incomplete rows separately for each feature | none |
| [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) | each feature is tested on its own complete cases | furrr, future (`parallel = TRUE`) |
| [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) | handled per predictor and outcome pair | none |
| [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md) | an error, for the outcome and for predictors; `impute = "mean"` fills predictors with full-data means and warns that this leaks | glmnet; foreach, doParallel (`parallel = TRUE`) |
| [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md) | rows with any missing value are dropped | caret, glmnet, Matrix; e1071 (classification); foreach, doParallel (`n_cores > 1`) |
| [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md) | rows with a missing outcome are dropped with a warning; missing predictors are imputed with training-row medians and modes | randomForest, caret; pROC (binary AUC); foreach, doParallel (`n_cores > 1`) |
| [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) | rows with any missing value are dropped | caret, earth; e1071 (classification); pROC, PRROC (binary ROC and PR AUC); MLmetrics with pROC (multiclass summary); foreach, doParallel (`n_cores > 1`) |
| [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md) | an error in the outcome or the training predictors; missing test values are not imputed | caret; randomForest (default function set, or `model_method = "rf"`); e1071 (classification); foreach, doParallel (`parallel = TRUE`) |
| [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) | an error | caret, kernlab; e1071 (classification); randomForest (`select_method = "rf_rfe"`); foreach, doParallel (`n_cores > 1`) |
| [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md) | an error | Boruta |
| [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md) | rows with any missing value are dropped, with a warning | MASS |
| [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) | rows missing the outcome, a predictor, or `date_col` are dropped | brms, loo, and a working Stan toolchain; pbapply (progress bar) |
| [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) | rows with a missing numeric value are dropped | ggplot2 (plot); bigstatsr (data of 1e7 cells or more, otherwise [`prcomp()`](https://rdrr.io/r/stats/prcomp.html) is used) |
| [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) | an error | RSpectra (approximate solver only) |

`seed`, parallelism, and which functions use random numbers at all are
covered in [Reproducibility and
parallelism](https://elkronos.github.io/featR/articles/reproducibility.md).
