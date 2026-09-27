# Choosing a method

featR has fourteen selection functions and two dimensionality-reduction
functions. This page helps you pick one. It gives a decision guide, the
trade-offs between the method families, and a reference table of what
each function needs and accepts.

## Start with the question

Different methods answer different questions. Pick the question first:

| Question | Function |
|----|----|
| Which columns are constant, near-constant, or mostly missing? | [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md) |
| Which columns duplicate each other? | [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) |
| Which numeric columns are individually associated with the outcome? | [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md) |
| Which categorical columns are associated with a categorical outcome? | [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md), [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) |
| How much does each column reduce uncertainty about the outcome, whatever its type? | [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) |
| Which predictors survive in a sparse linear model? | [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md), [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md) |
| Which predictors carry *any* information, including redundant ones? | [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md) |
| How does each predictor rank in a random forest, and how good is that forest on held-out rows? | [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md) |
| Which predictors does a model with automatic non-linearities and interactions use? | [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) |
| What is the smallest subset a particular model needs? | [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md), [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) |
| Which terms does AIC keep in a linear regression? | [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md) |
| Which predictor subset has the best out-of-sample predictive fit under a Bayesian model? | [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) |
| Can I summarize these columns in a few directions? | [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md), [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) |

## Filters, embedded methods, and wrappers

**Filters**
([`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md),
[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md),
[`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md),
[`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md),
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md))
score each feature without fitting a predictive model. They are fast and
scale to thousands of columns. The cost is that they are **univariate**:

- Two copies of the same signal both score highly.
- A feature that matters only in combination with another scores low.

Use filters for a first cut, then refine with something else.

**Embedded methods**
([`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md),
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md),
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md),
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md))
fit one model and read the selection off its structure: non-zero
coefficients, importance, or retained terms. They account for the other
predictors, but the answer belongs to that model family.

**Wrappers**
([`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md),
[`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md),
[`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md),
[`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md),
[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md))
refit a model over many candidate subsets. They are the most expensive
and the most tailored to one model. They are also the most prone to
overfitting the selection itself, so validate their output on held-out
data.

## A sensible default pipeline

For a new tabular problem:

1.  **Clean.** Drop constant or mostly missing columns with
    [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md).
    This uses no outcome, so it is safe to run before any split.
2.  **Split.** Hold out a test set now, before anything looks at the
    outcome.
3.  **Deduplicate.** Prune near-duplicate columns with
    [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
    on the training rows.
4.  **Select.** Run the method that matches your final model on the
    training rows:
    [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
    for a linear model,
    [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
    or
    [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
    for trees,
    [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
    for an SVM.
5.  **Validate.** Evaluate the final model on the test rows.

The [validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.md)
article shows this pipeline end to end.

## Reference: what each function accepts

Every selection function takes `data` first. The functions with an
outcome take `target`, the **name** of the outcome column, second. The
housekeeping arguments differ between functions, so check the table
before you write a loop over several methods:

| Function | `target` | Outcome types | `seed` | Parallel | Engine packages (Suggests) |
|----|----|----|----|----|----|
| [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md) | — | none | — | — | none |
| [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) | — | none | yes | `parallel`, `n_cores` (point-biserial only) | polycor (polychoric), foreach + doParallel |
| [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md) | yes | numeric, factor | — | — | none |
| [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) | yes | categorical | yes | `parallel`, `n_cores` | furrr + future when parallel |
| [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) | yes | any (discretized) | — | — | none |
| [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md) | yes | numeric | yes | `parallel`, `n_cores` | glmnet, Matrix |
| [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md) | yes | numeric, factor | yes | `n_cores` | caret, glmnet, Matrix |
| [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md) | yes | set by `task` | yes | `n_cores` | randomForest, caret, pROC (AUC) |
| [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) | yes | numeric, factor | yes | `n_cores` | caret, earth, pROC/PRROC (AUC) |
| [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md) | yes | numeric, factor | yes | `parallel` | caret, randomForest (default functions), e1071 |
| [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) | yes | set by `task` | yes | `n_cores` | caret, kernlab, e1071, randomForest (rf_rfe) |
| [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md) | yes | numeric, factor | yes | — | Boruta |
| [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md) | yes | numeric | — (deterministic) | — | MASS |
| [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) | yes | any brms family | yes (subset sampling) | `parallel_combinations`, `n_cores` | brms, loo, a Stan toolchain |
| [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) | — | none | — | — | ggplot2 (plot), bigstatsr (large data) |
| [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) | — (`x`) | none | — | — | RSpectra (approximate solver) |

Every function takes `verbose`. All of them are sequential by default.
[`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md),
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md),
and
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
default to `n_cores = 2`, but that value is used only when you also set
`parallel = TRUE`.

If an engine package is missing, the function stops and prints the exact
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
call you need. Nothing fails deep inside a model fit.
