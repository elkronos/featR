# Validation and leakage

Feature selection is easy to get wrong in a way that makes results look
better than they are. The usual cause is that the rows used to choose
the features are also used to judge them. This article shows the problem
on data where the right answer is known, lists which numbers featR
reports are honest and which are optimistic, and walks through a
pipeline that keeps them apart.

## The rule

Anything that looks at the outcome is part of model fitting. That covers
choosing features, tuning, and any imputation or scaling learned from
the data. If rows that will later measure performance took part in any
of those steps, the measurement is optimistic. The classic case is
selecting features on the full data and then cross-validating a model on
those features. On pure noise it reports a fit where there is none.

## Selection bias, demonstrated

Here the outcome is pure noise. No feature predicts it, so an honest
estimate of out-of-sample R² should be at or below zero. The code picks
the 10 predictors most correlated with `y` using
[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md),
fits [`lm()`](https://rdrr.io/r/stats/lm.html) on them, and scores it by
5-fold cross-validation, once with the selection outside the folds and
once inside.

``` r

set.seed(42)
n <- 60; p <- 500
X <- as.data.frame(matrix(rnorm(n * p), n, p))
X$y <- rnorm(n)

folds <- sample(rep(1:5, length.out = n))

cv_r2 <- function(select_inside) {
  pred <- numeric(n)
  if (!select_inside) {
    # WRONG: choose the 10 "best" features using all rows, including every
    # future test fold
    keep <- names(sort(fs_supervised(X, "y")$scores, decreasing = TRUE))[1:10]
  }
  for (k in 1:5) {
    train <- X[folds != k, ]
    test  <- X[folds == k, ]
    if (select_inside) {
      # RIGHT: choose features using the training fold only
      keep <- names(sort(fs_supervised(train, "y")$scores,
                         decreasing = TRUE))[1:10]
    }
    fit <- lm(y ~ ., data = train[, c(keep, "y")])
    pred[folds == k] <- predict(fit, test)
  }
  1 - sum((X$y - pred)^2) / sum((X$y - mean(X$y))^2)
}

c(selection_outside_cv = cv_r2(FALSE), selection_inside_cv = cv_r2(TRUE))
#> selection_outside_cv  selection_inside_cv 
#>            0.4496028           -0.8262182
```

With the selection outside the loop, cross-validation reports an R² of
0.45 for a model of noise. With the selection inside the loop, R² is
-0.83. Nothing predicts `y`, and a model of 10 noise predictors fitted
on 48 rows predicts worse than the mean, so a negative R² is what an
honest estimate should show. Ambroise and McLachlan (2002) describe this
selection bias. It applies to every method in featR, including the cheap
filters.

## What featR does for you

Where a featR function reports a performance number, it tries to make
that number honest, and where it cannot, the documentation says so.

- [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md),
  [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md),
  [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md),
  and [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
  split the data into training and test rows first. Selection, tuning,
  and any preprocessing that learns from the data (near-zero-variance
  filters, correlation filters, one-hot encoders, imputation values) are
  fitted on the training rows only, so their test-set metrics come from
  rows the selection never saw. The exception is
  [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)’s
  `control$preprocess` hook, which runs on the full data before the
  split.
- [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
  repeats SVM-RFE inside every cross-validation fold when it chooses how
  many features to keep. `fs_elastic(use_pca = TRUE)` refits the
  centering, scaling, and PCA inside every resample. Class upsampling in
  [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
  and [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
  also happens inside each resample, so a duplicated row never appears
  on both sides of a fold.
- [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  refuses missing predictors by default instead of imputing them from
  the full data. If you opt in with `impute = "mean"`, it uses full-data
  means and warns that this leaks.
- Numbers computed on the rows used for selection are documented as
  optimistic. The table below lists them.
- Known false-positive behavior is documented too.
  [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
  can confirm pure-noise features, especially with
  `resolve_tentative = TRUE`, which settles undecided features with
  [`Boruta::TentativeRoughFix()`](https://rdrr.io/pkg/Boruta/man/TentativeRoughFix.html);
  the [wrapper
  methods](https://elkronos.github.io/featR/articles/wrappers.md) guide
  shows a real case.

## Which reported numbers are honest

A number is honest here if it was computed on rows that took no part in
choosing the features or the tuning values. Anything else is optimistic
and will, on average, flatter the model.

| Function | Honest (held-out) | Optimistic or in-sample |
|----|----|----|
| [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md), [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md), [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md), [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) | No performance number. They never look at an outcome. | None from the function. |
| [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md), [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) | No performance number. | `scores` describe the rows you passed. Any model then scored on those rows is optimistic. |
| [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) | No performance number. | `scores` are adjusted p-values of one test per feature. They are marginal tests, not a model’s performance, and a model built on the significant features and scored on the same rows is optimistic. |
| [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md) | None. No rows are held out. | The cross-validated error curve in `model$cvm` (with `return_model = TRUE`). Its value at `lambda.min` is the minimum over the whole lambda path. |
| [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md) | None. No rows are held out. | `details$metric_value` (the best value over the whole alpha/lambda grid) and `model$results`. |
| [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md) | `details$metrics` | `details$oob` when a `control$feature_select` hook looked at the target. Anything learned in `control$preprocess`, which runs before the split, also flatters `details$metrics`. |
| [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) | `details$metrics` | `model$results` and `model$resample`: tuning cross-validation on the training rows, and the best row over the grid. |
| [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md) | `details$test_metrics` | `details$resampling_results`, and `model$results` when `return_final_model = TRUE`. |
| [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) | `details$performance` | The chosen size’s entry in `details$selection$size_scores` (the best of up to six estimates), and `model$results` and `model$resample` when `feature_select = TRUE`. |
| [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md) | No performance number. | `details$decisions` can confirm noise features. In the simulation described in [`?fs_boruta`](https://elkronos.github.io/featR/reference/fs_boruta.md) (one real and three noise predictors, 150 rows), Boruta confirmed 5 percent of the noise predictors, and 8.3 percent with `resolve_tentative = TRUE`. |
| [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md) | None. | `scores` (absolute *t* statistics), the p-values in `details$coefficients`, and anything from `summary(model)`. |
| [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) | None. | `details$mae` and `details$rmse` (in-sample), `details$best_elpd` and `details$loo_comparison` (the winner of the search), and the posterior of the returned model. |

The held-out metrics also follow different conventions across functions.
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md),
[`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md),
and [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
report caret’s R², which is the squared correlation between predictions
and observations. It is never negative and ignores bias, so predictions
of `2 * y` score 1.
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
reports `1 - SSE/SST` on the test rows, which can be negative. For
binary classification,
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
treats the second factor level as the positive class by default and
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) the
first. ROC AUC does not depend on that choice, but sensitivity and PR
AUC do.

Every held-out number above comes from one train/test split, so on small
data it is noisy. A held-out split protects the metrics of that one
call, not your workflow.

## What you still have to do

If you try five methods, look at their test metrics, and keep the best
one, the test set has become part of selection. The remedy:

1.  Split off a final test set once, before anything looks at the
    outcome.
2.  Do all exploration, method comparison, and tuning on the remaining
    rows. Where you cross-validate, repeat the selection inside every
    fold, as in the demonstration above.
3.  Use the test set once, at the end, to report the pipeline you chose.

## A worked pipeline

The data have 20 independent standard-normal predictors plus `X21`, a
near-copy of `X1`. The outcome depends on `X1`, `X2`, and `X3` with
noise standard deviation 1.

``` r

set.seed(7)
n <- 300
d <- data.frame(matrix(rnorm(n * 20), n, 20))
d$X21 <- d$X1 + rnorm(n, sd = 0.05)                  # near-duplicate of X1
d$y <- 1.5 * d$X1 - 1 * d$X2 + 0.5 * d$X3 + rnorm(n)

# 1. Split once
test_rows <- sample(n, 75)
train <- d[-test_rows, ]
test  <- d[test_rows, ]

# 2. Outcome-free cleaning, then redundancy pruning, on training rows only
predictors <- setdiff(names(train), "y")
dedup <- fs_correlation(train[, predictors], threshold = 0.95)
dedup$details$dropped
#> [1] "X1"

# 3. Model-based selection on training rows only
sel <- fs_lasso(train[, c(selected(dedup), "y")], "y", nfolds = 5, seed = 1)
selected(sel)
#> [1] "X21" "X2"  "X3"  "X20" "X19" "X11" "X18"

# 4. Refit and evaluate once on the untouched test rows
fit <- lm(reformulate(selected(sel), "y"), data = train)
pred <- predict(fit, test)
c(test_RMSE = sqrt(mean((test$y - pred)^2)))
#> test_RMSE 
#>  1.159347
```

[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
does not look at the outcome, so it may keep either member of the
`X1`/`X21` pair. Here it dropped `X1` and kept `X21`, which carries the
same information. Lasso then kept `X21`, `X2`, and `X3` plus four noise
predictors, because `lambda.min` optimizes prediction rather than
recovery of the true predictors; `lambda = "1se"` gives a sparser
choice. The noise standard deviation is 1, so no model can do much
better than a test RMSE of 1. This pipeline’s 1.16 is an honest estimate
from 75 rows that took no part in any step.

## Post-selection inference

The p-values, confidence intervals, and posterior intervals of a model
whose predictors were chosen on the same data are not valid in the usual
sense. They are too small, too narrow, or too confident. That applies to
`summary(fs_stepwise(...)$model)` and to the posterior of the model
[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
returns. Treat them as descriptive. For valid inference, select on one
part of the data and estimate on another (sample splitting), or use a
dedicated selective-inference method.

## Reference

Ambroise, C. and McLachlan, G. J. (2002). Selection bias in gene
extraction on the basis of microarray gene-expression data. *PNAS*
99(10), 6562–6566. <https://doi.org/10.1073/pnas.102102699>
