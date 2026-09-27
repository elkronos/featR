# Validation and leakage

Feature selection is part of model fitting. Any performance estimate
that reuses rows the selection has seen is optimistic, sometimes wildly
so. This article shows the problem on data where the right answer is
known, then shows the patterns featR uses to avoid it.

## Selection bias, demonstrated

Here the outcome is **pure noise**. No feature predicts it, so any
honest estimate of out-of-sample R² should be at or below zero.

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

Selecting outside the cross-validation loop reports that noise predicts
noise. Selecting inside the loop gives the correct answer: nothing does.
This is the selection bias described by Ambroise and McLachlan (2002).
It applies to every method in featR, including the cheap filters.

## Rules featR follows

Where a featR function reports a performance number, it tries to make
that number honest:

- **Held-out splits.**
  [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md),
  [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md),
  [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md),
  and [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
  split the data first. Selection, tuning, and preprocessing that learns
  from data (near-zero-variance filters, correlation filters, one-hot
  encoders, imputation values) are fitted on the training rows only. The
  metrics in `details` come from the held-out rows.
- **Resampling-aware preprocessing.** `fs_elastic(use_pca = TRUE)`
  refits centering, scaling, and PCA inside each resample. Class
  upsampling in
  [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
  and [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
  also happens inside each resample, so duplicated rows never appear on
  both sides of a fold.
- **No silent imputation.**
  [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  refuses missing predictors by default. `impute = "mean"` uses
  full-data means and warns you that it leaks.
- **Labeled in-sample numbers.**
  [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)’s
  `details$mae` and `details$rmse`, and
  [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md)’s
  *t* statistics and p-values, are computed on the rows used for
  selection. The documentation says so.

## What featR cannot do for you

A held-out split inside one function call protects that call’s metrics.
It does not protect your **workflow**. If you try five methods, look at
their test metrics, and keep the best one, the test set has become part
of selection. The usual remedy:

1.  Split off a final test set **once**, at the start.
2.  Do all exploration, method comparison, and tuning on the remaining
    rows, using cross-validation that repeats the selection inside every
    fold, as above.
3.  Touch the test set once, at the end, to report the chosen pipeline’s
    performance.

### Post-selection inference

p-values, confidence intervals, and posterior intervals computed on a
model whose predictors were chosen from the same data are not valid in
the usual sense. They are too small, too narrow, or too confident. That
applies to `summary(fs_stepwise(...)$model)` and to the posterior of the
model
[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
returns. For valid inference, select on one part of the data and
estimate on another (sample splitting), or use a dedicated
selective-inference method.

## A worked pipeline

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
`X1`/`X21` pair. Either one carries the same information. The true model
uses `X1`, `X2`, and `X3`, with noise standard deviation 1. A test RMSE
close to 1 is the best any model can do here. Lasso at `lambda.min`
often keeps a few noise variables as well. That is expected:
`lambda.min` optimizes prediction, not recovery of the true support.

## Reference

Ambroise, C. and McLachlan, G. J. (2002). Selection bias in gene
extraction on the basis of microarray gene-expression data. *PNAS*
99(10), 6562–6566. <https://doi.org/10.1073/pnas.102102699>
