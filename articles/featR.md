# Getting started with featR

## Why featR

Feature selection in R is spread across a dozen packages, each with its
own calling convention and its own idea of what a result looks like.
Comparing two methods means learning two APIs and writing glue code to
line up the answers.

featR wraps the common methods behind one convention and one return
type, so that swapping
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
for
[`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
is a one-word change. It also tries hard not to flatter itself: where a
method can leak information from a held-out set, featR either prevents
it or says so in the documentation.

## One convention

Every selection function takes the data first and the name of the
outcome column second:

``` r
fs_<method>(data, target, <method options>, ...)
```

Housekeeping arguments come last. Every function takes `verbose`.
Functions that use randomness take `seed = NULL`, and functions that can
run in parallel take `n_cores`, `parallel`, or both. The methods that
have no single outcome,
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md),
[`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md),
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md), and
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md), drop
`target`.
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) calls
its input `x`, because it takes a matrix rather than a table of
observations.

Three conventions hold everywhere:

- **Sequential by default.** Parallelism is opt-in through `n_cores` or
  `parallel`, and requests are capped at the cores you actually have.
- **The RNG is never touched** unless you pass `seed`, and the previous
  random state is restored when the call returns.
- **Modeling engines are optional.** They live in `Suggests`; a function
  that needs one checks at the point of use and tells you what to
  install.

## One return type

Selection functions return an `fs_result`:

``` r

d <- data.frame(
  spread = c(1, 2, 3, 4, 100, 6, 7, 8),
  flat   = rep(2, 8),
  gappy  = c(1, NA, 3, NA, 5, 6, NA, 8)
)

res <- fs_unsupervised(d, method = "variance", threshold = 1)
res
#> <fs_result> unsupervised_variance
#> Selected 2 of 3 features
#>   spread, gappy
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
```

Seven components, the same for every method:

``` r

selected(res)       # the features that were kept
#> [1] "spread" "gappy"
res$scores          # per-feature scores, comparable within a method
#>   spread     flat    gappy 
#> 1147.696    0.000    7.300
res$method          # which method produced this
#> [1] "unsupervised_variance"
res$task            # "classification", "regression", or NA
#> [1] NA
names(res$details)  # everything method-specific
#> [1] "mask"       "indices"    "filtered"   "threshold"  "direction" 
#> [6] "action"     "n_features"
```

[`summary()`](https://rdrr.io/r/base/summary.html) adds the call and a
ranked score table, with an asterisk marking what was selected:

``` r

summary(res)
#> <fs_result> unsupervised_variance
#> Selected 2 of 3 features
#>   spread, gappy
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
#> 
#> Call:
#>   fs_unsupervised(data = d, method = "variance", threshold = 1)
#> 
#> Scores (3 features, ranked):
#>  feature  score selected
#>   spread 1148.0        *
#>    gappy    7.3        *
#>     flat    0.0         
#> (* = selected)
```

Because the shape is shared, methods become interchangeable.
[`selected()`](https://elkronos.github.io/featR/reference/selected.md)
works on the result of any of them.

## A supervised filter

[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
scores each feature against the target – absolute Pearson correlation
for a numeric target, ANOVA F for a factor – and keeps those past a
threshold:

``` r

train <- data.frame(
  strong = c(1, 2, 3, 4, 5, 6),
  mirror = c(6, 5, 4, 3, 2, 1),
  noise  = c(1, 0, 1, 0, 1, 0),
  y      = c(1, 2, 3, 4, 5, 6)
)

fs_supervised(train, target = "y", threshold = 0.9)
#> <fs_result> supervised_correlation (regression)
#> Selected 2 of 3 features
#>   strong, mirror
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
```

Both `strong` and `mirror` have \|r\| = 1: a filter like this ranks
features one at a time, so it cannot tell you that the second is
redundant given the first. That is what
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
is for.

## Removing redundancy

[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
looks only at how the features relate to each other. With `prune = TRUE`
(the default) it returns a reduced set with no two members correlated
above the threshold:

``` r

fs_correlation(train[, c("strong", "mirror", "noise")], threshold = 0.9)
#> <fs_result> correlation_pearson
#> Selected 2 of 3 features
#>   strong, noise
#> Details: corr_matrix, pairs, dropped, redundant, n_features (in $details)
```

Set `prune = FALSE` if you would rather see every feature involved in a
high-correlation pair, which is the set most correlation filters report.

## Information gain

[`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md)
measures how many bits of uncertainty about the target each feature
removes. Numeric features are binned; dates expand into year, month and
day:

``` r

ig <- data.frame(
  perfect = rep(c("a", "b", "c", "c"), 5),
  half    = rep(c("n1", "n2"), 10),
  target  = factor(rep(c("a", "b", "c", "c"), 5))
)

fs_infogain(ig, target = "target")$scores
#> perfect    half 
#>     1.5     0.5
```

Raw information gain rewards high-cardinality features – an ID column
can score near the target’s entropy while predicting nothing
generalizable. Pass `normalize = "gain_ratio"` to divide by each
feature’s own entropy, which corrects that bias.

## Methods that need a modeling engine

The remaining methods depend on suggested packages, so they are shown
here without being run:

``` r

# Regularization
fs_lasso(mtcars, "mpg", nfolds = 5, seed = 1)        # glmnet
fs_elastic(mtcars, "mpg", seed = 1)                  # caret + glmnet

# Wrappers and model-based
fs_boruta(iris, "Species", seed = 1)                 # Boruta
fs_randomforest(iris, "Species", task = "classification", seed = 1)
fs_recursivefeature(mtcars, "mpg", sizes = c(2, 4), seed = 1)  # caret
fs_stepwise(mtcars, "mpg", direction = "both")       # MASS
fs_mars(mtcars, "mpg", seed = 1)                     # caret + earth
fs_svm(iris, "Species", task = "classification",     # caret + kernlab
       feature_select = TRUE, select_method = "svm_rfe", seed = 1)
fs_bayes(mtcars, "mpg", predictors = c("wt", "hp"))  # brms + loo
```

Each errors with the exact
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
call it needs, so nothing fails mysteriously.

## Choosing a method

| If you want | Use |
|----|----|
| A fast first cut on many features | [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md), [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md) |
| To drop near-duplicate features | [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) |
| To rank categorical predictors of a categorical outcome | [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md), [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) |
| A sparse, interpretable linear model | [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md), [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md) |
| Every feature with any relevance, not a minimal set | [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md) |
| Selection tied to the model you will actually deploy | [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md), [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) |
| Non-linear relationships found automatically | [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) |
| Fewer dimensions rather than fewer variables | [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md), [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) |

Filters are cheap and model-agnostic but judge features one at a time.
Wrappers respect interactions but cost a model fit per subset and tie
the answer to that model. Neither is a substitute for validating the
selected set on data that played no part in choosing it.

## What featR will not do quietly

A few behaviors worth knowing, all documented per function:

- Class upsampling in
  [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
  and [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
  happens *inside* resampling folds, so resampled metrics are not
  inflated by duplicated rows.
- [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
  evaluates on rows the selection never saw and trains its final model
  on training rows only.
- [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  refuses to mean-impute by default, because imputing on the full data
  leaks across folds; pass `impute = "mean"` to opt in and get a
  warning.
- [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md)
  labels its p-values as invalid for inference, because post-selection
  they are.
- In-sample metrics are called in-sample.
