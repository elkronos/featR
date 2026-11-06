# Getting started with featR

## What featR is for

Feature selection in R is spread across a dozen packages, each with its
own calling convention and its own idea of what a result looks like.
Comparing two methods means learning two APIs and writing glue code to
line up the answers.

featR wraps 14 selection methods behind one calling convention and one
return type, so swapping
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
for
[`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
is a one-word change. The methods cover statistical filters, regularized
and tree-based models, and wrappers that refit a model per candidate
subset. Two dimensionality-reduction helpers,
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) and
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md),
share the calling convention but not the return type.

## Installing

The released version is on CRAN:

``` r

install.packages("featR")
```

The development version is on GitHub:

``` r

# install.packages("remotes")
remotes::install_github("elkronos/featR")
```

featR itself depends only on data.table, withr, and base R. The modeling
engines (glmnet, caret, Boruta, randomForest, brms, and others) are in
`Suggests`. A function that needs one checks when you call it and stops
with the
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
line you need. [Optional
dependencies](https://elkronos.github.io/featR/articles/reproducibility.html#optional-dependencies)
has one command that installs all of them.

## The calling convention

Every selection function takes the data first and the name of the
outcome column second:

``` r
fs_<method>(data, target, <method options>, ...)
```

Housekeeping arguments come last. Every function takes `verbose`.
Functions that use randomness take `seed = NULL`, and functions that can
run in parallel take `n_cores`, `parallel`, or both. The four methods
with no outcome,
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md),
[`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md),
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md), and
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md), drop
`target`.
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) calls
its input `x`, because it takes a matrix rather than a table of
observations.

featR runs sequentially unless you ask for parallel workers, and caps
any request at the cores the machine has. It never touches your random
number generator unless you pass `seed`, and then restores the previous
state when the call returns. [Reproducibility and
parallelism](https://elkronos.github.io/featR/articles/reproducibility.html)
has the details.

Every selection function returns an `fs_result`, and
[`selected()`](https://elkronos.github.io/featR/reference/selected.md)
pulls the kept feature names out of any of them.
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) and
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md)
reduce dimensions rather than choose variables, so they return their own
decomposition lists instead.

## A worked example

This example predicts `mpg` in `mtcars` from the other ten columns. It
uses two filters, both of which run with no suggested packages
installed.

[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
scores each feature against the target on its own: absolute Pearson
correlation when the target is numeric, a one-way ANOVA F statistic when
it is categorical. The two scales differ (\|r\| runs from 0 to 1, F has
no upper bound), so a threshold that suits one is meaningless for the
other. Keep the features with \|r\| above 0.5:

``` r

relevant <- fs_supervised(mtcars, target = "mpg", threshold = 0.5)
relevant
#> <fs_result> supervised_correlation (regression)
#> Selected 8 of 10 features
#>   cyl, disp, hp, drat, wt, vs, am, carb
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
```

Eight of the ten columns pass. `gear` (\|r\| = 0.48) and `qsec` (0.42)
fall below the cut. The scores sit in `relevant$scores`:

``` r

round(sort(relevant$scores, decreasing = TRUE), 2)
#>   wt  cyl disp   hp drat   vs   am carb gear qsec 
#> 0.87 0.85 0.85 0.78 0.68 0.66 0.60 0.55 0.48 0.42
```

A filter that looks at one feature at a time cannot see that two
survivors carry the same information. `cyl` and `disp` both score 0.85
here, and they are correlated 0.90 with each other.
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
ignores the outcome and looks only at how the features relate to one
another. With `prune = TRUE`, the default, it drops features one at a
time until no remaining pair is correlated above the threshold:

``` r

pruned <- fs_correlation(mtcars[, selected(relevant)], threshold = 0.8)
pruned$details$dropped
#> [1] "cyl"  "disp"
```

It dropped `cyl` and `disp`.
[`summary()`](https://rdrr.io/r/base/summary.html) shows the call and a
ranked score table, with an asterisk on each feature that was kept. For
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md),
a feature’s score is its largest absolute correlation with any other
input feature, dropped ones included. So `wt` (0.89 with `disp`) and
`hp` (0.83 with `cyl`) score above 0.8 and survive anyway, because their
partners were dropped.

``` r

summary(pruned)
#> <fs_result> correlation_pearson
#> Selected 6 of 8 features
#>   hp, drat, wt, vs, am, carb
#> Details: corr_matrix, pairs, dropped, redundant, n_features (in $details)
#> 
#> Call:
#>   fs_correlation(data = mtcars[, selected(relevant)], threshold = 0.8)
#> 
#> Scores (8 features, ranked):
#>  feature  score selected
#>     drat 0.7127        *
#>       am 0.7127        *
#>     carb 0.7498        *
#>       vs 0.8108        *
#>       hp 0.8324        *
#>       wt 0.8880        *
#>      cyl 0.9020         
#>     disp 0.9020         
#> (* = selected)
```

[`selected()`](https://elkronos.github.io/featR/reference/selected.md)
returns the final set, ready to pass to a model:

``` r

keep <- selected(pruned)
keep
#> [1] "hp"   "drat" "wt"   "vs"   "am"   "carb"
fit <- lm(reformulate(keep, response = "mpg"), data = mtcars)
```

[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
looked at `mpg` to choose these six columns, so any error estimate for
`fit` computed on the same 32 rows is optimistic. Run the selection on
training rows only and score the model on rows it never saw. [Validation
and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.html)
shows how, and lists which numbers featR reports are honest held-out
estimates.

To try another method, change the function and its options. With glmnet
installed, the lasso version of the same question is:

``` r

lasso <- fs_lasso(mtcars, target = "mpg", seed = 1)
selected(lasso)
#> [1] "wt"   "cyl"  "hp"   "am"   "carb" "drat"
```

[The fs_result
object](https://elkronos.github.io/featR/articles/fs-result.html) covers
the seven components, how to read
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html), what `scores` means
for each method, how to compare results side by side, and what to do
when nothing is selected.

## The four families

| Family | Functions | Guide |
|----|----|----|
| Filters | [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md), [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md), [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md), [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md), [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) | [Filter methods](https://elkronos.github.io/featR/articles/filters.html) |
| Regularization and embedded importance | [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md), [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md), [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md), [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) | [Regularization and embedded importance](https://elkronos.github.io/featR/articles/regularization.html) |
| Wrappers | [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md), [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md), [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md), [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md), [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) | [Wrapper methods](https://elkronos.github.io/featR/articles/wrappers.html) |
| Dimensionality reduction | [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md), [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) | [Dimensionality reduction](https://elkronos.github.io/featR/articles/dimensionality-reduction.html) |

## Where to go next

If you are not sure which function fits your data, start with [Choosing
a
method](https://elkronos.github.io/featR/articles/choosing-a-method.html).
It pairs common questions with functions and has a table of what each
function accepts: task type, data types, missing values, and which
suggested package it needs.

Before you report any number from a selection, read [Validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.html).
The full list of functions and arguments is in the
[reference](https://elkronos.github.io/featR/reference/index.html).
