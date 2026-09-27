# The fs_result object

Fourteen of featR’s sixteen functions return the same object, an
`fs_result`. This article covers what is in it, how to read it, and how
to use it to compare methods. The two exceptions,
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) and
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md), do
dimensionality reduction and return their own lists. The [dimensionality
reduction](https://elkronos.github.io/featR/articles/dimensionality-reduction.md)
article covers them.

## Anatomy

An `fs_result` is a plain list with seven elements, always in this
order:

| Element | Type | Meaning |
|----|----|----|
| `selected` | character | The features the method kept. It may be `character(0)`, never `NULL`. |
| `scores` | named numeric, data.frame, or `NULL` | Per-feature scores. They are comparable within one method, not across methods. |
| `method` | string | What actually ran, for example `"supervised_correlation"` or `"svm_linear"`. |
| `task` | string | `"classification"`, `"regression"`, or `NA`. |
| `model` | object or `NULL` | The fitted model, when the method fits one. |
| `details` | named list | Method-specific extras, documented on each help page. |
| `call` | call | The matched call. |

``` r

d <- data.frame(
  signal = c(1.1, 2.3, 2.9, 4.2, 5.1, 5.8, 7.2, 8.1),
  flip   = c(8, 7, 6, 5, 4, 3, 2, 1),
  noise  = c(3, 1, 4, 1, 5, 9, 2, 6),
  y      = 1:8
)
res <- fs_supervised(d, target = "y", threshold = 0.5)

str(unclass(res), max.level = 1, give.attr = FALSE)
#> List of 7
#>  $ selected: chr [1:2] "signal" "flip"
#>  $ scores  : Named num [1:3] 0.998 1 0.477
#>  $ method  : chr "supervised_correlation"
#>  $ task    : chr "regression"
#>  $ model   : NULL
#>  $ details :List of 7
#>  $ call    : language fs_supervised(data = d, target = "y", threshold = 0.5)
```

## Reading a result

[`print()`](https://rdrr.io/r/base/print.html) gives a compact overview:

``` r

res
#> <fs_result> supervised_correlation (regression)
#> Selected 2 of 3 features
#>   signal, flip
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
```

[`summary()`](https://rdrr.io/r/base/summary.html) adds the call and a
ranked score table. An asterisk marks the features that were kept:

``` r

summary(res)
#> <fs_result> supervised_correlation (regression)
#> Selected 2 of 3 features
#>   signal, flip
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
#> 
#> Call:
#>   fs_supervised(data = d, target = "y", threshold = 0.5)
#> 
#> Scores (3 features, ranked):
#>  feature  score selected
#>     flip 1.0000        *
#>   signal 0.9978        *
#>    noise 0.4775         
#> (* = selected)
```

[`selected()`](https://elkronos.github.io/featR/reference/selected.md)
is the stable accessor for the chosen names. Prefer it to `res$selected`
in code you intend to keep:

``` r

selected(res)
#> [1] "signal" "flip"
```

## What `scores` means, method by method

Scores are only comparable **within** one method. A correlation of 0.8
and a Boruta importance of 0.8 have nothing in common. The direction
also differs: for
[`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md),
smaller is better. [`summary()`](https://rdrr.io/r/base/summary.html)
knows this and sorts accordingly.

| Function | `scores` | Better is |
|----|----|----|
| [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md) | absolute Pearson *r* (numeric target) or ANOVA *F* (factor target) | larger |
| [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md) | variance, MAD, IQR, range, missing proportion, or distinct count | depends on the criterion |
| [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) | multiplicity-adjusted p-value | **smaller** |
| [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) | information gain in bits, or gain ratio | larger |
| [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) | each variable’s largest absolute correlation with any other variable | larger means **more redundant** |
| [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md) | data.frame of standardized coefficients (`AbsCoefficient` ranks) | larger magnitude |
| [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md) | absolute coefficients on the scale the model saw (`NULL` for multinomial) | larger |
| [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md) | permutation importance (mean decrease in accuracy, or %IncMSE) | larger |
| [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) | earth variable importance via [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html) | larger |
| [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md) | resample-averaged importance from the RFE model | larger |
| [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) | SVM-RFE criterion `w^2`, or random-forest importance (`NULL` without selection) | larger |
| [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md) | median Boruta importance | larger |
| [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md) | absolute *t* statistics of the retained terms (not valid for inference) | larger |
| [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) | `NULL`: selection is between whole models, so there is no per-feature score | not applicable |

Some methods score **design-matrix columns** rather than original
columns.
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md),
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md),
and [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
all do this. A factor predictor `colour` therefore shows up as
`colourgreen`, `colourred`, and so on. Map the names back yourself if
you need the original variables.

## Comparing methods

Because every result has the same shape, a comparison is a loop:

``` r

d2 <- data.frame(
  x1 = c(2.1, 3.9, 6.2, 7.8, 10.1, 12.2, 13.8, 16.1, 18.2, 19.9),
  x2 = c(1, 3, 2, 5, 4, 6, 8, 7, 10, 9),
  x3 = c(5, 5, 4, 5, 6, 5, 4, 5, 6, 5),
  y  = c(1.0, 2.1, 2.9, 4.2, 5.0, 6.1, 6.8, 8.1, 9.0, 9.9)
)

runs <- list(
  supervised = fs_supervised(d2, "y", threshold = 0.9),
  infogain   = fs_infogain(d2, "y", top_n = 2)
)
lapply(runs, selected)
#> $supervised
#> [1] "x1" "x2"
#> 
#> $infogain
#> [1] "x1" "x2"
```

Agreement between methods that work on different principles is weak
evidence that a feature matters. Disagreement tells you to look closer.
Neither is a substitute for validating the final set on data that played
no part in choosing it. The [validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.md)
article covers that.

## Empty selections

When nothing passes, `selected` is `character(0)` and a warning says so.
Code that consumes results should handle a length-zero selection:

``` r

none <- suppressWarnings(fs_unsupervised(d[, 1:3], threshold = 1e6))
selected(none)
#> character(0)
length(selected(none)) == 0
#> [1] TRUE
```
