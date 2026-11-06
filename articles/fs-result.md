# The fs_result object

Fourteen of featR’s sixteen functions return the same object, an
`fs_result`. The two exceptions,
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) and
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md), do
dimensionality reduction and return their own lists; the [dimensionality
reduction](https://elkronos.github.io/featR/articles/dimensionality-reduction.md)
article covers them.
[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
and
[`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
also have an `output` argument that can return the filtered data, a
logical mask, or column indices instead; its default, `"result"`, gives
an `fs_result`.

## The seven components

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

The first line names the method and, in parentheses, the task. The
second counts the kept features against the candidates, and the third
lists the kept names, cut off after the first 10 (`print(res, n = 20)`
shows more). A `Model:` line appears when the method stored a fitted
model, and the last line names the elements of `details`, which are
described under Value on each function’s help page.

[`summary()`](https://rdrr.io/r/base/summary.html) adds the call and a
table of up to 20 scores (set `n` for more), ranked strongest first. An
asterisk marks the features that were kept:

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

Here `scores` is the absolute correlation with `y`. `flip` is a perfect
negative correlation, so it scores 1 and ranks above `signal` (0.998).
`noise` scores 0.48, just under the 0.5 threshold.

[`selected()`](https://elkronos.github.io/featR/reference/selected.md)
is the stable accessor for the chosen names. Prefer it to `res$selected`
in code you intend to keep:

``` r

selected(res)
#> [1] "signal" "flip"
```

## What `scores` means, method by method

A correlation of 0.8 and a Boruta importance of 0.8 have nothing in
common, so compare scores only within one method. The table gives the
quantity each function reports and which direction is stronger.

| Function | `scores` | Stronger is |
|----|----|----|
| [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md) | the chosen criterion per column: variance, MAD (on the standard-deviation scale), IQR, range, proportion missing, or number of distinct values; `NA` when undefined | depends on the criterion and on `direction` |
| [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) | each variable’s largest absolute correlation with any other variable; `NA` when none could be computed | larger means more redundant |
| [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md) | absolute Pearson *r* for a numeric outcome, in \[0, 1\]; one-way ANOVA *F* for a categorical outcome, `Inf` for perfect separation; `NA` for a constant feature | larger |
| [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) | p-value adjusted for multiple testing (Bonferroni by default); `NA` for a feature that could not be tested | smaller |
| [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) | information gain in bits, or the gain ratio with `normalize = "gain_ratio"`; sorted, largest first | larger |
| [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md) | a data.frame with `Variable`, signed `Coefficient`, and `AbsCoefficient`, one row per design-matrix column, zeros included; standardized (coefficient times the column’s standard deviation) when `standardize = TRUE`, the default. Raw coefficients are in `details$coefficients` | larger magnitude |
| [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md) | absolute coefficients at the winning alpha and lambda, on the scale of the columns the model saw (not standardized), zeros included; `NULL` for multinomial fits | larger |
| [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md) | permutation importance: mean decrease in accuracy, or mean increase in MSE for regression; divided by its standard error unless `control$scale_importance = FALSE`; `NULL` when `control$importance = FALSE` | larger; negative means worse than a permuted copy |
| [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) | unscaled earth variable importance from [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html), one entry per original predictor; pruned predictors score 0, and a predictor the final model uses can score 0 or less; `NULL` if `varImp()` fails | larger, but `selected` is the predictors in the final model, not a cut on the score |
| [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md) | each candidate’s importance in the full-size fit (all candidates present), averaged over every resample; falls back to [`caret::varImp()`](https://rdrr.io/pkg/caret/man/varImp.html) when the function set records no per-resample importance | larger |
| [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) | with SVM-RFE, the squared weight `w^2` of each column in the first, full-feature linear SVM; with `select_method = "rf_rfe"`, random-forest importance averaged over resamples; `NULL` when `feature_select = FALSE` | larger |
| [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md) | median Boruta importance over the runs; `NA` for a feature with no importance history | larger |
| [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md) | one entry per retained term: the absolute *t* statistic, or the largest absolute *t* among a factor’s or interaction’s coefficients. Biased upward by the selection, so not valid for inference | larger |
| [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) | `NULL`: the method compares whole models, so there is no per-feature score | not applicable |

[`summary()`](https://rdrr.io/r/base/summary.html) ranks by these rules:

- Smaller ranks first for
  [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md),
  for `fs_correlation(prune = TRUE)` (which keeps the least redundant
  variables), and for
  [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
  and
  [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
  when the kept side is the low one (`direction = "below"` with
  `action = "keep"`, or `"above"` with `"remove"`).
- [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  ranks by absolute coefficient. Every other method ranks by the signed
  value, so a negative permutation or Boruta importance ranks below a
  useful feature instead of above it.
- For
  [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
  and `fs_correlation(prune = TRUE)`, which do not select by a cut on
  the score, the selected features are listed first, each group in score
  order, so a kept feature is never ranked below a dropped one.

### Design-matrix columns

Some methods expand each factor or character predictor into one column
per level and then score and select those columns. A factor `color` then
shows up under its level columns, and you have to map them back yourself
if you want the original variables.

- [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  and
  [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
  paste the level onto the name: `colorgreen`, `colorred`.
- [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) and
  `fs_recursivefeature(handle_categorical = TRUE)` use caret’s
  `dummyVars()`, which puts a dot between name and level for factors
  with three or more levels (`color.green`, `color.red`) and none for a
  two-level factor (`g1` for factor `g` with levels `"0"` and `"1"`).
- [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md)
  keys `scores` and `selected` by term, so the factor appears once as
  `color`. The per-coefficient values (`colorgreen`, `colorred`) are in
  `details$coefficient_scores`.
- [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
  attributes each dummy’s importance back to its source predictor and
  reports `color`.

## Comparing methods

Because every result has the same shape, comparing methods is a loop:

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
evidence that a feature matters, and disagreement is a reason to look
closer. Neither replaces validating the final set on data that played no
part in choosing it; the [validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.md)
article shows how.

## Empty selections

When nothing passes, `selected` is `character(0)`.
[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
and
[`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
also warn that nothing met the criteria; the other functions do not warn
about an empty selection. Code that consumes results should handle a
length-zero selection:

``` r

none <- suppressWarnings(fs_unsupervised(d[, 1:3], threshold = 1e6))
selected(none)
#> character(0)
length(selected(none)) == 0
#> [1] TRUE
```

An empty selection from a threshold filter usually means the threshold
is stricter than every score.
[`summary()`](https://rdrr.io/r/base/summary.html) still ranks every
scored feature, so read the scores there before you choose a new cut. An
empty [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md)
result can also come from Monte Carlo p-values that cannot get below
`sig_level` after the multiple-testing adjustment; featR warns when that
happens, and [Filter
methods](https://elkronos.github.io/featR/articles/filters.md) explains
the fix. If `fs_lasso(lambda = "1se")` keeps nothing, the default
`lambda = "min"` selects at a smaller penalty and usually keeps more.
