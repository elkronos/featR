# Regularization and embedded importance

These methods fit **one** model and read the selection off its
structure: non-zero coefficients
([`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md),
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)),
permutation importance
([`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)),
or the terms a spline model retains
([`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)).
Unlike filters, they judge each predictor in the presence of the others.

The examples use a simulated regression problem with a known answer.
`x1`–`x3` matter, `x1_copy` duplicates `x1`, `x4`–`x10` are noise, and
`x_nl` has a purely non-linear effect.

``` r

set.seed(11)
n <- 300
d <- as.data.frame(matrix(rnorm(n * 10), n, 10,
                          dimnames = list(NULL, paste0("x", 1:10))))
d$x1_copy <- d$x1 + rnorm(n, sd = 0.1)
d$x_nl    <- runif(n, -2, 2)
d$y <- 2 * d$x1 - 1.5 * d$x2 + 0.75 * d$x3 + 1.5 * (d$x_nl^2 - 4 / 3) + rnorm(n)
```

## `fs_lasso()`

[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
fits an L1-penalized linear model with
[`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
and keeps the predictors with non-zero coefficients at the
cross-validated penalty. It handles numeric outcomes only (gaussian
family).

``` r

las <- fs_lasso(d, "y", nfolds = 5, seed = 1)
las
#> <fs_result> lasso (regression)
#> Selected 8 of 12 features
#>   x2, x1, x1_copy, x3, x_nl, x4, x5, x9
#> Details: lambda_min, lambda_1se, lambda_used, coefficients, n_features (in $details)
head(las$scores, 6)
#>   Variable Coefficient AbsCoefficient
#> 1       x2  -1.4237946      1.4237946
#> 2       x1   1.0857606      1.0857606
#> 3  x1_copy   0.7641129      0.7641129
#> 4       x3   0.6011092      0.6011092
#> 5     x_nl   0.2535626      0.2535626
#> 6       x4   0.0425803      0.0425803
```

Things to know:

- **Which penalty.** `lambda = "min"` (the default) uses the penalty
  that minimizes cross-validated error. That choice is tuned for
  prediction and usually keeps some noise variables. `lambda = "1se"`
  uses the largest penalty within one standard error of the minimum,
  which gives a sparser and more stable set.

``` r

selected(fs_lasso(d, "y", nfolds = 5, seed = 1, lambda = "1se"))
#> [1] "x2"      "x1"      "x1_copy" "x3"      "x_nl"
```

- **Correlated predictors.** The lasso tends to keep one member of a
  correlated group and zero out the rest. Here `x1` and `x1_copy`
  compete, and an absent feature is **not** evidence that it is
  unrelated to the outcome.
- **Linear only.** `x_nl` has a strong U-shaped effect but almost no
  linear one. The lasso gives it only a small coefficient, from whatever
  linear trend this sample happens to contain, and badly understates how
  much it matters. Compare the random forest and MARS results below.
- **Scores.** `scores` holds coefficients multiplied by each column’s
  standard deviation. That makes them comparable across predictors
  measured in different units. The raw coefficients are in
  `details$coefficients`.
- **Factors** are expanded into dummy columns, and the selection names
  those columns (`colourred`, not `colour`).
- **Missing values** are an error by default. Mean-imputing on the full
  data before cross-validation leaks information between folds.

## `fs_elastic()`

[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
tunes both the mixing parameter `alpha` and the penalty `lambda` by
resampling, through `caret::train(method = "glmnet")`. It also supports
classification. When the winning `alpha` is below 1, the ridge part of
the penalty spreads weight across correlated predictors, so a correlated
group tends to survive together.

``` r

el <- fs_elastic(d, "y", alpha_seq = c(0.25, 0.5, 1), seed = 1)
el$details$best_alpha
#> [1] 0.5
selected(el)
#>  [1] "x1"      "x2"      "x3"      "x4"      "x5"      "x7"      "x8"     
#>  [8] "x9"      "x1_copy" "x_nl"
```

The default grid is `seq(0.1, 1, by = 0.1)`. It deliberately excludes
`alpha = 0`, pure ridge, which never sets a coefficient to zero: if
ridge won the tuning, every predictor would be “selected”. `scores` here
are absolute coefficients on the scale of the columns as supplied, so
standardize the predictors first if you want to compare them.
`use_pca = TRUE` fits the model on principal components that are
recomputed inside every resample, and then the selection names
components (`PC1`, …) rather than your columns.

## `fs_randomforest()`

[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
splits the data, grows a forest on the training rows, and reports
**permutation importance** plus metrics on the held-out rows. A forest
*ranks* rather than *selects*. Unless you pass a
`control$feature_select` hook, `selected` is every predictor, ordered by
importance.

``` r

rf <- fs_randomforest(d, "y", task = "regression",
                      control = list(ntree = 300), seed = 1)
head(rf$scores, 6)
#>        x2      x_nl        x1   x1_copy        x3        x7 
#> 26.187296 26.142210 18.069593 17.606422  4.794183  1.894942
rf$details$metrics
#> $RMSE
#> [1] 1.645803
#> 
#> $MAE
#> [1] 1.326286
#> 
#> $R2
#> [1] 0.770357
```

The forest finds `x_nl`, which the linear methods cannot. To turn the
ranking into a selection, pass a hook. It runs on the training rows
only, so the held-out metrics stay honest:

``` r

top5 <- function(dt) {
  keep <- c("x1", "x2", "x3", "x_nl", "x1_copy")  # any rule you like
  dt[, c(keep, "y"), with = FALSE]
}
rf_sel <- fs_randomforest(d, "y", task = "regression",
                          control = list(ntree = 300, feature_select = top5),
                          seed = 1)
selected(rf_sel)
#> [1] "x_nl"    "x2"      "x1"      "x1_copy" "x3"
```

`task` must be given explicitly, because a numeric column can be a class
label. Permutation importance splits credit between correlated
predictors (see `x1` and `x1_copy`) and favors predictors with many
distinct values, so treat close scores as ties.

## `fs_mars()`

[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
fits multivariate adaptive regression splines (earth) through caret with
repeated cross-validation. It reports the predictors that the pruned
model retains, and it evaluates on a held-out split. MARS finds
non-linear effects and interactions on its own, using piecewise-linear
hinge functions.

``` r

ma <- fs_mars(d, "y", degree = 1:2, nprune = c(5, 10, 15),
              number = 3, repeats = 1, seed = 1)
selected(ma)
#> [1] "x1_copy" "x_nl"    "x2"      "x3"
ma$details$metrics
#> $RMSE
#> [1] 0.9944926
#> 
#> $MAE
#> [1] 0.8114338
#> 
#> $R2
#> [1] 0.9184193
ma$details$removed_predictors
#> $nzv
#> character(0)
#> 
#> $corr
#> [1] "x1"
```

Before fitting,
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
drops near-zero-variance predictors and one member of each pair
correlated above `corr_cut` (default 0.95). It fits those filters on the
training rows only, and `details$removed_predictors` records what went.
Here one of `x1` and `x1_copy` is removed before the model ever sees it.
The filter does not look at the outcome, so which one goes is arbitrary.
For classification, class upsampling (when requested by caret’s
`sampling`) happens inside each resample. Test-set ROC and PR AUC are
reported for binary outcomes when pROC and PRROC are installed.

The reported regression `R2` is caret’s squared correlation between
predictions and observations, not `1 - SSE/SST`. A biased model can
still score well on it, so read it together with RMSE.
