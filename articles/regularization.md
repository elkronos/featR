# Regularization and embedded importance

The four functions on this page each fit one model and read the
selection off that model’s structure.
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
and
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
keep the predictors whose penalized coefficients are not zero,
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
ranks predictors by permutation importance, and
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
keeps the predictors that appear in its pruned spline terms. Because
every predictor is in the model at once, each one is judged in the
presence of the others, which a univariate filter cannot do.

[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
and
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
fit linear models, so they miss a predictor whose effect is curved, and
neither holds out any rows, so neither gives you an honest performance
estimate.
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
and [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
pick up non-linear effects, and both report metrics on a held-out split
that the selection never saw.

For which quantities are honest and which are optimistic across all of
featR, see [Validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.md).
For help choosing between these and the filter or wrapper methods, see
[Choosing a
method](https://elkronos.github.io/featR/articles/choosing-a-method.md).

## Example data

Every example below uses the same simulated regression problem, so you
can check each method against the known answer. `y` depends on `x1`,
`x2` and `x3` linearly and on `x_nl` through a U-shaped curve with
almost no linear trend. `x1_copy` is `x1` plus a little noise, and `x4`
to `x10` are pure noise.

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

The lasso is linear regression with an L1 penalty on the coefficients.
The penalty shrinks coefficients toward zero and sets some of them
exactly to zero, and the predictors left with a non-zero coefficient are
the selection.
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
fits it with
[`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html),
choosing the penalty strength by cross-validation (`nfolds = 5` by
default). It handles numeric outcomes only (gaussian family); for
classification, use
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md).

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

The lasso kept 8 of the 12 predictors: the four real ones, `x1_copy`,
and three noise columns (`x4`, `x5`, `x9`). The default,
`lambda = "min"`, selects at the penalty that minimizes cross-validated
error. That penalty is tuned for prediction, and it usually lets some
noise through. `lambda = "1se"` uses the largest penalty whose error is
within one standard error of the minimum, which gives a sparser and more
stable set. On this data it drops the three noise columns:

``` r

selected(fs_lasso(d, "y", nfolds = 5, seed = 1, lambda = "1se"))
#> [1] "x2"      "x1"      "x1_copy" "x3"      "x_nl"
```

Both penalty values are stored in `details$lambda_min` and
`details$lambda_1se`, whichever one you select at.

`scores` holds each coefficient multiplied by its column’s standard
deviation, so predictors measured in different units can be ranked
against each other. The raw coefficients are in `details$coefficients`.
With `standardize = FALSE` the two tables are identical, and you should
compare them only across predictors that share a scale.

Read the selection with three limits in mind:

- Among strongly correlated predictors, the lasso tends to keep one and
  zero out the rest, so a missing predictor is not evidence that it is
  unrelated to the outcome. Here `x1` and `x1_copy` both survive, but
  they split one effect between them (scores 1.09 and 0.76, which
  together come close to the true coefficient of 2), so neither
  coefficient measures it alone.
- The model is linear. `x_nl` has one of the strongest effects in the
  data, but its score is 0.25 against 1.42 for `x2`, coming only from
  whatever linear trend this sample happens to contain. Compare the
  random forest and MARS results below.
- [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  reports no performance estimate. If you keep the fitted model
  (`return_model = TRUE`), its cross-validated error is the one the
  penalty was chosen to minimize, so it is optimistic.

Factors and character columns are expanded into dummy columns, and the
selection names those columns (`colorred`, not `color`). A factor with
fewer than two observed levels is an error, and so is a dummy name that
collides with another column (factor `g` with level `b` next to a column
called `gb`). Infinite values are an error too.

Missing predictor values are an error under the default
`impute = "none"`. `impute = "mean"` fills them with column means
computed on the whole data set, before cross-validation, so the folds
see each other; featR warns when you use it. If you need imputation, do
it inside your own resampling loop.

`alpha` defaults to 1. A value between 0 and 1 gives an elastic-net
penalty at that fixed mixing value;
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
tunes it for you instead.

## `fs_elastic()`

The elastic net mixes the lasso’s L1 penalty with the L2 (ridge)
penalty. The mixing parameter `alpha` runs from 0 (pure ridge) to 1
(pure lasso).
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
tunes `alpha` and the penalty strength `lambda` together by resampling,
through `caret::train(method = "glmnet")`, with 5-fold cross-validation
unless you pass your own `trControl`. Unlike
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md),
it also handles classification.

``` r

el <- fs_elastic(d, "y", alpha_seq = c(0.25, 0.5, 1), seed = 1)
el$details$best_alpha
#> [1] 0.5
el$details$metric_value
#> [1] 2.110188
selected(el)
#>  [1] "x1"      "x2"      "x3"      "x4"      "x5"      "x7"      "x8"     
#>  [8] "x9"      "x1_copy" "x_nl"
```

The winning `alpha` was 0.5, and 10 of the 12 predictors survived,
including five of the seven noise columns. When `alpha` is below 1, the
ridge part of the penalty spreads weight across correlated predictors,
so a group like `x1` and `x1_copy` tends to stay in together rather than
being cut to one representative. The cost, as here, is a less sparse
selection.

The winner is the pair with the best resampled metric, and picking the
best of many candidates leans toward too little penalty. On pure-noise
data (100 rows, 50 predictors) the default selected 8.5 predictors on
average and at least one in every run. Passing a
[`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html)
with `selectionFunction = "oneSE"` picks the most penalized pair within
one standard error of the best, which selected 1.8 on average in the
same simulation.

The same optimism applies to `details$metric_value`, the resampled RMSE
of the winning pair (2.11 here). It is the best value over the whole
grid, and
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
holds out no test rows, so treat it as optimistic and wrap the whole
call in an outer resampling loop if you need an honest estimate. In
`model$results`, caret’s `Rsquared` is the squared correlation between
predictions and observations, not `1 - SSE/SST`.

The default `alpha_seq` is `seq(0.1, 1, by = 0.1)`. It leaves out
`alpha = 0` because ridge never sets a coefficient exactly to zero; if
you add 0 yourself and it wins, every predictor is “selected” and featR
warns. `scores` are absolute coefficients on the scale of the columns as
supplied (`NULL` for an outcome with more than two classes, where each
predictor has one coefficient per class).
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
does not rescale them the way
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
does, so standardize the predictors first if you want to rank by
`scores`.

Rows with a missing outcome or any missing predictor are dropped before
fitting. Factors are expanded into k - 1 dummy columns, and the
selection names those columns. With `use_pca = TRUE` (and `nPCs` set),
the model is fit on principal components that are recomputed inside
every resample, and the selection then names components (`PC1`, `PC2`,
…) rather than your columns.

## `fs_randomforest()`

A random forest averages many decision trees, each grown on a bootstrap
sample with a random subset of predictors tried at each split.
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
splits the data (`train_ratio = 0.75` by default, set through
`control`), grows the forest on the training rows, and reports
permutation importance plus metrics on the held-out rows. Permutation
importance measures how much the forest’s error grows when one
predictor’s values are shuffled.

`task` defaults to `"classification"`, because a numeric column can be a
class label, so pass `task = "regression"` for a numeric outcome. featR
warns when a numeric target meets the default.

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

The forest ranks `x_nl` second, almost tied with `x2`, where the linear
methods gave it a small coefficient. `x1` and `x1_copy` split their
shared credit (18.1 and 17.6), and the noise columns trail far behind,
led by `x7` at 1.9. These scores are the mean increase in MSE divided by
its standard error over trees; set `control$scale_importance = FALSE`
for the unscaled version. Importance can also favor predictors with many
distinct values, so treat close scores as ties.

A forest ranks predictors without selecting any. Unless you supply a
selection rule, `selected` is every predictor, ordered by importance. To
turn the ranking into a selection, pass a function as
`control$feature_select`. It receives the training rows as a data.table
and returns the columns to keep, including the outcome. The hook below
keeps a fixed list for illustration; a real rule would compute something
from `dt`.

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

The hook runs after the split and sees only the training rows, so the
held-out `details$metrics` stay honest even when the rule looks at the
outcome. The out-of-bag error in `details$oob` does not: the forest is
grown on the same rows the hook just selected on, so after a hook that
consults the outcome the OOB error is optimistic. `control$preprocess`,
by contrast, runs on the full data before the split, so keep it to
row-wise transformations.

Missing predictor values are imputed by default with the median
(numeric) or most common value (other types), learned on the training
rows only. Rows with a missing outcome are dropped with a warning. With
`n_cores` above 1 the forest is grown in pieces and combined, which
discards the OOB results, so `details$oob` is `NULL`.

## `fs_mars()`

MARS (multivariate adaptive regression splines) builds a model from
piecewise-linear hinge functions, adding terms greedily and then pruning
them back. It finds curved effects and, with `degree` above 1,
interactions on its own.
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
fits it with the earth package through caret, tunes `degree` and
`nprune` (the number of terms kept) by repeated cross-validation,
evaluates on a held-out split (`train_ratio = 0.8`), and selects the
predictors that appear in the pruned model. The task follows the
outcome: numeric means regression, factor or character means
classification.

The defaults tune `degree = 1:3` with 5-fold cross-validation repeated 3
times. The example uses a smaller grid to keep it fast.

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

MARS found `x_nl` along with `x2`, `x3` and `x1_copy`, and kept none of
the noise columns. `x1` is missing because of a filter that runs before
the model is fit.
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
drops near-zero-variance predictors and one member of each pair
correlated above `corr_cut` (default 0.95), and
`details$removed_predictors` records what went. Both filters are fit on
the training rows only, and neither looks at the outcome, so which of
`x1` and `x1_copy` survives says nothing about which matters more. Set
`corr_cut = 0` to turn the correlation filter off.

The reported `R2` is caret’s squared correlation between predictions and
observations, not `1 - SSE/SST`, and a biased model can still score well
on it, so read it together with RMSE.
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
reports `1 - SSE/SST`, so the two `R2` values on this page are not
comparable. RMSE is on the same scale for both (0.99 for MARS against
1.65 for the forest), but each comes from one split of a different size,
so on small data treat the gap as rough.

Rows with any missing value are dropped before anything else, so
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
needs fairly complete data. Data with more than `sample_size` rows
(default 10000) are then randomly down-sampled to that size before the
split.

For classification
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
needs the e1071 package, and it up-samples the smaller classes inside
each resample rather than before cross-validation. It treats the first
factor level as the positive class for PR AUC, sensitivity and
specificity, with no override, so use
[`stats::relevel()`](https://rdrr.io/r/stats/relevel.html) to change it.
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
uses the second level unless you set `control$positive_class`, so those
class-specific numbers do not compare across the two functions (ROC AUC
does). Test-set ROC and PR AUC are reported for binary outcomes when
pROC and PRROC are installed.
