# Filter methods

Filters score each feature without fitting a predictive model. They are
the cheapest methods in featR and the right first step on wide data.
They are also **univariate**: each feature is judged on its own, so a
filter cannot see redundancy or interactions.

The examples below use one simulated data set, so the answers are known
in advance:

``` r

set.seed(1)
n <- 200
sim <- data.frame(
  x_strong = rnorm(n),
  x_weak   = rnorm(n),
  x_noise  = rnorm(n),
  x_const  = 5,
  x_gappy  = ifelse(runif(n) < 0.6, NA, rnorm(n))
)
sim$x_copy <- sim$x_strong + rnorm(n, sd = 0.05)   # near-duplicate
sim$y      <- 2 * sim$x_strong + 0.5 * sim$x_weak + rnorm(n)
sim$class  <- factor(ifelse(sim$y > 0, "pos", "neg"))
```

## `fs_unsupervised()`: clean-up without an outcome

[`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
scores each column by a target-free statistic: `"variance"`, `"mad"`,
`"iqr"`, `"range"`, `"missing_prop"`, or `"n_unique"`. Because it never
looks at the outcome, it is safe to run before a train/test split.

Drop constant columns:

``` r

num <- sim[, c("x_strong", "x_weak", "x_noise", "x_const", "x_copy")]
fs_unsupervised(num, method = "variance", threshold = 0)
#> <fs_result> unsupervised_variance
#> Selected 4 of 5 features
#>   x_strong, x_weak, x_noise, x_copy
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
```

Drop columns that are more than half missing. Note `action = "remove"`.
The default `action = "keep"` would keep the *most* missing columns, and
featR warns if you leave all three of `threshold`, `direction`, and
`action` at their defaults for `"missing_prop"`.

``` r

fs_unsupervised(sim[, c("x_strong", "x_gappy")], method = "missing_prop",
                threshold = 0.5, action = "remove")$selected
#> [1] "x_strong"
```

Thresholds are on the scale of the statistic: a variance is in squared
units, `"missing_prop"` is in \[0, 1\], and `"n_unique"` is a count. A
threshold chosen for one method means nothing for another. `"mad"` uses
R’s default normal-consistency constant (1.4826), so it estimates a
standard deviation.

## `fs_supervised()`: one feature at a time against the outcome

For a numeric target the score is the absolute Pearson correlation. For
a factor target it is the one-way ANOVA *F* statistic. `method = "auto"`
picks the right one.

``` r

reg <- sim[, c("x_strong", "x_weak", "x_noise", "x_copy", "y")]
res <- fs_supervised(reg, target = "y", threshold = 0.2)
summary(res)
#> <fs_result> supervised_correlation (regression)
#> Selected 2 of 4 features
#>   x_strong, x_copy
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
#> 
#> Call:
#>   fs_supervised(data = reg, target = "y", threshold = 0.2)
#> 
#> Scores (4 features, ranked):
#>   feature   score selected
#>  x_strong 0.85750        *
#>    x_copy 0.85560        *
#>    x_weak 0.17000         
#>   x_noise 0.05669         
#> (* = selected)
```

`x_copy` is kept alongside `x_strong` even though it adds nothing. This
is the univariate blind spot. The *F* scale for a factor target is
unbounded, so pick a threshold on that scale:

``` r

cls <- sim[, c("x_strong", "x_weak", "x_noise", "class")]
fs_supervised(cls, target = "class", threshold = 10)$scores
#>    x_strong      x_weak     x_noise 
#> 141.5738542   5.4736576   0.2682509
```

Pearson correlation measures **linear** association only. A feature with
a strong U-shaped effect can score near zero. Use
[`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md)
or a model-based method if you suspect non-linear effects.

## `fs_correlation()`: remove redundancy

[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
ignores the outcome and looks at how the features relate to each other.
With `prune = TRUE` (the default) it drops features one at a time until
no two survivors correlate above `threshold`:

``` r

red <- fs_correlation(num[, c("x_strong", "x_weak", "x_noise", "x_copy")],
                      threshold = 0.9)
red$selected
#> [1] "x_strong" "x_weak"   "x_noise"
red$details$dropped
#> [1] "x_copy"
red$details$pairs
#>       Var1   Var2 Correlation
#> 1 x_strong x_copy   0.9984364
```

Pruning uses the
[`caret::findCorrelation()`](https://rdrr.io/pkg/caret/man/findCorrelation.html)
heuristic. Within the strongest remaining pair, it drops the member with
the larger mean absolute correlation to everything else. That choice
does not consult the outcome, so it may drop the member that predicts
better.
[`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
prunes by importance instead.

Supported correlation types are `"pearson"`, `"spearman"`, `"kendall"`,
`"polychoric"` (ordered factors, needs polycor), and `"pointbiserial"`
(continuous against dichotomous).

## `fs_chi()`: categorical features against a categorical outcome

[`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) runs
a chi-squared test of independence for every factor or character
feature. It applies a multiple-testing correction (Bonferroni by
default) and selects features whose adjusted p-value is below
`sig_level`. When any expected cell count is below 5 it switches to a
Monte-Carlo p-value, so pass `seed` for reproducibility.

``` r

cat_data <- data.frame(
  colour = factor(ifelse(sim$x_strong > 0, "red", "blue")),
  size   = factor(sample(c("S", "M", "L"), n, replace = TRUE)),
  class  = sim$class
)
chi <- fs_chi(cat_data, target = "class", seed = 1)
chi$details$results
#>   feature   n df      p_value  adj_p_value significant     method
#> 1  colour 200  1 3.090128e-16 6.180256e-16        TRUE asymptotic
#> 2    size 200  2 6.356383e-01 1.000000e+00       FALSE asymptotic
#>   correction_applied min_expected
#> 1               TRUE        43.24
#> 2              FALSE        25.76
```

Scores are adjusted p-values, so **smaller is stronger**.
[`summary()`](https://rdrr.io/r/base/summary.html) sorts them ascending.
A p-value measures evidence against independence, not effect size: with
enough rows, a negligible association is still significant. Numeric
columns are ignored, so discretize them first if you want them tested.

## `fs_infogain()`: bits of uncertainty removed

Information gain is `H(Y) - H(Y | X)` in bits. Numeric predictors are
discretized into equal-width bins. A numeric target is discretized once,
up front, so that every predictor is scored against the same classes.

``` r

ig <- fs_infogain(sim[, c("x_strong", "x_weak", "x_noise", "class")],
                  target = "class")
ig$scores
#>   x_strong     x_weak    x_noise 
#> 0.41907995 0.11210176 0.05730707
```

Raw information gain favors features with many levels. An ID column can
achieve the maximum possible gain while predicting nothing. Use
`normalize = "gain_ratio"`, which divides by the feature’s own entropy,
when features differ in cardinality:

``` r

with_id <- cbind(id = factor(seq_len(n)), sim[, c("x_strong", "class")])
fs_infogain(with_id, "class")$scores
#>        id  x_strong 
#> 0.9953784 0.4190800
fs_infogain(with_id, "class", normalize = "gain_ratio")$scores
#>        id  x_strong 
#> 0.1302194 0.1285049
```

The gain ratio cuts the ID column’s score by roughly a factor of eight,
but on this data the ID column still ranks first. The gain ratio reduces
the cardinality bias. It does not remove it. Drop identifier and other
near-unique columns before you score anything.

By default `selected` holds every feature with a score above zero. With
finite data almost every feature has *some* positive sample gain, so use
`top_n` for a meaningful cut.
