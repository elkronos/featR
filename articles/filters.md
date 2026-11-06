# Filter methods

A filter scores each feature with a statistic and keeps the features
whose score passes a cut. No predictive model is fitted, so filters are
the cheapest methods in featR and the usual first step on wide data.
Each one is **univariate**: it judges every feature on its own, so it
cannot see redundancy between features or effects that only appear in
combination.

featR has five filters, and they split by whether they look at the
outcome:

- [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
  and
  [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
  never touch the outcome. You can run them before a train/test split.
- [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md),
  [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md),
  and
  [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md)
  score features against the outcome. Selecting with them is part of
  fitting the model, so run them on training rows only, or inside every
  fold of a cross-validation.

None of the filters reports a held-out performance number. Their scores
are statistics on whatever rows you pass in. [Validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.md)
shows how much a filter run on all the data inflates a later
cross-validation estimate, and lists which numbers in featR are honest.

The examples use one simulated data set, so the right answers are known
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

## Filters that ignore the outcome

### `fs_unsupervised()`

[`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md)
scores each column by a statistic that needs no outcome: `"variance"`
(the default), `"mad"`, `"iqr"`, `"range"`, `"missing_prop"`, or
`"n_unique"`. Use it to drop constant columns, columns that are mostly
missing, or columns with too few distinct values. Every column of `data`
must be numeric.

With the defaults (`method = "variance"`, `threshold = 0`), it keeps
every column whose variance is above zero, which drops the constant
ones.

``` r

num <- sim[, c("x_strong", "x_weak", "x_noise", "x_const", "x_copy")]
fs_unsupervised(num, method = "variance", threshold = 0)
#> <fs_result> unsupervised_variance
#> Selected 4 of 5 features
#>   x_strong, x_weak, x_noise, x_copy
#> Details: mask, indices, filtered, threshold, direction, action, n_features (in $details)
```

To drop columns that are more than half missing, you have to say
`action = "remove"`:

``` r

fs_unsupervised(sim[, c("x_strong", "x_gappy")], method = "missing_prop",
                threshold = 0.5, action = "remove")$selected
#> [1] "x_strong"
```

With the default `action = "keep"`, the same call would keep the most
missing columns. featR warns when you use `"missing_prop"` with all
three of `threshold`, `direction`, and `action` at their defaults,
because that combination keeps every column with any missing value.

Thresholds are on the scale of the statistic. A variance is in squared
units, `"missing_prop"` is a proportion in \[0, 1\], and `"n_unique"` is
a count, so a threshold chosen for one method means nothing for another.
`"mad"` uses R’s default consistency constant (1.4826), so for normally
distributed data it estimates the standard deviation rather than giving
the raw median absolute deviation.

On missing values: `na_rm = TRUE` (the default) drops them before
computing `"variance"`, `"mad"`, `"iqr"`, and `"range"`.
`"missing_prop"` and `"n_unique"` always count the whole column. A
column whose score comes out `NA` is never selected, and a warning says
how many were excluded.

A high variance says nothing about usefulness. A noisy column can have
the largest variance in the data and a strong predictor the smallest, so
use this function for clean-up, not for ranking.

### `fs_correlation()`

[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
looks only at how the features relate to each other. It flags every pair
whose absolute correlation is above `threshold` (which has no default),
and with `prune = TRUE` (the default) it drops features one at a time
until no two survivors are above it. In `sim`, `x_copy` is `x_strong`
plus a little noise:

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

The pair correlates at 0.998, and one member goes. Each step takes the
strongest remaining pair and drops the member with the larger mean
absolute correlation to the other retained features. That is the
tie-break
[`caret::findCorrelation()`](https://rdrr.io/pkg/caret/man/findCorrelation.html)
uses, but caret scans in a different order, so the two can drop
different features. Neither is guaranteed to drop the fewest.

The choice never consults the outcome, so it can drop the member that
predicts better. Here it dropped `x_copy` and kept `x_strong`, but it
could as easily have gone the other way. If the choice inside a
correlated group should follow importance for a target, use
[`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
instead.

Reading the result:

- `scores` holds each feature’s largest absolute correlation with any
  other feature.
- With `prune = FALSE`, `selected` is the redundant set (both members of
  every flagged pair) and nothing is dropped. `details$redundant` holds
  that set in either mode.

The correlation types are `"pearson"` (the default), `"spearman"`,
`"kendall"`, `"polychoric"` (all columns ordered factors; needs the
polycor package), and `"pointbiserial"` (a continuous column against a
column with exactly two values).

Missing values are not removed by default (`na.rm = FALSE`). An `NA` in
a column makes its correlations `NA`, and pairs with an `NA` correlation
are never flagged or pruned, so redundancy you cannot see is treated as
none. featR warns when this happens. Set `na.rm = TRUE` for pairwise
deletion, or casewise deletion under `"polychoric"`, which stops with an
error on missing values otherwise.

## Filters that score against the outcome

### `fs_supervised()`

[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
scores every feature against `target`, and every feature column must be
numeric. For a numeric target the score is the absolute Pearson
correlation, in \[0, 1\]. For a factor, character, or logical target it
is the one-way ANOVA *F* statistic, which has no upper bound. The
default `method = "auto"` picks between them from the target’s type.

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

`x_copy` scores 0.856, almost the same as `x_strong`, and is kept even
though it adds nothing once `x_strong` is in. Every univariate filter
behaves this way. Run
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md)
afterwards to prune the duplicate.

`x_weak` has a real effect (coefficient 0.5) but scores 0.17 and falls
below the cut. The default `threshold = 0` would have kept all four
features: any non-constant feature has a sample correlation above zero,
even pure noise. Choose a threshold to make an actual selection.

For a factor target, choose the threshold on the *F* scale:

``` r

cls <- sim[, c("x_strong", "x_weak", "x_noise", "class")]
fs_supervised(cls, target = "class", threshold = 10)$scores
#>    x_strong      x_weak     x_noise 
#> 141.5738542   5.4736576   0.2682509
```

`x_strong` scores 141.6, and `x_weak` (5.5) and `x_noise` (0.27) fall
below 10. A feature that separates the classes perfectly scores `Inf`.

Caveats:

- The task in the result is `"regression"` for a numeric target and
  `"classification"` for a factor. A character or logical target is
  scored with ANOVA, but the task is recorded as `NA`.
- Pearson correlation and the *F* test measure a linear relationship or
  a difference in class means. A feature with a strong U-shaped effect
  can score near zero; the
  [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md)
  section below shows one.
- With `na_rm = TRUE` (the default), each feature is scored on the rows
  where both it and the target are observed. A constant feature has no
  defined score; it scores `NA` and is never selected.

### `fs_chi()`

[`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) runs
a chi-squared test of independence between each categorical feature and
the target. Character columns become factors. Numeric, logical, and date
columns are ignored, so discretize them first if you want them tested.
The target is coerced to a factor too, and the task is always
`"classification"`.

``` r

cat_data <- data.frame(
  color = factor(ifelse(sim$x_strong > 0, "red", "blue")),
  size  = factor(sample(c("S", "M", "L"), n, replace = TRUE)),
  class = sim$class
)
chi <- fs_chi(cat_data, target = "class", seed = 1)
chi$selected
#> [1] "color"
chi$details$results
#>   feature   n df      p_value  adj_p_value significant     method
#> 1   color 200  1 3.090128e-16 6.180256e-16        TRUE asymptotic
#> 2    size 200  2 6.356383e-01 1.000000e+00       FALSE asymptotic
#>   correction_applied min_expected
#> 1               TRUE        43.24
#> 2              FALSE        25.76
```

`color` is a coarsened copy of `x_strong`, and its adjusted p-value is
about 6e-16. `size` is random and gets an adjusted p-value of 1.

The score for each feature is its adjusted p-value, so smaller means
stronger evidence of association, and
[`summary()`](https://rdrr.io/r/base/summary.html) sorts the scores
ascending. A feature is selected when its adjusted p-value is below
`sig_level` (default 0.05). The adjustment is Bonferroni by default
(`p_adjust_method`), counting only the features that were actually
tested.

When every expected cell count is at least 5, the test is the usual
asymptotic one, with Yates’ continuity correction on 2x2 tables (the
`correction_applied` column). When any expected count is below 5, the
p-value comes from a Monte Carlo simulation with `simulation_B = 2000`
replicates and `df` is `NA`. Pass `seed` to make those p-values
reproducible. A simulated p-value can never fall below 1 /
(`simulation_B` + 1), so with many features and a Bonferroni correction
it may be unable to reach `sig_level` at all; featR warns and says how
large `simulation_B` would need to be.

Each feature is tested on its own complete cases, so `n` can differ
between rows of the results table. A feature or target left with fewer
than two levels after dropping missing values is skipped, with an `NA`
p-value.

A p-value measures evidence against independence. It is not an effect
size: with enough rows, a negligible association is still significant.

### `fs_infogain()`

[`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md)
scores each feature by its information gain, `H(Y) - H(Y | X)`: how many
bits of uncertainty about the target you lose by knowing the feature. It
accepts numeric, factor, character, logical, and date features in the
same data frame, and it does not assume a linear relationship.

``` r

ig <- fs_infogain(sim[, c("x_strong", "x_weak", "x_noise", "class")],
                  target = "class")
ig$scores
#>   x_strong     x_weak    x_noise 
#> 0.41907995 0.11210176 0.05730707
```

The target is always treated as categorical, and the task is always
`"classification"`. Numeric features are cut into equal-width bins,
using the larger of the Freedman-Diaconis and Sturges bin counts unless
you set `numeric_bins`. A numeric target is binned the same way, once
and up front, so every feature is scored against the same classes. Date
columns are expanded into year, month, and day columns. Missing values
are dropped pair by pair, never across the whole data set.

Because it bins rather than fitting a line, information gain catches the
U-shaped effect that
[`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md)
misses:

``` r

set.seed(2)
u <- data.frame(x_u = rnorm(n), x_lin = rnorm(n))
u$y <- u$x_u^2 + 0.3 * u$x_lin + rnorm(n, sd = 0.3)
fs_supervised(u, target = "y")$scores
#>        x_u      x_lin 
#> 0.04566085 0.36929494
fs_infogain(u, target = "y")$scores
#>       x_u     x_lin 
#> 1.2513951 0.6727366
```

`x_u` drives most of `y`, yet its absolute correlation is 0.05 against
0.37 for `x_lin`. Information gain ranks it first, 1.25 bits against
0.67.

Raw information gain favors features with many levels. An identifier
column splits the data into single rows and reaches the largest gain
possible, while predicting nothing about new data.
`normalize = "gain_ratio"` divides each gain by the feature’s own
entropy, which grows with the number of levels:

``` r

with_id <- cbind(id = factor(seq_len(n)), sim[, c("x_strong", "class")])
fs_infogain(with_id, "class")$scores
#>        id  x_strong 
#> 0.9953784 0.4190800
fs_infogain(with_id, "class", normalize = "gain_ratio")$scores
#>        id  x_strong 
#> 0.1302194 0.1285049
```

The gain ratio cuts the ID column’s score from 1.00 to 0.13, roughly a
factor of eight, but on this data the ID column still ranks first (0.130
against 0.129 for `x_strong`). The gain ratio reduces the bias toward
many levels without removing it, so drop identifier and other
near-unique columns before you score anything. Gain ratios are not on
the bits scale, so a threshold chosen for raw gains does not carry over.

By default `selected` holds every feature with a score above zero. With
finite data, almost every feature has some positive sample gain,
including pure noise, so the default selects nearly everything. Use
`top_n` to keep a fixed number of the highest-scoring features.

`data` can also be a list of data frames that share the target column.
The scores are then the union across the list, and a feature name that
appears in more than one data frame keeps its highest score;
`details$collisions` records which.
