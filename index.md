# featR

[![CRAN
status](https://www.r-pkg.org/badges/version/featR)](https://CRAN.R-project.org/package=featR)

featR puts 14 feature-selection methods for R behind one calling
convention and one return type: statistical filters, regularized and
tree-based models, and wrappers, plus two dimensionality-reduction
helpers. Every supervised selection function takes `(data, target, ...)`
and returns an `fs_result`, so comparing the lasso with Boruta or
recursive feature elimination is a one-word change. The modeling engines
(glmnet, caret, Boruta, randomForest, brms, and others) are optional;
each function checks for the one it needs and tells you what to install.

Documentation: <https://elkronos.github.io/featR/>

## Installation

Install the released version from CRAN:

``` r

install.packages("featR")
```

Or the development version from GitHub:

``` r

# install.packages("remotes")
remotes::install_github("elkronos/featR")
```

## Example

Keep the `mtcars` columns correlated with `mpg` above 0.5, then drop the
redundant ones:

``` r

library(featR)

relevant <- fs_supervised(mtcars, target = "mpg", threshold = 0.5)
pruned   <- fs_correlation(mtcars[, selected(relevant)], threshold = 0.8)
selected(pruned)
#> [1] "hp"   "drat" "wt"   "vs"   "am"   "carb"

summary(pruned)   # ranked scores, kept features marked

# Same question, different method (needs glmnet)
selected(fs_lasso(mtcars, target = "mpg", seed = 1))
#> [1] "wt"   "cyl"  "hp"   "am"   "carb" "drat"
```

The [Getting started
guide](https://elkronos.github.io/featR/articles/featR.html) walks
through this example and the calling convention.

## Functions

| Family | Function | What it does |
|----|----|----|
| [Filters](https://elkronos.github.io/featR/articles/filters.html) | [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.html) | Target-free threshold filter: variance, MAD, IQR, range, missing proportion, distinct-value count |
|  | [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.html) | Absolute correlation (numeric target) or ANOVA F (factor target) against a threshold |
|  | [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.html) | Drops features until no pair is correlated above a threshold |
|  | [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.html) | Chi-squared tests between categorical features and a categorical target |
|  | [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.html) | Information gain, optionally as gain ratio |
| [Regularization and embedded](https://elkronos.github.io/featR/articles/regularization.html) | [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.html) | Cross-validated lasso (glmnet) |
|  | [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.html) | Elastic net tuned over an alpha grid (caret, glmnet) |
|  | [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.html) | Random-forest permutation importance with held-out evaluation |
|  | [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.html) | Multivariate adaptive regression splines (caret, earth) |
| [Wrappers](https://elkronos.github.io/featR/articles/wrappers.html) | [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.html) | Recursive feature elimination (caret), evaluated on held-out rows |
|  | [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.html) | SVM with SVM-RFE or random-forest RFE (caret, kernlab) |
|  | [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.html) | Boruta all-relevant selection |
|  | [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.html) | Stepwise linear regression by AIC (MASS) |
|  | [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.html) | Bayesian comparison of predictor subsets by LOO (brms, loo) |
| [Dimensionality reduction](https://elkronos.github.io/featR/articles/dimensionality-reduction.html) | [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.html) | Principal components; returns its own list, not an `fs_result` |
|  | [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.html) | Exact SVD, or an approximate truncated one (RSpectra); returns its own list, not an `fs_result` |

Not sure which to use? See [Choosing a
method](https://elkronos.github.io/featR/articles/choosing-a-method.html).

## Validation

Any selection that looks at the outcome is part of model fitting, so
error estimates from the rows that chose the features are optimistic.
[Validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.html)
lists which numbers featR reports are held-out estimates and which are
not, and shows a pipeline that keeps them honest.

## License

MIT (c) Justin Chase. See `LICENSE.md`.
