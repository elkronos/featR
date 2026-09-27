# featR

A unified R package of feature-selection methods: statistical filters,
regularization, model-based wrappers, and dimensionality reduction — all
behind **one calling convention** and **one return type**.

Heavy modeling engines (brms, caret, glmnet, randomForest, …) are
optional **Suggests**; each function checks for what it needs and tells
you what to install.

**Documentation:** <https://elkronos.github.io/featR/>. It has the
function reference, a getting-started guide, method guides for every
family, and articles on validation and reproducibility.

## Installation

``` r

# install.packages("devtools")
devtools::install_github("elkronos/featR")
```

## The convention

Every selection function takes the data first and the **name** of the
target column second, and returns the same object:

``` r
fs_<method>(data, target, <method options>, ...)
```

Housekeeping arguments come last. Every function takes `verbose`.
Functions that use randomness take `seed = NULL`, and functions that can
run in parallel take `n_cores` and/or `parallel`. The [method
guide](https://elkronos.github.io/featR/articles/choosing-a-method.html)
has a table showing exactly which function accepts which. Functions with
no outcome,
[`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md),
[`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md),
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md), and
[`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md), take
no `target`.

``` r

library(featR)

res <- fs_lasso(mtcars, "mpg", nfolds = 5, seed = 1)

res               # <fs_result> lasso (regression), with the selected features
selected(res)     # character vector of chosen features
res$scores        # per-feature scores, comparable within a method
res$model         # the fitted model (fs_lasso keeps it only with return_model = TRUE)
res$details       # everything method-specific
summary(res)      # ranked score table, selected features marked
```

Because the shape is shared, methods are interchangeable:

``` r

runs <- list(
  lasso    = fs_lasso(mtcars, "mpg", nfolds = 5, seed = 1),
  stepwise = fs_stepwise(mtcars, "mpg"),
  rfe      = fs_recursivefeature(mtcars, "mpg", sizes = c(2, 4), seed = 1)
)
lapply(runs, selected)
```

Functions are **sequential by default** (parallelism is opt-in and
capped at the detected core count), **never seed the RNG** unless you
pass `seed` (and then restore your RNG state on exit), and signal
progress with suppressible
[`message()`](https://rdrr.io/r/base/message.html)s.

## Functions

### Filters

| Function | What it does |
|----|----|
| [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) | Chi-square tests of association between categorical features and a categorical target, with p-value adjustment and Monte-Carlo fallback for sparse tables. |
| [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) | Flags variable pairs above a correlation threshold. `prune = TRUE` (default) returns the reduced non-redundant set; `prune = FALSE` returns every flagged pair member. |
| [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md) | Information gain w.r.t. a target, for numeric (binned), categorical, and date features. `normalize = "gain_ratio"` corrects the bias toward many-leveled predictors. |
| [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md) | Threshold filter: absolute correlation (numeric target) or ANOVA F (factor target). |
| [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md) | Target-free threshold filter: variance, MAD, IQR, range, missing proportion, distinct-value count. |

### Regularization

| Function | What it does |
|----|----|
| [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md) | Cross-validated LASSO (glmnet). Scores are standardized coefficients; raw-scale ones are in `details`. |
| [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md) | Elastic net over an alpha grid via caret + glmnet, with optional PCA fit **inside** each resample. |

### Wrappers and model-based

| Function | What it does |
|----|----|
| [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) | Bayesian model comparison over predictor subsets (brms), ranked by LOO with a 1-SE parsimony rule. |
| [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md) | Boruta all-relevant selection, with importance-aware correlation pruning. |
| [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md) | Random-forest permutation importance with held-out evaluation. |
| [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md) | caret RFE on a training split, honestly evaluated on held-out rows. |
| [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md) | Stepwise linear regression via [`MASS::stepAIC`](https://rdrr.io/pkg/MASS/man/stepAIC.html). |
| [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) | SVM pipeline with true **SVM-RFE** (linear-kernel weight ranking, refit per elimination step); random-forest screening available via `select_method = "rf_rfe"`. |
| [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) | MARS (earth) with tuning and optional ROC/PR AUC. |

### Dimensionality reduction

These reduce dimensions rather than select features, so they return
their own decomposition structure rather than an `fs_result`.

| Function | What it does |
|----|----|
| [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md) | PCA (prcomp, or bigstatsr for large data) with loadings, scores, variance explained, optional plot. |
| [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) | Exact or approximate (RSpectra) truncated SVD. Pass `n_singular_values < min(dim(x))` to enable the approximate solver. |

## Notes on honesty

Several methods can silently flatter themselves. featR tries not to:

- Class upsampling happens **inside** resampling folds
  ([`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md),
  [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)),
  never before.
- [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
  evaluates on rows the selection never saw, and its final model trains
  on training rows only.
- [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
  fits PCA per fold;
  [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  refuses to mean-impute by default (`impute = "none"`) because
  full-data imputation leaks across folds.
- [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)’s
  `feature_select` hook runs on the training split only.
- Post-selection statistics
  ([`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md)
  p-values) are labeled as invalid for inference rather than presented
  as if they were not.

## License

MIT (c) Justin Chase. See `LICENSE.md`.
