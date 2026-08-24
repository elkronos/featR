# featR

A unified R package of feature-selection methods: statistical filters, regularization, model-based wrappers, and dimensionality reduction — all behind **one calling convention** and **one return type**.

Heavy modeling engines (brms, caret, glmnet, randomForest, ...) are optional **Suggests**; each function checks for what it needs and tells you what to install.

## Installation

```r
# install.packages("devtools")
devtools::install_github("elkronos/featR")
```

## The convention

Every selection function takes the data first and the target column second, and returns the same object:

```r
fs_<method>(data, target, ..., seed = NULL, verbose = FALSE, n_cores = 1L)
```

```r
library(featR)

res <- fs_lasso(mtcars, "mpg", nfolds = 5, seed = 1)

res
#> <fs_result> lasso (regression)
#> Selected 5 of 10 features
#>   wt, cyl, hp, ...

selected(res)     # character vector of chosen features
res$scores        # per-feature scores, comparable within a method
res$model         # the fitted model, when the method produced one
res$details       # everything method-specific
summary(res)      # ranked score table, selected features marked
```

Because the shape is shared, methods are interchangeable:

```r
methods <- list(lasso = fs_lasso, boruta = fs_boruta, rfe = fs_recursivefeature)
lapply(methods, function(f) selected(f(mtcars, "mpg", seed = 1)))
```

Functions are **sequential by default** (parallelism is opt-in and capped), **never seed the RNG** unless you pass `seed`, and signal progress with suppressible `message()`s.

## Functions

### Filters

| Function | What it does |
|---|---|
| `fs_chi()` | Chi-square tests of association between categorical features and a categorical target, with p-value adjustment and Monte-Carlo fallback for sparse tables. |
| `fs_correlation()` | Flags variable pairs above a correlation threshold. `prune = TRUE` (default) returns the reduced non-redundant set; `prune = FALSE` returns every flagged pair member. |
| `fs_infogain()` | Information gain w.r.t. a target, for numeric (binned), categorical, and date features. `normalize = "gain_ratio"` corrects the bias toward many-leveled predictors. |
| `fs_supervised()` | Threshold filter: absolute correlation (numeric target) or ANOVA F (factor target). |
| `fs_unsupervised()` | Target-free threshold filter: variance, MAD, IQR, range, missing proportion, distinct-value count. |

### Regularization

| Function | What it does |
|---|---|
| `fs_lasso()` | Cross-validated LASSO (glmnet). Scores are standardized coefficients; raw-scale ones are in `details`. |
| `fs_elastic()` | Elastic net over an alpha grid via caret + glmnet, with optional PCA fit **inside** each resample. |

### Wrappers and model-based

| Function | What it does |
|---|---|
| `fs_bayes()` | Bayesian model comparison over predictor subsets (brms), ranked by LOO with a 1-SE parsimony rule. |
| `fs_boruta()` | Boruta all-relevant selection, with importance-aware correlation pruning. |
| `fs_randomforest()` | Random-forest permutation importance with held-out evaluation. |
| `fs_recursivefeature()` | caret RFE on a training split, honestly evaluated on held-out rows. |
| `fs_stepwise()` | Stepwise linear regression via `MASS::stepAIC`. |
| `fs_svm()` | SVM pipeline with true **SVM-RFE** (linear-kernel weight ranking, refit per elimination step); random-forest screening available via `select_method = "rf_rfe"`. |
| `fs_mars()` | MARS (earth) with tuning and optional ROC/PR AUC. |

### Dimensionality reduction

These reduce dimensions rather than select features, so they return their own decomposition structure rather than an `fs_result`.

| Function | What it does |
|---|---|
| `fs_pca()` | PCA (prcomp, or bigstatsr for large data) with loadings, scores, variance explained, optional plot. |
| `fs_svd()` | Exact or approximate (RSpectra) truncated SVD. Pass `n_singular_values < min(dim(x))` to enable the approximate solver. |

## Notes on honesty

Several methods can silently flatter themselves. featR tries not to:

- Class upsampling happens **inside** resampling folds (`fs_mars()`, `fs_svm()`), never before.
- `fs_recursivefeature()` evaluates on rows the selection never saw, and its final model trains on training rows only.
- `fs_elastic()` fits PCA per fold; `fs_lasso()` refuses to mean-impute by default (`impute = "none"`) because full-data imputation leaks across folds.
- `fs_randomforest()`'s `feature_select` hook runs on the training split only.
- Post-selection statistics (`fs_stepwise()` p-values) are labeled as invalid for inference rather than presented as if they were not.

## License

MIT (c) Justin Chase. See `LICENSE.md`.
