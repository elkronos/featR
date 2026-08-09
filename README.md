# featR

A unified R package of feature-selection methods: statistical filters, regularization, model-based wrappers, and dimensionality reduction behind a consistent set of `fs_*()` functions.

Heavy modeling engines (brms, caret, glmnet, randomForest, ...) are optional **Suggests** — each function checks for what it needs and tells you what to install.

## Installation

```r
# From a local checkout:
# install.packages("devtools")
devtools::install()

# Or build/check from the shell:
# R CMD build . && R CMD check --as-cran featR_0.1.0.tar.gz
```

## Functions

### 1. Statistical & Filter Methods

| Function | What it does | Notes |
|---|---|---|
| `fs_chi` | Chi-square tests of association between categorical features and a categorical target, with p-value adjustment, Yates/Monte-Carlo handling. | Returns a results table plus `significant_features`. |
| `fs_correlation` | Computes a correlation matrix (Pearson/Spearman/Kendall/point-biserial/polychoric) and flags variable pairs above a threshold. | Returns **both** members of each high-correlation pair (the redundant set) — prune accordingly. |
| `fs_infogain` | Information gain of each feature w.r.t. a target; supports numeric (binned), categorical, and date features. | The target is discretized once so scores are comparable across features. |
| `fs_supervised` | Threshold filter scoring features against a target: absolute Pearson correlation (numeric target) or ANOVA F (factor target). | Flexible output shapes (matrix, data.table, mask, indices, names, list). |
| `fs_unsupervised` | Target-free threshold filter: variance, MAD, IQR, range, missing proportion, or distinct-value count. | Variance filtering lives here (`method = "variance"`). |

### 2. Regularization Methods

| Function | What it does | Notes |
|---|---|---|
| `fs_elastic` | Elastic-net over an alpha/lambda grid via caret + glmnet, optional PCA preprocessing. | Reports best alpha/lambda and coefficients. |
| `fs_lasso` | Cross-validated LASSO (glmnet) returning coefficient-based importance at `lambda.min`. | Importance is on the original predictor scale — see docs. |

### 3. Model-Based & Wrapper Methods

| Function | What it does | Notes |
|---|---|---|
| `fs_bayes` | Bayesian model comparison over predictor combinations (brms), ranked by LOO. | Expensive; MAE/RMSE reported are in-sample. |
| `fs_boruta` | Boruta all-relevant selection with optional correlation-based pruning. | |
| `fs_randomforest` | Random-forest train/evaluate pipeline with permutation importance and preprocessing options. | |
| `fs_recursivefeature` | caret RFE on a training split, evaluated on a held-out test split; optional final model on training data. | |
| `fs_stepwise` | Stepwise linear regression via `MASS::stepAIC` (forward/backward/both). | Post-selection p-values are not valid for inference — see docs. |
| `fs_svm` | SVM (caret) train/evaluate pipeline with optional feature selection and class-imbalance handling. | Feature selection uses random-forest RFE, not SVM-RFE. Classification returns a confusion matrix; regression returns RMSE/R2/MAE. |
| `fs_mars` | MARS (earth) train/evaluate pipeline with tuning and optional ROC/PR AUC. | |

### 4. Dimensionality Reduction

| Function | What it does | Notes |
|---|---|---|
| `fs_pca` | PCA (prcomp, or bigstatsr for large data) with loadings, scores, variance explained, optional plot. | |
| `fs_svd` | Exact or approximate (RSpectra) truncated SVD. | Request `n_singular_values < min(dim(x))` to enable the approximate solver. |

## Conventions

All functions validate inputs up front, are sequential by default (parallelism is opt-in and capped), never seed the RNG unless you pass `seed = ...` (and then restore its state), and signal progress with suppressible `message()`s.

## License

MIT (c) Justin Chase. See `LICENSE.md`.
