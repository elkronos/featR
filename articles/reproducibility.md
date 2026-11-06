# Reproducibility and parallelism

featR leaves your random number generator alone unless you pass `seed`,
runs on one core unless you ask for more, and loads its modeling engines
only when a function needs them.

## Seeds

No featR function calls
[`set.seed()`](https://rdrr.io/r/base/Random.html) unless you pass
`seed`. When you do, the seed applies to that call only, and your
previous RNG state is restored when the function returns, even if it
errors. So two calls with the same `seed` give the same answer, and a
seeded featR call does not change the random numbers your own code draws
afterwards:

``` r

d <- data.frame(a = sin(1:50), b = cos(1:50), c = sin(1:50) + 0.1 * cos(3:52))

set.seed(2024)
expected <- runif(3)

set.seed(2024)
res <- fs_correlation(d, threshold = 0.9, sample_frac = 0.5, seed = 1)
observed <- runif(3)

# The seeded call left the caller's stream exactly where it was
identical(expected, observed)
#> [1] TRUE
```

Without `seed`, a randomized function draws from your global stream like
any other R function, so calling
[`set.seed()`](https://rdrr.io/r/base/Random.html) first also makes a
sequential run repeatable. Parallel workers do not draw from your global
stream, so a parallel run needs `seed`. For
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
with more than one worker, `seed` is the only way to get the same forest
twice.

## Which functions use randomness

| Source of randomness | Functions |
|----|----|
| Train/test split | [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md), [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md), [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md), [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) |
| Cross-validation folds | [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md), [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md), [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md), [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md), [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) |
| Random forests | [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md), [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md) |
| Monte Carlo p-values, when expected cell counts are small | [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) |
| Row down-sampling | [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) with `sample_frac < 1`, [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md) above `sample_size` rows (default 10000), [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md) with `control$sample_size` |
| Subset sampling and MCMC | [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) |
| None | [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md), [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md), [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md), [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md), [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md), [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md) |

[`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md),
[`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md), and
the other deterministic functions have no `seed` argument because they
never touch the RNG.

[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
fits each candidate model by MCMC. Its `seed` controls which subsets
`sample_combinations` draws and, unless you pass
`brm_args = list(seed = ...)`, also draws one Stan seed per candidate
model. A seeded call therefore reproduces the MCMC fits too, including
under `parallel_combinations = TRUE`. An explicit `brm_args$seed` is
used for every model instead.

## Parallelism

Every function runs on one core unless you ask for more:

| Function | How to ask | Workers when you ask |
|----|----|----|
| [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md) | `parallel = TRUE` | `n_cores`, default 2 |
| [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) | `parallel = TRUE` | `n_cores`, default 2, or the future plan you already set |
| [`fs_correlation()`](https://elkronos.github.io/featR/reference/fs_correlation.md) | `parallel = TRUE`, `method = "pointbiserial"` only | `n_cores`, default 2 |
| [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md) | `parallel = TRUE` | 2 |
| [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md), [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md), [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md), [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) | `n_cores` greater than 1 | `n_cores` |
| [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md) | `parallel_combinations = TRUE` | `n_cores`; not available on Windows |

Worker requests are capped at
[`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html),
so asking for 64 workers on a 4-core laptop gives 4. Clusters featR
creates are stopped when the call returns, including on error, and a
foreach backend or future plan you registered yourself is restored
afterwards. If you have already set a non-sequential future plan,
[`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md) uses
that plan as is, ignores `n_cores`, and leaves your workers running.

### Seeded parallel runs

For most functions, two seeded parallel runs agree with each other
because everything random is decided in the main R process from the
local seed: the train/test split, the resampling folds, and, for the
caret-based
[`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md),
[`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md),
and
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md),
one seed per resample that caret draws and hands to whichever worker
fits that resample. Seeding the workers’ own RNG streams would not be
enough on its own. foreach gives each resample to whichever worker is
free, so which stream fits which resample changes from run to run.
[`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
and
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
need nothing more, because glmnet fits are deterministic once the folds
are fixed.

[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
works differently. It hands each worker a fixed block of trees and seeds
the workers with L’Ecuyer-CMRG streams.

A seeded parallel run need not match a seeded sequential one.
[`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md)
guarantees that its two runs agree.
[`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md) and
[`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
make no such promise, and
[`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md)’s
parallel p-values come from furrr’s own streams, so they differ from the
sequential ones for the same `seed`.

``` r

fs_randomforest(big_data, "y", task = "regression", n_cores = 4, seed = 1)
fs_lasso(big_data, "y", parallel = TRUE, n_cores = 4, seed = 1)
```

When
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
grows its forest on more than one worker, it merges the pieces with
[`randomForest::combine()`](https://rdrr.io/pkg/randomForest/man/combine.html),
which drops the out-of-bag error estimates, so `details$oob` is `NULL`.
Use `n_cores = 1` if you need the OOB numbers.
[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
with `parallel_combinations = TRUE` holds each model to a single MCMC
chain, so cross-chain diagnostics such as R-hat are not available; it
warns when that happens.

## Optional dependencies

featR’s hard dependencies are data.table, methods, parallel, stats,
utils, and withr. Every modeling engine is in `Suggests` and is loaded
only on the code path that needs it. A missing package stops the call
with the command to fix it:

    Error: Package 'glmnet' is required for lasso feature selection but not
    installed. Install with: install.packages(c("glmnet"))

The [choosing a
method](https://elkronos.github.io/featR/articles/choosing-a-method.md)
guide lists which packages each function needs. To install all of them
at once:

``` r

install.packages(c(
  "Boruta", "brms", "caret", "doParallel", "e1071", "earth", "foreach",
  "furrr", "future", "ggplot2", "glmnet", "kernlab", "loo", "MASS", "Matrix",
  "MLmetrics", "pbapply", "polycor", "pROC", "PRROC", "randomForest",
  "RSpectra", "bigstatsr"
))
```

[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
also needs a working C++ toolchain for Stan; the [brms installation
notes](https://github.com/paul-buerkner/brms#how-do-i-install-brms)
explain how to set one up.
