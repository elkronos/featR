# Reproducibility, randomness, and parallelism

## featR does not touch your random stream

No featR function calls
[`set.seed()`](https://rdrr.io/r/base/Random.html) unless you pass
`seed`. When you do, the seed applies **for that call only**. Your
previous RNG state is restored when the function returns, even when it
errors. Two consequences follow:

- Two calls with the same `seed` give the same answer.
- Calling featR with a seed does not change the random numbers your own
  code draws afterwards.

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

Without a `seed`, randomized methods draw from your global stream like
any other R function, so
[`set.seed()`](https://rdrr.io/r/base/Random.html) before the call also
works.

### Which functions use randomness

- **Train/test splits:**
  [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md),
  [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md),
  [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md),
  [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md).
- **Cross-validation folds:**
  [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md),
  [`fs_elastic()`](https://elkronos.github.io/featR/reference/fs_elastic.md),
  [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md),
  [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md),
  [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md).
- **Model fitting:**
  [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
  and
  [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
  grow random forests.
- **Monte-Carlo p-values:**
  [`fs_chi()`](https://elkronos.github.io/featR/reference/fs_chi.md),
  when expected cell counts are small.
- **Row sampling:** `fs_correlation(sample_frac < 1)`.
- **Subset sampling:** `fs_bayes(sample_combinations = ...)`.
- **Deterministic:**
  [`fs_supervised()`](https://elkronos.github.io/featR/reference/fs_supervised.md),
  [`fs_unsupervised()`](https://elkronos.github.io/featR/reference/fs_unsupervised.md),
  [`fs_infogain()`](https://elkronos.github.io/featR/reference/fs_infogain.md),
  [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md),
  [`fs_pca()`](https://elkronos.github.io/featR/reference/fs_pca.md),
  and [`fs_svd()`](https://elkronos.github.io/featR/reference/fs_svd.md)
  with the exact solver.

[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
is special. Its `seed` controls only which subsets are sampled. To make
the MCMC reproducible, pass `brm_args = list(seed = ...)` as well.

## Parallelism is opt-in and bounded

Every function runs on one core unless you ask for more. The rules are:

- Worker requests are **capped** at
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).
  Asking for 64 on a 4-core laptop gives 4.
- Clusters featR creates are **stopped when the call returns**,
  including on error.
- Parallel backends you registered yourself are restored afterwards.
- With a `seed`, parallel workers get reproducible L’Ecuyer-CMRG
  streams. Two seeded parallel runs therefore agree with each other.
  They need not agree with a seeded *sequential* run, because the
  workers draw from different streams.

``` r

fs_randomforest(big_data, "y", task = "regression", n_cores = 4, seed = 1)
fs_lasso(big_data, "y", parallel = TRUE, n_cores = 4, seed = 1)
```

One caveat: when
[`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md)
grows its forest on more than one worker, the pieces are merged with
[`randomForest::combine()`](https://rdrr.io/pkg/randomForest/man/combine.html).
That drops the out-of-bag error estimates, so `details$oob` is `NULL`.
Use `n_cores = 1` if you need the OOB numbers.

## Optional dependencies

The only hard dependencies are data.table, parallel, stats, utils, and
withr. Every modeling engine is in `Suggests` and is loaded only on the
code path that needs it. A missing package gives an actionable error:

    Error: Packages 'glmnet', 'Matrix' are required for lasso feature selection
    but not installed. Install with: install.packages(c("glmnet", "Matrix"))

To install everything at once:

``` r

install.packages(c(
  "Boruta", "brms", "caret", "doParallel", "e1071", "earth", "foreach",
  "furrr", "future", "ggplot2", "glmnet", "kernlab", "loo", "MASS", "Matrix",
  "MLmetrics", "pbapply", "polycor", "pROC", "PRROC", "randomForest",
  "RSpectra", "bigstatsr"
))
```

[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
also needs a working C++ toolchain for Stan. See the [brms installation
notes](https://github.com/paul-buerkner/brms#how-do-i-install-brms).
