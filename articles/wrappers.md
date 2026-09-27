# Wrapper methods

Wrappers refit a model over many candidate subsets and keep the subset
that model likes best. They are the most expensive methods in featR, and
their answer is specific to the model family they wrap. The search
itself can overfit too, so always validate the chosen set on rows the
search never saw.

## `fs_recursivefeature()`: recursive feature elimination

[`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md)
wraps [`caret::rfe()`](https://rdrr.io/pkg/caret/man/rfe.html). It first
splits the data. On the training rows it repeatedly fits a model, ranks
the predictors, and drops the weakest. Resampling chooses the subset
size. It then evaluates the result on the held-out rows.

``` r

rfe <- fs_recursivefeature(
  iris, "Species",
  sizes = 1:4,
  rfe_control = list(method = "cv", number = 5),
  seed = 1
)
selected(rfe)
#> [1] "Petal.Width"  "Petal.Length"
rfe$details$optimal_size
#> [1] 2
rfe$details$test_metrics         # held out: the honest estimate
#>  Accuracy     Kappa 
#> 0.9666667 0.9500000
rfe$details$resampling_results   # inside the training rows
#>   Variables  Accuracy Kappa AccuracySD    KappaSD
#> 1         1 0.9333333 0.900  0.0372678 0.05590170
#> 2         2 0.9666667 0.950  0.0186339 0.02795085
#> 3         3 0.9500000 0.925  0.0186339 0.02795085
#> 4         4 0.9500000 0.925  0.0186339 0.02795085
```

By default the elimination uses random forests
([`caret::rfFuncs`](https://rdrr.io/pkg/caret/man/caretFuncs.html)).
Pass another caret function set to match your model, for example
`rfe_control = list(method = "cv", number = 5, functions = caret::lmFuncs)`
for linear regression. `handle_categorical = TRUE` one-hot encodes
factors with an encoder fitted on the training rows only.
`return_final_model = TRUE` also trains a caret model (`model_method`)
on the selected features.

Only `details$test_metrics` is a held-out estimate. `resampling_results`
is computed inside the training rows over the candidate sizes, and the
size with the best score is chosen from it, so the winning size’s score
is optimistic.

## `fs_svm()`: SVM-RFE

[`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
trains a support vector machine through caret and kernlab. It can run
**SVM-RFE** (Guyon et al., 2002) first. SVM-RFE fits a linear SVM on
centered and scaled training data, ranks features by their squared
weight `w^2`, drops the weakest, and refits, until one feature is left.
The order in which features were eliminated gives the ranking.

``` r

sv <- fs_svm(
  iris, "Species",
  task = "classification",
  kernel = "linear",
  tune_grid = data.frame(C = c(0.1, 1)),
  feature_select = TRUE,
  select_method = "svm_rfe",
  nfolds = 5,
  seed = 1
)
selected(sv)
#> [1] "Petal.Width"
sv$details$selection$ranking
#> [1] "Petal.Width"  "Petal.Length" "Sepal.Width"  "Sepal.Length"
sv$details$performance$overall[c("Accuracy", "Kappa")]
#>  Accuracy     Kappa 
#> 0.9333333 0.9000000
```

Without `n_features`, the number of features to keep is chosen by
cross-validation over a short ladder of sizes (1, 2, 4, 8, …, p). You
must state `task` explicitly. SVM-RFE needs `kernel = "linear"`, because
the ranking uses the primal weight vector, which exists only for a
linear kernel. For other kernels, use `select_method = "rf_rfe"`, which
screens with random forests instead. Selection always runs on the
training split, and `details$performance` comes from the test split.
Selection works on the **dummy-encoded** predictors, so factor levels
are selected individually.

## `fs_boruta()`: all-relevant selection

Boruta asks a different question: which features carry *any* information
about the outcome? It adds “shadow” copies of every feature with their
values shuffled, grows random forests, and confirms each feature that
beats the best shadow significantly more often than chance. Redundant
but informative features are **all** confirmed. That is useful for
understanding a data set, and less useful when you want a compact model.

``` r

set.seed(3)
bd <- data.frame(
  signal  = rnorm(150),
  noise1  = rnorm(150),
  noise2  = rnorm(150)
)
bd$echo <- bd$signal + rnorm(150, sd = 0.2)
bd$y <- factor(ifelse(bd$signal + rnorm(150, sd = 0.5) > 0, "a", "b"))

bo <- fs_boruta(bd, "y", maxRuns = 50, cutoff_cor = NULL, seed = 1)
bo$details$decisions
#>    signal    noise1    noise2      echo 
#> Confirmed  Rejected Confirmed Confirmed 
#> Levels: Tentative Confirmed Rejected
selected(bo)
#> [1] "signal" "noise2" "echo"

# featR addition: within each correlated group, keep the most important member
selected(fs_boruta(bd, "y", maxRuns = 50, cutoff_cor = 0.7, seed = 1))
#> [1] "signal" "noise2"
```

Look at `noise2`. It was generated independently of `y`, yet it is
confirmed. Two things combine here. In this particular sample `noise2`
happens to carry some random-forest importance: Boruta on its own leaves
it Tentative after 50 runs and confirms it in two of three seeds after
250. Then `resolve_tentative = TRUE` (the default) settles every feature
still undecided with
[`Boruta::TentativeRoughFix()`](https://rdrr.io/pkg/Boruta/man/TentativeRoughFix.html).
That compares median importance with the median of the best shadow,
which is a weaker test than the one used during the run. In a simulation
of 20 data sets like this one, Boruta alone confirmed 5% of the
pure-noise features, and 8.3% after the rough fix.

So Boruta limits false discoveries; it does not eliminate them. When
they are costly, set `resolve_tentative = FALSE`, inspect
`details$decisions`, raise `maxRuns`, and validate the confirmed set on
held-out data.

## `fs_stepwise()`: AIC stepwise regression

[`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md)
runs [`MASS::stepAIC()`](https://rdrr.io/pkg/MASS/man/stepAIC.html) on a
linear regression. It is cheap, deterministic, and returns an ordinary
`lm`.

``` r

st <- fs_stepwise(mtcars, "mpg", direction = "both")
selected(st)
#> [1] "wt"   "qsec" "am"
st$scores
#>       wt     qsec       am 
#> 5.506882 4.246676 2.080819
```

Pass `k = log(nrow(data))` to use BIC, which selects fewer terms.
Stepwise search is greedy and unstable: small changes to the data can
change the answer. The *t* statistics in `scores` and the p-values in
`details$coefficients` are computed after selection on the same data, so
they are biased toward significance. **Do not use them for inference.**

## `fs_bayes()`: Bayesian model comparison

[`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)
fits a brms model for every candidate predictor subset and compares them
by approximate leave-one-out cross-validation, using
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html).
By default it applies a one-standard-error rule: the chosen subset is
the smallest one whose expected log predictive density is within one
standard error of the best.

``` r

res <- fs_bayes(
  mtcars, "mpg",
  predictors = c("wt", "hp", "qsec"),
  brm_args = list(chains = 2, iter = 1000, seed = 1),
  seed = 1
)
selected(res)
res$details$loo_comparison
```

This is the most expensive method in featR. With `p` candidates it fits
`2^p - 1` models, and each one compiles and samples a Stan program.
Bound the search with `max_comb_size` or `sample_combinations`. It needs
brms, loo, and a working C++ toolchain, which is why this example is not
run here. `details$mae` and `details$rmse` are in-sample, and the
posterior of the returned model is post-selection, so neither is an
honest estimate on its own.

## References

Guyon, I., Weston, J., Barnhill, S. and Vapnik, V. (2002). Gene
selection for cancer classification using support vector machines.
*Machine Learning* 46, 389–422.
<https://doi.org/10.1023/A:1012487302797>

Kursa, M. B. and Rudnicki, W. R. (2010). Feature selection with the
Boruta package. *Journal of Statistical Software* 36(11).
<https://doi.org/10.18637/jss.v036.i11>

Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing* 27, 1413–1432.
<https://doi.org/10.1007/s11222-016-9696-4>
