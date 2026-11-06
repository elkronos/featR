# Random forest importance with a held-out evaluation

Fits a random forest on a stratified training split, scores each
predictor by permutation importance, and evaluates the forest on the
held-out rows. Unless you supply `control$feature_select`, `selected`
holds every predictor the forest used, ordered by importance, and you
choose where to cut the list.

## Usage

``` r
fs_randomforest(
  data,
  target,
  task = c("classification", "regression"),
  control = list(),
  seed = NULL,
  verbose = FALSE,
  n_cores = 1L
)
```

## Arguments

- data:

  A data.frame or data.table with at least one row and one column,
  holding the target and the candidate predictors (every other column).
  Column names must be unique and non-empty, and predictors may not
  contain infinite values. `Date` columns are converted to numeric
  before modeling.

- target:

  Name of the target column of `data`, as a single string (a column
  index is not accepted). For classification the target is coerced with
  [`as.factor()`](https://rdrr.io/r/base/factor.html), an ordered factor
  is treated as unordered, and at least two levels must remain. For
  regression it is coerced with
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html), a factor
  through its labels (`as.numeric(as.character(y))`) and never its
  internal level codes, and an infinite value is an error. Either way,
  rows whose target is `NA` after coercion are dropped with a warning.

- task:

  `"classification"` (the default) or `"regression"`, matched with
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html). The task is
  not inferred from the target. If you leave `task` at its default and
  the target is numeric, a warning says that every distinct value will
  become a class.

- control:

  List of method-specific options. Every accepted entry and its default
  is listed under "The control list" in Details. Default
  [`list()`](https://rdrr.io/r/base/list.html), meaning all defaults.

- seed:

  Optional single whole number within the range of an R integer;
  fractional or out-of-range values are an error. The seed applies for
  the duration of the call and the previous RNG state is restored on
  exit. With more than one worker it is also passed to
  [`parallel::clusterSetRNGStream()`](https://rdrr.io/r/parallel/RngStream.html),
  and it is then the only way to make the run reproducible. Default
  `NULL`: the RNG is never seeded unless you ask.

- verbose:

  Logical. If `TRUE`, print the split sizes, how many predictors the
  hook kept, the tree and worker counts, and a note when AUC is skipped
  because 'pROC' is missing. Default `FALSE`.

- n_cores:

  Number of workers used to grow the forest, a whole number \>= 1.
  Default `1L` (sequential; no cluster is created). Capped at the
  detected core count and at `control$ntree`. An effective value above 1
  needs the suggested packages 'foreach' and 'doParallel', and leaves
  `details$oob` empty (see Details).

## Value

An object of class `fs_result` with:

- selected:

  Character vector. With `control$feature_select`, the predictors the
  hook kept; otherwise every predictor that reached the forest. Ordered
  by decreasing importance when importance was computed.

- scores:

  Named numeric vector of permutation importance from
  `randomForest::importance(type = 1)`: mean decrease in accuracy for
  classification, mean increase in MSE for regression (randomForest
  names that column with a percent sign, but it is not a percentage).
  Scaled or not according to `control$scale_importance`. `NULL` when
  `control$importance = FALSE`.

- method:

  `"randomforest"`.

- task:

  `"classification"` or `"regression"`, echoing the `task` argument.

- model:

  The fitted `randomForest` object, or the result of
  [`randomForest::combine()`](https://rdrr.io/pkg/randomForest/man/combine.html)
  when more than one worker was used.

- details:

  A list with:

  - `metrics`: held-out metrics, a named list with `accuracy`, `kappa`,
    and `auc` for classification, or `RMSE`, `MAE`, and `R2` for
    regression.

  - `predictions`: test-set predictions.

  - `probabilities`: test-set class probability matrix (classification
    only; `NULL` if [`predict()`](https://rdrr.io/r/stats/predict.html)
    cannot produce one).

  - `importance`: data.frame with columns `feature` and `importance`,
    sorted descending; `NULL` when `control$importance = FALSE`.

  - `confusion`: Observed x Predicted table (classification only).

  - `oob`: out-of-bag `accuracy` or `RMSE`; `NULL` when trained in
    parallel or switched off.

  - `feature_names`: the predictors the forest used.

  - `train_index`: integer training rows of the cleaned and optionally
    downsampled data.

  - `test_data`: the held-out rows when
    `control$return_test_data = TRUE`, `NULL` otherwise.

  - `control`: the merged control list.

  - `n_features`: candidate predictors after cleaning and before
    selection.

- call:

  The matched call.

## Details

A random forest averages many decision trees, each grown on a bootstrap
sample of the training rows with a random subset of predictors tried at
each split. Forests take any mix of numeric, factor, character, and
`Date` predictors and need no scaling, so `fs_randomforest()` is a
reasonable default when the feature types are messy. Permutation
importance is the drop in out-of-bag accuracy (or rise in out-of-bag
MSE) when one predictor's values are shuffled. It tends to favor
predictors with many distinct values and to split credit between
correlated predictors, so treat close scores as ties.

The honest performance estimate is `details$metrics`, computed on rows
the forest and the selection hook never saw (`control$preprocess` does
see them; see "What sees the held-out rows"). The [Validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.html)
article lists which numbers in each featR result are held out and which
are optimistic.

### Order of operations

1.  `control$preprocess` runs on the full table.

2.  `Date` columns become numeric, and the target is coerced and cleaned
    (see `target`).

3.  If `control$sample_size` is set, the table is downsampled.

4.  The rows are split into training and test sets, stratified on the
    target, with `control$train_ratio` of them used for training.

5.  `control$feature_select` runs on the training rows.

6.  Character predictors become factors, unused factor levels are
    dropped, and the test factors are re-leveled to the training levels.

7.  Near-zero-variance predictors are removed, judged on the training
    rows.

8.  Missing predictor values are imputed with values learned on the
    training rows.

9.  The forest is grown, on `n_cores` workers.

10. The forest predicts the test rows, and permutation importance is
    read from it.

Because near-zero-variance removal comes after the hook,
`details$feature_names` (the predictors the forest used) can be a subset
of `selected`.

### The control list

Every accepted entry, with its default:

- `train_ratio = 0.75`: training proportion of the stratified split,
  strictly between 0 and 1.

- `sample_size = NULL`: optional whole number. When set, the data is
  downsampled to about this many rows *before* the split, proportionally
  within each class for classification and uniformly at random for
  regression. Values above the available row count are capped. `NULL`
  keeps every row.

- `ntree = 500`: number of trees, a whole number \>= 1.

- `importance = TRUE`: compute permutation importance. When `FALSE`,
  `scores` and `details$importance` are `NULL` and `selected` keeps
  column order.

- `scale_importance = TRUE`: passed as `scale` to
  [`randomForest::importance()`](https://rdrr.io/pkg/randomForest/man/importance.html),
  which divides each permutation importance by its standard error over
  trees. Set `FALSE` for the raw importance. With more than one worker
  the standard error is pooled across the sub-forests, so scaled scores
  do not depend on `n_cores`.

- `mtry = NULL`: predictors tried at each split. `NULL` means
  `floor(sqrt(p))` for classification and `floor(p / 3)` for regression.
  Any value, supplied or derived, is clamped to `[1, p]`, where `p`
  counts the predictors left after the hook and near-zero-variance
  removal.

- `nodesize = NULL`: minimum size of terminal nodes, a whole number
  \>= 1. `NULL` keeps the `randomForest` default (1 for classification,
  5 for regression).

- `maxnodes = NULL`: maximum number of terminal nodes per tree, a whole
  number \>= 2, or `NULL` for no limit.

- `sampsize = NULL`: rows drawn to grow each tree, passed to
  `randomForest`. Regression takes a single number (a vector is an
  error); classification also accepts one number per class. Entries must
  be whole numbers \>= 1. A single value is clamped to the training row
  count. A per-class (or, with `strata`, per-stratum) vector has each
  entry clamped to that class's (stratum's) count in the training rows,
  with a warning, because `randomForest` refuses larger entries and you
  cannot know the training split in advance. An `NA` entry becomes the
  full training size when `replace = TRUE` and `ceiling(0.632 * n)`
  otherwise, and is then clamped the same way.

- `classwt = NULL`: class priors for classification, passed to
  `randomForest` unchanged and unvalidated.

- `strata = NULL`: name of a column of `data` to stratify the per-tree
  sampling on (used with `sampsize`). Its values are read from the
  training rows so they line up with the rows the forest is grown on.
  Passing the values as a vector is an error, because the training rows
  are chosen inside the function. The column must not contain `NA`, and
  it stays in the model as a predictor unless you remove it.

- `replace = TRUE`: sample rows with replacement when growing each tree.

- `preprocess = NULL`: a function `dt -> dt` applied to the full data
  before the split. It must return a data.frame or data.table that still
  contains the target. See "What sees the held-out rows" below.

- `feature_select = NULL`: a function `dt -> dt` that runs on the
  training rows only. It must keep the target and may only subset and
  reorder existing columns, because the same columns are then taken from
  the test rows. Put feature engineering in `preprocess`.

- `impute = TRUE`: replace missing predictor values with the median
  (numeric) or the most frequent value (factor, character, logical),
  learned on the training rows and applied to both splits.

- `drop_zerovar = TRUE`: remove near-zero-variance predictors with
  [`caret::nearZeroVar()`](https://rdrr.io/pkg/caret/man/nearZeroVar.html),
  using the training rows only. With `FALSE`, training rows in which
  every predictor is constant are an error, since the trees have nothing
  to split on.

- `oob = TRUE`: report out-of-bag metrics in `details$oob` (`accuracy`
  for classification, `RMSE` for regression). They are optimistic after
  a supervised hook and unavailable after parallel training; see below.

- `return_test_data = FALSE`: when `TRUE`, the held-out rows are
  returned in `details$test_data`.

- `positive_class = NULL`: level treated as the positive class for
  binary AUC. `NULL` means the second factor level. A string that does
  not name a level of the cleaned target is an error.

Unknown entries are an error, so typos and arguments that moved out of
`control` (`seed`, `n_cores`, and `split_ratio`, now `train_ratio`) fail
loudly.

### What sees the held-out rows

`control$feature_select` runs after the split on the training rows only,
so a hook that looks at the target does not leak the test rows into
`details$metrics`.

The out-of-bag metrics in `details$oob` do not get that protection. The
forest is grown on the same training rows the hook selected on, so an
out-of-bag row was left out of some trees but was still used for the
selection. With a hook that consults the target, `details$oob` is
optimistic, and on pure noise it can read well above chance.

`control$preprocess` runs on the full data by design. Any quantity it
learns from other rows, such as an imputation value, a scaling constant,
or anything computed from the target, includes the test rows and will
flatter `details$metrics`. Keep it to row-wise transformations.
Downsampling with `control$sample_size` also happens before the split,
which is why `details$train_index` indexes the cleaned and downsampled
table rather than the rows of `data`.

When more than one worker is used (`n_cores` after the caps), the
sub-forests are merged with
[`randomForest::combine()`](https://rdrr.io/pkg/randomForest/man/combine.html),
which drops the out-of-bag error structures (`err.rate`, `mse`,
`confusion`). `details$oob` is then `NULL`, with a warning if
`control$oob = TRUE`. `details$metrics` is unaffected; use `n_cores = 1`
to get the out-of-bag numbers.

### Factor levels and missing values

Factor predictors carry only the levels seen in the training rows. A
test value unseen in training becomes `NA` and is imputed when
`control$impute = TRUE`. With `control$impute = FALSE`, any `NA` left in
a predictor reaches `randomForest`, which fails on it.

### Metrics

Classification reports `accuracy`, `kappa`, and `auc` on the held-out
rows. `kappa` is Cohen's kappa from the held-out confusion table, `NA`
when chance agreement is 1. `auc` is filled in only when the target has
exactly two levels and the suggested package 'pROC' is installed. It is
`NA` for every multi-class fit, and also (with a warning) when the
held-out rows contain only one of the two classes.

For a binary target, the positive class (whose predicted probability is
scored) is `control$positive_class` or, by default, the *second* factor
level after unused levels are dropped.
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
uses the *first* level. ROC AUC is the same whichever class is called
positive, so `auc` does not depend on this choice, but class-specific
quantities such as
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)'s
PR AUC or sensitivity are not directly comparable with anything computed
here.

Regression reports `RMSE`, `MAE`, and `R2` on the held-out rows. `R2` is
`1 - SSE/SST`, so it can be negative; it is not the squared correlation
that
[`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md)
reports. It is `NA` when the held-out target has no variance, including
variance that is only floating-point round-off relative to the target's
magnitude.

## See also

The [Regularization and embedded
importance](https://elkronos.github.io/featR/articles/regularization.html)
guide, which runs this function on a worked example.

## Examples

``` r
# \donttest{
if (requireNamespace("randomForest", quietly = TRUE) &&
    requireNamespace("caret", quietly = TRUE)) {
  res <- fs_randomforest(
    iris,
    target = "Species",
    task = "classification",
    control = list(ntree = 100),
    seed = 42
  )
  # every predictor, in decreasing order of importance
  print(res$selected)
  print(res$scores)
  print(res$details$metrics)
}
#> [1] "Petal.Length" "Petal.Width"  "Sepal.Length" "Sepal.Width" 
#> Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
#>    15.409812    12.423520     3.745021     1.804016 
#> $accuracy
#> [1] 0.9444444
#> 
#> $kappa
#> [1] 0.9166667
#> 
#> $auc
#> [1] NA
#> 
# }
```
