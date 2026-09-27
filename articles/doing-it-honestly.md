# Doing it honestly

Feature selection is easy to get subtly wrong in ways that make results
look better than they are. The usual cause is that the data used to
choose the features is also used to judge them. featR is built to avoid
that where it can, and to say so plainly where it cannot. This page
summarizes what that means in practice and points to the details.

## The one rule

**Anything that looks at the outcome is part of model fitting.** That
covers choosing features, tuning, imputing, and scaling. If rows that
will later be used to measure performance took part in any of those
steps, the measurement is optimistic. Selecting features on the full
data and then cross-validating a model on those features is the classic
case. On pure noise it can report a strong fit where there is none.

[Validation and
leakage](https://elkronos.github.io/featR/articles/validation-and-leakage.md)
demonstrates this with data where the right answer is known, and walks
through a pipeline that avoids it.

## What featR does for you

- **Held-out splits.**
  [`fs_randomforest()`](https://elkronos.github.io/featR/reference/fs_randomforest.md),
  [`fs_mars()`](https://elkronos.github.io/featR/reference/fs_mars.md),
  [`fs_recursivefeature()`](https://elkronos.github.io/featR/reference/fs_recursivefeature.md),
  and [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
  split the data first and fit every data-dependent step on the training
  rows only. The metrics in `details` come from rows the selection never
  saw.
- **Selection inside resampling.**
  [`fs_svm()`](https://elkronos.github.io/featR/reference/fs_svm.md)
  repeats SVM-RFE inside every cross-validation fold when it chooses how
  many features to keep. `fs_elastic(use_pca = TRUE)` refits the PCA in
  every resample, and class upsampling happens inside folds rather than
  before them.
- **No silent leakage by default.**
  [`fs_lasso()`](https://elkronos.github.io/featR/reference/fs_lasso.md)
  refuses missing predictors rather than imputing them from the full
  data. If you opt in to `impute = "mean"`, it warns you.
- **Labeled in-sample numbers.** Statistics computed on the data used
  for selection are documented as such, for example
  [`fs_stepwise()`](https://elkronos.github.io/featR/reference/fs_stepwise.md)’s
  p-values and
  [`fs_bayes()`](https://elkronos.github.io/featR/reference/fs_bayes.md)’s
  `details$mae`.
- **Known false-positive behavior is documented.** For example,
  [`fs_boruta()`](https://elkronos.github.io/featR/reference/fs_boruta.md)
  can confirm pure-noise features, especially after
  `TentativeRoughFix()`. The [wrapper
  methods](https://elkronos.github.io/featR/articles/wrappers.md) guide
  shows a real case.

## What you still have to do

A held-out split inside one function call protects that call. It does
not protect your workflow:

1.  Set aside a final test set **once**, before anything looks at the
    outcome.
2.  Compare methods and tune on the remaining rows only.
3.  Use the test set once, at the end, to report the pipeline you chose.
4.  Treat p-values and intervals from a model whose features were
    selected on the same data as descriptive, not as valid inference.

## Reproducible results

Honest results should also be repeatable. featR never touches your
random number stream unless you pass `seed`. When you do, the seed
applies to that call only, and your previous state is restored
afterwards. Parallelism is opt-in, capped at your core count, and
cleaned up when the call returns. [Reproducibility, randomness, and
parallelism](https://elkronos.github.io/featR/articles/reproducibility.md)
has the details.
