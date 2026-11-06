# cran-comments

## Submission notes

This is an update of featR from 0.1.0 to 0.2.0.

It is a correctness release. Two statistical, methodological, and
programmatic reviews of the whole package fixed silent wrong answers, hangs,
crashes, and documentation that did not match behavior. Each fix has a
regression test; the test suite grew from 2013 to 2560 passing assertions.
Some fixes change results or turn previously accepted, invalid input into an
informative error. NEWS.md lists every change, with behavior changes first.

## Test environments

* GitHub Actions (r-lib/actions): macOS latest (R release), Windows latest
  (R release), Ubuntu latest (R devel, R release, R oldrel-1).
* Ubuntu (R release) with every suggested package except Boruta, bigstatsr,
  and PRROC installed: 2560 passing assertions, 7 skipped for those packages.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.

## Notes for reviewers

* All modeling engines (brms, caret, glmnet, randomForest, kernlab, earth,
  Boruta, and others) are in Suggests. Every function that needs one checks
  availability with `requireNamespace()` and fails with an informative message
  naming the packages to install. All examples that use a suggested package are
  wrapped in `\donttest{}` behind `requireNamespace()` guards, and the test
  suite skips those paths when the package is absent.
* `fs_bayes()` compiles a Stan program per candidate model. Its example is in
  `\donttest{}`, guarded on brms being installed, limited to two
  single-predictor models, and wrapped in `try()`, so a machine without a
  working Stan toolchain skips it rather than failing. The equivalent call is
  exercised in the test suite under `skip_on_cran()`.
* Parallel execution is opt-in throughout. Every function defaults to a single
  worker, requested worker counts are capped at the detected core count, and no
  example or test uses more than two cores.
* The package never calls `set.seed()` unless the user supplies a `seed`
  argument, and then restores the previous RNG state on exit via
  `withr::local_seed()`.
* No package code writes to the user's file system, home directory, working
  directory, or global environment.
