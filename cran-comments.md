# cran-comments

## Submission notes

This is a new submission of featR to CRAN.

featR collects filter, wrapper, and embedded feature-selection methods behind
one calling convention -- `fs_<method>(data, target, ...)` -- and one return
type, an `fs_result` object with `selected`, `scores`, `method`, `task`,
`model`, and `details` components.

## Test environments

* local macOS 26.5 (aarch64-apple-darwin23), R 4.6.1 -- 0 errors, 0 warnings,
  0 notes, with all Suggests except brms installed
* win-builder, R-devel -- (paste result before submitting)
* macOS builder (mac.r-project.org), R-release -- (paste result before submitting)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for reviewers

* All modeling engines (brms, caret, glmnet, randomForest, kernlab, earth,
  Boruta, and others) are in Suggests. Every function that needs one checks
  availability with `requireNamespace()` and fails with an informative message
  naming the packages to install. All examples that use a suggested package are
  wrapped in `\donttest{}` behind `requireNamespace()` guards, and the test
  suite skips those paths when the package is absent.
* `fs_bayes()` compiles a Stan program per candidate model, so its example uses
  `\dontrun{}`: compilation alone exceeds a reasonable example budget. The
  equivalent call is exercised in the test suite under `skip_on_cran()`.
* Parallel execution is opt-in throughout. Every function defaults to a single
  worker, requested worker counts are capped at the detected core count, and no
  example or test uses more than two cores.
* The package never calls `set.seed()` unless the user supplies a `seed`
  argument, and restores the previous RNG state on exit via `withr::local_seed()`.
* No package code writes to the user's file system, home directory, working
  directory, or global environment.
