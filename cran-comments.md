# cran-comments

## Submission notes

First submission of featR to CRAN.

## Test environments

* local: macOS (aarch64-apple-darwin23), R 4.6.1
* win-builder: R-devel (fill in after running devtools::check_win_devel())
* mac-builder: R-release (fill in after https://mac.r-project.org/macbuilder/submit.html)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for reviewers

* Heavy modeling engines (brms, caret, glmnet, randomForest, ...) are in
  Suggests; every function that needs one checks availability via
  requireNamespace() and errors informatively when absent. All examples using
  Suggests packages are guarded.
* Parallel code paths are opt-in, default to sequential execution, and cap
  worker counts; examples and tests use at most 2 cores.
