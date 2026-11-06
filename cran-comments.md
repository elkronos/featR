# cran-comments

## Submission notes

This is an update of featR from 0.1.0 to 0.2.0.

It is a correctness release. A statistical, methodological, and programmatic
review of the whole package fixed silent wrong answers, hangs, crashes, and
documentation that did not match behavior. Each fix has a regression test.
Some fixes change results or turn previously accepted, invalid input into an
informative error. NEWS.md lists every change, with behavior changes first.

## Test environments

* GitHub Actions (r-lib/actions): macOS latest (R release), Windows latest
  (R release), Ubuntu latest (R devel, R release, R oldrel-1).
* win-builder (R devel).
* Local Ubuntu (R release), `R CMD check --as-cran` twice: once with every
  suggested package installed and `--run-donttest`, and once with no
  suggested packages installed (`_R_CHECK_FORCE_SUGGESTS_=false`), to confirm
  that examples, tests, and the vignette all run without them.

## R CMD check results

0 errors | 0 warnings | 0 notes

* Words that may be flagged as possibly misspelled in DESCRIPTION (Barnhill,
  Boruta, Gabry, Guyon, Hastie, Kursa, Rudnicki, Tibshirani, Vapnik, Vehtari,
  Zou) are author surnames from the cited references and the name of the
  Boruta method.

## Reverse dependencies

There are no reverse dependencies on CRAN.

## Notes for reviewers

* As requested in the review of 0.1.0, Boruta is no longer in single quotes
  in the Description (it names the method), and the methods are cited as
  authors (year) <doi:...>.
* All modeling engines (brms, caret, glmnet, randomForest, kernlab, earth,
  Boruta, and others) are in Suggests. Every function that needs one checks
  availability with `requireNamespace()` and stops with a message naming the
  packages to install. Examples that use a suggested package are guarded with
  `requireNamespace()`. The tests skip those paths when a package is absent.
* Examples run in under 2 seconds each and are not wrapped in `\donttest{}`,
  with two exceptions. `fs_bayes()` compiles a Stan program per candidate
  model; its example is in `\donttest{}`, guarded on brms, limited to two
  single-predictor models, and wrapped in `try()` so that a machine without a
  working Stan toolchain skips it rather than failing. `fs_boruta()` is in
  `\donttest{}` because its run time depends on the number of Boruta
  iterations.
* `fs_mars()` with `search = "random"` and a factor outcome attaches earth (a
  suggested package) for the duration of that call with
  `withr::local_package()`, because caret's random-grid code for earth looks
  up `contr.earth.response` on the search path. On exit, earth and any
  packages it attached are detached, so the user's search path is unchanged.
* Parallel execution is opt-in throughout. Every function defaults to a single
  worker, requested worker counts are capped at the detected core count, and
  no example or test uses more than two cores.
* The package never seeds the random number generator unless the user
  supplies a `seed` argument, and then restores the previous RNG state on exit
  via `withr::local_seed()`.
* Package code writes nothing to the user's home directory, working
  directory, or global environment. The only file it creates is a temporary
  backing file under `tempdir()` for `fs_pca()` on very large data, which is
  removed when the call returns.
