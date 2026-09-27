# Feature Selection via Information Gain

Accepts either a single data.frame or a list of data.frames and scores
every predictor by its information gain (in bits) with respect to the
target, optionally normalized to a gain ratio.

## Usage

``` r
fs_infogain(
  data,
  target,
  numeric_bins = NULL,
  normalize = c("none", "gain_ratio"),
  top_n = NULL,
  remove_na = TRUE,
  verbose = FALSE
)
```

## Arguments

- data:

  A data.frame (a data.table is accepted and is copied, never modified
  in place), or a list of data.frames each containing `target`. Column
  names must be unique within each data.frame.

- target:

  Character. Name of the target column.

- numeric_bins:

  Optional whole number \>= 1 (values below 2 are clamped to 2)
  overriding the automatic bin count for numeric predictors and for a
  numeric target. Default `NULL` (bins chosen per column).

- normalize:

  One of `"none"` (default, raw information gain in bits) or
  `"gain_ratio"` (gain divided by the predictor's split entropy). See
  the section on cardinality bias.

- top_n:

  Optional whole number \>= 1. When supplied, `selected` holds the
  `top_n` highest-scoring features (fewer if fewer were scored). This is
  a rank cut, not a score floor: a zero-scoring feature is selected if
  the ranking reaches it. When `NULL` (default), `selected` instead
  holds every feature whose score is strictly greater than 0. A gain
  within floating-point rounding of zero (as an exactly independent
  predictor produces) is reported as exactly 0, so it is not selected.

- remove_na:

  Logical. If `TRUE` (default), rows with NA in the target are removed
  up front. See Details for its narrow practical effect.

- verbose:

  Logical. If `TRUE`, report how many features were scored, how many
  names collided across data.frames, and how many were selected. Default
  `FALSE`.

## Value

An object of class `fs_result` with elements:

- `selected`: character vector of selected feature names, ordered by
  decreasing score. Features with an undefined (`NA`) score are never
  selected.

- `scores`: named numeric vector of the (possibly normalized) score for
  every candidate feature, ranked in decreasing order with `NA` scores
  last. For list input this is the union of features across data.frames;
  a name occurring in several data.frames keeps its highest score.

- `method`: `"infogain"`.

- `task`: `"classification"` (the target is always discretized).

- `model`: `NULL` (this is a filter; nothing is fitted).

- `details`: a list holding `table` (the full scored table, one row per
  scored column of each input: `Variable`, `InfoGain`, plus
  `SplitEntropy` and `GainRatio` when `normalize = "gain_ratio"`, plus
  `Origin` for list input), `normalize` (as resolved), `numeric_bins`
  (the requested bin count as an integer, `NULL` when automatic),
  `n_features` (the number of distinct scored features, i.e.
  `length(scores)`), and, for list input, `collisions` (a table of the
  feature names found in more than one data.frame, with `Kept` marking
  the row whose score won; zero rows when there are none).

- `call`: the matched call.

## Details

Use this to rank candidate predictors cheaply, before any model is
fitted: it answers "how many bits of uncertainty about the target does
knowing this one predictor remove?". It needs no distributional
assumptions and handles mixed column types, but it is a univariate
filter – each predictor is scored on its own, so two redundant copies of
the same information both score highly, and a predictor that only
matters in combination with another scores low. Treat the ranking as a
shortlist, not a final feature set.

- Numeric predictors are discretized into
  `max(Freedman-Diaconis, Sturges)` equal-width bins (never fewer than
  2), unless `numeric_bins` overrides the count. Under the automatic
  rule, a column with a zero range or a zero interquartile range falls
  back to 2 bins; a column with a single distinct value becomes a
  single-level factor either way and scores 0.

- The target is always treated categorically, so `task` is always
  `"classification"`. Numeric targets are discretized the same way, ONCE
  on all rows with a non-NA target, so bin breaks are shared and scores
  are comparable across predictors.

- Date-like predictors are expanded into `*_year`, `*_month`, `*_day`
  columns (base R; the originals are dropped). Date-like targets are
  treated as categorical days.

- NAs are handled per predictor/target pair; rows are never dropped
  globally for other predictors. A predictor whose score is undefined
  (`NA`) is never selected.

- `remove_na` has a deliberately narrow effect: rows with NA in the
  target are excluded per pair anyway, so the observable difference is
  only when the target is entirely NA – `remove_na = TRUE` stops with an
  error, while `remove_na = FALSE` returns `NA` information gain for
  every predictor.

## Cardinality bias and the gain ratio

Raw information gain systematically favors predictors with many distinct
levels. In the limit, a near-unique identifier column splits the data
into near-singleton groups, drives the conditional entropy H(Y \| X) to
zero and therefore attains the largest gain any predictor can attain,
H(Y) – while carrying no generalizable signal whatsoever. Comparing raw
gains across predictors of different cardinality is therefore comparing
unlike things.

`normalize = "gain_ratio"` applies Quinlan's correction: each
predictor's gain is divided by that predictor's own split entropy H(X),
the entropy of its (discretized) level distribution. H(X) grows with
cardinality – it is `log2(k)` for a predictor with `k` equally frequent
levels – so dividing by it charges a predictor for the fineness of the
split it makes. A constant predictor has `H(X) = 0` and, rather than
dividing by zero, is assigned a gain ratio of 0 (its gain is zero too).
Because gain ratios are scaled gains, do not compare them against
thresholds calibrated for raw gains in bits.

## Examples

``` r
# Single data.frame:
df <- data.frame(
  A = rep(1:10, each = 10),
  B = rep(c("yes", "no"), 50),
  when = as.Date("2020-01-01") + rep(0:24, 4),
  target = rep(1:2, 50)
)
res <- fs_infogain(df, target = "target")
res$selected
#> [1] "B"
res$scores
#>          B          A  when_year when_month   when_day 
#>          1          0          0          0          0 
res$details$table
#>     Variable InfoGain
#> 1          A        0
#> 2          B        1
#> 3  when_year        0
#> 4 when_month        0
#> 5   when_day        0

# Normalize by split entropy to offset the bias toward many-leveled
# predictors, and keep the two best-ranked features:
fs_infogain(df, target = "target", normalize = "gain_ratio", top_n = 2)
#> <fs_result> infogain (classification)
#> Selected 2 of 5 features
#>   B, A
#> Details: table, normalize, numeric_bins, n_features (in $details)

# List of data.frames:
df1 <- data.frame(A = rep(1:5, 20), target = rep(1:2, 50))
df2 <- data.frame(B = rep(c("yes", "no"), 50), target = rep(letters[1:2], 50))
fs_infogain(list(df1 = df1, df2 = df2), target = "target")
#> <fs_result> infogain (classification)
#> Selected 1 of 2 features
#>   B
#> Details: table, normalize, numeric_bins, n_features, collisions (in $details)
```
