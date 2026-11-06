# Rank features by information gain

Scores every predictor by its information gain about the target, in
bits, optionally divided by the predictor's own entropy to give a gain
ratio. Accepts a single data frame or a list of data frames.

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

  A data frame, or a list of data frames that each contain `target`. A
  data.table is copied, never modified in place. Column names must be
  unique, non-empty, and not `NA` within each data frame, and list
  columns are not supported.

- target:

  Name of the target column.

- numeric_bins:

  Optional whole number of bins for numeric predictors and a numeric
  target, replacing the automatic count. Values below 2 are raised to 2.
  Default `NULL` (bins chosen per column).

- normalize:

  `"none"` (default) for raw information gain in bits, or `"gain_ratio"`
  for gain divided by the predictor's split entropy. See "Cardinality
  bias and the gain ratio".

- top_n:

  Optional whole number of at least 1. When supplied, `selected` holds
  the `top_n` highest-scoring features (fewer if fewer were scored).
  This is a rank cut, not a score floor, so a feature scoring 0 is
  selected if the ranking reaches it. When `NULL` (default), `selected`
  holds every feature with a score strictly greater than 0. A gain
  within floating-point rounding of zero, as an exactly independent
  predictor produces, is reported as exactly 0 and is not selected.

- remove_na:

  If `TRUE` (default), rows with a missing target are removed first. See
  "Missing values" for why this rarely changes the result.

- verbose:

  If `TRUE`, report how many features were scored, how many names
  occurred in more than one data frame, and how many were selected.
  Default `FALSE`.

## Value

An `fs_result` object with:

- `selected`:

  Selected feature names in decreasing order of score. A feature with an
  `NA` score is never selected.

- `scores`:

  Named numeric vector of the score (gain or gain ratio) for every
  candidate feature, in decreasing order with `NA` last. For list input
  it covers the union of features across data frames, and a name found
  in several data frames keeps its highest score.

- `method`:

  `"infogain"`.

- `task`:

  `"classification"`, because the target is always treated as
  categorical.

- `model`:

  `NULL`; no model is fitted.

- `details`:

  A list with:

  - `table`: the full scored table, one row per scored column of each
    input, with columns `Variable` and `InfoGain`, plus `SplitEntropy`
    and `GainRatio` when `normalize = "gain_ratio"`, plus `Origin` for
    list input.

  - `normalize`: the value used.

  - `numeric_bins`: the requested bin count as an integer, or `NULL`
    when automatic.

  - `n_features`: the number of distinct scored features, equal to
    `length(scores)`.

  - `collisions` (list input only): the feature names found in more than
    one data frame, with `Kept` marking the row whose score was used. It
    has zero rows when no name repeats.

- `call`:

  The matched call.

## Details

Information gain is the drop in the entropy of the target once you know
the value of one predictor: how many bits of uncertainty about the
target that predictor removes. It makes no distributional assumptions
and handles mixed column types, which makes it a cheap way to rank
candidates before fitting any model.

It is a univariate filter. Each predictor is scored on its own, so two
copies of the same information both score highly, and a predictor that
matters only in combination with another scores low. Treat the ranking
as a shortlist rather than a final feature set.

The default selection rule keeps every feature with a score above 0,
which screens out almost nothing. On a finite sample, a predictor
unrelated to the target still has a positive gain in almost every data
set, and more so with more bins or levels. In a 200-row example with 50
pure-noise columns, all 50 are selected. Use `top_n`, or compare against
gains computed on a permuted target, to make a real cut.

## How columns are prepared

- Numeric predictors are cut into equal-width bins. The number of bins
  is the larger of the Freedman-Diaconis and Sturges counts, at least 2,
  unless `numeric_bins` sets it. Under the automatic rule, a column with
  zero range or zero interquartile range gets 2 bins. A column with a
  single distinct value is one level either way and scores 0.

- Character and logical predictors become factors.

- Date and date-time predictors are replaced by three numeric columns,
  `<name>_year`, `<name>_month`, and `<name>_day`, which are then binned
  like any numeric column. If one of those names already exists in
  `data`, the call stops.

- The target is always treated as categorical, so `task` is always
  `"classification"`. A numeric target is binned by the same rule, once,
  on all rows where it is observed, so every predictor is scored against
  the same bins. A date target is treated as one class per day.

## Missing values

Missing values are handled for each predictor-target pair separately: a
missing value in one predictor does not remove that row for the others.
A predictor with no complete pairs scores `NA` and is never selected.

`remove_na` has a narrow effect. Rows with a missing target are excluded
pair by pair anyway, so the only difference shows when the target is
entirely missing: `remove_na = TRUE` stops with an error, and
`remove_na = FALSE` returns an `NA` score for every predictor.

## Cardinality bias and the gain ratio

Raw information gain favors predictors with many distinct levels. A
near-unique identifier column splits the data into groups of about one
row, which drives the conditional entropy H(Y \| X) to zero and gives it
the largest gain any predictor can reach, H(Y), while carrying no signal
that generalizes. Raw gains of predictors with different numbers of
levels are therefore not comparable.

`normalize = "gain_ratio"` applies Quinlan's correction: each
predictor's gain is divided by its split entropy H(X), the entropy of
its (binned) level distribution. H(X) grows with the number of levels
(it is `log2(k)` for `k` equally frequent levels), so the division
penalizes a predictor for how finely it splits the data. A constant
predictor has H(X) = 0 and gets a gain ratio of 0 instead of a division
by zero. Gain ratios are on a different scale from raw gains, so do not
reuse a threshold chosen for gains in bits.

## See also

The [Filter
methods](https://elkronos.github.io/featR/articles/filters.html) guide,
which runs this function on a worked example.

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
