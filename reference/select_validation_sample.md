# Select a Human Validation Sample with Exact Inclusion Probabilities & IPW Weights

Draws a reproducible human-validation subset from a corpus of LLM-coded
texts using simple random, subgroup-stratified (proportional,
equal-per-stratum for DPF power, or Neyman-optimal), score-quantile
stratified, or cluster-stratified sampling. Returns exact inclusion
probabilities \\\pi_i \in (0, 1\]\\ and Horvitz-Thompson
inverse-probability weights \\w_i = 1 / \pi_i\\ ready to pass directly
into weighted
[`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md)
or
[`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md)
(active / IPW-PPI).

## Usage

``` r
select_validation_sample(
  data,
  n = NULL,
  strategy = c("simple", "stratified", "neyman", "quantile", "cluster"),
  strata_col = NULL,
  score_col = NULL,
  cluster_col = NULL,
  allocation = c("proportional", "equal", "neyman"),
  n_quantiles = 4L,
  n_clusters = NULL,
  min_per_stratum = 2L,
  seed = NULL
)
```

## Arguments

- data:

  A `data.frame` (or `llm_coded_data`) representing the full
  population/corpus of units to sample from.

- n:

  Target total number of units to select for human validation. For
  `strategy = "cluster"`, used to determine the number of clusters if
  `n_clusters` is `NULL`.

- strategy:

  Sampling design:

  - `"simple"`: simple random sampling without replacement (SRSWOR).

  - `"stratified"`: stratified sampling across `strata_col` using
    `allocation`.

  - `"neyman"`: shorthand for Neyman-optimal allocation (`n_h`
    proportional to `N_h * SD_h`) using `score_col` (and `strata_col`,
    or quantile bins of `score_col` if `strata_col` is `NULL`).

  - `"quantile"`: stratifies units into `n_quantiles` bins of
    `score_col` (covering distribution tails evenly).

  - `"cluster"`: whole-cluster sampling over `cluster_col`.

- strata_col:

  Name of the categorical stratification column (required for
  `strategy = "stratified"`, optional for `"neyman"`).

- score_col:

  Name of a numeric proxy/LLM score column used to estimate stratum SDs
  for Neyman allocation or to form quantile strata.

- cluster_col:

  Name of the cluster identifier column (required for
  `strategy = "cluster"`).

- allocation:

  Stratum allocation rule for `"stratified"` or `"quantile"`:
  `"proportional"` (default), `"equal"` (maximizes power for subgroup
  DPF tests), or `"neyman"` (minimizes overall variance).

- n_quantiles:

  Integer number of quantile strata for `strategy = "quantile"` (or
  `"neyman"` without `strata_col`). Default `4L`.

- n_clusters:

  Optional integer number of whole clusters to select when
  `strategy = "cluster"`. Overrides `n` if supplied.

- min_per_stratum:

  Minimum units selected per stratum (capped by stratum size). Default
  `2L`.

- seed:

  Optional integer random seed. Uses local RNG state restoration so
  global `.Random.seed` is never mutated.

## Value

An S3 object of class `"llm_validation_sample"` containing:

- `selected_idx`: sorted integer vector of selected row indices.

- `selected`: logical indicator vector of length `nrow(data)`.

- `inclusion_prob`: numeric vector of exact positive inclusion
  probabilities \\\pi_i\\.

- `weights`: numeric vector of Horvitz-Thompson weights \\w_i = 1 /
  \pi_i\\.

- `data`: tibble copy of `data` augmented with `is_validation`,
  `inclusion_prob`, and `sampling_weight`.

- `stratum_summary`: tibble summarizing population size, selected count,
  inclusion probability, and weight per stratum.

- `strategy`, `allocation`, `n_target`, `n_selected`, `n_population`.

## Examples

``` r
data(ema_coded)
agg <- aggregate_scores(ema_coded, score_col = "llm_score")
vs <- select_validation_sample(
  agg, n = 40, strategy = "stratified",
  strata_col = "group", allocation = "equal", seed = 42
)
vs
#> 
#> ── Validation Sample Plan (stratified / equal) 
#> Selected 40 of 120 units (33.3%) | Weight range: [2.90, 3.10]
#> # A tibble: 2 × 6
#>   stratum n_pop n_selected inclusion_prob weight stratum_sd
#>   <chr>   <int>      <int>          <dbl>  <dbl>      <dbl>
#> 1 group_a    58         20          0.345    2.9         NA
#> 2 group_b    62         20          0.323    3.1         NA
```
