# Aggregate Multi-Facet LLM Scores to Unit Level

Collapses multi-facet scores (e.g., across prompts, models,
temperatures, or replicates) into unit-level composite scores suitable
for calibration, DPF, or downstream Prediction-Powered Inference.
Unit-level covariates that are constant within each aggregation group
are preserved automatically.

## Usage

``` r
aggregate_scores(
  x,
  score_col = "score",
  id_col = "unit_id",
  by = NULL,
  fn = mean,
  na.rm = TRUE
)
```

## Arguments

- x:

  An `llm_coded_data` object or a `data.frame`.

- score_col:

  Name of the numeric score column to aggregate.

- id_col:

  Name of the unit identifier column. Default `"unit_id"`.

- by:

  Optional character vector of additional grouping columns (e.g.,
  `"model"` or `"prompt"`).

- fn:

  Summary function applied to `score_col` (default `mean`).

- na.rm:

  Logical; remove missing values before aggregating. Default `TRUE`.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
containing `id_col`, `by` columns, aggregated `score_col`, `n_ratings`,
`rating_sd`, and preserved group-constant metadata columns.

## Examples

``` r
data(ema_coded)
agg <- aggregate_scores(ema_coded, score_col = "llm_score", id_col = "unit_id")
head(agg)
#> # A tibble: 6 × 8
#>   unit_id llm_score n_ratings rating_sd cluster human_score group        x
#>   <chr>       <dbl>     <int>     <dbl> <chr>         <dbl> <fct>    <dbl>
#> 1 u_0001       2.72         6     0.330 cl_001        NA    group_b -0.529
#> 2 u_0002       3.48         6     0.372 cl_002        NA    group_b  0.166
#> 3 u_0003       2.76         6     0.497 cl_003        NA    group_b -0.255
#> 4 u_0004       2.36         6     0.918 cl_004         2.58 group_a  0.333
#> 5 u_0005       3.49         6     0.487 cl_005         2.60 group_b  0.182
#> 6 u_0006       2.99         6     0.493 cl_006         3.11 group_a  1.16 
```
