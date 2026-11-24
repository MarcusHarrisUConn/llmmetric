# Differential Prompt Functioning (DPF) & Measurement Invariance Test

Tests whether LLM-generated scores exhibit uniform (intercept shift) or
non-uniform (slope difference) bias across observed demographic or
experimental subgroups conditional on human gold-standard scores. Adapts
nested OLS / logistic Differential Item Functioning (DIF) regression
frameworks (Swaminathan & Rogers, 1990; Zumbo, 1999) to LLM coding
pipelines, with automatic unit-clustered sandwich inference when units
appear across multiple prompts or models, per-prompt FDR screening, and
a global `prompt x group` interaction test.

## Usage

``` r
dpf_test(
  data,
  score_col = "score",
  truth_col = "human_score",
  group_col = "group",
  prompt_col = NULL,
  id_col = NULL,
  family = c("auto", "gaussian", "binomial"),
  poly_degree = 1L,
  alpha = 0.05,
  na.rm = TRUE
)
```

## Arguments

- data:

  An `llm_coded_data` object or a `data.frame`.

- score_col:

  Name of the LLM score column (numeric for `"gaussian"`, or binary
  `{0,1}` / logical / 2-level factor for `"binomial"`).

- truth_col:

  Name of the numeric human gold-standard (matching criterion) column.

- group_col:

  Name of the categorical subgroup column (at least 2 levels).

- prompt_col:

  Optional name of a prompt column. When supplied with at least 2
  levels, runs per-prompt DPF screening (with Benjamini-Hochberg
  FDR-adjusted p-values) and a global `prompt x group` interaction test.

- id_col:

  Optional name of the unit identifier column. When repeated
  observations exist per unit, cluster-robust sandwich Wald tests are
  used to prevent pseudoreplication Type-I error inflation.

- family:

  Outcome family: `"auto"` (detects binary vs continuous), `"gaussian"`,
  or `"binomial"`.

- poly_degree:

  Positive integer polynomial degree for the matching criterion
  `truth_col`. Default `1L` (linear matching).

- alpha:

  Significance threshold for flagging DPF. Default `0.05`.

- na.rm:

  Logical; drop rows with missing values. Default `TRUE`.

## Value

An S3 object of class `"llm_dpf"` containing:

- `tests`: tibble of nested hypothesis tests (`uniform`, `nonuniform`,
  `omnibus`) with test statistics, df, p-values, \\\Delta R^2\\, Cohen's
  \\f^2\\ (Gaussian), magnitude classification, and `flagged`.

- `subgroup_summary`: tibble of per-subgroup raw bias, conditional bias
  at mean gold-standard score (with SE and 95% CI), conditional slope,
  RMSE, and correlation.

- `prompt_dpf`: tibble of per-prompt DPF tests with BH-adjusted p-values
  (or `NULL` if `prompt_col` is `NULL`).

- `prompt_interaction_test`: tibble testing `prompt x group` interaction
  controlling for `truth_col` (or `NULL`).

- `dpf_detected`: logical scalar indicating omnibus DPF at `alpha`.

- `clustered`: logical indicating whether unit-clustered inference was
  used.

- `family`, `reference_group`, `models`, `score_col`, `truth_col`,
  `group_col`, `prompt_col`, `alpha`, `data`.

## Examples

``` r
data(ema_coded)
agg <- aggregate_scores(ema_coded, score_col = "llm_score")
res <- dpf_test(agg, score_col = "llm_score", truth_col = "human_score", group_col = "group")
res
#> 
#> ── Differential Prompt Functioning (DPF) Diagnostics 
#> Family: gaussian | Reference group: "group_a" | Clustered: FALSE | DPF
#> detected: TRUE
#> # A tibble: 3 × 7
#>   test       statistic    df p_value delta_r2 magnitude  flagged
#>   <chr>          <dbl> <dbl> <chr>      <dbl> <chr>      <lgl>  
#> 1 uniform       88.1       1 <0.0001   0.0776 large      TRUE   
#> 2 nonuniform     0.998     1 0.3230    0.0009 negligible FALSE  
#> 3 omnibus       44.6       2 <0.0001   0.0784 large      TRUE   
```
