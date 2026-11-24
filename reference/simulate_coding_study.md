# Simulate a Realistic Multi-Facet LLM Coding & Validation Study

Generates a complete synthetic research dataset combining:

1.  Units (optionally nested in clusters) with covariates, subgroup
    membership, latent true scores, and a human gold-standard validation
    subset (`NA` on unlabeled units).

2.  Multi-facet LLM scorings executed via
    [`coding_design()`](https://marcusharrisphd.com/llmmetric/reference/coding_design.md),
    [`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md),
    and
    [`backend_mock()`](https://marcusharrisphd.com/llmmetric/reference/backend_mock.md)
    with configurable reliability variance components, calibration
    slope/bias, and Differential Prompt Functioning (DPF).

## Usage

``` r
simulate_coding_study(
  n_units = 120L,
  n_labeled = 40L,
  n_clusters = 30L,
  prompts = c(direct = "Rate construct intensity: {text}", rubric =
    "Apply anchored rubric: {text}", cot = "Reason carefully then rate: {text}"),
  models = c("model_a", "model_b"),
  beta = c(`(Intercept)` = 2.5, x = 1.1, group_b = 0.6),
  cluster_sd = 0.5,
  resid_sd = 0.8,
  llm_intercept = 0.4,
  llm_slope = 0.85,
  llm_error_sd = 0.45,
  prompt_sd = 0.25,
  model_sd = 0.2,
  unit_prompt_sd = 0.25,
  unit_model_sd = 0.2,
  dpf_uniform = c(group_b = 0.55),
  dpf_nonuniform = NULL,
  dpf_prompt_group = NULL,
  seed = 2026L
)
```

## Arguments

- n_units:

  Total number of text units. Default `120L`.

- n_labeled:

  Number of units with observed human gold-standard scores. Default
  `40L`.

- n_clusters:

  Number of clusters (e.g., participants or classrooms) that units are
  nested within. Default `30L`.

- prompts:

  Named character vector of prompt templates.

- models:

  Character vector of model names.

- beta:

  Named or unnamed length-3 numeric vector
  `c(intercept, beta_x, beta_group)` governing the true data-generating
  regression \\Y_i = \beta_0 + \beta_x X_i + \beta_g \mathbb{I}(G_i
  = B) + u\_{c(i)} + \varepsilon_i\\.

- cluster_sd:

  SD of cluster random intercept \\u_c\\. Default `0.5`.

- resid_sd:

  SD of unit-level outcome residual \\\varepsilon_i\\. Default `0.8`.

- llm_intercept:

  Additive LLM calibration bias. Default `0.4`.

- llm_slope:

  LLM calibration slope on true score. Default `0.85`.

- llm_error_sd:

  Residual LLM measurement noise SD. Default `0.45`.

- prompt_sd:

  SD of prompt main effects. Default `0.25`.

- model_sd:

  SD of model main effects. Default `0.20`.

- unit_prompt_sd:

  SD of `unit x prompt` interaction. Default `0.25`.

- unit_model_sd:

  SD of `unit x model` interaction. Default `0.20`.

- dpf_uniform:

  Optional named vector of uniform subgroup DPF shifts (e.g.,
  `c(group_b = 0.6)`).

- dpf_nonuniform:

  Optional named vector of non-uniform subgroup DPF slope shifts.

- dpf_prompt_group:

  Optional matrix of prompt-by-group DPF shifts.

- seed:

  Integer random seed. Default `2026L`.

## Value

A list with:

- `coded`: an `llm_coded_data` object (long multi-facet scores).

- `unit_data`: unit-level tibble with aggregated `llm_score`,
  `human_score` (`NA` on unlabeled units), `true_score`, `x`, `group`,
  and `cluster`.

- `true_beta`: ground-truth regression parameters.

## Examples

``` r
sim <- simulate_coding_study(n_units = 40, n_labeled = 20, n_clusters = 10, seed = 1)
g_study(sim$coded)
#> 
#> ── Generalizability Study (G-Study) 
#> Units (unit_id): 40 | Facets: prompt=3, model=2 | Method: REML | Balanced: TRUE
#> Observed design: G (Erho^2) = 0.953 | Phi = 0.950 | SEM(rel) = 0.264, SEM(abs)
#> = 0.274
#> Single-facet (n=1): G = 0.816 | Phi = 0.802
#> # A tibble: 7 × 5
#>   component      variance    sd pct_total role                 
#>   <chr>             <dbl> <dbl>     <dbl> <chr>                
#> 1 unit_id          1.41   1.19      80.2  universe             
#> 2 prompt           0      0          0    absolute_only        
#> 3 model            0      0          0    absolute_only        
#> 4 unit_id:prompt   0.0358 0.189      2.03 relative_and_absolute
#> 5 unit_id:model    0.0324 0.180      1.84 relative_and_absolute
#> 6 prompt:model     0.031  0.176      1.76 absolute_only        
#> 7 Residual         0.249  0.499     14.2  relative_and_absolute
```
