# Synthetic Multi-Facet EMA Diary LLM Coding Dataset

A compact, reproducible synthetic dataset simulating an Ecological
Momentary Assessment (EMA) emotional-distress coding study. Contains 120
diary entries nested within 30 participants (`cluster`), scored across a
fully crossed `3 prompts x 2 models` LLM coding design (`720` rows
total). A random validation subset of 50 entries carries human
gold-standard ratings (`human_score`), while the remaining 70 entries
are unlabeled (`NA`), making `ema_coded` ready for demonstrating
[`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md),
[`d_study()`](https://marcusharrisphd.com/llmmetric/reference/d_study.md),
[`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md),
[`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md),
[`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md),
and
[`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md).

## Usage

``` r
ema_coded
```

## Format

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with 720 rows and 10 variables:

- unit_id:

  Character unique diary entry identifier (`"u_0001"`..`"u_0120"`).

- cluster:

  Character participant identifier (`"cl_001"`..`"cl_030"`).

- prompt:

  Character prompt template ID (`"direct"`, `"rubric"`, `"cot"`).

- model:

  Character LLM model ID (`"model_a"`, `"model_b"`).

- temperature:

  Numeric sampling temperature (`0`).

- replicate:

  Integer replicate index (`1L`).

- llm_score:

  Numeric LLM-generated emotional distress rating.

- human_score:

  Numeric human gold-standard distress rating (observed on 50 validation
  entries; `NA` on 70 unlabeled entries).

- group:

  Factor demographic/experimental subgroup (`"group_a"`, `"group_b"`),
  with mild uniform DPF injected for `group_b`.

- x:

  Numeric standardized daily stressor predictor.

## Source

Deterministically generated via
[`simulate_coding_study()`](https://marcusharrisphd.com/llmmetric/reference/simulate_coding_study.md).

## Examples

``` r
data(ema_coded)
g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
#> 
#> ── Generalizability Study (G-Study) 
#> Units (unit_id): 120 | Facets: prompt=3, model=2 | Method: REML | Balanced:
#> TRUE
#> Observed design: G (Erho^2) = 0.951 | Phi = 0.944 | SEM(rel) = 0.298, SEM(abs)
#> = 0.321
#> Single-facet (n=1): G = 0.842 | Phi = 0.821
#> # A tibble: 7 × 5
#>   component      variance     sd pct_total role                 
#>   <chr>             <dbl>  <dbl>     <dbl> <chr>                
#> 1 unit_id          1.74   1.32       82.1  universe             
#> 2 prompt           0.0219 0.148       1.03 absolute_only        
#> 3 model            0.005  0.0708      0.24 absolute_only        
#> 4 unit_id:prompt   0.101  0.318       4.78 relative_and_absolute
#> 5 unit_id:model    0.0525 0.229       2.48 relative_and_absolute
#> 6 prompt:model     0.0249 0.158       1.17 absolute_only        
#> 7 Residual         0.173  0.416       8.16 relative_and_absolute
```
