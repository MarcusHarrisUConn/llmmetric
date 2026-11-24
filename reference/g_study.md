# Generalizability Study (G-Study) for Multi-Facet LLM Coding

Estimates crossed random-effects variance components for LLM-coded
scores (e.g., `unit`, `prompt`, `model`, `temperature`, interactions,
and residual) via REML
([`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html)) with an exact
balanced-ANOVA Expected Mean Squares fallback, and computes relative
Generalizability (\\E\rho^2\\) and absolute Dependability (\\\Phi\\)
coefficients.

## Usage

``` r
g_study(
  data,
  score_col = "score",
  id_col = "unit_id",
  facets = NULL,
  method = c("auto", "reml", "anova"),
  max_interaction = 2L,
  na.rm = TRUE
)
```

## Arguments

- data:

  An `llm_coded_data` object produced by
  [`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md)
  or a `data.frame` containing unit IDs, facet columns, and numeric
  scores.

- score_col:

  Name of the numeric score column. Defaults to the primary score column
  for `llm_coded_data`, or `"score"` otherwise.

- id_col:

  Name of the object-of-measurement (unit/person/text) column. Default
  `"unit_id"`.

- facets:

  Optional character vector of measurement facet column names (e.g.,
  `c("prompt", "model")`). If `NULL`, automatically detects which of
  `c("prompt", "model", "temperature")` exist in `data` with at least 2
  distinct levels.

- method:

  Estimation engine: `"auto"` (default: REML via
  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html), falling back
  cleanly to ANOVA/EMS if REML fails), `"reml"`, or `"anova"` (Brennan's
  Expected Mean Squares Method of Moments for balanced crossed designs).

- max_interaction:

  Maximum random-effect interaction order to estimate separately from
  residual error. Default `2L` (main effects and all pairwise
  interactions; higher-order interactions fold into residual unless
  replicates exist and `max_interaction` is increased).

- na.rm:

  Logical; remove rows with missing score or facet values before
  fitting. Default `TRUE`.

## Value

An S3 object of class `"llm_g_study"` with elements:

- `variance_components`: tibble with `component`, `variance`, `sd`,
  `pct_total`, `in_relative_error`, `in_absolute_error`,
  `facets_involved`, and (for ANOVA) `raw_variance`.

- `g_coef`, `phi_coef`: observed-design relative \\E\rho^2\\ and
  absolute \\\Phi\\ coefficients.

- `g_single`, `phi_single`: single-observation reliability (all facets =
  1).

- `sigma2_universe`, `sigma2_relative`, `sigma2_absolute`,
  `sem_relative`, `sem_absolute`.

- `n_units`, `n_facets`, `n_replicates`, `balanced`, `method_used`,
  `singular`, `facets`, `id_col`, `score_col`.

## Examples

``` r
data(ema_coded)
g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
g
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
