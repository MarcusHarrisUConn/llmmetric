# Decision Study (D-Study) Reliability Projections & Optimal Design Solver

Projects Generalizability (\\E\rho^2\\) and Dependability (\\\Phi\\)
coefficients across candidate facet counts (e.g., grids of
`n_prompts x n_models`), computes single-facet reliability asymptotes,
and identifies minimum-cost designs achieving a target reliability
threshold.

## Usage

``` r
d_study(
  g_study,
  grid = NULL,
  target = 0.85,
  metric = c("g", "phi"),
  n_replicates = 1L,
  costs = NULL
)
```

## Arguments

- g_study:

  An object of class `"llm_g_study"` returned by
  [`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md).

- grid:

  Optional named list of positive integer vectors giving candidate
  counts for each facet in `g_study$facets` (e.g.
  `list(prompt = 1:8, model = 1:4)`). Missing facets default to sensible
  candidate sequences.

- target:

  Target reliability threshold in `(0, 1)`. Default `0.85`.

- metric:

  Reliability coefficient used for target recommendations: `"g"`
  (relative \\E\rho^2\\, default) or `"phi"` (absolute \\\Phi\\).

- n_replicates:

  Positive integer replicate count per cell in the D-study projection.
  Defaults to `1L`.

- costs:

  Optional named numeric vector of relative cost weights per unit
  increase in each facet (e.g. `c(prompt = 1, model = 2)`). If `NULL`,
  total LLM calls (`prod(n_facets) * n_replicates`) is minimized.

## Value

An S3 object of class `"llm_d_study"` with elements:

- `grid`: tibble of all evaluated D-study designs with facet counts,
  `total_calls`, `cost`, `g_coef`, `phi_coef`, `sem_relative`,
  `sem_absolute`, and `meets_target`.

- `recommendations`: tibble of Pareto-minimal / lowest-cost designs
  satisfying `metric >= target` (empty tibble if unattainable in
  `grid`).

- `asymptotes`: tibble giving theoretical ceiling \\E\rho^2\\ and
  \\\Phi\\ when each facet tends to infinity while other facets stay at
  1 or at their observed G-study counts.

- `target`, `metric`, `facets`, `n_replicates`, `g_study`.

## Examples

``` r
data(ema_coded)
g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
d <- d_study(g, grid = list(prompt = 1:8, model = 1:4), target = 0.85)
d
#> 
#> ── Decision Study (D-Study) Projections 
#> Evaluated 32 designs | Target: G (Erho^2) >= 0.85
#> Lowest-cost recommended design: prompt=2, model=1 (2 calls/unit -> G = 0.902,
#> Phi = 0.889)
#> # A tibble: 2 × 10
#>   prompt model n_replicates total_calls  cost g_coef phi_coef sem_relative
#>    <int> <int>        <int>       <dbl> <dbl>  <dbl>    <dbl>        <dbl>
#> 1      2     1            1           2     2  0.902    0.889        0.436
#> 2      1     2            1           2     2  0.890    0.874        0.463
#> # ℹ 2 more variables: sem_absolute <dbl>, meets_target <lgl>
```
