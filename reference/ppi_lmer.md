# Multilevel Random-Intercept Prediction-Powered Inference (`ppi_lmer`)

Combines a multilevel random-intercept linear mixed model
([`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html)) with
Prediction-Powered Inference (PPI / PPI++). Accepts either an
`lme4`-style random-intercept formula (`y ~ x1 + x2 + (1 | cluster)`) or
a fixed-effects formula (`y ~ x1 + x2`) plus `cluster`. Estimates
variance components (\\\tau^2\\, \\\sigma^2\\, ICC) via REML on the
human-validated set, forms exact Woodbury random-intercept GLS weights
(when clusters are disjoint across labeled and unlabeled sets, or
total-cluster inverse-variance weights when clusters overlap, preserving
exact unbiasedness under arbitrary LLM bias), and computes
cluster-robust PPI++ sandwich standard errors alongside human-only and
naive-LLM multilevel benchmarks.

## Usage

``` r
ppi_lmer(
  formula,
  data,
  pred_col,
  cluster = NULL,
  unlabeled = NULL,
  method = c("ppi++", "ppi"),
  lambda = NULL,
  conf_level = 0.95,
  weight_mode = c("gls", "ols"),
  small_sample_df = TRUE
)
```

## Arguments

- formula:

  Either a random-intercept formula `y ~ x1 + x2 + (1 | grp)` or a
  fixed-effects formula `y ~ x1 + x2` paired with `cluster`.

- data:

  A `data.frame` containing response, covariates, `pred_col`, and
  cluster IDs. When `unlabeled = NULL`, rows with missing response form
  the unlabeled set.

- pred_col:

  Name of the LLM-coded outcome column.

- cluster:

  Optional cluster column name (inferred from `(1 | cluster)` in
  `formula` if omitted).

- unlabeled:

  Optional separate `data.frame` of unlabeled observations.

- method:

  `"ppi++"` (default) or `"ppi"` (`lambda = 1`).

- lambda:

  Optional numeric override for `lambda` in `[0, 1]`.

- conf_level:

  Confidence level in `(0, 1)`. Default `0.95`.

- weight_mode:

  `"gls"` (default, REML random-intercept GLS weighting) or `"ols"`
  (unweighted cluster-robust PPI++).

- small_sample_df:

  Logical; use \\t\\ degrees of freedom based on effective cluster
  count. Default `TRUE`.

## Value

An S3 object of class
`c("llm_ppi_lmer", "llm_ppi_clustered", "llm_ppi")` with
`$variance_components` (human vs. naive-LLM \\\tau^2\\, \\\sigma^2\\,
ICC) in addition to standard `llm_ppi` components.

## Examples

``` r
set.seed(11)
G <- 50
m <- 5
cid <- rep(paste0("p", seq_len(G)), each = m)
u <- rep(rnorm(G, sd = 0.9), each = m)
x <- rnorm(G * m)
y_true <- 2.0 + 1.0 * x + u + rnorm(G * m, sd = 0.6)
y_llm <- 0.4 + 0.8 * y_true + rnorm(G * m, sd = 0.5)
df <- tibble::tibble(
  person = cid,
  x = x,
  human_y = ifelse(cid %in% paste0("p", 1:20), y_true, NA_real_),
  llm_y = y_llm
)
fit <- ppi_lmer(human_y ~ x + (1 | person), data = df, pred_col = "llm_y")
fit
#> 
#> ── Prediction-Powered Inference (ppi++_lmer) 
#> Labeled n = 100 | Unlabeled N = 150 | lambda = 0.535 | CI = 95%
#> Clusters (person): 20 labeled, 30 unlabeled (50 total; disjoint)
#> Random-intercept REML (human): tau2 = 0.526, sigma2 = 0.371, ICC = 0.586 |
#> Weighting: gls_woodbury
#> # A tibble: 2 × 7
#>   term        estimate std_error conf_low conf_high p_value var_gain
#>   <chr>          <dbl>     <dbl>    <dbl>     <dbl> <chr>      <dbl>
#> 1 (Intercept)    1.75     0.117     1.51       2.00 <0.0001     2.17
#> 2 x              0.991    0.0595    0.866      1.12 <0.0001     0.74
```
