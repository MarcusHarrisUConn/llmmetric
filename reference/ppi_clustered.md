# Cluster-Robust Prediction-Powered Inference (PPI / PPI++)

Extends Prediction-Powered Inference (`gaussian`, `binomial`, and
`poisson` canonical-link GLMs) to clustered research designs (e.g.,
repeated EMA diary entries within participants, therapy turns within
patients, or students nested in classrooms). Automatically handles both
**disjoint cluster validation** (distinct clusters in labeled vs.
unlabeled sets) and **within-cluster validation** (shared clusters
containing both labeled and unlabeled observations), computing exact
cluster-optimal power-tuning \\\hat{\lambda}^\*\_{\text{cluster}}\\ and
cluster-robust sandwich standard errors.

## Usage

``` r
ppi_clustered(
  formula,
  data,
  pred_col,
  cluster,
  family = stats::gaussian(),
  unlabeled = NULL,
  method = c("ppi++", "ppi"),
  lambda = NULL,
  conf_level = 0.95,
  small_sample_df = TRUE,
  max_iter = 60L,
  tol = 1e-09
)
```

## Arguments

- formula:

  Two-sided regression formula `y ~ x1 + x2`.

- data:

  A `data.frame` containing response, covariates, `pred_col`, and
  `cluster`. When `unlabeled = NULL`, rows with missing response form
  the unlabeled set.

- pred_col:

  Name of the LLM-coded outcome column.

- cluster:

  Name of the cluster identifier column.

- family:

  Canonical GLM family:
  [`gaussian()`](https://rdrr.io/r/stats/family.html) (default),
  [`binomial()`](https://rdrr.io/r/stats/family.html), or
  [`poisson()`](https://rdrr.io/r/stats/family.html).

- unlabeled:

  Optional separate `data.frame` of unlabeled observations.

- method:

  `"ppi++"` (default, cluster-optimal power-tuning) or `"ppi"`
  (`lambda = 1`).

- lambda:

  Optional numeric override for `lambda` in `[0, 1]`.

- conf_level:

  Confidence level in `(0, 1)`. Default `0.95`.

- small_sample_df:

  Logical; if `TRUE` (default), uses a \\t\\ reference distribution with
  \\G\_{\text{eff}} - 1\\ degrees of freedom for small-cluster
  inference.

- max_iter:

  Maximum Newton-Raphson iterations for non-Gaussian families.

- tol:

  Convergence tolerance.

## Value

An S3 object of class `c("llm_ppi_clustered", "llm_ppi")`.

## Examples

``` r
set.seed(99)
G <- 60
m <- 6
cid <- rep(paste0("c", seq_len(G)), each = m)
u <- rep(rnorm(G, sd = 0.8), each = m)
x <- rnorm(G * m)
y_true <- 1.0 + 1.2 * x + u + rnorm(G * m, sd = 0.7)
y_llm <- 0.5 + 0.85 * y_true + 0.35 * x + rnorm(G * m, sd = 0.4)
is_val <- cid %in% paste0("c", 1:25)
df <- tibble::tibble(
  cluster = cid,
  x = x,
  human_y = ifelse(is_val, y_true, NA_real_),
  llm_y = y_llm
)
fit <- ppi_clustered(human_y ~ x, data = df, pred_col = "llm_y", cluster = "cluster")
fit
#> 
#> ── Prediction-Powered Inference (ppi++_clustered) 
#> Labeled n = 150 | Unlabeled N = 210 | lambda = 0.630 | CI = 95%
#> Clusters (cluster): 25 labeled, 35 unlabeled (60 total; disjoint)
#> # A tibble: 2 × 7
#>   term        estimate std_error conf_low conf_high p_value var_gain
#>   <chr>          <dbl>     <dbl>    <dbl>     <dbl> <chr>      <dbl>
#> 1 (Intercept)    0.758    0.117     0.516      1.00 <0.0001     2.52
#> 2 x              1.29     0.0561    1.18       1.41 <0.0001     2.85
```
