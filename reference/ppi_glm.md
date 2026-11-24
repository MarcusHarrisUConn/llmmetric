# Prediction-Powered Generalized Linear Model (PPI / PPI++ / Calibrated)

Fits an error-corrected canonical-link GLM (`gaussian`, `binomial`, or
`poisson`) combining a human-validated subset with a larger LLM-coded
unlabeled corpus via Angelopoulos et al. (2024) M-estimation PPI/PPI++
or coordinate-wise control-variate calibration.

## Usage

``` r
ppi_glm(
  formula,
  data,
  pred_col,
  family = stats::binomial(),
  unlabeled = NULL,
  method = c("ppi++", "ppi", "calibrated"),
  lambda = NULL,
  conf_level = 0.95,
  max_iter = 60L,
  tol = 1e-09
)
```

## Arguments

- formula:

  Two-sided model formula `y ~ x1 + x2`.

- data:

  A `data.frame` containing the response, predictors, and LLM prediction
  column. When `unlabeled = NULL`, rows with missing response form the
  unlabeled set.

- pred_col:

  Name of the LLM prediction column (numeric; for binomial models may be
  binary `{0, 1}` or predicted probabilities in `[0, 1]`).

- family:

  Canonical GLM family:
  [`gaussian()`](https://rdrr.io/r/stats/family.html),
  [`binomial()`](https://rdrr.io/r/stats/family.html), or
  [`poisson()`](https://rdrr.io/r/stats/family.html) (or matching string
  name). Default
  [`stats::binomial()`](https://rdrr.io/r/stats/family.html).

- unlabeled:

  Optional separate `data.frame` of unlabeled observations.

- method:

  Estimation method: `"ppi++"` (default, optimal power-tuning
  \\\hat{\lambda}^\* \in \[0, 1\]\\), `"ppi"` (`lambda = 1`), or
  `"calibrated"` (coordinate-wise influence-function calibration).

- lambda:

  Optional numeric override for scalar `lambda` in `[0, 1]`.

- conf_level:

  Confidence level in `(0, 1)`. Default `0.95`.

- max_iter:

  Maximum Newton-Raphson iterations. Default `60L`.

- tol:

  Convergence tolerance for gradient/step norm. Default `1e-9`.

## Value

An S3 object of class `c("llm_ppi_glm", "llm_ppi")`.

## Examples

``` r
set.seed(7)
n_tot <- 800
x <- rnorm(n_tot)
p_true <- plogis(-0.3 + 1.1 * x)
y_true <- rbinom(n_tot, 1, p_true)
y_llm <- plogis(-0.3 + 1.1 * x + 0.7 + rnorm(n_tot, sd = 0.6))
df <- tibble::tibble(
  x = x,
  human_y = c(y_true[1:150], rep(NA_real_, n_tot - 150)),
  llm_prob = y_llm
)
fit <- ppi_glm(human_y ~ x, data = df, pred_col = "llm_prob", family = binomial())
fit
#> 
#> ── Prediction-Powered Inference (ppi++) 
#> Labeled n = 150 | Unlabeled N = 650 | lambda = 0.162 | CI = 95%
#> # A tibble: 2 × 7
#>   term        estimate std_error conf_low conf_high p_value var_gain
#>   <chr>          <dbl>     <dbl>    <dbl>     <dbl> <chr>      <dbl>
#> 1 (Intercept)   -0.253     0.186   -0.619     0.112 0.1748      1.02
#> 2 x              1.06      0.228    0.612     1.51  <0.0001     1.06
```
