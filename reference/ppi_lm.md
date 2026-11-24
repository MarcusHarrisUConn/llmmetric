# Prediction-Powered Linear Regression (PPI / PPI++ / Calibrated Control Variate)

Fits an error-corrected linear regression model combining a
human-validated subset with a larger LLM-coded dataset. Supports:

- `"ppi++"` (default): Angelopoulos et al. (2024) M-estimation PPI++
  with optimal power-tuning \\\hat{\lambda}^\* \in \[0, 1\]\\.

- `"ppi"`: standard Prediction-Powered Inference (`lambda = 1`).

- `"calibrated"`: coordinate-wise influence-function control-variate
  calibration \\\hat{\theta}\_L +
  \hat{\Lambda}^\*(\hat{\theta}\_{\tilde{U}} -
  \hat{\theta}\_{\tilde{L}})\\, which adapts to slope attenuation per
  coefficient and also supports LLM-coded predictors via `pred_map`.

## Usage

``` r
ppi_lm(
  formula,
  data,
  pred_col = NULL,
  unlabeled = NULL,
  method = c("ppi++", "ppi", "calibrated"),
  lambda = NULL,
  conf_level = 0.95,
  pred_map = NULL
)
```

## Arguments

- formula:

  Two-sided linear model formula `y ~ x1 + x2`.

- data:

  A `data.frame` containing the response, predictors, and LLM prediction
  column(s). When `unlabeled = NULL`, rows with missing response are
  treated as the unlabeled set.

- pred_col:

  Name of the LLM-coded column approximating the response variable in
  `formula`. Optional if `pred_map` is supplied.

- unlabeled:

  Optional separate `data.frame` of unlabeled observations.

- method:

  Estimation method: `"ppi++"` (default), `"ppi"`, or `"calibrated"`.

- lambda:

  Optional numeric override for the scalar power-tuning parameter (for
  `"ppi++"` or `"ppi"`).

- conf_level:

  Confidence level in `(0, 1)`. Default `0.95`.

- pred_map:

  Optional named character vector mapping human-coded variable names in
  `formula` to their LLM-coded surrogate column names (e.g.,
  `c(distress = "llm_distress")` when an LLM codes a predictor on the
  RHS). Automatically uses `method = "calibrated"` when RHS predictors
  are mapped.

## Value

An S3 object of class `c("llm_ppi_lm", "llm_ppi")`.

## Examples

``` r
set.seed(123)
n_tot <- 800
x <- rnorm(n_tot)
y_true <- 1.0 + 1.5 * x + rnorm(n_tot, sd = 1.0)
y_llm <- 0.6 + 0.9 * y_true + 0.4 * x + rnorm(n_tot, sd = 0.5)
df <- tibble::tibble(
  x = x,
  human_y = c(y_true[1:120], rep(NA_real_, n_tot - 120)),
  llm_y = y_llm
)
fit <- ppi_lm(human_y ~ x, data = df, pred_col = "llm_y")
fit
#> 
#> ── Prediction-Powered Inference (ppi++) 
#> Labeled n = 120 | Unlabeled N = 680 | lambda = 0.589 | CI = 95%
#> # A tibble: 2 × 7
#>   term        estimate std_error conf_low conf_high p_value var_gain
#>   <chr>          <dbl>     <dbl>    <dbl>     <dbl> <chr>      <dbl>
#> 1 (Intercept)     1.03    0.0619    0.904      1.15 <0.0001     2.52
#> 2 x               1.50    0.0616    1.38       1.63 <0.0001     2.77
```
