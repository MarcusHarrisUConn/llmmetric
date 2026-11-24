# Cross-Fitted Non-Linear Calibration for Prediction-Powered Inference

Handles non-linear LLM scoring distortions (ceiling/floor compression,
S-curve saturation, quadratic distortion, or covariate-specific
non-linear bias) by learning a flexible calibration mapping
\\\hat{m}(\hat{Y}, X) \approx Y\\ via strict \\K\\-fold cross-fitting on
the human-validated subset before applying Prediction-Powered Inference
(`PPI++`). Because every labeled observation's calibrated score is
computed strictly out-of-fold and unlabeled observations average the
\\K\\ fold-specific calibrators with exact fold-size weights, the
canonical-link PPI++ rectifier preserves exact unbiasedness and nominal
95% CI coverage while dramatically reducing standard errors relative to
both Human-Only and uncalibrated linear PPI++.

## Usage

``` r
ppi_calibrated(
  formula,
  data,
  pred_col,
  family = stats::gaussian(),
  unlabeled = NULL,
  calibrator = c("spline", "poly", "logistic"),
  df = 4L,
  folds = 5L,
  include_covariates = TRUE,
  lambda = "auto",
  conf_level = 0.95,
  seed = 1234L
)
```

## Arguments

- formula:

  Two-sided model formula (e.g., `human_y ~ x1 + x2` or `human_y ~ 1`
  for calibrated mean estimation).

- data:

  A `data.frame` containing response, predictors, and `pred_col`. When
  `unlabeled = NULL`, rows with missing response form the unlabeled set.

- pred_col:

  Name of the numeric LLM prediction column to calibrate.

- family:

  Canonical GLM family:
  [`gaussian()`](https://rdrr.io/r/stats/family.html) (default),
  [`binomial()`](https://rdrr.io/r/stats/family.html), or
  [`poisson()`](https://rdrr.io/r/stats/family.html).

- unlabeled:

  Optional separate `data.frame` of unlabeled observations.

- calibrator:

  Non-linear calibration learner:

  - `"spline"` (default): natural cubic spline
    ([`splines::ns()`](https://rdrr.io/r/splines/ns.html)) on `pred_col`
    plus optional covariates from `formula`.

  - `"poly"`: polynomial basis of degree `df` on `pred_col` plus
    covariates.

  - `"logistic"`: binomial GLM with natural cubic spline on `pred_col`
    plus covariates (ideal for binary outcomes).

  - Or a custom function
    `function(train_df, new_df, truth_col, pred_col, rhs_vars)`
    returning numeric calibrated predictions for `new_df`.

- df:

  Positive integer basis degrees of freedom (for `"spline"` /
  `"logistic"`) or polynomial degree (for `"poly"`). Default `4L`.

- folds:

  Integer number of cross-fitting folds (`>= 2L`). Default `5L`.

- include_covariates:

  Logical; if `TRUE` (default), includes main effects of RHS predictors
  from `formula` inside the calibration model so covariate-specific bias
  (DPF) is removed before PPI++.

- lambda:

  Power-tuning parameter passed to PPI++: `"auto"` (default) or numeric
  scalar in `[0, 1]`.

- conf_level:

  Confidence level in `(0, 1)`. Default `0.95`.

- seed:

  Integer random seed for deterministic fold assignment (preserves
  global `.Random.seed`). Default `1234L`.

## Value

An S3 object of class `c("llm_ppi_calibrated", "llm_ppi")` containing
all standard `llm_ppi` components (with `$benchmarks` comparing
`ppi++_calibrated`, `ppi++_uncalibrated`, `human_only`, and `naive_llm`)
plus `$calibration_diagnostics` (`r_raw`, `r_calibrated_oof`,
`rmse_raw`, `rmse_calibrated_oof`, `folds`, `calibrator`).

## Examples

``` r
set.seed(42)
n_tot <- 800
x <- rnorm(n_tot)
y_true <- 1.0 + 1.2 * x + rnorm(n_tot, sd = 0.8)
# Severely non-linear S-curve compressed LLM scores
y_llm <- 5 * plogis(1.4 * (y_true - 1.0) + 0.5 * x) + rnorm(n_tot, sd = 0.15)
df <- tibble::tibble(
  x = x,
  human_y = c(y_true[1:140], rep(NA_real_, n_tot - 140)),
  llm_y = y_llm
)
fit <- ppi_calibrated(human_y ~ x, data = df, pred_col = "llm_y")
fit
#> 
#> ── Prediction-Powered Inference (ppi++_calibrated) 
#> Labeled n = 140 | Unlabeled N = 660 | lambda = 0.674 | CI = 95%
#> Cross-fitted calibration (spline, 5-fold, df=4): r 0.940 -> 0.957 | RMSE 1.634
#> -> 0.443
#> # A tibble: 2 × 7
#>   term        estimate std_error conf_low conf_high p_value var_gain
#>   <chr>          <dbl>     <dbl>    <dbl>     <dbl> <chr>      <dbl>
#> 1 (Intercept)    0.982    0.0466    0.890      1.07 <0.0001     2.29
#> 2 x              1.27     0.065     1.14       1.39 <0.0001     1.07
```
