# Prediction-Powered Mean Estimation (PPI / PPI++)

Estimates the population mean \\\theta^\* = \mathbb{E}\[Y\]\\ by
combining a human-validated sample (`truth_col` observed) with a larger
LLM-coded unlabeled sample (`pred_col` only). Implements both standard
PPI (`lambda = 1`) and power-tuned PPI++ (`lambda = "auto"`, minimizing
asymptotic variance via the exact optimal control-variate weight),
alongside human-only and naive-LLM benchmarks.

## Usage

``` r
ppi_mean(
  data,
  truth_col = "human_score",
  pred_col = "llm_score",
  unlabeled = NULL,
  lambda = "auto",
  conf_level = 0.95,
  clip_lambda = FALSE
)
```

## Arguments

- data:

  A `data.frame` (or `llm_coded_data`) containing `truth_col` and
  `pred_col`. When `unlabeled = NULL`, rows with non-missing `truth_col`
  form the labeled validation set and rows with `is.na(truth_col)` form
  the unlabeled set.

- truth_col:

  Name of the numeric human gold-standard column.

- pred_col:

  Name of the numeric LLM prediction column.

- unlabeled:

  Optional separate `data.frame` of unlabeled observations containing
  `pred_col`.

- lambda:

  Power-tuning parameter: `"auto"` (default, optimal PPI++), `1`
  (standard PPI), `0` (human-only), or any finite numeric scalar.

- conf_level:

  Confidence level in `(0, 1)`. Default `0.95`.

- clip_lambda:

  Logical; if `TRUE`, constrains auto-tuned `lambda` to `[0, 1]`.
  Default `FALSE` (unconstrained optimal regression control variate is
  always convex for mean estimation).

## Value

An S3 object of class `c("llm_ppi_mean", "llm_ppi")` containing
`$estimates`, `$benchmarks`, `$efficiency`, `$lambda`, `$vcov`, and S3
methods ([`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).

## Examples

``` r
set.seed(42)
n_tot <- 600
y_true <- rnorm(n_tot, mean = 3.5, sd = 1.2)
y_llm <- 0.8 + 0.75 * y_true + rnorm(n_tot, sd = 0.6)
df <- tibble::tibble(
  human_score = c(y_true[1:100], rep(NA_real_, n_tot - 100)),
  llm_score = y_llm
)
fit <- ppi_mean(df, truth_col = "human_score", pred_col = "llm_score")
fit
#> 
#> ── Prediction-Powered Inference (ppi++) 
#> Labeled n = 100 | Unlabeled N = 500 | lambda = 0.814 | CI = 95%
#> # A tibble: 1 × 7
#>   term   estimate std_error conf_low conf_high p_value var_gain
#>   <chr>     <dbl>     <dbl>    <dbl>     <dbl> <chr>      <dbl>
#> 1 (Mean)     3.52    0.0789     3.37      3.67 <0.0001     2.51
```
