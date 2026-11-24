# Plot Prediction-Powered Inference vs. Human-Only & Naive LLM Benchmarks

Produces a publication-ready forest plot comparing parameter point
estimates and confidence intervals across **Naive LLM**, **Human-Only**,
and **PPI / PPI++**, optionally overlaying true simulation values.

## Usage

``` r
# S3 method for class 'llm_ppi'
autoplot(
  object,
  terms = NULL,
  include_intercept = FALSE,
  true_values = NULL,
  ...
)
```

## Arguments

- object:

  An `"llm_ppi"` object returned by
  [`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md),
  [`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md),
  [`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md),
  [`ppi_clustered()`](https://marcusharrisphd.com/llmmetric/reference/ppi_clustered.md),
  or
  [`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md).

- terms:

  Optional character vector restricting displayed model terms.

- include_intercept:

  Logical; whether to display `"(Intercept)"` when other regression
  predictors are present. Default `FALSE`.

- true_values:

  Optional named numeric vector of ground-truth parameter values to
  overlay as vertical dashed reference lines (useful for simulation
  diagnostics).

- ...:

  Unused; included for S3 consistency.

## Value

A
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
set.seed(123)
n_tot <- 500
x <- rnorm(n_tot)
y_true <- 1 + 1.5 * x + rnorm(n_tot)
y_llm <- 0.5 + 0.8 * y_true + 0.5 * x + rnorm(n_tot, sd = 0.5)
df <- tibble::tibble(
  x = x,
  human_y = c(y_true[1:100], rep(NA_real_, n_tot - 100)),
  llm_y = y_llm
)
fit <- ppi_lm(human_y ~ x, data = df, pred_col = "llm_y")
autoplot(fit)
```
