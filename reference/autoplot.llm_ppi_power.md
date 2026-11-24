# Plot PPI++ Power or Standard Error Curves

Visualizes projected PPI++ statistical power (when `effect` was
supplied) or standard error reduction across human validation sample
sizes `n_labeled`, directly compared against human-only estimation.

## Usage

``` r
# S3 method for class 'llm_ppi_power'
autoplot(object, metric = c("auto", "power", "se"), ...)
```

## Arguments

- object:

  An `"llm_ppi_power"` object returned by
  [`ppi_power()`](https://marcusharrisphd.com/llmmetric/reference/ppi_power.md).

- metric:

  `"auto"` (plots power when `effect` is available, else SE), `"power"`,
  or `"se"`.

- ...:

  Unused; included for S3 consistency.

## Value

A
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
pw <- ppi_power(
  n_labeled = seq(40, 200, by = 40),
  n_total = 1500,
  effect = 0.25,
  sd = 1.2,
  r = c(0.6, 0.8)
)
autoplot(pw)
```
