# Plot Human Inter-Rater Reliability Ceiling vs. LLM Calibration

Visualizes human-vs-human inter-rater reliability metrics, and when an
`llm_calibration` benchmark was supplied to
[`human_irr()`](https://marcusharrisphd.com/llmmetric/reference/human_irr.md),
plots side-by-side bars comparing LLM-human agreement directly against
the human inter-rater ceiling.

## Usage

``` r
# S3 method for class 'llm_human_irr'
autoplot(object, ...)
```

## Arguments

- object:

  An `"llm_human_irr"` object returned by
  [`human_irr()`](https://marcusharrisphd.com/llmmetric/reference/human_irr.md).

- ...:

  Unused; included for S3 consistency.

## Value

A
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
set.seed(10)
z <- rnorm(50, mean = 4, sd = 1.2)
df <- tibble::tibble(
  h1 = z + rnorm(50, sd = 0.4),
  h2 = z + rnorm(50, sd = 0.45),
  llm = 0.1 + 0.9 * z + rnorm(50, sd = 0.5)
)
cal <- calibration_metrics(df, pred_col = "llm", truth_col = "h1")
irr <- human_irr(df, rater_cols = c("h1", "h2"), llm_calibration = cal)
autoplot(irr)
```
