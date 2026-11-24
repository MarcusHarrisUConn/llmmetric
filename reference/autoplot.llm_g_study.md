# Plot Generalizability Study Variance Decomposition

Visualizes estimated G-study variance components as percentages of total
score variance, colored by their psychometric error role (Universe
Score, Relative & Absolute Error, or Absolute Error Only).

## Usage

``` r
# S3 method for class 'llm_g_study'
autoplot(object, ...)
```

## Arguments

- object:

  An `"llm_g_study"` object returned by
  [`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md).

- ...:

  Unused; included for S3 consistency.

## Value

A
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
data(ema_coded)
g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
autoplot(g)
```
