# Plot Decision Study (D-Study) Reliability Curves

Visualizes projected Generalizability (\\E\rho^2\\) or Dependability
(\\\Phi\\) curves across candidate facet counts, marking the target
reliability threshold and highlighting the lowest-cost recommended
design.

## Usage

``` r
# S3 method for class 'llm_d_study'
autoplot(object, metric = NULL, ...)
```

## Arguments

- object:

  An `"llm_d_study"` object returned by
  [`d_study()`](https://marcusharrisphd.com/llmmetric/reference/d_study.md).

- metric:

  Optional override: `"g"`, `"phi"`, or `"both"`. Defaults to
  `object$metric`.

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
d <- d_study(g, grid = list(prompt = 1:8, model = 1:4), target = 0.85)
autoplot(d)
```
