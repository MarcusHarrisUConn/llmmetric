# Plot Temporal or Model-Version Scoring Drift

Plot Temporal or Model-Version Scoring Drift

## Usage

``` r
# S3 method for class 'llm_drift'
autoplot(object, type = c("diff", "trajectory"), ...)
```

## Arguments

- object:

  An `"llm_drift"` object returned by
  [`check_drift()`](https://marcusharrisphd.com/llmmetric/reference/check_drift.md).

- type:

  `"diff"` (default: paired mean shift vs. baseline with 95% CIs) or
  `"trajectory"` (mean score across versions \\\pm 1.96\\\text{SE}\\).

- ...:

  Unused; included for S3 consistency.

## Value

A
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
set.seed(1)
anchor <- tibble::tibble(
  unit_id = rep(paste0("u", 1:30), 3),
  version = rep(c("v1", "v2", "v3"), each = 30),
  base = rep(rnorm(30), 3),
  score = base + rep(c(0, 0.05, 0.5), each = 30) + rnorm(90, 0, 0.25)
)
dr <- check_drift(anchor, score_col = "score", version_col = "version")
autoplot(dr)
```
