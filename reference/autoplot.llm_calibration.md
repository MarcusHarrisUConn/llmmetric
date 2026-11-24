# Plot LLM-vs-Human Calibration Diagnostics

For continuous ratings, plots raw pairs, quantile-binned calibration
means (\\\pm 1.96\\\text{SE}\\), linear calibration fit, and the ideal
45-degree identity line. For categorical/ordinal ratings, renders an
annotated confusion matrix heatmap.

## Usage

``` r
# S3 method for class 'llm_calibration'
autoplot(object, ...)
```

## Arguments

- object:

  An `"llm_calibration"` object returned by
  [`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md).

- ...:

  Unused; included for S3 consistency.

## Value

A
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
data(ema_coded)
agg <- aggregate_scores(ema_coded, score_col = "llm_score")
cal <- calibration_metrics(agg, pred_col = "llm_score", truth_col = "human_score")
autoplot(cal)
```
