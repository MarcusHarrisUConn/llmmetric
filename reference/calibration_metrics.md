# Calibration Metrics Comparing LLM Scores to Human Gold Standards

Evaluates agreement, bias, dispersion, and calibration between
LLM-generated ratings and human gold-standard labels. Supports
continuous ratings (Pearson \\r\\, Spearman \\\rho\\, Lin's CCC, two-way
\\\text{ICC}(A,1)\\ and \\\text{ICC}(C,1)\\, MAE, RMSE, bias,
calibration intercept/slope, and calibration-in-the-large \\F\\-test) as
well as categorical/ordinal ratings (accuracy, macro-\\F_1\\, unweighted
and weighted Cohen's \\\kappa\\, Krippendorff's \\\alpha\\, and
confusion matrix).

## Usage

``` r
calibration_metrics(
  data,
  pred_col = "score",
  truth_col = "human_score",
  type = c("auto", "continuous", "categorical", "ordinal"),
  by = NULL,
  n_bins = 10L,
  na.rm = TRUE
)
```

## Arguments

- data:

  An `llm_coded_data` object or a `data.frame`.

- pred_col:

  Name of the LLM score column.

- truth_col:

  Name of the human gold-standard score column.

- type:

  Measurement scale: `"auto"` (default: numeric -\> `"continuous"`,
  factor/character/logical -\> `"categorical"`), `"continuous"`,
  `"categorical"`, or `"ordinal"`.

- by:

  Optional character vector of grouping columns (e.g., `"model"`,
  `"prompt"`, or `"group"`) to compute stratified calibration metrics.

- n_bins:

  Number of quantile bins for continuous calibration curve summaries
  (used by
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)).
  Default `10L`.

- na.rm:

  Logical; drop rows where `pred_col` or `truth_col` is `NA`. Default
  `TRUE`.

## Value

An S3 object of class `"llm_calibration"` with elements:

- `metrics`: tibble of overall calibration statistics.

- `by_group`: tibble of stratified calibration statistics (or `NULL` if
  `by` is `NULL`).

- `calibration_bins`: binned calibration curve tibble (continuous only).

- `confusion`: confusion matrix table (categorical/ordinal only).

- `type`, `pred_col`, `truth_col`, `by`, `data`.

## Examples

``` r
data(ema_coded)
agg <- aggregate_scores(ema_coded, score_col = "llm_score")
cal <- calibration_metrics(agg, pred_col = "llm_score", truth_col = "human_score")
cal
#> 
#> ── LLM Calibration Diagnostics (continuous) 
#> Prediction: "llm_score" vs Human Gold Standard: "human_score" (n = 50)
#> r = 0.939 | rho = 0.900 | CCC = 0.932 | ICC(A,1) = 0.933
#> MAE = 0.394 | RMSE = 0.484 | Bias = 0.143 | Cal Slope = 0.987, Intercept =
#> -0.106 (p = 0.1095)
```
