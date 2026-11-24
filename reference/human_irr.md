# Human Inter-Rater Reliability (IRR) Ceiling Benchmark

Computes human-vs-human inter-rater reliability across two or more human
annotators in either **wide format**
(`rater_cols = c("h1", "h2", "h3")`) or **long format** (`id_col`,
`rater_col`, `score_col`), and optionally benchmarks an LLM's
[`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md)
directly against the human inter-rater ceiling.

## Usage

``` r
human_irr(
  data,
  rater_cols = NULL,
  id_col = NULL,
  rater_col = NULL,
  score_col = NULL,
  type = c("auto", "continuous", "categorical", "ordinal"),
  llm_calibration = NULL,
  conf_level = 0.95,
  na.rm = TRUE
)
```

## Arguments

- data:

  A `data.frame` containing human ratings in wide or long format.

- rater_cols:

  Character vector of column names for wide-format ratings (at least 2
  human rater columns). Mutually exclusive with
  `(id_col, rater_col, score_col)`.

- id_col:

  Unit identifier column name for long-format ratings.

- rater_col:

  Human rater identifier column name for long-format ratings.

- score_col:

  Score/label column name for long-format ratings.

- type:

  Measurement scale: `"auto"` (numeric -\> `"continuous"`,
  factor/character/logical -\> `"categorical"`), `"continuous"`,
  `"categorical"`, or `"ordinal"`.

- llm_calibration:

  Optional `"llm_calibration"` object returned by
  [`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md)
  to benchmark LLM-human agreement directly against the human
  inter-rater ceiling.

- conf_level:

  Confidence level for Fleiss's \\\kappa\\ CI. Default `0.95`.

- na.rm:

  Logical; drop units/rows with insufficient ratings. Default `TRUE`.

## Value

An S3 object of class `"llm_human_irr"` containing:

- `metrics`: tibble of human-vs-human reliability statistics.

- `benchmark`: tibble comparing LLM calibration against the human
  ceiling (or `NULL` if `llm_calibration` is `NULL`).

- `type`, `n_units`, `n_raters`, `balanced`, `method_used`.

## Details

- **Continuous ratings:** two-way random absolute agreement
  \\\mathrm{ICC}(A,1)\\ (single rater) and \\\mathrm{ICC}(A,k)\\
  (average of \\k\\ raters), consistency \\\mathrm{ICC}(C,1)\\ and
  \\\mathrm{ICC}(C,k)\\ (using
  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) REML
  automatically when ratings are unbalanced/missing), plus mean pairwise
  Pearson \\r\\, Spearman \\\rho\\, MAE, and RMSE.

- **Categorical / ordinal ratings:** pairwise percent agreement,
  unweighted and weighted Cohen's \\\kappa\\ (when \\k = 2\\), Fleiss's
  \\\kappa\\ (for any \\k \ge 2\\ with analytic SE and 95% CI), and
  multi-rater Krippendorff's \\\alpha\\ (nominal or ordinal distance
  metric, supporting arbitrary missing ratings).

## Examples

``` r
set.seed(10)
true_z <- rnorm(60, mean = 4, sd = 1.2)
wide_df <- tibble::tibble(
  h1 = true_z + rnorm(60, sd = 0.40),
  h2 = true_z + rnorm(60, sd = 0.45),
  h3 = true_z + rnorm(60, sd = 0.42),
  llm = 0.2 + 0.9 * true_z + rnorm(60, sd = 0.48)
)
cal <- calibration_metrics(wide_df, pred_col = "llm", truth_col = "h1")
irr <- human_irr(wide_df, rater_cols = c("h1", "h2", "h3"), llm_calibration = cal)
irr
#> 
#> ── Human Inter-Rater Reliability Ceiling (continuous) 
#> Units: 60 | Human raters: 3 | Balanced: TRUE | Method: two_way_anova
#> ICC(A,1) = 0.877 | ICC(A,k) = 0.955 | ICC(C,1) = 0.877 | Mean r = 0.880 |
#> Pairwise RMSE = 0.584
#> LLM Calibration vs. Human Inter-Rater Ceiling:
#> # A tibble: 4 × 5
#>   metric       human_ceiling llm_vs_human      gap pct_of_ceiling
#>   <chr>                <dbl>        <dbl>    <dbl>          <dbl>
#> 1 ICC(A,1)             0.877        0.844 -0.0330            96.2
#> 2 ICC(C,1)             0.877        0.850 -0.0274            96.9
#> 3 Pearson r            0.880        0.851 -0.0292            96.7
#> 4 Spearman rho         0.853        0.845 -0.00758           99.1
```
