# Prediction-Powered Category Proportion & Prevalence Estimation (PPI / PPI++)

Estimates population class prevalences \\\pi_c = \mathbb{P}(Y = c)\\ for
binary or multi-class categorical content coding by combining a
human-validated subset (`truth_col` observed) with a larger LLM-coded
unlabeled corpus (`pred_col` only). Computes class-optimal PPI++ weights
\\\hat{\lambda}\_c^\*\\, the full joint \\K \times K\\ asymptotic
covariance matrix, marginal or family-wise simultaneous confidence
intervals, and direct Human-Only and Naive-LLM benchmarks.

## Usage

``` r
ppi_proportion(
  data,
  truth_col = "human_label",
  pred_col = "llm_label",
  unlabeled = NULL,
  levels = NULL,
  lambda = "auto",
  conf_level = 0.95,
  simultaneous = FALSE,
  clamp_ci = TRUE,
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

  Name of the human gold-standard categorical/binary column (factor,
  character, logical, or integer/numeric labels).

- pred_col:

  Either a single column name of discrete LLM class labels (or binary
  predicted probabilities in `[0, 1]` when `truth_col` is binary `0/1`
  or logical), OR a named character vector mapping category levels to
  LLM predicted-probability column names (e.g.,
  `c(negative = "p_neg", neutral = "p_neu", positive = "p_pos")`).

- unlabeled:

  Optional separate `data.frame` of unlabeled observations.

- levels:

  Optional character vector specifying the ordered category levels to
  estimate. If `NULL`, inferred from `truth_col` and `pred_col`.

- lambda:

  Power-tuning parameter: `"auto"` (default: class-optimal PPI++), `1`
  (standard PPI), `0` (human-only), or a finite numeric scalar.

- conf_level:

  Confidence level in `(0, 1)`. Default `0.95`.

- simultaneous:

  Logical; if `TRUE`, computes family-wise simultaneous confidence
  intervals across all \\K\\ categories using Bonferroni multiplicity
  control (\\\alpha / K\\). Default `FALSE` (marginal CIs).

- clamp_ci:

  Logical; if `TRUE` (default), clamps confidence interval endpoints to
  `[0, 1]` while preserving exact unclipped standard errors.

- clip_lambda:

  Logical; if `TRUE`, clips auto-tuned \\\hat{\lambda}\_c^\*\\ to
  `[0, 1]`. Default `FALSE`.

## Value

An S3 object of class `c("llm_ppi_proportion", "llm_ppi")` compatible
with [`coef()`](https://rdrr.io/r/stats/coef.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
and
[`format_apa_table()`](https://marcusharrisphd.com/llmmetric/reference/format_apa_table.md).

## Examples

``` r
set.seed(42)
n_tot <- 800
lvls <- c("low", "moderate", "high")
y_true <- factor(sample(lvls, n_tot, replace = TRUE, prob = c(0.50, 0.30, 0.20)), levels = lvls)
# Biased LLM over-predicts "high" when true label is "moderate"
y_llm <- y_true
flip <- (y_true == "moderate") & (runif(n_tot) < 0.45)
y_llm[flip] <- "high"
df <- tibble::tibble(
  human_label = factor(
    c(as.character(y_true[1:150]), rep(NA_character_, n_tot - 150)),
    levels = lvls
  ),
  llm_label = y_llm
)
fit <- ppi_proportion(df, truth_col = "human_label", pred_col = "llm_label")
fit
#> 
#> ── Prediction-Powered Inference (ppi++) 
#> Labeled n = 150 | Unlabeled N = 650 | lambda = [0.81, 0.69, 0.49] | CI = 95%
#> # A tibble: 3 × 7
#>   term     estimate std_error conf_low conf_high p_value var_gain
#>   <chr>       <dbl>     <dbl>    <dbl>     <dbl> <chr>      <dbl>
#> 1 low         0.524    0.0176    0.489     0.558 <0.0001     5.28
#> 2 moderate    0.307    0.031     0.247     0.368 <0.0001     1.59
#> 3 high        0.199    0.0271    0.146     0.252 <0.0001     1.63
```
