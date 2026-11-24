# Format Publication-Ready APA Comparison Tables

Produces clean APA 7th-edition formatted summary tables for `llmmetric`
results. For any Prediction-Powered Inference fit
([`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md),
[`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md),
[`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md),
[`ppi_clustered()`](https://marcusharrisphd.com/llmmetric/reference/ppi_clustered.md),
[`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md),
[`ppi_proportion()`](https://marcusharrisphd.com/llmmetric/reference/ppi_proportion.md)),
contrasts **Naive LLM**, **Human-Only**, and **PPI / PPI++** point
estimates, standard errors, confidence intervals, APA-formatted p-values
(no leading zero, `< .001`), and effective sample-size / variance gain
vs. Human-Only. Also supports
[`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md),
[`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md),
and
[`human_irr()`](https://marcusharrisphd.com/llmmetric/reference/human_irr.md)
objects.

## Usage

``` r
format_apa_table(
  object,
  digits = 3L,
  format = c("markdown", "tibble", "latex"),
  terms = NULL,
  include_intercept = TRUE,
  caption = NULL,
  note = NULL
)
```

## Arguments

- object:

  An `"llm_ppi"`, `"llm_g_study"`, `"llm_dpf"`, or `"llm_human_irr"`
  object.

- digits:

  Number of decimal places for numeric estimates and CIs. Default `3L`.

- format:

  Output format: `"markdown"` (default, GitHub-Flavored Markdown table
  ready for Quarto/RMarkdown), `"tibble"` (tidy character/numeric
  tibble), or `"latex"` (booktabs-compatible LaTeX tabular).

- terms:

  Optional character vector restricting displayed terms (for `"llm_ppi"`
  objects).

- include_intercept:

  Logical; whether to retain `"(Intercept)"` in regression tables.
  Default `TRUE`.

- caption:

  Optional character table caption.

- note:

  Optional APA table note appended below Markdown/LaTeX output.

## Value

An S3 object of class `"llm_apa_table"` with components:

- `table`: tidy
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  of formatted table cells.

- `formatted`: rendered Markdown or LaTeX character string (or the
  tibble when `format = "tibble"`).

- `format`, `caption`, `note`.

## Examples

``` r
data(ema_coded)
agg <- aggregate_scores(ema_coded, score_col = "llm_score")
fit <- ppi_lm(human_score ~ x + group, data = agg, pred_col = "llm_score")
tab <- format_apa_table(fit)
tab
#> **Comparison of Naive LLM, Human-Only, and PPI++ Parameter Estimates**
#> 
#> | Term | Estimator | Estimate | SE | 95% CI | p | Eff. Gain |
#> | --- | --- | --- | --- | --- | --- | --- |
#> | (Intercept) | Naive LLM | 2.338 | 0.099 | [2.144, 2.532] | &lt; .001 | -- |
#> | (Intercept) | Human-Only | 2.527 | 0.177 | [2.179, 2.875] | &lt; .001 | 1.00x |
#> | (Intercept) | PPI++ | 2.500 | 0.127 | [2.251, 2.749] | &lt; .001 | 1.95x |
#> | x | Naive LLM | 0.883 | 0.063 | [0.760, 1.005] | &lt; .001 | -- |
#> | x | Human-Only | 1.043 | 0.165 | [0.720, 1.365] | &lt; .001 | 1.00x |
#> | x | PPI++ | 1.028 | 0.085 | [0.861, 1.194] | &lt; .001 | 3.75x |
#> | groupgroup_b | Naive LLM | 1.296 | 0.145 | [1.012, 1.579] | &lt; .001 | -- |
#> | groupgroup_b | Human-Only | 0.579 | 0.281 | [0.028, 1.131] | .040 | 1.00x |
#> | groupgroup_b | PPI++ | 0.716 | 0.183 | [0.357, 1.075] | &lt; .001 | 2.36x |
#> 
#> *Note.* Labeled validation n = 50; unlabeled LLM N = 70; power-tuning lambda = 0.664. Eff. Gain denotes variance reduction (effective sample-size multiplier) relative to Human-Only.
```
