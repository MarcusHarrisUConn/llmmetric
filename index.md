# llmmetric

> **Psychometric reliability, measurement invariance, and
> error-corrected statistical inference for LLM-coded research data in
> R.**

Researchers across psychology, education, sociology, management, and
health sciences increasingly use large language models (LLMs) to code
open-ended survey responses, therapy transcripts, interviews, and
ecological momentary assessment (EMA) diaries into quantitative
variables. Treating LLM outputs as error-free gold-standard measurements
ignores prompt sensitivity, model drift, subgroup bias, and measurement
error—invalidating standard errors, confidence intervals, and *p*-values
in downstream analyses.

`llmmetric` bridges modern AI annotation pipelines and formal
psychometric & statistical theory by treating **LLMs explicitly as
fallible measurement instruments**.

------------------------------------------------------------------------

## Core Capabilities

| Pillar | Functions | What It Solves |
|:---|:---|:---|
| **1. Multi-Facet Structured Coding Engine** | [`coding_schema()`](https://marcusharrisphd.com/llmmetric/reference/coding_schema.md), [`schema_field()`](https://marcusharrisphd.com/llmmetric/reference/schema_field.md), [`as_json_schema()`](https://marcusharrisphd.com/llmmetric/reference/as_json_schema.md), [`validate_coded_output()`](https://marcusharrisphd.com/llmmetric/reference/validate_coded_output.md), [`coding_cache()`](https://marcusharrisphd.com/llmmetric/reference/coding_cache.md), [`coding_design()`](https://marcusharrisphd.com/llmmetric/reference/coding_design.md), [`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md), [`aggregate_scores()`](https://marcusharrisphd.com/llmmetric/reference/aggregate_scores.md), [`backend_openai()`](https://marcusharrisphd.com/llmmetric/reference/backend_openai.md), [`backend_ollama()`](https://marcusharrisphd.com/llmmetric/reference/backend_ollama.md), [`backend_custom()`](https://marcusharrisphd.com/llmmetric/reference/backend_custom.md), [`backend_mock()`](https://marcusharrisphd.com/llmmetric/reference/backend_mock.md) | Reproducible crossed coding (`unit × prompt × model × temperature × replicate`) with strict hardened JSON validation, resumable atomic checkpointing (`checkpoint_file`), SHA-256 integrity-checked memory/disk caching, zero-leak credential handling, and pluggable cloud, local, custom R callable, or deterministic mock backends. |
| **2. LLM Psychometrics & Generalizability** | [`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md), [`d_study()`](https://marcusharrisphd.com/llmmetric/reference/d_study.md), [`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md), [`human_irr()`](https://marcusharrisphd.com/llmmetric/reference/human_irr.md), [`check_drift()`](https://marcusharrisphd.com/llmmetric/reference/check_drift.md) | Crossed multi-facet Generalizability Theory variance decomposition (REML & balanced ANOVA), $`E\rho^2`$ and $`\Phi`$ coefficients, cost-aware D-study reliability projections, calibration vs. human gold standards (ICC, CCC, $`\kappa`$, Krippendorff’s $`\alpha`$), human inter-rater reliability ceilings (ICC, Fleiss’s $`\kappa`$, multi-rater $`\alpha`$), and temporal/version drift testing. |
| **3. Differential Prompt Functioning (DPF)** | [`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md) | Uniform and non-uniform subgroup measurement invariance tests conditional on human gold standards (OLS & logistic DIF frameworks), unit-clustered sandwich inference, per-prompt FDR screening, and `prompt × group` interaction tests. |
| **4. Error-Corrected Inference & Planning** | [`select_validation_sample()`](https://marcusharrisphd.com/llmmetric/reference/select_validation_sample.md), [`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md), [`ppi_proportion()`](https://marcusharrisphd.com/llmmetric/reference/ppi_proportion.md), [`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md), [`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md), [`ppi_calibrated()`](https://marcusharrisphd.com/llmmetric/reference/ppi_calibrated.md), [`ppi_clustered()`](https://marcusharrisphd.com/llmmetric/reference/ppi_clustered.md), [`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md), [`ppi_sample_size()`](https://marcusharrisphd.com/llmmetric/reference/ppi_sample_size.md), [`ppi_power()`](https://marcusharrisphd.com/llmmetric/reference/ppi_power.md), [`format_apa_table()`](https://marcusharrisphd.com/llmmetric/reference/format_apa_table.md) | Stratified/Neyman/quantile/cluster validation sampling, Prediction-Powered Inference (`lambda = 1`), optimal power-tuned PPI++ (`lambda = "auto"`), Horvitz-Thompson active IPW-PPI, $`K`$-fold cross-fitted non-linear calibration ([`splines::ns()`](https://rdrr.io/r/splines/ns.html)), multi-class prevalences, clustered/multilevel GLS PPI++, sample-size/power planning, and APA 7th-edition tables. |
| **5. Publication Diagnostics** | [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html) methods for all result classes | Clean `ggplot2` diagnostics for variance decompositions, D-study curves, calibration curves & confusion matrices, human ceiling benchmarks, drift trajectories, DPF subgroup curves, PPI++ power curves, and Naive vs. Human-Only vs. PPI++ forest plots. |

------------------------------------------------------------------------

## Installation

``` r

# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/llmmetric")
```

------------------------------------------------------------------------

## Quickstart

### 1. Generalizability Study & Cost-Aware Decision Study

``` r

library(llmmetric)
data(ema_coded)

# Decompose variance across unit, prompt, model, interactions, and residual
g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
g
autoplot(g)

# How many prompts and models are needed to hit G >= 0.90?
d <- d_study(g, grid = list(prompt = 1:8, model = 1:4), target = 0.90)
d
autoplot(d)
```

### 2. Calibration, Human Inter-Rater Ceiling & Differential Prompt Functioning

``` r

unit_df <- aggregate_scores(ema_coded, score_col = "llm_score")

# Evaluate calibration against human gold-standard ratings
cal <- calibration_metrics(unit_df, pred_col = "llm_score", truth_col = "human_score")
cal
autoplot(cal)

# Audit uniform/non-uniform subgroup bias across prompts (unit-clustered Wald tests)
dpf <- dpf_test(
  ema_coded,
  score_col = "llm_score",
  truth_col = "human_score",
  group_col = "group",
  prompt_col = "prompt",
  id_col = "unit_id"
)
dpf
autoplot(dpf, type = "curves")
```

### 3. Error-Corrected Inference (PPI++ / `ppi_lmer`), Planning & APA Tables

``` r

# Multilevel random-intercept PPI++ accounting for participant clustering
fit_ml <- ppi_lmer(human_score ~ x + group + (1 | cluster), data = unit_df, pred_col = "llm_score")
format_apa_table(fit_ml, include_intercept = FALSE)
autoplot(fit_ml, true_values = c(x = 1.1, groupgroup_b = 0.5))

# Size the human validation subset for a 2,000-entry follow-up study
ppi_sample_size(target_se = 0.06, n_total = 2000, pilot = fit_ml, term = "x")
```

------------------------------------------------------------------------

## Documentation, Vignettes & Reference Website

- **Website:** <https://marcusharrisphd.com/llmmetric/>
- [`vignette("getting-started", package = "llmmetric")`](https://marcusharrisphd.com/llmmetric/articles/getting-started.md)
  — End-to-end psychometric coding, G/D-study, DPF, drift, and PPI++
  workflow.
- [`vignette("ppi-coverage-proof", package = "llmmetric")`](https://marcusharrisphd.com/llmmetric/articles/ppi-coverage-proof.md)
  — Monte Carlo simulation proof showing nominal 95% CI coverage for PPI
  / PPI++ / `ppi_lmer` where naive LLM regressions fail.
- [`vignette("dpf-and-fairness", package = "llmmetric")`](https://marcusharrisphd.com/llmmetric/articles/dpf-and-fairness.md)
  — Differential Prompt Functioning, multi-group invariance, human
  inter-rater ceilings
  ([`human_irr()`](https://marcusharrisphd.com/llmmetric/reference/human_irr.md)),
  and bias mitigation.
- [`vignette("designing-validation-studies", package = "llmmetric")`](https://marcusharrisphd.com/llmmetric/articles/designing-validation-studies.md)
  — Cost-aware D-studies,
  [`ppi_sample_size()`](https://marcusharrisphd.com/llmmetric/reference/ppi_sample_size.md)
  &
  [`ppi_power()`](https://marcusharrisphd.com/llmmetric/reference/ppi_power.md),
  [`ppi_proportion()`](https://marcusharrisphd.com/llmmetric/reference/ppi_proportion.md),
  checkpointed batch coding, and APA reporting tables.
- [`vignette("stratified-and-nonlinear-ppi", package = "llmmetric")`](https://marcusharrisphd.com/llmmetric/articles/stratified-and-nonlinear-ppi.md)
  — Stratified & Neyman validation sampling
  ([`select_validation_sample()`](https://marcusharrisphd.com/llmmetric/reference/select_validation_sample.md)),
  Horvitz-Thompson active IPW-PPI, cross-fitted non-linear calibration
  ([`ppi_calibrated()`](https://marcusharrisphd.com/llmmetric/reference/ppi_calibrated.md)),
  and custom offline scorers
  ([`backend_custom()`](https://marcusharrisphd.com/llmmetric/reference/backend_custom.md)).
- [ARCHITECTURE.md](https://marcusharrisphd.com/llmmetric/ARCHITECTURE.md)
  & [SECURITY.md](https://marcusharrisphd.com/llmmetric/SECURITY.md) —
  Formal statistical derivations, architecture, and security audit.

## License

MIT © Marcus Harris
