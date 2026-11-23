# llmmetric

> **Psychometric reliability, measurement invariance, and error-corrected statistical inference for LLM-coded research data in R.**

Researchers across psychology, education, sociology, management, and health sciences increasingly use large language models (LLMs) to code open-ended survey responses, therapy transcripts, interviews, and ecological momentary assessment (EMA) diaries into quantitative variables. Treating LLM outputs as error-free gold-standard measurements ignores prompt sensitivity, model drift, subgroup bias, and measurement error—invalidating standard errors, confidence intervals, and *p*-values in downstream analyses.

`llmmetric` bridges modern AI annotation pipelines and formal psychometric & statistical theory by treating **LLMs explicitly as fallible measurement instruments**.

---

## Core Capabilities

| Pillar | Functions | What It Solves |
| :--- | :--- | :--- |
| **1. Multi-Facet Structured Coding Engine** | `coding_schema()`, `schema_field()`, `as_json_schema()`, `validate_coded_output()`, `coding_cache()`, `coding_design()`, `code_texts()`, `aggregate_scores()`, `backend_openai()`, `backend_ollama()`, `backend_mock()` | Reproducible crossed coding (`unit × prompt × model × temperature × replicate`) with strict hardened JSON validation, resumable atomic checkpointing (`checkpoint_file`), SHA-256 integrity-checked memory/disk caching, zero-leak credential handling, and pluggable cloud, local, or deterministic mock backends. |
| **2. LLM Psychometrics & Generalizability** | `g_study()`, `d_study()`, `calibration_metrics()`, `human_irr()`, `check_drift()` | Crossed multi-facet Generalizability Theory variance decomposition (REML & balanced ANOVA), $E\rho^2$ and $\Phi$ coefficients, cost-aware D-study reliability projections, calibration vs. human gold standards (ICC, CCC, $\kappa$, Krippendorff's $\alpha$), human inter-rater reliability ceilings (ICC, Fleiss's $\kappa$, multi-rater $\alpha$), and temporal/version drift testing. |
| **3. Differential Prompt Functioning (DPF)** | `dpf_test()` | Uniform and non-uniform subgroup measurement invariance tests conditional on human gold standards (OLS & logistic DIF frameworks), unit-clustered sandwich inference, per-prompt FDR screening, and `prompt × group` interaction tests. |
| **4. Error-Corrected Inference & Planning** | `ppi_mean()`, `ppi_proportion()`, `ppi_lm()`, `ppi_glm()`, `ppi_clustered()`, `ppi_lmer()`, `ppi_sample_size()`, `ppi_power()`, `format_apa_table()` | Prediction-Powered Inference (`lambda = 1`), optimal power-tuned PPI++ (`lambda = "auto"`), multi-class category prevalences (marginal & simultaneous CIs), coordinate-wise calibrated control variates (LLM outcomes *and* predictors), clustered/multilevel random-intercept GLS PPI++, validation sample-size/power planning, and APA 7th-edition comparison tables. |
| **5. Publication Diagnostics** | `autoplot()` methods for all result classes | Clean `ggplot2` diagnostics for variance decompositions, D-study curves, calibration curves & confusion matrices, human ceiling benchmarks, drift trajectories, DPF subgroup curves, PPI++ power curves, and Naive vs. Human-Only vs. PPI++ forest plots. |

---

## Installation

```r
# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/llmmetric")
```

---

## Quickstart

### 1. Generalizability Study & Cost-Aware Decision Study

```r
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

```r
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

```r
# Multilevel random-intercept PPI++ accounting for participant clustering
fit_ml <- ppi_lmer(human_score ~ x + group + (1 | cluster), data = unit_df, pred_col = "llm_score")
format_apa_table(fit_ml, include_intercept = FALSE)
autoplot(fit_ml, true_values = c(x = 1.1, groupgroup_b = 0.5))

# Size the human validation subset for a 2,000-entry follow-up study
ppi_sample_size(target_se = 0.06, n_total = 2000, pilot = fit_ml, term = "x")
```

---

## Documentation, Vignettes & Reference Website

- **Website:** <https://marcusharrisphd.com/llmmetric/>
- `vignette("getting-started", package = "llmmetric")` — End-to-end psychometric coding, G/D-study, DPF, drift, and PPI++ workflow.
- `vignette("ppi-coverage-proof", package = "llmmetric")` — Monte Carlo simulation proof showing nominal 95% CI coverage for PPI / PPI++ / `ppi_lmer` where naive LLM regressions fail.
- `vignette("dpf-and-fairness", package = "llmmetric")` — Differential Prompt Functioning, multi-group invariance, human inter-rater ceilings (`human_irr()`), and bias mitigation.
- `vignette("designing-validation-studies", package = "llmmetric")` — Cost-aware D-studies, `ppi_sample_size()` & `ppi_power()`, `ppi_proportion()`, checkpointed batch coding, and APA reporting tables.
- [ARCHITECTURE.md](ARCHITECTURE.md) & [SECURITY.md](SECURITY.md) — Formal statistical derivations, architecture, and security audit.

## License

MIT © Marcus Harris
