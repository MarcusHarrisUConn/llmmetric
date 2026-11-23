# llmmetric

> **Psychometric reliability, measurement invariance, and error-corrected statistical inference for LLM-coded research data in R.**

Researchers across psychology, education, sociology, management, and health sciences increasingly use large language models (LLMs) to code open-ended survey responses, therapy transcripts, interviews, and ecological momentary assessment (EMA) diaries into quantitative variables. Treating LLM outputs as error-free gold-standard measurements ignores prompt sensitivity, model drift, subgroup bias, and measurement error—invalidating standard errors, confidence intervals, and *p*-values in downstream analyses.

`llmmetric` bridges modern AI annotation pipelines and formal psychometric & statistical theory by treating **LLMs explicitly as fallible measurement instruments**.

---

## Core Capabilities

| Pillar | Functions | What It Solves |
| :--- | :--- | :--- |
| **1. Multi-Facet Structured Coding Engine** | `coding_schema()`, `schema_field()`, `as_json_schema()`, `validate_coded_output()`, `coding_cache()`, `coding_design()`, `code_texts()`, `aggregate_scores()`, `backend_openai()`, `backend_ollama()`, `backend_mock()` | Reproducible crossed coding (`unit × prompt × model × temperature × replicate`) with strict JSON schema validation, retry handling, SHA-256 memory/disk caching, and pluggable cloud, local, or deterministic mock backends. |
| **2. LLM Psychometrics & Generalizability** | `g_study()`, `d_study()`, `calibration_metrics()`, `check_drift()` | Crossed multi-facet Generalizability Theory variance decomposition (REML & balanced ANOVA), $E\rho^2$ and $\Phi$ coefficients, D-study reliability projections & minimal-cost design solver, calibration vs. human gold standards (ICC, CCC, $\kappa$, Krippendorff's $\alpha$), and temporal/version drift testing. |
| **3. Differential Prompt Functioning (DPF)** | `dpf_test()` | Uniform and non-uniform subgroup measurement invariance tests conditional on human gold standards (OLS & logistic DIF frameworks), unit-clustered sandwich inference, per-prompt FDR screening, and `prompt × group` interaction tests. |
| **4. Error-Corrected Inference (PPI / PPI++)** | `ppi_mean()`, `ppi_lm()`, `ppi_glm()`, `ppi_clustered()`, `ppi_lmer()` | Prediction-Powered Inference (`lambda = 1`), optimal power-tuned PPI++ (`lambda = "auto"`), coordinate-wise calibrated control variates (supporting LLM-coded outcomes *and* predictors), and clustered/multilevel random-intercept GLS PPI++ with human-only and naive-LLM benchmarks. |
| **5. Publication Diagnostics** | `autoplot()` methods for all result classes | Clean `ggplot2` diagnostics for variance decompositions, D-study curves, calibration curves & confusion matrices, drift trajectories, DPF subgroup curves, and Naive vs. Human-Only vs. PPI++ forest plots. |

---

## Installation

```r
# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/llmmetric")
```

---

## Quickstart

### 1. Multi-Facet Coding with Caching & Schema Validation

```r
library(llmmetric)

# Define strict structured output schema and crossed coding design
schema <- coding_schema(
  llm_score = schema_field("numeric", min = -5, max = 10, description = "Distress rating")
)

design <- coding_design(
  prompts = c(
    direct = "Rate emotional distress in this diary entry: {text}",
    rubric = "Apply the anchored clinical distress rubric: {text}",
    cot    = "Analyze emotional cues step by step, then rate distress: {text}"
  ),
  models = c("model_a", "model_b"),
  temperatures = 0,
  schema = schema
)

# Run offline with deterministic mock rater (swap for backend_openai() or backend_ollama() in production)
sim <- simulate_coding_study(n_units = 120, n_labeled = 50, n_clusters = 30, seed = 2026)
```

### 2. Generalizability Study & Decision Study

```r
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

### 3. Calibration & Differential Prompt Functioning (DPF)

```r
# Collapse multi-facet scores to unit level
unit_df <- aggregate_scores(ema_coded, score_col = "llm_score")

# Evaluate calibration against human gold-standard ratings
cal <- calibration_metrics(unit_df, pred_col = "llm_score", truth_col = "human_score")
cal
autoplot(cal)

# Audit uniform/non-uniform subgroup bias across prompts (with unit-clustered Wald tests)
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

### 4. Error-Corrected Downstream Inference (PPI++ & Multilevel `ppi_lmer`)

```r
# Combine 50 human-validated entries with 70 LLM-only entries
fit_lm <- ppi_lm(human_score ~ x + group, data = unit_df, pred_col = "llm_score")
summary(fit_lm)
autoplot(fit_lm)

# Multilevel random-intercept PPI++ accounting for participant clustering
fit_ml <- ppi_lmer(human_score ~ x + group + (1 | cluster), data = unit_df, pred_col = "llm_score")
fit_ml
```

---

## Using Live API or Local Backends

```r
# OpenAI-compatible API (OpenAI, vLLM, OpenRouter, LiteLLM, Azure proxy)
oa_backend <- backend_openai(
  model = "gpt-4o-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  response_format = "json_schema"
)

# Local Ollama server
ol_backend <- backend_ollama(
  model = "llama3.2",
  base_url = "http://localhost:11434",
  format = "schema"
)

# Persistent disk cache avoids re-paying for identical (text, prompt, model) calls
disk_cache <- coding_cache("disk", dir = "llm_cache")
```

---

## Documentation & Vignettes

- `vignette("getting-started", package = "llmmetric")` — Complete end-to-end psychometric coding, G/D-study, DPF, drift, and PPI++ workflow.
- `vignette("ppi-coverage-proof", package = "llmmetric")` — Monte Carlo simulation proof showing nominal 95% CI coverage for PPI / PPI++ / `ppi_lmer` where naive LLM regressions fail.
- [ARCHITECTURE.md](ARCHITECTURE.md) — Formal statistical derivations and software architecture.

## License

MIT © Marcus Harris
