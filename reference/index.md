# Package index

## Multi-Facet Structured Coding Engine

Schemas, crossed experimental coding designs, resumable checkpointing,
integrity-checked caches, and pluggable backends.

- [`schema_field()`](https://marcusharrisphd.com/llmmetric/reference/schema_field.md)
  : Define a Field for an LLM Coding Schema
- [`coding_schema()`](https://marcusharrisphd.com/llmmetric/reference/coding_schema.md)
  : Define a Structured Output Schema for LLM Coding
- [`as_json_schema()`](https://marcusharrisphd.com/llmmetric/reference/as_json_schema.md)
  : Convert a Coding Schema to a JSON Schema List
- [`validate_coded_output()`](https://marcusharrisphd.com/llmmetric/reference/validate_coded_output.md)
  : Validate and Parse Structured LLM Output Against a Coding Schema
- [`coding_cache()`](https://marcusharrisphd.com/llmmetric/reference/coding_cache.md)
  : Create a Content-Addressed Response Cache for LLM Coding
- [`cache_key()`](https://marcusharrisphd.com/llmmetric/reference/cache_key.md)
  : Compute Content-Addressed SHA-256 Cache Key
- [`cache_get()`](https://marcusharrisphd.com/llmmetric/reference/cache_operations.md)
  [`cache_set()`](https://marcusharrisphd.com/llmmetric/reference/cache_operations.md)
  [`cache_clear()`](https://marcusharrisphd.com/llmmetric/reference/cache_operations.md)
  [`cache_stats()`](https://marcusharrisphd.com/llmmetric/reference/cache_operations.md)
  : Retrieve, Store, Clear, or Inspect a Coding Cache
- [`coding_design()`](https://marcusharrisphd.com/llmmetric/reference/coding_design.md)
  : Specify a Multi-Facet LLM Coding Design
- [`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md)
  : Execute a Multi-Facet Structured LLM Coding Design
- [`aggregate_scores()`](https://marcusharrisphd.com/llmmetric/reference/aggregate_scores.md)
  : Aggregate Multi-Facet LLM Scores to Unit Level
- [`backend_score()`](https://marcusharrisphd.com/llmmetric/reference/backend_score.md)
  : Score a Single Coding Request via an LLM Backend
- [`backend_mock()`](https://marcusharrisphd.com/llmmetric/reference/backend_mock.md)
  : Deterministic Synthetic LLM Rater Backend
- [`backend_openai()`](https://marcusharrisphd.com/llmmetric/reference/backend_openai.md)
  : OpenAI-Compatible HTTP Coding Backend
- [`backend_ollama()`](https://marcusharrisphd.com/llmmetric/reference/backend_ollama.md)
  : Local Ollama HTTP Coding Backend

## LLM Psychometrics & Generalizability

Generalizability Theory variance decomposition, Decision Studies,
calibration against human gold standards, human ceilings, and drift
monitoring.

- [`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md)
  : Generalizability Study (G-Study) for Multi-Facet LLM Coding
- [`d_study()`](https://marcusharrisphd.com/llmmetric/reference/d_study.md)
  : Decision Study (D-Study) Reliability Projections & Optimal Design
  Solver
- [`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md)
  : Calibration Metrics Comparing LLM Scores to Human Gold Standards
- [`human_irr()`](https://marcusharrisphd.com/llmmetric/reference/human_irr.md)
  : Human Inter-Rater Reliability (IRR) Ceiling Benchmark
- [`check_drift()`](https://marcusharrisphd.com/llmmetric/reference/check_drift.md)
  : Test for Temporal or Model-Version Scoring Drift

## Differential Prompt Functioning & Fairness

Uniform and non-uniform subgroup measurement invariance testing and
prompt-level FDR screening.

- [`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md)
  : Differential Prompt Functioning (DPF) & Measurement Invariance Test

## Prediction-Powered Inference & Multilevel Extensions

Error-corrected estimators combining human validation subsets with large
LLM-coded corpora.

- [`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md)
  : Prediction-Powered Mean Estimation (PPI / PPI++)

- [`ppi_proportion()`](https://marcusharrisphd.com/llmmetric/reference/ppi_proportion.md)
  : Prediction-Powered Category Proportion & Prevalence Estimation (PPI
  / PPI++)

- [`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md)
  : Prediction-Powered Linear Regression (PPI / PPI++ / Calibrated
  Control Variate)

- [`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md)
  : Prediction-Powered Generalized Linear Model (PPI / PPI++ /
  Calibrated)

- [`ppi_clustered()`](https://marcusharrisphd.com/llmmetric/reference/ppi_clustered.md)
  : Cluster-Robust Prediction-Powered Inference (PPI / PPI++)

- [`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md)
  :

  Multilevel Random-Intercept Prediction-Powered Inference (`ppi_lmer`)

## Study Planning & APA Reporting

Validation sample-size planning, power curves, and publication-ready APA
7th-edition tables.

- [`ppi_sample_size()`](https://marcusharrisphd.com/llmmetric/reference/ppi_sample_size.md)
  : Plan Human Validation Sample Size for Prediction-Powered Inference
  (PPI++)
- [`ppi_power()`](https://marcusharrisphd.com/llmmetric/reference/ppi_power.md)
  : Compute Projected Precision and Statistical Power for PPI++
- [`format_apa_table()`](https://marcusharrisphd.com/llmmetric/reference/format_apa_table.md)
  : Format Publication-Ready APA Comparison Tables

## Diagnostic Visualizations

Publication-ready ggplot2 autoplot() methods for all diagnostic and
inference objects.

- [`autoplot(`*`<llm_g_study>`*`)`](https://marcusharrisphd.com/llmmetric/reference/autoplot.llm_g_study.md)
  : Plot Generalizability Study Variance Decomposition
- [`autoplot(`*`<llm_d_study>`*`)`](https://marcusharrisphd.com/llmmetric/reference/autoplot.llm_d_study.md)
  : Plot Decision Study (D-Study) Reliability Curves
- [`autoplot(`*`<llm_calibration>`*`)`](https://marcusharrisphd.com/llmmetric/reference/autoplot.llm_calibration.md)
  : Plot LLM-vs-Human Calibration Diagnostics
- [`autoplot(`*`<llm_human_irr>`*`)`](https://marcusharrisphd.com/llmmetric/reference/autoplot.llm_human_irr.md)
  : Plot Human Inter-Rater Reliability Ceiling vs. LLM Calibration
- [`autoplot(`*`<llm_drift>`*`)`](https://marcusharrisphd.com/llmmetric/reference/autoplot.llm_drift.md)
  : Plot Temporal or Model-Version Scoring Drift
- [`autoplot(`*`<llm_dpf>`*`)`](https://marcusharrisphd.com/llmmetric/reference/autoplot.llm_dpf.md)
  : Plot Differential Prompt Functioning (DPF) Diagnostics
- [`autoplot(`*`<llm_ppi>`*`)`](https://marcusharrisphd.com/llmmetric/reference/autoplot.llm_ppi.md)
  : Plot Prediction-Powered Inference vs. Human-Only & Naive LLM
  Benchmarks
- [`autoplot(`*`<llm_ppi_power>`*`)`](https://marcusharrisphd.com/llmmetric/reference/autoplot.llm_ppi_power.md)
  : Plot PPI++ Power or Standard Error Curves
- [`reexports`](https://marcusharrisphd.com/llmmetric/reference/reexports.md)
  [`autoplot`](https://marcusharrisphd.com/llmmetric/reference/reexports.md)
  : Objects exported from other packages

## Datasets & Simulation

Built-in multi-facet EMA coding study dataset and synthetic study
simulator.

- [`ema_coded`](https://marcusharrisphd.com/llmmetric/reference/ema_coded.md)
  : Synthetic Multi-Facet EMA Diary LLM Coding Dataset
- [`simulate_coding_study()`](https://marcusharrisphd.com/llmmetric/reference/simulate_coding_study.md)
  : Simulate a Realistic Multi-Facet LLM Coding & Validation Study
