# llmmetric: Psychometric Validation and Inference for LLM-Coded Data

Treats large language models (LLMs) explicitly as fallible psychometric
measurement instruments. Provides multi-facet structured coding
orchestration with resumable checkpointing and hardened caching,
Generalizability Theory (G-study and D-study), calibration against human
gold standards and inter-rater reliability ceilings, temporal drift
monitoring, Differential Prompt Functioning (DPF) invariance tests,
Prediction-Powered Inference (PPI, PPI++, prevalence proportions, and
clustered/multilevel extensions), validation sample-size/power planning,
APA reporting tables, and publication-ready
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
diagnostics.

## See also

Key workflow entry points:

- Multi-facet coding:
  [`coding_schema()`](https://marcusharrisphd.com/llmmetric/reference/coding_schema.md),
  [`coding_design()`](https://marcusharrisphd.com/llmmetric/reference/coding_design.md),
  [`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md),
  [`coding_cache()`](https://marcusharrisphd.com/llmmetric/reference/coding_cache.md),
  [`backend_openai()`](https://marcusharrisphd.com/llmmetric/reference/backend_openai.md),
  [`backend_ollama()`](https://marcusharrisphd.com/llmmetric/reference/backend_ollama.md),
  [`backend_mock()`](https://marcusharrisphd.com/llmmetric/reference/backend_mock.md)

- Psychometrics & fairness:
  [`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md),
  [`d_study()`](https://marcusharrisphd.com/llmmetric/reference/d_study.md),
  [`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md),
  [`human_irr()`](https://marcusharrisphd.com/llmmetric/reference/human_irr.md),
  [`check_drift()`](https://marcusharrisphd.com/llmmetric/reference/check_drift.md),
  [`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md)

- Error-corrected inference & planning:
  [`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md),
  [`ppi_proportion()`](https://marcusharrisphd.com/llmmetric/reference/ppi_proportion.md),
  [`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md),
  [`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md),
  [`ppi_clustered()`](https://marcusharrisphd.com/llmmetric/reference/ppi_clustered.md),
  [`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md),
  [`ppi_sample_size()`](https://marcusharrisphd.com/llmmetric/reference/ppi_sample_size.md),
  [`ppi_power()`](https://marcusharrisphd.com/llmmetric/reference/ppi_power.md),
  [`format_apa_table()`](https://marcusharrisphd.com/llmmetric/reference/format_apa_table.md)

## Author

**Maintainer**: Marcus Harris <marcus.harris.uconn@gmail.com>
\[copyright holder\]

Authors:

- Marcus Harris <marcus.harris.uconn@gmail.com> \[copyright holder\]
