#' llmmetric: Psychometric Validation and Inference for LLM-Coded Data
#'
#' Treats large language models (LLMs) explicitly as fallible psychometric
#' measurement instruments. Provides multi-facet structured coding
#' orchestration with resumable checkpointing and hardened caching,
#' Generalizability Theory (G-study and D-study), calibration against human
#' gold standards and inter-rater reliability ceilings, temporal drift
#' monitoring, Differential Prompt Functioning (DPF) invariance tests,
#' Prediction-Powered Inference (PPI, PPI++, prevalence proportions, and
#' clustered/multilevel extensions), validation sample-size/power planning,
#' APA reporting tables, and publication-ready `ggplot2::autoplot()`
#' diagnostics.
#'
#' @seealso
#' Key workflow entry points:
#' - Multi-facet coding: [coding_schema()], [coding_design()], [code_texts()],
#'   [coding_cache()], [backend_openai()], [backend_ollama()], [backend_mock()]
#' - Psychometrics & fairness: [g_study()], [d_study()], [calibration_metrics()],
#'   [human_irr()], [check_drift()], [dpf_test()]
#' - Error-corrected inference & planning: [ppi_mean()], [ppi_proportion()],
#'   [ppi_lm()], [ppi_glm()], [ppi_clustered()], [ppi_lmer()],
#'   [ppi_sample_size()], [ppi_power()], [format_apa_table()]
#'
#' @keywords internal
#' @importFrom rlang .data %||%
#' @importFrom tibble tibble as_tibble
"_PACKAGE"

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot
