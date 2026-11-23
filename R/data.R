#' Synthetic Multi-Facet EMA Diary LLM Coding Dataset
#'
#' A compact, reproducible synthetic dataset simulating an Ecological Momentary
#' Assessment (EMA) emotional-distress coding study. Contains 120 diary entries
#' nested within 30 participants (`cluster`), scored across a fully crossed
#' `3 prompts x 2 models` LLM coding design (`720` rows total). A random
#' validation subset of 50 entries carries human gold-standard ratings
#' (`human_score`), while the remaining 70 entries are unlabeled (`NA`), making
#' `ema_coded` ready for demonstrating [g_study()], [d_study()],
#' [calibration_metrics()], [dpf_test()], [ppi_lm()], and [ppi_lmer()].
#'
#' @format A [tibble::tibble()] with 720 rows and 10 variables:
#' \describe{
#'   \item{unit_id}{Character unique diary entry identifier (`"u_0001"`..`"u_0120"`).}
#'   \item{cluster}{Character participant identifier (`"cl_001"`..`"cl_030"`).}
#'   \item{prompt}{Character prompt template ID (`"direct"`, `"rubric"`, `"cot"`).}
#'   \item{model}{Character LLM model ID (`"model_a"`, `"model_b"`).}
#'   \item{temperature}{Numeric sampling temperature (`0`).}
#'   \item{replicate}{Integer replicate index (`1L`).}
#'   \item{llm_score}{Numeric LLM-generated emotional distress rating.}
#'   \item{human_score}{Numeric human gold-standard distress rating (observed on 50 validation entries; `NA` on 70 unlabeled entries).}
#'   \item{group}{Factor demographic/experimental subgroup (`"group_a"`, `"group_b"`), with mild uniform DPF injected for `group_b`.}
#'   \item{x}{Numeric standardized daily stressor predictor.}
#' }
#' @source Deterministically generated via [simulate_coding_study()].
#' @examples
#' data(ema_coded)
#' g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
"ema_coded"
