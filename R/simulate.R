#' Simulate a Realistic Multi-Facet LLM Coding & Validation Study
#'
#' Generates a complete synthetic research dataset combining:
#' 1. Units (optionally nested in clusters) with covariates, subgroup membership,
#'    latent true scores, and a human gold-standard validation subset (`NA` on
#'    unlabeled units).
#' 2. Multi-facet LLM scorings executed via [coding_design()], [code_texts()],
#'    and [backend_mock()] with configurable reliability variance components,
#'    calibration slope/bias, and Differential Prompt Functioning (DPF).
#'
#' @param n_units Total number of text units. Default `120L`.
#' @param n_labeled Number of units with observed human gold-standard scores.
#'   Default `40L`.
#' @param n_clusters Number of clusters (e.g., participants or classrooms)
#'   that units are nested within. Default `30L`.
#' @param prompts Named character vector of prompt templates.
#' @param models Character vector of model names.
#' @param beta Named or unnamed length-3 numeric vector `c(intercept, beta_x, beta_group)`
#'   governing the true data-generating regression
#'   \eqn{Y_i = \beta_0 + \beta_x X_i + \beta_g \mathbb{I}(G_i = B) + u_{c(i)} + \varepsilon_i}.
#' @param cluster_sd SD of cluster random intercept \eqn{u_c}. Default `0.5`.
#' @param resid_sd SD of unit-level outcome residual \eqn{\varepsilon_i}. Default `0.8`.
#' @param llm_intercept Additive LLM calibration bias. Default `0.4`.
#' @param llm_slope LLM calibration slope on true score. Default `0.85`.
#' @param llm_error_sd Residual LLM measurement noise SD. Default `0.45`.
#' @param prompt_sd SD of prompt main effects. Default `0.25`.
#' @param model_sd SD of model main effects. Default `0.20`.
#' @param unit_prompt_sd SD of `unit x prompt` interaction. Default `0.25`.
#' @param unit_model_sd SD of `unit x model` interaction. Default `0.20`.
#' @param dpf_uniform Optional named vector of uniform subgroup DPF shifts
#'   (e.g., `c(group_b = 0.6)`).
#' @param dpf_nonuniform Optional named vector of non-uniform subgroup DPF slope
#'   shifts.
#' @param dpf_prompt_group Optional matrix of prompt-by-group DPF shifts.
#' @param seed Integer random seed. Default `2026L`.
#'
#' @return A list with:
#'   - `coded`: an `llm_coded_data` object (long multi-facet scores).
#'   - `unit_data`: unit-level tibble with aggregated `llm_score`,
#'     `human_score` (`NA` on unlabeled units), `true_score`, `x`, `group`, and
#'     `cluster`.
#'   - `true_beta`: ground-truth regression parameters.
#' @export
#'
#' @examples
#' sim <- simulate_coding_study(n_units = 40, n_labeled = 20, n_clusters = 10, seed = 1)
#' g_study(sim$coded)
simulate_coding_study <- function(n_units = 120L,
                                  n_labeled = 40L,
                                  n_clusters = 30L,
                                  prompts = c(
                                    direct = "Rate construct intensity: {text}",
                                    rubric = "Apply anchored rubric: {text}",
                                    cot = "Reason carefully then rate: {text}"
                                  ),
                                  models = c("model_a", "model_b"),
                                  beta = c(`(Intercept)` = 2.5, x = 1.1, group_b = 0.6),
                                  cluster_sd = 0.5,
                                  resid_sd = 0.8,
                                  llm_intercept = 0.4,
                                  llm_slope = 0.85,
                                  llm_error_sd = 0.45,
                                  prompt_sd = 0.25,
                                  model_sd = 0.20,
                                  unit_prompt_sd = 0.25,
                                  unit_model_sd = 0.20,
                                  dpf_uniform = c(group_b = 0.55),
                                  dpf_nonuniform = NULL,
                                  dpf_prompt_group = NULL,
                                  seed = 2026L) {
  assert_scalar_numeric(n_units, "n_units", min = 10)
  assert_scalar_numeric(n_labeled, "n_labeled", min = 5, max = n_units - 5)
  assert_scalar_numeric(n_clusters, "n_clusters", min = 2, max = n_units)
  n_units <- as.integer(round(n_units))
  n_labeled <- as.integer(round(n_labeled))
  n_clusters <- as.integer(round(n_clusters))

  units_df <- with_local_seed(seed, {
    cl_idx <- rep(seq_len(n_clusters), length.out = n_units)
    cl_id <- paste0("cl_", sprintf("%03d", cl_idx))
    u_cl <- stats::rnorm(n_clusters, mean = 0, sd = cluster_sd)[cl_idx]
    x <- stats::rnorm(n_units)
    grp_bin <- stats::rbinom(n_units, 1L, 0.5)
    grp <- factor(ifelse(grp_bin == 1L, "group_b", "group_a"), levels = c("group_a", "group_b"))
    y_true <- beta[1L] + beta[2L] * x + beta[3L] * grp_bin + u_cl + stats::rnorm(n_units, sd = resid_sd)
    val_idx <- sample.int(n_units, size = n_labeled)
    human_score <- rep(NA_real_, n_units)
    human_score[val_idx] <- y_true[val_idx]

    tibble::tibble(
      unit_id = paste0("u_", sprintf("%04d", seq_len(n_units))),
      cluster = cl_id,
      group = grp,
      x = x,
      true_score = y_true,
      human_score = human_score,
      text = paste0("Entry ", seq_len(n_units), " (", grp, ", ", cl_id, ")")
    )
  })

  des <- coding_design(
    prompts = prompts,
    models = models,
    schema = coding_schema(llm_score = schema_field("numeric"))
  )

  be <- backend_mock(
    seed = as.integer(seed) + 100L,
    intercept = llm_intercept,
    true_score_slope = llm_slope,
    true_score_col = "true_score",
    error_sd = llm_error_sd,
    prompt_sd = prompt_sd,
    model_sd = model_sd,
    unit_prompt_sd = unit_prompt_sd,
    unit_model_sd = unit_model_sd,
    dpf_uniform = dpf_uniform,
    dpf_nonuniform = dpf_nonuniform,
    dpf_prompt_group = dpf_prompt_group,
    group_col = "group"
  )

  coded <- code_texts(
    data = units_df,
    text_col = "text",
    id_col = "unit_id",
    design = des,
    backend = be,
    cache = coding_cache("memory")
  )

  agg <- aggregate_scores(coded, score_col = "llm_score", id_col = "unit_id")

  list(
    coded = coded,
    unit_data = agg,
    true_beta = stats::setNames(as.numeric(beta), c("(Intercept)", "x", "groupgroup_b"))
  )
}
