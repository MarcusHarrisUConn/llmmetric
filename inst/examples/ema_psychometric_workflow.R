# End-to-End Psychometric AI & Prediction-Powered Inference Workflow
# Runs 100% offline using llmmetric's deterministic mock backend and built-in data.

library(llmmetric)

# 1. Inspect built-in multi-facet EMA coding study (3 prompts x 2 models x 120 units)
data(ema_coded)

# 2. Generalizability Study & Cost-Aware Decision Study
g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
print(g)

d <- d_study(g, grid = list(prompt = 1:8, model = 1:4), target = 0.90, costs = c(prompt = 1, model = 3))
print(d)

# 3. Aggregate multi-facet scores to unit level & audit calibration vs. human ratings
unit_df <- aggregate_scores(ema_coded, score_col = "llm_score", id_col = "unit_id")
cal <- calibration_metrics(unit_df, pred_col = "llm_score", truth_col = "human_score")
print(cal)

# 4. Differential Prompt Functioning (DPF) across prompts with unit-clustered Wald tests
dpf <- dpf_test(
  ema_coded,
  score_col = "llm_score",
  truth_col = "human_score",
  group_col = "group",
  prompt_col = "prompt",
  id_col = "unit_id"
)
print(dpf)

# 5. Error-Corrected Multilevel Inference (PPI++) & APA Reporting Table
fit_ml <- ppi_lmer(human_score ~ x + group + (1 | cluster), data = unit_df, pred_col = "llm_score")
print(format_apa_table(fit_ml, include_intercept = FALSE))

# 6. Plan Validation Sample Size for a Full 2,000-Entry Study
plan <- ppi_sample_size(target_se = 0.06, n_total = 2000, pilot = fit_ml, term = "x")
print(plan)
