test_that("all autoplot methods build valid ggplot2 objects without warnings", {
  data(ema_coded)

  # G-study
  g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
  p_g <- autoplot(g)
  expect_s3_class(p_g, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p_g))

  # D-study (g, phi, both)
  d <- d_study(g, grid = list(prompt = 1:6, model = 1:3), target = 0.85)
  for (mt in c("g", "phi", "both")) {
    p_d <- autoplot(d, metric = mt)
    expect_s3_class(p_d, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p_d))
  }

  # Calibration (continuous & categorical)
  agg <- aggregate_scores(ema_coded, score_col = "llm_score")
  cal_c <- calibration_metrics(agg, pred_col = "llm_score", truth_col = "human_score")
  p_cal_c <- autoplot(cal_c)
  expect_s3_class(p_cal_c, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p_cal_c))

  cat_df <- tibble::tibble(
    truth = factor(rep(c("low", "mid", "high"), each = 10), levels = c("low", "mid", "high")),
    pred = factor(c(rep("low", 8), rep("mid", 12), rep("high", 10)), levels = c("low", "mid", "high"))
  )
  cal_cat <- calibration_metrics(cat_df, pred_col = "pred", truth_col = "truth", type = "ordinal")
  p_cal_cat <- autoplot(cal_cat)
  expect_s3_class(p_cal_cat, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p_cal_cat))

  # Drift (diff & trajectory)
  set.seed(12)
  dr_df <- tibble::tibble(
    unit_id = rep(paste0("u", 1:25), 3),
    version = rep(c("v1", "v2", "v3"), each = 25),
    base = rep(rnorm(25), 3),
    score = base + rep(c(0, 0.05, 0.5), each = 25) + rnorm(75, sd = 0.2)
  )
  dr <- check_drift(dr_df, score_col = "score", version_col = "version")
  for (tp in c("diff", "trajectory")) {
    p_dr <- autoplot(dr, type = tp)
    expect_s3_class(p_dr, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p_dr))
  }

  # DPF (curves, bias, prompts)
  dpf <- dpf_test(
    ema_coded,
    score_col = "llm_score",
    truth_col = "human_score",
    group_col = "group",
    prompt_col = "prompt",
    id_col = "unit_id"
  )
  for (tp in c("curves", "bias", "prompts")) {
    p_dpf <- autoplot(dpf, type = tp)
    expect_s3_class(p_dpf, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p_dpf))
  }

  # PPI (with and without true_values)
  fit <- ppi_lm(human_score ~ x + group, data = agg, pred_col = "llm_score")
  p_ppi1 <- autoplot(fit)
  p_ppi2 <- autoplot(fit, true_values = c(x = 1.1, groupgroup_b = 0.5))
  expect_s3_class(p_ppi1, "ggplot")
  expect_s3_class(p_ppi2, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p_ppi1))
  expect_no_error(ggplot2::ggplot_build(p_ppi2))
})
