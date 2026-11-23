test_that("g_study recovers variance components consistently across REML and ANOVA", {
  set.seed(2026)
  n_u <- 80
  units <- tibble::tibble(
    id = paste0("u", seq_len(n_u)),
    text = paste("T", seq_len(n_u)),
    true_score = rnorm(n_u, sd = 1.2)
  )
  des <- coding_design(
    prompts = paste0("Prompt ", 1:4, ": {text}"),
    models = c("m1", "m2", "m3"),
    schema = coding_schema(score = schema_field("numeric"))
  )
  be <- backend_mock(
    seed = 101,
    true_score_slope = 1.0,
    error_sd = 0.4,
    prompt_sd = 0.35,
    model_sd = 0.30,
    unit_prompt_sd = 0.35,
    unit_model_sd = 0.25,
    prompt_model_sd = 0.15
  )
  coded <- code_texts(units, "text", id_col = "id", design = des, backend = be)

  g_reml <- g_study(coded, method = "reml")
  g_anova <- g_study(coded, method = "anova")

  expect_s3_class(g_reml, "llm_g_study")
  expect_s3_class(g_anova, "llm_g_study")
  expect_equal(g_reml$method_used, "reml")
  expect_equal(g_anova$method_used, "anova")

  # REML and ANOVA should agree closely on a balanced crossed design
  expect_equal(g_reml$g_coef, g_anova$g_coef, tolerance = 0.02)
  expect_equal(g_reml$phi_coef, g_anova$phi_coef, tolerance = 0.02)
  expect_lte(g_reml$phi_coef, g_reml$g_coef + 1e-10)
  expect_gt(g_reml$g_coef, g_reml$g_single)

  # Universe variance dominates and matches ~1.2^2 = 1.44 reasonably
  expect_gt(g_reml$sigma2_universe, 0.8)
  expect_lt(g_reml$sigma2_universe, 2.3)

  # 1-facet design with replicates > 1 agrees between REML and ANOVA
  des_rep <- coding_design(
    prompts = paste0("P", 1:4, ": {text}"),
    models = "m1",
    replicates = 2L,
    schema = coding_schema(score = schema_field("numeric"))
  )
  coded_rep <- code_texts(units[1:40, ], "text", id_col = "id", design = des_rep, backend = be)
  g1_reml <- g_study(coded_rep, facets = "prompt", method = "reml")
  g1_anova <- g_study(coded_rep, facets = "prompt", method = "anova")
  expect_equal(g1_reml$n_replicates, 2L)
  expect_equal(g1_reml$g_coef, g1_anova$g_coef, tolerance = 0.03)
  expect_equal(g1_reml$phi_coef, g1_anova$phi_coef, tolerance = 0.03)

  # Unbalanced design works in REML / auto and errors cleanly in ANOVA
  unbal <- coded$scores[-seq(1, 25, by = 3), ]
  g_unbal <- g_study(unbal, score_col = "score", facets = c("prompt", "model"), method = "auto")
  expect_false(g_unbal$balanced)
  expect_equal(g_unbal$method_used, "reml")
  expect_error(g_study(unbal, score_col = "score", facets = c("prompt", "model"), method = "anova"), "balanced")
})

test_that("d_study projections are monotone and recommendations hit target reliability", {
  data(ema_coded)
  g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
  d <- d_study(g, grid = list(prompt = 1:8, model = 1:4), target = 0.90, metric = "g")

  expect_s3_class(d, "llm_d_study")
  expect_equal(nrow(d$grid), 32L)

  # Strict monotonicity in n_prompts for fixed n_models
  for (m in 1:4) {
    sub <- d$grid[d$grid$model == m, ]
    sub <- sub[order(sub$prompt), ]
    expect_true(all(diff(sub$g_coef) >= -1e-12))
    expect_true(all(diff(sub$phi_coef) >= -1e-12))
  }

  # Strict monotonicity in n_models for fixed n_prompts
  for (p in 1:8) {
    sub <- d$grid[d$grid$prompt == p, ]
    sub <- sub[order(sub$model), ]
    expect_true(all(diff(sub$g_coef) >= -1e-12))
    expect_true(all(diff(sub$phi_coef) >= -1e-12))
  }

  # Phi <= G everywhere
  expect_true(all(d$grid$phi_coef <= d$grid$g_coef + 1e-12))

  # Recommendations meet target and are Pareto-minimal
  expect_gt(nrow(d$recommendations), 0L)
  expect_true(all(d$recommendations$g_coef >= 0.90))

  # Asymptotes exceed finite reliability when other facet = 1
  g_m1_max <- max(d$grid$g_coef[d$grid$model == 1])
  asym_p <- d$asymptotes$g_ceiling_others_1[d$asymptotes$facet_to_inf == "prompt"]
  expect_gte(asym_p, g_m1_max - 1e-12)

  # Custom costs prioritize cheaper facet
  d_cost <- d_study(g, grid = list(prompt = 1:8, model = 1:4), target = 0.90, costs = c(prompt = 1, model = 10))
  expect_equal(d_cost$recommendations$model[1L], 1L)
})
