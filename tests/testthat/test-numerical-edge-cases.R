test_that("PPI estimators handle rank-deficient / collinear design matrices cleanly", {
  set.seed(3101)
  n_tot <- 400
  x1 <- rnorm(n_tot)
  x2 <- 2 * x1 # exact collinearity
  z <- rnorm(n_tot)
  y <- 1.0 + 1.5 * x1 - 0.7 * z + rnorm(n_tot, sd = 0.8)
  f <- 0.4 + 0.85 * y + 0.3 * x1 + rnorm(n_tot, sd = 0.4)
  cl <- rep(paste0("c", 1:40), each = 10)

  df <- tibble::tibble(
    x1 = x1, x2 = x2, z = z, cl = cl,
    y = c(y[1:120], rep(NA_real_, n_tot - 120)),
    f = f
  )

  expect_warning(fit_lm <- ppi_lm(y ~ x1 + x2 + z, data = df, pred_col = "f"), "Rank-deficient")
  expect_equal(fit_lm$aliased_terms, "x2")
  expect_true(is.na(coef(fit_lm)["x2"]))
  expect_true(is.na(fit_lm$estimates$std_error[fit_lm$estimates$term == "x2"]))
  expect_equal(unname(coef(fit_lm)["x1"]), 1.5, tolerance = 0.25)
  expect_equal(unname(coef(fit_lm)["z"]), -0.7, tolerance = 0.25)

  expect_warning(fit_cal <- ppi_lm(y ~ x1 + x2 + z, data = df, pred_col = "f", method = "calibrated"), "Rank-deficient")
  expect_true(is.na(coef(fit_cal)["x2"]))
  expect_true(is.finite(coef(fit_cal)["x1"]))

  expect_warning(
    fit_cl <- ppi_clustered(y ~ x1 + x2 + z, data = df, pred_col = "f", cluster = "cl"),
    "Rank-deficient"
  )
  expect_true(is.na(coef(fit_cl)["x2"]))
  expect_true(is.finite(coef(fit_cl)["x1"]))

  expect_warning(
    fit_ml <- ppi_lmer(y ~ x1 + x2 + z + (1 | cl), data = df, pred_col = "f"),
    "Rank-deficient"
  )
  expect_true(is.na(coef(fit_ml)["x2"]))
  expect_true(is.finite(coef(fit_ml)["x1"]))
})

test_that("ppi_glm guards quasi-complete binomial separation and handles extreme Poisson counts", {
  set.seed(3102)
  x_sep <- rnorm(300)
  y_sep <- as.numeric(x_sep > 0) # complete separation
  f_sep <- stats::plogis(3 * x_sep)
  df_sep <- tibble::tibble(
    x = x_sep,
    y = c(y_sep[1:60], rep(NA_real_, 240)),
    f = f_sep
  )
  expect_warning(
    fit_sep <- ppi_glm(y ~ x, data = df_sep, pred_col = "f", family = stats::binomial()),
    "separation"
  )
  expect_true(isTRUE(fit_sep$separation_detected))
  expect_true(all(is.finite(coef(fit_sep))))
  expect_lt(max(abs(coef(fit_sep))), 40)
  # SE must not falsely collapse to tiny values under separation
  se_x <- fit_sep$estimates$std_error[fit_sep$estimates$term == "x"]
  expect_gt(se_x, 0.5)

  # Extreme Poisson counts
  x_p <- rnorm(400, sd = 2.0)
  mu_p <- exp(pmin(0.4 + 0.6 * x_p, 7))
  y_p <- stats::rpois(400, mu_p)
  f_p <- pmax(0.1, 1.2 * y_p + 0.5)
  df_p <- tibble::tibble(x = x_p, y = c(y_p[1:120], rep(NA_real_, 280)), f = f_p)
  fit_p <- ppi_glm(y ~ x, data = df_p, pred_col = "f", family = stats::poisson())
  expect_true(all(is.finite(coef(fit_p))))
  expect_true(all(is.finite(fit_p$estimates$std_error)))
})

test_that("PPI handles constant LLM predictions, perfect LLM predictions, small n, and non-finite inputs", {
  set.seed(3103)
  n_tot <- 300
  x <- rnorm(n_tot)
  y <- 1 + 1.2 * x + rnorm(n_tot)

  # Constant zero-variance LLM prediction -> lambda = 0, finite SEs, matches human_only
  df_zero <- tibble::tibble(
    x = x,
    y = c(y[1:80], rep(NA_real_, n_tot - 80)),
    f = rep(5.0, n_tot)
  )
  m_zero <- ppi_mean(df_zero, "y", "f")
  expect_equal(m_zero$lambda, 0)
  expect_true(is.finite(m_zero$estimates$std_error))

  lm_zero <- ppi_lm(y ~ x, data = df_zero, pred_col = "f")
  expect_equal(lm_zero$lambda, 0)
  expect_true(all(is.finite(lm_zero$estimates$std_error)))

  # Perfect LLM predictions (f == y_true identically on all observations)
  y_true <- y
  df_perf <- tibble::tibble(
    x = x,
    y = c(y_true[1:80], rep(NA_real_, n_tot - 80)),
    f = y_true
  )
  m_perf <- ppi_mean(df_perf, "y", "f")
  expect_true(is.finite(m_perf$lambda))
  expect_true(is.finite(m_perf$estimates$std_error))
  expect_gt(m_perf$estimates$std_error, 0)

  lm_perf <- ppi_lm(y ~ x, data = df_perf, pred_col = "f")
  expect_true(is.finite(lm_perf$lambda))
  expect_gte(lm_perf$lambda, 0)
  expect_lte(lm_perf$lambda, 1)
  expect_true(all(is.finite(lm_perf$estimates$std_error)))
  expect_true(all(lm_perf$estimates$std_error > 0))

  # Small validation sample triggers warning and uses finite t df
  df_small <- tibble::tibble(
    x = x[1:120],
    y = c(y_true[1:12], rep(NA_real_, 108)),
    f = y_true[1:120] + rnorm(120, sd = 0.5)
  )
  expect_warning(m_sm <- ppi_mean(df_small, "y", "f"), "Small labeled")
  expect_equal(m_sm$df, 11)
  expect_warning(lm_sm <- ppi_lm(y ~ x, data = df_small, pred_col = "f"), "Small labeled")
  expect_equal(lm_sm$df, 10)

  # Non-finite Inf / NaN values rejected cleanly
  df_inf <- df_zero
  df_inf$f[5] <- Inf
  expect_error(ppi_lm(y ~ x, data = df_inf, pred_col = "f"), "non-finite")
})

test_that("ppi_lm remains unbiased and achieves nominal coverage under nonlinear heteroskedastic DPF bias", {
  set.seed(3104)
  n_sim <- 100L
  true_bx <- 1.0
  true_bg <- 0.5
  cov_x <- logical(n_sim)
  cov_g <- logical(n_sim)
  cov_naive_g <- logical(n_sim)

  for (s in seq_len(n_sim)) {
    n_tot <- 600L
    x <- rnorm(n_tot)
    g <- stats::rbinom(n_tot, 1L, 0.5)
    y <- 1.0 + true_bx * x + true_bg * g + rnorm(n_tot, sd = 0.8)
    # Nonlinear + heteroskedastic + subgroup DPF biased LLM
    f <- 0.4 + 0.8 * y + 0.25 * (x^2) + 0.7 * g + (0.3 + 0.4 * abs(x)) * rnorm(n_tot)
    df <- tibble::tibble(
      x = x, g = g,
      y = c(y[1:120], rep(NA_real_, n_tot - 120)),
      f = f
    )
    fit <- ppi_lm(y ~ x + g, data = df, pred_col = "f")
    rx <- fit$estimates[fit$estimates$term == "x", ]
    rg <- fit$estimates[fit$estimates$term == "g", ]
    rng <- fit$benchmarks[fit$benchmarks$method == "naive_llm" & fit$benchmarks$term == "g", ]
    cov_x[s] <- rx$conf_low <= true_bx && true_bx <= rx$conf_high
    cov_g[s] <- rg$conf_low <= true_bg && true_bg <= rg$conf_high
    cov_naive_g[s] <- rng$conf_low <= true_bg && true_bg <= rng$conf_high
  }

  expect_gt(mean(cov_x), 0.89)
  expect_gt(mean(cov_g), 0.89)
  expect_lt(mean(cov_naive_g), 0.15)
})

test_that("psychometric functions handle constant ratings, single-class categories, and >2 subgroups cleanly", {
  df_const <- tibble::tibble(
    unit_id = rep(paste0("u", 1:20), 2),
    prompt = rep(c("p1", "p2"), each = 20),
    score = rep(5.0, 40),
    human_score = rep(5.0, 40),
    group = factor(rep(c("a", "b"), 20)),
    version = rep(c("v1", "v2"), each = 20)
  )

  # Constant g_study warns cleanly and returns NA reliability (never hallucinating G=0.925)
  expect_warning(g_c <- g_study(df_const, "score", facets = "prompt"), "constant")
  expect_true(isTRUE(g_c$constant_scores))
  expect_true(is.na(g_c$g_coef))
  expect_true(is.na(g_c$phi_coef))
  expect_true(all(g_c$variance_components$variance == 0))

  # d_study handles degenerate constant g_study cleanly
  d_c <- d_study(g_c, grid = list(prompt = 1:4), target = 0.85)
  expect_equal(nrow(d_c$recommendations), 0L)
  expect_true(all(is.na(d_c$grid$g_coef)))

  # Constant calibration returns NA correlations without NaN/crash
  cal_c <- calibration_metrics(df_const, "score", "human_score")
  expect_true(is.na(cal_c$metrics$pearson_r))
  expect_equal(cal_c$metrics$rmse, 0)
  expect_equal(cal_c$metrics$bias, 0)

  # Single-class categorical calibration returns clean NA for kappa/alpha (no NaN)
  cat_one <- tibble::tibble(
    pred = factor(rep("low", 20), levels = c("low", "high")),
    truth = factor(rep("low", 20), levels = c("low", "high"))
  )
  cal_one <- calibration_metrics(cat_one, "pred", "truth", type = "categorical")
  expect_equal(cal_one$metrics$accuracy, 1)
  expect_true(is.na(cal_one$metrics$cohen_kappa))
  expect_true(is.na(cal_one$metrics$krippendorff_alpha))

  # Constant check_drift emits zero lm warnings and drift_detected = FALSE
  expect_no_warning(dr_c <- check_drift(df_const, "score", "version"))
  expect_false(dr_c$drift_detected)
  expect_equal(dr_c$overall_test$p_value, 1)

  # Constant dpf_test emits one clean warning (no lm summary spam), delta_r2 = 0, flagged = FALSE
  expect_warning(dpf_c <- dpf_test(df_const, "score", "human_score", "group"), "constant")
  expect_false(dpf_c$dpf_detected)
  expect_true(all(dpf_c$tests$delta_r2 == 0))
  expect_true(all(!dpf_c$tests$flagged))

  # Constant truth with real group difference in score still runs uniform test cleanly
  df_const_tr <- df_const
  df_const_tr$score <- ifelse(df_const_tr$group == "b", 6.5, 4.5) + rep(c(-0.1, 0.1), 20)
  dpf_ctr <- dpf_test(df_const_tr, "score", "human_score", "group")
  expect_true(dpf_ctr$dpf_detected)
  expect_true(is.na(dpf_ctr$tests$p_value[dpf_ctr$tests$test == "nonuniform"]))

  # >2 subgroups (4 subgroups) DPF
  set.seed(3105)
  n4 <- 200
  tr4 <- rnorm(n4)
  g4 <- factor(rep(c("g1", "g2", "g3", "g4"), times = c(60, 60, 60, 20)))
  sc4 <- tr4 + ifelse(g4 == "g3", 0.8, 0) + ifelse(g4 == "g4", -0.6, 0) + rnorm(n4, sd = 0.4)
  dpf4 <- dpf_test(tibble::tibble(s = sc4, t = tr4, g = g4), "s", "t", "g")
  expect_equal(nrow(dpf4$subgroup_summary), 4L)
  expect_true(dpf4$dpf_detected)
  expect_equal(dpf4$tests$df[dpf4$tests$test == "uniform"], 3)
})

test_that("ppi_sample_size and ppi_power are analytically exact, monotone, and pilot-aware", {
  # Monotonicity in r and exact r = 0 equivalence to human-only
  p_r0 <- ppi_sample_size(target_se = 0.10, sd = 1.2, r = 0, n_total = 2000)
  p_r6 <- ppi_sample_size(target_se = 0.10, sd = 1.2, r = 0.6, n_total = 2000)
  p_r9 <- ppi_sample_size(target_se = 0.10, sd = 1.2, r = 0.9, n_total = 2000)

  expect_s3_class(p_r0, "llm_ppi_sample_size")
  expect_equal(p_r0$n_labeled, p_r0$n_human_only)
  expect_equal(p_r0$annotations_saved, 0L)
  expect_lt(p_r6$n_labeled, p_r0$n_labeled)
  expect_lt(p_r9$n_labeled, p_r6$n_labeled)
  expect_lte(p_r9$achieved_se, 0.10 + 1e-12)

  # Power-based sizing hits >= 80% power
  p_pow <- ppi_sample_size(effect = 0.30, power = 0.80, sd = 1.2, r = 0.80, n_total = 1500)
  expect_true(p_pow$feasible)
  expect_gte(p_pow$achieved_power, 0.80)
  expect_gt(p_pow$savings_pct, 40)

  # Finite n_unlabeled mode vs n_total mode consistency
  p_nu <- ppi_sample_size(target_se = 0.10, sd = 1.2, r = 0.80, n_unlabeled = 1000)
  expect_lte(p_nu$achieved_se, 0.10 + 1e-12)
  expect_equal(p_nu$n_unlabeled, 1000L)

  # Infeasible target below corpus floor triggers warning and feasible = FALSE
  expect_warning(
    p_inf <- ppi_sample_size(target_se = 0.01, sd = 1.2, r = 0.90, n_total = 200),
    "irreducible corpus floor"
  )
  expect_false(p_inf$feasible)
  expect_true(is.na(p_inf$n_labeled))
  expect_equal(p_inf$min_n_total_needed, 14400L)

  # Pilot extraction from llm_ppi and llm_calibration
  data(ema_coded)
  agg <- aggregate_scores(ema_coded, score_col = "llm_score")
  pil_lm <- ppi_lm(human_score ~ x + group, data = agg, pred_col = "llm_score")
  plan_lm <- ppi_sample_size(target_se = 0.06, n_total = 1500, pilot = pil_lm, term = "x")
  expect_true(plan_lm$feasible)
  expect_gt(plan_lm$params$r, 0.5)
  expect_gt(plan_lm$savings_pct, 25)

  pil_cal <- calibration_metrics(agg, pred_col = "llm_score", truth_col = "human_score")
  plan_cal <- ppi_sample_size(target_se = 0.10, n_total = 1500, pilot = pil_cal)
  expect_true(plan_cal$feasible)
  expect_equal(plan_cal$params$r, abs(pil_cal$metrics$pearson_r), tolerance = 1e-8)

  # ppi_power grid + autoplot rendering
  pw <- ppi_power(
    n_labeled = c(40, 80, 120, 160),
    n_total = 1500,
    effect = 0.25,
    sd = 1.2,
    r = c(0.5, 0.85)
  )
  expect_s3_class(pw, "llm_ppi_power")
  expect_equal(nrow(pw$grid), 8L)
  expect_true(all(pw$grid$power_ppi >= pw$grid$power_human - 1e-12))
  expect_true(all(pw$grid$se_ppi <= pw$grid$se_human + 1e-12))

  p1 <- autoplot(pw, metric = "power")
  p2 <- autoplot(pw, metric = "se")
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p1))
  expect_no_error(ggplot2::ggplot_build(p2))
})
