# Comprehensive branch & S3 print/summary coverage tests across all modules

test_that("simulate_coding_study generates valid multi-facet studies across configurations", {
  sim <- simulate_coding_study(
    n_units = 36L,
    n_labeled = 16L,
    n_clusters = 9L,
    dpf_uniform = c(group_b = 0.5),
    dpf_nonuniform = c(group_b = -0.2),
    seed = 701L
  )
  expect_s3_class(sim$coded, "llm_coded_data")
  expect_equal(nrow(sim$unit_data), 36L)
  expect_equal(sum(!is.na(sim$unit_data$human_score)), 16L)
  expect_equal(length(sim$true_beta), 3L)
  expect_s3_class(as_tibble(sim$coded), "tbl_df")
})

test_that("all S3 print and summary methods execute cleanly without error", {
  s <- coding_schema(
    score = schema_field("numeric", min = 1, max = 7),
    cat = schema_field("factor", levels = c("a", "b")),
    note = schema_field("character", required = FALSE, nullable = TRUE)
  )
  expect_no_error(capture.output(print(s)))

  mc <- coding_cache("memory")
  expect_no_error(capture.output(print(mc)))
  tmp_d <- withr::local_tempdir()
  dc <- coding_cache("disk", dir = tmp_d)
  expect_no_error(capture.output(print(dc)))

  des <- coding_design(prompts = c(p1 = "{text}", p2 = "{text}"), models = c("m1", "m2"), schema = s)
  expect_no_error(capture.output(print(des)))

  be_m <- backend_mock(seed = 5)
  be_ol <- backend_ollama()
  expect_no_error(capture.output(print(be_m)))
  expect_no_error(capture.output(summary(be_m)))
  expect_no_error(capture.output(print(be_ol)))

  texts <- tibble::tibble(text = paste("T", 1:6), true_score = seq(-1, 1, length.out = 6))
  coded <- code_texts(
    texts, "text", id_col = NULL, design = des,
    backend = list(.default = be_m),
    keep_cols = FALSE, keep_raw = TRUE
  )
  expect_true("raw_output" %in% names(coded$scores))
  expect_no_error(capture.output(print(coded)))
  sm_c <- summary(coded)
  expect_no_error(capture.output(print(sm_c)))

  # Categorical primary score summary branch
  des_cat <- coding_design(
    prompts = c(p1 = "{text}"), models = "m1",
    schema = coding_schema(cat = schema_field("factor", levels = c("a", "b")))
  )
  coded_cat <- code_texts(texts, "text", design = des_cat, backend = be_m, keep_cols = c("true_score"))
  expect_no_error(capture.output(print(summary(coded_cat))))

  data(ema_coded)
  g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
  expect_no_error(capture.output(print(g)))
  expect_no_error(capture.output(summary(g)))

  d_default <- d_study(g) # default grid = NULL
  expect_no_error(capture.output(print(d_default)))
  d_partial <- d_study(g, grid = list(prompt = 1:3), target = 0.9999) # partial grid & unattainable target
  expect_no_error(capture.output(print(d_partial)))

  agg <- aggregate_scores(ema_coded, score_col = "llm_score")
  cal <- calibration_metrics(agg, pred_col = "llm_score", truth_col = "human_score", by = "group")
  expect_no_error(capture.output(print(cal)))

  dpf <- dpf_test(ema_coded, score_col = "llm_score", truth_col = "human_score",
                  group_col = "group", prompt_col = "prompt", id_col = "unit_id")
  expect_no_error(capture.output(print(dpf)))

  fit_lm <- ppi_lm(human_score ~ x + group, data = agg, pred_col = "llm_score")
  expect_no_error(capture.output(print(fit_lm)))
  expect_no_error(capture.output(summary(fit_lm)))
  expect_equal(nrow(confint(fit_lm, parm = "x")), 1L)

  fit_ml <- ppi_lmer(human_score ~ x + group + (1 | cluster), data = agg, pred_col = "llm_score")
  expect_no_error(capture.output(print(fit_ml)))

  plan_ok <- ppi_sample_size(effect = 0.25, power = 0.80, sd = 1.2, r = 0.8, n_total = 1500)
  expect_no_error(capture.output(print(plan_ok)))
  expect_warning(plan_bad <- ppi_sample_size(target_se = 0.005, sd = 1.2, r = 0.8, n_total = 100), "corpus floor")
  expect_no_error(capture.output(print(plan_bad)))

  pw <- ppi_power(n_labeled = c(40, 80), n_unlabeled = 500, sd = 1.2, r = 0.8)
  expect_no_error(capture.output(print(pw)))
  expect_no_error(ggplot2::ggplot_build(autoplot(pw)))
  expect_error(autoplot(pw, metric = "power"), "requires")

  tab <- format_apa_table(fit_lm)
  expect_no_error(capture.output(print(tab)))
  tab_t <- format_apa_table(fit_lm, format = "tibble")
  expect_no_error(capture.output(print(tab_t)))
})

test_that("dpf_test covers poly_degree >= 2, clustered binomial DPF, and unclustered prompt interaction", {
  set.seed(702)
  n <- 120
  tr <- rnorm(n)
  gr <- factor(rep(c("A", "B"), each = n / 2))

  # Quadratic matching (poly_degree = 2) -> exercises fixed t_c + I(t_c^2) branch
  df_poly <- tibble::tibble(
    human = tr,
    group = gr,
    llm = 0.5 + 0.9 * tr + 0.25 * (tr^2) + ifelse(gr == "B", 0.6, 0) + rnorm(n, sd = 0.35)
  )
  res_poly <- dpf_test(df_poly, "llm", "human", "group", poly_degree = 2L)
  expect_true(res_poly$dpf_detected)
  expect_true(all(is.finite(res_poly$subgroup_summary$cond_slope)))

  # Unclustered Gaussian prompt interaction
  df_up <- tibble::tibble(
    human = tr,
    group = gr,
    prompt = factor(rep(c("p1", "p2"), times = n / 2)),
    llm = tr + ifelse(gr == "B" & rep(c("p1", "p2"), times = n / 2) == "p2", 0.9, 0) + rnorm(n, sd = 0.3)
  )
  res_up <- dpf_test(df_up, "llm", "human", "group", prompt_col = "prompt")
  expect_false(res_up$clustered)
  expect_true(res_up$prompt_interaction_test$flagged)

  # Clustered binomial DPF with prompt_col + id_col
  ids <- rep(paste0("u", 1:80), 2)
  tr_b <- rep(rnorm(80), 2)
  gr_b <- factor(rep(rep(c("A", "B"), each = 40), 2))
  pr_b <- factor(rep(c("p1", "p2"), each = 80))
  p_b <- stats::plogis(1.2 * tr_b + ifelse(gr_b == "B" & pr_b == "p2", 1.5, 0))
  df_cb <- tibble::tibble(
    id = ids, human = tr_b, group = gr_b, prompt = pr_b,
    llm = factor(ifelse(stats::rbinom(160, 1L, p_b) == 1L, "yes", "no"))
  )
  res_cb <- dpf_test(df_cb, "llm", "human", "group", prompt_col = "prompt", id_col = "id")
  expect_equal(res_cb$family, "binomial")
  expect_true(res_cb$clustered)
  expect_false(is.null(res_cb$prompt_interaction_test))

  # Unclustered binomial prompt interaction
  res_ub <- dpf_test(df_cb[1:120, ], "llm", "human", "group", prompt_col = "prompt")
  expect_false(res_ub$clustered)
  expect_false(is.null(res_ub$prompt_interaction_test))
})

test_that("additional branches in schema, coding checkpoints, drift, human_irr, ppi_glm, ppi_power, and reporting work cleanly", {
  # Schema: list input, missing required field, nullable NULL, non-nullable NULL, coercion of factor & character
  s <- coding_schema(
    score = schema_field("numeric", min = 0, max = 10),
    cnt = schema_field("integer", min = 0, max = 10, nullable = TRUE),
    lab = schema_field("factor", levels = c("1", "2")),
    note = schema_field("character", required = FALSE, nullable = TRUE)
  )
  expect_true(validate_coded_output(list(score = 5, cnt = NULL, lab = "1", note = NULL), s)$valid)
  expect_false(validate_coded_output(list(score = NULL, cnt = 1, lab = "1"), s)$valid)
  expect_false(validate_coded_output(list(cnt = 1, lab = "1"), s)$valid)
  expect_true(validate_coded_output(list(score = "5", cnt = "2", lab = 1, note = 99), s, coerce = TRUE)$valid)
  expect_false(validate_coded_output(12345, s)$valid)
  expect_false(validate_coded_output('{"score": 5, "cnt": 1, "lab": "1"', s)$valid)
  expect_error(schema_field("character", min = 1))
  expect_error(schema_field("numeric", levels = c("a", "b")))
  expect_error(coding_schema(a = 123))

  # Corrupted checkpoint file is ignored with warning and coding succeeds cleanly
  tmp_d <- withr::local_tempdir()
  bad_ckpt <- file.path(tmp_d, "corrupt_ckpt.rds")
  writeBin(charToRaw("CORRUPTED"), bad_ckpt)
  tdf <- tibble::tibble(id = c("u1", "u2"), text = c("a", "b"))
  des <- coding_design(prompts = c(p1 = "{text}"), models = "m")
  expect_warning(
    cc <- code_texts(tdf, "text", id_col = "id", design = des, backend = backend_mock(seed = 1),
                     checkpoint_file = bad_ckpt),
    "corrupted checkpoint"
  )
  expect_true(all(cc$scores$valid))

  # check_drift with sparse (< 3) shared anchors + custom baseline + print
  dr_sparse <- tibble::tibble(
    unit_id = c("u1", "u2", "u3", "u4", "u1", "u5", "u6", "u7"),
    version = c("v1", "v1", "v1", "v1", "v2", "v2", "v2", "v2"),
    score = c(1, 2, 3, 4, 2, 3, 4, 5)
  )
  dr_s <- check_drift(dr_sparse, "score", "version", baseline = "v2")
  expect_equal(dr_s$baseline, "v2")
  expect_no_error(capture.output(print(dr_s)))

  # human_irr: 2-rater categorical (exercises Cohen's kappa), constant continuous, scale mismatch, and APA table
  set.seed(703)
  cat2 <- tibble::tibble(
    h1 = factor(sample(c("a", "b", "c"), 40, replace = TRUE)),
    h2 = factor(sample(c("a", "b", "c"), 40, replace = TRUE))
  )
  cat2$h2[1:25] <- cat2$h1[1:25]
  irr2 <- human_irr(cat2, rater_cols = c("h1", "h2"), type = "categorical")
  expect_true(is.finite(irr2$metrics$cohen_kappa))
  expect_no_error(capture.output(print(irr2)))
  expect_s3_class(format_apa_table(irr2), "llm_apa_table")

  const_h <- tibble::tibble(h1 = rep(3, 15), h2 = rep(3, 15))
  expect_warning(irr_const <- human_irr(const_h, rater_cols = c("h1", "h2")), "constant")
  expect_true(is.na(irr_const$metrics$icc_a1))

  cont_h <- tibble::tibble(h1 = rnorm(25), h2 = rnorm(25))
  cal_cat <- calibration_metrics(cat2, pred_col = "h2", truth_col = "h1", type = "categorical")
  expect_warning(
    irr_mis <- human_irr(cont_h, rater_cols = c("h1", "h2"), llm_calibration = cal_cat),
    "Scale mismatch"
  )
  expect_null(irr_mis$benchmark)

  # Format APA table for human_irr WITH benchmark
  z <- rnorm(40)
  wdf <- tibble::tibble(h1 = z + rnorm(40, sd = 0.3), h2 = z + rnorm(40, sd = 0.3), llm = z + rnorm(40, sd = 0.4))
  cal_w <- calibration_metrics(wdf, "llm", "h1")
  irr_w <- human_irr(wdf, rater_cols = c("h1", "h2"), llm_calibration = cal_w)
  expect_no_error(capture.output(print(irr_w)))
  expect_match(as.character(format_apa_table(irr_w)), "Human Ceiling")

  # ppi_glm with method = "calibrated" and custom lambda
  set.seed(704)
  n_t <- 500
  xg <- rnorm(n_t)
  yg <- stats::rbinom(n_t, 1L, stats::plogis(-0.2 + 1.0 * xg))
  fg <- stats::plogis(-0.2 + 0.8 * xg + 0.4)
  dfg <- tibble::tibble(x = xg, y = c(yg[1:120], rep(NA_real_, n_t - 120)), f = fg)
  fit_gcal <- ppi_glm(y ~ x, data = dfg, pred_col = "f", family = stats::binomial(), method = "calibrated")
  fit_glam <- ppi_glm(y ~ x, data = dfg, pred_col = "f", family = stats::binomial(), lambda = 0.5)
  expect_s3_class(fit_gcal, "llm_ppi_glm")
  expect_equal(fit_glam$lambda, 0.5)

  # ppi_sample_size edge modes: n_unlabeled = 0 and n_unlabeled = Inf
  ps0 <- ppi_sample_size(target_se = 0.1, sd = 1.0, r = 0.8, n_unlabeled = 0)
  expect_equal(ps0$n_labeled, ps0$n_human_only)
  ps_inf <- ppi_sample_size(target_se = 0.1, sd = 1.0, r = 0.8, n_unlabeled = Inf)
  expect_lt(ps_inf$n_labeled, ps0$n_labeled)
})
