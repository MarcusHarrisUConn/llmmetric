test_that("select_validation_sample supports all 5 strategies with exact HT weights and RNG safety", {
  set.seed(5001)
  M <- 300L
  df <- tibble::tibble(
    id = paste0("u", seq_len(M)),
    group = factor(rep(c("majority", "minority_a", "minority_b"), times = c(210, 60, 30))),
    cluster = rep(paste0("cl_", seq_len(30)), each = 10),
    llm_score = c(rnorm(210, 2, 0.6), rnorm(60, 4, 1.8), rnorm(30, 5, 2.5))
  )

  # Verify zero global .Random.seed mutation
  seed_before <- .Random.seed
  vs_simple <- select_validation_sample(df, n = 60, strategy = "simple", seed = 101)
  expect_identical(seed_before, .Random.seed)
  expect_s3_class(vs_simple, "llm_validation_sample")
  expect_equal(vs_simple$n_selected, 60L)
  expect_equal(sum(vs_simple$weights[vs_simple$selected]), M, tolerance = 1e-8)

  # Equal-per-stratum stratified (ideal for DPF subgroup power)
  vs_eq <- select_validation_sample(
    df, n = 60, strategy = "stratified", strata_col = "group",
    allocation = "equal", seed = 102
  )
  expect_equal(vs_eq$n_selected, 60L)
  expect_equal(vs_eq$stratum_summary$n_selected, c(20L, 20L, 20L))
  expect_equal(sum(vs_eq$weights[vs_eq$selected]), M, tolerance = 1e-8)

  # Proportional stratified
  vs_prop <- select_validation_sample(
    df, n = 60, strategy = "stratified", strata_col = "group",
    allocation = "proportional", seed = 103
  )
  expect_equal(vs_prop$n_selected, 60L)
  expect_equal(sum(vs_prop$weights[vs_prop$selected]), M, tolerance = 1e-8)

  # Neyman-optimal allocation allocates higher sampling rate to high-SD minority strata
  vs_ney <- select_validation_sample(
    df, n = 60, strategy = "neyman", strata_col = "group",
    score_col = "llm_score", seed = 104
  )
  expect_equal(vs_ney$n_selected, 60L)
  expect_equal(sum(vs_ney$weights[vs_ney$selected]), M, tolerance = 1e-8)
  pi_maj <- vs_ney$stratum_summary$inclusion_prob[vs_ney$stratum_summary$stratum == "majority"]
  pi_min <- vs_ney$stratum_summary$inclusion_prob[vs_ney$stratum_summary$stratum == "minority_b"]
  expect_gt(pi_min, pi_maj)

  # Quantile stratified & Neyman without strata_col
  vs_q <- select_validation_sample(df, n = 60, strategy = "quantile", score_col = "llm_score",
                                   allocation = "equal", n_quantiles = 4L, seed = 105)
  expect_equal(nrow(vs_q$stratum_summary), 4L)
  expect_equal(vs_q$n_selected, 60L)
  expect_equal(sum(vs_q$weights[vs_q$selected]), M, tolerance = 1e-8)

  vs_ney_q <- select_validation_sample(df, n = 60, strategy = "neyman", score_col = "llm_score", seed = 106)
  expect_equal(vs_ney_q$n_selected, 60L)

  # Cluster-stratified sampling
  vs_cl <- select_validation_sample(df, n_clusters = 8L, strategy = "cluster", cluster_col = "cluster", seed = 107)
  expect_equal(vs_cl$n_selected, 80L)
  expect_equal(vs_cl$stratum_summary$n_clusters_selected, 8L)
  expect_equal(unique(vs_cl$inclusion_prob), 8 / 30)

  vs_cl2 <- select_validation_sample(df, n = 60, strategy = "cluster", cluster_col = "cluster", seed = 107)
  expect_equal(vs_cl2$stratum_summary$n_clusters_selected, 6L)

  # Error checks
  expect_error(select_validation_sample(df, n = 20, strategy = "stratified"), "strata_col")
  expect_error(select_validation_sample(df, n = 20, strategy = "quantile"), "score_col")
  expect_error(select_validation_sample(df, n = 20, strategy = "cluster"), "cluster_col")
  expect_no_error(capture.output(print(vs_ney)))
})

test_that("Monte Carlo proof: IPW ppi_mean and ppi_lm maintain ~95% coverage under non-uniform stratified sampling", {
  set.seed(5002)
  n_sim <- 140L
  M <- 800L
  true_beta_x <- 1.15
  # Population has 80% stratum A (low mean) and 20% stratum B (high mean), sampled EQUALLY (4x weight imbalance!)
  n_A <- 640L
  n_B <- 160L
  true_mean <- 0.8 * 1.0 + 0.2 * 4.0 # = 1.60

  cov_ipw_mean <- logical(n_sim)
  cov_naive_mean <- logical(n_sim)
  cov_ipw_lm <- logical(n_sim)
  cov_naive_lm <- logical(n_sim)
  se_ipw_lm <- numeric(n_sim)
  se_hum_lm <- numeric(n_sim)

  for (r in seq_len(n_sim)) {
    strata <- factor(c(rep("A", n_A), rep("B", n_B)))
    x <- rnorm(M)
    # Heterogeneous mean & slope across strata (true population marginal slope of x is 1.15)
    y <- ifelse(strata == "A", 1.0, 4.0) + true_beta_x * x + rnorm(M, sd = 0.85)
    # Biased LLM with DPF + covariate shift
    f <- 0.5 + 0.8 * y + 0.4 * x + ifelse(strata == "B", 0.9, 0) + rnorm(M, sd = 0.40)

    pop <- tibble::tibble(strata = strata, x = x, y_true = y, llm = f)
    vs <- select_validation_sample(
      pop, n = 120L, strategy = "stratified",
      strata_col = "strata", allocation = "equal"
    )
    dat <- vs$data
    dat$human <- ifelse(dat$is_validation, dat$y_true, NA_real_)

    fm <- ppi_mean(dat, truth_col = "human", pred_col = "llm", inclusion_prob = "inclusion_prob")
    fl <- ppi_lm(human ~ x, data = dat, pred_col = "llm", inclusion_prob = "inclusion_prob")

    rm_ppi <- fm$estimates
    rm_nv <- fm$benchmarks[fm$benchmarks$method == "naive_llm", ]
    rl_ppi <- fl$estimates[fl$estimates$term == "x", ]
    rl_hum <- fl$benchmarks[fl$benchmarks$method == "human_only" & fl$benchmarks$term == "x", ]
    rl_nv <- fl$benchmarks[fl$benchmarks$method == "naive_llm" & fl$benchmarks$term == "x", ]

    cov_ipw_mean[r] <- rm_ppi$conf_low <= true_mean && true_mean <= rm_ppi$conf_high
    cov_naive_mean[r] <- rm_nv$conf_low <= true_mean && true_mean <= rm_nv$conf_high
    cov_ipw_lm[r] <- rl_ppi$conf_low <= true_beta_x && true_beta_x <= rl_ppi$conf_high
    cov_naive_lm[r] <- rl_nv$conf_low <= true_beta_x && true_beta_x <= rl_nv$conf_high
    se_ipw_lm[r] <- rl_ppi$std_error
    se_hum_lm[r] <- rl_hum$std_error
  }

  expect_gt(mean(cov_ipw_mean), 0.90)
  expect_lt(mean(cov_naive_mean), 0.15)
  expect_gt(mean(cov_ipw_lm), 0.90)
  expect_lt(mean(cov_naive_lm), 0.15)
  # IPW-PPI++ strictly beats IPW Human-Only SE
  expect_lt(mean(se_ipw_lm), mean(se_hum_lm) * 0.78)

  # Equivalence of `weights` vs `inclusion_prob` column vs numeric vector
  fm_w <- ppi_mean(dat, truth_col = "human", pred_col = "llm", weights = "sampling_weight")
  fm_v <- ppi_mean(dat, truth_col = "human", pred_col = "llm", weights = dat$sampling_weight)
  expect_equal(coef(fm), coef(fm_w))
  expect_equal(vcov(fm), vcov(fm_v))

  fl_w <- ppi_lm(human ~ x, data = dat, pred_col = "llm", weights = "sampling_weight")
  fl_p1 <- ppi_lm(human ~ x, data = dat, pred_col = "llm", weights = dat$sampling_weight, method = "ppi")
  expect_equal(coef(fl), coef(fl_w))
  expect_equal(fl_p1$lambda, 1)
  expect_error(ppi_mean(dat, "human", "llm", weights = "sampling_weight", inclusion_prob = "inclusion_prob"))
  expect_error(ppi_lm(human ~ x, data = dat, pred_col = "llm", weights = "sampling_weight", inclusion_prob = "inclusion_prob"))
  expect_no_error(capture.output(print(fl)))
})

test_that("ppi_calibrated improves nonlinear precision while maintaining nominal ~95% CI coverage", {
  set.seed(5003)
  n_sim <- 120L
  n_lab <- 150L
  n_unlab <- 850L
  true_beta <- 1.2

  cov_cal <- logical(n_sim)
  cov_naive <- logical(n_sim)
  se_cal <- numeric(n_sim)
  se_uncal <- numeric(n_sim)
  se_hum <- numeric(n_sim)
  r_gain <- numeric(n_sim)

  for (s in seq_len(n_sim)) {
    n_tot <- n_lab + n_unlab
    x <- rnorm(n_tot)
    y <- 1.0 + true_beta * x + rnorm(n_tot, sd = 0.85)
    # Severe non-linear S-curve + cubic/ceiling distortion
    f <- 6 * stats::plogis(1.4 * (y - 1.0) - 0.8 * x) + 0.4 * (y - 1.0)^2 / 4 + rnorm(n_tot, sd = 0.12)

    df <- tibble::tibble(
      x = x,
      y = c(y[seq_len(n_lab)], rep(NA_real_, n_unlab)),
      f = f
    )
    fit <- ppi_calibrated(y ~ x, data = df, pred_col = "f", calibrator = "spline", df = 4L, folds = 5L, seed = 100L + s)

    rc <- fit$estimates[fit$estimates$term == "x", ]
    ru <- fit$benchmarks[fit$benchmarks$method == "ppi++_uncalibrated" & fit$benchmarks$term == "x", ]
    rh <- fit$benchmarks[fit$benchmarks$method == "human_only" & fit$benchmarks$term == "x", ]
    rn <- fit$benchmarks[fit$benchmarks$method == "naive_llm" & fit$benchmarks$term == "x", ]

    cov_cal[s] <- rc$conf_low <= true_beta && true_beta <= rc$conf_high
    cov_naive[s] <- rn$conf_low <= true_beta && true_beta <= rn$conf_high
    se_cal[s] <- rc$std_error
    se_uncal[s] <- ru$std_error
    se_hum[s] <- rh$std_error
    r_gain[s] <- fit$calibration_diagnostics$r_calibrated_oof - fit$calibration_diagnostics$r_raw
  }

  # Maintains nominal ~95% coverage while dramatically shrinking SE vs BOTH human-only AND uncalibrated PPI++
  expect_gt(mean(cov_cal), 0.90)
  expect_lt(mean(cov_cal), 0.995)
  expect_lt(mean(cov_naive), 0.10)
  expect_lt(mean(se_cal), mean(se_uncal) * 0.85)
  expect_lt(mean(se_cal), mean(se_hum) * 0.65)
  expect_gt(mean(r_gain), 0.05)

  # Test poly, logistic (binomial), custom calibrator, RNG safety, autoplot, and APA table
  seed_before <- .Random.seed
  fit_poly <- ppi_calibrated(y ~ x, data = df, pred_col = "f", calibrator = "poly", df = 3L, seed = 7)
  expect_identical(seed_before, .Random.seed)
  expect_s3_class(fit_poly, "llm_ppi_calibrated")

  # Custom calibrator function
  custom_fn <- function(trn, newd, tcol, pcol, covs) {
    m <- stats::lm(stats::as.formula(paste(tcol, "~", pcol, "+ I(", pcol, "^2)")), data = trn)
    as.numeric(stats::predict(m, newdata = newd))
  }
  fit_cust <- ppi_calibrated(y ~ x, data = df, pred_col = "f", calibrator = custom_fn, seed = 7)
  expect_equal(fit_cust$calibration_diagnostics$calibrator, "custom")

  # Logistic calibrator on binary outcome
  set.seed(5004)
  xb <- rnorm(600)
  pb <- stats::plogis(-0.2 + 1.0 * xb)
  yb <- stats::rbinom(600, 1L, pb)
  fb <- stats::qlogis(pmin(pmax(pb, 0.01), 0.99)) + 0.6 * sin(xb) + rnorm(600, sd = 0.4)
  dfb <- tibble::tibble(x = xb, y = c(yb[1:150], rep(NA_real_, 450)), f = fb)
  fit_log <- ppi_calibrated(y ~ x, data = dfb, pred_col = "f", family = stats::binomial(),
                            calibrator = "logistic", df = 3L, folds = 4L)
  expect_true(all(is.finite(coef(fit_log))))

  # autoplot, APA table, and print methods work seamlessly
  expect_no_error(ggplot2::ggplot_build(autoplot(fit_poly)))
  tab <- format_apa_table(fit_poly, include_intercept = FALSE)
  expect_match(as.character(tab), "PPI\\+\\+ \\(Calibrated\\)")
  expect_match(as.character(tab), "PPI\\+\\+ \\(Uncal\\.\\)")
  expect_no_error(capture.output(print(fit_poly)))
})

test_that("backend_custom adapts arbitrary R callables into code_texts with caching and validation", {
  texts <- tibble::tibble(
    id = paste0("u", 1:6),
    text = c("a", "ab", "abc", "abcd", "abcde", "abcdef"),
    group = factor(rep(c("g1", "g2"), 3))
  )
  des <- coding_design(
    prompts = c(p1 = "P1: {text}", p2 = "P2: {text}"),
    models = "local_custom",
    schema = coding_schema(score = schema_field("numeric", min = 0, max = 20))
  )

  # 1. Named list return + cache hit verification
  calls <- 0L
  be_list <- backend_custom(
    fn = function(req) {
      calls <<- calls + 1L
      list(score = nchar(req$text) + if (req$prompt_id == "p2") 0.5 else 0)
    },
    name = "local_list_scorer",
    version = "1.0"
  )
  expect_s3_class(be_list, "llm_backend_custom")
  expect_no_error(capture.output(print(be_list)))

  mc <- coding_cache("memory")
  c1 <- code_texts(texts, "text", id_col = "id", design = des, backend = be_list, cache = mc)
  expect_equal(calls, 12L)
  expect_true(all(c1$scores$valid))

  # Re-run with a freshly constructed identical backend_custom hits cache 100%
  be_list2 <- backend_custom(
    fn = function(req) stop("should not be called on cache hit"),
    name = "local_list_scorer",
    version = "1.0"
  )
  expect_identical(be_list$id, be_list2$id)
  c2 <- code_texts(texts, "text", id_col = "id", design = des, backend = be_list2, cache = mc)
  expect_true(all(c2$scores$cache_hit))
  expect_equal(c1$scores$score, c2$scores$score)

  # 2. JSON string return, 1-row data.frame return, and envelope list(content=...) return
  be_json <- backend_custom(fn = function(req) '{"score": 3.5}', name = "json_fn")
  be_df <- backend_custom(fn = function(req) tibble::tibble(score = 4.5), name = "df_fn")
  be_env <- backend_custom(fn = function(req) list(content = list(score = 5.5), model = "wrapped"), name = "env_fn")

  des1 <- coding_design(prompts = c(p1 = "{text}"), models = "m", schema = des$schema)
  expect_equal(code_texts(texts[1:2, ], "text", id_col = "id", design = des1, backend = be_json)$scores$score, c(3.5, 3.5))
  expect_equal(code_texts(texts[1:2, ], "text", id_col = "id", design = des1, backend = be_df)$scores$score, c(4.5, 4.5))
  expect_equal(code_texts(texts[1:2, ], "text", id_col = "id", design = des1, backend = be_env)$scores$score, c(5.5, 5.5))

  # 3. Error on invalid custom backend return
  be_bad <- backend_custom(fn = function(req) 42, name = "bad_fn")
  expect_error(backend_score(be_bad, list(model = "m")), "must return")
  be_bad_df <- backend_custom(fn = function(req) tibble::tibble(score = c(1, 2)), name = "bad_df")
  expect_error(backend_score(be_bad_df, list(model = "m")), "expected exactly 1 row")
  expect_error(backend_custom("not_a_fn"), "must be an R function")
})
