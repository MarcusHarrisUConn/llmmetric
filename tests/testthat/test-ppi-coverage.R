# Monte Carlo coverage proofs verifying PPI / PPI++ maintain nominal ~95% CI
# coverage under biased and noisy LLM predictions where naive LLM regressions fail.

test_that("Monte Carlo proof: ppi_mean maintains ~95% CI coverage under biased LLM", {
  set.seed(9001)
  n_sim <- 200L
  n_lab <- 100L
  n_unlab <- 900L
  true_mu <- 3.0

  cov_ppi <- logical(n_sim)
  cov_naive <- logical(n_sim)
  err_ppi <- numeric(n_sim)
  err_hum <- numeric(n_sim)

  for (s in seq_len(n_sim)) {
    y <- rnorm(n_lab + n_unlab, mean = true_mu, sd = 1.2)
    # Biased & attenuated LLM score: E[f] = 1.2 + 0.75 * 3 = 3.45 (bias = +0.45)
    f <- 1.2 + 0.75 * y + rnorm(n_lab + n_unlab, sd = 0.5)
    df <- tibble::tibble(
      y = c(y[seq_len(n_lab)], rep(NA_real_, n_unlab)),
      f = f
    )
    fit <- ppi_mean(df, truth_col = "y", pred_col = "f")
    r_ppi <- fit$estimates
    r_hum <- fit$benchmarks[fit$benchmarks$method == "human_only", ]
    r_nv <- fit$benchmarks[fit$benchmarks$method == "naive_llm", ]

    cov_ppi[s] <- r_ppi$conf_low <= true_mu && true_mu <= r_ppi$conf_high
    cov_naive[s] <- r_nv$conf_low <= true_mu && true_mu <= r_nv$conf_high
    err_ppi[s] <- (r_ppi$estimate - true_mu)^2
    err_hum[s] <- (r_hum$estimate - true_mu)^2
  }

  # Nominal 95% coverage for PPI++, severe undercoverage for naive LLM
  expect_gt(mean(cov_ppi), 0.90)
  expect_lt(mean(cov_ppi), 0.99)
  expect_lt(mean(cov_naive), 0.50)
  # PPI++ achieves substantially lower MSE than human-only
  expect_lt(mean(err_ppi), mean(err_hum) * 0.65)
})

test_that("Monte Carlo proof: ppi_lm maintains ~95% CI coverage under covariate-biased LLM", {
  set.seed(9002)
  n_sim <- 200L
  n_lab <- 120L
  n_unlab <- 1000L
  true_beta <- 1.25

  cov_pp <- logical(n_sim)
  cov_p1 <- logical(n_sim)
  cov_naive <- logical(n_sim)
  se_pp <- numeric(n_sim)
  se_hum <- numeric(n_sim)

  for (s in seq_len(n_sim)) {
    n_tot <- n_lab + n_unlab
    x <- rnorm(n_tot)
    y <- 1.0 + true_beta * x + rnorm(n_tot, sd = 1.0)
    # Biased LLM with differential covariate bias (+0.35 * x) and noise
    f <- 0.5 + 0.85 * y + 0.35 * x + rnorm(n_tot, sd = 0.45)

    df <- tibble::tibble(
      x = x,
      y = c(y[seq_len(n_lab)], rep(NA_real_, n_unlab)),
      f = f
    )
    fit_pp <- ppi_lm(y ~ x, data = df, pred_col = "f", method = "ppi++")
    fit_p1 <- ppi_lm(y ~ x, data = df, pred_col = "f", method = "ppi")

    r_pp <- fit_pp$estimates[fit_pp$estimates$term == "x", ]
    r_p1 <- fit_p1$estimates[fit_p1$estimates$term == "x", ]
    r_hum <- fit_pp$benchmarks[fit_pp$benchmarks$method == "human_only" & fit_pp$benchmarks$term == "x", ]
    r_nv <- fit_pp$benchmarks[fit_pp$benchmarks$method == "naive_llm" & fit_pp$benchmarks$term == "x", ]

    cov_pp[s] <- r_pp$conf_low <= true_beta && true_beta <= r_pp$conf_high
    cov_p1[s] <- r_p1$conf_low <= true_beta && true_beta <= r_p1$conf_high
    cov_naive[s] <- r_nv$conf_low <= true_beta && true_beta <= r_nv$conf_high
    se_pp[s] <- r_pp$std_error
    se_hum[s] <- r_hum$std_error
  }

  expect_gt(mean(cov_pp), 0.90)
  expect_lt(mean(cov_pp), 0.99)
  expect_gt(mean(cov_p1), 0.90)
  expect_lt(mean(cov_naive), 0.25)
  expect_lt(mean(se_pp), mean(se_hum) * 0.75)
})

test_that("Monte Carlo proof: ppi_glm (binomial) maintains ~95% CI coverage under biased LLM", {
  set.seed(9003)
  n_sim <- 140L
  n_lab <- 180L
  n_unlab <- 1000L
  true_beta <- 1.0

  cov_pp <- logical(n_sim)
  cov_naive <- logical(n_sim)

  for (s in seq_len(n_sim)) {
    n_tot <- n_lab + n_unlab
    x <- rnorm(n_tot)
    p <- stats::plogis(-0.2 + true_beta * x)
    y <- stats::rbinom(n_tot, 1L, p)
    # Biased LLM classifier probabilities (attenuated slope + shift)
    f <- stats::plogis(0.6 + 0.55 * x + 0.7 * (y - p))

    df <- tibble::tibble(
      x = x,
      y = c(y[seq_len(n_lab)], rep(NA_real_, n_unlab)),
      f = f
    )
    fit <- ppi_glm(y ~ x, data = df, pred_col = "f", family = stats::binomial())
    r_pp <- fit$estimates[fit$estimates$term == "x", ]
    r_nv <- fit$benchmarks[fit$benchmarks$method == "naive_llm" & fit$benchmarks$term == "x", ]

    cov_pp[s] <- r_pp$conf_low <= true_beta && true_beta <= r_pp$conf_high
    cov_naive[s] <- r_nv$conf_low <= true_beta && true_beta <= r_nv$conf_high
  }

  expect_gt(mean(cov_pp), 0.90)
  expect_lt(mean(cov_pp), 0.99)
  expect_lt(mean(cov_naive), 0.15)
})

test_that("Monte Carlo proof: ppi_lmer maintains ~95% coverage for disjoint and overlapping clusters", {
  set.seed(9004)
  n_sim <- 120L
  G <- 80L
  m <- 5L
  true_beta <- 1.0

  cov_disj <- logical(n_sim)
  cov_ov <- logical(n_sim)
  cov_naive <- logical(n_sim)

  cid <- rep(paste0("c", seq_len(G)), each = m)
  disj_val <- cid %in% paste0("c", 1:30)

  for (s in seq_len(n_sim)) {
    u <- rep(rnorm(G, sd = 0.8), each = m)
    x <- rnorm(G * m)
    y <- 1.5 + true_beta * x + u + rnorm(G * m, sd = 0.7)
    # Biased LLM with covariate shift
    f <- 0.5 + 0.8 * y + 0.4 * x + rnorm(G * m, sd = 0.35)

    # Disjoint cluster validation (Woodbury GLS)
    df_d <- tibble::tibble(
      cl = cid, x = x,
      y = ifelse(disj_val, y, NA_real_),
      f = f
    )
    fit_d <- ppi_lmer(y ~ x + (1 | cl), data = df_d, pred_col = "f")
    r_d <- fit_d$estimates[fit_d$estimates$term == "x", ]
    r_nv <- fit_d$benchmarks[fit_d$benchmarks$method == "naive_llm" & fit_d$benchmarks$term == "x", ]
    cov_disj[s] <- r_d$conf_low <= true_beta && true_beta <= r_d$conf_high
    cov_naive[s] <- r_nv$conf_low <= true_beta && true_beta <= r_nv$conf_high

    # Overlapping within-cluster validation (2 of 5 entries labeled per cluster)
    ov_val <- rep(c(TRUE, TRUE, FALSE, FALSE, FALSE), G)
    df_o <- tibble::tibble(
      cl = cid, x = x,
      y = ifelse(ov_val, y, NA_real_),
      f = f
    )
    fit_o <- ppi_lmer(y ~ x + (1 | cl), data = df_o, pred_col = "f")
    r_o <- fit_o$estimates[fit_o$estimates$term == "x", ]
    cov_ov[s] <- r_o$conf_low <= true_beta && true_beta <= r_o$conf_high
  }

  expect_gt(mean(cov_disj), 0.89)
  expect_lt(mean(cov_disj), 0.995)
  expect_gt(mean(cov_ov), 0.89)
  expect_lt(mean(cov_ov), 0.995)
  expect_lt(mean(cov_naive), 0.25)
})
