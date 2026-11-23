test_that("ppi_mean matches analytical formulas, optimal lambda, and degrades safely on noise", {
  set.seed(1001)
  n_tot <- 1000
  y_true <- rnorm(n_tot, mean = 4.0, sd = 1.5)
  y_llm <- 1.2 + 0.8 * y_true + rnorm(n_tot, sd = 0.5)
  y_noise <- rnorm(n_tot, mean = 10, sd = 3)

  df <- tibble::tibble(
    human = c(y_true[1:120], rep(NA_real_, n_tot - 120)),
    llm = y_llm,
    noise = y_noise
  )

  # Separate unlabeled vs single-df NA split must be identical
  f_single <- ppi_mean(df, truth_col = "human", pred_col = "llm")
  f_split <- ppi_mean(df[1:120, ], truth_col = "human", pred_col = "llm", unlabeled = df[121:n_tot, ])
  expect_equal(coef(f_single), coef(f_split))
  expect_equal(vcov(f_single), vcov(f_split))

  # lambda = 0 equals human_only exactly
  f_zero <- ppi_mean(df, truth_col = "human", pred_col = "llm", lambda = 0)
  hum_row <- f_zero$benchmarks[f_zero$benchmarks$method == "human_only", ]
  expect_equal(unname(coef(f_zero)), hum_row$estimate, tolerance = 1e-12)
  expect_equal(f_zero$estimates$std_error, hum_row$std_error, tolerance = 1e-12)

  # PPI++ beats human-only SE substantially on informative LLM
  expect_gt(f_single$efficiency$var_ratio_vs_human, 2.0)

  # On pure noise LLM, optimal lambda shrinks near 0 and SE stays close to human-only
  f_noise <- ppi_mean(df, truth_col = "human", pred_col = "noise")
  expect_lt(abs(f_noise$lambda), 0.25)
  expect_gt(f_noise$efficiency$var_ratio_vs_human, 0.90)
})

test_that("ppi_lm supports ppi++, ppi, calibrated, and LLM-coded predictors via pred_map", {
  set.seed(1002)
  n_tot <- 1000
  x <- rnorm(n_tot)
  z <- rnorm(n_tot)
  y_true <- 1.5 + 1.2 * x - 0.8 * z + rnorm(n_tot, sd = 0.9)
  # Biased & covariate-distorted LLM outcome
  y_llm <- 0.7 + 0.75 * y_true + 0.5 * x + rnorm(n_tot, sd = 0.45)

  df <- tibble::tibble(
    x = x,
    z = z,
    human_y = c(y_true[1:150], rep(NA_real_, n_tot - 150)),
    llm_y = y_llm
  )

  fit_pp <- ppi_lm(human_y ~ x + z, data = df, pred_col = "llm_y", method = "ppi++")
  fit_p1 <- ppi_lm(human_y ~ x + z, data = df, pred_col = "llm_y", method = "ppi")
  fit_cal <- ppi_lm(human_y ~ x + z, data = df, pred_col = "llm_y", method = "calibrated")

  expect_s3_class(fit_pp, "llm_ppi_lm")
  expect_equal(fit_p1$lambda, 1)
  expect_gte(fit_pp$lambda, 0)
  expect_lte(fit_pp$lambda, 1)
  expect_equal(length(fit_cal$lambda), 3L)

  # S3 contract
  expect_equal(length(coef(fit_pp)), 3L)
  expect_equal(dim(vcov(fit_pp)), c(3L, 3L))
  ci <- confint(fit_pp)
  expect_equal(dim(ci), c(3L, 2L))
  expect_true(all(ci[, 1] < ci[, 2]))

  # PPI++ and calibrated both recover true slope x = 1.2 within 95% CI while naive LLM is biased
  naive_x <- fit_pp$benchmarks[fit_pp$benchmarks$method == "naive_llm" & fit_pp$benchmarks$term == "x", ]
  expect_gt(abs(naive_x$estimate - 1.2), 0.12)
  expect_true(ci["x", 1] < 1.2 && 1.2 < ci["x", 2])

  # LLM-coded predictor via pred_map removes attenuation bias
  x_llm <- 0.4 + 0.6 * x + rnorm(n_tot, sd = 0.6)
  df_pred <- tibble::tibble(
    y = y_true,
    z = z,
    x = c(x[1:150], rep(NA_real_, n_tot - 150)),
    llm_x = x_llm
  )
  fit_rhs <- ppi_lm(y ~ x + z, data = df_pred, pred_map = c(x = "llm_x"))
  ci_rhs <- confint(fit_rhs)
  expect_true(ci_rhs["x", 1] < 1.2 && 1.2 < ci_rhs["x", 2])
})

test_that("ppi_glm supports binomial and poisson canonical GLMs cleanly", {
  set.seed(1003)
  n_tot <- 1200
  x <- rnorm(n_tot)

  # Binomial (logistic) with biased LLM predicted probabilities
  p_true <- stats::plogis(-0.4 + 1.2 * x)
  y_bin <- stats::rbinom(n_tot, 1L, p_true)
  p_llm <- stats::plogis(0.5 + 0.8 * x + rnorm(n_tot, sd = 0.4))
  df_bin <- tibble::tibble(
    x = x,
    y = c(y_bin[1:200], rep(NA_real_, n_tot - 200)),
    llm_p = p_llm
  )
  fit_bin <- ppi_glm(y ~ x, data = df_bin, pred_col = "llm_p", family = stats::binomial())
  expect_s3_class(fit_bin, "llm_ppi_glm")
  expect_true(fit_bin$converged)
  ci_bin <- confint(fit_bin)
  expect_true(ci_bin["x", 1] < 1.2 && 1.2 < ci_bin["x", 2])

  # Poisson with biased LLM counts
  mu_pois <- exp(0.4 + 0.5 * x)
  y_pois <- stats::rpois(n_tot, mu_pois)
  yhat_pois <- pmax(0.1, 1.3 * y_pois + 0.5 + rnorm(n_tot, sd = 0.6))
  df_pois <- tibble::tibble(
    x = x,
    y = c(y_pois[1:200], rep(NA_real_, n_tot - 200)),
    llm_cnt = yhat_pois
  )
  fit_pois <- ppi_glm(y ~ x, data = df_pois, pred_col = "llm_cnt", family = "poisson")
  expect_true(fit_pois$converged)
  ci_pois <- confint(fit_pois)
  expect_true(ci_pois["x", 1] < 0.5 && 0.5 < ci_pois["x", 2])
})
