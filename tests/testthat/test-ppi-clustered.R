test_that("ppi_clustered handles disjoint and overlapping clusters and accounts for ICC", {
  set.seed(2001)
  G <- 80
  m <- 8
  cid <- rep(paste0("c", seq_len(G)), each = m)
  u <- rep(rnorm(G, sd = 1.2), each = m)
  # Cluster-level predictor x_c (strongly affected by ICC!)
  x_c <- rep(rnorm(G), each = m)
  y_true <- 1.0 + 1.0 * x_c + u + rnorm(G * m, sd = 0.6)
  y_llm <- 0.5 + 0.85 * y_true + 0.35 * x_c + rnorm(G * m, sd = 0.4)

  # Disjoint clusters: first 30 clusters validated, remaining 50 unlabeled
  df_disj <- tibble::tibble(
    cluster = cid,
    x_c = x_c,
    human_y = ifelse(cid %in% paste0("c", 1:30), y_true, NA_real_),
    llm_y = y_llm
  )
  fit_cl_disj <- ppi_clustered(human_y ~ x_c, data = df_disj, pred_col = "llm_y", cluster = "cluster")
  fit_iid <- ppi_lm(human_y ~ x_c, data = df_disj, pred_col = "llm_y")

  expect_s3_class(fit_cl_disj, "llm_ppi_clustered")
  expect_equal(fit_cl_disj$cluster_structure, "disjoint")
  expect_equal(fit_cl_disj$n_clusters_labeled, 30L)
  expect_equal(fit_cl_disj$n_clusters_unlabeled, 50L)

  # Cluster-robust SE for cluster-level predictor x_c must be larger than naive i.i.d. SE
  se_cl <- fit_cl_disj$estimates$std_error[fit_cl_disj$estimates$term == "x_c"]
  se_iid <- fit_iid$estimates$std_error[fit_iid$estimates$term == "x_c"]
  expect_gt(se_cl, se_iid * 1.4)

  # Overlapping clusters: 30% of observations within each cluster validated
  is_val <- stats::rbinom(G * m, 1L, 0.35) == 1L
  df_ov <- tibble::tibble(
    cluster = cid,
    x_c = x_c,
    human_y = ifelse(is_val, y_true, NA_real_),
    llm_y = y_llm
  )
  fit_cl_ov <- ppi_clustered(human_y ~ x_c, data = df_ov, pred_col = "llm_y", cluster = "cluster")
  expect_equal(fit_cl_ov$cluster_structure, "overlapping")
  expect_true(all(is.finite(fit_cl_ov$estimates$std_error)))

  # Binomial clustered PPI++
  p_bin <- stats::plogis(-0.2 + 0.9 * x_c + 0.7 * u)
  df_bin_cl <- tibble::tibble(
    cluster = cid,
    x_c = x_c,
    human_y = ifelse(cid %in% paste0("c", 1:35), stats::rbinom(G * m, 1L, p_bin), NA_real_),
    llm_p = stats::plogis(0.4 + 0.6 * x_c + 0.6 * u + rnorm(G * m, sd = 0.3))
  )
  fit_cl_bin <- ppi_clustered(
    human_y ~ x_c, data = df_bin_cl, pred_col = "llm_p",
    cluster = "cluster", family = stats::binomial()
  )
  expect_equal(fit_cl_bin$family, "binomial")
  expect_true(all(is.finite(fit_cl_bin$estimates$std_error)))
})

test_that("ppi_lmer parses random-intercept syntax and supports Woodbury GLS and overlapping weights", {
  set.seed(2002)
  G <- 70
  m <- 6
  cid <- rep(paste0("p", seq_len(G)), each = m)
  u <- rep(rnorm(G, sd = 1.0), each = m)
  x_within <- rnorm(G * m)
  y_true <- 2.0 + 1.1 * x_within + u + rnorm(G * m, sd = 0.6)
  y_llm <- 0.4 + 0.85 * y_true + rnorm(G * m, sd = 0.4)

  # Disjoint clusters -> Woodbury GLS
  df_disj <- tibble::tibble(
    person = cid,
    x = x_within,
    human_y = ifelse(cid %in% paste0("p", 1:25), y_true, NA_real_),
    llm_y = y_llm
  )
  fit_gls <- ppi_lmer(human_y ~ x + (1 | person), data = df_disj, pred_col = "llm_y")
  expect_s3_class(fit_gls, "llm_ppi_lmer")
  expect_equal(fit_gls$weight_mode, "gls_woodbury")
  expect_equal(nrow(fit_gls$variance_components), 2L)
  expect_gt(fit_gls$variance_components$icc[1L], 0.4)

  # Equivalent call using fixed formula + explicit cluster argument
  fit_gls2 <- ppi_lmer(human_y ~ x, data = df_disj, pred_col = "llm_y", cluster = "person")
  expect_equal(coef(fit_gls), coef(fit_gls2))

  # Overlapping clusters -> cluster-size-weighted GLS
  is_val <- rep(c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE), G)
  df_ov <- tibble::tibble(
    person = cid,
    x = x_within,
    human_y = ifelse(is_val, y_true, NA_real_),
    llm_y = y_llm
  )
  fit_ov <- ppi_lmer(human_y ~ x + (1 | person), data = df_ov, pred_col = "llm_y")
  expect_equal(fit_ov$cluster_structure, "overlapping")
  expect_equal(fit_ov$weight_mode, "gls_cluster_size_weighted")
  expect_true(all(is.finite(fit_ov$estimates$std_error)))
})
