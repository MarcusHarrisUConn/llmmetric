test_that("dpf_test distinguishes uniform, non-uniform, and null Gaussian DPF", {
  set.seed(303)
  n <- 300
  truth <- rnorm(n)
  grp <- factor(rep(c("A", "B"), each = n / 2))

  # Uniform DPF only (+0.7 shift for B)
  df_unif <- tibble::tibble(
    human_score = truth,
    group = grp,
    llm_score = truth + ifelse(grp == "B", 0.7, 0) + rnorm(n, sd = 0.4)
  )
  res_u <- dpf_test(df_unif, score_col = "llm_score", truth_col = "human_score", group_col = "group")
  expect_s3_class(res_u, "llm_dpf")
  expect_true(res_u$dpf_detected)
  expect_lt(res_u$tests$p_value[res_u$tests$test == "uniform"], 1e-6)
  expect_gt(res_u$tests$delta_r2[res_u$tests$test == "uniform"], 0.05)
  expect_lt(res_u$tests$delta_r2[res_u$tests$test == "nonuniform"], 0.01)
  expect_equal(res_u$tests$magnitude[res_u$tests$test == "nonuniform"], "negligible")
  b_row <- res_u$subgroup_summary[res_u$subgroup_summary$group == "B", ]
  expect_equal(b_row$cond_bias, 0.7, tolerance = 0.15)

  # Non-uniform DPF (slope difference for B)
  df_nu <- tibble::tibble(
    human_score = truth,
    group = grp,
    llm_score = truth + ifelse(grp == "B", -0.5 * truth, 0) + rnorm(n, sd = 0.4)
  )
  res_nu <- dpf_test(df_nu, score_col = "llm_score", truth_col = "human_score", group_col = "group")
  expect_true(res_nu$dpf_detected)
  expect_lt(res_nu$tests$p_value[res_nu$tests$test == "nonuniform"], 1e-6)
})

test_that("dpf_test supports binomial outcomes and prompt-level DPF screening", {
  set.seed(404)
  n <- 320
  truth <- rnorm(n)
  grp <- factor(rep(c("A", "B"), each = n / 2))
  prob <- stats::plogis(1.4 * truth + ifelse(grp == "B", 1.1, 0))
  df_bin <- tibble::tibble(
    human_score = truth,
    group = grp,
    llm_bin = stats::rbinom(n, 1L, prob)
  )
  res_bin <- dpf_test(df_bin, score_col = "llm_bin", truth_col = "human_score", group_col = "group")
  expect_equal(res_bin$family, "binomial")
  expect_true(res_bin$dpf_detected)
  expect_lt(res_bin$tests$p_value[res_bin$tests$test == "uniform"], 0.005)

  # Prompt-level screening: only prompt p3 has DPF, p1 and p2 are invariant
  units <- tibble::tibble(
    id = paste0("u", 1:100),
    text = paste("Text", 1:100),
    true_score = rnorm(100),
    human_score = NA_real_,
    group = factor(rep(c("A", "B"), each = 50))
  )
  units$human_score <- units$true_score
  pg_mat <- matrix(c(0, 0, 0, 0, 0, 1.0), nrow = 3, ncol = 2,
                   dimnames = list(c("p1", "p2", "p3"), c("A", "B")))
  des <- coding_design(prompts = c(p1 = "{text}", p2 = "{text}", p3 = "{text}"), models = "m1")
  be <- backend_mock(seed = 88, error_sd = 0.35, prompt_sd = 0.1, unit_prompt_sd = 0.15,
                     dpf_prompt_group = pg_mat)
  coded <- code_texts(units, "text", id_col = "id", design = des, backend = be)

  res_p <- dpf_test(coded, score_col = "score", truth_col = "human_score",
                    group_col = "group", prompt_col = "prompt", id_col = "unit_id")
  expect_true(res_p$clustered)
  expect_false(is.null(res_p$prompt_dpf))
  expect_false(is.null(res_p$prompt_interaction_test))
  expect_true(res_p$prompt_dpf$flagged[res_p$prompt_dpf$prompt == "p3"])
  expect_false(res_p$prompt_dpf$flagged[res_p$prompt_dpf$prompt == "p1"])
  expect_true(res_p$prompt_interaction_test$flagged)
})

test_that("dpf_test controls Type-I error near 5% under null (both i.i.d. and clustered)", {
  set.seed(505)
  n_rep <- 120L
  false_pos_iid <- 0L
  false_pos_cl <- 0L

  for (r in seq_len(n_rep)) {
    tr <- rnorm(100)
    gr <- factor(rep(c("A", "B"), each = 50))
    y_iid <- tr + rnorm(100, sd = 0.5)
    d1 <- dpf_test(tibble::tibble(s = y_iid, t = tr, g = gr), "s", "t", "g")
    if (d1$dpf_detected) false_pos_iid <- false_pos_iid + 1L

    # Repeated 3 prompts per unit sharing a unit-level random error (pseudoreplication test)
    u_err <- rnorm(100, sd = 0.5)
    cl_df <- tibble::tibble(
      id = rep(paste0("u", 1:100), 3),
      t = rep(tr, 3),
      g = rep(gr, 3),
      s = rep(tr + u_err, 3) + rnorm(300, sd = 0.25)
    )
    d2 <- dpf_test(cl_df, "s", "t", "g", id_col = "id")
    if (d2$dpf_detected) false_pos_cl <- false_pos_cl + 1L
  }

  rate_iid <- false_pos_iid / n_rep
  rate_cl <- false_pos_cl / n_rep
  expect_lt(rate_iid, 0.12)
  expect_lt(rate_cl, 0.12)
})
