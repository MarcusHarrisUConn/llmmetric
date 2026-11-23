test_that("PPI estimators handle complex interactions, polynomials, and mismatched factor levels cleanly", {
  set.seed(4101)
  n_tot <- 500
  x1 <- rnorm(n_tot)
  x2 <- rnorm(n_tot)
  grp <- factor(sample(c("ctrl", "trt_a", "trt_b"), n_tot, replace = TRUE), levels = c("ctrl", "trt_a", "trt_b"))
  y_true <- 1.0 + 0.9 * x1 - 0.5 * x2 + 0.6 * x1 * x2 +
    ifelse(grp == "trt_a", 0.7, ifelse(grp == "trt_b", -0.4, 0)) + rnorm(n_tot, sd = 0.8)
  y_llm <- 0.4 + 0.85 * y_true + 0.3 * x1 + rnorm(n_tot, sd = 0.4)

  df <- tibble::tibble(
    x1 = x1, x2 = x2, grp = grp,
    y = c(y_true[1:140], rep(NA_real_, n_tot - 140)),
    f = y_llm
  )

  # Interaction + factor + quadratic term
  fit_int <- ppi_lm(y ~ x1 * x2 + factor(grp) + I(x1^2), data = df, pred_col = "f")
  expect_s3_class(fit_int, "llm_ppi_lm")
  expect_true("x1:x2" %in% names(coef(fit_int)))
  ci <- confint(fit_int)
  expect_true(ci["x1:x2", 1] < 0.6 && 0.6 < ci["x1:x2", 2])

  # Unseen factor level in unlabeled data emits actionable warning and aliases cleanly without crash
  lab_df <- df[1:140, ]
  lab_df <- lab_df[lab_df$grp != "trt_b", ]
  lab_df$grp <- droplevels(lab_df$grp)
  unlab_df <- df[141:n_tot, ]
  unlab_df$grp <- as.character(unlab_df$grp) # mixed factor vs character across splits!

  warns <- character()
  fit_unseen <- withCallingHandlers(
    ppi_lm(y ~ x1 + grp, data = lab_df, unlabeled = unlab_df, pred_col = "f"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("never observed in the labeled validation sample", warns)))
  expect_true(is.na(coef(fit_unseen)["grptrt_b"]))
  expect_true(is.finite(coef(fit_unseen)["x1"]))
  expect_true(is.finite(coef(fit_unseen)["grptrt_a"]))

  # Constant 1-level factor errors with clear message instead of cryptic contrasts error
  df_one <- df
  df_one$grp <- factor("only_one")
  expect_error(ppi_lm(y ~ x1 + grp, data = df_one, pred_col = "f"), "fewer than 2 distinct levels")
})

test_that("ppi_proportion estimates multi-class, probability, and binary prevalences with valid coverage", {
  set.seed(4102)
  lvls <- c("low", "moderate", "high")
  true_p <- c(low = 0.50, moderate = 0.30, high = 0.20)
  n_lab <- 140L
  n_unlab <- 860L

  # Single run: multi-class discrete labels + simultaneous CIs + soft probabilities
  n_tot <- n_lab + n_unlab
  y_t <- factor(sample(lvls, n_tot, replace = TRUE, prob = true_p), levels = lvls)
  y_f <- y_t
  flip <- (y_t == "moderate") & (runif(n_tot) < 0.50)
  y_f[flip] <- "high"

  df_prop <- tibble::tibble(
    human = factor(c(as.character(y_t[seq_len(n_lab)]), rep(NA_character_, n_unlab)), levels = lvls),
    llm = y_f
  )

  fit_marg <- ppi_proportion(df_prop, truth_col = "human", pred_col = "llm")
  fit_sim <- ppi_proportion(df_prop, truth_col = "human", pred_col = "llm", simultaneous = TRUE)

  expect_s3_class(fit_marg, "llm_ppi_proportion")
  expect_equal(names(coef(fit_marg)), lvls)
  expect_equal(dim(vcov(fit_marg)), c(3L, 3L))
  # Simultaneous CIs must be strictly wider than marginal CIs
  w_m <- fit_marg$estimates$conf_high - fit_marg$estimates$conf_low
  w_s <- fit_sim$estimates$conf_high - fit_sim$estimates$conf_low
  expect_true(all(w_s >= w_m - 1e-12))
  expect_true(all(fit_marg$estimates$conf_low >= 0 & fit_marg$estimates$conf_high <= 1))

  # Soft probability columns mode
  df_soft <- df_prop
  df_soft$p_low <- ifelse(y_f == "low", 0.85, 0.075)
  df_soft$p_mod <- ifelse(y_f == "moderate", 0.85, 0.075)
  df_soft$p_hi <- ifelse(y_f == "high", 0.85, 0.075)
  fit_soft <- ppi_proportion(
    df_soft,
    truth_col = "human",
    pred_col = c(low = "p_low", moderate = "p_mod", high = "p_hi")
  )
  expect_equal(names(coef(fit_soft)), lvls)
  expect_true(all(is.finite(fit_soft$estimates$std_error)))

  # Binary outcome + continuous probability column
  df_bin_p <- tibble::tibble(
    y_bin = c(as.integer(y_t[seq_len(n_lab)] == "high"), rep(NA_integer_, n_unlab)),
    p_high = df_soft$p_hi
  )
  fit_bp <- ppi_proportion(df_bin_p, truth_col = "y_bin", pred_col = "p_high")
  expect_equal(names(coef(fit_bp)), "positive")
  expect_true(is.finite(fit_bp$estimates$std_error))

  # Monte Carlo coverage proof for biased multi-class classifier (120 reps)
  n_sim <- 120L
  cov_hi_ppi <- logical(n_sim)
  cov_hi_naive <- logical(n_sim)
  cov_all_sim <- logical(n_sim)

  for (r in seq_len(n_sim)) {
    yt <- factor(sample(lvls, n_tot, replace = TRUE, prob = true_p), levels = lvls)
    yf <- yt
    fl <- (yt == "moderate") & (runif(n_tot) < 0.45)
    yf[fl] <- "high"
    d <- tibble::tibble(
      h = factor(c(as.character(yt[seq_len(n_lab)]), rep(NA_character_, n_unlab)), levels = lvls),
      f = yf
    )
    fm <- ppi_proportion(d, truth_col = "h", pred_col = "f")
    fs <- ppi_proportion(d, truth_col = "h", pred_col = "f", simultaneous = TRUE)
    rh <- fm$estimates[fm$estimates$term == "high", ]
    rn <- fm$benchmarks[fm$benchmarks$method == "naive_llm" & fm$benchmarks$term == "high", ]
    cov_hi_ppi[r] <- rh$conf_low <= true_p["high"] && true_p["high"] <= rh$conf_high
    cov_hi_naive[r] <- rn$conf_low <= true_p["high"] && true_p["high"] <= rn$conf_high
    cov_all_sim[r] <- all(fs$estimates$conf_low <= true_p[fs$estimates$term] &
                            true_p[fs$estimates$term] <= fs$estimates$conf_high)
  }

  expect_gt(mean(cov_hi_ppi), 0.90)
  expect_lt(mean(cov_hi_naive), 0.05)
  expect_gt(mean(cov_all_sim), 0.91)
})

test_that("human_irr computes continuous and categorical IRR ceilings and benchmarks LLM calibration", {
  set.seed(4103)
  z <- rnorm(70, mean = 4.0, sd = 1.3)
  wide_cont <- tibble::tibble(
    h1 = z + rnorm(70, sd = 0.35),
    h2 = z + rnorm(70, sd = 0.40),
    h3 = z + rnorm(70, sd = 0.38),
    llm = 0.25 + 0.90 * z + rnorm(70, sd = 0.55)
  )
  cal_c <- calibration_metrics(wide_cont, pred_col = "llm", truth_col = "h1")
  irr_c <- human_irr(wide_cont, rater_cols = c("h1", "h2", "h3"), llm_calibration = cal_c)

  expect_s3_class(irr_c, "llm_human_irr")
  expect_equal(irr_c$type, "continuous")
  expect_true(irr_c$balanced)
  expect_gt(irr_c$metrics$icc_a1, 0.85)
  expect_gt(irr_c$metrics$icc_ak, irr_c$metrics$icc_a1)
  expect_equal(nrow(irr_c$benchmark), 4L)
  expect_no_error(ggplot2::ggplot_build(autoplot(irr_c)))

  # Long unbalanced continuous -> REML fallback
  long_c <- tibble::tibble(
    unit = rep(paste0("u", 1:70), 3),
    rater = rep(c("h1", "h2", "h3"), each = 70),
    val = c(wide_cont$h1, wide_cont$h2, wide_cont$h3)
  )[-c(5, 19, 88, 145), ]
  irr_unbal <- human_irr(long_c, id_col = "unit", rater_col = "rater", score_col = "val")
  expect_false(irr_unbal$balanced)
  expect_equal(irr_unbal$method_used, "reml")
  expect_gt(irr_unbal$metrics$icc_a1, 0.85)
  expect_no_error(ggplot2::ggplot_build(autoplot(irr_unbal)))

  # Categorical / ordinal 3-rater IRR + LLM ceiling benchmark
  lvls <- c("low", "mid", "high")
  base_cat <- sample(lvls, 80, replace = TRUE)
  perturb <- function(x, p) {
    out <- x
    idx <- which(runif(length(x)) < p)
    out[idx] <- sample(lvls, length(idx), replace = TRUE)
    factor(out, levels = lvls)
  }
  wide_cat <- tibble::tibble(
    h1 = perturb(base_cat, 0.12),
    h2 = perturb(base_cat, 0.12),
    h3 = perturb(base_cat, 0.12),
    llm = perturb(base_cat, 0.20)
  )
  cal_cat <- calibration_metrics(wide_cat, pred_col = "llm", truth_col = "h1", type = "ordinal")
  irr_cat <- human_irr(wide_cat, rater_cols = c("h1", "h2", "h3"), type = "ordinal", llm_calibration = cal_cat)
  expect_gt(irr_cat$metrics$fleiss_kappa, 0.5)
  expect_gt(irr_cat$metrics$krippendorff_alpha, 0.5)
  expect_true(is.finite(irr_cat$metrics$fleiss_se))
  expect_equal(nrow(irr_cat$benchmark), 3L)
  expect_no_error(ggplot2::ggplot_build(autoplot(irr_cat)))
})

test_that("code_texts supports resumable atomic checkpointing and partial-failure recovery", {
  tmp_dir <- withr::local_tempdir()
  ckpt_path <- file.path(tmp_dir, "batch_checkpoint.rds")

  df <- tibble::tibble(
    id = paste0("u", sprintf("%02d", 1:10)),
    text = paste("Entry", 1:10),
    true_score = seq(-1, 1, length.out = 10)
  )
  des <- coding_design(
    prompts = c(p1 = "Rate: {text}"),
    models = "gpt-4o-mini",
    schema = coding_schema(score = schema_field("numeric"))
  )

  # Fail on unit u07 during run 1 (simulating API rate-limit mid-batch)
  calls_run1 <- character()
  be_flaky <- backend_openai(
    api_key = "demo",
    transport = function(spec) {
      txt <- spec$body$messages[[2L]]$content
      calls_run1 <<- c(calls_run1, txt)
      if (grepl("Entry 7$", txt)) {
        stop("HTTP 429 rate limit exceeded")
      }
      list(choices = list(list(message = list(content = '{"score": 3.5}'))))
    }
  )

  expect_error(
    code_texts(
      df, "text", id_col = "id", design = des, backend = be_flaky,
      cache = coding_cache("none"),
      max_validation_retries = 0L,
      checkpoint_file = ckpt_path,
      checkpoint_every = 3L,
      on_error = "stop"
    ),
    "unit index 7"
  )
  expect_true(file.exists(ckpt_path))
  expect_equal(length(calls_run1), 7L)

  # Resume run 2 with healthy backend and empty cache -> restores units 1..6 from checkpoint!
  calls_run2 <- character()
  be_healthy <- backend_openai(
    api_key = "demo",
    transport = function(spec) {
      txt <- spec$body$messages[[2L]]$content
      calls_run2 <<- c(calls_run2, txt)
      list(choices = list(list(message = list(content = '{"score": 4.0}'))))
    }
  )

  resumed <- code_texts(
    df, "text", id_col = "id", design = des, backend = be_healthy,
    cache = coding_cache("none"),
    max_validation_retries = 0L,
    checkpoint_file = ckpt_path,
    on_error = "stop"
  )
  expect_equal(resumed$resumed_count, 6L)
  expect_equal(length(calls_run2), 4L) # Only units 7..10 scored!
  expect_true(all(resumed$scores$valid))
  expect_equal(resumed$scores$score[1:6], rep(3.5, 6L))
  expect_equal(resumed$scores$score[7:10], rep(4.0, 4L))

  # Partial failure with on_error = "warn_na" preserves all other units and reports failed_indices
  expect_warning(
    partial <- code_texts(
      df, "text", id_col = "id", design = des, backend = be_flaky,
      cache = coding_cache("none"),
      max_validation_retries = 0L,
      on_error = "warn_na"
    ),
    "1 coding cell failed"
  )
  expect_equal(partial$failed_indices, 7L)
  expect_equal(partial$failed_units, "u07")
  expect_equal(sum(partial$scores$valid), 9L)
})

test_that("format_apa_table renders markdown, tibble, and LaTeX tables cleanly", {
  data(ema_coded)
  agg <- aggregate_scores(ema_coded, score_col = "llm_score")
  fit <- ppi_lm(human_score ~ x + group, data = agg, pred_col = "llm_score")

  tab_md <- format_apa_table(fit, format = "markdown", include_intercept = FALSE)
  expect_s3_class(tab_md, "llm_apa_table")
  expect_equal(nrow(tab_md$table), 6L) # 2 terms x 3 estimators
  expect_match(as.character(tab_md), "\\| Term \\| Estimator \\|")
  expect_match(as.character(tab_md), "Naive LLM")
  expect_match(as.character(tab_md), "PPI\\+\\+")

  tab_ltx <- format_apa_table(fit, format = "latex")
  expect_match(as.character(tab_ltx), "\\\\begin\\{tabular\\}")
  expect_match(as.character(tab_ltx), "\\\\toprule")

  tab_tbl <- format_apa_table(fit, format = "tibble")
  expect_s3_class(tab_tbl$formatted, "tbl_df")

  # Works for g_study, dpf_test, and human_irr
  g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
  expect_s3_class(format_apa_table(g), "llm_apa_table")

  dpf <- dpf_test(agg, score_col = "llm_score", truth_col = "human_score", group_col = "group")
  expect_s3_class(format_apa_table(dpf), "llm_apa_table")
})
