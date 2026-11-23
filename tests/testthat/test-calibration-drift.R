test_that("calibration_metrics accurately evaluates continuous ratings and catches miscalibration", {
  set.seed(55)
  n <- 200
  truth <- rnorm(n, mean = 3, sd = 1.2)
  # Well-calibrated prediction
  pred_good <- truth + rnorm(n, sd = 0.35)
  # Biased & attenuated prediction
  pred_bad <- 1.2 + 0.65 * truth + rnorm(n, sd = 0.4)

  df <- tibble::tibble(
    model = rep(c("good", "bad"), each = n),
    truth = c(truth, truth),
    pred = c(pred_good, pred_bad)
  )

  cal_good <- calibration_metrics(df[df$model == "good", ], pred_col = "pred", truth_col = "truth")
  cal_bad <- calibration_metrics(df[df$model == "bad", ], pred_col = "pred", truth_col = "truth")
  cal_by <- calibration_metrics(df, pred_col = "pred", truth_col = "truth", by = "model")

  expect_s3_class(cal_good, "llm_calibration")
  expect_equal(cal_good$type, "continuous")
  expect_gt(cal_good$metrics$pearson_r, 0.92)
  expect_gt(cal_good$metrics$icc_agreement, 0.92)
  expect_gt(cal_good$metrics$ccc, 0.92)
  expect_lt(abs(cal_good$metrics$bias), 0.1)

  # Biased model has lower ICC/CCC and highly significant calibration F-test
  expect_lt(cal_bad$metrics$icc_agreement, cal_good$metrics$icc_agreement)
  expect_lt(cal_bad$metrics$cal_p_value, 1e-6)
  expect_equal(nrow(cal_by$by_group), 2L)
})

test_that("calibration_metrics accurately computes categorical and ordinal reliability", {
  # Perfect agreement -> accuracy = 1, kappa = 1, alpha = 1
  perf <- tibble::tibble(
    truth = factor(rep(c("low", "mid", "high"), each = 20), levels = c("low", "mid", "high")),
    pred = factor(rep(c("low", "mid", "high"), each = 20), levels = c("low", "mid", "high"))
  )
  cal_perf <- calibration_metrics(perf, pred_col = "pred", truth_col = "truth", type = "ordinal")
  expect_equal(cal_perf$metrics$accuracy, 1)
  expect_equal(cal_perf$metrics$cohen_kappa, 1)
  expect_equal(cal_perf$metrics$kappa_quadratic, 1)
  expect_equal(cal_perf$metrics$krippendorff_alpha, 1)

  # Imperfect ordinal agreement: quadratic weighted kappa > unweighted kappa when errors are adjacent
  adj <- perf
  adj$pred[1:5] <- "mid"
  adj$pred[21:25] <- "high"
  cal_ord <- calibration_metrics(adj, pred_col = "pred", truth_col = "truth", type = "ordinal")
  expect_gt(cal_ord$metrics$kappa_quadratic, cal_ord$metrics$cohen_kappa)
  expect_gt(cal_ord$metrics$krippendorff_alpha, 0.5)
  expect_true(is.table(cal_ord$confusion))

  # Nominal categorical auto-detection
  cal_nom <- calibration_metrics(adj, pred_col = "pred", truth_col = "truth", type = "auto")
  expect_equal(cal_nom$type, "categorical")
  expect_lt(cal_nom$metrics$accuracy, 1)
  expect_gt(cal_nom$metrics$accuracy, 0.7)
})

test_that("check_drift detects real version shift and remains negative when stable", {
  set.seed(123)
  n_u <- 50
  base_t <- rnorm(n_u, mean = 4, sd = 1)

  # Stable anchor dataset (no drift)
  stable_df <- tibble::tibble(
    unit_id = rep(paste0("u", seq_len(n_u)), 3),
    version = rep(c("v1", "v2", "v3"), each = n_u),
    human_score = rep(base_t, 3),
    score = rep(base_t, 3) + rnorm(3 * n_u, sd = 0.25)
  )
  dr_stable <- check_drift(stable_df, score_col = "score", version_col = "version", truth_col = "human_score")
  expect_s3_class(dr_stable, "llm_drift")
  expect_false(dr_stable$drift_detected)
  expect_true(all(dr_stable$summary$icc_agreement > 0.85))

  # Drifted dataset (+0.65 shift in v3)
  drifted_df <- stable_df
  drifted_df$score[drifted_df$version == "v3"] <- drifted_df$score[drifted_df$version == "v3"] + 0.65
  dr_shift <- check_drift(drifted_df, score_col = "score", version_col = "version", truth_col = "human_score")
  expect_true(dr_shift$drift_detected)
  expect_true(dr_shift$summary$drift_flag[dr_shift$summary$version == "v3"])
  expect_false(dr_shift$summary$drift_flag[dr_shift$summary$version == "v2"])
  expect_lt(dr_shift$overall_test$p_value, 1e-6)
  expect_gt(dr_shift$summary$bias_vs_truth[dr_shift$summary$version == "v3"], 0.4)
})
