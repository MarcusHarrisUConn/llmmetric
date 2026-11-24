#' Test for Temporal or Model-Version Scoring Drift
#'
#' Evaluates whether LLM scores on a shared anchor set of texts drift across
#' timepoints, waves, or model versions. Performs an overall within-unit drift
#' test across versions plus pairwise comparisons against a baseline version
#' (paired mean shift, paired Cohen's \eqn{d_z}, variance ratio, rank/agreement
#' stability, and Kolmogorov-Smirnov distributional shift).
#'
#' @param data An `llm_coded_data` object or a `data.frame`.
#' @param score_col Name of the numeric LLM score column.
#' @param version_col Name of the column identifying timepoint, wave, or model
#'   version (must have at least 2 distinct values).
#' @param id_col Name of the anchor unit identifier column. Default `"unit_id"`.
#' @param baseline Optional value in `version_col` serving as the reference
#'   version. Defaults to the first factor level (or first sorted unique value).
#' @param truth_col Optional name of a human gold-standard score column to track
#'   calibration bias and RMSE drift across versions.
#' @param alpha Significance level for drift flagging. Default `0.05`.
#' @param min_effect Minimum absolute paired Cohen's \eqn{d_z} required
#'   alongside statistical significance to trigger `drift_flag`. Default `0.20`.
#' @param na.rm Logical; drop missing rows. Default `TRUE`.
#'
#' @return An S3 object of class `"llm_drift"` containing:
#'   - `summary`: tibble of per-version statistics and pairwise drift tests vs
#'     `baseline`.
#'   - `overall_test`: tibble summarizing the omnibus within-unit test across
#'     all versions.
#'   - `baseline`: reference version label.
#'   - `drift_detected`: logical scalar (`TRUE` if any version triggers drift).
#'   - `alpha`, `min_effect`, `version_col`, `score_col`, `data`.
#' @export
#'
#' @examples
#' set.seed(1)
#' anchor <- tibble::tibble(
#'   unit_id = rep(paste0("u", 1:40), 3),
#'   version = rep(c("v1", "v2", "v3"), each = 40),
#'   base = rep(rnorm(40), 3),
#'   score = base + rep(c(0, 0.05, 0.55), each = 40) + rnorm(120, 0, 0.25)
#' )
#' check_drift(anchor, score_col = "score", version_col = "version")
check_drift <- function(data,
                        score_col = "score",
                        version_col = "version",
                        id_col = "unit_id",
                        baseline = NULL,
                        truth_col = NULL,
                        alpha = 0.05,
                        min_effect = 0.20,
                        na.rm = TRUE) {
  if (inherits(data, "llm_coded_data") && missing(score_col)) {
    score_col <- data$primary_score
  }
  df <- extract_data_frame(data, arg = "data")
  assert_scalar_character(score_col, "score_col")
  assert_scalar_character(version_col, "version_col")
  assert_scalar_character(id_col, "id_col")
  assert_scalar_character(truth_col, "truth_col", allow_null = TRUE)
  assert_scalar_numeric(alpha, "alpha", min = 1e-6, max = 0.5)
  assert_scalar_numeric(min_effect, "min_effect", min = 0)
  assert_flag(na.rm, "na.rm")

  req_cols <- unique(c(id_col, version_col, score_col, truth_col %||% character()))
  assert_columns(df, req_cols, arg = "data")
  if (!is.numeric(df[[score_col]])) {
    cli::cli_abort("Column {.val {score_col}} must be numeric.")
  }

  if (isTRUE(na.rm)) {
    keep <- !is.na(df[[score_col]]) & !is.na(df[[version_col]]) & !is.na(df[[id_col]])
    df <- df[keep, req_cols, drop = FALSE]
  } else if (anyNA(df[[score_col]]) || anyNA(df[[version_col]]) || anyNA(df[[id_col]])) {
    cli::cli_abort("Missing values detected in {.fn check_drift} and {.code na.rm = FALSE}.")
  }
  assert_finite_values(df[[score_col]], score_col)
  if (!is.null(truth_col)) {
    assert_finite_values(df[[truth_col]], truth_col)
  }

  # Collapse multiple ratings per (unit_id, version) to mean cell score
  cell_df <- aggregate_scores(df, score_col = score_col, id_col = id_col, by = version_col)
  if (!is.null(truth_col) && !(truth_col %in% names(cell_df))) {
    # Attach mean truth per (unit_id, version) if it varied
    truth_agg <- aggregate_scores(df, score_col = truth_col, id_col = id_col, by = version_col)
    cell_df[[truth_col]] <- truth_agg[[truth_col]]
  }

  versions <- if (is.factor(df[[version_col]])) {
    intersect(levels(df[[version_col]]), unique(as.character(cell_df[[version_col]])))
  } else if (is.numeric(df[[version_col]])) {
    as.character(sort(unique(cell_df[[version_col]])))
  } else {
    unique(as.character(cell_df[[version_col]]))
  }

  if (length(versions) < 2L) {
    cli::cli_abort("{.arg version_col} ({.val {version_col}}) must contain at least 2 distinct versions.")
  }

  cell_df$version_chr <- factor(as.character(cell_df[[version_col]]), levels = versions)
  cell_df$unit_chr <- factor(as.character(cell_df[[id_col]]))

  if (is.null(baseline)) {
    baseline <- versions[1L]
  } else {
    baseline <- as.character(baseline)[1L]
    if (!(baseline %in% versions)) {
      cli::cli_abort("{.arg baseline} ({.val {baseline}}) not found in {.val {version_col}}.")
    }
  }

  # Overall within-unit drift test (fixed unit + version ANOVA)
  overall_df <- cell_df
  overall_df$y <- as.numeric(overall_df[[score_col]])
  has_multi_units <- length(unique(overall_df$unit_chr)) >= 2L
  if (has_multi_units && !is_constant_numeric(overall_df$y)) {
    fit_full <- stats::lm(y ~ unit_chr + version_chr, data = overall_df)
    fit_null <- stats::lm(y ~ unit_chr, data = overall_df)
    anv <- suppressWarnings(stats::anova(fit_null, fit_full))
    overall_f <- as.numeric(anv$F[2L])
    overall_df1 <- as.numeric(anv$Df[2L])
    overall_df2 <- as.numeric(anv$Res.Df[2L])
    overall_p <- as.numeric(anv$`Pr(>F)`[2L])
  } else if (has_multi_units && is_constant_numeric(overall_df$y)) {
    overall_f <- 0
    overall_df1 <- length(versions) - 1
    overall_df2 <- max(nrow(overall_df) - length(unique(overall_df$unit_chr)) - overall_df1, 1)
    overall_p <- 1
  } else {
    overall_f <- NA_real_
    overall_df1 <- NA_real_
    overall_df2 <- NA_real_
    overall_p <- NA_real_
  }

  overall_test <- tibble::tibble(
    test = "within_unit_anova",
    statistic = overall_f,
    df1 = overall_df1,
    df2 = overall_df2,
    p_value = overall_p
  )

  # Baseline slice
  base_rows <- cell_df[as.character(cell_df$version_chr) == baseline, , drop = FALSE]
  base_map <- stats::setNames(as.numeric(base_rows[[score_col]]), as.character(base_rows[[id_col]]))

  # Pairwise comparisons against baseline
  sum_rows <- lapply(versions, function(v) {
    sub <- cell_df[as.character(cell_df$version_chr) == v, , drop = FALSE]
    sc <- as.numeric(sub[[score_col]])
    n_v <- length(sc)
    m_v <- mean(sc)
    sd_v <- if (n_v > 1L) stats::sd(sc) else NA_real_
    se_v <- if (n_v > 1L) sd_v / sqrt(n_v) else NA_real_

    # Truth calibration tracking if provided
    bias_truth <- NA_real_
    rmse_truth <- NA_real_
    if (!is.null(truth_col) && truth_col %in% names(sub)) {
      tr <- as.numeric(sub[[truth_col]])
      ok_t <- !is.na(tr)
      if (any(ok_t)) {
        bias_truth <- mean(sc[ok_t] - tr[ok_t])
        rmse_truth <- sqrt(mean((sc[ok_t] - tr[ok_t])^2))
      }
    }

    if (identical(v, baseline)) {
      return(tibble::tibble(
        version = v,
        is_baseline = TRUE,
        n = n_v,
        n_shared = n_v,
        mean_score = m_v,
        sd_score = sd_v,
        se_score = se_v,
        mean_diff = 0,
        se_diff = 0,
        conf_low = 0,
        conf_high = 0,
        cohens_dz = 0,
        t_stat = 0,
        p_value = 1,
        var_ratio = 1,
        pearson_r = 1,
        spearman_rho = 1,
        icc_agreement = 1,
        mae_vs_base = 0,
        ks_stat = 0,
        ks_p_value = 1,
        bias_vs_truth = bias_truth,
        rmse_vs_truth = rmse_truth
      ))
    }

    sub_ids <- as.character(sub[[id_col]])
    shared_ids <- intersect(names(base_map), sub_ids)
    n_sh <- length(shared_ids)

    if (n_sh >= 3L) {
      x_base <- unname(base_map[shared_ids])
      x_curr <- sc[match(shared_ids, sub_ids)]
      diffs <- x_curr - x_base
      md <- mean(diffs)
      sd_d <- stats::sd(diffs)
      se_d <- sd_d / sqrt(n_sh)
      t_crit <- stats::qt(1 - alpha / 2, df = n_sh - 1L)
      ci_lo <- md - t_crit * se_d
      ci_hi <- md + t_crit * se_d
      dz <- if (sd_d > 0) md / sd_d else if (md == 0) 0 else sign(md) * Inf
      t_st <- if (se_d > 0) md / se_d else if (md == 0) 0 else sign(md) * Inf
      p_val <- if (se_d > 0) 2 * stats::pt(abs(t_st), df = n_sh - 1L, lower.tail = FALSE) else if (md == 0) 1 else 0
      vr <- if (stats::var(x_base) > 0) stats::var(x_curr) / stats::var(x_base) else NA_real_
      pr <- if (stats::sd(x_base) > 0 && stats::sd(x_curr) > 0) stats::cor(x_base, x_curr) else NA_real_
      sr <- if (stats::sd(x_base) > 0 && stats::sd(x_curr) > 0) {
        suppressWarnings(stats::cor(x_base, x_curr, method = "spearman"))
      } else {
        NA_real_
      }
      icc_a <- compute_icc_two_way(x_curr, x_base)$icc_agreement
      mae_b <- mean(abs(diffs))
      ks <- suppressWarnings(stats::ks.test(x_curr, x_base))
      ks_d <- unname(as.numeric(ks$statistic))
      ks_p <- as.numeric(ks$p.value)
    } else {
      md <- m_v - mean(base_map)
      se_d <- NA_real_
      ci_lo <- NA_real_
      ci_hi <- NA_real_
      dz <- NA_real_
      t_st <- NA_real_
      p_val <- NA_real_
      vr <- NA_real_
      pr <- NA_real_
      sr <- NA_real_
      icc_a <- NA_real_
      mae_b <- NA_real_
      ks_d <- NA_real_
      ks_p <- NA_real_
    }

    tibble::tibble(
      version = v,
      is_baseline = FALSE,
      n = n_v,
      n_shared = n_sh,
      mean_score = m_v,
      sd_score = sd_v,
      se_score = se_v,
      mean_diff = md,
      se_diff = se_d,
      conf_low = ci_lo,
      conf_high = ci_hi,
      cohens_dz = dz,
      t_stat = t_st,
      p_value = p_val,
      var_ratio = vr,
      pearson_r = pr,
      spearman_rho = sr,
      icc_agreement = icc_a,
      mae_vs_base = mae_b,
      ks_stat = ks_d,
      ks_p_value = ks_p,
      bias_vs_truth = bias_truth,
      rmse_vs_truth = rmse_truth
    )
  })

  sum_tbl <- do.call(rbind, sum_rows)
  non_base <- !sum_tbl$is_baseline
  sum_tbl$p_adj <- sum_tbl$p_value
  if (sum(non_base) > 1L) {
    sum_tbl$p_adj[non_base] <- stats::p.adjust(sum_tbl$p_value[non_base], method = "BH")
  }
  sum_tbl$drift_flag <- non_base &
    !is.na(sum_tbl$p_adj) & (sum_tbl$p_adj < alpha) &
    !is.na(sum_tbl$cohens_dz) & (abs(sum_tbl$cohens_dz) >= min_effect)

  drift_detected <- any(sum_tbl$drift_flag)

  structure(
    list(
      summary = sum_tbl,
      overall_test = overall_test,
      baseline = baseline,
      drift_detected = drift_detected,
      alpha = alpha,
      min_effect = min_effect,
      version_col = version_col,
      score_col = score_col,
      data = cell_df
    ),
    class = "llm_drift"
  )
}

#' @export
print.llm_drift <- function(x, ...) {
  cli::cli_h3("LLM Scoring Drift Diagnostics")
  cli::cli_text(
    "Baseline: {.val {x$baseline}} | Overall ANOVA F({x$overall_test$df1}, {x$overall_test$df2}) = {fmt_num(x$overall_test$statistic)}, p = {fmt_pval(x$overall_test$p_value)}"
  )
  cli::cli_text("Drift detected (alpha={x$alpha}, |d_z|>={x$min_effect}): {x$drift_detected}")
  disp <- tibble::tibble(
    version = x$summary$version,
    n_shared = x$summary$n_shared,
    mean_diff = round(x$summary$mean_diff, 3L),
    cohens_dz = round(x$summary$cohens_dz, 3L),
    icc = round(x$summary$icc_agreement, 3L),
    p_adj = round(x$summary$p_adj, 4L),
    drift_flag = x$summary$drift_flag
  )
  print(disp, n = nrow(disp))
  invisible(x)
}
