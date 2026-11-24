#' Human Inter-Rater Reliability (IRR) Ceiling Benchmark
#'
#' Computes human-vs-human inter-rater reliability across two or more human
#' annotators in either **wide format** (`rater_cols = c("h1", "h2", "h3")`) or
#' **long format** (`id_col`, `rater_col`, `score_col`), and optionally
#' benchmarks an LLM's [calibration_metrics()] directly against the human
#' inter-rater ceiling.
#'
#' - **Continuous ratings:** two-way random absolute agreement
#'   \eqn{\mathrm{ICC}(A,1)} (single rater) and \eqn{\mathrm{ICC}(A,k)}
#'   (average of \eqn{k} raters), consistency \eqn{\mathrm{ICC}(C,1)} and
#'   \eqn{\mathrm{ICC}(C,k)} (using `lme4::lmer` REML automatically when
#'   ratings are unbalanced/missing), plus mean pairwise Pearson \eqn{r},
#'   Spearman \eqn{\rho}, MAE, and RMSE.
#' - **Categorical / ordinal ratings:** pairwise percent agreement, unweighted
#'   and weighted Cohen's \eqn{\kappa} (when \eqn{k = 2}), Fleiss's \eqn{\kappa}
#'   (for any \eqn{k \ge 2} with analytic SE and 95% CI), and multi-rater
#'   Krippendorff's \eqn{\alpha} (nominal or ordinal distance metric, supporting
#'   arbitrary missing ratings).
#'
#' @param data A `data.frame` containing human ratings in wide or long format.
#' @param rater_cols Character vector of column names for wide-format ratings
#'   (at least 2 human rater columns). Mutually exclusive with
#'   `(id_col, rater_col, score_col)`.
#' @param id_col Unit identifier column name for long-format ratings.
#' @param rater_col Human rater identifier column name for long-format ratings.
#' @param score_col Score/label column name for long-format ratings.
#' @param type Measurement scale: `"auto"` (numeric -> `"continuous"`,
#'   factor/character/logical -> `"categorical"`), `"continuous"`,
#'   `"categorical"`, or `"ordinal"`.
#' @param llm_calibration Optional `"llm_calibration"` object returned by
#'   [calibration_metrics()] to benchmark LLM-human agreement directly against
#'   the human inter-rater ceiling.
#' @param conf_level Confidence level for Fleiss's \eqn{\kappa} CI. Default `0.95`.
#' @param na.rm Logical; drop units/rows with insufficient ratings. Default `TRUE`.
#'
#' @return An S3 object of class `"llm_human_irr"` containing:
#'   - `metrics`: tibble of human-vs-human reliability statistics.
#'   - `benchmark`: tibble comparing LLM calibration against the human ceiling
#'     (or `NULL` if `llm_calibration` is `NULL`).
#'   - `type`, `n_units`, `n_raters`, `balanced`, `method_used`.
#' @export
#'
#' @examples
#' set.seed(10)
#' true_z <- rnorm(60, mean = 4, sd = 1.2)
#' wide_df <- tibble::tibble(
#'   h1 = true_z + rnorm(60, sd = 0.40),
#'   h2 = true_z + rnorm(60, sd = 0.45),
#'   h3 = true_z + rnorm(60, sd = 0.42),
#'   llm = 0.2 + 0.9 * true_z + rnorm(60, sd = 0.48)
#' )
#' cal <- calibration_metrics(wide_df, pred_col = "llm", truth_col = "h1")
#' irr <- human_irr(wide_df, rater_cols = c("h1", "h2", "h3"), llm_calibration = cal)
#' irr
human_irr <- function(data,
                      rater_cols = NULL,
                      id_col = NULL,
                      rater_col = NULL,
                      score_col = NULL,
                      type = c("auto", "continuous", "categorical", "ordinal"),
                      llm_calibration = NULL,
                      conf_level = 0.95,
                      na.rm = TRUE) {
  df <- extract_data_frame(data, arg = "data")
  type <- match.arg(type)
  assert_scalar_numeric(conf_level, "conf_level", min = 0.5, max = 0.9999)
  assert_flag(na.rm, "na.rm")
  if (!is.null(llm_calibration) && !inherits(llm_calibration, "llm_calibration")) {
    cli::cli_abort("{.arg llm_calibration} must be an {.cls llm_calibration} object or NULL.")
  }

  # Normalize wide or long format into a unified long table AND wide rating matrix
  if (!is.null(rater_cols)) {
    if (!is.character(rater_cols) || anyNA(rater_cols)) {
      cli::cli_abort("{.arg rater_cols} must name at least 2 distinct human rater columns.")
    }
    rater_cols <- unique(rater_cols)
    if (length(rater_cols) < 2L) {
      cli::cli_abort("{.arg rater_cols} must name at least 2 distinct human rater columns.")
    }
    assert_columns(df, rater_cols, arg = "data")
    unit_ids <- paste0("u_", seq_len(nrow(df)))
    long_list <- lapply(rater_cols, function(rc) {
      tibble::tibble(unit = unit_ids, rater = rc, value = df[[rc]])
    })
    long_df <- do.call(rbind, long_list)
    rater_levels <- rater_cols
  } else {
    assert_scalar_character(id_col, "id_col")
    assert_scalar_character(rater_col, "rater_col")
    assert_scalar_character(score_col, "score_col")
    assert_columns(df, c(id_col, rater_col, score_col), arg = "data")
    long_df <- tibble::tibble(
      unit = as.character(df[[id_col]]),
      rater = as.character(df[[rater_col]]),
      value = df[[score_col]]
    )
    rater_levels <- unique(long_df$rater[!is.na(long_df$rater)])
    if (length(rater_levels) < 2L) {
      cli::cli_abort("{.arg rater_col} ({.val {rater_col}}) must contain at least 2 distinct raters.")
    }
  }

  if (isTRUE(na.rm)) {
    long_df <- long_df[!is.na(long_df$unit) & !is.na(long_df$rater) & !is.na(long_df$value), , drop = FALSE]
  } else if (anyNA(long_df$value) || anyNA(long_df$unit) || anyNA(long_df$rater)) {
    cli::cli_abort("Missing ratings detected and {.code na.rm = FALSE}.")
  }

  # Recompute active raters AFTER na.rm filtering so all-NA raters do not reach REML/ANOVA
  active_raters <- intersect(rater_levels, unique(long_df$rater))
  if (length(active_raters) < 2L) {
    cli::cli_abort(
      "At least 2 human raters with non-missing ratings are required for {.fn human_irr} (found {length(active_raters)})."
    )
  }
  rater_levels <- active_raters

  if (nrow(long_df) < 4L) {
    cli::cli_abort("Not enough valid human ratings to compute inter-rater reliability.")
  }

  if (type == "auto") {
    type <- if (is.numeric(long_df$value)) "continuous" else "categorical"
  }

  u_units <- unique(long_df$unit)
  n_units <- length(u_units)
  k_raters <- length(rater_levels)
  if (n_units < 3L) {
    cli::cli_abort("At least 3 rated units are required for {.fn human_irr}.")
  }

  if (type == "continuous") {
    assert_finite_values(long_df$value, "human ratings")
    res <- compute_human_irr_continuous(long_df, u_units, rater_levels)
  } else {
    res <- compute_human_irr_categorical(
      long_df, u_units, rater_levels,
      ordinal = (type == "ordinal"),
      conf_level = conf_level
    )
  }

  bench_tbl <- NULL
  if (!is.null(llm_calibration)) {
    bench_tbl <- build_irr_ceiling_benchmark(res$metrics, llm_calibration, type = type)
  }

  structure(
    list(
      metrics = res$metrics,
      benchmark = bench_tbl,
      type = type,
      n_units = n_units,
      n_raters = k_raters,
      balanced = res$balanced,
      method_used = res$method_used
    ),
    class = "llm_human_irr"
  )
}

#' Continuous human-vs-human ICC & pairwise metrics
#' @noRd
compute_human_irr_continuous <- function(long_df, u_units, rater_levels) {
  n <- length(u_units)
  k <- length(rater_levels)
  y <- as.numeric(long_df$value)

  # Build n x k matrix (averaging duplicates if any)
  mat <- matrix(NA_real_, nrow = n, ncol = k, dimnames = list(u_units, rater_levels))
  for (r_i in seq_len(k)) {
    sub <- long_df[long_df$rater == rater_levels[r_i], , drop = FALSE]
    if (nrow(sub) > 0L) {
      vals <- tapply(as.numeric(sub$value), sub$unit, mean)
      mat[names(vals), r_i] <- as.numeric(vals)
    }
  }
  balanced <- !anyNA(mat) && (nrow(long_df) == n * k)
  n_multi <- sum(rowSums(!is.na(mat)) >= 2L)
  if (n_multi < 2L) {
    cli::cli_abort(
      "At least 2 overlapping units rated by >= 2 human raters are required to estimate continuous inter-rater reliability (found {n_multi})."
    )
  }

  if (is_constant_numeric(y)) {
    cli::cli_warn("Human ratings are constant (zero variance); ICC and correlations are undefined (NA).")
    return(list(
      metrics = tibble::tibble(
        n_units = n, n_raters = k,
        icc_a1 = NA_real_, icc_ak = NA_real_,
        icc_c1 = NA_real_, icc_ck = NA_real_,
        mean_pearson_r = NA_real_, mean_spearman_rho = NA_real_,
        mean_pairwise_mae = 0, mean_pairwise_rmse = 0
      ),
      balanced = balanced,
      method_used = "constant"
    ))
  }

  if (balanced) {
    grand_mean <- mean(mat)
    row_means <- rowMeans(mat)
    col_means <- colMeans(mat)
    ss_r <- k * sum((row_means - grand_mean)^2)
    ss_c <- n * sum((col_means - grand_mean)^2)
    ss_tot <- sum((mat - grand_mean)^2)
    ss_e <- max(ss_tot - ss_r - ss_c, 0)

    ms_r <- ss_r / (n - 1)
    ms_c <- ss_c / (k - 1)
    ms_e <- ss_e / ((n - 1) * (k - 1))

    d_a1 <- ms_r + (k - 1) * ms_e + (k / n) * (ms_c - ms_e)
    d_ak <- ms_r + (ms_c - ms_e) / n
    d_c1 <- ms_r + (k - 1) * ms_e
    d_ck <- ms_r

    icc_a1 <- if (d_a1 > 0) (ms_r - ms_e) / d_a1 else NA_real_
    icc_ak <- if (d_ak > 0) (ms_r - ms_e) / d_ak else NA_real_
    icc_c1 <- if (d_c1 > 0) (ms_r - ms_e) / d_c1 else NA_real_
    icc_ck <- if (d_ck > 0) (ms_r - ms_e) / d_ck else NA_real_
    method_used <- "two_way_anova"
  } else {
    work <- data.frame(y = y, u = factor(long_df$unit), r = factor(long_df$rater))
    ctrl <- lme4::lmerControl(
      check.nobs.vs.nlev = "ignore",
      check.nobs.vs.nRE = "ignore",
      check.nlev.gtreq.5 = "ignore"
    )
    fit <- suppressMessages(suppressWarnings(
      lme4::lmer(y ~ 1 + (1 | u) + (1 | r), data = work, REML = TRUE, control = ctrl)
    ))
    vc <- as.data.frame(lme4::VarCorr(fit))
    vu <- max(as.numeric(vc$vcov[vc$grp == "u"]), 0)
    vr <- max(as.numeric(vc$vcov[vc$grp == "r"]), 0)
    ve <- max(as.numeric(vc$vcov[vc$grp == "Residual"]), 0)
    k_eff <- mean(rowSums(!is.na(mat)))

    icc_a1 <- if (vu + vr + ve > 0) vu / (vu + vr + ve) else NA_real_
    icc_ak <- if (vu + (vr + ve) / k_eff > 0) vu / (vu + (vr + ve) / k_eff) else NA_real_
    icc_c1 <- if (vu + ve > 0) vu / (vu + ve) else NA_real_
    icc_ck <- if (vu + ve / k_eff > 0) vu / (vu + ve / k_eff) else NA_real_
    method_used <- "reml"
  }

  # Mean pairwise correlations & errors across all rater pairs
  pr_vals <- sr_vals <- mae_vals <- rmse_vals <- numeric()
  for (i in seq_len(k - 1L)) {
    for (j in seq.int(i + 1L, k)) {
      ok <- !is.na(mat[, i]) & !is.na(mat[, j])
      if (sum(ok) >= 3L) {
        xi <- mat[ok, i]
        xj <- mat[ok, j]
        if (!is_constant_numeric(xi) && !is_constant_numeric(xj)) {
          pr_vals <- c(pr_vals, stats::cor(xi, xj))
          sr_vals <- c(sr_vals, suppressWarnings(stats::cor(xi, xj, method = "spearman")))
        }
        mae_vals <- c(mae_vals, mean(abs(xi - xj)))
        rmse_vals <- c(rmse_vals, sqrt(mean((xi - xj)^2)))
      }
    }
  }

  metrics <- tibble::tibble(
    n_units = n,
    n_raters = k,
    icc_a1 = icc_a1,
    icc_ak = icc_ak,
    icc_c1 = icc_c1,
    icc_ck = icc_ck,
    mean_pearson_r = if (length(pr_vals) > 0L) mean(pr_vals) else NA_real_,
    mean_spearman_rho = if (length(sr_vals) > 0L) mean(sr_vals) else NA_real_,
    mean_pairwise_mae = if (length(mae_vals) > 0L) mean(mae_vals) else NA_real_,
    mean_pairwise_rmse = if (length(rmse_vals) > 0L) mean(rmse_vals) else NA_real_
  )

  list(metrics = metrics, balanced = balanced, method_used = method_used)
}

#' Categorical / ordinal multi-rater IRR (Fleiss's kappa, Cohen's kappa, Krippendorff's alpha)
#' @noRd
compute_human_irr_categorical <- function(long_df, u_units, rater_levels, ordinal, conf_level) {
  n <- length(u_units)
  k <- length(rater_levels)
  lvls <- if (is.factor(long_df$value)) {
    levels(long_df$value)
  } else if (is.numeric(long_df$value)) {
    as.character(sort(unique(long_df$value[!is.na(long_df$value)])))
  } else {
    sort(unique(as.character(long_df$value)))
  }
  q <- length(lvls)

  # Build n x k character matrix
  mat <- matrix(NA_character_, nrow = n, ncol = k, dimnames = list(u_units, rater_levels))
  for (r_i in seq_len(k)) {
    sub <- long_df[long_df$rater == rater_levels[r_i], , drop = FALSE]
    if (nrow(sub) > 0L) {
      # Take first rating per (unit, rater)
      first_idx <- !duplicated(sub$unit)
      mat[sub$unit[first_idx], r_i] <- as.character(sub$value[first_idx])
    }
  }
  balanced <- !anyNA(mat)

  # Build unit-by-category count matrix N_mat (n x q)
  N_mat <- matrix(0, nrow = n, ncol = q, dimnames = list(u_units, lvls))
  for (c_i in seq_len(q)) {
    N_mat[, c_i] <- rowSums(mat == lvls[c_i], na.rm = TRUE)
  }
  m_u <- rowSums(N_mat) # ratings per unit

  # Pairwise percent agreement across units with >= 2 ratings
  valid_u <- m_u >= 2L
  if (!any(valid_u)) {
    cli::cli_abort("No units have >= 2 human ratings; cannot compute inter-rater reliability.")
  }
  N_v <- N_mat[valid_u, , drop = FALSE]
  m_v <- m_u[valid_u]
  P_i <- rowSums(N_v * (N_v - 1)) / (m_v * (m_v - 1))
  pct_agree <- mean(P_i)

  # Fleiss's kappa (exact for constant m_v, and generalized p_j for slight imbalance)
  p_j <- colSums(N_v) / sum(N_v)
  P_e <- sum(p_j^2)
  fleiss_kappa <- if (1 - P_e > 1e-12) (pct_agree - P_e) / (1 - P_e) else NA_real_

  # Analytic Fleiss (1971) standard error under balanced ratings
  fleiss_se <- NA_real_
  fleiss_lo <- NA_real_
  fleiss_hi <- NA_real_
  if (!is.na(fleiss_kappa) && length(unique(m_v)) == 1L && (1 - P_e) > 1e-12) {
    m0 <- m_v[1L]
    n0 <- nrow(N_v)
    num_var <- 2 * (P_e - (2 * m0 - 3) * (P_e^2) + 2 * (m0 - 2) * sum(p_j^3))
    den_var <- n0 * m0 * (m0 - 1) * ((1 - P_e)^2)
    if (den_var > 0 && num_var >= 0) {
      fleiss_se <- sqrt(num_var / den_var)
      zc <- stats::qnorm(1 - (1 - conf_level) / 2)
      fleiss_lo <- fleiss_kappa - zc * fleiss_se
      fleiss_hi <- fleiss_kappa + zc * fleiss_se
    }
  }

  # Exact multi-rater Krippendorff's alpha via general coincidence matrix O (q x q)
  O <- matrix(0, nrow = q, ncol = q)
  for (u_i in seq_len(nrow(N_v))) {
    nu <- N_v[u_i, ]
    mu <- m_v[u_i]
    # Off-diagonal: n_c * n_k / (mu - 1); diagonal: n_c * (n_c - 1) / (mu - 1)
    pair_mat <- outer(nu, nu)
    diag(pair_mat) <- nu * (nu - 1)
    O <- O + pair_mat / (mu - 1)
  }
  n_c <- rowSums(O)
  tot_pairs <- sum(n_c)
  E_mat <- (outer(n_c, n_c) - diag(n_c)) / max(tot_pairs - 1, 1)

  if (ordinal && q >= 2L) {
    D_mat <- matrix(0, nrow = q, ncol = q)
    for (c_i in seq_len(q)) {
      for (k_i in seq_len(q)) {
        if (c_i != k_i) {
          lo <- min(c_i, k_i)
          hi <- max(c_i, k_i)
          g_sum <- sum(n_c[lo:hi]) - 0.5 * (n_c[c_i] + n_c[k_i])
          D_mat[c_i, k_i] <- g_sum^2
        }
      }
    }
  } else {
    D_mat <- matrix(1, nrow = q, ncol = q)
    diag(D_mat) <- 0
  }

  do_krip <- sum(O * D_mat)
  de_krip <- sum(E_mat * D_mat)
  krip_alpha <- if (de_krip > 1e-12) 1 - do_krip / de_krip else NA_real_

  # Cohen's kappa when exactly 2 raters
  cohen_k <- NA_real_
  cohen_kq <- NA_real_
  if (k == 2L) {
    ok2 <- !is.na(mat[, 1L]) & !is.na(mat[, 2L])
    if (sum(ok2) >= 3L) {
      c2 <- compute_categorical_calibration(
        factor(mat[ok2, 1L], levels = lvls),
        factor(mat[ok2, 2L], levels = lvls),
        ordinal = ordinal
      )
      cohen_k <- c2$metrics$cohen_kappa
      cohen_kq <- c2$metrics$kappa_quadratic
    }
  }

  metrics <- tibble::tibble(
    n_units = nrow(N_v),
    n_raters = k,
    pct_agreement = pct_agree,
    fleiss_kappa = fleiss_kappa,
    fleiss_se = fleiss_se,
    fleiss_conf_low = fleiss_lo,
    fleiss_conf_high = fleiss_hi,
    cohen_kappa = cohen_k,
    kappa_quadratic = cohen_kq,
    krippendorff_alpha = krip_alpha
  )

  list(metrics = metrics, balanced = balanced, method_used = if (ordinal) "ordinal_irr" else "nominal_irr")
}

#' Build direct LLM-vs-Human-Ceiling benchmark tibble
#' @noRd
build_irr_ceiling_benchmark <- function(irr_metrics, llm_cal, type) {
  cm <- llm_cal$metrics
  if (type == "continuous" && identical(llm_cal$type, "continuous")) {
    metric_names <- c("ICC(A,1)", "ICC(C,1)", "Pearson r", "Spearman rho")
    human_vals <- c(
      irr_metrics$icc_a1, irr_metrics$icc_c1,
      irr_metrics$mean_pearson_r, irr_metrics$mean_spearman_rho
    )
    llm_vals <- c(cm$icc_agreement, cm$icc_consistency, cm$pearson_r, cm$spearman_rho)
  } else if (type != "continuous" && !identical(llm_cal$type, "continuous")) {
    metric_names <- c("Agreement / Accuracy", "Kappa (Fleiss vs. Cohen)", "Krippendorff's alpha")
    human_vals <- c(irr_metrics$pct_agreement, irr_metrics$fleiss_kappa, irr_metrics$krippendorff_alpha)
    llm_vals <- c(cm$accuracy, cm$cohen_kappa, cm$krippendorff_alpha)
  } else {
    cli::cli_warn(
      "Scale mismatch between {.fn human_irr} ({type}) and {.arg llm_calibration} ({llm_cal$type}); skipping ceiling benchmark."
    )
    return(NULL)
  }

  tibble::tibble(
    metric = metric_names,
    human_ceiling = as.numeric(human_vals),
    llm_vs_human = as.numeric(llm_vals),
    gap = as.numeric(llm_vals - human_vals),
    pct_of_ceiling = ifelse(is.na(human_vals) | abs(human_vals) < 1e-12, NA_real_, 100 * llm_vals / human_vals)
  )
}

#' @export
print.llm_human_irr <- function(x, ...) {
  cli::cli_h3("Human Inter-Rater Reliability Ceiling ({x$type})")
  cli::cli_text(
    "Units: {x$n_units} | Human raters: {x$n_raters} | Balanced: {x$balanced} | Method: {x$method_used}"
  )
  m <- x$metrics
  if (x$type == "continuous") {
    cli::cli_text(
      "ICC(A,1) = {fmt_num(m$icc_a1)} | ICC(A,k) = {fmt_num(m$icc_ak)} | ICC(C,1) = {fmt_num(m$icc_c1)} | Mean r = {fmt_num(m$mean_pearson_r)} | Pairwise RMSE = {fmt_num(m$mean_pairwise_rmse)}"
    )
  } else {
    cli::cli_text(
      "Agreement = {fmt_num(m$pct_agreement)} | Fleiss's kappa = {fmt_num(m$fleiss_kappa)} | Krippendorff's alpha = {fmt_num(m$krippendorff_alpha)}"
    )
  }
  if (!is.null(x$benchmark)) {
    cli::cli_text("LLM Calibration vs. Human Inter-Rater Ceiling:")
    print(x$benchmark, n = nrow(x$benchmark))
  }
  invisible(x)
}
