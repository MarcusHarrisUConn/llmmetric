#' Calibration Metrics Comparing LLM Scores to Human Gold Standards
#'
#' Evaluates agreement, bias, dispersion, and calibration between LLM-generated
#' ratings and human gold-standard labels. Supports continuous ratings (Pearson
#' \eqn{r}, Spearman \eqn{\rho}, Lin's CCC, two-way \eqn{\text{ICC}(A,1)} and
#' \eqn{\text{ICC}(C,1)}, MAE, RMSE, bias, calibration intercept/slope, and
#' calibration-in-the-large \eqn{F}-test) as well as categorical/ordinal ratings
#' (accuracy, macro-\eqn{F_1}, unweighted and weighted Cohen's \eqn{\kappa},
#' Krippendorff's \eqn{\alpha}, and confusion matrix).
#'
#' @param data An `llm_coded_data` object or a `data.frame`.
#' @param pred_col Name of the LLM score column.
#' @param truth_col Name of the human gold-standard score column.
#' @param type Measurement scale: `"auto"` (default: numeric -> `"continuous"`,
#'   factor/character/logical -> `"categorical"`), `"continuous"`,
#'   `"categorical"`, or `"ordinal"`.
#' @param by Optional character vector of grouping columns (e.g., `"model"`,
#'   `"prompt"`, or `"group"`) to compute stratified calibration metrics.
#' @param n_bins Number of quantile bins for continuous calibration curve
#'   summaries (used by `autoplot()`). Default `10L`.
#' @param na.rm Logical; drop rows where `pred_col` or `truth_col` is `NA`.
#'   Default `TRUE`.
#'
#' @return An S3 object of class `"llm_calibration"` with elements:
#'   - `metrics`: tibble of overall calibration statistics.
#'   - `by_group`: tibble of stratified calibration statistics (or `NULL` if
#'     `by` is `NULL`).
#'   - `calibration_bins`: binned calibration curve tibble (continuous only).
#'   - `confusion`: confusion matrix table (categorical/ordinal only).
#'   - `type`, `pred_col`, `truth_col`, `by`, `data`.
#' @export
#'
#' @examples
#' data(ema_coded)
#' agg <- aggregate_scores(ema_coded, score_col = "llm_score")
#' cal <- calibration_metrics(agg, pred_col = "llm_score", truth_col = "human_score")
#' cal
calibration_metrics <- function(data,
                                pred_col = "score",
                                truth_col = "human_score",
                                type = c("auto", "continuous", "categorical", "ordinal"),
                                by = NULL,
                                n_bins = 10L,
                                na.rm = TRUE) {
  if (inherits(data, "llm_coded_data") && missing(pred_col)) {
    pred_col <- data$primary_score
  }
  df <- extract_data_frame(data, arg = "data")
  type <- match.arg(type)
  assert_scalar_character(pred_col, "pred_col")
  assert_scalar_character(truth_col, "truth_col")
  assert_scalar_numeric(n_bins, "n_bins", min = 2)
  n_bins <- as.integer(round(n_bins))
  assert_flag(na.rm, "na.rm")
  if (!is.null(by)) {
    if (!is.character(by) || length(by) < 1L || anyNA(by)) {
      cli::cli_abort("{.arg by} must be a character vector of column names or NULL.")
    }
    by <- unique(by)
  }

  req_cols <- unique(c(pred_col, truth_col, by %||% character()))
  assert_columns(df, req_cols, arg = "data")

  if (isTRUE(na.rm)) {
    keep <- !is.na(df[[pred_col]]) & !is.na(df[[truth_col]])
    if (!is.null(by)) {
      keep <- keep & stats::complete.cases(df[, by, drop = FALSE])
    }
    df <- df[keep, req_cols, drop = FALSE]
  } else if (anyNA(df[[pred_col]]) || anyNA(df[[truth_col]])) {
    cli::cli_abort("Missing values detected and {.code na.rm = FALSE}.")
  }

  if (nrow(df) < 3L) {
    cli::cli_abort("At least 3 complete paired observations are required for {.fn calibration_metrics}.")
  }

  if (type == "auto") {
    type <- if (is.numeric(df[[pred_col]]) && is.numeric(df[[truth_col]])) {
      "continuous"
    } else {
      "categorical"
    }
  }
  if (type == "continuous") {
    assert_finite_values(df[[pred_col]], pred_col)
    assert_finite_values(df[[truth_col]], truth_col)
  }

  compute_one <- if (type == "continuous") {
    compute_continuous_calibration
  } else {
    function(pred, truth) compute_categorical_calibration(pred, truth, ordinal = (type == "ordinal"))
  }

  overall_res <- compute_one(df[[pred_col]], df[[truth_col]])

  by_tbl <- NULL
  if (!is.null(by)) {
    key_parts <- lapply(by, function(col) as.character(df[[col]]))
    gkey <- do.call(paste, c(key_parts, list(sep = "\r")))
    u_keys <- unique(gkey)
    idx_list <- split(seq_len(nrow(df)), factor(gkey, levels = u_keys))
    rows <- lapply(idx_list, function(ii) {
      sub_res <- compute_one(df[[pred_col]][ii], df[[truth_col]][ii])
      grp_cols <- df[ii[1L], by, drop = FALSE]
      tibble::as_tibble(cbind(grp_cols, sub_res$metrics))
    })
    by_tbl <- do.call(rbind, rows)
  }

  cal_bins <- NULL
  if (type == "continuous") {
    cal_bins <- compute_calibration_bins(
      pred = as.numeric(df[[pred_col]]),
      truth = as.numeric(df[[truth_col]]),
      n_bins = n_bins
    )
  }

  structure(
    list(
      metrics = overall_res$metrics,
      by_group = by_tbl,
      calibration_bins = cal_bins,
      confusion = overall_res$confusion,
      type = type,
      pred_col = pred_col,
      truth_col = truth_col,
      by = by,
      data = df
    ),
    class = "llm_calibration"
  )
}

#' Compute two-way random/mixed ICC(A,1) and ICC(C,1) for 2 raters
#' @noRd
compute_icc_two_way <- function(x, y) {
  n <- length(x)
  if (n < 2L) {
    return(list(icc_agreement = NA_real_, icc_consistency = NA_real_))
  }
  mat <- cbind(x, y)
  k <- 2
  grand_mean <- mean(mat)
  row_means <- rowMeans(mat)
  col_means <- colMeans(mat)

  ss_rows <- k * sum((row_means - grand_mean)^2)
  ss_cols <- n * sum((col_means - grand_mean)^2)
  ss_tot <- sum((mat - grand_mean)^2)
  ss_err <- max(ss_tot - ss_rows - ss_cols, 0)

  ms_r <- ss_rows / (n - 1)
  ms_c <- ss_cols / (k - 1)
  ms_e <- ss_err / ((n - 1) * (k - 1))

  denom_a <- ms_r + (k - 1) * ms_e + (k / n) * (ms_c - ms_e)
  denom_c <- ms_r + (k - 1) * ms_e

  icc_a <- if (denom_a > 0) (ms_r - ms_e) / denom_a else NA_real_
  icc_c <- if (denom_c > 0) (ms_r - ms_e) / denom_c else NA_real_

  list(icc_agreement = icc_a, icc_consistency = icc_c)
}

#' Compute continuous calibration metrics
#' @noRd
compute_continuous_calibration <- function(pred, truth) {
  p <- as.numeric(pred)
  t <- as.numeric(truth)
  n <- length(p)

  sd_p <- stats::sd(p)
  sd_t <- stats::sd(t)
  pearson_r <- if (n >= 2L && sd_p > 0 && sd_t > 0) stats::cor(p, t) else NA_real_
  spearman_rho <- if (n >= 2L && sd_p > 0 && sd_t > 0) {
    suppressWarnings(stats::cor(p, t, method = "spearman"))
  } else {
    NA_real_
  }

  # Lin's Concordance Correlation Coefficient (CCC)
  var_p <- stats::var(p)
  var_t <- stats::var(t)
  cov_pt <- stats::cov(p, t)
  ccc_denom <- var_p + var_t + (mean(p) - mean(t))^2
  ccc <- if (n >= 2L && ccc_denom > 0) (2 * cov_pt) / ccc_denom else NA_real_

  icc <- compute_icc_two_way(p, t)

  diff_vec <- p - t
  mae <- mean(abs(diff_vec))
  rmse <- sqrt(mean(diff_vec^2))
  bias <- mean(diff_vec)
  sd_ratio <- if (!is.na(sd_t) && sd_t > 0) sd_p / sd_t else NA_real_

  # Calibration regression: truth = intercept + slope * pred + e
  cal_intercept <- NA_real_
  cal_slope <- NA_real_
  r_squared <- NA_real_
  cal_f_stat <- NA_real_
  cal_p_value <- NA_real_

  if (n >= 3L && !is_constant_numeric(p) && !is_constant_numeric(t)) {
    fit <- stats::lm(t ~ p)
    cf <- stats::coef(fit)
    cal_intercept <- unname(cf[1L])
    cal_slope <- unname(cf[2L])
    r_squared <- suppressWarnings(summary(fit)$r.squared)

    # Joint F-test for H0: (intercept = 0, slope = 1)
    rss0 <- sum((t - p)^2)
    rss1 <- sum(stats::residuals(fit)^2)
    df2 <- n - 2L
    if (df2 > 0L && rss1 > 1e-12 * max(rss0, 1)) {
      cal_f_stat <- ((rss0 - rss1) / 2) / (rss1 / df2)
      cal_f_stat <- max(cal_f_stat, 0)
      cal_p_value <- stats::pf(cal_f_stat, df1 = 2, df2 = df2, lower.tail = FALSE)
    } else if (rss1 <= 1e-12 && rss0 <= 1e-12) {
      cal_f_stat <- 0
      cal_p_value <- 1
    }
  }

  metrics <- tibble::tibble(
    n = n,
    pearson_r = pearson_r,
    spearman_rho = spearman_rho,
    ccc = ccc,
    icc_agreement = icc$icc_agreement,
    icc_consistency = icc$icc_consistency,
    mae = mae,
    rmse = rmse,
    bias = bias,
    sd_ratio = sd_ratio,
    cal_intercept = cal_intercept,
    cal_slope = cal_slope,
    r_squared = r_squared,
    cal_f_stat = cal_f_stat,
    cal_p_value = cal_p_value
  )

  list(metrics = metrics, confusion = NULL)
}

#' Compute quantile-binned calibration curve summary
#' @noRd
compute_calibration_bins <- function(pred, truth, n_bins = 10L) {
  n <- length(pred)
  n_eff <- min(n_bins, length(unique(pred)))
  if (n_eff < 2L) {
    return(tibble::tibble(
      bin = 1L,
      n = n,
      pred_mean = mean(pred),
      truth_mean = mean(truth),
      truth_se = if (n > 1L) stats::sd(truth) / sqrt(n) else NA_real_
    ))
  }
  probs <- seq(0, 1, length.out = n_eff + 1L)
  brks <- unique(stats::quantile(pred, probs = probs, na.rm = TRUE))
  if (length(brks) < 2L) {
    bins <- rep(1L, n)
  } else {
    bins <- as.integer(cut(pred, breaks = brks, include.lowest = TRUE))
  }
  u_bins <- sort(unique(bins))
  rows <- lapply(u_bins, function(b) {
    ii <- which(bins == b)
    tibble::tibble(
      bin = b,
      n = length(ii),
      pred_mean = mean(pred[ii]),
      truth_mean = mean(truth[ii]),
      truth_se = if (length(ii) > 1L) stats::sd(truth[ii]) / sqrt(length(ii)) else NA_real_
    )
  })
  do.call(rbind, rows)
}

#' Compute categorical / ordinal calibration metrics
#' @noRd
compute_categorical_calibration <- function(pred, truth, ordinal = FALSE) {
  lvls <- if (is.factor(truth)) {
    union(levels(truth), unique(as.character(pred)))
  } else if (is.factor(pred)) {
    union(levels(pred), unique(as.character(truth)))
  } else if (is.numeric(truth) && is.numeric(pred)) {
    as.character(sort(unique(c(truth[!is.na(truth)], pred[!is.na(pred)]))))
  } else {
    sort(unique(c(as.character(truth), as.character(pred))))
  }
  p_fac <- factor(as.character(pred), levels = lvls)
  t_fac <- factor(as.character(truth), levels = lvls)
  n <- length(p_fac)
  k <- length(lvls)

  cm <- table(pred = p_fac, truth = t_fac)
  cm_mat <- as.matrix(cm)

  acc <- sum(diag(cm_mat)) / n

  # Macro-F1 across observed classes
  f1_vals <- vapply(seq_len(k), function(i) {
    tp <- cm_mat[i, i]
    fp <- sum(cm_mat[i, ]) - tp
    fn <- sum(cm_mat[, i]) - tp
    denom <- 2 * tp + fp + fn
    if (denom == 0) NA_real_ else (2 * tp) / denom
  }, numeric(1L))
  macro_f1 <- if (all(is.na(f1_vals))) NA_real_ else mean(f1_vals, na.rm = TRUE)

  # Cohen's Kappa (unweighted, linear-weighted, quadratic-weighted)
  P_obs <- cm_mat / n
  p_row <- rowSums(P_obs)
  p_col <- colSums(P_obs)
  P_exp <- outer(p_row, p_col)

  calc_kappa <- function(W_disagree) {
    d_o <- sum(W_disagree * P_obs)
    d_e <- sum(W_disagree * P_exp)
    if (d_e <= 1e-12) return(NA_real_)
    1 - d_o / d_e
  }

  idx_mat <- abs(outer(seq_len(k), seq_len(k), "-"))
  W_unw <- ifelse(idx_mat == 0, 0, 1)
  kappa_unw <- calc_kappa(W_unw)
  kappa_lin <- if (k >= 2L) calc_kappa(idx_mat / (k - 1)) else NA_real_
  kappa_quad <- if (k >= 2L) calc_kappa((idx_mat / (k - 1))^2) else NA_real_

  # Krippendorff's alpha (2 raters: exact coincidence matrix formulation)
  O <- cm_mat + t(cm_mat)
  n_c <- rowSums(O)
  tot_pairs <- sum(n_c) # = 2*n
  E_mat <- (outer(n_c, n_c) - diag(n_c)) / max(tot_pairs - 1, 1)

  if (ordinal && k >= 2L) {
    D_mat <- matrix(0, nrow = k, ncol = k)
    for (c_i in seq_len(k)) {
      for (k_i in seq_len(k)) {
        if (c_i != k_i) {
          lo <- min(c_i, k_i)
          hi <- max(c_i, k_i)
          g_sum <- sum(n_c[lo:hi]) - 0.5 * (n_c[c_i] + n_c[k_i])
          D_mat[c_i, k_i] <- g_sum^2
        }
      }
    }
  } else {
    D_mat <- W_unw
  }

  do_krip <- sum(O * D_mat)
  de_krip <- sum(E_mat * D_mat)
  krip_alpha <- if (de_krip > 1e-12) 1 - do_krip / de_krip else NA_real_

  metrics <- tibble::tibble(
    n = n,
    accuracy = acc,
    macro_f1 = macro_f1,
    cohen_kappa = kappa_unw,
    kappa_linear = kappa_lin,
    kappa_quadratic = kappa_quad,
    krippendorff_alpha = krip_alpha
  )

  list(metrics = metrics, confusion = cm)
}

#' @export
print.llm_calibration <- function(x, ...) {
  cli::cli_h3("LLM Calibration Diagnostics ({x$type})")
  cli::cli_text("Prediction: {.val {x$pred_col}} vs Human Gold Standard: {.val {x$truth_col}} (n = {x$metrics$n})")
  if (x$type == "continuous") {
    m <- x$metrics
    cli::cli_text(
      "r = {fmt_num(m$pearson_r)} | rho = {fmt_num(m$spearman_rho)} | CCC = {fmt_num(m$ccc)} | ICC(A,1) = {fmt_num(m$icc_agreement)}"
    )
    cli::cli_text(
      "MAE = {fmt_num(m$mae)} | RMSE = {fmt_num(m$rmse)} | Bias = {fmt_num(m$bias)} | Cal Slope = {fmt_num(m$cal_slope)}, Intercept = {fmt_num(m$cal_intercept)} (p = {fmt_pval(m$cal_p_value)})"
    )
  } else {
    m <- x$metrics
    cli::cli_text(
      "Accuracy = {fmt_num(m$accuracy)} | Macro-F1 = {fmt_num(m$macro_f1)} | Cohen's Kappa = {fmt_num(m$cohen_kappa)} | Krippendorff's Alpha = {fmt_num(m$krippendorff_alpha)}"
    )
  }
  if (!is.null(x$by_group)) {
    cli::cli_text("Stratified by: {.val {x$by}}")
    print(x$by_group, n = min(nrow(x$by_group), 10L))
  }
  invisible(x)
}
