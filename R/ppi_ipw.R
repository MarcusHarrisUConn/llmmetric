# Internal Horvitz-Thompson / Active IPW-PPI engines for ppi_mean() and ppi_lm()

#' Extract and validate labeled IPW sampling weights aligned with prepare_ppi_data()
#' @noRd
resolve_ipw_data_and_weights <- function(data, truth_col, pred_col, unlabeled = NULL,
                                         extra_cols = character(),
                                         weights = NULL, inclusion_prob = NULL) {
  df_main <- extract_data_frame(data, arg = "data")
  w_col_internal <- ".llmmetric_ipw_w"

  # Resolve weights or inclusion_prob onto df_main before prepare_ppi_data() filters NAs
  if (!is.null(inclusion_prob)) {
    if (is.character(inclusion_prob) && length(inclusion_prob) == 1L) {
      assert_columns(df_main, inclusion_prob, arg = "data")
      pi_raw <- as.numeric(df_main[[inclusion_prob]])
    } else if (is.numeric(inclusion_prob)) {
      if (length(inclusion_prob) == nrow(df_main)) {
        pi_raw <- as.numeric(inclusion_prob)
      } else if (is.null(unlabeled) && length(inclusion_prob) == sum(!is.na(df_main[[truth_col]]))) {
        pi_raw <- rep(NA_real_, nrow(df_main))
        pi_raw[!is.na(df_main[[truth_col]])] <- as.numeric(inclusion_prob)
      } else {
        cli::cli_abort(
          "{.arg inclusion_prob} must be a column name in {.arg data} or a numeric vector matching {.code nrow(data)} (or labeled rows)."
        )
      }
    } else {
      cli::cli_abort("{.arg inclusion_prob} must be a column name or numeric vector.")
    }
    lab_mask <- !is.na(df_main[[truth_col]])
    pi_lab_chk <- pi_raw[lab_mask]
    if (anyNA(pi_lab_chk) || any(!is.finite(pi_lab_chk)) || any(pi_lab_chk <= 0 | pi_lab_chk > 1)) {
      cli::cli_abort("All labeled {.arg inclusion_prob} values must lie strictly in (0, 1].")
    }
    # Enforce Horvitz-Thompson positivity across all supplied non-NA inclusion probabilities
    pi_non_na <- pi_raw[!is.na(pi_raw)]
    if (length(pi_non_na) > 0L && (any(!is.finite(pi_non_na)) || any(pi_non_na <= 0 | pi_non_na > 1))) {
      cli::cli_abort(
        "All {.arg inclusion_prob} values must lie strictly in (0, 1] (zero or non-finite inclusion probabilities violate Horvitz-Thompson positivity)."
      )
    }
    w_raw <- ifelse(is.na(pi_raw) | pi_raw <= 0, NA_real_, 1 / pi_raw)
  } else {
    if (is.character(weights) && length(weights) == 1L) {
      assert_columns(df_main, weights, arg = "data")
      w_raw <- as.numeric(df_main[[weights]])
    } else if (is.numeric(weights)) {
      if (length(weights) == nrow(df_main)) {
        w_raw <- as.numeric(weights)
      } else if (is.null(unlabeled) && length(weights) == sum(!is.na(df_main[[truth_col]]))) {
        w_raw <- rep(NA_real_, nrow(df_main))
        w_raw[!is.na(df_main[[truth_col]])] <- as.numeric(weights)
      } else {
        cli::cli_abort(
          "{.arg weights} must be a column name in {.arg data} or a numeric vector matching {.code nrow(data)} (or labeled rows)."
        )
      }
    } else {
      cli::cli_abort("{.arg weights} must be a column name or numeric vector.")
    }
    lab_mask <- !is.na(df_main[[truth_col]])
    w_lab_chk <- w_raw[lab_mask]
    if (anyNA(w_lab_chk) || any(!is.finite(w_lab_chk)) || any(w_lab_chk <= 0)) {
      cli::cli_abort("All labeled {.arg weights} values must be strictly positive finite numbers.")
    }
    # Reject infinite/non-positive weights anywhere in the supplied vector (e.g. from zero-probability strata)
    w_non_na <- w_raw[!is.na(w_raw)]
    if (length(w_non_na) > 0L && (any(!is.finite(w_non_na)) || any(w_non_na <= 0))) {
      cli::cli_abort(
        "All {.arg weights} values must be strictly positive finite numbers (infinite or zero weights violate Horvitz-Thompson positivity)."
      )
    }
  }

  df_main[[w_col_internal]] <- w_raw
  if (!is.null(unlabeled)) {
    unlab_df <- extract_data_frame(unlabeled, arg = "unlabeled")
    unlab_df[[w_col_internal]] <- 1 # placeholder for complete.cases filtering on unlabeled
  } else {
    unlab_df <- NULL
    df_main[[w_col_internal]][is.na(df_main[[truth_col]])] <- 1
  }

  sp <- prepare_ppi_data(
    data = df_main,
    truth_col = truth_col,
    pred_col = pred_col,
    unlabeled = unlab_df,
    extra_cols = unique(c(extra_cols, w_col_internal))
  )

  w_lab_raw <- as.numeric(sp$labeled[[w_col_internal]])
  n <- nrow(sp$labeled)
  N <- nrow(sp$unlabeled)
  M <- n + N
  # Exact Hajek normalization so sum_{i in L} w_i = M (unchanged when exact stratified weights sum to M)
  w_lab <- w_lab_raw * (M / sum(w_lab_raw))

  list(labeled = sp$labeled, unlabeled = sp$unlabeled, w_lab = w_lab, n = n, N = N, M = M)
}

#' Exact Horvitz-Thompson Active / IPW-PPI Mean Estimator (Zrnic & Candes, 2024)
#' @noRd
fit_ipw_ppi_mean <- function(data, truth_col, pred_col, unlabeled, lambda,
                             conf_level, clip_lambda, weights, inclusion_prob) {
  ipw <- resolve_ipw_data_and_weights(
    data = data, truth_col = truth_col, pred_col = pred_col,
    unlabeled = unlabeled, weights = weights, inclusion_prob = inclusion_prob
  )
  y_lab <- as.numeric(ipw$labeled[[truth_col]])
  f_lab <- as.numeric(ipw$labeled[[pred_col]])
  f_unlab <- as.numeric(ipw$unlabeled[[pred_col]])
  w_lab <- ipw$w_lab
  n <- ipw$n
  N <- ipw$N
  M <- ipw$M

  y_pad <- c(y_lab, rep(0, N))
  f_all <- c(f_lab, f_unlab)
  rw <- c(w_lab, rep(0, N)) # length M, mean(rw) == 1

  n_eff <- (sum(w_lab)^2) / sum(w_lab^2)
  if (!is.finite(n_eff) || n_eff < 2) {
    cli::cli_abort(
      "Kish effective labeled sample size under IPW weights (n_eff = {round(n_eff, 2L)}) is less than 2; variance is unidentifiable."
    )
  }

  # IPW Human-Only benchmark (lambda = 0)
  est_hum <- mean(rw * y_pad)
  phi_hum <- rw * (y_pad - est_hum)
  df_adj_hum <- n_eff / max(n_eff - 1, 1)
  var_hum <- df_adj_hum * stats::var(phi_hum) / M

  # Optimal IPW lambda* minimizing Var(a_i + lambda * b_i)
  if (identical(lambda, "auto")) {
    a_vec <- rw * (y_pad - est_hum)
    b_vec <- (1 - rw) * (f_all - mean(f_all))
    vb <- stats::var(b_vec)
    cab <- stats::cov(a_vec, b_vec)
    lam_val <- if (is.finite(vb) && vb > 1e-12 && is.finite(cab)) -cab / vb else 0
    if (isTRUE(clip_lambda)) {
      lam_val <- min(max(lam_val, 0), 1)
    }
    method_label <- "ipw_ppi++"
  } else {
    assert_scalar_numeric(lambda, "lambda")
    lam_val <- as.numeric(lambda)
    method_label <- if (abs(lam_val - 1) < 1e-12) "ipw_ppi" else paste0("ipw_ppi(lambda=", round(lam_val, 2L), ")")
  }

  df_ref <- Inf
  small_sample <- (n < 20L) || (n_eff < 20)
  if (small_sample) {
    df_ref <- max(min(n, n_eff) - 1, 1)
    cli::cli_warn(
      "Small effective labeled validation sample (n = {n}, Kish n_eff = {round(n_eff, 1L)}); using t({round(df_ref, 1L)}) reference distribution for finite-sample inference."
    )
  }

  # Exact Active / IPW-PPI mean estimator and sandwich variance
  est_ppi <- mean(lam_val * f_all + rw * (y_pad - lam_val * f_all))
  phi_ppi <- lam_val * (f_all - est_ppi) + rw * ((y_pad - est_ppi) - lam_val * (f_all - est_ppi))
  lab_res <- rw * ((y_pad - est_ppi) - lam_val * (f_all - est_ppi))
  ss_phi <- sum((phi_ppi - mean(phi_ppi))^2)
  lab_share <- if (ss_phi > 1e-16) min(sum(lab_res^2) / ss_phi, 1) else 0
  df_adj_ppi <- 1 + (1 / max(n_eff - 1, 1)) * lab_share
  var_ppi <- df_adj_ppi * stats::var(phi_ppi) / M

  theta_ppi <- stats::setNames(est_ppi, "(Mean)")
  V_ppi <- matrix(max(var_ppi, 0), 1L, 1L, dimnames = list("(Mean)", "(Mean)"))

  theta_hum <- stats::setNames(est_hum, "(Mean)")
  V_hum <- matrix(max(var_hum, 0), 1L, 1L, dimnames = list("(Mean)", "(Mean)"))

  theta_naive <- stats::setNames(mean(f_all), "(Mean)")
  V_naive <- matrix(max(stats::var(f_all) / M, 0), 1L, 1L, dimnames = list("(Mean)", "(Mean)"))

  tbl_ppi <- tidy_ppi_table(theta_ppi, V_ppi, method_label = method_label, conf_level = conf_level, df = df_ref)
  tbl_hum <- tidy_ppi_table(theta_hum, V_hum, method_label = "human_only", conf_level = conf_level, df = df_ref)
  tbl_naive <- tidy_ppi_table(theta_naive, V_naive, method_label = "naive_llm", conf_level = conf_level, df = Inf)

  new_llm_ppi(
    subclass = "llm_ppi_mean",
    estimates = tbl_ppi,
    benchmarks = rbind(tbl_ppi, tbl_hum, tbl_naive),
    theta = theta_ppi,
    vcov_mat = V_ppi,
    lambda = lam_val,
    method_label = method_label,
    n_labeled = n,
    n_unlabeled = N,
    conf_level = conf_level,
    df = df_ref,
    extra = list(
      truth_col = truth_col,
      pred_col = pred_col,
      weighted = TRUE,
      effective_n_weights = (sum(w_lab)^2) / sum(w_lab^2),
      small_sample = small_sample
    )
  )
}

#' Exact Horvitz-Thompson Active / IPW-PPI Linear Regression Estimator
#' @noRd
fit_ipw_ppi_lm <- function(formula, data, pred_col, unlabeled, method, lambda,
                           conf_level, weights, inclusion_prob) {
  truth_col <- all.vars(formula[[2L]])
  rhs_vars <- all.vars(formula[[3L]])

  ipw <- resolve_ipw_data_and_weights(
    data = data, truth_col = truth_col, pred_col = pred_col,
    unlabeled = unlabeled, extra_cols = rhs_vars,
    weights = weights, inclusion_prob = inclusion_prob
  )

  mats <- build_ppi_model_matrices(formula, ipw$labeled, ipw$unlabeled)
  rank_info <- check_design_rank(mats$X_lab, mats$X_unlab)
  if (!isTRUE(rank_info$full_rank)) {
    cli::cli_warn(
      "Rank-deficient design matrix detected; dropping aliased column{?s}: {.val {rank_info$aliased_cols}}."
    )
  }
  X_lab <- mats$X_lab[, rank_info$keep_idx, drop = FALSE]
  X_unlab <- mats$X_unlab[, rank_info$keep_idx, drop = FALSE]
  X_all <- rbind(X_lab, X_unlab)

  y_lab <- as.numeric(ipw$labeled[[truth_col]])
  yhat_lab <- as.numeric(ipw$labeled[[pred_col]])
  yhat_unlab <- as.numeric(ipw$unlabeled[[pred_col]])
  w_lab <- ipw$w_lab
  n <- ipw$n
  N <- ipw$N
  M <- ipw$M
  p <- ncol(X_lab)

  if (n <= p) {
    cli::cli_abort("Number of labeled observations (n = {n}) must exceed identifiable parameters (p = {p}).")
  }
  n_eff <- (sum(w_lab)^2) / sum(w_lab^2)
  if (!is.finite(n_eff) || n_eff <= p) {
    cli::cli_abort(
      "Kish effective labeled sample size under IPW weights (n_eff = {round(n_eff, 2L)}) must exceed identifiable parameters (p = {p})."
    )
  }

  df_ref <- Inf
  small_sample <- (n < max(20L, 2L * p + 5L)) || (n_eff < max(20, 2 * p + 5))
  if (small_sample) {
    df_ref <- max(min(n, n_eff) - p, 1)
    cli::cli_warn(
      "Small effective labeled validation sample (n = {n}, Kish n_eff = {round(n_eff, 1L)}, p = {p}); using t({round(df_ref, 1L)}) reference distribution for finite-sample inference."
    )
  }

  y_pad <- c(y_lab, rep(0, N))
  f_all <- c(yhat_lab, yhat_unlab)
  rw <- c(w_lab, rep(0, N)) # mean(rw) == 1

  # IPW Human-Only WLS benchmark (lambda = 0)
  H_hum <- crossprod(X_all, X_all * rw) / M
  rhs_hum <- as.numeric(crossprod(X_all, rw * y_pad) / M)
  theta_hum <- as.numeric(safe_solve(H_hum, rhs_hum))
  names(theta_hum) <- colnames(X_lab)
  Phi_hum <- X_all * (rw * (as.numeric(X_all %*% theta_hum) - y_pad))
  H_hum_inv <- safe_solve(H_hum)
  V_hum_sub <- (n / max(n - p, 1)) * (1 / M) * (H_hum_inv %*% safe_cov(Phi_hum) %*% H_hum_inv)
  V_hum_sub <- 0.5 * (V_hum_sub + t(V_hum_sub))
  dimnames(V_hum_sub) <- list(colnames(X_lab), colnames(X_lab))

  # Naive LLM OLS benchmark (unweighted over full corpus)
  bench_naive <- fit_glm_benchmark(X_all, f_all, family_name = "gaussian")

  # Optimal IPW power-tuning lambda* in [0, 1]
  if (!is.null(lambda)) {
    assert_scalar_numeric(lambda, "lambda", min = 0, max = 1)
    lam_val <- as.numeric(lambda)
    method_label <- if (abs(lam_val - 1) < 1e-12) "ipw_ppi" else paste0("ipw_ppi(lambda=", round(lam_val, 2L), ")")
  } else if (method == "ppi") {
    lam_val <- 1
    method_label <- "ipw_ppi"
  } else {
    H_pool <- crossprod(X_all) / M
    H_inv_p <- safe_solve(H_pool)
    eta0 <- as.numeric(X_all %*% theta_hum)
    A_mat <- X_all * (rw * (eta0 - y_pad))
    B_mat <- X_all * ((1 - rw) * (eta0 - f_all))
    C_ab <- safe_cross_cov(A_mat, B_mat)
    V_b <- safe_cov(B_mat)
    num <- -sum(diag(H_inv_p %*% C_ab %*% H_inv_p))
    denom <- sum(diag(H_inv_p %*% V_b %*% H_inv_p))
    lam_val <- if (is.finite(denom) && denom > 1e-12 && is.finite(num)) min(max(num / denom, 0), 1) else 0
    method_label <- "ipw_ppi++"
  }

  # Solve exact IPW-PPI normal equations
  w_hess <- lam_val + rw * (1 - lam_val)
  H_ppi <- crossprod(X_all, X_all * w_hess) / M
  rhs_ppi <- as.numeric(crossprod(X_all, lam_val * f_all + rw * (y_pad - lam_val * f_all)) / M)
  theta_ppi <- as.numeric(safe_solve(H_ppi, rhs_ppi))
  names(theta_ppi) <- colnames(X_lab)

  # Exact IPW-PPI sandwich covariance
  eta_hat <- as.numeric(X_all %*% theta_ppi)
  Phi_ppi <- X_all * (lam_val * (eta_hat - f_all) + rw * ((eta_hat - y_pad) - lam_val * (eta_hat - f_all)))
  Lab_dev <- X_all * (rw * ((eta_hat - y_pad) - lam_val * (eta_hat - f_all)))
  ss_phi <- sum(Phi_ppi^2)
  lab_share <- if (ss_phi > 1e-16) min(sum(Lab_dev^2) / ss_phi, 1) else 0
  df_adj <- 1 + (p / max(n - p, 1)) * lab_share

  H_ppi_inv <- safe_solve(H_ppi)
  V_ppi_sub <- df_adj * (1 / M) * (H_ppi_inv %*% safe_cov(Phi_ppi) %*% H_ppi_inv)
  V_ppi_sub <- 0.5 * (V_ppi_sub + t(V_ppi_sub))
  dimnames(V_ppi_sub) <- list(colnames(X_lab), colnames(X_lab))

  exp_ppi <- expand_aliased_results(theta_ppi, V_ppi_sub, rank_info)
  exp_hum <- expand_aliased_results(theta_hum, V_hum_sub, rank_info)
  exp_naive <- expand_aliased_results(bench_naive$theta, bench_naive$vcov, rank_info)

  tbl_ppi <- tidy_ppi_table(exp_ppi$theta, exp_ppi$vcov, method_label = method_label, conf_level = conf_level, df = df_ref)
  tbl_hum <- tidy_ppi_table(exp_hum$theta, exp_hum$vcov, method_label = "human_only", conf_level = conf_level, df = df_ref)
  tbl_naive <- tidy_ppi_table(exp_naive$theta, exp_naive$vcov, method_label = "naive_llm", conf_level = conf_level, df = Inf)

  new_llm_ppi(
    subclass = "llm_ppi_lm",
    estimates = tbl_ppi,
    benchmarks = rbind(tbl_ppi, tbl_hum, tbl_naive),
    theta = exp_ppi$theta,
    vcov_mat = exp_ppi$vcov,
    lambda = lam_val,
    method_label = method_label,
    n_labeled = n,
    n_unlabeled = N,
    conf_level = conf_level,
    df = df_ref,
    extra = list(
      formula = formula,
      family = "gaussian",
      pred_col = pred_col,
      weighted = TRUE,
      effective_n_weights = (sum(w_lab)^2) / sum(w_lab^2),
      aliased_terms = rank_info$aliased_cols,
      small_sample = small_sample
    )
  )
}
