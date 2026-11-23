# Core internal helpers for Prediction-Powered Inference (PPI, PPI++, Clustered/Multilevel)

#' Split single data frame (with NA truth on unlabeled rows) or validate labeled + unlabeled pair
#' @noRd
prepare_ppi_data <- function(data,
                             truth_col,
                             pred_col,
                             unlabeled = NULL,
                             extra_cols = character()) {
  df_main <- extract_data_frame(data, arg = "data")
  assert_scalar_character(truth_col, "truth_col")
  assert_scalar_character(pred_col, "pred_col")
  extra_cols <- unique(extra_cols[!is.na(extra_cols) & nzchar(extra_cols)])

  if (is.null(unlabeled)) {
    assert_columns(df_main, c(truth_col, pred_col, extra_cols), arg = "data")
    is_lab <- !is.na(df_main[[truth_col]])
    labeled_df <- df_main[is_lab, , drop = FALSE]
    unlabeled_df <- df_main[!is_lab, , drop = FALSE]
  } else {
    labeled_df <- df_main
    unlabeled_df <- extract_data_frame(unlabeled, arg = "unlabeled")
    assert_columns(labeled_df, c(truth_col, pred_col, extra_cols), arg = "data")
    assert_columns(unlabeled_df, c(pred_col, extra_cols), arg = "unlabeled")
    labeled_df <- labeled_df[!is.na(labeled_df[[truth_col]]), , drop = FALSE]
  }

  # Drop rows with missing pred_col or extra_cols
  lab_keep <- !is.na(labeled_df[[truth_col]]) & !is.na(labeled_df[[pred_col]])
  if (length(extra_cols) > 0L) {
    lab_keep <- lab_keep & stats::complete.cases(labeled_df[, extra_cols, drop = FALSE])
  }
  labeled_df <- labeled_df[lab_keep, , drop = FALSE]

  unlab_keep <- !is.na(unlabeled_df[[pred_col]])
  if (length(extra_cols) > 0L) {
    unlab_keep <- unlab_keep & stats::complete.cases(unlabeled_df[, extra_cols, drop = FALSE])
  }
  unlabeled_df <- unlabeled_df[unlab_keep, , drop = FALSE]

  if (nrow(labeled_df) < 5L) {
    cli::cli_abort("At least 5 complete labeled validation observations are required (found {nrow(labeled_df)}).")
  }
  if (nrow(unlabeled_df) < 5L) {
    cli::cli_abort(
      "At least 5 complete unlabeled LLM-coded observations are required (found {nrow(unlabeled_df)}). Supply {.arg unlabeled} or rows where {.val {truth_col}} is NA."
    )
  }

  list(labeled = labeled_df, unlabeled = unlabeled_df)
}

#' Build synchronized model matrices on labeled and unlabeled data frames
#' @noRd
build_ppi_model_matrices <- function(formula, labeled_df, unlabeled_df) {
  # Combine covariate columns so factor levels and contrasts match identically across L and U
  rhs_fml <- stats::delete.response(stats::terms(formula, data = labeled_df))
  rhs_vars <- all.vars(rhs_fml)

  n_lab <- nrow(labeled_df)
  n_unlab <- nrow(unlabeled_df)

  if (length(rhs_vars) > 0L) {
    comb <- rbind(
      labeled_df[, rhs_vars, drop = FALSE],
      unlabeled_df[, rhs_vars, drop = FALSE]
    )
    mf_comb <- stats::model.frame(rhs_fml, data = comb, na.action = stats::na.fail)
    X_comb <- stats::model.matrix(rhs_fml, data = mf_comb)
    X_lab <- X_comb[seq_len(n_lab), , drop = FALSE]
    X_unlab <- X_comb[seq.int(n_lab + 1L, n_lab + n_unlab), , drop = FALSE]
  } else {
    X_lab <- matrix(1, nrow = n_lab, ncol = 1L, dimnames = list(NULL, "(Intercept)"))
    X_unlab <- matrix(1, nrow = n_unlab, ncol = 1L, dimnames = list(NULL, "(Intercept)"))
  }

  list(X_lab = X_lab, X_unlab = X_unlab, terms = rhs_fml)
}

#' Normalize canonical GLM family specification (gaussian, binomial, poisson)
#' @noRd
resolve_ppi_family <- function(family) {
  if (is.character(family) && length(family) == 1L) {
    family <- switch(
      tolower(family),
      gaussian = stats::gaussian(),
      binomial = stats::binomial(),
      poisson = stats::poisson(),
      cli::cli_abort("Unsupported family {.val {family}}. Supported families: gaussian, binomial, poisson.")
    )
  } else if (is.function(family)) {
    family <- family()
  }
  if (!inherits(family, "family")) {
    cli::cli_abort("{.arg family} must be a GLM family object or one of \"gaussian\", \"binomial\", \"poisson\".")
  }
  fam_name <- family$family
  if (!(fam_name %in% c("gaussian", "binomial", "poisson"))) {
    cli::cli_abort(
      "Family {.val {fam_name}} is not supported in {.fn ppi_glm}; supported canonical families are gaussian, binomial, and poisson."
    )
  }
  family
}

#' Canonical-link cumulant helpers: b(eta), mu(eta), v(eta)
#' @noRd
canonical_glm_ops <- function(family_name) {
  if (family_name == "gaussian") {
    list(
      b = function(eta) 0.5 * eta^2,
      mu = function(eta) eta,
      var = function(eta) rep(1, length(eta))
    )
  } else if (family_name == "binomial") {
    list(
      b = function(eta) ifelse(eta > 0, eta + log1p(exp(-eta)), log1p(exp(eta))),
      mu = function(eta) {
        eta_c <- pmin(pmax(eta, -30), 30)
        stats::plogis(eta_c)
      },
      var = function(eta) {
        eta_c <- pmin(pmax(eta, -30), 30)
        p <- stats::plogis(eta_c)
        pmax(p * (1 - p), 1e-8)
      }
    )
  } else if (family_name == "poisson") {
    list(
      b = function(eta) exp(pmin(eta, 25)),
      mu = function(eta) exp(pmin(eta, 25)),
      var = function(eta) pmax(exp(pmin(eta, 25)), 1e-8)
    )
  } else {
    cli::cli_abort("Unsupported canonical GLM family {.val {family_name}}.")
  }
}

# Solve canonical-link GLM M-estimation PPI++ objective via damped Newton-Raphson
# Minimizes:
#   L_lambda(theta) = (1/n) sum_L (b(X_L theta) - Y_L X_L theta)
#                   + lambda * ( (1/N) sum_U (b(X_U theta) - Yhat_U X_U theta)
#                              - (1/n) sum_L (b(X_L theta) - Yhat_L X_L theta) )
solve_ppi_glm_objective <- function(X_lab, y_lab, yhat_lab, X_unlab, yhat_unlab,
                                    family_name, lambda,
                                    w_lab = NULL, w_unlab = NULL,
                                    start = NULL, max_iter = 60L, tol = 1e-9) {
  n <- nrow(X_lab)
  N <- nrow(X_unlab)
  p <- ncol(X_lab)
  ops <- canonical_glm_ops(family_name)

  if (is.null(w_lab)) w_lab <- rep(1, n)
  if (is.null(w_unlab)) w_unlab <- rep(1, N)
  # Normalize weights so mean weight = 1
  w_lab <- w_lab / mean(w_lab)
  w_unlab <- w_unlab / mean(w_unlab)

  if (family_name == "gaussian") {
    H_L <- crossprod(X_lab, X_lab * w_lab) / n
    H_U <- crossprod(X_unlab, X_unlab * w_unlab) / N
    H <- (1 - lambda) * H_L + lambda * H_U
    rhs <- crossprod(X_lab, w_lab * (y_lab - lambda * yhat_lab)) / n +
      lambda * crossprod(X_unlab, w_unlab * yhat_unlab) / N
    theta <- as.numeric(safe_solve(H, rhs))
    names(theta) <- colnames(X_lab)
    return(list(theta = theta, hessian = H, converged = TRUE, iterations = 1L))
  }

  # Target linear term does not depend on theta
  lin_rhs <- as.numeric(
    crossprod(X_lab, w_lab * (y_lab - lambda * yhat_lab)) / n +
      lambda * crossprod(X_unlab, w_unlab * yhat_unlab) / N
  )

  obj_fn <- function(th) {
    eta_L <- as.numeric(X_lab %*% th)
    eta_U <- as.numeric(X_unlab %*% th)
    val <- (1 - lambda) * mean(w_lab * ops$b(eta_L)) +
      lambda * mean(w_unlab * ops$b(eta_U)) -
      sum(th * lin_rhs)
    val
  }

  grad_hess <- function(th) {
    eta_L <- as.numeric(X_lab %*% th)
    eta_U <- as.numeric(X_unlab %*% th)
    mu_L <- ops$mu(eta_L)
    mu_U <- ops$mu(eta_U)
    v_L <- ops$var(eta_L)
    v_U <- ops$var(eta_U)

    grad <- as.numeric(
      (1 - lambda) * crossprod(X_lab, w_lab * mu_L) / n +
        lambda * crossprod(X_unlab, w_unlab * mu_U) / N -
        lin_rhs
    )
    H <- (1 - lambda) * crossprod(X_lab, X_lab * (w_lab * v_L)) / n +
      lambda * crossprod(X_unlab, X_unlab * (w_unlab * v_U)) / N
    list(grad = grad, H = H)
  }

  theta <- if (!is.null(start) && length(start) == p && all(is.finite(start))) {
    as.numeric(start)
  } else {
    rep(0, p)
  }

  converged <- FALSE
  cur_obj <- obj_fn(theta)
  iter_used <- 0L

  for (iter in seq_len(max_iter)) {
    iter_used <- iter
    gh <- grad_hess(theta)
    if (max(abs(gh$grad)) < tol) {
      converged <- TRUE
      break
    }
    # Add tiny Levenberg damping only if near-singular
    step <- as.numeric(safe_solve(gh$H + diag(1e-10, p), gh$grad))
    if (!all(is.finite(step))) {
      break
    }

    # Backtracking line search ensuring Armijo decrease
    alpha <- 1
    accepted <- FALSE
    dir_deriv <- sum(gh$grad * step)
    for (ls in seq_len(25L)) {
      cand <- theta - alpha * step
      cand_obj <- obj_fn(cand)
      if (is.finite(cand_obj) && cand_obj <= cur_obj - 1e-4 * alpha * max(dir_deriv, 0) + 1e-12) {
        theta <- cand
        cur_obj <- cand_obj
        accepted <- TRUE
        break
      }
      alpha <- alpha * 0.5
    }
    if (!accepted) {
      break
    }
    if (max(abs(alpha * step)) < tol) {
      converged <- TRUE
      break
    }
  }

  gh_final <- grad_hess(theta)
  names(theta) <- colnames(X_lab)
  dimnames(gh_final$H) <- list(colnames(X_lab), colnames(X_lab))

  list(
    theta = theta,
    hessian = gh_final$H,
    converged = converged,
    iterations = iter_used
  )
}

#' Compute optimal scalar power-tuning lambda* for i.i.d. M-estimation PPI++
#' @noRd
compute_optimal_lambda_iid <- function(G_lab, G_tilde_lab, G_tilde_unlab, H, clip = TRUE) {
  n <- nrow(G_lab)
  N <- nrow(G_tilde_unlab)
  H_inv <- safe_solve(H)

  C_gg <- safe_cross_cov(G_lab, G_tilde_lab)
  # Pool unlabeled + labeled LLM gradients for lower-variance V_tilde estimate
  V_tilde <- safe_cov(rbind(G_tilde_lab, G_tilde_unlab))

  num <- sum(diag(H_inv %*% C_gg %*% H_inv))
  denom <- (1 + n / N) * sum(diag(H_inv %*% V_tilde %*% H_inv))

  if (!is.finite(denom) || denom <= 1e-12) {
    return(0)
  }
  lam <- num / denom
  if (!is.finite(lam)) {
    return(0)
  }
  if (isTRUE(clip)) {
    lam <- min(max(lam, 0), 1)
  }
  lam
}

#' Compute i.i.d. PPI++ sandwich covariance matrix with HC1 small-sample correction
#' @noRd
compute_ppi_vcov_iid <- function(G_lab, G_tilde_lab, G_tilde_unlab, H, lambda) {
  n <- nrow(G_lab)
  N <- nrow(G_tilde_unlab)
  p <- ncol(G_lab)
  H_inv <- safe_solve(H)

  D_lab <- G_lab - lambda * G_tilde_lab
  # HC1-style finite-sample df adjustment on labeled set
  df_adj_lab <- if (n > p) (n - 1) / (n - p) else 1
  V_lab <- df_adj_lab * safe_cov(D_lab)
  V_unlab <- safe_cov(G_tilde_unlab)

  meat <- (1 / n) * V_lab + (lambda^2 / N) * V_unlab
  V <- H_inv %*% meat %*% H_inv
  V <- 0.5 * (V + t(V))
  dimnames(V) <- list(colnames(G_lab), colnames(G_lab))
  V
}

#' Format an estimate vector and covariance matrix into a tidy inference table
#' @noRd
tidy_ppi_table <- function(theta, vcov_mat, method_label, conf_level = 0.95, df = Inf) {
  terms <- names(theta)
  if (is.null(terms)) {
    terms <- paste0("param_", seq_along(theta))
  }
  se <- sqrt(pmax(diag(as.matrix(vcov_mat)), 0))
  stat <- ifelse(se > 0, theta / se, NA_real_)
  alpha <- 1 - conf_level

  if (is.finite(df) && df > 0) {
    crit <- stats::qt(1 - alpha / 2, df = df)
    pval <- ifelse(is.na(stat), NA_real_, 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE))
  } else {
    crit <- stats::qnorm(1 - alpha / 2)
    pval <- ifelse(is.na(stat), NA_real_, 2 * stats::pnorm(abs(stat), lower.tail = FALSE))
  }

  tibble::tibble(
    method = rep(method_label, length(theta)),
    term = terms,
    estimate = as.numeric(theta),
    std_error = as.numeric(se),
    conf_low = as.numeric(theta - crit * se),
    conf_high = as.numeric(theta + crit * se),
    statistic = as.numeric(stat),
    p_value = as.numeric(pval)
  )
}

#' Fit canonical GLM benchmark (human-only or naive-LLM) with robust HC1 sandwich SE
#' @noRd
fit_glm_benchmark <- function(X, y, family_name, w = NULL) {
  n <- nrow(X)
  p <- ncol(X)
  if (is.null(w)) w <- rep(1, n)
  w <- w / mean(w)

  # Reuse canonical objective solver with lambda = 0 (pure single-sample MLE)
  sol <- solve_ppi_glm_objective(
    X_lab = X, y_lab = y, yhat_lab = rep(0, n),
    X_unlab = X[1L, , drop = FALSE], yhat_unlab = 0,
    family_name = family_name, lambda = 0, w_lab = w
  )
  ops <- canonical_glm_ops(family_name)
  mu <- ops$mu(as.numeric(X %*% sol$theta))
  G <- X * (w * (mu - y))
  H_inv <- safe_solve(sol$hessian)
  df_adj <- if (n > p) (n - 1) / (n - p) else 1
  V <- H_inv %*% ((df_adj / n) * safe_cov(G)) %*% H_inv
  V <- 0.5 * (V + t(V))
  dimnames(V) <- list(colnames(X), colnames(X))

  list(theta = sol$theta, vcov = V, hessian = sol$hessian, scores = G)
}

#' Construct standardized `llm_ppi` S3 object
#' @noRd
new_llm_ppi <- function(subclass,
                        estimates,
                        benchmarks,
                        theta,
                        vcov_mat,
                        lambda,
                        method_label,
                        n_labeled,
                        n_unlabeled,
                        conf_level,
                        df = Inf,
                        extra = list()) {
  # Compute per-term variance reduction / effective sample size multiplier vs human_only
  hum_tbl <- benchmarks[benchmarks$method == "human_only", , drop = FALSE]
  eff_tbl <- tibble::tibble(
    term = estimates$term,
    var_ratio_vs_human = (hum_tbl$std_error^2) / pmax(estimates$std_error^2, 1e-16),
    se_reduction_pct = 100 * (1 - estimates$std_error / pmax(hum_tbl$std_error, 1e-16))
  )

  obj <- c(
    list(
      estimates = estimates,
      benchmarks = benchmarks,
      efficiency = eff_tbl,
      coefficients = theta,
      vcov = vcov_mat,
      lambda = lambda,
      method = method_label,
      n_labeled = as.integer(n_labeled),
      n_unlabeled = as.integer(n_unlabeled),
      conf_level = conf_level,
      df = df
    ),
    extra
  )
  structure(obj, class = c(subclass, "llm_ppi"))
}
