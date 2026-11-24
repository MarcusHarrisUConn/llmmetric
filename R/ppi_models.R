#' Prediction-Powered Mean Estimation (PPI / PPI++ / Active IPW-PPI)
#'
#' Estimates the population mean \eqn{\theta^* = \mathbb{E}[Y]} by combining
#' a human-validated sample (`truth_col` observed) with a larger LLM-coded
#' unlabeled sample (`pred_col` only). Implements both standard PPI
#' (`lambda = 1`) and power-tuned PPI++ (`lambda = "auto"`), and supports
#' Horvitz-Thompson inverse-probability-weighted (IPW) active/stratified
#' validation designs via `weights` or `inclusion_prob` (e.g., produced by
#' [select_validation_sample()]).
#'
#' @param data A `data.frame` (or `llm_coded_data`) containing `truth_col` and
#'   `pred_col`. When `unlabeled = NULL`, rows with non-missing `truth_col`
#'   form the labeled validation set and rows with `is.na(truth_col)` form the
#'   unlabeled set.
#' @param truth_col Name of the numeric human gold-standard column.
#' @param pred_col Name of the numeric LLM prediction column.
#' @param unlabeled Optional separate `data.frame` of unlabeled observations
#'   containing `pred_col`.
#' @param lambda Power-tuning parameter: `"auto"` (default, optimal PPI++),
#'   `1` (standard PPI), `0` (human-only), or any finite numeric scalar.
#' @param conf_level Confidence level in `(0, 1)`. Default `0.95`.
#' @param clip_lambda Logical; if `TRUE`, constrains auto-tuned `lambda` to
#'   `[0, 1]`. Default `FALSE` (unconstrained optimal regression control variate
#'   is always convex for mean estimation).
#' @param weights Optional Horvitz-Thompson sampling weights \eqn{w_i > 0} for
#'   stratified/active validation sampling: either a column name in `data` or a
#'   numeric vector aligned with `data` (or the labeled subset). Mutually
#'   exclusive with `inclusion_prob`.
#' @param inclusion_prob Optional validation inclusion probabilities
#'   \eqn{\pi_i \in (0, 1]} (converting to \eqn{w_i = 1 / \pi_i}): either a
#'   column name in `data` or a numeric vector. Mutually exclusive with
#'   `weights`.
#'
#' @return An S3 object of class `c("llm_ppi_mean", "llm_ppi")` containing
#'   `$estimates`, `$benchmarks`, `$efficiency`, `$lambda`, `$vcov`, and S3
#'   methods (`print()`, `summary()`, `coef()`, `vcov()`, `confint()`,
#'   `autoplot()`).
#' @export
#'
#' @examples
#' set.seed(42)
#' n_tot <- 600
#' y_true <- rnorm(n_tot, mean = 3.5, sd = 1.2)
#' y_llm <- 0.8 + 0.75 * y_true + rnorm(n_tot, sd = 0.6)
#' df <- tibble::tibble(
#'   human_score = c(y_true[1:100], rep(NA_real_, n_tot - 100)),
#'   llm_score = y_llm
#' )
#' fit <- ppi_mean(df, truth_col = "human_score", pred_col = "llm_score")
#' fit
ppi_mean <- function(data,
                     truth_col = "human_score",
                     pred_col = "llm_score",
                     unlabeled = NULL,
                     lambda = "auto",
                     conf_level = 0.95,
                     clip_lambda = FALSE,
                     weights = NULL,
                     inclusion_prob = NULL) {
  assert_scalar_numeric(conf_level, "conf_level", min = 0.5, max = 0.9999)
  assert_flag(clip_lambda, "clip_lambda")

  if (!is.null(weights) && !is.null(inclusion_prob)) {
    cli::cli_abort("Specify at most one of {.arg weights} or {.arg inclusion_prob}.")
  }

  # Handle weighted / IPW active-inference mode cleanly
  if (!is.null(weights) || !is.null(inclusion_prob)) {
    return(fit_ipw_ppi_mean(
      data = data,
      truth_col = truth_col,
      pred_col = pred_col,
      unlabeled = unlabeled,
      lambda = lambda,
      conf_level = conf_level,
      clip_lambda = clip_lambda,
      weights = weights,
      inclusion_prob = inclusion_prob
    ))
  }

  sp <- prepare_ppi_data(data, truth_col = truth_col, pred_col = pred_col, unlabeled = unlabeled)
  y_lab <- as.numeric(sp$labeled[[truth_col]])
  f_lab <- as.numeric(sp$labeled[[pred_col]])
  f_unlab <- as.numeric(sp$unlabeled[[pred_col]])
  n <- length(y_lab)
  N <- length(f_unlab)

  if (identical(lambda, "auto")) {
    cov_yf <- stats::cov(y_lab, f_lab)
    var_f_eff <- stats::var(f_lab) + (n / N) * stats::var(f_unlab)
    lam_val <- if (is.finite(var_f_eff) && var_f_eff > 1e-12 && is.finite(cov_yf)) {
      cov_yf / var_f_eff
    } else {
      0
    }
    if (isTRUE(clip_lambda)) {
      lam_val <- min(max(lam_val, 0), 1)
    }
    method_label <- "ppi++"
  } else {
    assert_scalar_numeric(lambda, "lambda")
    lam_val <- as.numeric(lambda)
    method_label <- if (abs(lam_val - 1) < 1e-12) "ppi" else paste0("ppi(lambda=", round(lam_val, 2L), ")")
  }

  df_ref <- Inf
  small_sample <- n < 20L
  if (small_sample) {
    df_ref <- max(n - 1L, 1L)
    cli::cli_warn(
      "Small labeled validation sample (n = {n}); using t({df_ref}) reference distribution for finite-sample inference."
    )
  }

  # PPI / PPI++ mean estimator & exact variance
  est_ppi <- mean(y_lab) + lam_val * (mean(f_unlab) - mean(f_lab))
  var_ppi <- stats::var(y_lab - lam_val * f_lab) / n + (lam_val^2) * stats::var(f_unlab) / N

  theta_ppi <- stats::setNames(est_ppi, "(Mean)")
  V_ppi <- matrix(max(var_ppi, 0), nrow = 1L, ncol = 1L, dimnames = list("(Mean)", "(Mean)"))

  # Benchmarks: human-only and naive LLM (all observations)
  est_hum <- stats::setNames(mean(y_lab), "(Mean)")
  V_hum <- matrix(max(stats::var(y_lab) / n, 0), 1L, 1L, dimnames = list("(Mean)", "(Mean)"))

  f_all <- c(f_lab, f_unlab)
  est_naive <- stats::setNames(mean(f_all), "(Mean)")
  V_naive <- matrix(max(stats::var(f_all) / length(f_all), 0), 1L, 1L, dimnames = list("(Mean)", "(Mean)"))

  tbl_ppi <- tidy_ppi_table(theta_ppi, V_ppi, method_label = method_label, conf_level = conf_level, df = df_ref)
  tbl_hum <- tidy_ppi_table(est_hum, V_hum, method_label = "human_only", conf_level = conf_level, df = df_ref)
  tbl_naive <- tidy_ppi_table(est_naive, V_naive, method_label = "naive_llm", conf_level = conf_level, df = Inf)

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
    extra = list(truth_col = truth_col, pred_col = pred_col, small_sample = small_sample)
  )
}

#' Prediction-Powered Linear Regression (PPI / PPI++ / Calibrated Control Variate)
#'
#' Fits an error-corrected linear regression model combining a human-validated
#' subset with a larger LLM-coded dataset. Supports:
#' - `"ppi++"` (default): Angelopoulos et al. (2024) M-estimation PPI++ with
#'   optimal power-tuning \eqn{\hat{\lambda}^* \in [0, 1]}.
#' - `"ppi"`: standard Prediction-Powered Inference (`lambda = 1`).
#' - `"calibrated"`: coordinate-wise influence-function control-variate
#'   calibration \eqn{\hat{\theta}_L + \hat{\Lambda}^*(\hat{\theta}_{\tilde{U}} - \hat{\theta}_{\tilde{L}})},
#'   which adapts to slope attenuation per coefficient and also supports
#'   LLM-coded predictors via `pred_map`.
#'
#' @param formula Two-sided linear model formula `y ~ x1 + x2`.
#' @param data A `data.frame` containing the response, predictors, and LLM
#'   prediction column(s). When `unlabeled = NULL`, rows with missing response
#'   are treated as the unlabeled set.
#' @param pred_col Name of the LLM-coded column approximating the response
#'   variable in `formula`. Optional if `pred_map` is supplied.
#' @param unlabeled Optional separate `data.frame` of unlabeled observations.
#' @param method Estimation method: `"ppi++"` (default), `"ppi"`, or
#'   `"calibrated"`.
#' @param lambda Optional numeric override for the scalar power-tuning
#'   parameter (for `"ppi++"` or `"ppi"`).
#' @param conf_level Confidence level in `(0, 1)`. Default `0.95`.
#' @param pred_map Optional named character vector mapping human-coded variable
#'   names in `formula` to their LLM-coded surrogate column names (e.g.,
#'   `c(distress = "llm_distress")` when an LLM codes a predictor on the RHS).
#'   Automatically uses `method = "calibrated"` when RHS predictors are mapped.
#' @param weights Optional Horvitz-Thompson sampling weights \eqn{w_i > 0} for
#'   stratified/active validation sampling (column name in `data` or numeric
#'   vector). Mutually exclusive with `inclusion_prob`.
#' @param inclusion_prob Optional validation inclusion probabilities
#'   \eqn{\pi_i \in (0, 1]} (column name in `data` or numeric vector, e.g.,
#'   from [select_validation_sample()]). Mutually exclusive with `weights`.
#'
#' @return An S3 object of class `c("llm_ppi_lm", "llm_ppi")`.
#' @export
#'
#' @examples
#' set.seed(123)
#' n_tot <- 800
#' x <- rnorm(n_tot)
#' y_true <- 1.0 + 1.5 * x + rnorm(n_tot, sd = 1.0)
#' y_llm <- 0.6 + 0.9 * y_true + 0.4 * x + rnorm(n_tot, sd = 0.5)
#' df <- tibble::tibble(
#'   x = x,
#'   human_y = c(y_true[1:120], rep(NA_real_, n_tot - 120)),
#'   llm_y = y_llm
#' )
#' fit <- ppi_lm(human_y ~ x, data = df, pred_col = "llm_y")
#' fit
ppi_lm <- function(formula,
                   data,
                   pred_col = NULL,
                   unlabeled = NULL,
                   method = c("ppi++", "ppi", "calibrated"),
                   lambda = NULL,
                   conf_level = 0.95,
                   pred_map = NULL,
                   weights = NULL,
                   inclusion_prob = NULL) {
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    cli::cli_abort("{.arg formula} must be a two-sided formula such as {.code y ~ x1 + x2}.")
  }
  method <- match.arg(method)
  assert_scalar_numeric(conf_level, "conf_level", min = 0.5, max = 0.9999)
  if (!is.null(weights) && !is.null(inclusion_prob)) {
    cli::cli_abort("Specify at most one of {.arg weights} or {.arg inclusion_prob}.")
  }

  truth_col <- all.vars(formula[[2L]])
  if (length(truth_col) != 1L) {
    cli::cli_abort("Left-hand side of {.arg formula} must name a single outcome column.")
  }

  if (!is.null(weights) || !is.null(inclusion_prob)) {
    if (is.null(pred_col)) {
      cli::cli_abort("Supply {.arg pred_col} when using weighted / IPW-PPI in {.fn ppi_lm}.")
    }
    if (method == "calibrated") {
      cli::cli_abort("Weighted IPW-PPI supports {.code method = \"ppi++\"} or {.code method = \"ppi\"}.")
    }
    return(fit_ipw_ppi_lm(
      formula = formula,
      data = data,
      pred_col = pred_col,
      unlabeled = unlabeled,
      method = method,
      lambda = lambda,
      conf_level = conf_level,
      weights = weights,
      inclusion_prob = inclusion_prob
    ))
  }

  # Handle predictor-side or multi-variable pred_map via control-variate calibration
  if (!is.null(pred_map)) {
    if (!is.character(pred_map) || is.null(names(pred_map)) || any(!nzchar(names(pred_map)))) {
      cli::cli_abort("{.arg pred_map} must be a named character vector mapping human variables to LLM surrogate columns.")
    }
    rhs_mapped <- any(setdiff(names(pred_map), truth_col) %in% all.vars(formula[[3L]]))
    if (rhs_mapped) {
      return(fit_ppi_calibrated_general(
        formula = formula,
        data = data,
        unlabeled = unlabeled,
        pred_map = pred_map,
        family = stats::gaussian(),
        conf_level = conf_level
      ))
    }
    if (is.null(pred_col) && truth_col %in% names(pred_map)) {
      pred_col <- unname(pred_map[[truth_col]])
    }
  }

  if (is.null(pred_col)) {
    cli::cli_abort("Supply {.arg pred_col} (the LLM-coded outcome column) or {.arg pred_map}.")
  }

  if (method == "calibrated") {
    return(fit_ppi_calibrated_general(
      formula = formula,
      data = data,
      unlabeled = unlabeled,
      pred_map = stats::setNames(pred_col, truth_col),
      family = stats::gaussian(),
      conf_level = conf_level
    ))
  }

  fit <- ppi_glm(
    formula = formula,
    data = data,
    pred_col = pred_col,
    family = stats::gaussian(),
    unlabeled = unlabeled,
    method = method,
    lambda = lambda,
    conf_level = conf_level
  )
  class(fit) <- c("llm_ppi_lm", "llm_ppi")
  fit
}

#' Prediction-Powered Generalized Linear Model (PPI / PPI++ / Calibrated)
#'
#' Fits an error-corrected canonical-link GLM (`gaussian`, `binomial`, or
#' `poisson`) combining a human-validated subset with a larger LLM-coded
#' unlabeled corpus via Angelopoulos et al. (2024) M-estimation PPI/PPI++ or
#' coordinate-wise control-variate calibration.
#'
#' @param formula Two-sided model formula `y ~ x1 + x2`.
#' @param data A `data.frame` containing the response, predictors, and LLM
#'   prediction column. When `unlabeled = NULL`, rows with missing response
#'   form the unlabeled set.
#' @param pred_col Name of the LLM prediction column (numeric; for binomial
#'   models may be binary `{0, 1}` or predicted probabilities in `[0, 1]`).
#' @param family Canonical GLM family: `gaussian()`, `binomial()`, or
#'   `poisson()` (or matching string name). Default `stats::binomial()`.
#' @param unlabeled Optional separate `data.frame` of unlabeled observations.
#' @param method Estimation method: `"ppi++"` (default, optimal power-tuning
#'   \eqn{\hat{\lambda}^* \in [0, 1]}), `"ppi"` (`lambda = 1`), or
#'   `"calibrated"` (coordinate-wise influence-function calibration).
#' @param lambda Optional numeric override for scalar `lambda` in `[0, 1]`.
#' @param conf_level Confidence level in `(0, 1)`. Default `0.95`.
#' @param max_iter Maximum Newton-Raphson iterations. Default `60L`.
#' @param tol Convergence tolerance for gradient/step norm. Default `1e-9`.
#'
#' @return An S3 object of class `c("llm_ppi_glm", "llm_ppi")`.
#' @export
#'
#' @examples
#' set.seed(7)
#' n_tot <- 800
#' x <- rnorm(n_tot)
#' p_true <- plogis(-0.3 + 1.1 * x)
#' y_true <- rbinom(n_tot, 1, p_true)
#' y_llm <- plogis(-0.3 + 1.1 * x + 0.7 + rnorm(n_tot, sd = 0.6))
#' df <- tibble::tibble(
#'   x = x,
#'   human_y = c(y_true[1:150], rep(NA_real_, n_tot - 150)),
#'   llm_prob = y_llm
#' )
#' fit <- ppi_glm(human_y ~ x, data = df, pred_col = "llm_prob", family = binomial())
#' fit
ppi_glm <- function(formula,
                    data,
                    pred_col,
                    family = stats::binomial(),
                    unlabeled = NULL,
                    method = c("ppi++", "ppi", "calibrated"),
                    lambda = NULL,
                    conf_level = 0.95,
                    max_iter = 60L,
                    tol = 1e-9) {
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    cli::cli_abort("{.arg formula} must be a two-sided formula such as {.code y ~ x1 + x2}.")
  }
  method <- match.arg(method)
  fam <- resolve_ppi_family(family)
  fam_name <- fam$family
  assert_scalar_numeric(conf_level, "conf_level", min = 0.5, max = 0.9999)
  assert_scalar_numeric(max_iter, "max_iter", min = 5)
  assert_scalar_numeric(tol, "tol", min = 1e-14, max = 1e-2)

  truth_col <- all.vars(formula[[2L]])
  if (length(truth_col) != 1L) {
    cli::cli_abort("Left-hand side of {.arg formula} must name a single outcome column.")
  }

  if (method == "calibrated") {
    return(fit_ppi_calibrated_general(
      formula = formula,
      data = data,
      unlabeled = unlabeled,
      pred_map = stats::setNames(pred_col, truth_col),
      family = fam,
      conf_level = conf_level
    ))
  }

  rhs_vars <- all.vars(formula[[3L]])
  sp <- prepare_ppi_data(
    data = data,
    truth_col = truth_col,
    pred_col = pred_col,
    unlabeled = unlabeled,
    extra_cols = rhs_vars
  )

  mats <- build_ppi_model_matrices(formula, sp$labeled, sp$unlabeled)
  X_lab_full <- mats$X_lab
  X_unlab_full <- mats$X_unlab

  rank_info <- check_design_rank(X_lab_full, X_unlab_full)
  if (!isTRUE(rank_info$full_rank)) {
    cli::cli_warn(
      "Rank-deficient design matrix detected; dropping aliased column{?s}: {.val {rank_info$aliased_cols}}."
    )
  }
  X_lab <- X_lab_full[, rank_info$keep_idx, drop = FALSE]
  X_unlab <- X_unlab_full[, rank_info$keep_idx, drop = FALSE]

  y_lab <- as.numeric(sp$labeled[[truth_col]])
  yhat_lab <- as.numeric(sp$labeled[[pred_col]])
  yhat_unlab <- as.numeric(sp$unlabeled[[pred_col]])
  n <- nrow(X_lab)
  N <- nrow(X_unlab)
  p <- ncol(X_lab)
  ops <- canonical_glm_ops(fam_name)

  if (n <= p) {
    cli::cli_abort(
      "Number of labeled observations (n = {n}) must exceed the number of identifiable model parameters (p = {p})."
    )
  }

  df_ref <- Inf
  small_sample <- n < max(20L, 2L * p + 5L)
  if (small_sample) {
    df_ref <- max(n - p, 1L)
    cli::cli_warn(
      "Small labeled validation sample (n = {n}, p = {p}); using t({df_ref}) reference distribution for finite-sample inference."
    )
  }

  # Benchmarks
  bench_hum <- fit_glm_benchmark(X_lab, y_lab, family_name = fam_name)
  X_all <- rbind(X_lab, X_unlab)
  yhat_all <- c(yhat_lab, yhat_unlab)
  bench_naive <- fit_glm_benchmark(X_all, yhat_all, family_name = fam_name)

  # Determine lambda
  if (!is.null(lambda)) {
    assert_scalar_numeric(lambda, "lambda", min = 0, max = 1)
    lam_val <- as.numeric(lambda)
    method_label <- if (abs(lam_val - 1) < 1e-12) "ppi" else paste0("ppi(lambda=", round(lam_val, 2L), ")")
  } else if (method == "ppi") {
    lam_val <- 1
    method_label <- "ppi"
  } else {
    # Optimal PPI++ lambda using pilot estimate (fall back to naive_llm pilot if human_only separated)
    pilot <- if (isTRUE(bench_hum$separation) && !isTRUE(bench_naive$separation)) {
      bench_naive$theta
    } else {
      bench_hum$theta
    }
    mu_L0 <- ops$mu(as.numeric(X_lab %*% pilot))
    mu_U0 <- ops$mu(as.numeric(X_unlab %*% pilot))
    G_L0 <- X_lab * (mu_L0 - y_lab)
    Gt_L0 <- X_lab * (mu_L0 - yhat_lab)
    Gt_U0 <- X_unlab * (mu_U0 - yhat_unlab)
    H_pool <- crossprod(X_all, X_all * ops$var(as.numeric(X_all %*% pilot))) / (n + N)

    lam_val <- compute_optimal_lambda_iid(G_L0, Gt_L0, Gt_U0, H_pool, clip = TRUE)
    method_label <- "ppi++"
  }

  # Solve PPI / PPI++ objective
  start_th <- if (isTRUE(bench_hum$separation)) rep(0, p) else bench_hum$theta
  sol <- solve_ppi_glm_objective(
    X_lab = X_lab,
    y_lab = y_lab,
    yhat_lab = yhat_lab,
    X_unlab = X_unlab,
    yhat_unlab = yhat_unlab,
    family_name = fam_name,
    lambda = lam_val,
    start = start_th,
    max_iter = as.integer(round(max_iter)),
    tol = tol
  )

  sep_flag <- isTRUE(sol$separation)
  if (sep_flag) {
    cli::cli_warn(
      "Quasi-complete separation or near-singular Hessian detected in {.val {fam_name}} GLM; estimates and standard errors may be unreliable."
    )
  }

  # Evaluate scores at final estimate for exact sandwich covariance
  mu_L <- ops$mu(as.numeric(X_lab %*% sol$theta))
  mu_U <- ops$mu(as.numeric(X_unlab %*% sol$theta))
  G_lab <- X_lab * (mu_L - y_lab)
  G_tilde_lab <- X_lab * (mu_L - yhat_lab)
  G_tilde_unlab <- X_unlab * (mu_U - yhat_unlab)

  V_ppi_sub <- compute_ppi_vcov_iid(
    G_lab = G_lab,
    G_tilde_lab = G_tilde_lab,
    G_tilde_unlab = G_tilde_unlab,
    H = sol$hessian,
    lambda = lam_val,
    separation = sep_flag
  )

  exp_ppi <- expand_aliased_results(sol$theta, V_ppi_sub, rank_info)
  exp_hum <- expand_aliased_results(bench_hum$theta, bench_hum$vcov, rank_info)
  exp_naive <- expand_aliased_results(bench_naive$theta, bench_naive$vcov, rank_info)

  tbl_ppi <- tidy_ppi_table(exp_ppi$theta, exp_ppi$vcov, method_label = method_label, conf_level = conf_level, df = df_ref)
  tbl_hum <- tidy_ppi_table(exp_hum$theta, exp_hum$vcov, method_label = "human_only", conf_level = conf_level, df = df_ref)
  tbl_naive <- tidy_ppi_table(exp_naive$theta, exp_naive$vcov, method_label = "naive_llm", conf_level = conf_level, df = Inf)

  new_llm_ppi(
    subclass = "llm_ppi_glm",
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
      family = fam_name,
      pred_col = pred_col,
      converged = sol$converged,
      iterations = sol$iterations,
      separation_detected = sep_flag,
      aliased_terms = rank_info$aliased_cols,
      small_sample = small_sample
    )
  )
}

#' General Coordinate-Wise Control-Variate Calibrated Estimator (supports LLM outcomes AND LLM predictors)
#' @noRd
fit_ppi_calibrated_general <- function(formula, data, unlabeled, pred_map, family, conf_level) {
  fam <- resolve_ppi_family(family)
  fam_name <- fam$family
  df_main <- extract_data_frame(data, arg = "data")

  all_f_vars <- all.vars(formula)
  human_vars <- intersect(names(pred_map), all_f_vars)
  if (length(human_vars) == 0L) {
    cli::cli_abort("None of the variables in {.arg pred_map} appear in {.arg formula}.")
  }
  llm_vars <- unname(pred_map[human_vars])
  clean_vars <- setdiff(all_f_vars, human_vars)

  if (is.null(unlabeled)) {
    assert_columns(df_main, unique(c(all_f_vars, llm_vars)), arg = "data")
    is_lab <- stats::complete.cases(df_main[, human_vars, drop = FALSE])
    lab_df <- df_main[is_lab, , drop = FALSE]
    unlab_df <- df_main[!is_lab, , drop = FALSE]
  } else {
    lab_df <- df_main
    unlab_df <- extract_data_frame(unlabeled, arg = "unlabeled")
    assert_columns(lab_df, unique(c(all_f_vars, llm_vars)), arg = "data")
    assert_columns(unlab_df, unique(c(clean_vars, llm_vars)), arg = "unlabeled")
  }

  lab_keep <- stats::complete.cases(lab_df[, unique(c(all_f_vars, llm_vars)), drop = FALSE])
  unlab_keep <- stats::complete.cases(unlab_df[, unique(c(clean_vars, llm_vars)), drop = FALSE])
  lab_df <- lab_df[lab_keep, , drop = FALSE]
  unlab_df <- unlab_df[unlab_keep, , drop = FALSE]

  n <- nrow(lab_df)
  N <- nrow(unlab_df)
  if (n < 10L || N < 10L) {
    cli::cli_abort("At least 10 complete labeled and 10 complete unlabeled observations are required.")
  }

  # Substitute LLM columns into surrogate data frames with identical variable names as formula
  make_surrogate_df <- function(d) {
    s <- d
    for (hv in human_vars) {
      s[[hv]] <- d[[pred_map[[hv]]]]
    }
    s
  }
  lab_surr <- make_surrogate_df(lab_df)
  unlab_surr <- make_surrogate_df(unlab_df)

  truth_col <- all.vars(formula[[2L]])
  mats_h <- build_ppi_model_matrices(formula, lab_df, lab_surr)
  X_hL_full <- mats_h$X_lab
  mats_s <- build_ppi_model_matrices(formula, lab_surr, unlab_surr)
  X_sL_full <- mats_s$X_lab
  X_sU_full <- mats_s$X_unlab

  rank_info <- check_design_rank(X_hL_full, X_sU_full)
  if (!isTRUE(rank_info$full_rank)) {
    cli::cli_warn(
      "Rank-deficient design matrix detected; dropping aliased column{?s}: {.val {rank_info$aliased_cols}}."
    )
  }
  X_hL <- X_hL_full[, rank_info$keep_idx, drop = FALSE]
  X_sL <- X_sL_full[, rank_info$keep_idx, drop = FALSE]
  X_sU <- X_sU_full[, rank_info$keep_idx, drop = FALSE]

  y_hL <- as.numeric(lab_df[[truth_col]])
  y_sL <- as.numeric(lab_surr[[truth_col]])
  y_sU <- as.numeric(unlab_surr[[truth_col]])
  assert_finite_values(y_hL, truth_col)
  assert_finite_values(y_sL, "LLM surrogate outcome (labeled)")
  assert_finite_values(y_sU, "LLM surrogate outcome (unlabeled)")

  fit_hL <- fit_glm_benchmark(X_hL, y_hL, family_name = fam_name)
  fit_sL <- fit_glm_benchmark(X_sL, y_sL, family_name = fam_name)
  fit_sU <- fit_glm_benchmark(X_sU, y_sU, family_name = fam_name)
  fit_naive <- fit_glm_benchmark(rbind(X_sL, X_sU), c(y_sL, y_sU), family_name = fam_name)

  # Observation-level influence functions: psi_i = -H^{-1} g_i
  psi_hL <- -(fit_hL$scores %*% t(safe_solve(fit_hL$hessian)))
  psi_sL <- -(fit_sL$scores %*% t(safe_solve(fit_sL$hessian)))
  psi_sU <- -(fit_sU$scores %*% t(safe_solve(fit_sU$hessian)))

  p <- ncol(X_hL)
  lam_vec <- numeric(p)
  names(lam_vec) <- colnames(X_hL)
  for (k in seq_len(p)) {
    cov_k <- stats::cov(psi_hL[, k], psi_sL[, k])
    var_k <- stats::var(psi_sL[, k]) + (n / N) * stats::var(psi_sU[, k])
    lam_vec[k] <- if (is.finite(var_k) && var_k > 1e-12 && is.finite(cov_k)) cov_k / var_k else 0
  }

  Lambda <- diag(lam_vec, nrow = p, ncol = p)
  theta_cal_sub <- as.numeric(fit_hL$theta + Lambda %*% (fit_sU$theta - fit_sL$theta))
  names(theta_cal_sub) <- colnames(X_hL)

  df_adj <- if (n > p) (n - 1) / (n - p) else 1
  diff_psi_L <- psi_hL - psi_sL %*% Lambda
  V_cal_sub <- (df_adj / n) * safe_cov(diff_psi_L) + (1 / N) * (Lambda %*% safe_cov(psi_sU) %*% Lambda)
  V_cal_sub <- 0.5 * (V_cal_sub + t(V_cal_sub))
  dimnames(V_cal_sub) <- list(colnames(X_hL), colnames(X_hL))

  exp_cal <- expand_aliased_results(theta_cal_sub, V_cal_sub, rank_info)
  exp_hum <- expand_aliased_results(fit_hL$theta, fit_hL$vcov, rank_info)
  exp_naive <- expand_aliased_results(fit_naive$theta, fit_naive$vcov, rank_info)

  tbl_cal <- tidy_ppi_table(exp_cal$theta, exp_cal$vcov, method_label = "ppi_calibrated", conf_level = conf_level)
  tbl_hum <- tidy_ppi_table(exp_hum$theta, exp_hum$vcov, method_label = "human_only", conf_level = conf_level)
  tbl_naive <- tidy_ppi_table(exp_naive$theta, exp_naive$vcov, method_label = "naive_llm", conf_level = conf_level)

  subclass <- if (fam_name == "gaussian") "llm_ppi_lm" else "llm_ppi_glm"
  new_llm_ppi(
    subclass = subclass,
    estimates = tbl_cal,
    benchmarks = rbind(tbl_cal, tbl_hum, tbl_naive),
    theta = exp_cal$theta,
    vcov_mat = exp_cal$vcov,
    lambda = lam_vec,
    method_label = "ppi_calibrated",
    n_labeled = n,
    n_unlabeled = N,
    conf_level = conf_level,
    extra = list(
      formula = formula,
      family = fam_name,
      pred_map = pred_map,
      aliased_terms = rank_info$aliased_cols
    )
  )
}

#' @export
coef.llm_ppi <- function(object, ...) {
  object$coefficients
}

#' @export
vcov.llm_ppi <- function(object, ...) {
  object$vcov
}

#' @export
confint.llm_ppi <- function(object, parm, level = object$conf_level, ...) {
  cf <- object$coefficients
  se <- sqrt(pmax(diag(as.matrix(object$vcov)), 0))
  alpha <- 1 - level
  crit <- if (is.finite(object$df) && object$df > 0) {
    stats::qt(1 - alpha / 2, df = object$df)
  } else {
    stats::qnorm(1 - alpha / 2)
  }
  ci <- cbind(cf - crit * se, cf + crit * se)
  pct <- paste0(format(100 * c(alpha / 2, 1 - alpha / 2), trim = TRUE, digits = 3L), " %")
  colnames(ci) <- pct
  rownames(ci) <- names(cf)
  if (!missing(parm)) {
    ci <- ci[parm, , drop = FALSE]
  }
  ci
}

#' @export
print.llm_ppi <- function(x, ...) {
  lam_str <- if (length(x$lambda) == 1L) {
    fmt_num(x$lambda, 3L)
  } else {
    paste0("[", paste(fmt_num(x$lambda, 2L), collapse = ", "), "]")
  }
  cli::cli_h3("Prediction-Powered Inference ({x$method})")
  cli::cli_text(
    "Labeled n = {x$n_labeled} | Unlabeled N = {x$n_unlabeled} | lambda = {lam_str} | CI = {round(100 * x$conf_level)}%"
  )
  if (!is.null(x$cluster_structure)) {
    cli::cli_text(
      "Clusters ({x$cluster}): {x$n_clusters_labeled} labeled, {x$n_clusters_unlabeled} unlabeled ({x$n_clusters_total} total; {x$cluster_structure})"
    )
  }
  if (!is.null(x$variance_components)) {
    vc_h <- x$variance_components[x$variance_components$source == "human_only", , drop = FALSE]
    cli::cli_text(
      "Random-intercept REML (human): tau2 = {fmt_num(vc_h$tau2_cluster)}, sigma2 = {fmt_num(vc_h$sigma2_resid)}, ICC = {fmt_num(vc_h$icc)} | Weighting: {x$weight_mode}"
    )
  }
  if (!is.null(x$calibration_diagnostics)) {
    cd <- x$calibration_diagnostics
    cli::cli_text(
      "Cross-fitted calibration ({cd$calibrator}, {cd$folds}-fold, df={cd$df}): r {fmt_num(cd$r_raw)} -> {fmt_num(cd$r_calibrated_oof)} | RMSE {fmt_num(cd$rmse_raw)} -> {fmt_num(cd$rmse_calibrated_oof)}"
    )
  }
  if (isTRUE(x$weighted)) {
    cli::cli_text("IPW weighted active inference | Kish effective labeled n = {fmt_num(x$effective_n_weights, 1L)}")
  }
  disp <- tibble::tibble(
    term = x$estimates$term,
    estimate = round(x$estimates$estimate, 4L),
    std_error = round(x$estimates$std_error, 4L),
    conf_low = round(x$estimates$conf_low, 4L),
    conf_high = round(x$estimates$conf_high, 4L),
    p_value = fmt_pval(x$estimates$p_value),
    var_gain = round(x$efficiency$var_ratio_vs_human, 2L)
  )
  print(disp, n = nrow(disp))
  invisible(x)
}

#' @export
summary.llm_ppi <- function(object, ...) {
  print(object, ...)
  cli::cli_text("Benchmarks (PPI vs Human-Only vs Naive LLM):")
  b <- object$benchmarks
  b_disp <- tibble::tibble(
    method = b$method,
    term = b$term,
    estimate = round(b$estimate, 4L),
    std_error = round(b$std_error, 4L),
    conf_low = round(b$conf_low, 4L),
    conf_high = round(b$conf_high, 4L),
    p_value = fmt_pval(b$p_value)
  )
  print(b_disp, n = nrow(b_disp))
  invisible(object)
}
