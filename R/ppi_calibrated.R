#' Cross-Fitted Non-Linear Calibration for Prediction-Powered Inference
#'
#' Handles non-linear LLM scoring distortions (ceiling/floor compression,
#' S-curve saturation, quadratic distortion, or covariate-specific non-linear
#' bias) by learning a flexible calibration mapping
#' \eqn{\hat{m}(\hat{Y}, X) \approx Y} via strict \eqn{K}-fold cross-fitting on
#' the human-validated subset before applying Prediction-Powered Inference
#' (`PPI++`). Because every labeled observation's calibrated score is computed
#' strictly out-of-fold and unlabeled observations average the \eqn{K}
#' fold-specific calibrators with exact fold-size weights, the canonical-link
#' PPI++ rectifier preserves exact unbiasedness and nominal 95% CI coverage
#' while dramatically reducing standard errors relative to both Human-Only and
#' uncalibrated linear PPI++.
#'
#' @param formula Two-sided model formula (e.g., `human_y ~ x1 + x2` or
#'   `human_y ~ 1` for calibrated mean estimation).
#' @param data A `data.frame` containing response, predictors, and `pred_col`.
#'   When `unlabeled = NULL`, rows with missing response form the unlabeled set.
#' @param pred_col Name of the numeric LLM prediction column to calibrate.
#' @param family Canonical GLM family: `gaussian()` (default), `binomial()`, or
#'   `poisson()`.
#' @param unlabeled Optional separate `data.frame` of unlabeled observations.
#' @param calibrator Non-linear calibration learner:
#'   - `"spline"` (default): natural cubic spline (`splines::ns()`) on
#'     `pred_col` plus optional covariates from `formula`.
#'   - `"poly"`: polynomial basis of degree `df` on `pred_col` plus covariates.
#'   - `"logistic"`: binomial GLM with natural cubic spline on `pred_col` plus
#'     covariates (ideal for binary outcomes).
#'   - Or a custom function `function(train_df, new_df, truth_col, pred_col, rhs_vars)`
#'     returning numeric calibrated predictions for `new_df`.
#' @param df Positive integer basis degrees of freedom (for `"spline"` /
#'   `"logistic"`) or polynomial degree (for `"poly"`). Default `4L`.
#' @param folds Integer number of cross-fitting folds (`>= 2L`). Default `5L`.
#' @param include_covariates Logical; if `TRUE` (default), includes main effects
#'   of RHS predictors from `formula` inside the calibration model so
#'   covariate-specific bias (DPF) is removed before PPI++.
#' @param lambda Power-tuning parameter passed to PPI++: `"auto"` (default) or
#'   numeric scalar in `[0, 1]`.
#' @param conf_level Confidence level in `(0, 1)`. Default `0.95`.
#' @param seed Integer random seed for deterministic fold assignment (preserves
#'   global `.Random.seed`). Default `1234L`.
#'
#' @return An S3 object of class `c("llm_ppi_calibrated", "llm_ppi")` containing
#'   all standard `llm_ppi` components (with `$benchmarks` comparing
#'   `ppi++_calibrated`, `ppi++_uncalibrated`, `human_only`, and `naive_llm`)
#'   plus `$calibration_diagnostics` (`r_raw`, `r_calibrated_oof`, `rmse_raw`,
#'   `rmse_calibrated_oof`, `folds`, `calibrator`).
#' @export
#'
#' @examples
#' set.seed(42)
#' n_tot <- 800
#' x <- rnorm(n_tot)
#' y_true <- 1.0 + 1.2 * x + rnorm(n_tot, sd = 0.8)
#' # Severely non-linear S-curve compressed LLM scores
#' y_llm <- 5 * plogis(1.4 * (y_true - 1.0) + 0.5 * x) + rnorm(n_tot, sd = 0.15)
#' df <- tibble::tibble(
#'   x = x,
#'   human_y = c(y_true[1:140], rep(NA_real_, n_tot - 140)),
#'   llm_y = y_llm
#' )
#' fit <- ppi_calibrated(human_y ~ x, data = df, pred_col = "llm_y")
#' fit
ppi_calibrated <- function(formula,
                           data,
                           pred_col,
                           family = stats::gaussian(),
                           unlabeled = NULL,
                           calibrator = c("spline", "poly", "logistic"),
                           df = 4L,
                           folds = 5L,
                           include_covariates = TRUE,
                           lambda = "auto",
                           conf_level = 0.95,
                           seed = 1234L) {
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    cli::cli_abort("{.arg formula} must be a two-sided formula such as {.code y ~ x1 + x2}.")
  }
  fam <- resolve_ppi_family(family)
  fam_name <- fam$family

  custom_cal_fn <- NULL
  if (is.function(calibrator)) {
    custom_cal_fn <- calibrator
    cal_name <- "custom"
  } else {
    cal_name <- match.arg(calibrator)
  }

  assert_scalar_numeric(df, "df", min = 1, max = 12)
  df <- as.integer(round(df))
  assert_scalar_numeric(folds, "folds", min = 2, max = 20)
  folds <- as.integer(round(folds))
  assert_flag(include_covariates, "include_covariates")
  assert_scalar_numeric(conf_level, "conf_level", min = 0.5, max = 0.9999)
  assert_scalar_numeric(seed, "seed")

  truth_col <- all.vars(formula[[2L]])
  if (length(truth_col) != 1L) {
    cli::cli_abort("Left-hand side of {.arg formula} must name a single outcome column.")
  }
  rhs_vars <- all.vars(formula[[3L]])

  sp <- prepare_ppi_data(
    data = data,
    truth_col = truth_col,
    pred_col = pred_col,
    unlabeled = unlabeled,
    extra_cols = rhs_vars
  )
  lab_df <- sp$labeled
  unlab_df <- sp$unlabeled
  n <- nrow(lab_df)
  N <- nrow(unlab_df)

  y_lab_chk <- as.numeric(lab_df[[truth_col]])
  validate_glm_domain(
    y_lab = y_lab_chk,
    yhat_lab = as.numeric(lab_df[[pred_col]]),
    yhat_unlab = as.numeric(unlab_df[[pred_col]]),
    fam_name = fam_name,
    check_pred = FALSE
  )
  if (identical(cal_name, "logistic") && (any(y_lab_chk < 0 | y_lab_chk > 1))) {
    cli::cli_abort(
      "{.code calibrator = \"logistic\"} requires outcomes in [0, 1] (typically with {.code family = binomial()})."
    )
  } else if (identical(cal_name, "logistic") && fam_name != "binomial") {
    cli::cli_warn(
      "{.code calibrator = \"logistic\"} is intended for binary outcomes; proceeding with binomial calibration learner."
    )
  }

  K <- min(folds, max(2L, as.integer(floor(n / 5L))))
  if (n < 10L) {
    cli::cli_abort("At least 10 complete labeled observations are required for cross-fitted {.fn ppi_calibrated}.")
  }

  # Deterministic balanced fold assignment without mutating .Random.seed
  fold_id <- with_local_seed(as.integer(seed), {
    sample(rep(seq_len(K), length.out = n))
  })

  yhat_oof_lab <- numeric(n)
  yhat_cf_unlab <- numeric(N)

  # Harmonize categorical covariates once across lab & unlab for safe out-of-fold prediction
  cov_vars <- if (isTRUE(include_covariates)) rhs_vars else character()
  lab_work <- lab_df
  unlab_work <- unlab_df
  for (cv in cov_vars) {
    if (is.factor(lab_work[[cv]]) || is.character(lab_work[[cv]]) ||
        is.factor(unlab_work[[cv]]) || is.character(unlab_work[[cv]])) {
      lv <- sort(unique(c(as.character(lab_work[[cv]]), as.character(unlab_work[[cv]]))))
      lab_work[[cv]] <- factor(as.character(lab_work[[cv]]), levels = lv)
      unlab_work[[cv]] <- factor(as.character(unlab_work[[cv]]), levels = lv)
    }
  }

  for (k in seq_len(K)) {
    val_idx <- which(fold_id == k)
    trn_idx <- which(fold_id != k)
    trn_df <- lab_work[trn_idx, , drop = FALSE]
    val_df <- lab_work[val_idx, , drop = FALSE]

    if (!is.null(custom_cal_fn)) {
      pred_val <- as.numeric(custom_cal_fn(trn_df, val_df, truth_col, pred_col, cov_vars))
      pred_u <- as.numeric(custom_cal_fn(trn_df, unlab_work, truth_col, pred_col, cov_vars))
    } else {
      preds <- fit_predict_nonlinear_calibrator(
        trn_df = trn_df,
        val_df = val_df,
        unlab_df = unlab_work,
        truth_col = truth_col,
        pred_col = pred_col,
        cov_vars = cov_vars,
        cal_name = cal_name,
        df_basis = df,
        fam_name = fam_name
      )
      pred_val <- preds$val
      pred_u <- preds$unlab
    }

    if (length(pred_val) != length(val_idx) || any(!is.finite(pred_val))) {
      cli::cli_abort("Cross-fitted calibrator returned invalid/non-finite validation predictions in fold {k}.")
    }
    if (length(pred_u) != N || any(!is.finite(pred_u))) {
      cli::cli_abort("Cross-fitted calibrator returned invalid/non-finite unlabeled predictions in fold {k}.")
    }

    yhat_oof_lab[val_idx] <- pred_val
    yhat_cf_unlab <- yhat_cf_unlab + (length(val_idx) / n) * pred_u
  }

  validate_glm_domain(
    y_lab = y_lab_chk,
    yhat_lab = yhat_oof_lab,
    yhat_unlab = yhat_cf_unlab,
    fam_name = fam_name,
    check_pred = TRUE
  )

  # Fit baseline uncalibrated PPI++ for direct benchmarking
  # (If raw pred_col lies outside canonical GLM bounds, e.g. raw logits for binomial, clamp only for benchmark)
  lab_uncal_df <- lab_df
  unlab_uncal_df <- unlab_df
  if (fam_name == "binomial") {
    raw_l <- as.numeric(lab_uncal_df[[pred_col]])
    raw_u <- as.numeric(unlab_uncal_df[[pred_col]])
    if (any(raw_l < 0 | raw_l > 1) || any(raw_u < 0 | raw_u > 1)) {
      lab_uncal_df[[pred_col]] <- pmin(pmax(raw_l, 0), 1)
      unlab_uncal_df[[pred_col]] <- pmin(pmax(raw_u, 0), 1)
    }
  } else if (fam_name == "poisson") {
    raw_l <- as.numeric(lab_uncal_df[[pred_col]])
    raw_u <- as.numeric(unlab_uncal_df[[pred_col]])
    if (any(raw_l < 0) || any(raw_u < 0)) {
      lab_uncal_df[[pred_col]] <- pmax(raw_l, 0)
      unlab_uncal_df[[pred_col]] <- pmax(raw_u, 0)
    }
  }
  uncal_fit <- ppi_glm(
    formula = formula,
    data = lab_uncal_df,
    pred_col = pred_col,
    family = fam,
    unlabeled = unlab_uncal_df,
    method = if (identical(lambda, "auto")) "ppi++" else "ppi",
    lambda = if (identical(lambda, "auto")) NULL else lambda,
    conf_level = conf_level
  )

  # Fit cross-fitted calibrated PPI++ using exact out-of-fold calibrated predictions
  cal_col <- ".llmmetric_cf_pred"
  lab_cf <- lab_df
  unlab_cf <- unlab_df
  lab_cf[[cal_col]] <- yhat_oof_lab
  unlab_cf[[cal_col]] <- yhat_cf_unlab

  cf_fit <- ppi_glm(
    formula = formula,
    data = lab_cf,
    pred_col = cal_col,
    family = fam,
    unlabeled = unlab_cf,
    method = if (identical(lambda, "auto")) "ppi++" else "ppi",
    lambda = if (identical(lambda, "auto")) NULL else lambda,
    conf_level = conf_level
  )

  # Relabel primary estimates & build 4-way benchmark table
  method_lbl <- "ppi++_calibrated"
  est_tbl <- cf_fit$estimates
  est_tbl$method <- method_lbl

  uncal_est <- uncal_fit$estimates
  uncal_est$method <- "ppi++_uncalibrated"
  hum_est <- uncal_fit$benchmarks[uncal_fit$benchmarks$method == "human_only", , drop = FALSE]
  naive_est <- uncal_fit$benchmarks[uncal_fit$benchmarks$method == "naive_llm", , drop = FALSE]

  y_true_lab <- as.numeric(lab_df[[truth_col]])
  y_raw_lab <- as.numeric(lab_df[[pred_col]])
  r_raw <- if (!is_constant_numeric(y_true_lab) && !is_constant_numeric(y_raw_lab)) {
    stats::cor(y_true_lab, y_raw_lab)
  } else {
    NA_real_
  }
  r_cal <- if (!is_constant_numeric(y_true_lab) && !is_constant_numeric(yhat_oof_lab)) {
    stats::cor(y_true_lab, yhat_oof_lab)
  } else {
    NA_real_
  }
  rmse_raw <- sqrt(mean((y_raw_lab - y_true_lab)^2))
  rmse_cal <- sqrt(mean((yhat_oof_lab - y_true_lab)^2))

  cal_diag <- tibble::tibble(
    calibrator = cal_name,
    folds = K,
    df = df,
    r_raw = r_raw,
    r_calibrated_oof = r_cal,
    rmse_raw = rmse_raw,
    rmse_calibrated_oof = rmse_cal
  )

  new_llm_ppi(
    subclass = "llm_ppi_calibrated",
    estimates = est_tbl,
    benchmarks = rbind(est_tbl, uncal_est, hum_est, naive_est),
    theta = cf_fit$coefficients,
    vcov_mat = cf_fit$vcov,
    lambda = cf_fit$lambda,
    method_label = method_lbl,
    n_labeled = n,
    n_unlabeled = N,
    conf_level = conf_level,
    df = cf_fit$df,
    extra = list(
      formula = formula,
      family = fam_name,
      pred_col = pred_col,
      converged = cf_fit$converged,
      separation_detected = isTRUE(cf_fit$separation_detected),
      aliased_terms = cf_fit$aliased_terms,
      calibration_diagnostics = cal_diag,
      oof_predictions = yhat_oof_lab,
      unlabeled_calibrated = yhat_cf_unlab
    )
  )
}

#' Fit a single-fold non-linear calibration learner with safe boundary clamping
#' @noRd
fit_predict_nonlinear_calibrator <- function(trn_df, val_df, unlab_df,
                                             truth_col, pred_col, cov_vars,
                                             cal_name, df_basis, fam_name) {
  y_trn <- as.numeric(trn_df[[truth_col]])
  p_trn <- as.numeric(trn_df[[pred_col]])
  p_val <- as.numeric(val_df[[pred_col]])
  p_unlab <- as.numeric(unlab_df[[pred_col]])

  # Fallback to constant mean if predictor is constant in training fold
  if (is_constant_numeric(p_trn)) {
    m0 <- mean(y_trn)
    return(list(val = rep(m0, length(p_val)), unlab = rep(m0, length(p_unlab))))
  }

  # Clamp out-of-sample predictions softly to training range to prevent wild spline/poly extrapolation
  rng <- range(p_trn)
  p_val_c <- pmin(pmax(p_val, rng[1L]), rng[2L])
  p_unlab_c <- pmin(pmax(p_unlab, rng[1L]), rng[2L])

  n_uniq <- length(unique(p_trn))
  eff_df <- min(df_basis, max(1L, n_uniq - 2L), max(1L, floor(nrow(trn_df) / 6L)))

  if (cal_name %in% c("spline", "logistic") && eff_df >= 2L) {
    # Construct explicit natural spline basis on training fold and reuse predict() cleanly
    ns_obj <- suppressWarnings(splines::ns(p_trn, df = eff_df))
    B_trn <- as.matrix(ns_obj)
    B_val <- as.matrix(stats::predict(ns_obj, newx = p_val_c))
    B_unlab <- as.matrix(stats::predict(ns_obj, newx = p_unlab_c))
  } else if (cal_name == "poly" && eff_df >= 2L) {
    p_mu <- mean(p_trn)
    p_sd <- max(stats::sd(p_trn), 1e-8)
    z_trn <- (p_trn - p_mu) / p_sd
    z_val <- (p_val_c - p_mu) / p_sd
    z_unlab <- (p_unlab_c - p_mu) / p_sd
    B_trn <- sapply(seq_len(eff_df), function(d) z_trn^d)
    B_val <- sapply(seq_len(eff_df), function(d) z_val^d)
    B_unlab <- sapply(seq_len(eff_df), function(d) z_unlab^d)
  } else {
    B_trn <- matrix(p_trn, ncol = 1L)
    B_val <- matrix(p_val_c, ncol = 1L)
    B_unlab <- matrix(p_unlab_c, ncol = 1L)
  }

  colnames(B_trn) <- colnames(B_val) <- colnames(B_unlab) <- paste0(".llmmetric_cal_b_", seq_len(ncol(B_trn)))
  df_trn_m <- cbind(data.frame(.llmmetric_cal_y = y_trn), as.data.frame(B_trn))
  df_val_m <- as.data.frame(B_val)
  df_unlab_m <- as.data.frame(B_unlab)

  # Decide active covariates STRICTLY from trn_df alone (zero leakage from val_df or unlab_df)
  # Use collision-proof internal column names so user covariates named `y`, `b_1`, or non-syntactic
  # names like `` `x 1` `` never overwrite outcome/basis columns or break formula parsing.
  cov_idx <- 0L
  for (cv in cov_vars) {
    col_t <- trn_df[[cv]]
    if (is.factor(col_t) || is.character(col_t)) {
      t_chr <- as.character(col_t)
      lv_t <- sort(unique(t_chr))
      if (length(lv_t) >= 2L) {
        cov_idx <- cov_idx + 1L
        cname <- paste0(".llmmetric_cal_cov_", cov_idx)
        tab_t <- tabulate(match(t_chr, lv_t), nbins = length(lv_t))
        ref_lv <- lv_t[which.max(tab_t)]

        v_chr <- as.character(val_df[[cv]])
        v_chr[!(v_chr %in% lv_t)] <- ref_lv
        u_chr <- as.character(unlab_df[[cv]])
        u_chr[!(u_chr %in% lv_t)] <- ref_lv

        df_trn_m[[cname]] <- factor(t_chr, levels = lv_t)
        df_val_m[[cname]] <- factor(v_chr, levels = lv_t)
        df_unlab_m[[cname]] <- factor(u_chr, levels = lv_t)
      }
    } else if (is.numeric(col_t) && !is_constant_numeric(col_t)) {
      cov_idx <- cov_idx + 1L
      cname <- paste0(".llmmetric_cal_cov_", cov_idx)
      df_trn_m[[cname]] <- as.numeric(col_t)
      df_val_m[[cname]] <- as.numeric(val_df[[cv]])
      df_unlab_m[[cname]] <- as.numeric(unlab_df[[cv]])
    }
  }

  rhs_names <- setdiff(names(df_trn_m), ".llmmetric_cal_y")
  fit_fml <- stats::as.formula(paste(".llmmetric_cal_y ~", paste(rhs_names, collapse = " + ")))
  use_binom <- (cal_name == "logistic") || (fam_name == "binomial" && all(y_trn >= 0 & y_trn <= 1))

  if (use_binom) {
    fit <- suppressWarnings(stats::glm(fit_fml, data = df_trn_m, family = stats::binomial()))
    pr_val <- suppressWarnings(as.numeric(stats::predict(fit, newdata = df_val_m, type = "response")))
    pr_unlab <- suppressWarnings(as.numeric(stats::predict(fit, newdata = df_unlab_m, type = "response")))
  } else {
    fit <- stats::lm(fit_fml, data = df_trn_m)
    pr_val <- suppressWarnings(as.numeric(stats::predict(fit, newdata = df_val_m)))
    pr_unlab <- suppressWarnings(as.numeric(stats::predict(fit, newdata = df_unlab_m)))
  }

  # Guard against rare NA predictions from rank-deficient fold covariates
  pr_val[!is.finite(pr_val)] <- mean(y_trn)
  pr_unlab[!is.finite(pr_unlab)] <- mean(y_trn)
  if (fam_name == "binomial") {
    pr_val <- pmin(pmax(pr_val, 0), 1)
    pr_unlab <- pmin(pmax(pr_unlab, 0), 1)
  } else if (fam_name == "poisson") {
    pr_val <- pmax(pr_val, 0)
    pr_unlab <- pmax(pr_unlab, 0)
  }

  list(val = pr_val, unlab = pr_unlab)
}
