#' Cluster-Robust Prediction-Powered Inference (PPI / PPI++)
#'
#' Extends Prediction-Powered Inference (`gaussian`, `binomial`, and `poisson`
#' canonical-link GLMs) to clustered research designs (e.g., repeated EMA
#' diary entries within participants, therapy turns within patients, or
#' students nested in classrooms). Automatically handles both **disjoint
#' cluster validation** (distinct clusters in labeled vs. unlabeled sets) and
#' **within-cluster validation** (shared clusters containing both labeled and
#' unlabeled observations), computing exact cluster-optimal power-tuning
#' \eqn{\hat{\lambda}^*_{\text{cluster}}} and cluster-robust sandwich standard
#' errors.
#'
#' @param formula Two-sided regression formula `y ~ x1 + x2`.
#' @param data A `data.frame` containing response, covariates, `pred_col`, and
#'   `cluster`. When `unlabeled = NULL`, rows with missing response form the
#'   unlabeled set.
#' @param pred_col Name of the LLM-coded outcome column.
#' @param cluster Name of the cluster identifier column.
#' @param family Canonical GLM family: `gaussian()` (default), `binomial()`, or
#'   `poisson()`.
#' @param unlabeled Optional separate `data.frame` of unlabeled observations.
#' @param method `"ppi++"` (default, cluster-optimal power-tuning) or `"ppi"`
#'   (`lambda = 1`).
#' @param lambda Optional numeric override for `lambda` in `[0, 1]`.
#' @param conf_level Confidence level in `(0, 1)`. Default `0.95`.
#' @param small_sample_df Logical; if `TRUE` (default), uses a \eqn{t}
#'   reference distribution with \eqn{G_{\text{eff}} - 1} degrees of freedom
#'   for small-cluster inference.
#' @param max_iter Maximum Newton-Raphson iterations for non-Gaussian families.
#' @param tol Convergence tolerance.
#'
#' @return An S3 object of class `c("llm_ppi_clustered", "llm_ppi")`.
#' @export
#'
#' @examples
#' set.seed(99)
#' G <- 60
#' m <- 6
#' cid <- rep(paste0("c", seq_len(G)), each = m)
#' u <- rep(rnorm(G, sd = 0.8), each = m)
#' x <- rnorm(G * m)
#' y_true <- 1.0 + 1.2 * x + u + rnorm(G * m, sd = 0.7)
#' y_llm <- 0.5 + 0.85 * y_true + 0.35 * x + rnorm(G * m, sd = 0.4)
#' is_val <- cid %in% paste0("c", 1:25)
#' df <- tibble::tibble(
#'   cluster = cid,
#'   x = x,
#'   human_y = ifelse(is_val, y_true, NA_real_),
#'   llm_y = y_llm
#' )
#' fit <- ppi_clustered(human_y ~ x, data = df, pred_col = "llm_y", cluster = "cluster")
#' fit
ppi_clustered <- function(formula,
                          data,
                          pred_col,
                          cluster,
                          family = stats::gaussian(),
                          unlabeled = NULL,
                          method = c("ppi++", "ppi"),
                          lambda = NULL,
                          conf_level = 0.95,
                          small_sample_df = TRUE,
                          max_iter = 60L,
                          tol = 1e-9) {
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    cli::cli_abort("{.arg formula} must be a two-sided formula such as {.code y ~ x1 + x2}.")
  }
  method <- match.arg(method)
  fam <- resolve_ppi_family(family)
  fam_name <- fam$family
  assert_scalar_character(cluster, "cluster")
  assert_scalar_numeric(conf_level, "conf_level", min = 0.5, max = 0.9999)
  assert_flag(small_sample_df, "small_sample_df")

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
    extra_cols = unique(c(rhs_vars, cluster))
  )

  mats <- build_ppi_model_matrices(formula, sp$labeled, sp$unlabeled)
  X_lab <- mats$X_lab
  X_unlab <- mats$X_unlab
  y_lab <- as.numeric(sp$labeled[[truth_col]])
  yhat_lab <- as.numeric(sp$labeled[[pred_col]])
  yhat_unlab <- as.numeric(sp$unlabeled[[pred_col]])
  cl_lab <- as.character(sp$labeled[[cluster]])
  cl_unlab <- as.character(sp$unlabeled[[cluster]])

  res <- fit_ppi_clustered_engine(
    X_lab = X_lab,
    y_lab = y_lab,
    yhat_lab = yhat_lab,
    cl_lab = cl_lab,
    X_unlab = X_unlab,
    yhat_unlab = yhat_unlab,
    cl_unlab = cl_unlab,
    family_name = fam_name,
    method = method,
    lambda = lambda,
    conf_level = conf_level,
    small_sample_df = small_sample_df,
    max_iter = as.integer(round(max_iter)),
    tol = tol
  )

  new_llm_ppi(
    subclass = "llm_ppi_clustered",
    estimates = res$tbl_ppi,
    benchmarks = rbind(res$tbl_ppi, res$tbl_hum, res$tbl_naive),
    theta = res$theta,
    vcov_mat = res$vcov,
    lambda = res$lambda,
    method_label = res$method_label,
    n_labeled = nrow(X_lab),
    n_unlabeled = nrow(X_unlab),
    conf_level = conf_level,
    df = res$df,
    extra = list(
      formula = formula,
      family = fam_name,
      cluster = cluster,
      pred_col = pred_col,
      cluster_structure = res$cluster_structure,
      n_clusters_labeled = res$G_lab,
      n_clusters_unlabeled = res$G_unlab,
      n_clusters_total = res$G_total
    )
  )
}

#' Multilevel Random-Intercept Prediction-Powered Inference (`ppi_lmer`)
#'
#' Combines a multilevel random-intercept linear mixed model (`lme4::lmer`)
#' with Prediction-Powered Inference (PPI / PPI++). Accepts either an
#' `lme4`-style random-intercept formula (`y ~ x1 + x2 + (1 | cluster)`) or a
#' fixed-effects formula (`y ~ x1 + x2`) plus `cluster`. Estimates
#' variance components (\eqn{\tau^2}, \eqn{\sigma^2}, ICC) via REML on the
#' human-validated set, forms exact Woodbury random-intercept GLS weights (when
#' clusters are disjoint across labeled and unlabeled sets, or total-cluster
#' inverse-variance weights when clusters overlap, preserving exact
#' unbiasedness under arbitrary LLM bias), and computes cluster-robust PPI++
#' sandwich standard errors alongside human-only and naive-LLM multilevel
#' benchmarks.
#'
#' @param formula Either a random-intercept formula `y ~ x1 + x2 + (1 | grp)`
#'   or a fixed-effects formula `y ~ x1 + x2` paired with `cluster`.
#' @param data A `data.frame` containing response, covariates, `pred_col`, and
#'   cluster IDs. When `unlabeled = NULL`, rows with missing response form the
#'   unlabeled set.
#' @param pred_col Name of the LLM-coded outcome column.
#' @param cluster Optional cluster column name (inferred from `(1 | cluster)`
#'   in `formula` if omitted).
#' @param unlabeled Optional separate `data.frame` of unlabeled observations.
#' @param method `"ppi++"` (default) or `"ppi"` (`lambda = 1`).
#' @param lambda Optional numeric override for `lambda` in `[0, 1]`.
#' @param conf_level Confidence level in `(0, 1)`. Default `0.95`.
#' @param weight_mode `"gls"` (default, REML random-intercept GLS weighting) or
#'   `"ols"` (unweighted cluster-robust PPI++).
#' @param small_sample_df Logical; use \eqn{t} degrees of freedom based on
#'   effective cluster count. Default `TRUE`.
#'
#' @return An S3 object of class `c("llm_ppi_lmer", "llm_ppi_clustered", "llm_ppi")`
#'   with `$variance_components` (human vs. naive-LLM \eqn{\tau^2}, \eqn{\sigma^2},
#'   ICC) in addition to standard `llm_ppi` components.
#' @export
#'
#' @examples
#' set.seed(11)
#' G <- 50
#' m <- 5
#' cid <- rep(paste0("p", seq_len(G)), each = m)
#' u <- rep(rnorm(G, sd = 0.9), each = m)
#' x <- rnorm(G * m)
#' y_true <- 2.0 + 1.0 * x + u + rnorm(G * m, sd = 0.6)
#' y_llm <- 0.4 + 0.8 * y_true + rnorm(G * m, sd = 0.5)
#' df <- tibble::tibble(
#'   person = cid,
#'   x = x,
#'   human_y = ifelse(cid %in% paste0("p", 1:20), y_true, NA_real_),
#'   llm_y = y_llm
#' )
#' fit <- ppi_lmer(human_y ~ x + (1 | person), data = df, pred_col = "llm_y")
#' fit
ppi_lmer <- function(formula,
                     data,
                     pred_col,
                     cluster = NULL,
                     unlabeled = NULL,
                     method = c("ppi++", "ppi"),
                     lambda = NULL,
                     conf_level = 0.95,
                     weight_mode = c("gls", "ols"),
                     small_sample_df = TRUE) {
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    cli::cli_abort("{.arg formula} must be a two-sided formula such as {.code y ~ x + (1 | cluster)}.")
  }
  method <- match.arg(method)
  weight_mode <- match.arg(weight_mode)
  assert_scalar_numeric(conf_level, "conf_level", min = 0.5, max = 0.9999)
  assert_flag(small_sample_df, "small_sample_df")

  parsed <- parse_lmer_formula(formula, cluster = cluster)
  fixed_formula <- parsed$fixed_formula
  cluster_col <- parsed$cluster
  truth_col <- all.vars(fixed_formula[[2L]])
  if (length(truth_col) != 1L) {
    cli::cli_abort("Left-hand side of {.arg formula} must name a single outcome column.")
  }
  rhs_vars <- all.vars(fixed_formula[[3L]])

  sp <- prepare_ppi_data(
    data = data,
    truth_col = truth_col,
    pred_col = pred_col,
    unlabeled = unlabeled,
    extra_cols = unique(c(rhs_vars, cluster_col))
  )

  mats <- build_ppi_model_matrices(fixed_formula, sp$labeled, sp$unlabeled)
  X_lab <- mats$X_lab
  X_unlab <- mats$X_unlab
  y_lab <- as.numeric(sp$labeled[[truth_col]])
  yhat_lab <- as.numeric(sp$labeled[[pred_col]])
  yhat_unlab <- as.numeric(sp$unlabeled[[pred_col]])
  cl_lab <- as.character(sp$labeled[[cluster_col]])
  cl_unlab <- as.character(sp$unlabeled[[cluster_col]])

  # Fit REML random-intercept lmer on labeled human data & naive LLM data for variance components
  vc_hum <- fit_random_intercept_vc(y = y_lab, X = X_lab, cluster = cl_lab)
  vc_naive <- fit_random_intercept_vc(
    y = c(yhat_lab, yhat_unlab),
    X = rbind(X_lab, X_unlab),
    cluster = c(cl_lab, cl_unlab)
  )

  overlap <- length(intersect(unique(cl_lab), unique(cl_unlab))) > 0L

  if (weight_mode == "gls" && !overlap && vc_hum$tau2 > 0 && vc_hum$sigma2 > 0) {
    # Disjoint clusters: exact cluster-level Woodbury GLS PPI++ (unbiased because cluster-size distributions match)
    res <- fit_ppi_gls_disjoint(
      X_lab = X_lab,
      y_lab = y_lab,
      yhat_lab = yhat_lab,
      cl_lab = cl_lab,
      X_unlab = X_unlab,
      yhat_unlab = yhat_unlab,
      cl_unlab = cl_unlab,
      tau2 = vc_hum$tau2,
      sigma2 = vc_hum$sigma2,
      method = method,
      lambda = lambda,
      conf_level = conf_level,
      small_sample_df = small_sample_df
    )
    weight_used <- "gls_woodbury"
  } else {
    # Overlapping clusters or OLS mode: identical observation weights across L and U guarantee exact unbiasedness
    w_lab <- NULL
    w_unlab <- NULL
    weight_used <- "ols"
    if (weight_mode == "gls" && vc_hum$tau2 > 0 && vc_hum$sigma2 > 0) {
      # Inverse total-cluster-variance weight depends ONLY on total cluster size (L + U), identical for L and U rows
      all_cl <- c(cl_lab, cl_unlab)
      tot_sizes <- table(all_cl)
      w_lab <- 1 / (vc_hum$sigma2 + as.numeric(tot_sizes[cl_lab]) * vc_hum$tau2)
      w_unlab <- 1 / (vc_hum$sigma2 + as.numeric(tot_sizes[cl_unlab]) * vc_hum$tau2)
      weight_used <- "gls_cluster_size_weighted"
    }

    res <- fit_ppi_clustered_engine(
      X_lab = X_lab,
      y_lab = y_lab,
      yhat_lab = yhat_lab,
      cl_lab = cl_lab,
      X_unlab = X_unlab,
      yhat_unlab = yhat_unlab,
      cl_unlab = cl_unlab,
      family_name = "gaussian",
      method = method,
      lambda = lambda,
      conf_level = conf_level,
      small_sample_df = small_sample_df,
      w_lab = w_lab,
      w_unlab = w_unlab
    )
  }

  vc_tbl <- tibble::tibble(
    source = c("human_only", "naive_llm"),
    tau2_cluster = c(vc_hum$tau2, vc_naive$tau2),
    sigma2_resid = c(vc_hum$sigma2, vc_naive$sigma2),
    icc = c(vc_hum$icc, vc_naive$icc)
  )

  new_llm_ppi(
    subclass = c("llm_ppi_lmer", "llm_ppi_clustered"),
    estimates = res$tbl_ppi,
    benchmarks = rbind(res$tbl_ppi, res$tbl_hum, res$tbl_naive),
    theta = res$theta,
    vcov_mat = res$vcov,
    lambda = res$lambda,
    method_label = res$method_label,
    n_labeled = nrow(X_lab),
    n_unlabeled = nrow(X_unlab),
    conf_level = conf_level,
    df = res$df,
    extra = list(
      formula = formula,
      fixed_formula = fixed_formula,
      family = "gaussian",
      cluster = cluster_col,
      pred_col = pred_col,
      cluster_structure = res$cluster_structure,
      weight_mode = weight_used,
      variance_components = vc_tbl,
      n_clusters_labeled = res$G_lab,
      n_clusters_unlabeled = res$G_unlab,
      n_clusters_total = res$G_total
    )
  )
}

#' Parse optional `(1 | cluster)` random-intercept term from formula
#' @noRd
parse_lmer_formula <- function(formula, cluster = NULL) {
  f_str <- paste(deparse(formula, width.cutoff = 500L), collapse = " ")
  m <- regexec("\\(\\s*1\\s*\\|\\s*([A-Za-z0-9_.]+)\\s*\\)", f_str)
  hits <- regmatches(f_str, m)[[1L]]

  if (length(hits) >= 2L) {
    detected_cluster <- hits[2L]
    if (!is.null(cluster) && !identical(cluster, detected_cluster)) {
      cli::cli_abort(
        "{.arg cluster} ({.val {cluster}}) conflicts with random-intercept term {.code (1 | {detected_cluster})}."
      )
    }
    cluster <- detected_cluster
    # Remove the random-intercept term cleanly
    lhs <- deparse(formula[[2L]], width.cutoff = 500L)
    rhs_raw <- paste(deparse(formula[[3L]], width.cutoff = 500L), collapse = " ")
    rhs_clean <- gsub("\\+?\\s*\\(\\s*1\\s*\\|\\s*[A-Za-z0-9_.]+\\s*\\)", "", rhs_raw)
    rhs_clean <- trimws(gsub("^\\s*\\+\\s*|\\s*\\+\\s*$", "", rhs_clean))
    if (!nzchar(rhs_clean)) rhs_clean <- "1"
    fixed_formula <- stats::as.formula(paste(lhs, "~", rhs_clean), env = environment(formula))
  } else {
    if (is.null(cluster)) {
      cli::cli_abort(
        "Specify a random-intercept term {.code (1 | cluster)} in {.arg formula} or pass {.arg cluster} explicitly."
      )
    }
    assert_scalar_character(cluster, "cluster")
    fixed_formula <- formula
  }

  list(fixed_formula = fixed_formula, cluster = cluster)
}

#' Estimate random-intercept variance components (tau2, sigma2, ICC) via lme4::lmer
#' @noRd
fit_random_intercept_vc <- function(y, X, cluster) {
  work <- data.frame(y = y, cluster_id = factor(cluster))
  x_df <- as.data.frame(X)
  # Avoid duplicate intercept column in lmer
  non_int <- setdiff(colnames(X), "(Intercept)")
  if (length(non_int) > 0L) {
    safe_names <- paste0("x_", seq_along(non_int))
    for (i in seq_along(non_int)) {
      work[[safe_names[i]]] <- X[, non_int[i]]
    }
    fml <- stats::as.formula(paste("y ~", paste(safe_names, collapse = " + "), "+ (1 | cluster_id)"))
  } else {
    fml <- stats::as.formula("y ~ 1 + (1 | cluster_id)")
  }

  ctrl <- lme4::lmerControl(
    check.nobs.vs.nlev = "ignore",
    check.nobs.vs.nRE = "ignore",
    check.nlev.gtreq.5 = "ignore"
  )
  fit <- tryCatch(
    suppressMessages(suppressWarnings(lme4::lmer(fml, data = work, REML = TRUE, control = ctrl))),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    v <- stats::var(y)
    return(list(tau2 = 0, sigma2 = max(v, 1e-8), icc = 0))
  }
  vc <- as.data.frame(lme4::VarCorr(fit))
  tau2 <- max(as.numeric(vc$vcov[vc$grp == "cluster_id"]), 0)
  sigma2 <- max(as.numeric(vc$vcov[vc$grp == "Residual"]), 1e-8)
  icc <- tau2 / (tau2 + sigma2)
  list(tau2 = tau2, sigma2 = sigma2, icc = icc)
}

#' Core cluster-robust PPI / PPI++ engine for disjoint AND overlapping clusters
#' @noRd
fit_ppi_clustered_engine <- function(X_lab, y_lab, yhat_lab, cl_lab,
                                     X_unlab, yhat_unlab, cl_unlab,
                                     family_name = "gaussian",
                                     method = "ppi++",
                                     lambda = NULL,
                                     conf_level = 0.95,
                                     small_sample_df = TRUE,
                                     w_lab = NULL,
                                     w_unlab = NULL,
                                     max_iter = 60L,
                                     tol = 1e-9) {
  n <- nrow(X_lab)
  N <- nrow(X_unlab)
  p <- ncol(X_lab)
  ops <- canonical_glm_ops(family_name)

  if (is.null(w_lab)) w_lab <- rep(1, n)
  if (is.null(w_unlab)) w_unlab <- rep(1, N)
  w_lab <- w_lab / mean(w_lab)
  w_unlab <- w_unlab / mean(w_unlab)

  cl_L_u <- unique(cl_lab)
  cl_U_u <- unique(cl_unlab)
  all_cl <- union(cl_L_u, cl_U_u)
  G_lab <- length(cl_L_u)
  G_unlab <- length(cl_U_u)
  G_total <- length(all_cl)
  overlap <- length(intersect(cl_L_u, cl_U_u)) > 0L
  cluster_structure <- if (overlap) "overlapping" else "disjoint"

  if (G_lab < 3L) {
    cli::cli_abort("At least 3 labeled clusters are required for cluster-robust inference (found {G_lab}).")
  }
  if (G_unlab < 3L) {
    cli::cli_abort("At least 3 unlabeled clusters are required for cluster-robust inference (found {G_unlab}).")
  }

  # Helper: align rowsum(M, cl) onto full cluster order `target_cl`
  sum_by_cluster <- function(M, cl, target_cl) {
    rs <- rowsum(M, group = cl, reorder = FALSE)
    out <- matrix(0, nrow = length(target_cl), ncol = ncol(M), dimnames = list(target_cl, colnames(M)))
    out[rownames(rs), ] <- rs
    out
  }

  # Benchmarks with cluster-robust CR1 SEs
  fit_cluster_bench <- function(X, y, cl, w) {
    sol_b <- solve_ppi_glm_objective(
      X_lab = X, y_lab = y, yhat_lab = rep(0, nrow(X)),
      X_unlab = X[1L, , drop = FALSE], yhat_unlab = 0,
      family_name = family_name, lambda = 0, w_lab = w,
      max_iter = max_iter, tol = tol
    )
    mu <- ops$mu(as.numeric(X %*% sol_b$theta))
    G_mat <- X * (w * (mu - y))
    G_c <- sweep(G_mat, 2L, colMeans(G_mat), "-")
    S <- rowsum(G_c, group = cl, reorder = FALSE)
    Gc <- nrow(S)
    H_inv <- safe_solve(sol_b$hessian)
    adj <- (Gc / max(Gc - 1, 1)) * ((nrow(X) - 1) / max(nrow(X) - p, 1))
    meat <- (adj / (nrow(X)^2)) * crossprod(S)
    V <- H_inv %*% meat %*% H_inv
    V <- 0.5 * (V + t(V))
    dimnames(V) <- list(colnames(X), colnames(X))
    list(theta = sol_b$theta, vcov = V, hessian = sol_b$hessian)
  }

  bench_hum <- fit_cluster_bench(X_lab, y_lab, cl_lab, w_lab)
  X_all <- rbind(X_lab, X_unlab)
  yhat_all <- c(yhat_lab, yhat_unlab)
  cl_all <- c(cl_lab, cl_unlab)
  w_all <- c(w_lab, w_unlab)
  bench_naive <- fit_cluster_bench(X_all, yhat_all, cl_all, w_all)

  # Determine cluster-optimal lambda*
  if (!is.null(lambda)) {
    assert_scalar_numeric(lambda, "lambda", min = 0, max = 1)
    lam_val <- as.numeric(lambda)
    method_label <- if (abs(lam_val - 1) < 1e-12) "ppi_clustered" else paste0("ppi_clustered(lambda=", round(lam_val, 2L), ")")
  } else if (method == "ppi") {
    lam_val <- 1
    method_label <- "ppi_clustered"
  } else {
    pilot <- bench_hum$theta
    mu_L0 <- ops$mu(as.numeric(X_lab %*% pilot))
    mu_U0 <- ops$mu(as.numeric(X_unlab %*% pilot))
    G_L0 <- X_lab * (w_lab * (mu_L0 - y_lab))
    Gt_L0 <- X_lab * (w_lab * (mu_L0 - yhat_lab))
    Gt_U0 <- X_unlab * (w_unlab * (mu_U0 - yhat_unlab))

    G_L0_c <- sweep(G_L0, 2L, colMeans(G_L0), "-")
    Gt_L0_c <- sweep(Gt_L0, 2L, colMeans(Gt_L0), "-")
    Gt_U0_c <- sweep(Gt_U0, 2L, colMeans(Gt_U0), "-")

    A_mat <- sum_by_cluster(G_L0_c, cl_lab, all_cl) / n
    B_mat <- sum_by_cluster(Gt_L0_c, cl_lab, all_cl) / n - sum_by_cluster(Gt_U0_c, cl_unlab, all_cl) / N

    H_pool <- crossprod(X_all, X_all * (w_all * ops$var(as.numeric(X_all %*% pilot)))) / (n + N)
    H_inv <- safe_solve(H_pool)

    HA <- A_mat %*% t(H_inv)
    HB <- B_mat %*% t(H_inv)
    num <- sum(HA * HB)
    denom <- sum(HB * HB)
    lam_val <- if (is.finite(denom) && denom > 1e-12) min(max(num / denom, 0), 1) else 0
    method_label <- "ppi++_clustered"
  }

  # Solve PPI++ objective
  sol <- solve_ppi_glm_objective(
    X_lab = X_lab,
    y_lab = y_lab,
    yhat_lab = yhat_lab,
    X_unlab = X_unlab,
    yhat_unlab = yhat_unlab,
    family_name = family_name,
    lambda = lam_val,
    w_lab = w_lab,
    w_unlab = w_unlab,
    start = bench_hum$theta,
    max_iter = max_iter,
    tol = tol
  )

  # Cluster-robust sandwich covariance at sol$theta
  mu_L <- ops$mu(as.numeric(X_lab %*% sol$theta))
  mu_U <- ops$mu(as.numeric(X_unlab %*% sol$theta))
  D_lab <- X_lab * (w_lab * ((mu_L - y_lab) - lam_val * (mu_L - yhat_lab)))
  U_unlab <- X_unlab * (w_unlab * (mu_U - yhat_unlab))

  D_lab_c <- sweep(D_lab, 2L, colMeans(D_lab), "-")
  U_unlab_c <- sweep(U_unlab, 2L, colMeans(U_unlab), "-")

  H_inv <- safe_solve(sol$hessian)

  if (!overlap) {
    S_L <- rowsum(D_lab_c, group = cl_lab, reorder = FALSE)
    S_U <- rowsum(U_unlab_c, group = cl_unlab, reorder = FALSE)
    adj_L <- (G_lab / max(G_lab - 1, 1)) * ((n - 1) / max(n - p, 1))
    adj_U <- (G_unlab / max(G_unlab - 1, 1)) * ((N - 1) / max(N - p, 1))
    meat <- (adj_L / (n^2)) * crossprod(S_L) + ((lam_val^2) * adj_U / (N^2)) * crossprod(S_U)
    df_eff <- if (isTRUE(small_sample_df)) max(G_lab - 1, 1) else Inf
  } else {
    S_tot <- sum_by_cluster(D_lab_c, cl_lab, all_cl) / n +
      lam_val * sum_by_cluster(U_unlab_c, cl_unlab, all_cl) / N
    adj_tot <- (G_total / max(G_total - 1, 1))
    meat <- adj_tot * crossprod(S_tot)
    df_eff <- if (isTRUE(small_sample_df)) max(G_total - 1, 1) else Inf
  }

  V_ppi <- H_inv %*% meat %*% H_inv
  V_ppi <- 0.5 * (V_ppi + t(V_ppi))
  dimnames(V_ppi) <- list(colnames(X_lab), colnames(X_lab))

  df_hum <- if (isTRUE(small_sample_df)) max(G_lab - 1, 1) else Inf
  df_naive <- if (isTRUE(small_sample_df)) max(G_total - 1, 1) else Inf

  tbl_ppi <- tidy_ppi_table(sol$theta, V_ppi, method_label = method_label, conf_level = conf_level, df = df_eff)
  tbl_hum <- tidy_ppi_table(bench_hum$theta, bench_hum$vcov, method_label = "human_only", conf_level = conf_level, df = df_hum)
  tbl_naive <- tidy_ppi_table(bench_naive$theta, bench_naive$vcov, method_label = "naive_llm", conf_level = conf_level, df = df_naive)

  list(
    theta = sol$theta,
    vcov = V_ppi,
    lambda = lam_val,
    method_label = method_label,
    tbl_ppi = tbl_ppi,
    tbl_hum = tbl_hum,
    tbl_naive = tbl_naive,
    df = df_eff,
    cluster_structure = cluster_structure,
    G_lab = G_lab,
    G_unlab = G_unlab,
    G_total = G_total
  )
}

#' Exact Woodbury random-intercept GLS PPI++ for disjoint cluster validation
#' @noRd
fit_ppi_gls_disjoint <- function(X_lab, y_lab, yhat_lab, cl_lab,
                                 X_unlab, yhat_unlab, cl_unlab,
                                 tau2, sigma2,
                                 method = "ppi++",
                                 lambda = NULL,
                                 conf_level = 0.95,
                                 small_sample_df = TRUE) {
  p <- ncol(X_lab)
  cl_L_u <- unique(cl_lab)
  cl_U_u <- unique(cl_unlab)
  G_lab <- length(cl_L_u)
  G_unlab <- length(cl_U_u)
  G_total <- G_lab + G_unlab

  if (G_lab < 3L || G_unlab < 3L) {
    cli::cli_abort("At least 3 labeled and 3 unlabeled clusters are required for {.fn ppi_lmer}.")
  }

  # Helper: apply Woodbury V_c^{-1} v for a cluster of size m
  apply_Vinv <- function(M, tau2, sigma2) {
    M <- as.matrix(M)
    m <- nrow(M)
    c_sum <- colSums(M)
    shrink <- tau2 / (sigma2 + m * tau2)
    (M - matrix(shrink * c_sum, nrow = m, ncol = ncol(M), byrow = TRUE)) / sigma2
  }

  # Precompute per-cluster GLS bread and linear terms
  build_cluster_gls <- function(X, y1, y2, cl, cl_u) {
    G <- length(cl_u)
    idx_list <- split(seq_len(nrow(X)), factor(cl, levels = cl_u))
    H_list <- vector("list", G)
    xy1_list <- vector("list", G)
    xy2_list <- vector("list", G)

    for (g in seq_len(G)) {
      ii <- idx_list[[g]]
      Xg <- X[ii, , drop = FALSE]
      Vinv_Xg <- apply_Vinv(Xg, tau2, sigma2)
      H_list[[g]] <- crossprod(Xg, Vinv_Xg)
      xy1_list[[g]] <- as.numeric(crossprod(Vinv_Xg, y1[ii]))
      if (!is.null(y2)) {
        xy2_list[[g]] <- as.numeric(crossprod(Vinv_Xg, y2[ii]))
      }
    }

    H_avg <- Reduce(`+`, H_list) / G
    xy1_avg <- Reduce(`+`, xy1_list) / G
    xy2_avg <- if (!is.null(y2)) Reduce(`+`, xy2_list) / G else NULL

    list(G = G, H_list = H_list, xy1_list = xy1_list, xy2_list = xy2_list,
         H_avg = H_avg, xy1_avg = xy1_avg, xy2_avg = xy2_avg)
  }

  gls_L <- build_cluster_gls(X_lab, y_lab, yhat_lab, cl_lab, cl_L_u)
  gls_U <- build_cluster_gls(X_unlab, yhat_unlab, NULL, cl_unlab, cl_U_u)

  # Cluster-level score matrices evaluated at theta: s_g(theta) = H_g theta - xy_g
  eval_cluster_scores <- function(gls_obj, theta, use_y2 = FALSE) {
    G <- gls_obj$G
    S <- matrix(0, nrow = G, ncol = p, dimnames = list(NULL, colnames(X_lab)))
    xy_l <- if (use_y2) gls_obj$xy2_list else gls_obj$xy1_list
    for (g in seq_len(G)) {
      S[g, ] <- as.numeric(gls_obj$H_list[[g]] %*% theta) - xy_l[[g]]
    }
    S
  }

  # Human-only GLS benchmark (safe_cov divides by G_lab - 1 -> exact CR1 cluster sandwich)
  theta_hum <- as.numeric(safe_solve(gls_L$H_avg, gls_L$xy1_avg))
  names(theta_hum) <- colnames(X_lab)
  S_hum <- eval_cluster_scores(gls_L, theta_hum, use_y2 = FALSE)
  H_hum_inv <- safe_solve(gls_L$H_avg)
  V_hum <- (1 / G_lab) * (H_hum_inv %*% safe_cov(S_hum) %*% H_hum_inv)
  V_hum <- 0.5 * (V_hum + t(V_hum))
  dimnames(V_hum) <- list(colnames(X_lab), colnames(X_lab))

  # Naive-LLM GLS benchmark (pooled L + U)
  H_naive <- (G_lab * gls_L$H_avg + G_unlab * gls_U$H_avg) / G_total
  xy_naive <- (G_lab * gls_L$xy2_avg + G_unlab * gls_U$xy1_avg) / G_total
  theta_naive <- as.numeric(safe_solve(H_naive, xy_naive))
  names(theta_naive) <- colnames(X_lab)
  S_naive <- rbind(
    eval_cluster_scores(gls_L, theta_naive, use_y2 = TRUE),
    eval_cluster_scores(gls_U, theta_naive, use_y2 = FALSE)
  )
  H_naive_inv <- safe_solve(H_naive)
  V_naive <- (1 / G_total) * (H_naive_inv %*% safe_cov(S_naive) %*% H_naive_inv)
  V_naive <- 0.5 * (V_naive + t(V_naive))
  dimnames(V_naive) <- list(colnames(X_lab), colnames(X_lab))

  # Determine cluster-optimal lambda*
  if (!is.null(lambda)) {
    assert_scalar_numeric(lambda, "lambda", min = 0, max = 1)
    lam_val <- as.numeric(lambda)
    method_label <- if (abs(lam_val - 1) < 1e-12) "ppi_lmer" else paste0("ppi_lmer(lambda=", round(lam_val, 2L), ")")
  } else if (method == "ppi") {
    lam_val <- 1
    method_label <- "ppi_lmer"
  } else {
    S_y0 <- eval_cluster_scores(gls_L, theta_hum, use_y2 = FALSE)
    S_fL0 <- eval_cluster_scores(gls_L, theta_hum, use_y2 = TRUE)
    S_fU0 <- eval_cluster_scores(gls_U, theta_hum, use_y2 = FALSE)
    H_inv_p <- safe_solve(H_naive)

    C_gg <- safe_cross_cov(S_y0, S_fL0)
    V_tilde <- safe_cov(rbind(S_fL0, S_fU0))
    num <- sum(diag(H_inv_p %*% C_gg %*% H_inv_p))
    denom <- (1 + G_lab / G_unlab) * sum(diag(H_inv_p %*% V_tilde %*% H_inv_p))
    lam_val <- if (is.finite(denom) && denom > 1e-12) min(max(num / denom, 0), 1) else 0
    method_label <- "ppi++_lmer"
  }

  # Solve GLS PPI++ normal equations
  H_ppi <- (1 - lam_val) * gls_L$H_avg + lam_val * gls_U$H_avg
  rhs_ppi <- (gls_L$xy1_avg - lam_val * gls_L$xy2_avg) + lam_val * gls_U$xy1_avg
  theta_ppi <- as.numeric(safe_solve(H_ppi, rhs_ppi))
  names(theta_ppi) <- colnames(X_lab)

  # Cluster-robust sandwich covariance
  S_y <- eval_cluster_scores(gls_L, theta_ppi, use_y2 = FALSE)
  S_fL <- eval_cluster_scores(gls_L, theta_ppi, use_y2 = TRUE)
  S_fU <- eval_cluster_scores(gls_U, theta_ppi, use_y2 = FALSE)

  H_ppi_inv <- safe_solve(H_ppi)
  meat <- (1 / G_lab) * safe_cov(S_y - lam_val * S_fL) + ((lam_val^2) / G_unlab) * safe_cov(S_fU)
  V_ppi <- H_ppi_inv %*% meat %*% H_ppi_inv
  V_ppi <- 0.5 * (V_ppi + t(V_ppi))
  dimnames(V_ppi) <- list(colnames(X_lab), colnames(X_lab))

  df_eff <- if (isTRUE(small_sample_df)) max(G_lab - 1, 1) else Inf
  df_naive <- if (isTRUE(small_sample_df)) max(G_total - 1, 1) else Inf

  tbl_ppi <- tidy_ppi_table(theta_ppi, V_ppi, method_label = method_label, conf_level = conf_level, df = df_eff)
  tbl_hum <- tidy_ppi_table(theta_hum, V_hum, method_label = "human_only", conf_level = conf_level, df = df_eff)
  tbl_naive <- tidy_ppi_table(theta_naive, V_naive, method_label = "naive_llm", conf_level = conf_level, df = df_naive)

  list(
    theta = theta_ppi,
    vcov = V_ppi,
    lambda = lam_val,
    method_label = method_label,
    tbl_ppi = tbl_ppi,
    tbl_hum = tbl_hum,
    tbl_naive = tbl_naive,
    df = df_eff,
    cluster_structure = "disjoint",
    G_lab = G_lab,
    G_unlab = G_unlab,
    G_total = G_total
  )
}
