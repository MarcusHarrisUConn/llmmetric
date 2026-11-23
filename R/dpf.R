#' Differential Prompt Functioning (DPF) & Measurement Invariance Test
#'
#' Tests whether LLM-generated scores exhibit uniform (intercept shift) or
#' non-uniform (slope difference) bias across observed demographic or
#' experimental subgroups conditional on human gold-standard scores. Adapts
#' nested OLS / logistic Differential Item Functioning (DIF) regression
#' frameworks (Swaminathan & Rogers, 1990; Zumbo, 1999) to LLM coding pipelines,
#' with automatic unit-clustered sandwich inference when units appear across
#' multiple prompts or models, per-prompt FDR screening, and a global
#' `prompt x group` interaction test.
#'
#' @param data An `llm_coded_data` object or a `data.frame`.
#' @param score_col Name of the LLM score column (numeric for `"gaussian"`, or
#'   binary `{0,1}` / logical / 2-level factor for `"binomial"`).
#' @param truth_col Name of the numeric human gold-standard (matching criterion)
#'   column.
#' @param group_col Name of the categorical subgroup column (at least 2 levels).
#' @param prompt_col Optional name of a prompt column. When supplied with at
#'   least 2 levels, runs per-prompt DPF screening (with Benjamini-Hochberg
#'   FDR-adjusted p-values) and a global `prompt x group` interaction test.
#' @param id_col Optional name of the unit identifier column. When repeated
#'   observations exist per unit, cluster-robust sandwich Wald tests are used
#'   to prevent pseudoreplication Type-I error inflation.
#' @param family Outcome family: `"auto"` (detects binary vs continuous),
#'   `"gaussian"`, or `"binomial"`.
#' @param poly_degree Positive integer polynomial degree for the matching
#'   criterion `truth_col`. Default `1L` (linear matching).
#' @param alpha Significance threshold for flagging DPF. Default `0.05`.
#' @param na.rm Logical; drop rows with missing values. Default `TRUE`.
#'
#' @return An S3 object of class `"llm_dpf"` containing:
#'   - `tests`: tibble of nested hypothesis tests (`uniform`, `nonuniform`,
#'     `omnibus`) with test statistics, df, p-values, \eqn{\Delta R^2},
#'     Cohen's \eqn{f^2} (Gaussian), magnitude classification, and `flagged`.
#'   - `subgroup_summary`: tibble of per-subgroup raw bias, conditional bias at
#'     mean gold-standard score (with SE and 95% CI), conditional slope, RMSE,
#'     and correlation.
#'   - `prompt_dpf`: tibble of per-prompt DPF tests with BH-adjusted p-values
#'     (or `NULL` if `prompt_col` is `NULL`).
#'   - `prompt_interaction_test`: tibble testing `prompt x group` interaction
#'     controlling for `truth_col` (or `NULL`).
#'   - `dpf_detected`: logical scalar indicating omnibus DPF at `alpha`.
#'   - `clustered`: logical indicating whether unit-clustered inference was used.
#'   - `family`, `reference_group`, `models`, `score_col`, `truth_col`,
#'     `group_col`, `prompt_col`, `alpha`, `data`.
#' @export
#'
#' @examples
#' data(ema_coded)
#' agg <- aggregate_scores(ema_coded, score_col = "llm_score")
#' res <- dpf_test(agg, score_col = "llm_score", truth_col = "human_score", group_col = "group")
#' res
dpf_test <- function(data,
                     score_col = "score",
                     truth_col = "human_score",
                     group_col = "group",
                     prompt_col = NULL,
                     id_col = NULL,
                     family = c("auto", "gaussian", "binomial"),
                     poly_degree = 1L,
                     alpha = 0.05,
                     na.rm = TRUE) {
  if (inherits(data, "llm_coded_data")) {
    if (missing(score_col)) score_col <- data$primary_score
    if (is.null(id_col) && "unit_id" %in% names(data$scores)) id_col <- "unit_id"
  }
  df <- extract_data_frame(data, arg = "data")
  family <- match.arg(family)
  assert_scalar_character(score_col, "score_col")
  assert_scalar_character(truth_col, "truth_col")
  assert_scalar_character(group_col, "group_col")
  assert_scalar_character(prompt_col, "prompt_col", allow_null = TRUE)
  assert_scalar_character(id_col, "id_col", allow_null = TRUE)
  assert_scalar_numeric(poly_degree, "poly_degree", min = 1, max = 4)
  poly_degree <- as.integer(round(poly_degree))
  assert_scalar_numeric(alpha, "alpha", min = 1e-6, max = 0.5)
  assert_flag(na.rm, "na.rm")

  req_cols <- unique(c(score_col, truth_col, group_col, prompt_col %||% character(), id_col %||% character()))
  assert_columns(df, req_cols, arg = "data")

  if (!is.numeric(df[[truth_col]])) {
    cli::cli_abort("{.arg truth_col} ({.val {truth_col}}) must be numeric.")
  }
  assert_finite_values(df[[truth_col]], truth_col)

  if (isTRUE(na.rm)) {
    keep <- stats::complete.cases(df[, req_cols, drop = FALSE])
    df <- df[keep, req_cols, drop = FALSE]
  } else if (any(!stats::complete.cases(df[, req_cols, drop = FALSE]))) {
    cli::cli_abort("Missing values detected and {.code na.rm = FALSE}.")
  }

  if (nrow(df) < 10L) {
    cli::cli_abort("At least 10 complete observations are required for {.fn dpf_test}.")
  }

  # Resolve family & numeric score vector
  raw_score <- df[[score_col]]
  if (family == "auto") {
    if (is.logical(raw_score) ||
        (is.factor(raw_score) && nlevels(droplevels(raw_score)) == 2L) ||
        (is.character(raw_score) && length(unique(raw_score)) == 2L) ||
        (is.numeric(raw_score) && all(unique(raw_score) %in% c(0, 1)))) {
      family <- "binomial"
    } else {
      family <- "gaussian"
    }
  }

  if (family == "binomial") {
    if (is.logical(raw_score)) {
      y_num <- as.numeric(raw_score)
    } else if (is.factor(raw_score) || is.character(raw_score)) {
      fac <- droplevels(factor(raw_score))
      if (nlevels(fac) != 2L) {
        cli::cli_abort("Binomial DPF requires exactly 2 outcome levels in {.val {score_col}}.")
      }
      y_num <- as.numeric(fac) - 1
    } else if (is.numeric(raw_score) && all(raw_score %in% c(0, 1))) {
      y_num <- as.numeric(raw_score)
    } else {
      cli::cli_abort("For {.code family = \"binomial\"}, {.val {score_col}} must be binary (0/1, logical, or 2-level factor).")
    }
  } else {
    if (!is.numeric(raw_score)) {
      cli::cli_abort("For {.code family = \"gaussian\"}, {.val {score_col}} must be numeric.")
    }
    assert_finite_values(raw_score, score_col)
    y_num <- as.numeric(raw_score)
  }

  grp_fac <- droplevels(factor(df[[group_col]]))
  if (nlevels(grp_fac) < 2L) {
    cli::cli_abort("{.arg group_col} ({.val {group_col}}) must have at least 2 distinct levels.")
  }
  ref_group <- levels(grp_fac)[1L]

  cluster_vec <- if (!is.null(id_col)) as.character(df[[id_col]]) else NULL
  clustered <- !is.null(cluster_vec) && any(duplicated(cluster_vec))

  core <- fit_dpf_core(
    y = y_num,
    truth = as.numeric(df[[truth_col]]),
    group = grp_fac,
    cluster = if (clustered) cluster_vec else NULL,
    family = family,
    poly_degree = poly_degree,
    alpha = alpha
  )

  # Prompt-level DPF screening and Prompt x Group interaction test
  prompt_dpf_tbl <- NULL
  prompt_int_tbl <- NULL
  if (!is.null(prompt_col)) {
    p_fac <- droplevels(factor(df[[prompt_col]]))
    if (nlevels(p_fac) >= 2L) {
      p_levels <- levels(p_fac)
      p_rows <- lapply(p_levels, function(pl) {
        ii <- which(as.character(p_fac) == pl)
        sub_grp <- droplevels(grp_fac[ii])
        if (length(ii) < 8L || nlevels(sub_grp) < 2L) {
          return(tibble::tibble(
            prompt = pl,
            n = length(ii),
            uniform_p = NA_real_,
            nonuniform_p = NA_real_,
            omnibus_p = NA_real_,
            delta_r2_uniform = NA_real_,
            delta_r2_nonuniform = NA_real_,
            delta_r2_total = NA_real_,
            magnitude = NA_character_
          ))
        }
        sub_cl <- if (!is.null(cluster_vec) && any(duplicated(cluster_vec[ii]))) cluster_vec[ii] else NULL
        sub_res <- fit_dpf_core(
          y = y_num[ii],
          truth = as.numeric(df[[truth_col]][ii]),
          group = sub_grp,
          cluster = sub_cl,
          family = family,
          poly_degree = poly_degree,
          alpha = alpha
        )
        t_u <- sub_res$tests[sub_res$tests$test == "uniform", , drop = FALSE]
        t_nu <- sub_res$tests[sub_res$tests$test == "nonuniform", , drop = FALSE]
        t_om <- sub_res$tests[sub_res$tests$test == "omnibus", , drop = FALSE]
        tibble::tibble(
          prompt = pl,
          n = length(ii),
          uniform_p = t_u$p_value,
          nonuniform_p = t_nu$p_value,
          omnibus_p = t_om$p_value,
          delta_r2_uniform = t_u$delta_r2,
          delta_r2_nonuniform = t_nu$delta_r2,
          delta_r2_total = t_om$delta_r2,
          magnitude = t_om$magnitude
        )
      })
      prompt_dpf_tbl <- do.call(rbind, p_rows)
      prompt_dpf_tbl$omnibus_p_adj <- stats::p.adjust(prompt_dpf_tbl$omnibus_p, method = "BH")
      prompt_dpf_tbl$flagged <- !is.na(prompt_dpf_tbl$omnibus_p_adj) & (prompt_dpf_tbl$omnibus_p_adj < alpha)

      # Global Prompt x Group interaction test controlling for truth
      prompt_int_tbl <- fit_prompt_group_interaction(
        y = y_num,
        truth = as.numeric(df[[truth_col]]),
        group = grp_fac,
        prompt = p_fac,
        cluster = if (clustered) cluster_vec else NULL,
        family = family,
        alpha = alpha
      )
    }
  }

  omnibus_row <- core$tests[core$tests$test == "omnibus", , drop = FALSE]
  dpf_detected <- isTRUE(omnibus_row$flagged[1L])

  plot_df <- tibble::tibble(
    score = y_num,
    truth = as.numeric(df[[truth_col]]),
    group = grp_fac
  )
  if (!is.null(prompt_col)) {
    plot_df$prompt <- factor(df[[prompt_col]])
  }

  structure(
    list(
      tests = core$tests,
      subgroup_summary = core$subgroup_summary,
      prompt_dpf = prompt_dpf_tbl,
      prompt_interaction_test = prompt_int_tbl,
      dpf_detected = dpf_detected,
      clustered = clustered,
      family = family,
      reference_group = ref_group,
      models = core$models,
      score_col = score_col,
      truth_col = truth_col,
      group_col = group_col,
      prompt_col = prompt_col,
      alpha = alpha,
      data = plot_df
    ),
    class = "llm_dpf"
  )
}

#' Classify DPF effect size magnitude using Zumbo-Thomas / Jodoin-Gierl thresholds
#' @noRd
classify_dpf_magnitude <- function(delta_r2) {
  ifelse(
    is.na(delta_r2),
    NA_character_,
    ifelse(
      delta_r2 < 0.035,
      "negligible",
      ifelse(delta_r2 < 0.070, "moderate", "large")
    )
  )
}

#' Compute Nagelkerke pseudo-R^2 for a binomial GLM
#' @noRd
nagelkerke_r2 <- function(fit) {
  n <- stats::nobs(fit)
  ll_null <- -0.5 * fit$null.deviance
  ll_mod <- -0.5 * fit$deviance
  cox_snell <- 1 - exp((2 / n) * (ll_null - ll_mod))
  max_r2 <- 1 - exp((2 / n) * ll_null)
  if (max_r2 <= 0) return(0)
  max(min(cox_snell / max_r2, 1), 0)
}

#' Cluster-robust CR1 sandwich covariance for lm or glm
#' @noRd
vcov_cluster_fit <- function(fit, cluster) {
  X <- stats::model.matrix(fit)
  n <- nrow(X)
  p <- ncol(X)
  cl <- as.character(cluster)
  u_cl <- unique(cl)
  G <- length(u_cl)
  if (G <= p) {
    return(stats::vcov(fit))
  }

  if (inherits(fit, "glm")) {
    mu <- stats::fitted(fit)
    y <- fit$y
    # Canonical link score contribution: X_i * (y_i - mu_i)
    w <- fit$weights
    bread <- safe_solve(crossprod(X, X * w))
    resid_score <- (y - mu) * fit$prior.weights
    U <- X * resid_score
  } else {
    bread <- safe_solve(crossprod(X))
    U <- X * stats::residuals(fit)
  }

  # Sum score vectors by cluster
  S <- rowsum(U, group = cl, reorder = FALSE)
  adj <- (G / (G - 1)) * ((n - 1) / max(n - p, 1))
  meat <- adj * crossprod(S)
  bread %*% meat %*% bread
}

#' Compute Wald chi-square test for a subset of coefficients using given vcov
#' @noRd
wald_subset_test <- function(fit, idx, V) {
  b <- stats::coef(fit)[idx]
  V_sub <- V[idx, idx, drop = FALSE]
  ok <- !is.na(b)
  b <- b[ok]
  V_sub <- V_sub[ok, ok, drop = FALSE]
  k <- length(b)
  if (k == 0L) {
    return(list(stat = NA_real_, df = 0, p = NA_real_))
  }
  stat <- as.numeric(t(b) %*% safe_solve(V_sub, b))
  stat <- max(stat, 0)
  p <- stats::pchisq(stat, df = k, lower.tail = FALSE)
  list(stat = stat, df = k, p = p)
}

#' Core DPF nested model fitting and hypothesis testing
#' @noRd
fit_dpf_core <- function(y, truth, group, cluster, family, poly_degree, alpha) {
  g_levels <- levels(group)
  t_mean <- mean(truth)
  t_c <- truth - t_mean

  # Guard 1: Constant (zero-variance) LLM scores -> zero DPF, no spurious lm/glm warnings
  if (is_constant_numeric(y)) {
    cli::cli_warn(
      "LLM scores are constant (zero variance); DPF statistics are undefined (NA) and no subgroup bias is detected."
    )
    tests_tbl <- tibble::tibble(
      test = c("uniform", "nonuniform", "omnibus"),
      stat_type = rep(if (family == "gaussian") "F" else "LRT_Chisq", 3L),
      statistic = rep(NA_real_, 3L),
      df = c(length(g_levels) - 1, length(g_levels) - 1, 2 * (length(g_levels) - 1)),
      p_value = rep(NA_real_, 3L),
      delta_r2 = c(0, 0, 0),
      cohens_f2 = if (family == "gaussian") c(0, 0, 0) else rep(NA_real_, 3L),
      magnitude = rep("negligible", 3L),
      flagged = rep(FALSE, 3L)
    )
    sub_rows <- lapply(g_levels, function(gl) {
      ii <- which(as.character(group) == gl)
      y_g <- y[ii]
      tr_g <- truth[ii]
      tibble::tibble(
        group = gl,
        is_reference = identical(gl, g_levels[1L]),
        n = length(ii),
        mean_llm = mean(y_g),
        mean_truth = mean(tr_g),
        raw_bias = mean(y_g - tr_g),
        cond_bias = 0,
        cond_bias_se = 0,
        conf_low = 0,
        conf_high = 0,
        cond_slope = 0,
        cond_slope_se = 0,
        rmse = sqrt(mean((y_g - tr_g)^2)),
        pearson_r = NA_real_
      )
    })
    return(list(
      tests = tests_tbl,
      subgroup_summary = do.call(rbind, sub_rows),
      models = list(null = NULL, uniform = NULL, nonuniform = NULL),
      degenerate = TRUE
    ))
  }

  work <- data.frame(y = y, t_c = t_c, group = group)
  const_truth <- is_constant_numeric(truth)

  # Build polynomial terms cleanly without orthogonal poly prediction quirks
  if (const_truth) {
    fml0 <- stats::as.formula("y ~ 1")
    fml1 <- stats::as.formula("y ~ group")
    fml2 <- stats::as.formula("y ~ group")
  } else {
    t_terms <- if (poly_degree == 1L) {
      "t_c"
    } else {
      paste0("I(t_c^", seq_len(poly_degree), ")", collapse = " + ")
    }
    fml0 <- stats::as.formula(paste("y ~", t_terms))
    fml1 <- stats::as.formula(paste("y ~", t_terms, "+ group"))
    fml2 <- stats::as.formula(paste("y ~", t_terms, "+ group + t_c:group"))
  }

  if (family == "gaussian") {
    m0 <- stats::lm(fml0, data = work)
    m1 <- stats::lm(fml1, data = work)
    m2 <- stats::lm(fml2, data = work)

    r2_0 <- suppressWarnings(summary(m0)$r.squared) %||% 0
    r2_1 <- suppressWarnings(summary(m1)$r.squared) %||% 0
    r2_2 <- suppressWarnings(summary(m2)$r.squared) %||% 0
    if (is.na(r2_0)) r2_0 <- 0
    if (is.na(r2_1)) r2_1 <- 0
    if (is.na(r2_2)) r2_2 <- 0
    dr2_u <- max(r2_1 - r2_0, 0)
    dr2_nu <- if (const_truth) 0 else max(r2_2 - r2_1, 0)
    dr2_tot <- max(r2_2 - r2_0, 0)
    f2_u <- if (1 - r2_1 > 1e-12) dr2_u / (1 - r2_1) else NA_real_
    f2_nu <- if (const_truth) 0 else if (1 - r2_2 > 1e-12) dr2_nu / (1 - r2_2) else NA_real_
    f2_tot <- if (1 - r2_2 > 1e-12) dr2_tot / (1 - r2_2) else NA_real_

    if (is.null(cluster)) {
      a_u <- suppressWarnings(stats::anova(m0, m1))
      a_nu <- if (const_truth) NULL else suppressWarnings(stats::anova(m1, m2))
      a_tot <- suppressWarnings(stats::anova(m0, m2))
      stat_type <- "F"
      stats_vec <- c(a_u$F[2L], if (is.null(a_nu)) NA_real_ else a_nu$F[2L], a_tot$F[2L])
      df_vec <- c(a_u$Df[2L], if (is.null(a_nu)) 0 else a_nu$Df[2L], a_tot$Df[2L])
      p_vec <- c(a_u$`Pr(>F)`[2L], if (is.null(a_nu)) NA_real_ else a_nu$`Pr(>F)`[2L], a_tot$`Pr(>F)`[2L])
      V2 <- suppressWarnings(stats::vcov(m2))
    } else {
      V1 <- vcov_cluster_fit(m1, cluster)
      V2 <- vcov_cluster_fit(m2, cluster)
      idx_g1 <- grep("^group", names(stats::coef(m1)))
      idx_int2 <- grep("t_c:group|group.*:t_c", names(stats::coef(m2)))
      idx_all2 <- union(grep("^group", names(stats::coef(m2))), idx_int2)
      w_u <- wald_subset_test(m1, idx_g1, V1)
      w_nu <- if (const_truth) list(stat = NA_real_, df = 0, p = NA_real_) else wald_subset_test(m2, idx_int2, V2)
      w_tot <- wald_subset_test(m2, idx_all2, V2)
      stat_type <- "Wald_Chisq"
      stats_vec <- c(w_u$stat, w_nu$stat, w_tot$stat)
      df_vec <- c(w_u$df, w_nu$df, w_tot$df)
      p_vec <- c(w_u$p, w_nu$p, w_tot$p)
    }
  } else {
    m0 <- suppressWarnings(stats::glm(fml0, data = work, family = stats::binomial()))
    m1 <- suppressWarnings(stats::glm(fml1, data = work, family = stats::binomial()))
    m2 <- suppressWarnings(stats::glm(fml2, data = work, family = stats::binomial()))

    r2_0 <- nagelkerke_r2(m0)
    r2_1 <- nagelkerke_r2(m1)
    r2_2 <- nagelkerke_r2(m2)
    dr2_u <- max(r2_1 - r2_0, 0)
    dr2_nu <- if (const_truth) 0 else max(r2_2 - r2_1, 0)
    dr2_tot <- max(r2_2 - r2_0, 0)
    f2_u <- NA_real_
    f2_nu <- NA_real_
    f2_tot <- NA_real_

    if (is.null(cluster)) {
      a_u <- suppressWarnings(stats::anova(m0, m1, test = "Chisq"))
      a_nu <- if (const_truth) NULL else suppressWarnings(stats::anova(m1, m2, test = "Chisq"))
      a_tot <- suppressWarnings(stats::anova(m0, m2, test = "Chisq"))
      stat_type <- "LRT_Chisq"
      stats_vec <- c(a_u$Deviance[2L], if (is.null(a_nu)) NA_real_ else a_nu$Deviance[2L], a_tot$Deviance[2L])
      df_vec <- c(a_u$Df[2L], if (is.null(a_nu)) 0 else a_nu$Df[2L], a_tot$Df[2L])
      p_vec <- c(a_u$`Pr(>Chi)`[2L], if (is.null(a_nu)) NA_real_ else a_nu$`Pr(>Chi)`[2L], a_tot$`Pr(>Chi)`[2L])
      V2 <- stats::vcov(m2)
    } else {
      V1 <- vcov_cluster_fit(m1, cluster)
      V2 <- vcov_cluster_fit(m2, cluster)
      idx_g1 <- grep("^group", names(stats::coef(m1)))
      idx_int2 <- grep("t_c:group|group.*:t_c", names(stats::coef(m2)))
      idx_all2 <- union(grep("^group", names(stats::coef(m2))), idx_int2)
      w_u <- wald_subset_test(m1, idx_g1, V1)
      w_nu <- if (const_truth) list(stat = NA_real_, df = 0, p = NA_real_) else wald_subset_test(m2, idx_int2, V2)
      w_tot <- wald_subset_test(m2, idx_all2, V2)
      stat_type <- "Wald_Chisq"
      stats_vec <- c(w_u$stat, w_nu$stat, w_tot$stat)
      df_vec <- c(w_u$df, w_nu$df, w_tot$df)
      p_vec <- c(w_u$p, w_nu$p, w_tot$p)
    }
  }

  dr2_vec <- c(dr2_u, dr2_nu, dr2_tot)
  tests_tbl <- tibble::tibble(
    test = c("uniform", "nonuniform", "omnibus"),
    stat_type = rep(stat_type, 3L),
    statistic = as.numeric(stats_vec),
    df = as.numeric(df_vec),
    p_value = as.numeric(p_vec),
    delta_r2 = dr2_vec,
    cohens_f2 = c(f2_u, f2_nu, f2_tot),
    magnitude = classify_dpf_magnitude(dr2_vec),
    flagged = !is.na(p_vec) & (p_vec < alpha)
  )

  # Subgroup bias & slope diagnostics from Model 2 (evaluated at t_c = 0, i.e. mean truth)
  cf2 <- stats::coef(m2)
  z_crit <- stats::qnorm(1 - alpha / 2)
  sub_rows <- lapply(g_levels, function(gl) {
    ii <- which(as.character(group) == gl)
    y_g <- y[ii]
    tr_g <- truth[ii]

    if (identical(gl, g_levels[1L])) {
      cond_bias <- 0
      bias_se <- 0
      slope <- if ("t_c" %in% names(cf2)) unname(cf2["t_c"]) else NA_real_
      slope_se <- if ("t_c" %in% rownames(V2)) sqrt(max(V2["t_c", "t_c"], 0)) else NA_real_
    } else {
      g_term <- paste0("group", gl)
      int_term1 <- paste0("t_c:group", gl)
      int_term2 <- paste0("group", gl, ":t_c")
      int_term <- if (int_term1 %in% names(cf2)) int_term1 else int_term2

      cond_bias <- if (g_term %in% names(cf2)) unname(cf2[g_term]) else NA_real_
      bias_se <- if (g_term %in% rownames(V2)) sqrt(max(V2[g_term, g_term], 0)) else NA_real_

      if ("t_c" %in% names(cf2) && int_term %in% names(cf2) && !is.na(cf2[int_term])) {
        slope <- unname(cf2["t_c"] + cf2[int_term])
        idx_s <- c("t_c", int_term)
        slope_se <- if (all(idx_s %in% rownames(V2))) sqrt(max(sum(V2[idx_s, idx_s]), 0)) else NA_real_
      } else if ("t_c" %in% names(cf2)) {
        slope <- unname(cf2["t_c"])
        slope_se <- if ("t_c" %in% rownames(V2)) sqrt(max(V2["t_c", "t_c"], 0)) else NA_real_
      } else {
        slope <- NA_real_
        slope_se <- NA_real_
      }
    }

    r_g <- if (length(ii) >= 3L && !is_constant_numeric(y_g) && !is_constant_numeric(tr_g)) {
      stats::cor(y_g, tr_g)
    } else {
      NA_real_
    }

    tibble::tibble(
      group = gl,
      is_reference = identical(gl, g_levels[1L]),
      n = length(ii),
      mean_llm = mean(y_g),
      mean_truth = mean(tr_g),
      raw_bias = mean(y_g - tr_g),
      cond_bias = cond_bias,
      cond_bias_se = bias_se,
      conf_low = cond_bias - z_crit * bias_se,
      conf_high = cond_bias + z_crit * bias_se,
      cond_slope = slope,
      cond_slope_se = slope_se,
      rmse = sqrt(mean((y_g - tr_g)^2)),
      pearson_r = r_g
    )
  })
  subgroup_tbl <- do.call(rbind, sub_rows)

  list(
    tests = tests_tbl,
    subgroup_summary = subgroup_tbl,
    models = list(null = m0, uniform = m1, nonuniform = m2),
    degenerate = FALSE
  )
}

#' Test Prompt x Group interaction controlling for human truth
#' @noRd
fit_prompt_group_interaction <- function(y, truth, group, prompt, cluster, family, alpha) {
  work <- data.frame(y = y, t_c = truth - mean(truth), group = group, prompt = prompt)
  if (family == "gaussian") {
    m_add <- stats::lm(y ~ t_c + group + prompt, data = work)
    m_int <- stats::lm(y ~ t_c + group * prompt, data = work)
    dr2 <- max(summary(m_int)$r.squared - summary(m_add)$r.squared, 0)
    if (is.null(cluster)) {
      anv <- stats::anova(m_add, m_int)
      stat <- as.numeric(anv$F[2L])
      df <- as.numeric(anv$Df[2L])
      pval <- as.numeric(anv$`Pr(>F)`[2L])
      stype <- "F"
    } else {
      V <- vcov_cluster_fit(m_int, cluster)
      idx <- grep(":", names(stats::coef(m_int)), fixed = TRUE)
      w <- wald_subset_test(m_int, idx, V)
      stat <- w$stat
      df <- w$df
      pval <- w$p
      stype <- "Wald_Chisq"
    }
  } else {
    m_add <- suppressWarnings(stats::glm(y ~ t_c + group + prompt, data = work, family = stats::binomial()))
    m_int <- suppressWarnings(stats::glm(y ~ t_c + group * prompt, data = work, family = stats::binomial()))
    dr2 <- max(nagelkerke_r2(m_int) - nagelkerke_r2(m_add), 0)
    if (is.null(cluster)) {
      anv <- stats::anova(m_add, m_int, test = "Chisq")
      stat <- as.numeric(anv$Deviance[2L])
      df <- as.numeric(anv$Df[2L])
      pval <- as.numeric(anv$`Pr(>Chi)`[2L])
      stype <- "LRT_Chisq"
    } else {
      V <- vcov_cluster_fit(m_int, cluster)
      idx <- grep(":", names(stats::coef(m_int)), fixed = TRUE)
      w <- wald_subset_test(m_int, idx, V)
      stat <- w$stat
      df <- w$df
      pval <- w$p
      stype <- "Wald_Chisq"
    }
  }

  tibble::tibble(
    test = "prompt_x_group",
    stat_type = stype,
    statistic = stat,
    df = df,
    p_value = pval,
    delta_r2 = dr2,
    flagged = !is.na(pval) & (pval < alpha)
  )
}

#' @export
print.llm_dpf <- function(x, ...) {
  cli::cli_h3("Differential Prompt Functioning (DPF) Diagnostics")
  cli::cli_text(
    "Family: {x$family} | Reference group: {.val {x$reference_group}} | Clustered: {x$clustered} | DPF detected: {x$dpf_detected}"
  )
  disp <- tibble::tibble(
    test = x$tests$test,
    statistic = round(x$tests$statistic, 3L),
    df = x$tests$df,
    p_value = fmt_pval(x$tests$p_value),
    delta_r2 = round(x$tests$delta_r2, 4L),
    magnitude = x$tests$magnitude,
    flagged = x$tests$flagged
  )
  print(disp, n = nrow(disp))
  if (!is.null(x$prompt_dpf)) {
    cli::cli_text("Per-prompt DPF screening ({nrow(x$prompt_dpf)} prompts):")
    print(x$prompt_dpf[, c("prompt", "n", "omnibus_p", "omnibus_p_adj", "delta_r2_total", "flagged")], n = nrow(x$prompt_dpf))
  }
  invisible(x)
}
