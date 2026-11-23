# Publication-ready ggplot2::autoplot() S3 methods for llmmetric objects

#' Internal publication theme for llmmetric plots
#' @noRd
theme_llmmetric <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      plot.subtitle = ggplot2::element_text(color = "#4B5563", size = base_size - 1),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' Colorblind-safe Okabe-Ito inspired palette
#' @noRd
llmmetric_colors <- function() {
  c(
    blue = "#0072B2",
    orange = "#D55E00",
    teal = "#009E73",
    purple = "#CC79A7",
    amber = "#E69F00",
    sky = "#56B4E9",
    slate = "#4B5563"
  )
}

#' Plot Generalizability Study Variance Decomposition
#'
#' Visualizes estimated G-study variance components as percentages of total
#' score variance, colored by their psychometric error role (Universe Score,
#' Relative & Absolute Error, or Absolute Error Only).
#'
#' @param object An `"llm_g_study"` object returned by [g_study()].
#' @param ... Unused; included for S3 consistency.
#'
#' @return A `ggplot2::ggplot` object.
#' @export
#'
#' @examples
#' data(ema_coded)
#' g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
#' autoplot(g)
autoplot.llm_g_study <- function(object, ...) {
  pal <- llmmetric_colors()
  df <- object$variance_components
  role_labels <- c(
    universe = "Universe Score (Signal)",
    relative_and_absolute = "Relative & Absolute Error",
    absolute_only = "Absolute Error Only"
  )
  df$role_label <- factor(
    role_labels[df$role],
    levels = unname(role_labels)
  )
  df$component_f <- factor(df$component, levels = rev(df$component[order(-df$pct_total)]))
  df$label_txt <- paste0(fmt_num(df$pct_total, 1L), "% (SD=", fmt_num(df$sd, 2L), ")")

  max_pct <- max(df$pct_total, 1)

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$pct_total, y = .data$component_f, fill = .data$role_label)
  ) +
    ggplot2::geom_col(width = 0.68, alpha = 0.92) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label_txt),
      hjust = -0.08,
      size = 3.2,
      color = "#1F2937"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Universe Score (Signal)" = unname(pal["teal"]),
        "Relative & Absolute Error" = unname(pal["orange"]),
        "Absolute Error Only" = unname(pal["amber"])
      ),
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, min(max_pct * 1.32, 118)),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(
      title = "Generalizability Study Variance Decomposition",
      subtitle = paste0(
        "Observed design: G (Erho^2) = ", fmt_num(object$g_coef),
        " | Phi = ", fmt_num(object$phi_coef),
        " | Method: ", toupper(object$method_used)
      ),
      x = "Share of Total Variance (%)",
      y = "Variance Component",
      fill = "Psychometric Role"
    ) +
    theme_llmmetric()
}

#' Plot Decision Study (D-Study) Reliability Curves
#'
#' Visualizes projected Generalizability (\eqn{E\rho^2}) or Dependability
#' (\eqn{\Phi}) curves across candidate facet counts, marking the target
#' reliability threshold and highlighting the lowest-cost recommended design.
#'
#' @param object An `"llm_d_study"` object returned by [d_study()].
#' @param metric Optional override: `"g"`, `"phi"`, or `"both"`. Defaults to
#'   `object$metric`.
#' @param ... Unused; included for S3 consistency.
#'
#' @return A `ggplot2::ggplot` object.
#' @export
#'
#' @examples
#' data(ema_coded)
#' g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
#' d <- d_study(g, grid = list(prompt = 1:8, model = 1:4), target = 0.85)
#' autoplot(d)
autoplot.llm_d_study <- function(object, metric = NULL, ...) {
  pal <- llmmetric_colors()
  metric <- metric %||% object$metric
  metric <- match.arg(metric, c("g", "phi", "both"))

  facets <- object$facets
  x_facet <- facets[1L]
  grid_df <- object$grid

  if (metric == "both") {
    df_g <- grid_df
    df_g$reliability <- df_g$g_coef
    df_g$metric_type <- "G (Relative)"
    df_p <- grid_df
    df_p$reliability <- df_p$phi_coef
    df_p$metric_type <- "Phi (Absolute)"
    plot_df <- rbind(df_g, df_p)
    y_lab <- "Projected Reliability"
  } else {
    plot_df <- grid_df
    plot_df$reliability <- if (metric == "g") plot_df$g_coef else plot_df$phi_coef
    y_lab <- if (metric == "g") "Generalizability Coefficient (G)" else "Dependability Index (Phi)"
  }

  plot_df$x_val <- plot_df[[x_facet]]

  if (length(facets) >= 2L) {
    c_facet <- facets[2L]
    plot_df$group_val <- factor(paste0(c_facet, " = ", plot_df[[c_facet]]),
                                levels = paste0(c_facet, " = ", sort(unique(plot_df[[c_facet]]))))
    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = .data$x_val, y = .data$reliability, color = .data$group_val, group = .data$group_val)
    ) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_point(size = 2.1) +
      ggplot2::labs(color = paste0("Number of ", c_facet, "s"))
  } else {
    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = .data$x_val, y = .data$reliability)
    ) +
      ggplot2::geom_line(linewidth = 1.0, color = unname(pal["blue"])) +
      ggplot2::geom_point(size = 2.2, color = unname(pal["blue"]))
  }

  if (metric == "both") {
    p <- p + ggplot2::facet_wrap(~metric_type)
  }

  p <- p +
    ggplot2::geom_hline(
      yintercept = object$target,
      linetype = "dashed",
      linewidth = 0.7,
      color = unname(pal["orange"])
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1.02)) +
    ggplot2::labs(
      title = "Decision Study (D-Study) Reliability Projections",
      subtitle = paste0("Dashed line indicates target reliability = ", fmt_num(object$target, 2L)),
      x = paste0("Number of ", x_facet, "s"),
      y = y_lab
    ) +
    theme_llmmetric()

  # Highlight lowest-cost recommended design if present
  if (metric != "both" && nrow(object$recommendations) > 0L) {
    best <- object$recommendations[1L, , drop = FALSE]
    best$x_val <- best[[x_facet]]
    best$reliability <- if (metric == "g") best$g_coef else best$phi_coef
    p <- p +
      ggplot2::geom_point(
        data = best,
        mapping = ggplot2::aes(x = .data$x_val, y = .data$reliability),
        inherit.aes = FALSE,
        shape = 21,
        size = 4.5,
        stroke = 1.2,
        fill = "white",
        color = unname(pal["orange"])
      )
  }

  p
}

#' Plot LLM-vs-Human Calibration Diagnostics
#'
#' For continuous ratings, plots raw pairs, quantile-binned calibration means
#' (\eqn{\pm 1.96\,\text{SE}}), linear calibration fit, and the ideal 45-degree
#' identity line. For categorical/ordinal ratings, renders an annotated
#' confusion matrix heatmap.
#'
#' @param object An `"llm_calibration"` object returned by
#'   [calibration_metrics()].
#' @param ... Unused; included for S3 consistency.
#'
#' @return A `ggplot2::ggplot` object.
#' @export
#'
#' @examples
#' data(ema_coded)
#' agg <- aggregate_scores(ema_coded, score_col = "llm_score")
#' cal <- calibration_metrics(agg, pred_col = "llm_score", truth_col = "human_score")
#' autoplot(cal)
autoplot.llm_calibration <- function(object, ...) {
  pal <- llmmetric_colors()
  m <- object$metrics

  if (object$type == "continuous") {
    raw_df <- tibble::tibble(
      pred = as.numeric(object$data[[object$pred_col]]),
      truth = as.numeric(object$data[[object$truth_col]])
    )
    bins <- object$calibration_bins
    bins$ci_lo <- bins$truth_mean - 1.96 * bins$truth_se
    bins$ci_hi <- bins$truth_mean + 1.96 * bins$truth_se

    return(
      ggplot2::ggplot(raw_df, ggplot2::aes(x = .data$pred, y = .data$truth)) +
        ggplot2::geom_point(alpha = 0.28, size = 1.7, color = unname(pal["slate"])) +
        ggplot2::geom_abline(
          slope = 1,
          intercept = 0,
          linetype = "dashed",
          linewidth = 0.8,
          color = "#6B7280"
        ) +
        ggplot2::geom_smooth(
          method = "lm",
          formula = y ~ x,
          se = TRUE,
          linewidth = 0.9,
          color = unname(pal["blue"]),
          fill = unname(pal["sky"]),
          alpha = 0.22
        ) +
        ggplot2::geom_errorbar(
          data = bins,
          mapping = ggplot2::aes(x = .data$pred_mean, ymin = .data$ci_lo, ymax = .data$ci_hi),
          inherit.aes = FALSE,
          width = 0,
          linewidth = 0.7,
          color = unname(pal["orange"])
        ) +
        ggplot2::geom_point(
          data = bins,
          mapping = ggplot2::aes(x = .data$pred_mean, y = .data$truth_mean),
          inherit.aes = FALSE,
          size = 2.6,
          color = unname(pal["orange"])
        ) +
        ggplot2::labs(
          title = "LLM Calibration Curve vs. Human Gold Standard",
          subtitle = paste0(
            "r = ", fmt_num(m$pearson_r),
            " | ICC(A,1) = ", fmt_num(m$icc_agreement),
            " | RMSE = ", fmt_num(m$rmse),
            " | Bias = ", fmt_num(m$bias),
            " | Slope = ", fmt_num(m$cal_slope)
          ),
          x = paste0("LLM Score (", object$pred_col, ")"),
          y = paste0("Human Gold Standard (", object$truth_col, ")")
        ) +
        theme_llmmetric()
    )
  }

  # Categorical / ordinal confusion matrix heatmap
  cm_df <- as.data.frame(object$confusion, stringsAsFactors = FALSE)
  names(cm_df) <- c("pred", "truth", "count")
  cm_df$prop <- cm_df$count / max(sum(cm_df$count), 1)
  cm_df$label <- paste0(cm_df$count, "\n(", fmt_num(100 * cm_df$prop, 1L), "%)")

  ggplot2::ggplot(cm_df, ggplot2::aes(x = .data$truth, y = .data$pred, fill = .data$count)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 3.3, color = "#111827") +
    ggplot2::scale_fill_gradient(low = "#F3F4F6", high = unname(pal["sky"])) +
    ggplot2::labs(
      title = "LLM vs. Human Confusion Matrix",
      subtitle = paste0(
        "Accuracy = ", fmt_num(m$accuracy),
        " | Cohen's Kappa = ", fmt_num(m$cohen_kappa),
        " | Krippendorff's Alpha = ", fmt_num(m$krippendorff_alpha)
      ),
      x = paste0("Human Gold Standard (", object$truth_col, ")"),
      y = paste0("LLM Score (", object$pred_col, ")"),
      fill = "Count"
    ) +
    theme_llmmetric()
}

#' Plot Temporal or Model-Version Scoring Drift
#'
#' @param object An `"llm_drift"` object returned by [check_drift()].
#' @param type `"diff"` (default: paired mean shift vs. baseline with 95% CIs)
#'   or `"trajectory"` (mean score across versions \eqn{\pm 1.96\,\text{SE}}).
#' @param ... Unused; included for S3 consistency.
#'
#' @return A `ggplot2::ggplot` object.
#' @export
#'
#' @examples
#' set.seed(1)
#' anchor <- tibble::tibble(
#'   unit_id = rep(paste0("u", 1:30), 3),
#'   version = rep(c("v1", "v2", "v3"), each = 30),
#'   base = rep(rnorm(30), 3),
#'   score = base + rep(c(0, 0.05, 0.5), each = 30) + rnorm(90, 0, 0.25)
#' )
#' dr <- check_drift(anchor, score_col = "score", version_col = "version")
#' autoplot(dr)
autoplot.llm_drift <- function(object, type = c("diff", "trajectory"), ...) {
  type <- match.arg(type)
  pal <- llmmetric_colors()
  df <- object$summary
  df$version_f <- factor(df$version, levels = df$version)
  df$status <- ifelse(
    df$is_baseline,
    "Baseline",
    ifelse(df$drift_flag, "Drift Flagged", "Stable")
  )
  df$status <- factor(df$status, levels = c("Baseline", "Stable", "Drift Flagged"))

  col_map <- c(
    "Baseline" = unname(pal["slate"]),
    "Stable" = unname(pal["teal"]),
    "Drift Flagged" = unname(pal["orange"])
  )

  if (type == "diff") {
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$version_f, y = .data$mean_diff, color = .data$status)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#6B7280") +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data$conf_low, ymax = .data$conf_high),
          width = 0.14,
          linewidth = 0.8
        ) +
        ggplot2::geom_point(size = 3.2) +
        ggplot2::scale_color_manual(values = col_map, drop = FALSE) +
        ggplot2::labs(
          title = "LLM Scoring Drift Relative to Baseline",
          subtitle = paste0(
            "Baseline: ", object$baseline,
            " | Overall ANOVA p = ", fmt_pval(object$overall_test$p_value),
            " | Drift detected: ", object$drift_detected
          ),
          x = object$version_col,
          y = "Paired Mean Shift vs. Baseline (95% CI)",
          color = "Status"
        ) +
        theme_llmmetric()
    )
  }

  df$ci_lo <- df$mean_score - 1.96 * df$se_score
  df$ci_hi <- df$mean_score + 1.96 * df$se_score
  base_mean <- df$mean_score[df$is_baseline][1L]

  ggplot2::ggplot(df, ggplot2::aes(x = .data$version_f, y = .data$mean_score, group = 1)) +
    ggplot2::geom_hline(yintercept = base_mean, linetype = "dashed", color = "#6B7280") +
    ggplot2::geom_line(linewidth = 0.8, color = unname(pal["blue"])) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$ci_lo, ymax = .data$ci_hi, color = .data$status),
      width = 0.14,
      linewidth = 0.8
    ) +
    ggplot2::geom_point(ggplot2::aes(color = .data$status), size = 3.2) +
    ggplot2::scale_color_manual(values = col_map, drop = FALSE) +
    ggplot2::labs(
      title = "LLM Anchor-Set Score Trajectory Across Versions",
      subtitle = paste0("Dashed line = baseline (", object$baseline, ") mean"),
      x = object$version_col,
      y = paste0("Mean ", object$score_col, " (95% CI)"),
      color = "Status"
    ) +
    theme_llmmetric()
}

#' Plot Differential Prompt Functioning (DPF) Diagnostics
#'
#' Visualizes subgroup measurement invariance diagnostics:
#' - `"curves"` (default): LLM score vs. human gold-standard curves stratified
#'   by subgroup, revealing uniform parallel shifts and non-uniform slope
#'   differences.
#' - `"bias"`: Forest plot of subgroup conditional bias at the mean gold
#'   standard (95% CIs vs. reference group).
#' - `"prompts"`: Per-prompt DPF effect sizes (\eqn{\Delta R^2}) colored by
#'   FDR-flagged status (requires `prompt_col` in [dpf_test()]).
#'
#' @param object An `"llm_dpf"` object returned by [dpf_test()].
#' @param type Plot type: `"curves"`, `"bias"`, or `"prompts"`.
#' @param ... Unused; included for S3 consistency.
#'
#' @return A `ggplot2::ggplot` object.
#' @export
#'
#' @examples
#' data(ema_coded)
#' agg <- aggregate_scores(ema_coded, score_col = "llm_score")
#' dpf <- dpf_test(agg, score_col = "llm_score", truth_col = "human_score", group_col = "group")
#' autoplot(dpf)
autoplot.llm_dpf <- function(object, type = c("curves", "bias", "prompts"), ...) {
  type <- match.arg(type)
  pal <- llmmetric_colors()

  t_u <- object$tests[object$tests$test == "uniform", , drop = FALSE]
  t_nu <- object$tests[object$tests$test == "nonuniform", , drop = FALSE]
  sub_txt <- paste0(
    "Uniform p = ", fmt_pval(t_u$p_value), " (dR2=", fmt_num(t_u$delta_r2, 3L), ")",
    " | Non-uniform p = ", fmt_pval(t_nu$p_value), " (dR2=", fmt_num(t_nu$delta_r2, 3L), ")"
  )

  if (type == "curves") {
    df <- object$data
    sm_method <- if (object$family == "binomial") "glm" else "lm"
    sm_args <- if (object$family == "binomial") list(family = stats::binomial()) else list()

    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data$truth, y = .data$score, color = .data$group, fill = .data$group)) +
        ggplot2::geom_point(alpha = 0.28, size = 1.7) +
        ggplot2::geom_smooth(
          method = sm_method,
          method.args = sm_args,
          formula = y ~ x,
          se = TRUE,
          linewidth = 1.0,
          alpha = 0.18
        ) +
        ggplot2::labs(
          title = "Differential Prompt Functioning (DPF) by Subgroup",
          subtitle = sub_txt,
          x = paste0("Human Gold Standard (", object$truth_col, ")"),
          y = paste0("LLM Score (", object$score_col, ")"),
          color = object$group_col,
          fill = object$group_col
        ) +
        theme_llmmetric()
    )
  }

  if (type == "bias") {
    sb <- object$subgroup_summary
    sb$group_f <- factor(sb$group, levels = rev(sb$group))
    sb$role <- ifelse(sb$is_reference, "Reference", "Comparison")

    return(
      ggplot2::ggplot(sb, ggplot2::aes(x = .data$cond_bias, y = .data$group_f, color = .data$role)) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#6B7280") +
        ggplot2::geom_errorbar(
          ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
          orientation = "y",
          width = 0.18,
          linewidth = 0.9
        ) +
        ggplot2::geom_point(size = 3.2) +
        ggplot2::scale_color_manual(
          values = c("Reference" = unname(pal["slate"]), "Comparison" = unname(pal["orange"]))
        ) +
        ggplot2::labs(
          title = "Conditional Subgroup Bias (at Mean Gold Standard)",
          subtitle = sub_txt,
          x = paste0("Conditional Shift vs. ", object$reference_group, " (95% CI)"),
          y = object$group_col,
          color = NULL
        ) +
        theme_llmmetric()
    )
  }

  # type == "prompts"
  if (is.null(object$prompt_dpf)) {
    cli::cli_abort("Prompt-level plot requires {.arg prompt_col} to have been supplied to {.fn dpf_test}.")
  }
  pd <- object$prompt_dpf
  pd$prompt_f <- factor(pd$prompt, levels = rev(pd$prompt[order(-pd$delta_r2_total)]))
  pd$status <- ifelse(pd$flagged, "Flagged (FDR < alpha)", "Not Flagged")

  ggplot2::ggplot(pd, ggplot2::aes(x = .data$delta_r2_total, y = .data$prompt_f, fill = .data$status)) +
    ggplot2::geom_col(width = 0.65, alpha = 0.9) +
    ggplot2::scale_fill_manual(
      values = c("Flagged (FDR < alpha)" = unname(pal["orange"]), "Not Flagged" = unname(pal["teal"]))
    ) +
    ggplot2::labs(
      title = "Prompt-Level Differential Prompt Functioning Screening",
      subtitle = "Total DPF effect size (Delta R^2) by prompt template",
      x = "Total DPF Delta R^2",
      y = "Prompt",
      fill = NULL
    ) +
    theme_llmmetric()
}

#' Plot Prediction-Powered Inference vs. Human-Only & Naive LLM Benchmarks
#'
#' Produces a publication-ready forest plot comparing parameter point estimates
#' and confidence intervals across **Naive LLM**, **Human-Only**, and
#' **PPI / PPI++**, optionally overlaying true simulation values.
#'
#' @param object An `"llm_ppi"` object returned by [ppi_mean()], [ppi_lm()],
#'   [ppi_glm()], [ppi_clustered()], or [ppi_lmer()].
#' @param terms Optional character vector restricting displayed model terms.
#' @param include_intercept Logical; whether to display `"(Intercept)"` when
#'   other regression predictors are present. Default `FALSE`.
#' @param true_values Optional named numeric vector of ground-truth parameter
#'   values to overlay as vertical dashed reference lines (useful for
#'   simulation diagnostics).
#' @param ... Unused; included for S3 consistency.
#'
#' @return A `ggplot2::ggplot` object.
#' @export
#'
#' @examples
#' set.seed(123)
#' n_tot <- 500
#' x <- rnorm(n_tot)
#' y_true <- 1 + 1.5 * x + rnorm(n_tot)
#' y_llm <- 0.5 + 0.8 * y_true + 0.5 * x + rnorm(n_tot, sd = 0.5)
#' df <- tibble::tibble(
#'   x = x,
#'   human_y = c(y_true[1:100], rep(NA_real_, n_tot - 100)),
#'   llm_y = y_llm
#' )
#' fit <- ppi_lm(human_y ~ x, data = df, pred_col = "llm_y")
#' autoplot(fit)
autoplot.llm_ppi <- function(object,
                             terms = NULL,
                             include_intercept = FALSE,
                             true_values = NULL,
                             ...) {
  pal <- llmmetric_colors()
  b <- object$benchmarks

  if (!is.null(terms)) {
    b <- b[b$term %in% terms, , drop = FALSE]
  } else if (!isTRUE(include_intercept) && length(unique(b$term)) > 1L) {
    b <- b[b$term != "(Intercept)", , drop = FALSE]
  }

  if (nrow(b) == 0L) {
    cli::cli_abort("No terms selected to plot in {.fn autoplot.llm_ppi}.")
  }

  primary_method <- object$method
  method_levels <- c(primary_method, "human_only", "naive_llm")
  pretty_labels <- stats::setNames(
    c(toupper(primary_method), "Human-Only", "Naive LLM"),
    method_levels
  )

  b$method_f <- factor(pretty_labels[b$method], levels = rev(unname(pretty_labels)))
  b$term_f <- factor(b$term, levels = unique(b$term))

  col_map <- stats::setNames(
    c(unname(pal["teal"]), unname(pal["blue"]), unname(pal["orange"])),
    unname(pretty_labels)
  )

  lam_txt <- if (length(object$lambda) == 1L) {
    fmt_num(object$lambda, 2L)
  } else {
    paste(fmt_num(object$lambda, 2L), collapse = ", ")
  }

  p <- ggplot2::ggplot(
    b,
    ggplot2::aes(x = .data$estimate, y = .data$method_f, color = .data$method_f)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
      orientation = "y",
      width = 0.22,
      linewidth = 0.9
    ) +
    ggplot2::geom_point(size = 3.0) +
    ggplot2::scale_color_manual(values = col_map) +
    ggplot2::facet_wrap(~term_f, scales = "free_x") +
    ggplot2::labs(
      title = "Error-Corrected Inference vs. Naive LLM & Human-Only Benchmarks",
      subtitle = paste0(
        "Labeled n = ", object$n_labeled,
        " | Unlabeled N = ", object$n_unlabeled,
        " | lambda = ", lam_txt,
        " | ", round(100 * object$conf_level), "% CI"
      ),
      x = "Parameter Estimate (with Confidence Interval)",
      y = NULL,
      color = "Estimator"
    ) +
    theme_llmmetric() +
    ggplot2::theme(legend.position = "none")

  if (!is.null(true_values)) {
    if (!is.numeric(true_values) || is.null(names(true_values))) {
      cli::cli_abort("{.arg true_values} must be a named numeric vector.")
    }
    tv_match <- intersect(names(true_values), levels(b$term_f))
    if (length(tv_match) > 0L) {
      tv_df <- tibble::tibble(
        term_f = factor(tv_match, levels = levels(b$term_f)),
        truth = as.numeric(true_values[tv_match])
      )
      p <- p +
        ggplot2::geom_vline(
          data = tv_df,
          mapping = ggplot2::aes(xintercept = .data$truth),
          linetype = "dashed",
          linewidth = 0.75,
          color = "#111827"
        )
    }
  }

  p
}

#' Plot PPI++ Power or Standard Error Curves
#'
#' Visualizes projected PPI++ statistical power (when `effect` was supplied) or
#' standard error reduction across human validation sample sizes `n_labeled`,
#' directly compared against human-only estimation.
#'
#' @param object An `"llm_ppi_power"` object returned by [ppi_power()].
#' @param metric `"auto"` (plots power when `effect` is available, else SE),
#'   `"power"`, or `"se"`.
#' @param ... Unused; included for S3 consistency.
#'
#' @return A `ggplot2::ggplot` object.
#' @export
#'
#' @examples
#' pw <- ppi_power(
#'   n_labeled = seq(40, 200, by = 40),
#'   n_total = 1500,
#'   effect = 0.25,
#'   sd = 1.2,
#'   r = c(0.6, 0.8)
#' )
#' autoplot(pw)
autoplot.llm_ppi_power <- function(object, metric = c("auto", "power", "se"), ...) {
  metric <- match.arg(metric)
  pal <- llmmetric_colors()
  g <- object$grid

  if (metric == "auto") {
    metric <- if (!is.null(object$effect) && !all(is.na(g$power_ppi))) "power" else "se"
  }
  if (metric == "power" && all(is.na(g$power_ppi))) {
    cli::cli_abort("Power plot requires {.arg effect} to have been supplied to {.fn ppi_power}.")
  }

  y_ppi <- if (metric == "power") g$power_ppi else g$se_ppi

  df_ppi <- tibble::tibble(
    n_labeled = g$n_labeled,
    value = y_ppi,
    curve = paste0("PPI++ (r = ", fmt_num(g$r, 2L), ")")
  )
  hum_sub <- g[!duplicated(g$n_labeled), , drop = FALSE]
  df_hum <- tibble::tibble(
    n_labeled = hum_sub$n_labeled,
    value = if (metric == "power") hum_sub$power_human else hum_sub$se_human,
    curve = "Human-Only"
  )
  plot_df <- rbind(df_ppi, df_hum)
  curve_lvls <- c(unique(df_ppi$curve), "Human-Only")
  plot_df$curve <- factor(plot_df$curve, levels = curve_lvls)

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$n_labeled, y = .data$value, color = .data$curve, linetype = .data$curve)
  ) +
    ggplot2::geom_line(linewidth = 0.95) +
    ggplot2::geom_point(size = 2.1) +
    ggplot2::labs(
      title = if (metric == "power") "PPI++ vs. Human-Only Statistical Power" else "PPI++ vs. Human-Only Standard Error",
      subtitle = paste0(
        "sd = ", fmt_num(object$params$sd),
        if (!is.null(object$effect)) paste0(" | effect = ", fmt_num(object$effect), " | alpha = ", object$alpha) else ""
      ),
      x = "Human Validation Sample Size (n_labeled)",
      y = if (metric == "power") "Two-Sided Power" else "Standard Error",
      color = "Design",
      linetype = "Design"
    ) +
    theme_llmmetric()

  if (metric == "power") {
    p <- p +
      ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed", color = unname(pal["slate"]), linewidth = 0.65) +
      ggplot2::scale_y_continuous(limits = c(0, 1.02))
  }

  p
}
