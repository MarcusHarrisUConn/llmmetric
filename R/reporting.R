#' Format Publication-Ready APA Comparison Tables
#'
#' Produces clean APA 7th-edition formatted summary tables for `llmmetric`
#' results. For any Prediction-Powered Inference fit (`ppi_mean()`, `ppi_lm()`,
#' `ppi_glm()`, `ppi_clustered()`, `ppi_lmer()`, `ppi_proportion()`), contrasts
#' **Naive LLM**, **Human-Only**, and **PPI / PPI++** point estimates,
#' standard errors, confidence intervals, APA-formatted p-values (no leading
#' zero, `< .001`), and effective sample-size / variance gain vs. Human-Only.
#' Also supports [g_study()], [dpf_test()], and [human_irr()] objects.
#'
#' @param object An `"llm_ppi"`, `"llm_g_study"`, `"llm_dpf"`, or
#'   `"llm_human_irr"` object.
#' @param digits Number of decimal places for numeric estimates and CIs.
#'   Default `3L`.
#' @param format Output format: `"markdown"` (default, GitHub-Flavored Markdown
#'   table ready for Quarto/RMarkdown), `"tibble"` (tidy character/numeric
#'   tibble), or `"latex"` (booktabs-compatible LaTeX tabular).
#' @param terms Optional character vector restricting displayed terms (for
#'   `"llm_ppi"` objects).
#' @param include_intercept Logical; whether to retain `"(Intercept)"` in
#'   regression tables. Default `TRUE`.
#' @param caption Optional character table caption.
#' @param note Optional APA table note appended below Markdown/LaTeX output.
#'
#' @return An S3 object of class `"llm_apa_table"` with components:
#'   - `table`: tidy [tibble::tibble()] of formatted table cells.
#'   - `formatted`: rendered Markdown or LaTeX character string (or the tibble
#'     when `format = "tibble"`).
#'   - `format`, `caption`, `note`.
#' @export
#'
#' @examples
#' data(ema_coded)
#' agg <- aggregate_scores(ema_coded, score_col = "llm_score")
#' fit <- ppi_lm(human_score ~ x + group, data = agg, pred_col = "llm_score")
#' tab <- format_apa_table(fit)
#' tab
format_apa_table <- function(object,
                             digits = 3L,
                             format = c("markdown", "tibble", "latex"),
                             terms = NULL,
                             include_intercept = TRUE,
                             caption = NULL,
                             note = NULL) {
  format <- match.arg(format)
  assert_scalar_numeric(digits, "digits", min = 1, max = 6)
  digits <- as.integer(round(digits))
  assert_flag(include_intercept, "include_intercept")
  assert_scalar_character(caption, "caption", allow_null = TRUE)
  assert_scalar_character(note, "note", allow_null = TRUE)

  if (inherits(object, "llm_ppi")) {
    built <- build_apa_ppi(
      object, digits = digits, terms = terms,
      include_intercept = include_intercept, caption = caption, note = note
    )
  } else if (inherits(object, "llm_g_study")) {
    built <- build_apa_g_study(object, digits = digits, caption = caption, note = note)
  } else if (inherits(object, "llm_dpf")) {
    built <- build_apa_dpf(object, digits = digits, caption = caption, note = note)
  } else if (inherits(object, "llm_human_irr")) {
    built <- build_apa_irr(object, digits = digits, caption = caption, note = note)
  } else {
    cli::cli_abort(
      "{.arg object} must inherit from {.cls llm_ppi}, {.cls llm_g_study}, {.cls llm_dpf}, or {.cls llm_human_irr}."
    )
  }

  tbl <- built$table
  cap <- built$caption
  nt <- built$note

  rendered <- if (format == "tibble") {
    tbl
  } else if (format == "markdown") {
    render_apa_markdown(tbl, caption = cap, note = nt)
  } else {
    render_apa_latex(tbl, caption = cap, note = nt)
  }

  structure(
    list(
      table = tbl,
      formatted = rendered,
      format = format,
      caption = cap,
      note = nt
    ),
    class = "llm_apa_table"
  )
}

#' Format APA p-value (no leading zero, `< .001`)
#' @noRd
fmt_apa_p <- function(p) {
  vapply(p, function(val) {
    if (is.na(val)) return("--")
    if (val < 0.001) return("< .001")
    if (val >= 0.9995) return("> .999")
    sub("^0\\.", ".", formatC(val, format = "f", digits = 3L))
  }, character(1L))
}

#' Format APA numeric cell
#' @noRd
fmt_apa_num <- function(x, digits = 3L) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
}

#' Format APA CI bracket string
#' @noRd
fmt_apa_ci <- function(lo, hi, digits = 3L) {
  ifelse(
    is.na(lo) | is.na(hi),
    "--",
    paste0("[", formatC(lo, format = "f", digits = digits), ", ", formatC(hi, format = "f", digits = digits), "]")
  )
}

#' Build APA table for llm_ppi fits
#' @noRd
build_apa_ppi <- function(obj, digits, terms, include_intercept, caption, note) {
  b <- obj$benchmarks
  if (!is.null(terms)) {
    b <- b[b$term %in% terms, , drop = FALSE]
  } else if (!isTRUE(include_intercept) && length(unique(b$term)) > 1L) {
    b <- b[b$term != "(Intercept)", , drop = FALSE]
  }
  if (nrow(b) == 0L) {
    cli::cli_abort("No model terms selected for {.fn format_apa_table}.")
  }

  primary_method <- obj$method
  has_uncal <- "ppi++_uncalibrated" %in% b$method
  if (has_uncal) {
    method_order <- c("naive_llm", "human_only", "ppi++_uncalibrated", primary_method)
    pretty_map <- stats::setNames(
      c("Naive LLM", "Human-Only", "PPI++ (Uncal.)", "PPI++ (Calibrated)"),
      method_order
    )
  } else {
    method_order <- c("naive_llm", "human_only", primary_method)
    pretty_map <- stats::setNames(
      c("Naive LLM", "Human-Only", toupper(primary_method)),
      method_order
    )
  }

  # Order grouped by term, then Naive -> Human-Only -> PPI++
  term_levels <- unique(b$term)
  b$term_ord <- match(b$term, term_levels)
  b$meth_ord <- match(b$method, method_order)
  b <- b[order(b$term_ord, b$meth_ord), , drop = FALSE]

  # Map efficiency gain to primary PPI rows
  eff_lookup <- stats::setNames(obj$efficiency$var_ratio_vs_human, obj$efficiency$term)
  eff_col <- ifelse(
    b$method == primary_method,
    ifelse(is.na(eff_lookup[b$term]), "--", paste0(formatC(eff_lookup[b$term], format = "f", digits = 2L), "x")),
    ifelse(b$method == "human_only", "1.00x", "--")
  )

  ci_pct <- paste0(round(100 * obj$conf_level), "% CI")
  tbl <- tibble::tibble(
    Term = b$term,
    Estimator = unname(pretty_map[b$method]),
    Estimate = fmt_apa_num(b$estimate, digits),
    SE = fmt_apa_num(b$std_error, digits),
    CI = fmt_apa_ci(b$conf_low, b$conf_high, digits),
    p = fmt_apa_p(b$p_value),
    `Eff. Gain` = unname(eff_col)
  )
  names(tbl)[names(tbl) == "CI"] <- ci_pct

  lam_str <- if (length(obj$lambda) == 1L) {
    fmt_apa_num(obj$lambda, 3L)
  } else {
    paste(fmt_apa_num(obj$lambda, 2L), collapse = ", ")
  }

  default_cap <- paste0(
    "Comparison of Naive LLM, Human-Only, and ", toupper(primary_method), " Parameter Estimates"
  )
  default_note <- paste0(
    "Labeled validation n = ", obj$n_labeled,
    "; unlabeled LLM N = ", obj$n_unlabeled,
    "; power-tuning lambda = ", lam_str,
    ". Eff. Gain denotes variance reduction (effective sample-size multiplier) relative to Human-Only."
  )

  list(table = tbl, caption = caption %||% default_cap, note = note %||% default_note)
}

#' Build APA table for llm_g_study
#' @noRd
build_apa_g_study <- function(obj, digits, caption, note) {
  vc <- obj$variance_components
  role_map <- c(
    universe = "Universe Score",
    relative_and_absolute = "Relative & Absolute Error",
    absolute_only = "Absolute Error Only"
  )
  tbl <- tibble::tibble(
    Component = vc$component,
    Variance = fmt_apa_num(vc$variance, digits),
    SD = fmt_apa_num(vc$sd, digits),
    `% Total` = fmt_apa_num(vc$pct_total, 1L),
    Role = unname(role_map[vc$role])
  )
  default_cap <- "Generalizability Study Variance Component Decomposition"
  default_note <- paste0(
    "Units = ", obj$n_units, "; G (Erho^2) = ", fmt_apa_num(obj$g_coef, digits),
    "; Phi = ", fmt_apa_num(obj$phi_coef, digits),
    "; estimation method = ", toupper(obj$method_used), "."
  )
  list(table = tbl, caption = caption %||% default_cap, note = note %||% default_note)
}

#' Build APA table for llm_dpf
#' @noRd
build_apa_dpf <- function(obj, digits, caption, note) {
  t_df <- obj$tests
  tbl <- tibble::tibble(
    Test = c("Uniform DPF", "Non-Uniform DPF", "Omnibus DPF")[match(t_df$test, c("uniform", "nonuniform", "omnibus"))],
    Statistic = fmt_apa_num(t_df$statistic, digits),
    df = fmt_apa_num(t_df$df, 0L),
    p = fmt_apa_p(t_df$p_value),
    `Delta R^2` = fmt_apa_num(t_df$delta_r2, digits),
    Magnitude = ifelse(is.na(t_df$magnitude), "--", t_df$magnitude),
    Flagged = ifelse(t_df$flagged, "Yes", "No")
  )
  default_cap <- "Differential Prompt Functioning (DPF) Measurement Invariance Tests"
  default_note <- paste0(
    "Family = ", obj$family, "; reference group = ", obj$reference_group,
    "; unit-clustered inference = ", obj$clustered, "; alpha = ", obj$alpha, "."
  )
  list(table = tbl, caption = caption %||% default_cap, note = note %||% default_note)
}

#' Build APA table for llm_human_irr
#' @noRd
build_apa_irr <- function(obj, digits, caption, note) {
  if (!is.null(obj$benchmark)) {
    bm <- obj$benchmark
    tbl <- tibble::tibble(
      Metric = bm$metric,
      `Human Ceiling` = fmt_apa_num(bm$human_ceiling, digits),
      `LLM vs. Human` = fmt_apa_num(bm$llm_vs_human, digits),
      Gap = fmt_apa_num(bm$gap, digits),
      `% of Ceiling` = fmt_apa_num(bm$pct_of_ceiling, 1L)
    )
    default_cap <- "LLM Calibration Relative to Human Inter-Rater Reliability Ceiling"
  } else {
    m <- obj$metrics
    tbl <- tibble::tibble(
      Metric = names(m),
      Value = vapply(m, function(col) fmt_apa_num(as.numeric(col)[1L], digits), character(1L))
    )
    default_cap <- "Human Inter-Rater Reliability Summary"
  }
  default_note <- paste0(
    "Units = ", obj$n_units, "; human raters = ", obj$n_raters,
    "; scale = ", obj$type, "; method = ", obj$method_used, "."
  )
  list(table = tbl, caption = caption %||% default_cap, note = note %||% default_note)
}

#' Render Markdown APA table
#' @noRd
render_apa_markdown <- function(tbl, caption = NULL, note = NULL) {
  cols <- names(tbl)
  mat <- as.matrix(tbl)
  lines <- character()
  if (!is.null(caption) && nzchar(caption)) {
    lines <- c(lines, paste0("**", caption, "**"), "")
  }
  lines <- c(
    lines,
    paste0("| ", paste(cols, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  )
  for (i in seq_len(nrow(mat))) {
    lines <- c(lines, paste0("| ", paste(mat[i, ], collapse = " | "), " |"))
  }
  if (!is.null(note) && nzchar(note)) {
    lines <- c(lines, "", paste0("*Note.* ", note))
  }
  paste(lines, collapse = "\n")
}

#' Escape LaTeX special characters cleanly
#' @noRd
escape_latex_cell <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("$", "\\$", x, fixed = TRUE)
  x <- gsub("#", "\\#", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
  x
}

#' Render LaTeX booktabs APA table
#' @noRd
render_apa_latex <- function(tbl, caption = NULL, note = NULL) {
  cols <- escape_latex_cell(names(tbl))
  mat <- apply(as.matrix(tbl), c(1L, 2L), escape_latex_cell)
  align <- paste(c("l", rep("r", ncol(tbl) - 1L)), collapse = "")
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering"
  )
  if (!is.null(caption) && nzchar(caption)) {
    lines <- c(lines, paste0("\\caption{", escape_latex_cell(caption), "}"))
  }
  lines <- c(
    lines,
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste0(paste(cols, collapse = " & "), " \\\\"),
    "\\midrule"
  )
  for (i in seq_len(nrow(mat))) {
    lines <- c(lines, paste0(paste(mat[i, ], collapse = " & "), " \\\\"))
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  if (!is.null(note) && nzchar(note)) {
    lines <- c(lines, paste0("\\par\\smallskip\\footnotesize\\textit{Note.} ", escape_latex_cell(note)))
  }
  lines <- c(lines, "\\end{table}")
  paste(lines, collapse = "\n")
}

#' @export
as.character.llm_apa_table <- function(x, ...) {
  if (is.character(x$formatted)) x$formatted else render_apa_markdown(x$table, x$caption, x$note)
}

#' @export
print.llm_apa_table <- function(x, ...) {
  if (is.character(x$formatted)) {
    cat(x$formatted, "\n", sep = "")
  } else {
    print(x$table)
  }
  invisible(x)
}
