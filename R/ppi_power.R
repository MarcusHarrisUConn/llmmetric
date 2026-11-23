#' Plan Human Validation Sample Size for Prediction-Powered Inference (PPI++)
#'
#' Computes the required number of human-coded validation annotations `n_labeled`
#' to achieve either a target standard error (`target_se`) or target two-sided
#' statistical power (`power` for a given `effect`) under optimal power-tuned
#' PPI++. Uses exact PPI++ asymptotic variance:
#' \deqn{\mathrm{Var}(\hat{\theta}_{\mathrm{PPI++}}) = \frac{\sigma^2}{n}\left(1 - r^2 \frac{N}{n + N}\right),}
#' where \eqn{\sigma} is the single-observation human-only standard error
#' scaling constant (\eqn{\sigma = \mathrm{SE}_{\mathrm{human}}\sqrt{n_0}}),
#' \eqn{r \in [0, 1]} is the correlation between human and LLM estimating
#' scores, and \eqn{N} is the unlabeled LLM-coded sample size.
#'
#' Parameters `(sd, r)` may be supplied directly or automatically extracted from
#' a pilot [ppi_mean()], [ppi_lm()], [ppi_glm()], [ppi_clustered()],
#' [ppi_lmer()], or continuous [calibration_metrics()] object via `pilot`.
#'
#' @param target_se Optional positive target standard error for the focal
#'   parameter. Specify either `target_se` or `effect`.
#' @param effect Optional non-zero target effect size for a two-sided Wald test
#'   at significance level `alpha` and target `power`. Ignored if `target_se` is
#'   supplied.
#' @param power Target statistical power in `(alpha, 1)` when sizing by `effect`.
#'   Default `0.80`.
#' @param sd Positive single-observation SD scaling constant
#'   (\eqn{\mathrm{SE}_{\mathrm{human}}\sqrt{n}}). Inferred from `pilot` if
#'   `NULL`.
#' @param r Correlation magnitude in `[0, 1]` between human and LLM scores (or
#'   estimating equations). Inferred from `pilot` if `NULL`.
#' @param n_total Optional finite total corpus size (`n_labeled + n_unlabeled`).
#'   When supplied, solves `n_labeled` accounting for the irreducible corpus
#'   floor \eqn{\sigma / \sqrt{n_{\mathrm{total}}}}. Mutually exclusive with a
#'   finite `n_unlabeled`.
#' @param n_unlabeled Unlabeled LLM-coded pool size `N` (default `Inf`,
#'   representing a large unlabeled corpus). Ignored if `n_total` is supplied.
#' @param pilot Optional pilot `"llm_ppi"` or continuous `"llm_calibration"`
#'   object from which `sd` and `r` are estimated automatically.
#' @param term Optional parameter term name to extract from a multi-term
#'   `"llm_ppi"` pilot object. Defaults to the first non-intercept term.
#' @param alpha Two-sided Type-I error rate in `(0, 1)`. Default `0.05`.
#' @param min_n Minimum practical validation sample size floor. Default `20L`.
#'
#' @return An S3 object of class `"llm_ppi_sample_size"` containing:
#'   - `feasible`: logical indicating whether the target is attainable given
#'     `n_total`.
#'   - `n_labeled`: required human validation sample size (integer, or `NA` if
#'     infeasible).
#'   - `n_unlabeled`: resulting unlabeled sample size.
#'   - `n_human_only`: human annotations required without PPI++.
#'   - `annotations_saved`, `savings_pct`: human annotation savings from PPI++.
#'   - `target_se`, `achieved_se`, `achieved_power`, `effective_n`,
#'     `min_n_total_needed`, `params`.
#' @export
#'
#' @examples
#' # Size a validation subset out of a 2,000-text corpus for 80% power on beta = 0.25
#' plan <- ppi_sample_size(effect = 0.25, power = 0.80, sd = 1.2, r = 0.82, n_total = 2000)
#' plan
#'
#' # Or size directly from a pilot PPI fit on ema_coded
#' data(ema_coded)
#' agg <- aggregate_scores(ema_coded, score_col = "llm_score")
#' pilot_fit <- ppi_lm(human_score ~ x + group, data = agg, pred_col = "llm_score")
#' ppi_sample_size(target_se = 0.06, n_total = 1500, pilot = pilot_fit, term = "x")
ppi_sample_size <- function(target_se = NULL,
                            effect = NULL,
                            power = 0.80,
                            sd = NULL,
                            r = NULL,
                            n_total = NULL,
                            n_unlabeled = Inf,
                            pilot = NULL,
                            term = NULL,
                            alpha = 0.05,
                            min_n = 20L) {
  assert_scalar_numeric(alpha, "alpha", min = 1e-6, max = 0.5)
  assert_scalar_numeric(power, "power", min = alpha + 1e-6, max = 1 - 1e-6)
  assert_scalar_numeric(min_n, "min_n", min = 5)
  min_n <- as.integer(round(min_n))

  params <- resolve_ppi_planning_params(sd = sd, r = r, pilot = pilot, term = term)
  sigma <- params$sd
  r_val <- params$r

  if (is.null(target_se) && is.null(effect)) {
    cli::cli_abort("Specify either {.arg target_se} or {.arg effect} (with {.arg power}).")
  }
  if (!is.null(target_se)) {
    assert_scalar_numeric(target_se, "target_se", min = 1e-9)
    se_goal <- target_se
    goal_mode <- "target_se"
  } else {
    assert_scalar_numeric(effect, "effect")
    if (abs(effect) <= 1e-12) {
      cli::cli_abort("{.arg effect} must be non-zero.")
    }
    z_a <- stats::qnorm(1 - alpha / 2)
    z_b <- stats::qnorm(power)
    se_goal <- abs(effect) / (z_a + z_b)
    goal_mode <- "power"
  }

  v_goal <- se_goal^2
  n_human_only <- max(as.integer(ceiling((sigma^2) / v_goal)), min_n)
  min_n_total_needed <- as.integer(ceiling((sigma^2) / v_goal))

  if (!is.null(n_total)) {
    assert_scalar_numeric(n_total, "n_total", min = 10)
    M <- as.numeric(round(n_total))
    se_floor <- sigma / sqrt(M)
    if (se_goal < se_floor * (1 - 1e-12)) {
      cli::cli_warn(
        "Target SE ({fmt_num(se_goal, 4L)}) is below the irreducible corpus floor ({fmt_num(se_floor, 4L)}) for n_total = {M}. Increase {.arg n_total} to at least {min_n_total_needed}."
      )
      return(structure(
        list(
          feasible = FALSE,
          goal_mode = goal_mode,
          n_labeled = NA_integer_,
          n_unlabeled = NA_integer_,
          n_total = as.integer(M),
          n_human_only = n_human_only,
          annotations_saved = NA_integer_,
          savings_pct = NA_real_,
          target_se = se_goal,
          target_power = if (goal_mode == "power") power else NA_real_,
          effect = effect %||% NA_real_,
          achieved_se = NA_real_,
          achieved_power = NA_real_,
          effective_n = NA_real_,
          min_n_total_needed = min_n_total_needed,
          params = params,
          alpha = alpha
        ),
        class = "llm_ppi_sample_size"
      ))
    }

    denom <- v_goal - (sigma^2) * (r_val^2) / M
    if (denom <= 1e-15) {
      n_req <- as.integer(M)
    } else {
      n_req <- as.integer(ceiling((sigma^2) * (1 - r_val^2) / denom))
    }
    n_req <- min(max(n_req, min_n), as.integer(M))
    n_unlab_res <- as.integer(M - n_req)
    n_tot_out <- as.integer(M)
  } else {
    if (!is.numeric(n_unlabeled) || length(n_unlabeled) != 1L || is.na(n_unlabeled) || n_unlabeled < 0) {
      cli::cli_abort("{.arg n_unlabeled} must be a non-negative number or {.code Inf}.")
    }
    N <- as.numeric(n_unlabeled)
    if (is.infinite(N)) {
      n_req <- as.integer(ceiling((sigma^2) * (1 - r_val^2) / v_goal))
      n_req <- max(n_req, min_n)
      n_unlab_res <- Inf
      n_tot_out <- Inf
    } else if (N == 0) {
      n_req <- n_human_only
      n_unlab_res <- 0L
      n_tot_out <- n_req
    } else {
      # Exact positive root of v_goal * n^2 + (N*v_goal - sigma^2)*n - sigma^2*N*(1 - r^2) = 0
      b_coef <- N * v_goal - sigma^2
      c_coef <- -(sigma^2) * N * (1 - r_val^2)
      disc <- max(b_coef^2 - 4 * v_goal * c_coef, 0)
      n_exact <- (-b_coef + sqrt(disc)) / (2 * v_goal)
      n_req <- max(as.integer(ceiling(n_exact)), min_n)
      n_unlab_res <- as.integer(round(N))
      n_tot_out <- as.integer(n_req + n_unlab_res)
    }
  }

  calc_se <- function(n_l, n_u) {
    frac <- if (is.infinite(n_u)) 1 else if (n_l + n_u == 0) 0 else n_u / (n_l + n_u)
    sigma * sqrt(max(1 - (r_val^2) * frac, 1e-12) / n_l)
  }
  calc_pow <- function(se_v) {
    if (is.null(effect)) return(NA_real_)
    z_a <- stats::qnorm(1 - alpha / 2)
    stats::pnorm(abs(effect) / se_v - z_a) + stats::pnorm(-abs(effect) / se_v - z_a)
  }

  ach_se <- calc_se(n_req, n_unlab_res)
  ach_pow <- calc_pow(ach_se)
  eff_n <- (sigma / ach_se)^2
  saved <- max(n_human_only - n_req, 0L)
  savings_pct <- if (n_human_only > 0L) 100 * saved / n_human_only else 0

  structure(
    list(
      feasible = TRUE,
      goal_mode = goal_mode,
      n_labeled = n_req,
      n_unlabeled = n_unlab_res,
      n_total = n_tot_out,
      n_human_only = n_human_only,
      annotations_saved = saved,
      savings_pct = savings_pct,
      target_se = se_goal,
      target_power = if (goal_mode == "power") power else NA_real_,
      effect = effect %||% NA_real_,
      achieved_se = ach_se,
      achieved_power = ach_pow,
      effective_n = eff_n,
      min_n_total_needed = min_n_total_needed,
      params = params,
      alpha = alpha
    ),
    class = "llm_ppi_sample_size"
  )
}

#' Compute Projected Precision and Statistical Power for PPI++
#'
#' Evaluates projected PPI++ standard error, effective sample size, variance
#' reduction factor, and (when `effect` is supplied) two-sided statistical power
#' across grids of human validation sizes `n_labeled` and/or LLM-human
#' correlations `r`, directly benchmarked against human-only estimation.
#'
#' @param n_labeled Positive integer vector of candidate human validation sample
#'   sizes.
#' @param n_unlabeled Optional non-negative numeric vector (or `Inf`) of
#'   unlabeled LLM sample sizes. Ignored if `n_total` is supplied. Defaults to
#'   `Inf` when `n_total` is `NULL`.
#' @param n_total Optional finite total corpus size (`n_labeled + n_unlabeled`).
#' @param effect Optional non-zero target effect size for power computation.
#' @param sd Positive single-observation SD scaling constant. Inferred from
#'   `pilot` if `NULL`.
#' @param r Numeric vector of LLM-human score correlations in `[0, 1]`.
#'   Inferred from `pilot` if `NULL`.
#' @param pilot Optional pilot `"llm_ppi"` or continuous `"llm_calibration"`
#'   object from which `sd` and `r` are estimated automatically.
#' @param term Optional parameter term name for multi-term `"llm_ppi"` pilots.
#' @param alpha Two-sided significance level in `(0, 1)`. Default `0.05`.
#'
#' @return An S3 object of class `"llm_ppi_power"` containing `$grid` tibble,
#'   `$params`, `$effect`, `$alpha`, and `autoplot()` support.
#' @export
#'
#' @examples
#' pw <- ppi_power(
#'   n_labeled = seq(40, 240, by = 40),
#'   n_total = 1500,
#'   effect = 0.25,
#'   sd = 1.2,
#'   r = c(0.6, 0.8, 0.9)
#' )
#' pw
ppi_power <- function(n_labeled,
                      n_unlabeled = NULL,
                      n_total = NULL,
                      effect = NULL,
                      sd = NULL,
                      r = NULL,
                      pilot = NULL,
                      term = NULL,
                      alpha = 0.05) {
  if (!is.numeric(n_labeled) || length(n_labeled) < 1L || anyNA(n_labeled) || any(n_labeled < 2)) {
    cli::cli_abort("{.arg n_labeled} must be a numeric vector of sample sizes >= 2.")
  }
  n_labeled <- sort(unique(as.integer(round(n_labeled))))
  assert_scalar_numeric(alpha, "alpha", min = 1e-6, max = 0.5)
  assert_scalar_numeric(effect, "effect", allow_null = TRUE)

  # Allow vector r while extracting sd (and default scalar r) from pilot if provided
  if (is.null(r)) {
    params <- resolve_ppi_planning_params(sd = sd, r = NULL, pilot = pilot, term = term)
    r_vec <- params$r
    sigma <- params$sd
  } else {
    if (!is.numeric(r) || length(r) < 1L || anyNA(r) || any(r < 0 | r > 1)) {
      cli::cli_abort("{.arg r} must contain correlation magnitudes in [0, 1].")
    }
    r_vec <- sort(unique(as.numeric(r)))
    params <- resolve_ppi_planning_params(sd = sd, r = r_vec[1L], pilot = pilot, term = term)
    sigma <- params$sd
  }

  if (!is.null(n_total)) {
    assert_scalar_numeric(n_total, "n_total", min = max(n_labeled))
    M <- as.integer(round(n_total))
    grid_df <- expand.grid(n_labeled = n_labeled, r = r_vec, KEEP.OUT.ATTRS = FALSE)
    grid_df$n_unlabeled <- M - grid_df$n_labeled
    grid_df$n_total <- M
  } else {
    n_u_vec <- if (is.null(n_unlabeled)) Inf else as.numeric(n_unlabeled)
    if (anyNA(n_u_vec) || any(n_u_vec < 0)) {
      cli::cli_abort("{.arg n_unlabeled} must be non-negative or Inf.")
    }
    grid_df <- expand.grid(n_labeled = n_labeled, n_unlabeled = unique(n_u_vec), r = r_vec, KEEP.OUT.ATTRS = FALSE)
    grid_df$n_total <- grid_df$n_labeled + grid_df$n_unlabeled
  }

  frac_u <- ifelse(
    is.infinite(grid_df$n_unlabeled),
    1,
    ifelse(grid_df$n_total > 0, grid_df$n_unlabeled / grid_df$n_total, 0)
  )
  se_human <- sigma / sqrt(grid_df$n_labeled)
  se_ppi <- sigma * sqrt(pmax(1 - (grid_df$r^2) * frac_u, 1e-12) / grid_df$n_labeled)
  var_gain <- (se_human / se_ppi)^2
  eff_n <- grid_df$n_labeled * var_gain

  z_a <- stats::qnorm(1 - alpha / 2)
  if (!is.null(effect)) {
    pow_human <- stats::pnorm(abs(effect) / se_human - z_a) + stats::pnorm(-abs(effect) / se_human - z_a)
    pow_ppi <- stats::pnorm(abs(effect) / se_ppi - z_a) + stats::pnorm(-abs(effect) / se_ppi - z_a)
  } else {
    pow_human <- rep(NA_real_, nrow(grid_df))
    pow_ppi <- rep(NA_real_, nrow(grid_df))
  }

  grid_tbl <- tibble::tibble(
    n_labeled = as.integer(grid_df$n_labeled),
    n_unlabeled = grid_df$n_unlabeled,
    n_total = grid_df$n_total,
    r = grid_df$r,
    se_human = se_human,
    se_ppi = se_ppi,
    var_reduction = var_gain,
    effective_n = eff_n,
    power_human = pow_human,
    power_ppi = pow_ppi
  )

  structure(
    list(
      grid = grid_tbl,
      params = list(sd = sigma, r = r_vec, source = params$source, term = params$term),
      effect = effect,
      alpha = alpha
    ),
    class = "llm_ppi_power"
  )
}

#' Extract planning parameters (sd, r) from explicit values or pilot objects
#' @noRd
resolve_ppi_planning_params <- function(sd = NULL, r = NULL, pilot = NULL, term = NULL) {
  est_sd <- NULL
  est_r <- NULL
  source_lbl <- "supplied"
  used_term <- term

  if (!is.null(pilot)) {
    if (inherits(pilot, "llm_ppi")) {
      source_lbl <- paste0("pilot_", pilot$method)
      terms_avail <- pilot$estimates$term[!is.na(pilot$estimates$std_error)]
      if (length(terms_avail) == 0L) {
        cli::cli_abort("Pilot {.cls llm_ppi} object has no identifiable terms with finite standard errors.")
      }
      if (is.null(used_term)) {
        non_int <- setdiff(terms_avail, "(Intercept)")
        used_term <- if (length(non_int) > 0L) non_int[1L] else terms_avail[1L]
      } else if (!(used_term %in% terms_avail)) {
        cli::cli_abort("Term {.val {used_term}} not found in pilot {.cls llm_ppi} object.")
      }

      row_ppi <- pilot$estimates[pilot$estimates$term == used_term, , drop = FALSE][1L, ]
      row_hum <- pilot$benchmarks[pilot$benchmarks$method == "human_only" & pilot$benchmarks$term == used_term, , drop = FALSE][1L, ]
      n0 <- pilot$n_labeled
      N0 <- pilot$n_unlabeled

      est_sd <- row_hum$std_error * sqrt(n0)
      vr <- (row_hum$std_error^2) / max(row_ppi$std_error^2, 1e-16)
      frac0 <- N0 / (n0 + N0)
      r2_imp <- if (frac0 > 0) (1 - 1 / max(vr, 1)) / frac0 else 0
      est_r <- sqrt(min(max(r2_imp, 0), 0.9999))
    } else if (inherits(pilot, "llm_calibration")) {
      if (!identical(pilot$type, "continuous")) {
        cli::cli_abort("Pilot {.cls llm_calibration} object must be continuous to extract {.arg sd} and {.arg r}.")
      }
      source_lbl <- "pilot_calibration"
      est_sd <- stats::sd(as.numeric(pilot$data[[pilot$truth_col]]), na.rm = TRUE)
      est_r <- abs(pilot$metrics$pearson_r)
      if (is.na(est_r)) est_r <- 0
    } else {
      cli::cli_abort("{.arg pilot} must be an {.cls llm_ppi} or continuous {.cls llm_calibration} object.")
    }
  }

  final_sd <- sd %||% est_sd
  final_r <- r %||% est_r

  if (is.null(final_sd) || is.null(final_r)) {
    cli::cli_abort("Supply both {.arg sd} and {.arg r}, or pass a {.arg pilot} object.")
  }
  assert_scalar_numeric(final_sd, "sd", min = 1e-9)
  assert_scalar_numeric(final_r, "r", min = 0, max = 1)

  list(sd = as.numeric(final_sd), r = as.numeric(final_r), source = source_lbl, term = used_term)
}

#' @export
print.llm_ppi_sample_size <- function(x, ...) {
  cli::cli_h3("PPI++ Human Validation Sample-Size Plan")
  cli::cli_text(
    "Inputs ({x$params$source}): sd = {fmt_num(x$params$sd)} | r = {fmt_num(x$params$r)} | Target SE = {fmt_num(x$target_se, 4L)}"
  )
  if (!isTRUE(x$feasible)) {
    cli::cli_alert_warning(
      "Infeasible at n_total = {x$n_total}; need n_total >= {x$min_n_total_needed}."
    )
    return(invisible(x))
  }
  cli::cli_text(
    "Required human validation n = {x$n_labeled} (vs. {x$n_human_only} human-only -> saves {x$annotations_saved} annotations, {fmt_num(x$savings_pct, 1L)}%)"
  )
  pow_txt <- if (is.na(x$achieved_power)) "" else paste0(" | Power = ", fmt_num(x$achieved_power))
  cli::cli_text(
    "Projected PPI++ SE = {fmt_num(x$achieved_se, 4L)} | Effective n = {fmt_num(x$effective_n, 1L)}{pow_txt}"
  )
  invisible(x)
}

#' @export
print.llm_ppi_power <- function(x, ...) {
  cli::cli_h3("PPI++ Precision & Power Projections")
  eff_txt <- if (is.null(x$effect)) "none" else fmt_num(x$effect)
  cli::cli_text(
    "Source: {x$params$source} | sd = {fmt_num(x$params$sd)} | effect = {eff_txt} | alpha = {x$alpha}"
  )
  print(x$grid, n = min(nrow(x$grid), 12L))
  invisible(x)
}
