#' Decision Study (D-Study) Reliability Projections & Optimal Design Solver
#'
#' Projects Generalizability (\eqn{E\rho^2}) and Dependability (\eqn{\Phi})
#' coefficients across candidate facet counts (e.g., grids of `n_prompts x
#' n_models`), computes single-facet reliability asymptotes, and identifies
#' minimum-cost designs achieving a target reliability threshold.
#'
#' @param g_study An object of class `"llm_g_study"` returned by [g_study()].
#' @param grid Optional named list of positive integer vectors giving candidate
#'   counts for each facet in `g_study$facets` (e.g.
#'   `list(prompt = 1:8, model = 1:4)`). Missing facets default to sensible
#'   candidate sequences.
#' @param target Target reliability threshold in `(0, 1)`. Default `0.85`.
#' @param metric Reliability coefficient used for target recommendations:
#'   `"g"` (relative \eqn{E\rho^2}, default) or `"phi"` (absolute \eqn{\Phi}).
#' @param n_replicates Positive integer replicate count per cell in the D-study
#'   projection. Defaults to `1L`.
#' @param costs Optional named numeric vector of relative cost weights per unit
#'   increase in each facet (e.g. `c(prompt = 1, model = 2)`). If `NULL`, total
#'   LLM calls (`prod(n_facets) * n_replicates`) is minimized.
#'
#' @return An S3 object of class `"llm_d_study"` with elements:
#'   - `grid`: tibble of all evaluated D-study designs with facet counts,
#'     `total_calls`, `cost`, `g_coef`, `phi_coef`, `sem_relative`,
#'     `sem_absolute`, and `meets_target`.
#'   - `recommendations`: tibble of Pareto-minimal / lowest-cost designs
#'     satisfying `metric >= target` (empty tibble if unattainable in `grid`).
#'   - `asymptotes`: tibble giving theoretical ceiling \eqn{E\rho^2} and
#'     \eqn{\Phi} when each facet tends to infinity while other facets stay at 1
#'     or at their observed G-study counts.
#'   - `target`, `metric`, `facets`, `n_replicates`, `g_study`.
#' @export
#'
#' @examples
#' data(ema_coded)
#' g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
#' d <- d_study(g, grid = list(prompt = 1:8, model = 1:4), target = 0.85)
#' d
d_study <- function(g_study,
                    grid = NULL,
                    target = 0.85,
                    metric = c("g", "phi"),
                    n_replicates = 1L,
                    costs = NULL) {
  if (!inherits(g_study, "llm_g_study")) {
    cli::cli_abort("{.arg g_study} must be an {.cls llm_g_study} object.")
  }
  metric <- match.arg(metric)
  assert_scalar_numeric(target, "target", min = 1e-6, max = 1 - 1e-6)
  assert_scalar_numeric(n_replicates, "n_replicates", min = 1)
  n_replicates <- as.integer(round(n_replicates))

  facets <- g_study$facets
  if (is.null(grid)) {
    grid <- lapply(seq_along(facets), function(i) {
      f <- facets[i]
      obs_n <- g_study$n_facets[[f]]
      upper <- if (i == 1L) max(10L, obs_n) else max(5L, obs_n)
      seq_len(upper)
    })
    names(grid) <- facets
  } else {
    if (!is.list(grid) || is.null(names(grid))) {
      cli::cli_abort("{.arg grid} must be a named list keyed by facet names ({.val {facets}}).")
    }
    unknown <- setdiff(names(grid), facets)
    if (length(unknown) > 0L) {
      cli::cli_abort("Unknown facet{?s} in {.arg grid}: {.val {unknown}}.")
    }
    full_grid <- list()
    for (f in facets) {
      if (f %in% names(grid)) {
        vals <- grid[[f]]
        if (!is.numeric(vals) || length(vals) < 1L || anyNA(vals) || any(!is.finite(vals)) || any(vals < 1)) {
          cli::cli_abort("Grid values for facet {.val {f}} must be finite positive numbers.")
        }
        full_grid[[f]] <- sort(unique(as.integer(round(vals))))
      } else {
        full_grid[[f]] <- as.integer(g_study$n_facets[[f]])
      }
    }
    grid <- full_grid
  }

  if (!is.null(costs)) {
    if (!is.numeric(costs) || is.null(names(costs)) || anyNA(costs) || any(!is.finite(costs)) || any(costs <= 0)) {
      cli::cli_abort("{.arg costs} must be a named finite positive numeric vector.")
    }
  }

  design_df <- expand.grid(grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  n_designs <- nrow(design_df)

  g_vec <- numeric(n_designs)
  phi_vec <- numeric(n_designs)
  sem_rel_vec <- numeric(n_designs)
  sem_abs_vec <- numeric(n_designs)
  calls_vec <- numeric(n_designs)
  cost_vec <- numeric(n_designs)

  vc_tbl <- g_study$variance_components

  for (i in seq_len(n_designs)) {
    nf <- stats::setNames(as.numeric(design_df[i, facets]), facets)
    rel <- compute_g_reliability(vc_tbl, n_facets = nf, n_replicates = n_replicates)
    g_vec[i] <- rel$g_coef
    phi_vec[i] <- rel$phi_coef
    sem_rel_vec[i] <- rel$sem_relative
    sem_abs_vec[i] <- rel$sem_absolute
    calls_vec[i] <- prod(nf) * n_replicates
    if (is.null(costs)) {
      cost_vec[i] <- calls_vec[i]
    } else {
      w <- vapply(facets, function(f) if (f %in% names(costs)) costs[[f]] else 1, numeric(1L))
      cost_vec[i] <- sum(w * nf) * n_replicates
    }
  }

  target_vals <- if (metric == "g") g_vec else phi_vec
  grid_tbl <- tibble::as_tibble(design_df)
  grid_tbl$n_replicates <- rep(n_replicates, n_designs)
  grid_tbl$total_calls <- calls_vec
  grid_tbl$cost <- cost_vec
  grid_tbl$g_coef <- g_vec
  grid_tbl$phi_coef <- phi_vec
  grid_tbl$sem_relative <- sem_rel_vec
  grid_tbl$sem_absolute <- sem_abs_vec
  grid_tbl$meets_target <- !is.na(target_vals) & (target_vals >= target)

  # Identify minimal / Pareto-efficient designs achieving target
  ok_tbl <- grid_tbl[grid_tbl$meets_target, , drop = FALSE]
  if (nrow(ok_tbl) > 0L) {
    # Keep Pareto frontier in facet counts (no other satisfying design has <= all facets and strictly < at least one)
    is_minimal <- vapply(seq_len(nrow(ok_tbl)), function(i) {
      row_i <- as.numeric(ok_tbl[i, facets])
      dominated <- any(vapply(seq_len(nrow(ok_tbl)), function(j) {
        if (i == j) return(FALSE)
        row_j <- as.numeric(ok_tbl[j, facets])
        all(row_j <= row_i) && any(row_j < row_i)
      }, logical(1L)))
      !dominated
    }, logical(1L))
    rec_tbl <- ok_tbl[is_minimal, , drop = FALSE]
    ord <- order(rec_tbl$cost, rec_tbl$total_calls, -rec_tbl[[if (metric == "g") "g_coef" else "phi_coef"]])
    rec_tbl <- rec_tbl[ord, , drop = FALSE]
  } else {
    rec_tbl <- ok_tbl
  }

  # Asymptotes: hold all other facets at 1 or at observed counts, send focal facet -> Inf
  asym_rows <- lapply(facets, function(f) {
    nf_one <- stats::setNames(rep(1, length(facets)), facets)
    nf_one[[f]] <- Inf
    r_one <- compute_g_reliability(vc_tbl, n_facets = nf_one, n_replicates = n_replicates)

    nf_obs <- as.numeric(g_study$n_facets)
    names(nf_obs) <- facets
    nf_obs[[f]] <- Inf
    r_obs <- compute_g_reliability(vc_tbl, n_facets = nf_obs, n_replicates = n_replicates)

    tibble::tibble(
      facet_to_inf = f,
      g_ceiling_others_1 = r_one$g_coef,
      phi_ceiling_others_1 = r_one$phi_coef,
      g_ceiling_others_obs = r_obs$g_coef,
      phi_ceiling_others_obs = r_obs$phi_coef
    )
  })
  asymptotes_tbl <- do.call(rbind, asym_rows)

  structure(
    list(
      grid = grid_tbl,
      recommendations = rec_tbl,
      asymptotes = asymptotes_tbl,
      target = target,
      metric = metric,
      facets = facets,
      n_replicates = n_replicates,
      g_study = g_study
    ),
    class = "llm_d_study"
  )
}

#' @export
print.llm_d_study <- function(x, ...) {
  metric_label <- if (x$metric == "g") "G (Erho^2)" else "Phi"
  cli::cli_h3("Decision Study (D-Study) Projections")
  cli::cli_text(
    "Evaluated {nrow(x$grid)} designs | Target: {metric_label} >= {fmt_num(x$target, 2L)}"
  )
  if (nrow(x$recommendations) > 0L) {
    best <- x$recommendations[1L, , drop = FALSE]
    f_str <- paste(paste0(x$facets, "=", as.integer(best[1L, x$facets])), collapse = ", ")
    cli::cli_text(
      "Lowest-cost recommended design: {f_str} ({best$total_calls} calls/unit -> G = {fmt_num(best$g_coef)}, Phi = {fmt_num(best$phi_coef)})"
    )
    print(x$recommendations, n = min(nrow(x$recommendations), 8L))
  } else {
    vals_m <- x$grid[[if (x$metric == "g") "g_coef" else "phi_coef"]]
    max_val <- if (all(is.na(vals_m))) NA_real_ else max(vals_m, na.rm = TRUE)
    cli::cli_alert_warning(
      "No design in the evaluated grid achieved {metric_label} >= {fmt_num(x$target, 2L)} (max achieved: {fmt_num(max_val)})."
    )
  }
  invisible(x)
}
