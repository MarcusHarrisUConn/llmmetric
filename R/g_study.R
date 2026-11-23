#' Generalizability Study (G-Study) for Multi-Facet LLM Coding
#'
#' Estimates crossed random-effects variance components for LLM-coded scores
#' (e.g., `unit`, `prompt`, `model`, `temperature`, interactions, and residual)
#' via REML (`lme4::lmer`) with an exact balanced-ANOVA Expected Mean Squares
#' fallback, and computes relative Generalizability (\eqn{E\rho^2}) and
#' absolute Dependability (\eqn{\Phi}) coefficients.
#'
#' @param data An `llm_coded_data` object produced by [code_texts()] or a
#'   `data.frame` containing unit IDs, facet columns, and numeric scores.
#' @param score_col Name of the numeric score column. Defaults to the primary
#'   score column for `llm_coded_data`, or `"score"` otherwise.
#' @param id_col Name of the object-of-measurement (unit/person/text) column.
#'   Default `"unit_id"`.
#' @param facets Optional character vector of measurement facet column names
#'   (e.g., `c("prompt", "model")`). If `NULL`, automatically detects which of
#'   `c("prompt", "model", "temperature")` exist in `data` with at least 2
#'   distinct levels.
#' @param method Estimation engine: `"auto"` (default: REML via `lme4::lmer`,
#'   falling back cleanly to ANOVA/EMS if REML fails), `"reml"`, or `"anova"`
#'   (Brennan's Expected Mean Squares Method of Moments for balanced crossed
#'   designs).
#' @param max_interaction Maximum random-effect interaction order to estimate
#'   separately from residual error. Default `2L` (main effects and all pairwise
#'   interactions; higher-order interactions fold into residual unless
#'   replicates exist and `max_interaction` is increased).
#' @param na.rm Logical; remove rows with missing score or facet values before
#'   fitting. Default `TRUE`.
#'
#' @return An S3 object of class `"llm_g_study"` with elements:
#'   - `variance_components`: tibble with `component`, `variance`, `sd`,
#'     `pct_total`, `in_relative_error`, `in_absolute_error`, `facets_involved`,
#'     and (for ANOVA) `raw_variance`.
#'   - `g_coef`, `phi_coef`: observed-design relative \eqn{E\rho^2} and
#'     absolute \eqn{\Phi} coefficients.
#'   - `g_single`, `phi_single`: single-observation reliability (all facets = 1).
#'   - `sigma2_universe`, `sigma2_relative`, `sigma2_absolute`,
#'     `sem_relative`, `sem_absolute`.
#'   - `n_units`, `n_facets`, `n_replicates`, `balanced`, `method_used`,
#'     `singular`, `facets`, `id_col`, `score_col`.
#' @export
#'
#' @examples
#' data(ema_coded)
#' g <- g_study(ema_coded, score_col = "llm_score", facets = c("prompt", "model"))
#' g
g_study <- function(data,
                    score_col = "score",
                    id_col = "unit_id",
                    facets = NULL,
                    method = c("auto", "reml", "anova"),
                    max_interaction = 2L,
                    na.rm = TRUE) {
  if (inherits(data, "llm_coded_data") && missing(score_col)) {
    score_col <- data$primary_score
  }
  df <- extract_data_frame(data, arg = "data")
  method <- match.arg(method)
  assert_scalar_character(score_col, "score_col")
  assert_scalar_character(id_col, "id_col")
  assert_scalar_numeric(max_interaction, "max_interaction", min = 1)
  max_interaction <- as.integer(round(max_interaction))
  assert_flag(na.rm, "na.rm")

  assert_columns(df, c(id_col, score_col), arg = "data")
  if (!is.numeric(df[[score_col]])) {
    cli::cli_abort("Column {.val {score_col}} must be numeric for {.fn g_study}.")
  }

  # Auto-detect active facets if NULL
  if (is.null(facets)) {
    cand <- intersect(c("prompt", "model", "temperature"), names(df))
    facets <- cand[vapply(cand, function(col) {
      vals <- df[[col]]
      length(unique(vals[!is.na(vals)])) >= 2L
    }, logical(1L))]
    if (length(facets) == 0L) {
      cli::cli_abort(
        "Could not auto-detect any facet columns with >= 2 levels. Please specify {.arg facets} explicitly."
      )
    }
  } else {
    if (!is.character(facets) || length(facets) < 1L || anyNA(facets)) {
      cli::cli_abort("{.arg facets} must be a non-empty character vector of column names.")
    }
    facets <- unique(facets)
    if (id_col %in% facets) {
      cli::cli_abort("{.arg id_col} ({.val {id_col}}) cannot also be listed in {.arg facets}.")
    }
    assert_columns(df, facets, arg = "data")
  }

  # Filter missing values
  needed_cols <- c(id_col, facets, score_col)
  if (isTRUE(na.rm)) {
    keep <- stats::complete.cases(df[, needed_cols, drop = FALSE])
    df <- df[keep, needed_cols, drop = FALSE]
  } else if (any(!stats::complete.cases(df[, needed_cols, drop = FALSE]))) {
    cli::cli_abort("Missing values detected in {.arg data} and {.code na.rm = FALSE}.")
  }

  if (nrow(df) < 4L) {
    cli::cli_abort("Not enough complete observations ({nrow(df)}) to fit a G-study.")
  }
  assert_finite_values(df[[score_col]], score_col)

  # Check level counts
  n_units <- length(unique(df[[id_col]]))
  if (n_units < 2L) {
    cli::cli_abort("Object of measurement {.val {id_col}} must have at least 2 distinct units.")
  }
  n_facets <- vapply(facets, function(f) length(unique(df[[f]])), integer(1L))
  names(n_facets) <- facets
  too_few <- names(n_facets)[n_facets < 2L]
  if (length(too_few) > 0L) {
    cli::cli_abort(
      "Facet{?s} {.val {too_few}} ha{?s/ve} fewer than 2 distinct levels; drop constant facets before running {.fn g_study}."
    )
  }

  # Assess balance and cell replication
  all_factors <- c(id_col, facets)
  cell_key <- do.call(paste, c(lapply(all_factors, function(col) as.character(df[[col]])), list(sep = "\r")))
  cell_counts <- table(cell_key)
  expected_cells <- prod(c(n_units, n_facets))
  balanced <- (length(cell_counts) == expected_cells) && (length(unique(as.integer(cell_counts))) == 1L)
  n_replicates <- if (balanced) as.integer(cell_counts[[1L]]) else mean(as.numeric(cell_counts))
  has_replicates <- isTRUE(max(as.integer(cell_counts)) > 1L)

  # Enumerate estimable variance component subsets
  M <- length(all_factors)
  max_sep_order <- if (has_replicates) min(max_interaction, M) else min(max_interaction, M - 1L)
  subsets <- list()
  for (ord in seq_len(max_sep_order)) {
    cmb <- utils::combn(all_factors, ord, simplify = FALSE)
    subsets <- c(subsets, cmb)
  }

  # Guard against constant (zero-variance) scores so numerical dust never fabricates high reliability
  if (is_constant_numeric(df[[score_col]])) {
    cli::cli_warn(
      "Scores in {.val {score_col}} are constant (zero variance); G and Phi coefficients are undefined (NA)."
    )
    comps <- lapply(subsets, function(s) {
      list(label = paste(s, collapse = ":"), factors = s, variance = 0, raw_variance = 0, is_residual = FALSE)
    })
    comps[[length(comps) + 1L]] <- list(
      label = "Residual", factors = all_factors, variance = 0, raw_variance = 0, is_residual = TRUE
    )
    vc_rows <- lapply(comps, function(comp) {
      is_resid <- isTRUE(comp$is_residual)
      f_inv <- if (is_resid) facets else intersect(comp$factors, facets)
      has_u <- if (is_resid) TRUE else (id_col %in% comp$factors)
      in_rel <- is_resid || (has_u && length(f_inv) > 0L)
      in_abs <- is_resid || (length(f_inv) > 0L)
      role <- if (!is_resid && has_u && length(f_inv) == 0L) {
        "universe"
      } else if (in_rel) {
        "relative_and_absolute"
      } else {
        "absolute_only"
      }
      tibble::tibble(
        component = comp$label,
        variance = 0,
        raw_variance = 0,
        sd = 0,
        role = role,
        in_relative_error = in_rel,
        in_absolute_error = in_abs,
        is_residual = is_resid,
        facets_involved = list(f_inv),
        pct_total = 0
      )
    })
    vc_tbl <- do.call(rbind, vc_rows)
    return(structure(
      list(
        variance_components = vc_tbl,
        g_coef = NA_real_,
        phi_coef = NA_real_,
        g_single = NA_real_,
        phi_single = NA_real_,
        sigma2_universe = 0,
        sigma2_relative = 0,
        sigma2_absolute = 0,
        sem_relative = 0,
        sem_absolute = 0,
        n_units = n_units,
        n_facets = n_facets,
        n_replicates = n_replicates,
        n_obs = nrow(df),
        balanced = balanced,
        method_used = "constant",
        singular = TRUE,
        constant_scores = TRUE,
        facets = facets,
        id_col = id_col,
        score_col = score_col
      ),
      class = "llm_g_study"
    ))
  }

  # Fit variance components
  fit_res <- NULL
  method_used <- method
  if (method == "anova") {
    if (!balanced) {
      cli::cli_abort(
        "{.code method = \"anova\"} requires a balanced crossed design; use {.code method = \"reml\"} or {.code method = \"auto\"} for unbalanced designs."
      )
    }
    fit_res <- fit_g_anova(df, score_col = score_col, all_factors = all_factors, n_replicates = as.integer(n_replicates), subsets = subsets)
    method_used <- "anova"
  } else {
    reml_attempt <- tryCatch(
      fit_g_reml(df, score_col = score_col, all_factors = all_factors, subsets = subsets),
      error = function(e) structure(list(error = conditionMessage(e)), class = "g_fit_error")
    )
    if (!inherits(reml_attempt, "g_fit_error")) {
      fit_res <- reml_attempt
      method_used <- "reml"
    } else if (method == "auto" && balanced) {
      fit_res <- fit_g_anova(df, score_col = score_col, all_factors = all_factors, n_replicates = as.integer(n_replicates), subsets = subsets)
      method_used <- "anova"
    } else {
      cli::cli_abort("REML estimation failed in {.fn g_study}: {reml_attempt$error}")
    }
  }

  # Build structured variance components tibble, zeroing sub-precision numerical dust
  obs_var <- stats::var(as.numeric(df[[score_col]]))
  dust_tol <- 1e-12 * max(obs_var, 1)

  vc_rows <- lapply(seq_along(fit_res$components), function(i) {
    comp <- fit_res$components[[i]]
    v_clean <- max(as.numeric(comp$variance), 0)
    if (v_clean < dust_tol) v_clean <- 0
    is_resid <- isTRUE(comp$is_residual)
    f_inv <- if (is_resid) facets else intersect(comp$factors, facets)
    has_u <- if (is_resid) TRUE else (id_col %in% comp$factors)
    in_rel <- is_resid || (has_u && length(f_inv) > 0L)
    in_abs <- is_resid || (length(f_inv) > 0L)
    role <- if (!is_resid && has_u && length(f_inv) == 0L) {
      "universe"
    } else if (in_rel) {
      "relative_and_absolute"
    } else {
      "absolute_only"
    }

    tibble::tibble(
      component = comp$label,
      variance = v_clean,
      raw_variance = comp$raw_variance,
      sd = sqrt(v_clean),
      role = role,
      in_relative_error = in_rel,
      in_absolute_error = in_abs,
      is_residual = is_resid,
      facets_involved = list(f_inv)
    )
  })
  vc_tbl <- do.call(rbind, vc_rows)
  total_var <- sum(vc_tbl$variance)
  vc_tbl$pct_total <- if (total_var > 0) 100 * vc_tbl$variance / total_var else 0

  # Compute observed-design and single-observation reliability
  rel_obs <- compute_g_reliability(vc_tbl, n_facets = n_facets, n_replicates = n_replicates)
  n_single <- stats::setNames(rep(1, length(facets)), facets)
  rel_single <- compute_g_reliability(vc_tbl, n_facets = n_single, n_replicates = 1)

  structure(
    list(
      variance_components = vc_tbl,
      g_coef = rel_obs$g_coef,
      phi_coef = rel_obs$phi_coef,
      g_single = rel_single$g_coef,
      phi_single = rel_single$phi_coef,
      sigma2_universe = rel_obs$sigma2_universe,
      sigma2_relative = rel_obs$sigma2_relative,
      sigma2_absolute = rel_obs$sigma2_absolute,
      sem_relative = rel_obs$sem_relative,
      sem_absolute = rel_obs$sem_absolute,
      n_units = n_units,
      n_facets = n_facets,
      n_replicates = n_replicates,
      n_obs = nrow(df),
      balanced = balanced,
      method_used = method_used,
      singular = isTRUE(fit_res$singular),
      constant_scores = FALSE,
      facets = facets,
      id_col = id_col,
      score_col = score_col
    ),
    class = "llm_g_study"
  )
}

#' Fit G-study variance components via REML (lme4::lmer)
#' @noRd
fit_g_reml <- function(df, score_col, all_factors, subsets) {
  work <- data.frame(y = as.numeric(df[[score_col]]))
  term_names <- character(length(subsets))
  labels <- character(length(subsets))

  for (i in seq_along(subsets)) {
    facs <- subsets[[i]]
    tname <- paste0("re_", i)
    term_names[i] <- tname
    labels[i] <- paste(facs, collapse = ":")
    if (length(facs) == 1L) {
      work[[tname]] <- factor(df[[facs]])
    } else {
      work[[tname]] <- factor(do.call(paste, c(lapply(facs, function(col) as.character(df[[col]])), list(sep = "\r"))))
    }
  }

  rhs <- paste(paste0("(1 | ", term_names, ")"), collapse = " + ")
  fml <- stats::as.formula(paste("y ~ 1 +", rhs))

  ctrl <- lme4::lmerControl(
    check.nobs.vs.nlev = "ignore",
    check.nobs.vs.nRE = "ignore",
    check.nlev.gtreq.5 = "ignore"
  )
  fit <- suppressMessages(suppressWarnings(
    lme4::lmer(fml, data = work, REML = TRUE, control = ctrl)
  ))

  vc_df <- as.data.frame(lme4::VarCorr(fit))
  comps <- vector("list", length(subsets) + 1L)
  for (i in seq_along(subsets)) {
    row_match <- which(vc_df$grp == term_names[i])
    v <- if (length(row_match) == 1L) max(as.numeric(vc_df$vcov[row_match]), 0) else 0
    comps[[i]] <- list(
      label = labels[i],
      factors = subsets[[i]],
      variance = v,
      raw_variance = v,
      is_residual = FALSE
    )
  }

  res_row <- which(vc_df$grp == "Residual")
  v_res <- if (length(res_row) == 1L) max(as.numeric(vc_df$vcov[res_row]), 0) else 0
  comps[[length(subsets) + 1L]] <- list(
    label = "Residual",
    factors = all_factors,
    variance = v_res,
    raw_variance = v_res,
    is_residual = TRUE
  )

  list(
    components = comps,
    singular = lme4::isSingular(fit),
    fit = fit
  )
}

#' Fit balanced crossed G-study variance components via exact EMS Möbius inversion
#' @noRd
fit_g_anova <- function(df, score_col, all_factors, n_replicates, subsets) {
  y <- as.numeric(df[[score_col]])
  M <- length(all_factors)
  N_vec <- vapply(all_factors, function(f) length(unique(df[[f]])), integer(1L))
  names(N_vec) <- all_factors
  n_total <- length(y)

  # Enumerate ALL subsets of all_factors (including empty set and full set)
  all_subsets <- list(character(0))
  for (k in seq_len(M)) {
    all_subsets <- c(all_subsets, utils::combn(all_factors, k, simplify = FALSE))
  }
  subset_keys <- vapply(all_subsets, function(s) paste(s, collapse = ":"), character(1L))

  # Compute T_S = sum_all_obs (mean_S)^2 for every subset S
  T_vals <- numeric(length(all_subsets))
  names(T_vals) <- subset_keys
  T_vals[1L] <- n_total * (mean(y)^2)

  for (idx in seq(2L, length(all_subsets))) {
    s <- all_subsets[[idx]]
    gkey <- do.call(paste, c(lapply(s, function(col) as.character(df[[col]])), list(sep = "\r")))
    means_s <- tapply(y, gkey, mean)
    counts_s <- tapply(y, gkey, length)
    T_vals[idx] <- sum(as.numeric(counts_s) * (as.numeric(means_s)^2))
  }

  # Möbius inversion for SS_S and MS_S
  MS_vals <- numeric(length(all_subsets))
  names(MS_vals) <- subset_keys
  for (idx in seq(2L, length(all_subsets))) {
    s <- all_subsets[[idx]]
    ss <- 0
    for (j in seq_along(all_subsets)) {
      a <- all_subsets[[j]]
      if (all(a %in% s)) {
        sign_val <- if ((length(s) - length(a)) %% 2L == 0L) 1 else -1
        ss <- ss + sign_val * T_vals[j]
      }
    }
    df_s <- prod(N_vec[s] - 1L)
    MS_vals[idx] <- ss / df_s
  }

  # Within-cell residual MS if n_replicates > 1
  ms_within <- 0
  if (n_replicates > 1L) {
    full_key <- subset_keys[length(subset_keys)]
    ss_within <- sum(y^2) - T_vals[[full_key]]
    df_within <- prod(N_vec) * (n_replicates - 1L)
    ms_within <- ss_within / df_within
  }

  # Exact EMS inversion: var_S = (1 / (n_r * prod_{k not in S} N_k)) * sum_{B >= S} (-1)^(|B|-|S|) MS_B
  raw_vars <- numeric(length(all_subsets))
  names(raw_vars) <- subset_keys
  for (idx in seq(2L, length(all_subsets))) {
    s <- all_subsets[[idx]]
    denom <- n_replicates * prod(c(1, N_vec[setdiff(all_factors, s)]))
    num <- 0
    for (j in seq(2L, length(all_subsets))) {
      b <- all_subsets[[j]]
      if (all(s %in% b)) {
        sign_val <- if ((length(b) - length(s)) %% 2L == 0L) 1 else -1
        num <- num + sign_val * MS_vals[j]
      }
    }
    if (n_replicates > 1L && length(s) == M) {
      # Only the full-factor interaction retains ms_within after Möbius cancellation
      num <- num - ms_within
    }
    raw_vars[idx] <- num / denom
  }

  # Assemble requested estimable subsets + fold omitted higher-order terms into residual
  req_keys <- vapply(subsets, function(s) paste(s, collapse = ":"), character(1L))
  comps <- vector("list", length(subsets) + 1L)
  for (i in seq_along(subsets)) {
    k <- req_keys[i]
    rv <- raw_vars[[k]]
    comps[[i]] <- list(
      label = k,
      factors = subsets[[i]],
      variance = max(rv, 0),
      raw_variance = rv,
      is_residual = FALSE
    )
  }

  omitted_keys <- setdiff(subset_keys[-1L], req_keys)
  raw_resid <- (if (n_replicates > 1L) ms_within else 0) + sum(raw_vars[omitted_keys])
  comps[[length(subsets) + 1L]] <- list(
    label = "Residual",
    factors = all_factors,
    variance = max(raw_resid, 0),
    raw_variance = raw_resid,
    is_residual = TRUE
  )

  list(
    components = comps,
    singular = any(vapply(comps, function(c) c$raw_variance < 0, logical(1L))),
    fit = NULL
  )
}

#' Compute G-theory reliability metrics for arbitrary facet counts
#' @noRd
compute_g_reliability <- function(vc_tbl, n_facets, n_replicates = 1) {
  u_rows <- vc_tbl$role == "universe"
  sigma2_u <- sum(vc_tbl$variance[u_rows])

  sigma2_rel <- 0
  sigma2_abs <- 0

  for (i in seq_len(nrow(vc_tbl))) {
    if (vc_tbl$role[i] == "universe") {
      next
    }
    v <- vc_tbl$variance[i]
    if (v <= 0) {
      next
    }
    f_inv <- vc_tbl$facets_involved[[i]]
    denom <- prod(as.numeric(n_facets[f_inv]))
    if (isTRUE(vc_tbl$is_residual[i])) {
      denom <- denom * as.numeric(n_replicates)
    }
    contrib <- v / denom
    if (isTRUE(vc_tbl$in_relative_error[i])) {
      sigma2_rel <- sigma2_rel + contrib
    }
    if (isTRUE(vc_tbl$in_absolute_error[i])) {
      sigma2_abs <- sigma2_abs + contrib
    }
  }

  g_coef <- if (sigma2_u + sigma2_rel > 0) sigma2_u / (sigma2_u + sigma2_rel) else NA_real_
  phi_coef <- if (sigma2_u + sigma2_abs > 0) sigma2_u / (sigma2_u + sigma2_abs) else NA_real_

  list(
    sigma2_universe = sigma2_u,
    sigma2_relative = sigma2_rel,
    sigma2_absolute = sigma2_abs,
    sem_relative = sqrt(max(sigma2_rel, 0)),
    sem_absolute = sqrt(max(sigma2_abs, 0)),
    g_coef = g_coef,
    phi_coef = phi_coef
  )
}

#' @export
print.llm_g_study <- function(x, ...) {
  facet_desc <- paste(paste0(names(x$n_facets), "=", x$n_facets), collapse = ", ")
  cli::cli_h3("Generalizability Study (G-Study)")
  cli::cli_text(
    "Units ({x$id_col}): {x$n_units} | Facets: {facet_desc} | Method: {toupper(x$method_used)} | Balanced: {x$balanced}"
  )
  cli::cli_text(
    "Observed design: G (Erho^2) = {fmt_num(x$g_coef)} | Phi = {fmt_num(x$phi_coef)} | SEM(rel) = {fmt_num(x$sem_relative)}, SEM(abs) = {fmt_num(x$sem_absolute)}"
  )
  cli::cli_text(
    "Single-facet (n=1): G = {fmt_num(x$g_single)} | Phi = {fmt_num(x$phi_single)}"
  )
  tbl <- tibble::tibble(
    component = x$variance_components$component,
    variance = round(x$variance_components$variance, 4L),
    sd = round(x$variance_components$sd, 4L),
    pct_total = round(x$variance_components$pct_total, 2L),
    role = x$variance_components$role
  )
  print(tbl, n = nrow(tbl))
  invisible(x)
}

#' @export
summary.llm_g_study <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
