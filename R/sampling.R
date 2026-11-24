#' Select a Human Validation Sample with Exact Inclusion Probabilities & IPW Weights
#'
#' Draws a reproducible human-validation subset from a corpus of LLM-coded texts
#' using simple random, subgroup-stratified (proportional, equal-per-stratum
#' for DPF power, or Neyman-optimal), score-quantile stratified, or
#' cluster-stratified sampling. Returns exact inclusion probabilities
#' \eqn{\pi_i \in (0, 1]} and Horvitz-Thompson inverse-probability weights
#' \eqn{w_i = 1 / \pi_i} ready to pass directly into weighted [ppi_mean()] or
#' [ppi_lm()] (active / IPW-PPI).
#'
#' @param data A `data.frame` (or `llm_coded_data`) representing the full
#'   population/corpus of units to sample from.
#' @param n Target total number of units to select for human validation. For
#'   `strategy = "cluster"`, used to determine the number of clusters if
#'   `n_clusters` is `NULL`.
#' @param strategy Sampling design:
#'   - `"simple"`: simple random sampling without replacement (SRSWOR).
#'   - `"stratified"`: stratified sampling across `strata_col` using
#'     `allocation`.
#'   - `"neyman"`: shorthand for Neyman-optimal allocation (`n_h` proportional
#'     to `N_h * SD_h`) using `score_col` (and `strata_col`, or quantile bins
#'     of `score_col` if `strata_col` is `NULL`).
#'   - `"quantile"`: stratifies units into `n_quantiles` bins of `score_col`
#'     (covering distribution tails evenly).
#'   - `"cluster"`: whole-cluster sampling over `cluster_col`.
#' @param strata_col Name of the categorical stratification column (required
#'   for `strategy = "stratified"`, optional for `"neyman"`).
#' @param score_col Name of a numeric proxy/LLM score column used to estimate
#'   stratum SDs for Neyman allocation or to form quantile strata.
#' @param cluster_col Name of the cluster identifier column (required for
#'   `strategy = "cluster"`).
#' @param allocation Stratum allocation rule for `"stratified"` or `"quantile"`:
#'   `"proportional"` (default), `"equal"` (maximizes power for subgroup DPF
#'   tests), or `"neyman"` (minimizes overall variance).
#' @param n_quantiles Integer number of quantile strata for `strategy = "quantile"`
#'   (or `"neyman"` without `strata_col`). Default `4L`.
#' @param n_clusters Optional integer number of whole clusters to select when
#'   `strategy = "cluster"`. Overrides `n` if supplied.
#' @param min_per_stratum Minimum units selected per stratum (capped by stratum
#'   size). Default `2L`.
#' @param seed Optional integer random seed. Uses local RNG state restoration
#'   so global `.Random.seed` is never mutated.
#'
#' @return An S3 object of class `"llm_validation_sample"` containing:
#'   - `selected_idx`: sorted integer vector of selected row indices.
#'   - `selected`: logical indicator vector of length `nrow(data)`.
#'   - `inclusion_prob`: numeric vector of exact positive inclusion
#'     probabilities \eqn{\pi_i}.
#'   - `weights`: numeric vector of Horvitz-Thompson weights \eqn{w_i = 1 / \pi_i}.
#'   - `data`: tibble copy of `data` augmented with `is_validation`,
#'     `inclusion_prob`, and `sampling_weight`.
#'   - `stratum_summary`: tibble summarizing population size, selected count,
#'     inclusion probability, and weight per stratum.
#'   - `strategy`, `allocation`, `n_target`, `n_selected`, `n_population`.
#' @export
#'
#' @examples
#' data(ema_coded)
#' agg <- aggregate_scores(ema_coded, score_col = "llm_score")
#' vs <- select_validation_sample(
#'   agg, n = 40, strategy = "stratified",
#'   strata_col = "group", allocation = "equal", seed = 42
#' )
#' vs
select_validation_sample <- function(data,
                                     n = NULL,
                                     strategy = c("simple", "stratified", "neyman", "quantile", "cluster"),
                                     strata_col = NULL,
                                     score_col = NULL,
                                     cluster_col = NULL,
                                     allocation = c("proportional", "equal", "neyman"),
                                     n_quantiles = 4L,
                                     n_clusters = NULL,
                                     min_per_stratum = 2L,
                                     seed = NULL) {
  df <- extract_data_frame(data, arg = "data")
  M <- nrow(df)
  if (M < 4L) {
    cli::cli_abort("{.arg data} must contain at least 4 rows (found {M}).")
  }
  strategy <- match.arg(strategy)
  allocation <- match.arg(allocation)
  assert_scalar_character(strata_col, "strata_col", allow_null = TRUE)
  assert_scalar_character(score_col, "score_col", allow_null = TRUE)
  assert_scalar_character(cluster_col, "cluster_col", allow_null = TRUE)
  assert_scalar_numeric(n_quantiles, "n_quantiles", min = 2, max = 20)
  n_quantiles <- as.integer(round(n_quantiles))
  assert_scalar_numeric(min_per_stratum, "min_per_stratum", min = 1)
  min_per_stratum <- as.integer(round(min_per_stratum))
  assert_scalar_numeric(seed, "seed", allow_null = TRUE)

  if (strategy == "cluster") {
    if (is.null(cluster_col)) {
      cli::cli_abort("{.arg cluster_col} is required when {.code strategy = \"cluster\"}.")
    }
    assert_columns(df, cluster_col, arg = "data")
    cl_vals <- as.character(df[[cluster_col]])
    if (anyNA(cl_vals)) {
      cli::cli_abort("{.arg cluster_col} ({.val {cluster_col}}) must not contain missing values.")
    }
    u_cl <- unique(cl_vals)
    G <- length(u_cl)
    if (G < 2L) {
      cli::cli_abort("{.arg cluster_col} must have at least 2 distinct clusters.")
    }
    if (!is.null(n_clusters)) {
      assert_scalar_numeric(n_clusters, "n_clusters", min = 1, max = G)
      g_sel <- as.integer(round(n_clusters))
    } else {
      assert_scalar_numeric(n, "n", min = 2, max = M)
      mean_m <- M / G
      g_sel <- max(1L, min(G, as.integer(round(n / mean_m))))
    }

    draw_fn <- function() sort(sample.int(G, size = g_sel, replace = FALSE))
    cl_idx <- if (is.null(seed)) draw_fn() else with_local_seed(as.integer(seed), draw_fn())
    chosen_cl <- u_cl[cl_idx]
    selected <- cl_vals %in% chosen_cl
    selected_idx <- which(selected)
    pi_vec <- rep(g_sel / G, M)
    w_vec <- 1 / pi_vec

    strata_tbl <- tibble::tibble(
      stratum = "all_clusters",
      n_pop = M,
      n_selected = length(selected_idx),
      n_clusters_pop = G,
      n_clusters_selected = g_sel,
      inclusion_prob = g_sel / G,
      weight = G / g_sel
    )

    out_df <- df
    out_df$is_validation <- selected
    out_df$inclusion_prob <- pi_vec
    out_df$sampling_weight <- w_vec

    return(structure(
      list(
        selected_idx = selected_idx,
        selected = selected,
        inclusion_prob = pi_vec,
        weights = w_vec,
        data = out_df,
        stratum_summary = strata_tbl,
        strategy = strategy,
        allocation = "cluster_srswor",
        n_target = if (!is.null(n_clusters)) g_sel else as.integer(round(n)),
        n_selected = length(selected_idx),
        n_population = M
      ),
      class = "llm_validation_sample"
    ))
  }

  # Non-cluster strategies require n
  assert_scalar_numeric(n, "n", min = 2, max = M)
  n_target <- as.integer(round(n))

  if (strategy == "simple") {
    draw_fn <- function() sort(sample.int(M, size = n_target, replace = FALSE))
    selected_idx <- if (is.null(seed)) draw_fn() else with_local_seed(as.integer(seed), draw_fn())
    selected <- seq_len(M) %in% selected_idx
    pi_vec <- rep(n_target / M, M)
    w_vec <- 1 / pi_vec

    strata_tbl <- tibble::tibble(
      stratum = "all",
      n_pop = M,
      n_selected = n_target,
      inclusion_prob = n_target / M,
      weight = M / n_target,
      stratum_sd = NA_real_
    )

    out_df <- df
    out_df$is_validation <- selected
    out_df$inclusion_prob <- pi_vec
    out_df$sampling_weight <- w_vec

    return(structure(
      list(
        selected_idx = selected_idx,
        selected = selected,
        inclusion_prob = pi_vec,
        weights = w_vec,
        data = out_df,
        stratum_summary = strata_tbl,
        strategy = strategy,
        allocation = "simple",
        n_target = n_target,
        n_selected = n_target,
        n_population = M
      ),
      class = "llm_validation_sample"
    ))
  }

  # Build stratum vector for stratified / neyman / quantile
  eff_alloc <- if (strategy == "neyman") "neyman" else allocation

  if (strategy == "quantile" || (strategy == "neyman" && is.null(strata_col))) {
    if (is.null(score_col)) {
      cli::cli_abort("{.arg score_col} is required for {.code strategy = \"{strategy}\"} (without {.arg strata_col}).")
    }
    assert_columns(df, score_col, arg = "data")
    sc <- as.numeric(df[[score_col]])
    assert_finite_values(sc, score_col)
    if (anyNA(sc)) {
      cli::cli_abort("{.arg score_col} ({.val {score_col}}) must not contain missing values.")
    }
    probs <- seq(0, 1, length.out = n_quantiles + 1L)
    brks <- unique(stats::quantile(sc, probs = probs, names = FALSE))
    if (length(brks) < 2L) {
      cli::cli_abort("{.arg score_col} ({.val {score_col}}) is constant; cannot form quantile strata.")
    }
    stratum_fac <- cut(sc, breaks = brks, include.lowest = TRUE, labels = paste0("Q", seq_len(length(brks) - 1L)))
    strata_vec <- as.character(stratum_fac)
  } else {
    if (is.null(strata_col)) {
      cli::cli_abort("{.arg strata_col} is required when {.code strategy = \"stratified\"}.")
    }
    assert_columns(df, strata_col, arg = "data")
    if (anyNA(df[[strata_col]])) {
      cli::cli_abort("{.arg strata_col} ({.val {strata_col}}) must not contain missing values.")
    }
    strata_vec <- as.character(df[[strata_col]])
  }

  if (eff_alloc == "neyman") {
    if (is.null(score_col)) {
      cli::cli_abort("{.arg score_col} is required for Neyman-optimal allocation.")
    }
    assert_columns(df, score_col, arg = "data")
    sc_neyman <- as.numeric(df[[score_col]])
    assert_finite_values(sc_neyman, score_col)
  } else {
    sc_neyman <- NULL
  }

  u_strata <- if (!is.null(strata_col) && is.factor(df[[strata_col]])) {
    intersect(levels(df[[strata_col]]), unique(strata_vec))
  } else {
    unique(strata_vec)
  }
  H <- length(u_strata)
  idx_by_h <- lapply(u_strata, function(h) which(strata_vec == h))
  N_h <- vapply(idx_by_h, length, integer(1L))
  sd_h <- if (!is.null(sc_neyman)) {
    vapply(idx_by_h, function(ii) {
      if (length(ii) <= 1L) return(1e-6)
      s <- stats::sd(sc_neyman[ii], na.rm = TRUE)
      if (!is.finite(s) || s <= 1e-8) 1e-6 else s
    }, numeric(1L))
  } else {
    rep(NA_real_, H)
  }

  # Compute exact stratum sample counts n_h summing to n_target with 1 <= n_h <= N_h
  raw_weights <- switch(
    eff_alloc,
    proportional = as.numeric(N_h),
    equal = rep(1, H),
    neyman = as.numeric(N_h) * sd_h
  )
  n_h <- allocate_stratum_counts(
    n_total = n_target,
    caps = N_h,
    weights = raw_weights,
    min_per = min_per_stratum
  )

  draw_stratified <- function() {
    sel_list <- lapply(seq_len(H), function(h) {
      pool <- idx_by_h[[h]]
      if (n_h[h] >= length(pool)) pool else sort(sample(pool, size = n_h[h], replace = FALSE))
    })
    sort(unlist(sel_list, use.names = FALSE))
  }

  selected_idx <- if (is.null(seed)) {
    draw_stratified()
  } else {
    with_local_seed(as.integer(seed), draw_stratified())
  }
  selected <- seq_len(M) %in% selected_idx

  pi_h <- n_h / N_h
  pi_vec <- numeric(M)
  for (h in seq_len(H)) {
    pi_vec[idx_by_h[[h]]] <- pi_h[h]
  }
  w_vec <- 1 / pi_vec

  strata_tbl <- tibble::tibble(
    stratum = u_strata,
    n_pop = N_h,
    n_selected = n_h,
    inclusion_prob = pi_h,
    weight = 1 / pi_h,
    stratum_sd = sd_h
  )

  out_df <- df
  out_df$is_validation <- selected
  out_df$inclusion_prob <- pi_vec
  out_df$sampling_weight <- w_vec

  structure(
    list(
      selected_idx = selected_idx,
      selected = selected,
      inclusion_prob = pi_vec,
      weights = w_vec,
      data = out_df,
      stratum_summary = strata_tbl,
      strategy = strategy,
      allocation = eff_alloc,
      n_target = n_target,
      n_selected = length(selected_idx),
      n_population = M
    ),
    class = "llm_validation_sample"
  )
}

# Allocate integer stratum sample sizes summing exactly to n_total subject to bounds
allocate_stratum_counts <- function(n_total, caps, weights, min_per = 2L) {
  H <- length(caps)
  if (n_total >= sum(caps)) {
    return(as.integer(caps))
  }
  # Respect capacity and ensure sum(floors) <= n_total
  floors <- pmin(as.integer(caps), as.integer(min_per))
  if (sum(floors) > n_total) {
    floors <- pmin(as.integer(caps), 1L)
  }
  if (sum(floors) > n_total) {
    floors <- rep(0L, H)
  }

  n_h <- floors
  rem <- n_total - sum(n_h)
  w <- pmax(as.numeric(weights), 1e-12)

  # Iterative water-filling respecting upper caps
  while (rem > 0L) {
    room <- caps - n_h
    open <- which(room > 0L)
    if (length(open) == 0L) break
    w_open <- w[open]
    share <- rem * w_open / sum(w_open)
    add_int <- pmin(room[open], as.integer(floor(share)))
    n_h[open] <- n_h[open] + add_int
    rem <- n_total - sum(n_h)
    if (rem <= 0L) break

    # Distribute remaining units by largest fractional remainder among still-open strata
    room2 <- caps - n_h
    open2 <- which(room2 > 0L)
    if (length(open2) == 0L) break
    frac <- (rem * w[open2] / sum(w[open2])) - floor(rem * w[open2] / sum(w[open2]))
    ord <- open2[order(-frac, -w[open2])]
    give <- min(rem, length(ord))
    n_h[ord[seq_len(give)]] <- n_h[ord[seq_len(give)]] + 1L
    rem <- n_total - sum(n_h)
  }

  as.integer(n_h)
}

#' @export
print.llm_validation_sample <- function(x, ...) {
  cli::cli_h3("Validation Sample Plan ({x$strategy} / {x$allocation})")
  cli::cli_text(
    "Selected {x$n_selected} of {x$n_population} units ({fmt_num(100 * x$n_selected / x$n_population, 1L)}%) | Weight range: [{fmt_num(min(x$weights), 2L)}, {fmt_num(max(x$weights), 2L)}]"
  )
  print(x$stratum_summary, n = min(nrow(x$stratum_summary), 12L))
  invisible(x)
}
