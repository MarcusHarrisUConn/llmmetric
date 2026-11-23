#' Prediction-Powered Category Proportion & Prevalence Estimation (PPI / PPI++)
#'
#' Estimates population class prevalences \eqn{\pi_c = \mathbb{P}(Y = c)} for
#' binary or multi-class categorical content coding by combining a
#' human-validated subset (`truth_col` observed) with a larger LLM-coded
#' unlabeled corpus (`pred_col` only). Computes class-optimal PPI++ weights
#' \eqn{\hat{\lambda}_c^*}, the full joint \eqn{K \times K} asymptotic
#' covariance matrix, marginal or family-wise simultaneous confidence intervals,
#' and direct Human-Only and Naive-LLM benchmarks.
#'
#' @param data A `data.frame` (or `llm_coded_data`) containing `truth_col` and
#'   `pred_col`. When `unlabeled = NULL`, rows with non-missing `truth_col` form
#'   the labeled validation set and rows with `is.na(truth_col)` form the
#'   unlabeled set.
#' @param truth_col Name of the human gold-standard categorical/binary column
#'   (factor, character, logical, or integer/numeric labels).
#' @param pred_col Either a single column name of discrete LLM class labels
#'   (or binary predicted probabilities in `[0, 1]` when `truth_col` is binary
#'   `0/1` or logical), OR a named character vector mapping category levels to
#'   LLM predicted-probability column names (e.g.,
#'   `c(negative = "p_neg", neutral = "p_neu", positive = "p_pos")`).
#' @param unlabeled Optional separate `data.frame` of unlabeled observations.
#' @param levels Optional character vector specifying the ordered category
#'   levels to estimate. If `NULL`, inferred from `truth_col` and `pred_col`.
#' @param lambda Power-tuning parameter: `"auto"` (default: class-optimal
#'   PPI++), `1` (standard PPI), `0` (human-only), or a finite numeric scalar.
#' @param conf_level Confidence level in `(0, 1)`. Default `0.95`.
#' @param simultaneous Logical; if `TRUE`, computes family-wise simultaneous
#'   confidence intervals across all \eqn{K} categories using Bonferroni
#'   multiplicity control (\eqn{\alpha / K}). Default `FALSE` (marginal CIs).
#' @param clamp_ci Logical; if `TRUE` (default), clamps confidence interval
#'   endpoints to `[0, 1]` while preserving exact unclipped standard errors.
#' @param clip_lambda Logical; if `TRUE`, clips auto-tuned \eqn{\hat{\lambda}_c^*}
#'   to `[0, 1]`. Default `FALSE`.
#'
#' @return An S3 object of class `c("llm_ppi_proportion", "llm_ppi")` compatible
#'   with `coef()`, `vcov()`, `confint()`, `print()`, `summary()`, `autoplot()`,
#'   and [format_apa_table()].
#' @export
#'
#' @examples
#' set.seed(42)
#' n_tot <- 800
#' lvls <- c("low", "moderate", "high")
#' y_true <- factor(sample(lvls, n_tot, replace = TRUE, prob = c(0.50, 0.30, 0.20)), levels = lvls)
#' # Biased LLM over-predicts "high" when true label is "moderate"
#' y_llm <- y_true
#' flip <- (y_true == "moderate") & (runif(n_tot) < 0.45)
#' y_llm[flip] <- "high"
#' df <- tibble::tibble(
#'   human_label = factor(
#'     c(as.character(y_true[1:150]), rep(NA_character_, n_tot - 150)),
#'     levels = lvls
#'   ),
#'   llm_label = y_llm
#' )
#' fit <- ppi_proportion(df, truth_col = "human_label", pred_col = "llm_label")
#' fit
ppi_proportion <- function(data,
                           truth_col = "human_label",
                           pred_col = "llm_label",
                           unlabeled = NULL,
                           levels = NULL,
                           lambda = "auto",
                           conf_level = 0.95,
                           simultaneous = FALSE,
                           clamp_ci = TRUE,
                           clip_lambda = FALSE) {
  assert_scalar_character(truth_col, "truth_col")
  assert_scalar_numeric(conf_level, "conf_level", min = 0.5, max = 0.9999)
  assert_flag(simultaneous, "simultaneous")
  assert_flag(clamp_ci, "clamp_ci")
  assert_flag(clip_lambda, "clip_lambda")

  df_main <- extract_data_frame(data, arg = "data")
  prob_mode <- length(pred_col) > 1L || (!is.null(names(pred_col)) && nzchar(names(pred_col)[1L]))
  pred_cols_vec <- unname(as.character(pred_col))

  if (length(pred_cols_vec) < 1L || anyNA(pred_cols_vec) || any(!nzchar(pred_cols_vec))) {
    cli::cli_abort("{.arg pred_col} must be a non-empty column name or named vector of probability columns.")
  }

  # Split labeled vs unlabeled
  if (is.null(unlabeled)) {
    assert_columns(df_main, c(truth_col, pred_cols_vec), arg = "data")
    is_lab <- !is.na(df_main[[truth_col]])
    lab_df <- df_main[is_lab, , drop = FALSE]
    unlab_df <- df_main[!is_lab, , drop = FALSE]
  } else {
    lab_df <- df_main
    unlab_df <- extract_data_frame(unlabeled, arg = "unlabeled")
    assert_columns(lab_df, c(truth_col, pred_cols_vec), arg = "data")
    assert_columns(unlab_df, pred_cols_vec, arg = "unlabeled")
    lab_df <- lab_df[!is.na(lab_df[[truth_col]]), , drop = FALSE]
  }

  lab_keep <- !is.na(lab_df[[truth_col]]) & stats::complete.cases(lab_df[, pred_cols_vec, drop = FALSE])
  unlab_keep <- stats::complete.cases(unlab_df[, pred_cols_vec, drop = FALSE])
  lab_df <- lab_df[lab_keep, , drop = FALSE]
  unlab_df <- unlab_df[unlab_keep, , drop = FALSE]

  n <- nrow(lab_df)
  N <- nrow(unlab_df)
  if (n < 5L) {
    cli::cli_abort("At least 5 complete labeled validation observations are required (found {n}).")
  }
  if (N < 5L) {
    cli::cli_abort("At least 5 complete unlabeled LLM observations are required (found {N}).")
  }

  # Build indicator matrices Y_lab (n x K), F_lab (n x K), F_unlab (N x K)
  raw_t <- lab_df[[truth_col]]

  if (prob_mode) {
    if (is.null(names(pred_col)) || any(!nzchar(names(pred_col)))) {
      cli::cli_abort("When supplying multiple columns in {.arg pred_col}, it must be a named vector mapping category labels to probability columns.")
    }
    cat_lvls <- levels %||% names(pred_col)
    missing_lvl <- setdiff(cat_lvls, names(pred_col))
    if (length(missing_lvl) > 0L) {
      cli::cli_abort("Levels {.val {missing_lvl}} missing from named {.arg pred_col}.")
    }
    K <- length(cat_lvls)
    Y_lab <- sapply(cat_lvls, function(lv) as.numeric(as.character(raw_t) == lv))
    F_lab <- sapply(cat_lvls, function(lv) as.numeric(lab_df[[pred_col[[lv]]]]))
    F_unlab <- sapply(cat_lvls, function(lv) as.numeric(unlab_df[[pred_col[[lv]]]]))
    if (K == 1L) {
      Y_lab <- matrix(Y_lab, ncol = 1L)
      F_lab <- matrix(F_lab, ncol = 1L)
      F_unlab <- matrix(F_unlab, ncol = 1L)
    }
    assert_finite_values(as.numeric(F_lab), "pred_col (labeled)")
    assert_finite_values(as.numeric(F_unlab), "pred_col (unlabeled)")
    colnames(Y_lab) <- colnames(F_lab) <- colnames(F_unlab) <- cat_lvls
  } else {
    pcol <- pred_cols_vec[1L]
    raw_fl <- lab_df[[pcol]]
    raw_fu <- unlab_df[[pcol]]

    # Special case: binary truth (0/1 or logical) + continuous LLM probability in [0, 1]
    is_bin_truth <- (is.logical(raw_t) || (is.numeric(raw_t) && all(raw_t %in% c(0, 1))))
    is_cont_prob <- is.numeric(raw_fl) && is.numeric(raw_fu) &&
      (any(!(raw_fl %in% c(0, 1))) || any(!(raw_fu %in% c(0, 1))))

    if (is_bin_truth && is_cont_prob && is.null(levels)) {
      assert_finite_values(raw_fl, pcol)
      assert_finite_values(raw_fu, pcol)
      cat_lvls <- "positive"
      K <- 1L
      Y_lab <- matrix(as.numeric(raw_t), ncol = 1L, dimnames = list(NULL, cat_lvls))
      F_lab <- matrix(as.numeric(raw_fl), ncol = 1L, dimnames = list(NULL, cat_lvls))
      F_unlab <- matrix(as.numeric(raw_fu), ncol = 1L, dimnames = list(NULL, cat_lvls))
    } else {
      if (is.null(levels)) {
        if (is.factor(raw_t)) {
          cat_lvls <- union(levels(raw_t), unique(c(as.character(raw_fl), as.character(raw_fu))))
        } else {
          cat_lvls <- sort(unique(c(as.character(raw_t), as.character(raw_fl), as.character(raw_fu))))
        }
      } else {
        cat_lvls <- unique(as.character(levels))
      }
      if (length(cat_lvls) < 2L) {
        cli::cli_abort("At least 2 category levels are required for discrete {.fn ppi_proportion}.")
      }
      K <- length(cat_lvls)
      Y_lab <- sapply(cat_lvls, function(lv) as.numeric(as.character(raw_t) == lv))
      F_lab <- sapply(cat_lvls, function(lv) as.numeric(as.character(raw_fl) == lv))
      F_unlab <- sapply(cat_lvls, function(lv) as.numeric(as.character(raw_fu) == lv))
      colnames(Y_lab) <- colnames(F_lab) <- colnames(F_unlab) <- cat_lvls
    }
  }

  # Compute per-category tuning weights lambda_k
  if (identical(lambda, "auto")) {
    lam_vec <- numeric(K)
    names(lam_vec) <- cat_lvls
    for (k in seq_len(K)) {
      cov_k <- stats::cov(Y_lab[, k], F_lab[, k])
      var_k <- stats::var(F_lab[, k]) + (n / N) * stats::var(F_unlab[, k])
      lk <- if (is.finite(var_k) && var_k > 1e-12 && is.finite(cov_k)) cov_k / var_k else 0
      if (isTRUE(clip_lambda)) lk <- min(max(lk, 0), 1)
      lam_vec[k] <- lk
    }
    method_label <- "ppi++"
  } else {
    assert_scalar_numeric(lambda, "lambda")
    lam_vec <- stats::setNames(rep(as.numeric(lambda), K), cat_lvls)
    method_label <- if (abs(lambda - 1) < 1e-12) "ppi" else paste0("ppi(lambda=", round(lambda, 2L), ")")
  }

  df_ref <- Inf
  small_sample <- n < 20L
  if (small_sample) {
    df_ref <- max(n - 1L, 1L)
    cli::cli_warn(
      "Small labeled validation sample (n = {n}); using t({df_ref}) reference distribution for finite-sample inference."
    )
  }

  # Point estimates & joint K x K covariance
  est_ppi <- colMeans(Y_lab) + lam_vec * (colMeans(F_unlab) - colMeans(F_lab))
  names(est_ppi) <- cat_lvls

  D_lab <- Y_lab - sweep(F_lab, 2L, lam_vec, "*")
  F_unlab_scaled <- sweep(F_unlab, 2L, lam_vec, "*")
  V_ppi <- (1 / n) * safe_cov(D_lab) + (1 / N) * safe_cov(F_unlab_scaled)
  V_ppi <- 0.5 * (V_ppi + t(V_ppi))
  dimnames(V_ppi) <- list(cat_lvls, cat_lvls)

  # Benchmarks
  est_hum <- colMeans(Y_lab)
  names(est_hum) <- cat_lvls
  V_hum <- (1 / n) * safe_cov(Y_lab)
  dimnames(V_hum) <- list(cat_lvls, cat_lvls)

  F_all <- rbind(F_lab, F_unlab)
  est_naive <- colMeans(F_all)
  names(est_naive) <- cat_lvls
  V_naive <- (1 / nrow(F_all)) * safe_cov(F_all)
  dimnames(V_naive) <- list(cat_lvls, cat_lvls)

  # Marginal vs simultaneous Bonferroni-adjusted confidence level
  eff_conf <- if (isTRUE(simultaneous) && K > 1L) 1 - (1 - conf_level) / K else conf_level

  tbl_ppi <- tidy_ppi_table(est_ppi, V_ppi, method_label = method_label, conf_level = eff_conf, df = df_ref)
  tbl_hum <- tidy_ppi_table(est_hum, V_hum, method_label = "human_only", conf_level = eff_conf, df = df_ref)
  tbl_naive <- tidy_ppi_table(est_naive, V_naive, method_label = "naive_llm", conf_level = eff_conf, df = Inf)

  if (isTRUE(clamp_ci)) {
    clamp_tbl <- function(tb) {
      tb$conf_low <- pmax(tb$conf_low, 0)
      tb$conf_high <- pmin(tb$conf_high, 1)
      tb
    }
    tbl_ppi <- clamp_tbl(tbl_ppi)
    tbl_hum <- clamp_tbl(tbl_hum)
    tbl_naive <- clamp_tbl(tbl_naive)
  }

  new_llm_ppi(
    subclass = "llm_ppi_proportion",
    estimates = tbl_ppi,
    benchmarks = rbind(tbl_ppi, tbl_hum, tbl_naive),
    theta = est_ppi,
    vcov_mat = V_ppi,
    lambda = lam_vec,
    method_label = method_label,
    n_labeled = n,
    n_unlabeled = N,
    conf_level = conf_level,
    df = df_ref,
    extra = list(
      truth_col = truth_col,
      pred_col = pred_col,
      categories = cat_lvls,
      simultaneous = simultaneous,
      clamp_ci = clamp_ci,
      small_sample = small_sample
    )
  )
}
