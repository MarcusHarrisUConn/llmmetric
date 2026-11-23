# Centralized strict argument validation and numerical safety guards

#' Extract tabular data from an `llm_coded_data` object or data.frame
#' @noRd
extract_data_frame <- function(x, arg = "data") {
  if (inherits(x, "llm_coded_data")) {
    return(tibble::as_tibble(x$scores))
  }
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a data.frame or {.cls llm_coded_data} object.")
  }
  tibble::as_tibble(x)
}

#' Check that required columns exist in a data.frame
#' @noRd
assert_columns <- function(df, cols, arg = "data") {
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "{.arg {arg}} is missing required column{?s}: {.val {missing_cols}}."
    )
  }
  invisible(TRUE)
}

#' Check scalar numeric argument
#' @noRd
assert_scalar_numeric <- function(x, arg, min = -Inf, max = Inf, allow_null = FALSE) {
  if (is.null(x) && allow_null) {
    return(invisible(TRUE))
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    cli::cli_abort("{.arg {arg}} must be a single finite numeric value.")
  }
  if (x < min || x > max) {
    cli::cli_abort("{.arg {arg}} must be between {min} and {max} (got {x}).")
  }
  invisible(TRUE)
}

#' Check scalar character argument
#' @noRd
assert_scalar_character <- function(x, arg, allow_null = FALSE) {
  if (is.null(x) && allow_null) {
    return(invisible(TRUE))
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    cli::cli_abort("{.arg {arg}} must be a single non-empty character string.")
  }
  invisible(TRUE)
}

#' Check logical flag
#' @noRd
assert_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be TRUE or FALSE.")
  }
  invisible(TRUE)
}

#' Verify all non-NA entries of a numeric vector/matrix are finite (no NaN/Inf)
#' @noRd
assert_finite_values <- function(x, arg) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be numeric.")
  }
  bad <- !is.na(x) & !is.finite(x)
  if (any(bad)) {
    cli::cli_abort(
      "{.arg {arg}} contains {sum(bad)} non-finite value{?s} ({.code Inf}/{.code -Inf}/{.code NaN})."
    )
  }
  invisible(TRUE)
}

#' Check whether a numeric vector is numerically constant (zero variance)
#' @noRd
is_constant_numeric <- function(x, tol = 1e-12) {
  vals <- as.numeric(x)
  vals <- vals[is.finite(vals)]
  if (length(vals) <= 1L) {
    return(TRUE)
  }
  scale_ref <- max(1, abs(mean(vals)))
  (max(vals) - min(vals)) <= tol * scale_ref
}

#' Check design matrix rank via QR pivoting and identify aliased/collinear columns
#'
#' Checks rank on labeled and unlabeled design matrices jointly so dropped
#' aliased columns are synchronized across both samples.
#' @noRd
check_design_rank <- function(X_lab, X_unlab = NULL, tol = 1e-7) {
  X_lab <- as.matrix(X_lab)
  p <- ncol(X_lab)
  col_names <- colnames(X_lab)
  if (is.null(col_names)) {
    col_names <- paste0("x", seq_len(p))
    colnames(X_lab) <- col_names
  }

  qr_L <- qr(X_lab, tol = tol)
  keep_idx <- sort(qr_L$pivot[seq_len(qr_L$rank)])

  # Also check unlabeled matrix rank if provided (keep intersection of identifiable cols)
  if (!is.null(X_unlab)) {
    X_u_sub <- as.matrix(X_unlab)[, keep_idx, drop = FALSE]
    qr_U <- qr(X_u_sub, tol = tol)
    if (qr_U$rank < length(keep_idx)) {
      keep_idx <- sort(keep_idx[qr_U$pivot[seq_len(qr_U$rank)]])
    }
  }

  aliased_idx <- setdiff(seq_len(p), keep_idx)
  list(
    full_rank = (length(aliased_idx) == 0L),
    rank = length(keep_idx),
    keep_idx = keep_idx,
    aliased_idx = aliased_idx,
    keep_cols = col_names[keep_idx],
    aliased_cols = col_names[aliased_idx],
    all_cols = col_names
  )
}

#' Expand reduced-rank parameter vector and covariance back to full term list with NAs on aliased slots
#' @noRd
expand_aliased_results <- function(theta_sub, vcov_sub, rank_info) {
  if (isTRUE(rank_info$full_rank)) {
    return(list(theta = theta_sub, vcov = vcov_sub))
  }
  all_cols <- rank_info$all_cols
  p <- length(all_cols)
  theta_full <- stats::setNames(rep(NA_real_, p), all_cols)
  theta_full[rank_info$keep_cols] <- theta_sub

  vcov_full <- matrix(NA_real_, nrow = p, ncol = p, dimnames = list(all_cols, all_cols))
  vcov_full[rank_info$keep_cols, rank_info$keep_cols] <- vcov_sub
  list(theta = theta_full, vcov = vcov_full)
}
