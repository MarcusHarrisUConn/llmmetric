# Internal utility helpers for llmmetric

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

#' Deterministically hash arbitrary R values to a valid positive 32-bit R seed
#' @noRd
hash_to_seed <- function(...) {
  h <- digest::digest(list(...), algo = "sha256")
  # Take 7 hex digits (28 bits -> max 268,435,455, safely within signed 32-bit int)
  val <- strtoi(substr(h, 1L, 7L), base = 16L)
  if (is.na(val) || val == 0L) {
    val <- 1L
  }
  as.integer(val)
}

#' Evaluate expression with a deterministic seed without mutating global RNG state
#' @noRd
with_local_seed <- function(seed, expr) {
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_seed) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(expr)
}

#' Robust matrix inversion / linear solver with Moore-Penrose fallback
#' @noRd
safe_solve <- function(A, b = NULL, tol = 1e-10) {
  A <- as.matrix(A)
  res <- tryCatch(
    if (is.null(b)) solve(A) else solve(A, b),
    error = function(e) NULL
  )
  if (!is.null(res) && all(is.finite(res))) {
    return(res)
  }
  sv <- svd(A)
  d_inv <- ifelse(sv$d > max(sv$d, 0) * tol, 1 / sv$d, 0)
  pinv <- sv$v %*% (d_inv * t(sv$u))
  if (is.null(b)) pinv else pinv %*% b
}

#' Compute centered sample covariance matrix safely even for 1-column matrices
#' @noRd
safe_cov <- function(M) {
  M <- as.matrix(M)
  n <- nrow(M)
  p <- ncol(M)
  if (n <= 1L) {
    return(matrix(0, nrow = p, ncol = p, dimnames = list(colnames(M), colnames(M))))
  }
  Mc <- sweep(M, 2L, colMeans(M), "-")
  crossprod(Mc) / (n - 1)
}

#' Compute centered cross-covariance between two n x p matrices
#' @noRd
safe_cross_cov <- function(A, B) {
  A <- as.matrix(A)
  B <- as.matrix(B)
  n <- nrow(A)
  if (n <= 1L) {
    return(matrix(0, nrow = ncol(A), ncol = ncol(B)))
  }
  Ac <- sweep(A, 2L, colMeans(A), "-")
  Bc <- sweep(B, 2L, colMeans(B), "-")
  crossprod(Ac, Bc) / (n - 1)
}

#' Render simple `{key}` template placeholders without evaluating arbitrary R code
#' @noRd
render_prompt_template <- function(template, values) {
  out <- template
  for (nm in names(values)) {
    placeholder <- paste0("{", nm, "}")
    if (grepl(placeholder, out, fixed = TRUE)) {
      val <- values[[nm]]
      val_str <- if (is.null(val) || length(val) == 0L || is.na(val[1L])) {
        ""
      } else {
        as.character(val[1L])
      }
      out <- gsub(placeholder, val_str, out, fixed = TRUE)
    }
  }
  out
}

#' Format numeric vector cleanly for print methods
#' @noRd
fmt_num <- function(x, digits = 3L) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

#' Format p-value cleanly
#' @noRd
fmt_pval <- function(p) {
  ifelse(
    is.na(p),
    "NA",
    ifelse(p < 0.0001, "<0.0001", formatC(p, format = "f", digits = 4L))
  )
}
