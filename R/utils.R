# Internal math, RNG, and formatting utilities for llmmetric

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
  dn <- dimnames(A)
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
  if (!is.null(dn)) {
    dimnames(pinv) <- list(dn[[2L]], dn[[1L]])
  }
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

#' Render simple `{key}` template placeholders in a single pass
#'
#' Scans only the original `template` for `{key}` tokens so untrusted values
#' (such as `{text}` containing `{ssn}` or `{human_score}`) can never trigger
#' second-order interpolation of other dataframe columns or R code.
#' @noRd
render_prompt_template <- function(template, values) {
  if (!is.character(template) || length(template) != 1L || is.na(template) || !nzchar(template)) {
    return(template)
  }
  m <- gregexpr("\\{[A-Za-z0-9_.]+\\}", template, perl = TRUE)[[1L]]
  if (length(m) == 1L && m[1L] == -1L) {
    return(template)
  }
  starts <- as.integer(m)
  lens <- as.integer(attr(m, "match.length"))
  ends <- starts + lens - 1L
  n_chr <- nchar(template, type = "chars")
  val_names <- names(values) %||% character()

  gap_starts <- c(1L, ends + 1L)
  gap_ends <- c(starts - 1L, n_chr)
  gaps <- ifelse(gap_starts <= gap_ends, substring(template, gap_starts, gap_ends), "")

  repls <- vapply(seq_along(starts), function(i) {
    tok <- substr(template, starts[i], ends[i])
    nm <- substr(tok, 2L, nchar(tok, type = "chars") - 1L)
    if (!(nm %in% val_names)) {
      return(tok)
    }
    val <- values[[nm]]
    if (is.null(val) || length(val) == 0L || is.na(val[1L])) {
      ""
    } else {
      as.character(val[1L])
    }
  }, character(1L), USE.NAMES = FALSE)

  pieces <- character(2L * length(starts) + 1L)
  pieces[seq.int(1L, 2L * length(starts) + 1L, by = 2L)] <- gaps
  pieces[seq.int(2L, 2L * length(starts), by = 2L)] <- repls
  paste(pieces, collapse = "")
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
