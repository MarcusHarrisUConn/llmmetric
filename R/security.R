# Centralized security, secret-redaction, URL/cache hardening, and untrusted JSON extraction

#' Validate an HTTP/HTTPS LLM API endpoint URL strictly
#'
#' Enforces `http://` or `https://` scheme, rejects control characters,
#' whitespace, embedded credentials (`user:pass@host`), and empty hosts.
#' @noRd
validate_endpoint_url <- function(url, arg = "base_url") {
  assert_scalar_character(url, arg)
  if (grepl("[[:cntrl:][:space:]]", url)) {
    cli::cli_abort("{.arg {arg}} must not contain whitespace or control characters.")
  }
  if (!grepl("^https?://", url, ignore.case = TRUE)) {
    cli::cli_abort(
      "{.arg {arg}} must use the {.val http://} or {.val https://} scheme (rejected unsafe URL)."
    )
  }
  # Strip scheme and inspect authority component (before first '/' or '?' or '#')
  rest <- sub("^https?://", "", url, ignore.case = TRUE)
  authority <- sub("[/?#].*$", "", rest)
  if (!nzchar(authority)) {
    cli::cli_abort("{.arg {arg}} must specify a non-empty host.")
  }
  if (grepl("@", authority, fixed = TRUE)) {
    cli::cli_abort("{.arg {arg}} must not embed userinfo credentials (`user:pass@`) in the URL.")
  }
  # Validate host (strip optional IPv6 brackets or :port)
  host <- if (startsWith(authority, "[")) {
    sub("\\](:[0-9]+)?$", "", sub("^\\[", "", authority))
  } else {
    sub(":[0-9]+$", "", authority)
  }
  if (!nzchar(host) || grepl("[^A-Za-z0-9._:-]", host)) {
    cli::cli_abort("{.arg {arg}} contains an invalid host specification: {.val {host}}.")
  }
  # Check optional port range if present
  if (!startsWith(authority, "[") && grepl(":", authority, fixed = TRUE)) {
    port_str <- sub("^[^:]+:", "", authority)
    if (!grepl("^[0-9]+$", port_str)) {
      cli::cli_abort("{.arg {arg}} contains an invalid port: {.val {port_str}}.")
    }
    port_num <- suppressWarnings(as.integer(port_str))
    if (is.na(port_num) || port_num < 1L || port_num > 65535L) {
      cli::cli_abort("{.arg {arg}} port must be between 1 and 65535.")
    }
  }
  sub("/+$", "", url)
}

#' Validate user-supplied extra HTTP headers against CRLF / control-char injection
#' @noRd
validate_http_headers <- function(headers, arg = "extra_headers") {
  if (is.null(headers) || length(headers) == 0L) {
    return(list())
  }
  if (!is.list(headers) || is.null(names(headers)) || any(!nzchar(names(headers)))) {
    cli::cli_abort("{.arg {arg}} must be a named list of scalar header values.")
  }
  for (nm in names(headers)) {
    if (grepl("[[:cntrl:][:space:]:]", nm)) {
      cli::cli_abort("Invalid HTTP header name in {.arg {arg}}: contains forbidden characters.")
    }
    val <- headers[[nm]]
    if (!is.character(val) || length(val) != 1L || is.na(val)) {
      cli::cli_abort("HTTP header {.val {nm}} in {.arg {arg}} must be a single non-NA character string.")
    }
    if (grepl("[[:cntrl:]]", val)) {
      cli::cli_abort("HTTP header {.val {nm}} in {.arg {arg}} must not contain control/newline characters.")
    }
  }
  headers
}

# Session-scoped non-serialized in-memory vault for API secrets so saved objects
# (saveRDS / dput / str / print) never contain raw credentials on disk.
.llmmetric_secret_vault <- new.env(parent = emptyenv())

#' Wrap an API secret in a non-serializing redacted handle to prevent leakage
#' @noRd
new_redacted_secret <- function(value, env_var = NULL) {
  if (is.null(value)) value <- ""
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    cli::cli_abort("{.arg api_key} must be a single character string.")
  }
  if (grepl("[[:cntrl:]]", value)) {
    cli::cli_abort("{.arg api_key} must not contain control or newline characters.")
  }
  token_id <- paste0(
    "sec_",
    substr(digest::digest(list(Sys.getpid(), proc.time()[["elapsed"]], length(ls(.llmmetric_secret_vault))), algo = "sha256"), 1L, 16L)
  )
  assign(token_id, value, envir = .llmmetric_secret_vault)

  structure(
    list(
      token_id = token_id,
      env_var = env_var,
      configured = nzchar(value)
    ),
    class = "llm_redacted_secret"
  )
}

#' Safely extract underlying secret value for outbound Authorization header only
#' @noRd
reveal_secret <- function(secret_obj) {
  if (inherits(secret_obj, "llm_redacted_secret")) {
    if (!is.null(secret_obj$token_id) &&
        exists(secret_obj$token_id, envir = .llmmetric_secret_vault, inherits = FALSE)) {
      return(get(secret_obj$token_id, envir = .llmmetric_secret_vault, inherits = FALSE))
    }
    if (!is.null(secret_obj$env_var) && nzchar(secret_obj$env_var)) {
      return(Sys.getenv(secret_obj$env_var, unset = ""))
    }
    return("")
  }
  if (is.character(secret_obj) && length(secret_obj) == 1L) {
    return(secret_obj)
  }
  ""
}

#' @export
format.llm_redacted_secret <- function(x, ...) {
  if (isTRUE(x$configured)) "<redacted>" else "<empty>"
}

#' @export
as.character.llm_redacted_secret <- function(x, ...) {
  format(x)
}

#' @export
print.llm_redacted_secret <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' Scrub known secret strings and Bearer tokens from error or diagnostic messages
#' @noRd
redact_secrets <- function(text, secrets = character()) {
  if (!is.character(text) || length(text) == 0L) {
    return(text)
  }
  out <- text
  secrets <- unique(secrets[!is.na(secrets) & nzchar(secrets)])
  for (sec in secrets) {
    out <- gsub(sec, "<redacted>", out, fixed = TRUE)
  }
  out <- gsub("Bearer\\s+[A-Za-z0-9._~+/=-]+", "Bearer <redacted>", out, ignore.case = TRUE)
  out
}

#' Validate a cache key is strictly path-safe (no traversal, separators, or control chars)
#' @noRd
validate_cache_key_safe <- function(key, arg = "key") {
  assert_scalar_character(key, arg)
  if (grepl("[/\\\\]", key) || grepl("\\.\\.", key) || grepl("[[:cntrl:][:space:]]", key)) {
    cli::cli_abort(
      "Unsafe cache key rejected in {.arg {arg}}: path separators, traversal sequences, and control characters are forbidden."
    )
  }
  if (!grepl("^[A-Za-z0-9_-]{1,128}$", key)) {
    cli::cli_abort(
      "Invalid cache key in {.arg {arg}}: keys must contain only alphanumeric, underscore, or hyphen characters (length 1-128)."
    )
  }
  key
}

#' Resolve a disk cache file path and verify it stays strictly within the cache directory
#' @noRd
resolve_safe_cache_path <- function(cache_dir, key) {
  safe_key <- validate_cache_key_safe(key)
  dir_norm <- normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
  candidate <- file.path(dir_norm, paste0(safe_key, ".rds"))
  parent_norm <- normalizePath(dirname(candidate), winslash = "/", mustWork = TRUE)
  if (!identical(parent_norm, dir_norm)) {
    cli::cli_abort("Resolved cache path escapes the configured cache directory.")
  }
  candidate
}

#' Bounded jittered exponential backoff without mutating global .Random.seed
#' @noRd
jittered_backoff <- function(attempt, base = 1, cap = 30, salt = 0L) {
  raw_wait <- min(base * (2^(max(attempt - 1L, 0L))), cap)
  if (raw_wait <= 0) {
    return(0)
  }
  # Deterministic-safe pseudo-jitter derived from PID + timestamp + attempt
  s <- hash_to_seed("backoff", Sys.getpid(), attempt, salt)
  jitter_mult <- with_local_seed(s, stats::runif(1L, min = 0.75, max = 1.25))
  min(raw_wait * jitter_mult, cap)
}

#' Enforce bounded HTTP response payload size
#' @noRd
check_response_size <- function(raw_bytes, max_bytes) {
  if (is.raw(raw_bytes)) {
    n_bytes <- length(raw_bytes)
  } else if (is.character(raw_bytes)) {
    n_bytes <- sum(nchar(raw_bytes, type = "bytes"), na.rm = TRUE)
  } else {
    n_bytes <- 0L
  }
  if (n_bytes > max_bytes) {
    cli::cli_abort(
      "Backend response size ({n_bytes} bytes) exceeds maximum allowed limit ({max_bytes} bytes)."
    )
  }
  invisible(TRUE)
}

#' Detect duplicate keys in a JSON object string before/after parsing
#'
#' Prevents JSON key-smuggling / parser-differential attacks such as
#' `{"score": 5, "score": 999}`.
#' @noRd
has_duplicate_json_keys <- function(parsed) {
  if (!is.list(parsed)) {
    return(FALSE)
  }
  nms <- names(parsed)
  if (!is.null(nms) && any(duplicated(nms))) {
    return(TRUE)
  }
  any(vapply(parsed, has_duplicate_json_keys, logical(1L)))
}

#' Safely extract a single JSON object string from untrusted LLM text output
#'
#' Handles:
#' - Raw JSON objects (`{"score": 4}`)
#' - Single markdown-fenced JSON blocks (with or without surrounding prose)
#' - Strictly rejects oversized responses, embedded NUL/control bytes, and
#'   ambiguous multi-block / injected conflicting JSON objects.
#' @noRd
extract_json_payload <- function(txt, max_bytes = 262144L) {
  if (!is.character(txt) || length(txt) != 1L || is.na(txt)) {
    return(list(ok = FALSE, json = NULL, error = "Response content must be a single non-NA string."))
  }
  n_bytes <- nchar(txt, type = "bytes")
  if (n_bytes > max_bytes) {
    return(list(
      ok = FALSE,
      json = NULL,
      error = paste0("Response exceeds maximum allowed JSON size (", max_bytes, " bytes).")
    ))
  }
  if (grepl("[\x01-\x08\x0b\x0c\x0e-\x1f]", txt)) {
    return(list(ok = FALSE, json = NULL, error = "Response contains disallowed control characters."))
  }

  trimmed <- trimws(txt)
  if (!nzchar(trimmed)) {
    return(list(ok = FALSE, json = NULL, error = "Response is empty."))
  }

  # Case 1: Pure JSON object directly (starts with '{' and ends with '}')
  if (startsWith(trimmed, "{") && endsWith(trimmed, "}")) {
    return(list(ok = TRUE, json = trimmed, error = NULL))
  }

  # Case 2: Extract markdown fenced blocks (```json ... ``` or ``` ... ```)
  fence_pattern <- "```(?:[a-zA-Z0-9_-]+)?\\s*([\\s\\S]*?)```"
  m <- gregexpr(fence_pattern, trimmed, perl = TRUE)
  matches <- regmatches(trimmed, m)[[1L]]
  if (length(matches) == 1L) {
    inner <- sub("^```(?:[a-zA-Z0-9_-]+)?\\s*", "", matches[1L], perl = TRUE)
    inner <- trimws(sub("\\s*```$", "", inner, perl = TRUE))
    if (startsWith(inner, "{") && endsWith(inner, "}")) {
      return(list(ok = TRUE, json = inner, error = NULL))
    }
    return(list(ok = FALSE, json = NULL, error = "Fenced block did not contain a JSON object."))
  } else if (length(matches) > 1L) {
    return(list(
      ok = FALSE,
      json = NULL,
      error = "Ambiguous response containing multiple fenced code blocks (rejected to prevent prompt injection)."
    ))
  }

  # Case 3: Scan for balanced top-level `{ ... }` JSON object(s) in prose
  chars <- strsplit(trimmed, "", fixed = TRUE)[[1L]]
  depth <- 0L
  in_str <- FALSE
  esc <- FALSE
  start_idx <- NA_integer_
  spans <- list()

  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (in_str) {
      if (esc) {
        esc <- FALSE
      } else if (ch == "\\") {
        esc <- TRUE
      } else if (ch == "\"") {
        in_str <- FALSE
      }
      next
    }
    if (ch == "\"") {
      in_str <- TRUE
    } else if (ch == "{") {
      if (depth == 0L) start_idx <- i
      depth <- depth + 1L
    } else if (ch == "}") {
      depth <- depth - 1L
      if (depth < 0L) {
        return(list(ok = FALSE, json = NULL, error = "Malformed JSON braces in response."))
      }
      if (depth == 0L && !is.na(start_idx)) {
        spans[[length(spans) + 1L]] <- c(start_idx, i)
        start_idx <- NA_integer_
      }
    }
  }

  if (depth != 0L || length(spans) == 0L) {
    return(list(ok = FALSE, json = NULL, error = "JSON parse error: no valid JSON object found."))
  }
  if (length(spans) > 1L) {
    return(list(
      ok = FALSE,
      json = NULL,
      error = "Ambiguous response containing multiple JSON objects (rejected to prevent prompt injection)."
    ))
  }

  sp <- spans[[1L]]
  candidate <- paste(chars[sp[1L]:sp[2L]], collapse = "")
  list(ok = TRUE, json = candidate, error = NULL)
}
