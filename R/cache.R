#' Create a Content-Addressed Response Cache for LLM Coding
#'
#' Creates a memory-backed, disk-backed, or disabled cache keyed by SHA-256
#' hashes of `(schema, unit_id, prompt, model, temperature, replicate, backend)`.
#' Disk caches enforce path-safe canonical keys (blocking directory traversal),
#' atomic temporary-file + rename writes, and SHA-256 payload integrity
#' checksums with automatic eviction of corrupted entries.
#'
#' @param type Storage mode: `"memory"` (default, session-scoped), `"disk"`
#'   (persistent checksummed `.rds` files in `dir`), or `"none"` (caching
#'   disabled).
#' @param dir Directory path for `"disk"` caching. Must be explicitly supplied
#'   when `type = "disk"` (created automatically if it does not exist).
#'
#' @return An object of class `"llm_coding_cache"`.
#' @export
#'
#' @examples
#' mem_cache <- coding_cache("memory")
#' cache_stats(mem_cache)
coding_cache <- function(type = c("memory", "disk", "none"), dir = NULL) {
  type <- match.arg(type)
  if (type == "disk") {
    assert_scalar_character(dir, "dir")
    if (grepl("[[:cntrl:]]", dir)) {
      cli::cli_abort("{.arg dir} must not contain control characters.")
    }
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  }

  state <- new.env(parent = emptyenv())
  state$store <- new.env(parent = emptyenv())
  state$hits <- 0L
  state$misses <- 0L
  state$writes <- 0L
  state$evictions <- 0L

  structure(
    list(
      type = type,
      dir = dir,
      state = state
    ),
    class = "llm_coding_cache"
  )
}

#' Compute Content-Addressed SHA-256 Cache Key
#'
#' @param ... Arbitrary R objects defining a unique coding request.
#' @return A 64-character hexadecimal SHA-256 hash string.
#' @export
#'
#' @examples
#' cache_key(unit = "u1", prompt = "p1", model = "gpt-4o-mini", temperature = 0)
cache_key <- function(...) {
  digest::digest(list(...), algo = "sha256")
}

#' Retrieve, Store, Clear, or Inspect a Coding Cache
#'
#' Low-level helpers operating on an [coding_cache()] object. All keys are
#' validated strictly against path traversal and unsafe characters; disk cache
#' writes are atomic and integrity-checked via SHA-256 checksums.
#'
#' @param cache An object of class `"llm_coding_cache"`.
#' @param key Character cache key (typically produced by [cache_key()]).
#' @param value R object to store in the cache.
#'
#' @return
#' - `cache_get()` returns the cached object or `NULL` on a miss (or after
#'   gracefully evicting a corrupted disk entry).
#' - `cache_set()` invisibly returns `TRUE` if written, `FALSE` if caching is
#'   disabled.
#' - `cache_clear()` invisibly returns the cache object.
#' - `cache_stats()` returns a named list with `type`, `dir`, `hits`, `misses`,
#'   `writes`, `evictions`, and `entries`.
#' @name cache_operations
#' @export
#'
#' @examples
#' mc <- coding_cache("memory")
#' k <- cache_key("u1", "p1", "m1")
#' cache_set(mc, k, list(score = 4.5))
#' cache_get(mc, k)
#' cache_stats(mc)
#' cache_clear(mc)
cache_get <- function(cache, key) {
  if (!inherits(cache, "llm_coding_cache")) {
    cli::cli_abort("{.arg cache} must be an {.cls llm_coding_cache} object.")
  }
  safe_key <- validate_cache_key_safe(key, arg = "key")

  if (cache$type == "none") {
    cache$state$misses <- cache$state$misses + 1L
    return(NULL)
  }
  if (cache$type == "memory") {
    if (exists(safe_key, envir = cache$state$store, inherits = FALSE)) {
      cache$state$hits <- cache$state$hits + 1L
      return(get(safe_key, envir = cache$state$store, inherits = FALSE))
    }
    cache$state$misses <- cache$state$misses + 1L
    return(NULL)
  }

  # Hardened disk cache with integrity verification and corruption eviction
  path <- resolve_safe_cache_path(cache$dir, safe_key)
  if (!file.exists(path)) {
    cache$state$misses <- cache$state$misses + 1L
    return(NULL)
  }

  envelope <- tryCatch(readRDS(path), error = function(e) NULL)
  valid_env <- is.list(envelope) &&
    identical(envelope$llmmetric_cache_v, 2L) &&
    identical(envelope$key, safe_key) &&
    is.character(envelope$checksum) &&
    length(envelope$checksum) == 1L &&
    "payload" %in% names(envelope)

  if (valid_env) {
    recomputed <- digest::digest(envelope$payload, algo = "sha256")
    if (identical(recomputed, envelope$checksum)) {
      cache$state$hits <- cache$state$hits + 1L
      return(envelope$payload)
    }
  }

  # Corrupted or tampered entry: evict cleanly and treat as miss
  unlink(path)
  cache$state$evictions <- cache$state$evictions + 1L
  cache$state$misses <- cache$state$misses + 1L
  cli::cli_warn(
    "Evicted corrupted or integrity-mismatched disk cache entry {.val {safe_key}}."
  )
  NULL
}

#' @rdname cache_operations
#' @export
cache_set <- function(cache, key, value) {
  if (!inherits(cache, "llm_coding_cache")) {
    cli::cli_abort("{.arg cache} must be an {.cls llm_coding_cache} object.")
  }
  safe_key <- validate_cache_key_safe(key, arg = "key")

  if (cache$type == "none") {
    return(invisible(FALSE))
  }
  if (cache$type == "memory") {
    assign(safe_key, value, envir = cache$state$store)
    cache$state$writes <- cache$state$writes + 1L
    return(invisible(TRUE))
  }

  path <- resolve_safe_cache_path(cache$dir, safe_key)
  envelope <- list(
    llmmetric_cache_v = 2L,
    key = safe_key,
    checksum = digest::digest(value, algo = "sha256"),
    payload = value
  )

  # Atomic write via unique temp file in same directory + file.rename()
  tmp_suffix <- substr(
    digest::digest(list(Sys.getpid(), safe_key, cache$state$writes, proc.time()[["elapsed"]]), algo = "sha256"),
    1L, 12L
  )
  tmp_path <- file.path(cache$dir, paste0(".", safe_key, "_", tmp_suffix, ".tmp"))
  on.exit(if (file.exists(tmp_path)) unlink(tmp_path), add = TRUE)

  saveRDS(envelope, file = tmp_path)
  suppressWarnings(Sys.chmod(tmp_path, mode = "0600"))
  ok <- file.rename(tmp_path, path)
  if (!isTRUE(ok)) {
    # Fallback overwrite if target existed on platforms where rename does not replace
    ok <- file.copy(tmp_path, path, overwrite = TRUE)
    unlink(tmp_path)
  }
  if (!isTRUE(ok)) {
    cli::cli_abort("Failed to atomically persist disk cache entry {.val {safe_key}}.")
  }
  suppressWarnings(Sys.chmod(path, mode = "0600"))

  cache$state$writes <- cache$state$writes + 1L
  invisible(TRUE)
}

#' @rdname cache_operations
#' @export
cache_clear <- function(cache) {
  if (!inherits(cache, "llm_coding_cache")) {
    cli::cli_abort("{.arg cache} must be an {.cls llm_coding_cache} object.")
  }
  if (cache$type == "memory") {
    rm(list = ls(envir = cache$state$store, all.names = TRUE), envir = cache$state$store)
  } else if (cache$type == "disk" && !is.null(cache$dir) && dir.exists(cache$dir)) {
    rds_files <- list.files(cache$dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(rds_files) > 0L) {
      unlink(rds_files)
    }
  }
  cache$state$hits <- 0L
  cache$state$misses <- 0L
  cache$state$writes <- 0L
  cache$state$evictions <- 0L
  invisible(cache)
}

#' @rdname cache_operations
#' @export
cache_stats <- function(cache) {
  if (!inherits(cache, "llm_coding_cache")) {
    cli::cli_abort("{.arg cache} must be an {.cls llm_coding_cache} object.")
  }
  entries <- if (cache$type == "memory") {
    length(ls(envir = cache$state$store, all.names = TRUE))
  } else if (cache$type == "disk" && !is.null(cache$dir) && dir.exists(cache$dir)) {
    length(list.files(cache$dir, pattern = "\\.rds$"))
  } else {
    0L
  }
  list(
    type = cache$type,
    dir = cache$dir,
    hits = cache$state$hits,
    misses = cache$state$misses,
    writes = cache$state$writes,
    evictions = cache$state$evictions,
    entries = entries
  )
}

#' @export
print.llm_coding_cache <- function(x, ...) {
  st <- cache_stats(x)
  cli::cli_h3("LLM Coding Cache ({st$type})")
  if (identical(st$type, "disk")) {
    cli::cli_text("Directory: {.path {st$dir}}")
  }
  cli::cli_text(
    "Entries: {st$entries} | Hits: {st$hits} | Misses: {st$misses} | Writes: {st$writes} | Evictions: {st$evictions}"
  )
  invisible(x)
}
