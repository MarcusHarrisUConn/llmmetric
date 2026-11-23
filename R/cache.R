#' Create a Content-Addressed Response Cache for LLM Coding
#'
#' Creates a memory-backed, disk-backed, or disabled cache keyed by SHA-256
#' hashes of `(schema, unit_id, prompt, model, temperature, replicate, backend)`.
#' Caching guarantees exact reproducibility and eliminates redundant API calls
#' across repeated runs.
#'
#' @param type Storage mode: `"memory"` (default, session-scoped), `"disk"`
#'   (persistent `.rds` files in `dir`), or `"none"` (caching disabled).
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
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    dir <- normalizePath(dir, mustWork = TRUE)
  }

  state <- new.env(parent = emptyenv())
  state$store <- new.env(parent = emptyenv())
  state$hits <- 0L
  state$misses <- 0L
  state$writes <- 0L

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
#' Low-level helpers operating on an [coding_cache()] object.
#'
#' @param cache An object of class `"llm_coding_cache"`.
#' @param key Character cache key (typically produced by [cache_key()]).
#' @param value R object to store in the cache.
#'
#' @return
#' - `cache_get()` returns the cached object or `NULL` on a miss.
#' - `cache_set()` invisibly returns `TRUE` if written, `FALSE` if caching is disabled.
#' - `cache_clear()` invisibly returns the cache object.
#' - `cache_stats()` returns a named list with `type`, `hits`, `misses`, `writes`,
#'   and `entries`.
#' @name cache_operations
#' @export
cache_get <- function(cache, key) {
  if (!inherits(cache, "llm_coding_cache")) {
    cli::cli_abort("{.arg cache} must be an {.cls llm_coding_cache} object.")
  }
  if (cache$type == "none") {
    cache$state$misses <- cache$state$misses + 1L
    return(NULL)
  }
  if (cache$type == "memory") {
    if (exists(key, envir = cache$state$store, inherits = FALSE)) {
      cache$state$hits <- cache$state$hits + 1L
      return(get(key, envir = cache$state$store, inherits = FALSE))
    }
    cache$state$misses <- cache$state$misses + 1L
    return(NULL)
  }

  # disk cache
  path <- file.path(cache$dir, paste0(key, ".rds"))
  if (file.exists(path)) {
    val <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(val)) {
      cache$state$hits <- cache$state$hits + 1L
      return(val)
    }
  }
  cache$state$misses <- cache$state$misses + 1L
  NULL
}

#' @rdname cache_operations
#' @export
cache_set <- function(cache, key, value) {
  if (!inherits(cache, "llm_coding_cache")) {
    cli::cli_abort("{.arg cache} must be an {.cls llm_coding_cache} object.")
  }
  if (cache$type == "none") {
    return(invisible(FALSE))
  }
  if (cache$type == "memory") {
    assign(key, value, envir = cache$state$store)
    cache$state$writes <- cache$state$writes + 1L
    return(invisible(TRUE))
  }

  path <- file.path(cache$dir, paste0(key, ".rds"))
  saveRDS(value, file = path)
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
    "Entries: {st$entries} | Hits: {st$hits} | Misses: {st$misses} | Writes: {st$writes}"
  )
  invisible(x)
}
