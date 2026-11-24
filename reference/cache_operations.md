# Retrieve, Store, Clear, or Inspect a Coding Cache

Low-level helpers operating on an
[`coding_cache()`](https://marcusharrisphd.com/llmmetric/reference/coding_cache.md)
object. All keys are validated strictly against path traversal and
unsafe characters; disk cache writes are atomic and integrity-checked
via SHA-256 checksums.

## Usage

``` r
cache_get(cache, key)

cache_set(cache, key, value)

cache_clear(cache)

cache_stats(cache)
```

## Arguments

- cache:

  An object of class `"llm_coding_cache"`.

- key:

  Character cache key (typically produced by
  [`cache_key()`](https://marcusharrisphd.com/llmmetric/reference/cache_key.md)).

- value:

  R object to store in the cache.

## Value

- `cache_get()` returns the cached object or `NULL` on a miss (or after
  gracefully evicting a corrupted disk entry).

- `cache_set()` invisibly returns `TRUE` if written, `FALSE` if caching
  is disabled.

- `cache_clear()` invisibly returns the cache object.

- `cache_stats()` returns a named list with `type`, `dir`, `hits`,
  `misses`, `writes`, `evictions`, and `entries`.

## Examples

``` r
mc <- coding_cache("memory")
k <- cache_key("u1", "p1", "m1")
cache_set(mc, k, list(score = 4.5))
cache_get(mc, k)
#> $score
#> [1] 4.5
#> 
cache_stats(mc)
#> $type
#> [1] "memory"
#> 
#> $dir
#> NULL
#> 
#> $hits
#> [1] 1
#> 
#> $misses
#> [1] 0
#> 
#> $writes
#> [1] 1
#> 
#> $evictions
#> [1] 0
#> 
#> $entries
#> [1] 1
#> 
cache_clear(mc)
```
