# Create a Content-Addressed Response Cache for LLM Coding

Creates a memory-backed, disk-backed, or disabled cache keyed by SHA-256
hashes of
`(schema, unit_id, prompt, model, temperature, replicate, backend)`.
Disk caches enforce path-safe canonical keys (blocking directory
traversal), atomic temporary-file + rename writes, and SHA-256 payload
integrity checksums with automatic eviction of corrupted entries.

## Usage

``` r
coding_cache(type = c("memory", "disk", "none"), dir = NULL)
```

## Arguments

- type:

  Storage mode: `"memory"` (default, session-scoped), `"disk"`
  (persistent checksummed `.rds` files in `dir`), or `"none"` (caching
  disabled).

- dir:

  Directory path for `"disk"` caching. Must be explicitly supplied when
  `type = "disk"` (created automatically if it does not exist).

## Value

An object of class `"llm_coding_cache"`.

## Examples

``` r
mem_cache <- coding_cache("memory")
cache_stats(mem_cache)
#> $type
#> [1] "memory"
#> 
#> $dir
#> NULL
#> 
#> $hits
#> [1] 0
#> 
#> $misses
#> [1] 0
#> 
#> $writes
#> [1] 0
#> 
#> $evictions
#> [1] 0
#> 
#> $entries
#> [1] 0
#> 
```
