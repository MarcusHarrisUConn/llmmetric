# Compute Content-Addressed SHA-256 Cache Key

Compute Content-Addressed SHA-256 Cache Key

## Usage

``` r
cache_key(...)
```

## Arguments

- ...:

  Arbitrary R objects defining a unique coding request.

## Value

A 64-character hexadecimal SHA-256 hash string.

## Examples

``` r
cache_key(unit = "u1", prompt = "p1", model = "gpt-4o-mini", temperature = 0)
#> [1] "269bbd3e5533c37ee86d1e1c41a7cb3c3a6098435ecd29e9e2cdf3ac12122757"
```
