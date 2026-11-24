# Validate and Parse Structured LLM Output Against a Coding Schema

Parses a JSON string (hardened against prompt-injected multi-block
payloads, duplicate keys, oversized responses, control characters, and
non-finite values) or validates an already-parsed R named list against
an
[`coding_schema()`](https://marcusharrisphd.com/llmmetric/reference/coding_schema.md).

## Usage

``` r
validate_coded_output(x, schema, coerce = FALSE, max_bytes = 262144L)
```

## Arguments

- x:

  A JSON character string or a named R list.

- schema:

  An object of class `"llm_coding_schema"`.

- coerce:

  Logical; if `TRUE`, allows safe coercion of finite numeric strings
  (e.g. `"4.5"` to `4.5`) and boolean strings. Non-finite strings such
  as `"NaN"`, `"Inf"`, or `"-Infinity"` are always rejected. Default
  `FALSE` (strict).

- max_bytes:

  Maximum allowed byte length of a raw JSON response string. Default
  `262144L` (256 KiB).

## Value

A list with elements:

- `valid`: logical scalar indicating whether validation succeeded.

- `data`: named list of validated R values (or `NULL` if invalid).

- `errors`: character vector of validation failure messages.

## Examples

``` r
s <- coding_schema(score = schema_field("integer", min = 1, max = 5))
validate_coded_output('{"score": 4}', s)
#> $valid
#> [1] TRUE
#> 
#> $data
#> $data$score
#> [1] 4
#> 
#> 
#> $errors
#> character(0)
#> 
validate_coded_output('{"score": 9}', s)
#> $valid
#> [1] FALSE
#> 
#> $data
#> NULL
#> 
#> $errors
#> [1] "Field 'score' (9) exceeds max (5)."
#> 
```
