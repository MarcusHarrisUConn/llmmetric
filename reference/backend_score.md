# Score a Single Coding Request via an LLM Backend

S3 generic invoked by
[`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md)
to obtain a raw JSON string or structured response from an `llm_backend`
object.

## Usage

``` r
backend_score(backend, request, ...)
```

## Arguments

- backend:

  An object inheriting from `"llm_backend"`.

- request:

  A named list containing `unit_id`, `text`, `prompt_id`, `prompt_text`,
  `model`, `temperature`, `replicate`, `schema`, `attempt`,
  `previous_errors`, and `meta`.

- ...:

  Additional backend-specific arguments.

## Value

A list with at least `content` (JSON character string or named list) and
optional metadata (`model`, `latency_ms`, `raw_response`).

## Examples

``` r
b <- backend_mock(seed = 1)
s <- coding_schema(score = schema_field("numeric"))
req <- list(
  unit_id = "u1", text = "Example text", prompt_id = "p1",
  prompt_text = "Rate: Example text", model = "m1",
  temperature = 0, replicate = 1L, schema = s, attempt = 1L,
  meta = list(true_score = 2.0)
)
backend_score(b, req)$content
#> [1] "{\"score\":1.6541}"
```
