# OpenAI-Compatible HTTP Coding Backend

Creates a hardened backend targeting any OpenAI-compatible
`/chat/completions` API (OpenAI, Azure OpenAI proxies, vLLM, OpenRouter,
Together, LiteLLM, or local servers) using `httr2` with strict
URL/header validation, sealed API-key redaction, bounded response sizes,
and jittered exponential backoff retries.

## Usage

``` r
backend_openai(
  model = "gpt-4o-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  base_url = "https://api.openai.com/v1",
  system_prompt = "Return only valid JSON matching the coding schema.",
  response_format = c("json_schema", "json_object", "none"),
  timeout = 60,
  max_retries = 3L,
  backoff_base = 1,
  max_response_bytes = 1048576L,
  extra_headers = list(),
  extra_body = list(),
  transport = NULL
)
```

## Arguments

- model:

  Default model identifier (overridden by design model column when
  [`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md)
  runs multi-model designs).

- api_key:

  API key string (wrapped in a non-printable redacted container; never
  echoed in [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), cache keys, or
  errors). Defaults to `Sys.getenv("OPENAI_API_KEY")`.

- base_url:

  Base HTTP/HTTPS API URL without trailing slash.

- system_prompt:

  Optional system instruction prepended to every request.

- response_format:

  Structured output mode: `"json_schema"` (default), `"json_object"`, or
  `"none"`.

- timeout:

  Request timeout in seconds. Default `60`.

- max_retries:

  Maximum HTTP retry attempts on transient errors.

- backoff_base:

  Base backoff seconds for jittered exponential retry.

- max_response_bytes:

  Maximum allowed response payload size in bytes. Default `1048576L` (1
  MiB).

- extra_headers:

  Optional named list of additional HTTP headers (checked against
  CRLF/control-character injection).

- extra_body:

  Optional named list merged into the JSON request body.

- transport:

  Optional custom transport function `function(req_spec)` returning a
  parsed list response (intended for offline testing/mocking).

## Value

An object of class `c("llm_backend_openai", "llm_backend")`.

## Examples

``` r
b <- backend_openai(model = "gpt-4o-mini", api_key = "demo-key")
b
#> 
#> ── LLM Backend (openai) 
#> ID: openai_d68c21a472ee
#> Model: gpt-4o-mini | Endpoint: https://api.openai.com/v1 | API key: <redacted>
#> | Timeout: 60s
```
