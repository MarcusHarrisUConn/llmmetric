# Local Ollama HTTP Coding Backend

Creates a hardened backend targeting a local or remote Ollama server
(`/api/chat`) via `httr2` with strict URL validation, bounded response
sizes, structured JSON/schema constraints, and jittered backoff retries.

## Usage

``` r
backend_ollama(
  model = "llama3.2",
  base_url = "http://localhost:11434",
  system_prompt = "Return only valid JSON matching the coding schema.",
  format = c("schema", "json", "none"),
  timeout = 120,
  max_retries = 2L,
  backoff_base = 1,
  max_response_bytes = 1048576L,
  options = list(),
  transport = NULL
)
```

## Arguments

- model:

  Default Ollama model tag (e.g., `"llama3.2"`, `"qwen2.5:7b"`).

- base_url:

  Ollama HTTP/HTTPS base URL without trailing slash.

- system_prompt:

  Optional system instruction prepended to messages.

- format:

  Structured output constraint: `"schema"` (sends full JSON Schema),
  `"json"` (sends `"json"`), or `"none"`.

- timeout:

  Request timeout in seconds. Default `120`.

- max_retries:

  Maximum HTTP retry attempts on transient errors.

- backoff_base:

  Base backoff seconds for jittered exponential retry.

- max_response_bytes:

  Maximum allowed response payload size in bytes. Default `1048576L` (1
  MiB).

- options:

  Named list of additional Ollama model options (e.g., `seed`,
  `num_ctx`).

- transport:

  Optional custom transport function `function(req_spec)` for offline
  testing.

## Value

An object of class `c("llm_backend_ollama", "llm_backend")`.

## Examples

``` r
b <- backend_ollama(model = "llama3.2")
b
#> 
#> ── LLM Backend (ollama) 
#> ID: ollama_a8a525ed863e
#> Model: llama3.2 | Endpoint: http://localhost:11434 | Timeout: 120s
```
