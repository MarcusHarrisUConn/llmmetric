# Custom R-Function LLM Coding Backend Adapter

Wraps any R callable `fn(request)` — such as an offline `airgapllm`
pipeline, local Python/C++ model wrapper, or custom heuristic scorer —
into an `llm_backend` compatible with
[`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md),
preserving full JSON schema validation, retries, content-addressed
caching, resumable checkpointing, and downstream G-study/DPF/PPI
analysis with zero extra package dependencies.

## Usage

``` r
backend_custom(fn, name = "custom", version = "1.0", metadata = list())
```

## Arguments

- fn:

  A function accepting a single `request` list (containing `unit_id`,
  `text`, `prompt_id`, `prompt_text`, `model`, `temperature`,
  `replicate`, `schema`, `attempt`, `previous_errors`, and `meta`). `fn`
  may return:

  - a JSON character string,

  - a named R list matching the fields of `request$schema`,

  - a 1-row `data.frame` matching `request$schema`, or

  - a full response list containing `content` (plus optional `model` and
    `raw_response`).

- name:

  Descriptive identifier for the custom backend (included in
  content-addressed cache keys). Default `"custom"`.

- version:

  Version string for cache invalidation when `fn` changes. Default
  `"1.0"`.

- metadata:

  Optional named list of extra cache-key metadata (e.g., model weights
  hash or runtime parameters).

## Value

An object of class `c("llm_backend_custom", "llm_backend")`.

## Examples

``` r
my_scorer <- backend_custom(
  fn = function(req) list(score = nchar(req$text) / 10),
  name = "length_scorer",
  version = "1.0"
)
texts <- tibble::tibble(
  id = c("u1", "u2"),
  text = c("Short note", "A slightly longer diary entry")
)
des <- coding_design(prompts = c(p1 = "{text}"), models = "local")
code_texts(texts, "text", id_col = "id", design = des, backend = my_scorer)
#> 
#> ── LLM Coded Data (default_design) 
#> Units: 2 | Rows: 2 | Valid: 2/2 | Failures: 0
#> Cache (memory): 0 hits, 2 misses
#> # A tibble: 2 × 9
#>   unit_id prompt model temperature replicate score cache_hit attempts valid
#>   <chr>   <chr>  <chr>       <dbl>     <int> <dbl> <lgl>        <int> <lgl>
#> 1 u1      p1     local           0         1   1   FALSE            1 TRUE 
#> 2 u2      p1     local           0         1   2.9 FALSE            1 TRUE 
```
