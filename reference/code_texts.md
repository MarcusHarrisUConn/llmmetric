# Execute a Multi-Facet Structured LLM Coding Design

Orchestrates crossed LLM scoring across
`units x prompts x models x temperatures x replicates` with
content-addressed caching, strict JSON schema validation, retry handling
on malformed/invalid outputs, resumable atomic checkpointing
(`checkpoint_file`), partial-failure recovery (`on_error`), and
pluggable backends.

## Usage

``` r
code_texts(
  data,
  text_col,
  id_col = NULL,
  design,
  backend,
  cache = coding_cache("memory"),
  max_validation_retries = 2L,
  coerce = FALSE,
  keep_cols = TRUE,
  keep_raw = FALSE,
  checkpoint_file = NULL,
  checkpoint_every = 25L,
  on_error = c("warn_na", "stop")
)
```

## Arguments

- data:

  A `data.frame` containing the texts to score and any optional
  unit-level metadata (e.g., `human_score`, `group`, `wave`, `cluster`,
  or downstream covariates).

- text_col:

  Name of the character column containing input texts.

- id_col:

  Optional name of the unique unit identifier column. If `NULL`,
  sequential IDs `"unit_1"`, `"unit_2"`, ... are created.

- design:

  An
  [`coding_design()`](https://marcusharrisphd.com/llmmetric/reference/coding_design.md)
  object.

- backend:

  Either a single `llm_backend` object (e.g.,
  [`backend_mock()`](https://marcusharrisphd.com/llmmetric/reference/backend_mock.md),
  [`backend_openai()`](https://marcusharrisphd.com/llmmetric/reference/backend_openai.md),
  [`backend_ollama()`](https://marcusharrisphd.com/llmmetric/reference/backend_ollama.md))
  or a named list of `llm_backend` objects keyed by model name (with
  optional `".default"` fallback).

- cache:

  An
  [`coding_cache()`](https://marcusharrisphd.com/llmmetric/reference/coding_cache.md)
  object. Defaults to an in-memory cache.

- max_validation_retries:

  Maximum retries when backend output fails JSON parsing or schema
  validation. Default `2L`.

- coerce:

  Logical passed to
  [`validate_coded_output()`](https://marcusharrisphd.com/llmmetric/reference/validate_coded_output.md).
  Default `FALSE`.

- keep_cols:

  Logical or character vector specifying which additional columns from
  `data` to attach to each coded row. `TRUE` (default) preserves all
  non-text columns that do not collide with facet or schema names;
  `FALSE` keeps none; a character vector preserves only those columns.

- keep_raw:

  Logical; if `TRUE`, retains raw JSON text in a `raw_output` column.
  Default `FALSE`.

- checkpoint_file:

  Optional path to an `.rds` checkpoint file for resumable batch coding.
  Completed valid cells are persisted atomically every
  `checkpoint_every` new cells and immediately upon failure or
  completion; re-running with the same `checkpoint_file` automatically
  resumes without re-scoring completed cells.

- checkpoint_every:

  Positive integer specifying how often (in newly completed cells) to
  flush `checkpoint_file`. Default `25L`.

- on_error:

  Failure handling policy after all retries for a cell are exhausted:
  `"warn_na"` (default: records `NA` for failed cells, populates
  `$failures` and `$failed_indices`, and emits a concise summary
  warning) or `"stop"` (flushes `checkpoint_file` first so completed
  cells are never lost, then aborts with an actionable error).

## Value

An S3 object of class `"llm_coded_data"` with components:

- `scores`: a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  with one row per `(unit, prompt, model, temperature, replicate)` cell,
  validated schema columns, `cache_hit`, `attempts`, `valid`, and
  preserved unit metadata.

- `design`: the input
  [`coding_design()`](https://marcusharrisphd.com/llmmetric/reference/coding_design.md).

- `schema`: the active
  [`coding_schema()`](https://marcusharrisphd.com/llmmetric/reference/coding_schema.md).

- `cache_stats`: snapshot of
  [`cache_stats()`](https://marcusharrisphd.com/llmmetric/reference/cache_operations.md)
  after execution.

- `failures`: tibble of cells that failed validation after all retries.

- `failed_indices`: integer vector of input row indices with at least
  one failed cell.

- `failed_units`: character vector of unit IDs with at least one failed
  cell.

- `resumed_count`: integer count of cells restored from
  `checkpoint_file`.

- `checkpoint_file`: checkpoint path (or `NULL`).

## Examples

``` r
texts <- tibble::tibble(
  id = paste0("u", 1:8),
  text = paste("Sample diary entry", 1:8),
  true_score = seq(-1.5, 1.5, length.out = 8)
)
des <- coding_design(
  prompts = c(p1 = "Rate: {text}", p2 = "Score: {text}"),
  models = c("m1", "m2"),
  schema = coding_schema(score = schema_field("numeric"))
)
coded <- code_texts(texts, text_col = "text", id_col = "id", design = des, backend = backend_mock())
coded
#> 
#> ── LLM Coded Data (default_design) 
#> Units: 8 | Rows: 32 | Valid: 32/32 | Failures: 0
#> Cache (memory): 0 hits, 32 misses
#> # A tibble: 32 × 10
#>   unit_id prompt model temperature replicate  score cache_hit attempts valid
#>   <chr>   <chr>  <chr>       <dbl>     <int>  <dbl> <lgl>        <int> <lgl>
#> 1 u1      p1     m1              0         1 -0.604 FALSE            1 TRUE 
#> 2 u2      p1     m1              0         1  0.325 FALSE            1 TRUE 
#> 3 u3      p1     m1              0         1 -0.611 FALSE            1 TRUE 
#> 4 u4      p1     m1              0         1 -0.670 FALSE            1 TRUE 
#> 5 u5      p1     m1              0         1  0.497 FALSE            1 TRUE 
#> 6 u6      p1     m1              0         1  1.14  FALSE            1 TRUE 
#> # ℹ 26 more rows
#> # ℹ 1 more variable: true_score <dbl>
```
