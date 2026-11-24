# Define a Structured Output Schema for LLM Coding

Creates a strict schema describing the JSON object returned by an LLM
rater. Schemas drive JSON Schema generation (for OpenAI structured
outputs or Ollama JSON schema constraints) as well as strict response
validation and retry logic in
[`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md).

## Usage

``` r
coding_schema(..., .fields = NULL, name = "llm_coding_output", strict = TRUE)
```

## Arguments

- ...:

  Named field specifications created with
  [`schema_field()`](https://marcusharrisphd.com/llmmetric/reference/schema_field.md)
  or shorthand type strings (`"numeric"`, `"integer"`, `"logical"`,
  `"character"`).

- .fields:

  Optional named list of field specifications (merged with `...`).

- name:

  Character name for the schema (used by JSON Schema APIs).

- strict:

  Logical; if `TRUE` (default), unexpected extra keys in the parsed JSON
  payload trigger validation failure.

## Value

An object of class `"llm_coding_schema"`.

## Examples

``` r
s <- coding_schema(
  score = schema_field("numeric", min = 1, max = 7),
  label = schema_field("factor", levels = c("negative", "neutral", "positive")),
  rationale = schema_field("character", required = FALSE)
)
s
#> 
#> ── LLM Coding Schema: llm_coding_output 
#> Strict: TRUE | Fields: 3 | Hash: 8434e3e3984f
#> • score: "numeric" [1, 7]
#> • label: "factor" {negative, neutral, positive}
#> • rationale: "character" optional
```
