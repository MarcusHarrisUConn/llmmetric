# Specify a Multi-Facet LLM Coding Design

Defines a crossed experimental coding design
(`prompts x models x temperatures x replicates`) paired with a strict
[`coding_schema()`](https://marcusharrisphd.com/llmmetric/reference/coding_schema.md)
for reproducible LLM measurement studies.

## Usage

``` r
coding_design(
  prompts,
  models,
  temperatures = 0,
  replicates = 1L,
  schema = NULL,
  name = "default_design"
)
```

## Arguments

- prompts:

  Named (or unnamed) character vector of prompt templates. Templates may
  reference `{text}` and any additional column names in the input data
  frame passed to
  [`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md).

- models:

  Character vector of model identifiers to cross with prompts.

- temperatures:

  Numeric vector of sampling temperatures (`>= 0`). Default `0`.

- replicates:

  Positive integer number of stochastic replicates per
  `(unit, prompt, model, temperature)` cell. Default `1L`.

- schema:

  An
  [`coding_schema()`](https://marcusharrisphd.com/llmmetric/reference/coding_schema.md)
  object defining expected JSON fields. Defaults to a single numeric
  `score` field.

- name:

  Optional descriptive name for the coding design.

## Value

An object of class `"llm_coding_design"`.

## Examples

``` r
des <- coding_design(
  prompts = c(
    direct = "Rate emotional distress (1-7): {text}",
    rubric = "Using the clinical rubric, rate distress (1-7): {text}"
  ),
  models = c("model_a", "model_b"),
  temperatures = c(0, 0.5),
  schema = coding_schema(score = schema_field("numeric", min = 1, max = 7))
)
des
#> 
#> ── LLM Coding Design: default_design 
#> Prompts: 2 | Models: 2 | Temperatures: 2 | Replicates: 1
#> Cells per unit: 8
#> Models: "model_a" and "model_b"
#> Prompt IDs: "direct" and "rubric"
```
