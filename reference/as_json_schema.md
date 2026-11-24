# Convert a Coding Schema to a JSON Schema List

Generates a draft-07 JSON Schema representation suitable for prompt
instructions, OpenAI
`response_format = list(type = "json_schema", ...)`, or Ollama `format`
parameters.

## Usage

``` r
as_json_schema(schema)
```

## Arguments

- schema:

  An object of class `"llm_coding_schema"`.

## Value

A named list representing a JSON Schema object.

## Examples

``` r
s <- coding_schema(score = schema_field("numeric", min = 0, max = 10))
as_json_schema(s)
#> $type
#> [1] "object"
#> 
#> $properties
#> $properties$score
#> $properties$score$type
#> [1] "number"
#> 
#> $properties$score$minimum
#> [1] 0
#> 
#> $properties$score$maximum
#> [1] 10
#> 
#> 
#> 
#> $required
#> [1] "score"
#> 
#> $additionalProperties
#> [1] FALSE
#> 
```
