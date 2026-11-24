# Define a Field for an LLM Coding Schema

Specifies the expected type, bounds, allowed levels, nullability, and
requirement status for a single field in structured JSON LLM output.

## Usage

``` r
schema_field(
  type = c("numeric", "integer", "factor", "enum", "logical", "character"),
  min = NULL,
  max = NULL,
  levels = NULL,
  required = TRUE,
  nullable = FALSE,
  description = NULL
)
```

## Arguments

- type:

  Field type: one of `"numeric"`, `"integer"`, `"factor"`, `"enum"`,
  `"logical"`, or `"character"`. (`"enum"` is an alias for `"factor"`.)

- min:

  Optional numeric lower bound (for `"numeric"` or `"integer"`).

- max:

  Optional numeric upper bound (for `"numeric"` or `"integer"`).

- levels:

  Character vector of allowed category labels (required when `type` is
  `"factor"` or `"enum"`).

- required:

  Logical; whether the field must be present in every parsed JSON
  payload. Default `TRUE`.

- nullable:

  Logical; whether JSON `null` / `NA` is allowed. Default `FALSE`.

- description:

  Optional character description embedded in JSON Schema prompts and
  structured-output specifications.

## Value

An object of class `"llm_schema_field"`.

## Examples

``` r
schema_field("numeric", min = 1, max = 7, description = "Distress rating")
#> $type
#> [1] "numeric"
#> 
#> $min
#> [1] 1
#> 
#> $max
#> [1] 7
#> 
#> $levels
#> NULL
#> 
#> $required
#> [1] TRUE
#> 
#> $nullable
#> [1] FALSE
#> 
#> $description
#> [1] "Distress rating"
#> 
#> attr(,"class")
#> [1] "llm_schema_field"
schema_field("factor", levels = c("low", "medium", "high"))
#> $type
#> [1] "factor"
#> 
#> $min
#> NULL
#> 
#> $max
#> NULL
#> 
#> $levels
#> [1] "low"    "medium" "high"  
#> 
#> $required
#> [1] TRUE
#> 
#> $nullable
#> [1] FALSE
#> 
#> $description
#> NULL
#> 
#> attr(,"class")
#> [1] "llm_schema_field"
```
