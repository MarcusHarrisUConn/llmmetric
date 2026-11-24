# Deterministic Synthetic LLM Rater Backend

Creates an offline, deterministic mock LLM backend parameterized by an
explicit psychometric measurement model. Supports configurable
true-score slope/calibration, additive and interaction variance
components (prompt, model, unit-by-prompt, unit-by-model,
prompt-by-model), temperature noise, uniform and non-uniform
Differential Prompt Functioning (DPF), prompt-by-group DPF interactions,
temporal/version drift, and optional malformed-JSON injection for
testing retry pipelines.

## Usage

``` r
backend_mock(
  seed = 1234L,
  intercept = 0,
  true_score_slope = 1,
  true_score_col = "true_score",
  error_sd = 0.5,
  temp_noise_sd = 0.5,
  prompt_sd = 0.25,
  model_sd = 0.2,
  unit_prompt_sd = 0.25,
  unit_model_sd = 0.2,
  prompt_model_sd = 0.15,
  prompt_bias = NULL,
  model_bias = NULL,
  dpf_uniform = NULL,
  dpf_nonuniform = NULL,
  dpf_prompt_group = NULL,
  group_col = "group",
  drift_per_wave = 0,
  version_shift = NULL,
  wave_col = "wave",
  malformed_rate = 0,
  score_field = NULL
)
```

## Arguments

- seed:

  Integer random seed for deterministic synthetic generation.

- intercept:

  Overall additive bias (calibration intercept shift).

- true_score_slope:

  Calibration slope multiplying the unit true score.

- true_score_col:

  Name of the numeric true-score column in the input data frame (if
  present in `request$meta`). If absent, a deterministic latent true
  score \\T_i \sim \mathcal{N}(0, 1)\\ is generated per `unit_id`.

- error_sd:

  Standard deviation of independent measurement error.

- temp_noise_sd:

  Additional error standard deviation multiplied by `temperature`.

- prompt_sd:

  SD of random prompt main effects (when `prompt_bias` does not specify
  a prompt).

- model_sd:

  SD of random model main effects (when `model_bias` does not specify a
  model).

- unit_prompt_sd:

  SD of `unit x prompt` interaction variance component.

- unit_model_sd:

  SD of `unit x model` interaction variance component.

- prompt_model_sd:

  SD of `prompt x model` interaction variance component.

- prompt_bias:

  Optional named numeric vector of fixed prompt main effects.

- model_bias:

  Optional named numeric vector of fixed model main effects.

- dpf_uniform:

  Optional named numeric vector of uniform subgroup shifts keyed by
  subgroup level (e.g. `c(group_b = 0.75)`).

- dpf_nonuniform:

  Optional named numeric vector of subgroup slope shifts multiplying the
  true score (e.g. `c(group_b = -0.35)`).

- dpf_prompt_group:

  Optional numeric matrix (rows = prompt IDs, cols = subgroup levels)
  specifying prompt-by-group DPF interactions.

- group_col:

  Column name in input data identifying subgroup membership.

- drift_per_wave:

  Numeric mean drift per unit increase in `wave_col`.

- version_shift:

  Optional named numeric vector of fixed shifts keyed by `wave_col` (or
  version label).

- wave_col:

  Column name in input data identifying temporal wave or model version.

- malformed_rate:

  Probability in `[0, 1]` of returning malformed JSON on attempt 1
  (useful for testing schema validation and retry behavior).

- score_field:

  Name of the primary numeric/factor field in the schema. Defaults to
  the first field in the schema.

## Value

An object of class `c("llm_backend_mock", "llm_backend")`.

## Details

Calls are hashed deterministically from
`(seed, unit_id, prompt_id, model, temperature, replicate)` without
mutating R's global `.Random.seed`.

## Examples

``` r
b <- backend_mock(seed = 42, error_sd = 0.4, prompt_sd = 0.3, model_sd = 0.2)
b
#> 
#> ── LLM Backend (mock) 
#> ID: mock_ada1a0b8a39e
#> Seed: 42 | Slope: 1 | Intercept: 0 | Error SD: 0.4
#> Facet SDs: prompt=0.3, model=0.2, u:p=0.25, u:m=0.2, p:m=0.15
```
