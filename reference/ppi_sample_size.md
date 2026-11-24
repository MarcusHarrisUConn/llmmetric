# Plan Human Validation Sample Size for Prediction-Powered Inference (PPI++)

Computes the required number of human-coded validation annotations
`n_labeled` to achieve either a target standard error (`target_se`) or
target two-sided statistical power (`power` for a given `effect`) under
optimal power-tuned PPI++. Uses exact PPI++ asymptotic variance:
\$\$\mathrm{Var}(\hat{\theta}\_{\mathrm{PPI++}}) =
\frac{\sigma^2}{n}\left(1 - r^2 \frac{N}{n + N}\right),\$\$ where
\\\sigma\\ is the single-observation human-only standard error scaling
constant (\\\sigma = \mathrm{SE}\_{\mathrm{human}}\sqrt{n_0}\\), \\r \in
\[0, 1\]\\ is the correlation between human and LLM estimating scores,
and \\N\\ is the unlabeled LLM-coded sample size.

## Usage

``` r
ppi_sample_size(
  target_se = NULL,
  effect = NULL,
  power = 0.8,
  sd = NULL,
  r = NULL,
  n_total = NULL,
  n_unlabeled = Inf,
  pilot = NULL,
  term = NULL,
  alpha = 0.05,
  min_n = 20L
)
```

## Arguments

- target_se:

  Optional positive target standard error for the focal parameter.
  Specify either `target_se` or `effect`.

- effect:

  Optional non-zero target effect size for a two-sided Wald test at
  significance level `alpha` and target `power`. Ignored if `target_se`
  is supplied.

- power:

  Target statistical power in `(alpha, 1)` when sizing by `effect`.
  Default `0.80`.

- sd:

  Positive single-observation SD scaling constant
  (\\\mathrm{SE}\_{\mathrm{human}}\sqrt{n}\\). Inferred from `pilot` if
  `NULL`.

- r:

  Correlation magnitude in `[0, 1]` between human and LLM scores (or
  estimating equations). Inferred from `pilot` if `NULL`.

- n_total:

  Optional finite total corpus size (`n_labeled + n_unlabeled`). When
  supplied, solves `n_labeled` accounting for the irreducible corpus
  floor \\\sigma / \sqrt{n\_{\mathrm{total}}}\\. Mutually exclusive with
  a finite `n_unlabeled`.

- n_unlabeled:

  Unlabeled LLM-coded pool size `N` (default `Inf`, representing a large
  unlabeled corpus). Ignored if `n_total` is supplied.

- pilot:

  Optional pilot `"llm_ppi"` or continuous `"llm_calibration"` object
  from which `sd` and `r` are estimated automatically.

- term:

  Optional parameter term name to extract from a multi-term `"llm_ppi"`
  pilot object. Defaults to the first non-intercept term.

- alpha:

  Two-sided Type-I error rate in `(0, 1)`. Default `0.05`.

- min_n:

  Minimum practical validation sample size floor. Default `20L`.

## Value

An S3 object of class `"llm_ppi_sample_size"` containing:

- `feasible`: logical indicating whether the target is attainable given
  `n_total`.

- `n_labeled`: required human validation sample size (integer, or `NA`
  if infeasible).

- `n_unlabeled`: resulting unlabeled sample size.

- `n_human_only`: human annotations required without PPI++.

- `annotations_saved`, `savings_pct`: human annotation savings from
  PPI++.

- `target_se`, `achieved_se`, `achieved_power`, `effective_n`,
  `min_n_total_needed`, `params`.

## Details

Parameters `(sd, r)` may be supplied directly or automatically extracted
from a pilot
[`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md),
[`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md),
[`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md),
[`ppi_clustered()`](https://marcusharrisphd.com/llmmetric/reference/ppi_clustered.md),
[`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md),
or continuous
[`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md)
object via `pilot`.

## Examples

``` r
# Size a validation subset out of a 2,000-text corpus for 80% power on beta = 0.25
plan <- ppi_sample_size(effect = 0.25, power = 0.80, sd = 1.2, r = 0.82, n_total = 2000)
plan
#> 
#> ── PPI++ Human Validation Sample-Size Plan 
#> Inputs (supplied): sd = 1.200 | r = 0.820 | Target SE = 0.0892
#> Required human validation n = 64 (vs. 181 human-only -> saves 117 annotations,
#> 64.6%)
#> Projected PPI++ SE = 0.0886 | Effective n = 183.3 | Power = 0.805

# Or size directly from a pilot PPI fit on ema_coded
data(ema_coded)
agg <- aggregate_scores(ema_coded, score_col = "llm_score")
pilot_fit <- ppi_lm(human_score ~ x + group, data = agg, pred_col = "llm_score")
ppi_sample_size(target_se = 0.06, n_total = 1500, pilot = pilot_fit, term = "x")
#> 
#> ── PPI++ Human Validation Sample-Size Plan 
#> Inputs (pilot_ppi++): sd = 1.163 | r = 1.000 | Target SE = 0.0600
#> Required human validation n = 20 (vs. 376 human-only -> saves 356 annotations,
#> 94.7%)
#> Projected PPI++ SE = 0.0301 | Effective n = 1489.0
```
