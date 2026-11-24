# Compute Projected Precision and Statistical Power for PPI++

Evaluates projected PPI++ standard error, effective sample size,
variance reduction factor, and (when `effect` is supplied) two-sided
statistical power across grids of human validation sizes `n_labeled`
and/or LLM-human correlations `r`, directly benchmarked against
human-only estimation.

## Usage

``` r
ppi_power(
  n_labeled,
  n_unlabeled = NULL,
  n_total = NULL,
  effect = NULL,
  sd = NULL,
  r = NULL,
  pilot = NULL,
  term = NULL,
  alpha = 0.05
)
```

## Arguments

- n_labeled:

  Positive integer vector of candidate human validation sample sizes.

- n_unlabeled:

  Optional non-negative numeric vector (or `Inf`) of unlabeled LLM
  sample sizes. Ignored if `n_total` is supplied. Defaults to `Inf` when
  `n_total` is `NULL`.

- n_total:

  Optional finite total corpus size (`n_labeled + n_unlabeled`).

- effect:

  Optional non-zero target effect size for power computation.

- sd:

  Positive single-observation SD scaling constant. Inferred from `pilot`
  if `NULL`.

- r:

  Numeric vector of LLM-human score correlations in `[0, 1]`. Inferred
  from `pilot` if `NULL`.

- pilot:

  Optional pilot `"llm_ppi"` or continuous `"llm_calibration"` object
  from which `sd` and `r` are estimated automatically.

- term:

  Optional parameter term name for multi-term `"llm_ppi"` pilots.

- alpha:

  Two-sided significance level in `(0, 1)`. Default `0.05`.

## Value

An S3 object of class `"llm_ppi_power"` containing `$grid` tibble,
`$params`, `$effect`, `$alpha`, and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
support.

## Examples

``` r
pw <- ppi_power(
  n_labeled = seq(40, 240, by = 40),
  n_total = 1500,
  effect = 0.25,
  sd = 1.2,
  r = c(0.6, 0.8, 0.9)
)
pw
#> 
#> ── PPI++ Precision & Power Projections 
#> Source: supplied | sd = 1.200 | effect = 0.250 | alpha = 0.05
#> # A tibble: 18 × 10
#>    n_labeled n_unlabeled n_total     r se_human se_ppi var_reduction effective_n
#>        <int>       <int>   <int> <dbl>    <dbl>  <dbl>         <dbl>       <dbl>
#>  1        40        1460    1500   0.6   0.190  0.153           1.54        61.6
#>  2        80        1420    1500   0.6   0.134  0.109           1.52       121. 
#>  3       120        1380    1500   0.6   0.110  0.0896          1.50       179. 
#>  4       160        1340    1500   0.6   0.0949 0.0781          1.47       236. 
#>  5       200        1300    1500   0.6   0.0849 0.0704          1.45       291. 
#>  6       240        1260    1500   0.6   0.0775 0.0647          1.43       344. 
#>  7        40        1460    1500   0.8   0.190  0.117           2.65       106. 
#>  8        80        1420    1500   0.8   0.134  0.0842          2.54       203. 
#>  9       120        1380    1500   0.8   0.110  0.0702          2.43       292. 
#> 10       160        1340    1500   0.8   0.0949 0.0621          2.33       374. 
#> 11       200        1300    1500   0.8   0.0849 0.0566          2.25       449. 
#> 12       240        1260    1500   0.8   0.0775 0.0527          2.16       519. 
#> # ℹ 6 more rows
#> # ℹ 2 more variables: power_human <dbl>, power_ppi <dbl>
```
