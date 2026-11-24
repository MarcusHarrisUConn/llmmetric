# Test for Temporal or Model-Version Scoring Drift

Evaluates whether LLM scores on a shared anchor set of texts drift
across timepoints, waves, or model versions. Performs an overall
within-unit drift test across versions plus pairwise comparisons against
a baseline version (paired mean shift, paired Cohen's \\d_z\\, variance
ratio, rank/agreement stability, and Kolmogorov-Smirnov distributional
shift).

## Usage

``` r
check_drift(
  data,
  score_col = "score",
  version_col = "version",
  id_col = "unit_id",
  baseline = NULL,
  truth_col = NULL,
  alpha = 0.05,
  min_effect = 0.2,
  na.rm = TRUE
)
```

## Arguments

- data:

  An `llm_coded_data` object or a `data.frame`.

- score_col:

  Name of the numeric LLM score column.

- version_col:

  Name of the column identifying timepoint, wave, or model version (must
  have at least 2 distinct values).

- id_col:

  Name of the anchor unit identifier column. Default `"unit_id"`.

- baseline:

  Optional value in `version_col` serving as the reference version.
  Defaults to the first factor level (or first sorted unique value).

- truth_col:

  Optional name of a human gold-standard score column to track
  calibration bias and RMSE drift across versions.

- alpha:

  Significance level for drift flagging. Default `0.05`.

- min_effect:

  Minimum absolute paired Cohen's \\d_z\\ required alongside statistical
  significance to trigger `drift_flag`. Default `0.20`.

- na.rm:

  Logical; drop missing rows. Default `TRUE`.

## Value

An S3 object of class `"llm_drift"` containing:

- `summary`: tibble of per-version statistics and pairwise drift tests
  vs `baseline`.

- `overall_test`: tibble summarizing the omnibus within-unit test across
  all versions.

- `baseline`: reference version label.

- `drift_detected`: logical scalar (`TRUE` if any version triggers
  drift).

- `alpha`, `min_effect`, `version_col`, `score_col`, `data`.

## Examples

``` r
set.seed(1)
anchor <- tibble::tibble(
  unit_id = rep(paste0("u", 1:40), 3),
  version = rep(c("v1", "v2", "v3"), each = 40),
  base = rep(rnorm(40), 3),
  score = base + rep(c(0, 0.05, 0.55), each = 40) + rnorm(120, 0, 0.25)
)
check_drift(anchor, score_col = "score", version_col = "version")
#> 
#> ── LLM Scoring Drift Diagnostics 
#> Baseline: "v1" | Overall ANOVA F(2, 78) = 43.315, p = <0.0001
#> Drift detected (alpha=0.05, |d_z|>=0.2): TRUE
#> # A tibble: 3 × 7
#>   version n_shared mean_diff cohens_dz   icc p_adj drift_flag
#>   <chr>      <int>     <dbl>     <dbl> <dbl> <dbl> <lgl>     
#> 1 v1            40     0         0     1     1     FALSE     
#> 2 v2            40     0.049     0.174 0.952 0.277 FALSE     
#> 3 v3            40     0.442     1.15  0.832 0     TRUE      
```
