# Changelog

## llmmetric 0.3.0

### New Features & Workflow Hardening

- **Categorical & Multi-Class Prevalence PPI
  ([`ppi_proportion()`](https://marcusharrisphd.com/llmmetric/reference/ppi_proportion.md)):**
  Estimates population class prevalences with per-category optimal PPI++
  weights, full joint $`K \times K`$ covariance matrices, marginal or
  Bonferroni-adjusted simultaneous confidence intervals, and support for
  discrete labels, binary probabilities, or multi-class
  predicted-probability columns.
- **Human Inter-Rater Reliability Ceiling
  ([`human_irr()`](https://marcusharrisphd.com/llmmetric/reference/human_irr.md) +
  [`autoplot.llm_human_irr()`](https://marcusharrisphd.com/llmmetric/reference/autoplot.llm_human_irr.md)):**
  Computes human-vs-human reliability across two or more annotators in
  wide or long format—continuous $`\mathrm{ICC}(A,1)`$,
  $`\mathrm{ICC}(A,k)`$, $`\mathrm{ICC}(C,1)`$, $`\mathrm{ICC}(C,k)`$
  (with automatic REML handling for unbalanced ratings), pairwise
  $`r/\rho`$/RMSE, plus categorical/ordinal percent agreement, Cohen’s
  $`\kappa`$, Fleiss’s $`\kappa`$ (with analytic SE/CI), and multi-rater
  Krippendorff’s $`\alpha`$—and benchmarks LLM calibration directly
  against the human ceiling.
- **Resumable Checkpointed Batch Coding
  ([`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md)):**
  Added atomic `checkpoint_file` persistence (`checkpoint_every`) and
  `on_error = c("warn_na", "stop")` partial-failure recovery reporting
  `$failed_indices`, `$failed_units`, and `$resumed_count`, ensuring
  mid-batch API rate limits or timeouts never discard completed cells.
- **Publication-Ready APA Tables
  ([`format_apa_table()`](https://marcusharrisphd.com/llmmetric/reference/format_apa_table.md)):**
  Formats Markdown, tibble, and LaTeX `booktabs` APA 7th-edition
  comparison tables contrasting Naive LLM, Human-Only, and PPI / PPI++
  estimates, SEs, 95% CIs, APA-formatted p-values, and
  variance-reduction gains (plus support for
  [`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md),
  [`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md),
  and
  [`human_irr()`](https://marcusharrisphd.com/llmmetric/reference/human_irr.md)).
- **Complex Formula & Factor-Level Alignment:** Hardened
  `build_ppi_model_matrices()` for interaction formulas
  (`y ~ x1 * x2 + factor(group) + I(x1^2)`), automatic factor/character
  alignment across labeled and unlabeled splits, clear warnings when
  unlabeled data contain unseen factor levels, and actionable errors on
  single-level constant factor predictors.

### Documentation, Vignettes & CRAN Readiness

- Expanded to 4 executable vignettes: `getting-started`,
  `ppi-coverage-proof`, `dpf-and-fairness`, and
  `designing-validation-studies`.
- Added `inst/CITATION`, runnable
  `inst/examples/ema_psychometric_workflow.R`,
  `inst/guides/llmmetric-guide.Rmd`, `inst/WORDLIST`,
  `cran-comments.md`, `_pkgdown.yml`, and GitHub Actions workflows
  (`.github/workflows/R-CMD-check.yaml`,
  `.github/workflows/pkgdown.yaml`).
- Verified 0 misspellings via `spelling::spell_check_package()` and
  `0 errors | 0 warnings | 0 notes` under full `R CMD check` (including
  PDF manual) and `--as-cran`.

------------------------------------------------------------------------

## llmmetric 0.2.0

### Security Hardening

- Added centralized `R/security.R` and `R/validate.R` modules and
  documented full red-team audit findings in `SECURITY.md`.
- **URL & Header Hardening:**
  [`backend_openai()`](https://marcusharrisphd.com/llmmetric/reference/backend_openai.md)
  and
  [`backend_ollama()`](https://marcusharrisphd.com/llmmetric/reference/backend_ollama.md)
  strictly enforce `http://`/`https://` schemes, valid hosts/ports, zero
  control/whitespace characters, zero embedded URL credentials
  (`user:pass@`), and CRLF-safe HTTP headers.
- **Zero-Leak API Secret Vault:**
  [`backend_openai()`](https://marcusharrisphd.com/llmmetric/reference/backend_openai.md)
  stores API keys in a non-serialized session-scoped internal vault
  (`llm_redacted_secret`). Raw keys never appear in
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`str()`](https://rdrr.io/r/utils/str.html),
  [`dput()`](https://rdrr.io/r/base/dput.html),
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) files,
  [`cache_key()`](https://marcusharrisphd.com/llmmetric/reference/cache_key.md),
  or error messages (`redact_secrets()` scrubs keys and Bearer tokens).
- **Bounded Responses & Jittered Backoff:** Added configurable
  `max_response_bytes` enforcement (default 1 MiB) and bounded jittered
  exponential backoff (without mutating `.Random.seed`).
- **Hardened Untrusted JSON Extraction:**
  [`validate_coded_output()`](https://marcusharrisphd.com/llmmetric/reference/validate_coded_output.md)
  rejects duplicate JSON keys (key-smuggling defense), ambiguous
  multi-block/multi-object prompt-injected payloads, oversized
  responses, control bytes, non-finite numeric strings (`"NaN"`,
  `"Inf"`, `"-Infinity"`), and unsafe numeric magnitudes, while cleanly
  extracting single unambiguous fenced or prose-wrapped JSON objects.
- **Atomic & Integrity-Checked Disk Cache:** `coding_cache("disk")`
  strictly rejects path traversal keys, writes atomically via
  temp-file + [`file.rename()`](https://rdrr.io/r/base/files.html),
  wraps entries in a SHA-256 checksummed integrity envelope, and
  automatically evicts corrupted or tampered cache files cleanly
  (tracked via `cache_stats()$evictions`).

### New Features

- Added exported validation sample-size and power planners
  [`ppi_sample_size()`](https://marcusharrisphd.com/llmmetric/reference/ppi_sample_size.md)
  and
  [`ppi_power()`](https://marcusharrisphd.com/llmmetric/reference/ppi_power.md),
  plus
  [`autoplot.llm_ppi_power()`](https://marcusharrisphd.com/llmmetric/reference/autoplot.llm_ppi_power.md),
  enabling researchers to size human validation subsets for target
  standard errors or two-sided statistical power from either direct
  `(sd, r)` parameters or pilot `llm_ppi` / `llm_calibration` objects.

### Statistical & Numerical Accuracy Fixes

- **Collinearity / Rank Deficiency:**
  [`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md),
  [`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md),
  [`ppi_clustered()`](https://marcusharrisphd.com/llmmetric/reference/ppi_clustered.md),
  and
  [`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md)
  detect rank-deficient design matrices via QR pivoting, warn clearly,
  drop aliased columns during fitting, and return `NA` for aliased terms
  while preserving exact estimates and valid SEs for all identifiable
  parameters.
- **Binomial GLM Separation Protection:**
  [`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md)
  caps Newton steps, detects quasi-complete separation / Hessian
  collapse (`separation_detected = TRUE`), emits a clear diagnostic
  warning, and prevents artificial sandwich SE collapse.
- **Optimal Power-Tuning Robustness:**
  [`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md),
  [`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md),
  [`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md),
  and
  [`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md)
  compute optimal $`\hat{\lambda}^*`$ using the exact separate-sample
  denominator `V_tilde_L + (n/N) * V_tilde_U`, remaining finite and
  well-behaved under constant, zero-variance, heteroskedastic, or
  perfect LLM predictions.
- **Small Validation Sample Guard:** Added finite-sample `t` reference
  degrees of freedom and informative diagnostic warnings when labeled
  validation size `n` is small.
- **Constant/Zero-Variance Psychometric Guards:**
  [`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md),
  [`d_study()`](https://marcusharrisphd.com/llmmetric/reference/d_study.md),
  [`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md),
  [`check_drift()`](https://marcusharrisphd.com/llmmetric/reference/check_drift.md),
  and
  [`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md)
  now handle constant zero-variance ratings and single-class categorical
  ratings cleanly without fabricating numerical-dust reliability or
  emitting spurious `lm` warnings.

------------------------------------------------------------------------

## llmmetric 0.1.0

- Initial CRAN-grade release of `llmmetric`:
  - Multi-facet structured coding engine
    ([`coding_schema()`](https://marcusharrisphd.com/llmmetric/reference/coding_schema.md),
    [`coding_design()`](https://marcusharrisphd.com/llmmetric/reference/coding_design.md),
    [`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md),
    [`aggregate_scores()`](https://marcusharrisphd.com/llmmetric/reference/aggregate_scores.md),
    [`coding_cache()`](https://marcusharrisphd.com/llmmetric/reference/coding_cache.md),
    [`backend_mock()`](https://marcusharrisphd.com/llmmetric/reference/backend_mock.md),
    [`backend_openai()`](https://marcusharrisphd.com/llmmetric/reference/backend_openai.md),
    [`backend_ollama()`](https://marcusharrisphd.com/llmmetric/reference/backend_ollama.md),
    [`simulate_coding_study()`](https://marcusharrisphd.com/llmmetric/reference/simulate_coding_study.md)).
  - LLM Psychometrics & Generalizability Suite
    ([`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md),
    [`d_study()`](https://marcusharrisphd.com/llmmetric/reference/d_study.md),
    [`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md),
    [`check_drift()`](https://marcusharrisphd.com/llmmetric/reference/check_drift.md)).
  - Differential Prompt Functioning & Fairness Diagnostics
    ([`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md)).
  - Error-Corrected Downstream Inference
    ([`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md),
    [`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md),
    [`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md),
    [`ppi_clustered()`](https://marcusharrisphd.com/llmmetric/reference/ppi_clustered.md),
    [`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md)).
  - Publication-ready
    [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
    S3 methods, built-in `ema_coded` dataset, and Monte Carlo coverage
    proof vignette.
