# llmmetric 0.2.0

## Security Hardening
- Added centralized `R/security.R` and `R/validate.R` modules and documented full red-team audit findings in `SECURITY.md`.
- **URL & Header Hardening:** `backend_openai()` and `backend_ollama()` strictly enforce `http://`/`https://` schemes, valid hosts/ports, zero control/whitespace characters, zero embedded URL credentials (`user:pass@`), and CRLF-safe HTTP headers.
- **Zero-Leak API Secret Vault:** `backend_openai()` stores API keys in a non-serialized session-scoped internal vault (`llm_redacted_secret`). Raw keys never appear in `print()`, `summary()`, `str()`, `dput()`, `saveRDS()` files, `cache_key()`, or error messages (`redact_secrets()` scrubs keys and Bearer tokens).
- **Bounded Responses & Jittered Backoff:** Added configurable `max_response_bytes` enforcement (default 1 MiB) and bounded jittered exponential backoff (without mutating `.Random.seed`).
- **Hardened Untrusted JSON Extraction:** `validate_coded_output()` rejects duplicate JSON keys (key-smuggling defense), ambiguous multi-block/multi-object prompt-injected payloads, oversized responses, control bytes, non-finite numeric strings (`"NaN"`, `"Inf"`, `"-Infinity"`), and unsafe numeric magnitudes, while cleanly extracting single unambiguous fenced or prose-wrapped JSON objects.
- **Atomic & Integrity-Checked Disk Cache:** `coding_cache("disk")` strictly rejects path traversal keys, writes atomically via temp-file + `file.rename()`, wraps entries in a SHA-256 checksummed integrity envelope, and automatically evicts corrupted or tampered cache files cleanly (tracked via `cache_stats()$evictions`).

## New Features
- Added exported validation sample-size and power planners `ppi_sample_size()` and `ppi_power()`, plus `autoplot.llm_ppi_power()`, enabling researchers to size human validation subsets for target standard errors or two-sided statistical power from either direct `(sd, r)` parameters or pilot `llm_ppi` / `llm_calibration` objects.

## Statistical & Numerical Accuracy Fixes
- **Collinearity / Rank Deficiency:** `ppi_lm()`, `ppi_glm()`, `ppi_clustered()`, and `ppi_lmer()` detect rank-deficient design matrices via QR pivoting, warn clearly, drop aliased columns during fitting, and return `NA` for aliased terms while preserving exact estimates and valid SEs for all identifiable parameters.
- **Binomial GLM Separation Protection:** `ppi_glm()` caps Newton steps, detects quasi-complete separation / Hessian collapse (`separation_detected = TRUE`), emits a clear diagnostic warning, and prevents artificial sandwich SE collapse.
- **Optimal Power-Tuning Robustness:** `ppi_mean()`, `ppi_lm()`, `ppi_glm()`, and `ppi_lmer()` compute optimal $\hat{\lambda}^*$ using the exact separate-sample denominator `V_tilde_L + (n/N) * V_tilde_U`, remaining finite and well-behaved under constant, zero-variance, heteroskedastic, or perfect LLM predictions.
- **Small Validation Sample Guard:** Added finite-sample `t` reference degrees of freedom and informative diagnostic warnings when labeled validation size `n` is small.
- **Constant/Zero-Variance Psychometric Guards:** `g_study()`, `d_study()`, `calibration_metrics()`, `check_drift()`, and `dpf_test()` now handle constant zero-variance ratings and single-class categorical ratings cleanly without fabricating numerical-dust reliability or emitting spurious `lm` warnings.

---

# llmmetric 0.1.0

- Initial CRAN-grade release of `llmmetric`:
  - Multi-facet structured coding engine (`coding_schema()`, `coding_design()`, `code_texts()`, `aggregate_scores()`, `coding_cache()`, `backend_mock()`, `backend_openai()`, `backend_ollama()`, `simulate_coding_study()`).
  - LLM Psychometrics & Generalizability Suite (`g_study()`, `d_study()`, `calibration_metrics()`, `check_drift()`).
  - Differential Prompt Functioning & Fairness Diagnostics (`dpf_test()`).
  - Error-Corrected Downstream Inference (`ppi_mean()`, `ppi_lm()`, `ppi_glm()`, `ppi_clustered()`, `ppi_lmer()`).
  - Publication-ready `ggplot2::autoplot()` S3 methods, built-in `ema_coded` dataset, and Monte Carlo coverage proof vignette.
