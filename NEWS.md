# llmmetric 0.5.2

- **CI Test Portability (`tests/testthat/test-schema-cache.R`):** Removed `httr2::req_dry_run()` (which required undeclared `httpuv`) in favor of inspecting `httr2::req_body_json()`'s serialized payload directly via `jsonlite::toJSON()`.

---

# llmmetric 0.5.1

## JSON Schema & Structured Output Fixes
- **Single-Field JSON Schema Array Serialization (`as_json_schema()`):** Wrapped `required` and `enum` in `I(as.character(...))` so `httr2::req_body_json(..., auto_unbox = TRUE)` and `jsonlite::toJSON(..., auto_unbox = TRUE)` always serialize `"required": ["score"]` (and `"required": []` / `"enum": [...]`) as JSON arrays rather than bare strings when a schema has a single required field—fixing HTTP 400 rejections on live Ollama `/api/chat` and OpenAI Structured Outputs endpoints.

## Cross-Platform & Windows Compatibility Fixes
- **Windows Path & Symlink Containment (`is_symlink_path()`, `resolve_safe_cache_path()`, `coding_cache()`):** Added `canonical_path_key()` normalizing backslashes, trailing slashes, and Windows case-insensitive drive paths, plus a canonical `normalizePath()` fallback in `is_symlink_path()` so Windows NTFS file symlinks (where `Sys.readlink()` may return `""`) and trailing-slash symlink directories are detected reliably without false positives on Windows 8.3 short paths (`RUNNER~1`) or macOS `/var` temp directories. Also rejects symlinked `dir` upfront in `coding_cache("disk", dir = ...)`.
- **Cross-Platform Permissions & Atomic Writes (`restrict_file_permissions()`, `cache_set()`, `code_texts()`):** Applied `0600` file permissions (`use_umask = FALSE`) exclusively on POSIX platforms (avoiding Windows `Sys.chmod()` quirks), placed disk cache temporary files inside `dirname(path)`, and suppressed fallback `file.rename()` overwrite warnings on Windows.
- **Portable Symlink Tests (`tests/testthat/test-peer-redteam.R`):** Isolated symlink security regression tests with an OS-availability guard (`file.symlink()`), accepted both symlink-rejection and path-escape error messages across platforms, and added direct unit tests for Windows path canonicalization and symlinked checkpoints/cache directories.

---

# llmmetric 0.5.0

## Security Hardening (Independent Peer Red-Team Audit Remediation)
- **Eliminated `cli` Glue Injection / RCE & Secret Exfiltration (`backend_openai()`, `backend_ollama()`):** Replaced dynamic `paste0()` error strings passed into `cli::cli_abort()` with safe variable interpolation (`"{safe_msg}"`), eliminating arbitrary `{...}` glue execution and API-key exfiltration from adversarial HTTP/transport error bodies.
- **Single-Pass Prompt Template Rendering (`render_prompt_template()`):** Rewrote template substitution to scan placeholders exclusively in the original template string in a single pass, preventing untrusted text containing `{ssn}`, `{patient_name}`, or `{human_score}` from triggering second-order substitution of adjacent data-frame columns.
- **Sealed Custom HTTP Headers (`backend_openai(extra_headers = ...)`):** All `extra_headers` values are now stored inside the non-serialized session secret vault (`llm_redacted_secret`) and scrubbed from error messages alongside `api_key`.
- **Disk Cache & Checkpoint Hardening (`resolve_safe_cache_path()`, `cache_set()`, `code_texts()`):** Blocked symlink escapes in disk cache directories/entries and checkpoint files, enforced `0600` owner-only permissions on written cache/checkpoint files, ensured `keep_raw = FALSE` omits raw model outputs from persisted disk caches and checkpoints, rejected directory paths as `checkpoint_file`, and verified atomic write success.
- **Markdown/HTML & LaTeX Escaping (`format_apa_table()`):** Escaped `|`, `<`, `>`, and newlines in Markdown tables (preventing column breakouts and raw HTML/XSS injection) and escaped `{`, `}`, `~`, `\`, and newlines cleanly in LaTeX tables.
- **Strict Untrusted JSON & IPv6 URL Validation (`extract_json_payload()`, `validate_endpoint_url()`):** Rejected multi-object payloads inside raw or fenced responses, prevented unmatched prose double quotes from breaking balanced JSON extraction, filtered `\x7f` DEL bytes, and enforced `[1, 65535]` port range validation on bracketed IPv6 hosts.

## Statistical Accuracy, Boundary Guards & Interop Improvements
- **Cross-Fitted Non-Linear Calibration (`ppi_calibrated()`):** Replaced internal model-frame names with collision-proof identifiers (fixing overwrites when covariates are named `y` or `b_1`, and supporting non-syntactic column names), eliminated held-out `val_df` leakage into unlabeled predictions by determining active fold covariates strictly from training folds, and added upfront domain validation for `calibrator = "logistic"` and canonical GLM families.
- **Horvitz-Thompson Positivity & Effective Sample Size (`select_validation_sample()`, `ppi_mean()`, `ppi_lm()`):** Enforced `n >= H` positivity in stratified sampling (preventing $\pi_i = 0$ and `Inf` weights), rejected non-finite/zero weights across all corpus rows in IPW-PPI, and enforced Kish effective sample-size checks (`n_eff >= 2` / `n_eff > p`) with effective-df `t` inference to prevent zero-SE collapse under extreme weights.
- **Numeric Ordinal Ordering & IRR Guards (`human_irr()`, `calibration_metrics()`):** Fixed lexicographic sorting on numeric ordinal scales (e.g., `1:10` now sorts numerically rather than `"1", "10", "2", ...`), validated `>= 2` distinct active non-NA raters after `na.rm` filtering, and required `>= 2` overlapping multi-rated units in continuous `human_irr()`.
- **Proportion PPI & `confint()` Parity (`ppi_proportion()`):** Prevented inverted clamped CIs (`pmin(pmax(..., 0), 1)` on both endpoints), validated continuous probability inputs in `[0, 1]`, and added `confint.llm_ppi_proportion()` honoring `simultaneous` and `clamp_ci`.
- **Calibrated `pred_map` Alignment & GLM Domain Checks (`ppi_lm()`, `ppi_glm()`):** Jointly harmonized categorical factor levels across human labeled, surrogate labeled, and surrogate unlabeled splits by exact column name, and enforced `[0, 1]` (`binomial()`) and `>= 0` (`poisson()`) domain constraints.
- **Clustered & Multilevel Inference (`ppi_clustered()`, `ppi_lmer()`):** Enforced `n > p`, aligned overlapping-cluster CR1 scaling and Welch-Satterthwaite effective cluster degrees of freedom (so `lambda = 0` matches `human_only` identically), and explicitly rejected unsupported multiple random-effect or random-slope formulas in `ppi_lmer()`.
- **Schema, Drift, D-Study & Cross-Package Interop (`validate_coded_output()`, `check_drift()`, `d_study()`, `g_study()`, `backend_custom()`):** Accepted `NA_character_` on `nullable = TRUE` fields and native R `factor` values under default `coerce = FALSE`; added upfront `na.rm = FALSE` error in `check_drift()` and `is.finite()` checks on `d_study()` `grid`/`costs`; expanded `g_study(facets = NULL)` auto-detection to include `prompt_id`, `seed`, and `pass_id` (seamless `airgapllm::consensus_passes()` interop); and stripped dot-prefixed metadata columns from 1-row data frames returned by `backend_custom()`.

---

# llmmetric 0.4.0

## New Features
- **Stratified & Active Validation Sampling (`select_validation_sample()`):** Draws reproducible human-validation subsets under simple random (`"simple"`), subgroup-stratified (`"stratified"` with `"proportional"`, `"equal"`, or `"neyman"` allocation), score-based Neyman (`"neyman"`), score-quantile (`"quantile"`), and whole-cluster (`"cluster"`) designs, returning exact inclusion probabilities $\pi_i \in (0, 1]$ and Horvitz-Thompson weights $w_i = 1 / \pi_i$ without mutating global `.Random.seed`.
- **Horvitz-Thompson Active / IPW-PPI (`ppi_mean()`, `ppi_lm()`):** Added `weights` and `inclusion_prob` parameters implementing exact Active/IPW-PPI++ (Zrnic & Candès, 2024) with Hájek weight normalization, optimal IPW power-tuning $\hat{\lambda}^*_{\text{IPW}}$, and valid IPW sandwich SEs—preserving nominal ~95% CI coverage under unequal-probability stratified validation sampling (verified via Monte Carlo simulation tests).
- **Cross-Fitted Non-Linear Calibration (`ppi_calibrated()`):** Handles ceiling/floor compression, S-curve saturation, and covariate-dependent non-linear LLM distortion via strict $K$-fold cross-fitted natural cubic splines (`splines::ns()`), polynomial bases, logistic splines, or custom R calibration functions, with fold-weighted unlabeled aggregation, out-of-fold calibration diagnostics (`$calibration_diagnostics`), and 4-way `autoplot()` & `format_apa_table()` comparisons (`ppi++_calibrated`, `ppi++_uncalibrated`, `human_only`, `naive_llm`).
- **Custom R Callable Backend Adapter (`backend_custom()`):** Zero-dependency adapter wrapping any local R function `fn(request)` (e.g., offline `airgapllm` pipelines, local Python/C++ model wrappers, or custom scorers) directly into `code_texts()` with full JSON schema validation, retry handling, content-addressed caching, and resumable checkpointing.

## Bug Fixes & Coverage Hardening
- **`covr`-Driven Latent Bug Audit:** Drove overall line coverage from 83.9% to > 92% via targeted branch tests and fixed three latent edge-case bugs uncovered during the audit:
  - Fixed polynomial basis formula construction in `dpf_test(poly_degree >= 2)` where `I(t_c^1)` collided with `t_c:group` interaction terms, causing `NA` conditional slopes.
  - Preserved column/row `dimnames` across SVD pseudo-inverse fallbacks in `safe_solve()` and `vcov_cluster_fit()`.
  - Added constant-outcome early-return protection in `fit_prompt_group_interaction()` to prevent spurious `summary.lm` warnings on zero-variance ratings.

## Documentation & Vignettes
- Added 5th executable vignette: `stratified-and-nonlinear-ppi` covering `select_validation_sample()`, active IPW-PPI, `ppi_calibrated()`, and `backend_custom()`.
- Updated the standalone Psychometric AI & PPI Methods Guide (`docs/llmmetric-guide.pdf`) and `pkgdown` reference site for version `0.4.0`.

---

# llmmetric 0.3.0

## New Features & Workflow Hardening
- **Categorical & Multi-Class Prevalence PPI (`ppi_proportion()`):** Estimates population class prevalences with per-category optimal PPI++ weights, full joint $K \times K$ covariance matrices, marginal or Bonferroni-adjusted simultaneous confidence intervals, and support for discrete labels, binary probabilities, or multi-class predicted-probability columns.
- **Human Inter-Rater Reliability Ceiling (`human_irr()` + `autoplot.llm_human_irr()`):** Computes human-vs-human reliability across two or more annotators in wide or long format—continuous $\mathrm{ICC}(A,1)$, $\mathrm{ICC}(A,k)$, $\mathrm{ICC}(C,1)$, $\mathrm{ICC}(C,k)$ (with automatic REML handling for unbalanced ratings), pairwise $r/\rho$/RMSE, plus categorical/ordinal percent agreement, Cohen's $\kappa$, Fleiss's $\kappa$ (with analytic SE/CI), and multi-rater Krippendorff's $\alpha$—and benchmarks LLM calibration directly against the human ceiling.
- **Resumable Checkpointed Batch Coding (`code_texts()`):** Added atomic `checkpoint_file` persistence (`checkpoint_every`) and `on_error = c("warn_na", "stop")` partial-failure recovery reporting `$failed_indices`, `$failed_units`, and `$resumed_count`, ensuring mid-batch API rate limits or timeouts never discard completed cells.
- **Publication-Ready APA Tables (`format_apa_table()`):** Formats Markdown, tibble, and LaTeX `booktabs` APA 7th-edition comparison tables contrasting Naive LLM, Human-Only, and PPI / PPI++ estimates, SEs, 95% CIs, APA-formatted p-values, and variance-reduction gains (plus support for `g_study()`, `dpf_test()`, and `human_irr()`).
- **Complex Formula & Factor-Level Alignment:** Hardened `build_ppi_model_matrices()` for interaction formulas (`y ~ x1 * x2 + factor(group) + I(x1^2)`), automatic factor/character alignment across labeled and unlabeled splits, clear warnings when unlabeled data contain unseen factor levels, and actionable errors on single-level constant factor predictors.

## Documentation, Vignettes & CRAN Readiness
- Expanded to 4 executable vignettes: `getting-started`, `ppi-coverage-proof`, `dpf-and-fairness`, and `designing-validation-studies`.
- Added `inst/CITATION`, runnable `inst/examples/ema_psychometric_workflow.R`, `inst/guides/llmmetric-guide.Rmd`, `inst/WORDLIST`, `cran-comments.md`, `_pkgdown.yml`, and GitHub Actions workflows (`.github/workflows/R-CMD-check.yaml`, `.github/workflows/pkgdown.yaml`).
- Verified 0 misspellings via `spelling::spell_check_package()` and `0 errors | 0 warnings | 0 notes` under full `R CMD check` (including PDF manual) and `--as-cran`.

---

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
