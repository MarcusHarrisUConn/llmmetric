# llmmetric Architecture & Formal Statistical Formulation

`llmmetric` treats large language models (LLMs) explicitly as fallible
psychometric measurement instruments. It integrates four tightly
connected layers:

1.  **Multi-Facet Structured Coding Engine** — reproducible
    `unit x prompt x model x temperature x replicate` orchestration with
    strict JSON schema validation, content-addressed caching, retry
    handling, and pluggable backends
    ([`backend_openai()`](https://marcusharrisphd.com/llmmetric/reference/backend_openai.md),
    [`backend_ollama()`](https://marcusharrisphd.com/llmmetric/reference/backend_ollama.md),
    [`backend_mock()`](https://marcusharrisphd.com/llmmetric/reference/backend_mock.md)).
2.  **LLM Psychometrics & Generalizability Suite** — Generalizability
    Theory
    ([`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md),
    [`d_study()`](https://marcusharrisphd.com/llmmetric/reference/d_study.md)),
    calibration against human gold standards
    ([`calibration_metrics()`](https://marcusharrisphd.com/llmmetric/reference/calibration_metrics.md)),
    and temporal/version drift diagnostics
    ([`check_drift()`](https://marcusharrisphd.com/llmmetric/reference/check_drift.md)).
3.  **Differential Prompt Functioning (DPF) & Fairness Diagnostics** —
    uniform and non-uniform measurement invariance testing across
    demographic or experimental subgroups
    ([`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md)),
    including prompt-level FDR screening and prompt-by-group interaction
    tests.
4.  **Measurement-Error-Corrected Downstream Inference** —
    Prediction-Powered Inference
    ([`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md),
    [`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md),
    [`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md)),
    optimal power-tuning (`PPI++`), and clustered/multilevel extensions
    ([`ppi_clustered()`](https://marcusharrisphd.com/llmmetric/reference/ppi_clustered.md),
    [`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md))
    providing asymptotically valid parameter estimates, confidence
    intervals, and hypothesis tests when combining large LLM-annotated
    datasets with smaller human-validated subsets.

------------------------------------------------------------------------

## 1. Formal Statistical Formulation

### 1.1 Generalizability Theory (G-Study and D-Study)

Let $`Y_{u, f_1, \dots, f_K, r}`$ denote the numeric score assigned to
measurement object (unit/text/person) $`u \in \{1, \dots, n_u\}`$ under
crossed measurement facets $`f_k \in \{1, \dots, n_{f_k}\}`$
($`k = 1, \dots, K`$, such as prompt $`p`$, model $`m`$, temperature
$`t`$) and replicate $`r \in \{1, \dots, n_r\}`$.

Under a fully crossed random-effects G-theory model (Brennan, 2001;
Shavelson & Webb, 1991), the observed score decomposes into orthogonal
random effects indexed by non-empty subsets
$`S \subseteq \{u\} \cup \mathcal{F}`$, where
$`\mathcal{F} = \{f_1, \dots, f_K\}`$:

``` math
Y_{u, f_1, \dots, f_K, r} = \mu + \sum_{\emptyset \neq S \subseteq \{u\} \cup \mathcal{F}} v_S + e_{u, f_1, \dots, f_K, r}, \qquad v_S \stackrel{\text{ind}}{\sim} (0, \sigma_S^2), \quad e \sim (0, \sigma_e^2).
```

For the canonical two-facet crossed design ($`u \times p \times m`$,
without replicates $`n_r = 1`$), the highest-order interaction
$`u \times p \times m`$ is confounded with residual error:

``` math
Y_{upm} = \mu + v_u + v_p + v_m + v_{up} + v_{um} + v_{pm} + v_{upm,e}.
```

#### Variance Component Estimation

- **REML (primary):** Estimated via `lme4::lmer(..., REML = TRUE)` with
  crossed random intercepts for `u`, each facet $`f_k`$, and interaction
  terms up to the estimable order. REML naturally handles unbalanced
  designs and guarantees non-negative variance component estimates
  ($`\hat{\sigma}_S^2 \ge 0`$).
- **Expected Mean Squares / ANOVA (fallback & option):** For balanced
  designs or when REML fails to converge,
  [`g_study()`](https://marcusharrisphd.com/llmmetric/reference/g_study.md)
  solves the exact linear system of Type-I/balanced ANOVA expected mean
  squares and truncates negative variance estimates to zero per Brennan
  (2001).

#### Error Variances and Reliability Coefficients

For a Decision Study (D-study) aggregating scores over facet sizes
$`\mathbf{n} = (n_{f_1}, \dots, n_{f_K})`$ and $`n_r`$ replicates per
cell:

- **Universe-score variance:** $`\sigma_\tau^2 = \sigma_u^2`$.
- **Relative error variance** (relevant for rank-ordering units): sums
  all components involving both $`u`$ and at least one facet, plus
  residual error:
  ``` math
  \sigma_\delta^2(\mathbf{n}) = \sum_{\substack{S \ni u \\ S \cap \mathcal{F} \neq \emptyset}} \frac{\sigma_S^2}{\prod_{f \in S \cap \mathcal{F}} n_f} + \frac{\sigma_e^2}{n_r \prod_{f \in \mathcal{F}} n_f}.
  ```
- **Absolute error variance** (relevant for absolute cutoffs,
  criterion-referenced scoring, and mean comparisons): adds all facet
  main effects and pure facet interactions:
  ``` math
  \sigma_\Delta^2(\mathbf{n}) = \sigma_\delta^2(\mathbf{n}) + \sum_{\substack{S \not\ni u \\ S \cap \mathcal{F} \neq \emptyset}} \frac{\sigma_S^2}{\prod_{f \in S \cap \mathcal{F}} n_f}.
  ```
- **Generalizability coefficient ($`E\rho^2`$) and Dependability index
  ($`\Phi`$):**
  ``` math
  E\rho^2(\mathbf{n}) = \frac{\sigma_u^2}{\sigma_u^2 + \sigma_\delta^2(\mathbf{n})}, \qquad \Phi(\mathbf{n}) = \frac{\sigma_u^2}{\sigma_u^2 + \sigma_\Delta^2(\mathbf{n})}.
  ```

For the two-facet $`u \times p \times m`$ design ($`n_r = 1`$), this
simplifies to:

``` math
\sigma_\delta^2(n_p, n_m) = \frac{\sigma_{up}^2}{n_p} + \frac{\sigma_{um}^2}{n_m} + \frac{\sigma_{upm,e}^2}{n_p n_m}, \qquad
\sigma_\Delta^2(n_p, n_m) = \frac{\sigma_p^2}{n_p} + \frac{\sigma_m^2}{n_m} + \frac{\sigma_{pm}^2}{n_p n_m} + \sigma_\delta^2(n_p, n_m).
```

[`d_study()`](https://marcusharrisphd.com/llmmetric/reference/d_study.md)
evaluates $`(E\rho^2, \Phi)`$ over a user-specified grid of facet
counts, computes single-facet asymptotes (e.g.,
$`\lim_{n_p \to \infty} E\rho^2(n_p, n_m)`$), and identifies
minimum-cost designs attaining a target reliability threshold (e.g.,
$`\ge 0.85`$).

------------------------------------------------------------------------

### 1.2 Calibration Metrics and Drift Testing

#### Calibration vs. Human Gold Standards (`calibration_metrics()`)

For human gold-standard scores $`Y_i`$ and LLM scores $`\hat{Y}_i`$
($`i = 1, \dots, n`$):

- **Continuous outcomes:** Pearson $`r`$, Spearman $`\rho`$, Lin’s
  Concordance Correlation Coefficient (CCC), two-way random
  absolute-agreement $`\text{ICC}(A,1)`$ and consistency
  $`\text{ICC}(C,1)`$, MAE, RMSE, mean bias
  ($`\bar{\hat{Y}} - \bar{Y}`$), SD ratio, and calibration regression
  $`Y_i = \alpha + \beta \hat{Y}_i + \varepsilon_i`$ with joint
  $`F`$-test of ideal calibration $`H_0: (\alpha, \beta) = (0, 1)`$.
- **Categorical/ordinal outcomes:** Accuracy, Macro-$`F_1`$, unweighted
  and linearly/quadratically weighted Cohen’s $`\kappa`$, and exact
  coincidence-matrix Krippendorff’s $`\alpha`$ (nominal or ordinal
  metric).

#### Temporal & Model-Version Drift (`check_drift()`)

Given anchor texts scored across timepoints or model versions
$`v \in \{1, \dots, V\}`$ with baseline $`v_0`$:

- Tests overall mean invariance via within-unit repeated-measures
  modeling (`score ~ version + (1 | unit_id)` / paired comparisons),
  reporting paired mean shift, paired Cohen’s $`d_z`$, variance ratio,
  anchor-set Pearson $`r`$, Spearman $`\rho`$, $`\text{ICC}(A,1)`$, MAE,
  and two-sample Kolmogorov-Smirnov statistic $`D`$ against $`v_0`$.

------------------------------------------------------------------------

### 1.3 Differential Prompt Functioning (DPF) & Measurement Invariance

Adapting psychometric Differential Item Functioning (Swaminathan &
Rogers, 1990; Zumbo, 1999) to LLM scoring,
[`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md)
tests whether LLM score $`S_i`$ exhibits subgroup-dependent bias across
observed group $`G_i`$ conditional on human gold-standard score $`Y_i`$:

- **Model 0 (Invariant):**
  $`g(\mathbb{E}[S_i \mid Y_i]) = \beta_0 + \beta_1 Y_i`$ (or polynomial
  basis $`P_d(Y_i)`$)
- **Model 1 (Uniform DPF):**
  $`g(\mathbb{E}[S_i \mid Y_i, G_i]) = \beta_0 + \beta_1 Y_i + \boldsymbol{\gamma}^\top \mathbf{D}(G_i)`$
- **Model 2 (Non-Uniform DPF):**
  $`g(\mathbb{E}[S_i \mid Y_i, G_i]) = \beta_0 + \beta_1 Y_i + \boldsymbol{\gamma}^\top \mathbf{D}(G_i) + \boldsymbol{\eta}^\top (Y_i \cdot \mathbf{D}(G_i))`$

where $`g`$ is the identity link for continuous scores (OLS) or logit
link for binary scores (binomial GLM).

- **Hypothesis tests:**
  - *Uniform DPF*: $`H_0: \boldsymbol{\gamma} = \mathbf{0}`$ (Model 1
    vs. Model 0)
  - *Non-uniform DPF*: $`H_0: \boldsymbol{\eta} = \mathbf{0}`$ (Model 2
    vs. Model 1)
  - *Omnibus (Total) DPF*:
    $`H_0: \boldsymbol{\gamma} = \mathbf{0}, \boldsymbol{\eta} = \mathbf{0}`$
    (Model 2 vs. Model 0)
  - When repeated rows per unit exist (e.g., multiple prompts per unit),
    [`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md)
    computes unit-clustered sandwich Wald $`\chi^2`$ tests to prevent
    pseudoreplication Type-I error inflation.
- **Effect sizes:** $`\Delta R^2`$ (OLS) or Nagelkerke $`\Delta R_N^2`$
  (logistic), Cohen’s $`f^2`$, and conditional subgroup bias estimates
  at $`\bar{Y}`$ with standard errors and 95% confidence intervals.
- **Prompt-level screening:** When `prompt_col` is supplied,
  [`dpf_test()`](https://marcusharrisphd.com/llmmetric/reference/dpf_test.md)
  runs per-prompt DPF tests with Benjamini-Hochberg FDR adjustment and
  tests the global Prompt $`\times`$ Group interaction controlling for
  $`Y_i`$.

------------------------------------------------------------------------

### 1.4 Prediction-Powered Inference (PPI, PPI++, and Clustered/Multilevel Extensions)

Let the observed data consist of: - **Labeled validation sample**
$`\mathcal{L} = \{(X_i, Y_i, \hat{Y}_i)\}_{i=1}^n`$ with human gold
standard $`Y_i`$ and LLM score $`\hat{Y}_i`$, - **Unlabeled sample**
$`\mathcal{U} = \{(X_j^u, \hat{Y}_j^u)\}_{j=1}^N`$ with covariates and
LLM scores only, drawn independently from the same population.

Let $`\theta^* = \arg\min_\theta \mathbb{E}[\ell_\theta(X, Y)]`$ denote
the target population parameter solving
$`\mathbb{E}[g_\theta(X, Y)] = 0`$, where
$`g_\theta = \nabla_\theta \ell_\theta`$.

#### Power-Tuned M-Estimation (`ppi_mean()`, `ppi_lm()`, `ppi_glm()`)

Following Angelopoulos, Bates, Fannjiang, Jordan, & Zrnic (2023) and
Angelopoulos, Duchi, & Zrnic (2024), the PPI++ objective with tuning
weight $`\lambda`$ is:

``` math
L_\lambda^{\text{PPI++}}(\theta) = \frac{1}{n}\sum_{i \in \mathcal{L}} \ell_\theta(X_i, Y_i) + \lambda\left(\frac{1}{N}\sum_{j \in \mathcal{U}} \ell_\theta(X_j^u, \hat{Y}_j^u) - \frac{1}{n}\sum_{i \in \mathcal{L}} \ell_\theta(X_i, \hat{Y}_i)\right).
```

- $`\lambda = 0`$ recovers the classical human-only estimator;
  $`\lambda = 1`$ recovers standard PPI.
- Because
  $`\mathbb{E}[\nabla_\theta \ell_{\theta^*}(X^u, \hat{Y}^u) - \nabla_\theta \ell_{\theta^*}(X, \hat{Y})] = \mathbf{0}`$
  for *any* (possibly biased, noisy, or nonlinearly distorted) LLM
  predictor $`\hat{Y}`$,
  $`\hat{\theta}_\lambda = \arg\min_\theta L_\lambda^{\text{PPI++}}(\theta)`$
  is consistent and asymptotically normal around $`\theta^*`$:
  ``` math
  \sqrt{n}(\hat{\theta}_\lambda - \theta^*) \xrightarrow{d} \mathcal{N}\!\left(\mathbf{0},\; H^{-1} \Sigma(\lambda) H^{-1}\right),
  ```
  with plug-in sandwich covariance:
  ``` math
  \widehat{\text{Var}}(\hat{\theta}_\lambda) = \hat{H}_\lambda^{-1}\left[
    \frac{1}{n}\widehat{\text{Cov}}_{\mathcal{L}}\!\big(g_i(\hat{\theta}_\lambda) - \lambda \tilde{g}_i(\hat{\theta}_\lambda)\big)
    + \frac{\lambda^2}{N}\widehat{\text{Cov}}_{\mathcal{U}}\!\big(\tilde{g}_j^u(\hat{\theta}_\lambda)\big)
  \right]\hat{H}_\lambda^{-1},
  ```
  where
  $`\tilde{g}_i(\theta) = \nabla_\theta \ell_\theta(X_i, \hat{Y}_i)`$
  and sample covariances are explicitly centered to account for non-zero
  $`\mathbb{E}[\tilde{g}_i(\theta^*)]`$ under biased LLMs.
- **Canonical-link GLMs (`gaussian`, `binomial`, `poisson`):** For
  cumulant $`b(\eta)`$ with $`\mu(\eta) = b'(\eta)`$ and
  $`v(\eta) = b''(\eta)`$, the Hessian
  $`\hat{H}_\lambda(\theta) = (1 - \lambda)\frac{1}{n}X_{\mathcal{L}}^\top W_{\mathcal{L}}(\theta) X_{\mathcal{L}} + \lambda\frac{1}{N}X_{\mathcal{U}}^\top W_{\mathcal{U}}(\theta) X_{\mathcal{U}}`$
  is strictly positive definite for all $`\lambda \in [0, 1]`$, solved
  via damped Newton-Raphson / IRLS.
- **Optimal power tuning ($`\hat{\lambda}^*`$):** Minimizing
  $`\text{tr}\big(\widehat{\text{Var}}(\hat{\theta}_\lambda)\big)`$
  yields:
  ``` math
  \hat{\lambda}^* = \frac{\text{tr}\!\left(\hat{H}^{-1} \hat{C}_{g,\tilde{g}} \hat{H}^{-1}\right)}{\left(1 + \frac{n}{N}\right)\text{tr}\!\left(\hat{H}^{-1} \hat{V}_{\tilde{g}} \hat{H}^{-1}\right)},
  ```
  where
  $`\hat{C}_{g,\tilde{g}} = \widehat{\text{Cov}}_{\mathcal{L}}(g_i, \tilde{g}_i)`$
  and
  $`\hat{V}_{\tilde{g}} = \widehat{\text{Cov}}_{\mathcal{L}\cup\mathcal{U}}(\tilde{g})`$.
  For
  [`ppi_mean()`](https://marcusharrisphd.com/llmmetric/reference/ppi_mean.md),
  $`\hat{\lambda}^*`$ is unconstrained (exact optimal control variate);
  for
  [`ppi_lm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lm.md)
  and
  [`ppi_glm()`](https://marcusharrisphd.com/llmmetric/reference/ppi_glm.md),
  $`\hat{\lambda}^*`$ is clipped to $`[0, 1]`$ to preserve finite-sample
  convexity of $`L_\lambda`$.
- **Coordinate-wise Control-Variate Calibration
  (`method = "calibrated"`):** Fits separate models on $`\mathcal{L}`$
  (human), $`\mathcal{L}`$ (LLM), and $`\mathcal{U}`$ (LLM) to compute
  $`\hat{\theta}_{\text{cv}} = \hat{\theta}_{\mathcal{L}} + \hat{\Lambda}^*(\hat{\theta}_{\tilde{\mathcal{U}}} - \hat{\theta}_{\tilde{\mathcal{L}}})`$
  using coordinate-optimal influence-function weights
  $`\hat{\Lambda}^* = \text{diag}(\hat{\lambda}_1^*, \dots, \hat{\lambda}_p^*)`$.
  This estimator handles severe slope attenuation *and* LLM-coded
  predictors seamlessly while preserving valid sandwich inference.

#### Clustered & Multilevel PPI++ (`ppi_clustered()`, `ppi_lmer()`)

Suppose observations belong to clusters
$`c \in \mathcal{C} = \mathcal{C}_{\mathcal{L}} \cup \mathcal{C}_{\mathcal{U}}`$
(e.g., EMA entries within participants, students within schools). Define
centered observation scores
$`d_i^c = (g_i - \lambda \tilde{g}_i) - \bar{d}_{\mathcal{L}}`$ and
$`u_j^c = \tilde{g}_j^u - \bar{\tilde{g}}_{\mathcal{U}}`$.

- **Disjoint cluster validation
  ($`\mathcal{C}_{\mathcal{L}} \cap \mathcal{C}_{\mathcal{U}} = \emptyset`$):**
  ``` math
  \hat{M}_{\text{disjoint}} = \frac{1}{n^2}\frac{G_{\mathcal{L}}}{G_{\mathcal{L}} - 1}\sum_{c \in \mathcal{C}_{\mathcal{L}}} \left(\sum_{i \in \mathcal{L}_c} d_i^c\right)\!\left(\sum_{i \in \mathcal{L}_c} d_i^c\right)^\top
  + \frac{\lambda^2}{N^2}\frac{G_{\mathcal{U}}}{G_{\mathcal{U}} - 1}\sum_{c \in \mathcal{C}_{\mathcal{U}}} \left(\sum_{j \in \mathcal{U}_c} u_j^c\right)\!\left(\sum_{j \in \mathcal{U}_c} u_j^c\right)^\top.
  ```
- **Overlapping/within-cluster validation
  ($`\mathcal{C}_{\mathcal{L}} \cap \mathcal{C}_{\mathcal{U}} \neq \emptyset`$):**
  Define unified cluster score
  $`S_c(\lambda) = \frac{1}{n}\sum_{i \in \mathcal{L}_c} d_i^c + \frac{\lambda}{N}\sum_{j \in \mathcal{U}_c} u_j^c = A_c - \lambda B_c`$.
  Since distinct clusters are independent,
  ``` math
  \hat{M}_{\text{shared}} = \frac{G}{G - 1}\sum_{c \in \mathcal{C}} S_c(\lambda) S_c(\lambda)^\top, \qquad
  \hat{\lambda}^*_{\text{cluster}} = \frac{\sum_{c \in \mathcal{C}} (\hat{H}^{-1} A_c)^\top (\hat{H}^{-1} B_c)}{\sum_{c \in \mathcal{C}} \|\hat{H}^{-1} B_c\|_2^2}.
  ```
  This automatically captures within-cluster correlation between labeled
  and unlabeled entries from the same cluster.
- **Random-intercept multilevel extension
  ([`ppi_lmer()`](https://marcusharrisphd.com/llmmetric/reference/ppi_lmer.md)):**
  Estimates random-intercept variance components
  $`(\hat{\tau}^2, \hat{\sigma}^2)`$ via
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html), forms exact
  Woodbury GLS weights
  $`V_c^{-1} = \hat{\sigma}^{-2}\big(I_{m_c} - \frac{\hat{\tau}^2}{\hat{\sigma}^2 + m_c\hat{\tau}^2}\mathbf{1}\mathbf{1}^\top\big)`$,
  solves the GLS-weighted PPI++ estimating equations, and computes
  cluster-robust PPI++ sandwich standard errors alongside human-only and
  naive-LLM multilevel benchmarks.

------------------------------------------------------------------------

## 2. Modular Package Architecture

``` text
llmmetric/
├── DESCRIPTION                 # Package metadata, lean Imports/Suggests
├── NAMESPACE                   # Generated via roxygen2
├── ARCHITECTURE.md             # Formal math & design blueprint (this file)
├── README.md                   # Overview, installation, quickstart examples
├── R/
│   ├── llmmetric-package.R     # Package-level docs & shared imports
│   ├── utils.R                 # Validation helpers, tidy formatting, math utilities
│   ├── schema.R                # coding_schema(), JSON Schema export, strict validation
│   ├── cache.R                 # coding_cache() content-addressed memory/disk cache
│   ├── backends.R              # backend_mock(), backend_openai(), backend_ollama()
│   ├── coding.R                # coding_design(), code_texts(), aggregate_scores()
│   ├── g_study.R               # g_study() REML + ANOVA multi-facet variance engine
│   ├── d_study.R               # d_study() reliability projections & target solver
│   ├── calibration.R           # calibration_metrics() continuous & categorical metrics
│   ├── drift.R                 # check_drift() temporal/version stability tests
│   ├── dpf.R                   # dpf_test() uniform/non-uniform & prompt-level DPF
│   ├── ppi_core.R              # Shared PPI/PPI++ data splitting, optimal lambda, sandwich core
│   ├── ppi_models.R            # ppi_mean(), ppi_lm(), ppi_glm() + S3 methods
│   ├── ppi_clustered.R         # ppi_clustered(), ppi_lmer() cluster/multilevel PPI++
│   ├── autoplot.R              # ggplot2::autoplot() S3 methods for all diagnostic objects
│   ├── simulate.R              # simulate_coding_study() synthetic study generator
│   └── data.R                  # Documentation for built-in dataset `ema_coded`
├── data/
│   └── ema_coded.rda           # Compact multi-facet EMA coding study dataset
├── tests/
│   ├── testthat.R
│   └── testthat/
│       ├── test-schema-cache.R
│       ├── test-backends-coding.R
│       ├── test-g-d-study.R
│       ├── test-calibration-drift.R
│       ├── test-dpf.R
│       ├── test-ppi-models.R
│       ├── test-ppi-clustered.R
│       ├── test-ppi-coverage.R # Monte Carlo 95% CI coverage proofs (biased/noisy LLMs)
│       └── test-autoplot.R
└── vignettes/
    ├── getting-started.Rmd     # End-to-end psychometric workflow
    └── ppi-coverage-proof.Rmd  # Monte Carlo proof: Naive LLM vs Human-Only vs PPI/PPI++
```

------------------------------------------------------------------------

## 3. Core Design Invariants

1.  **Dual-input ergonomics:** Psychometric and inference functions
    accept both `llm_coded_data` objects produced by
    [`code_texts()`](https://marcusharrisphd.com/llmmetric/reference/code_texts.md)
    and standard `data.frame`/`tibble` objects from external coding
    pipelines.
2.  **Offline reproducibility:** Every test, example, and vignette runs
    deterministically offline using
    [`backend_mock()`](https://marcusharrisphd.com/llmmetric/reference/backend_mock.md)
    and mockable HTTP transports for
    [`backend_openai()`](https://marcusharrisphd.com/llmmetric/reference/backend_openai.md)
    /
    [`backend_ollama()`](https://marcusharrisphd.com/llmmetric/reference/backend_ollama.md).
3.  **Zero global RNG pollution:**
    [`backend_mock()`](https://marcusharrisphd.com/llmmetric/reference/backend_mock.md)
    derives deterministic per-cell hashes from
    `(seed, unit_id, prompt_id, model, temperature, replicate)` while
    preserving `.Random.seed`.
4.  **CRAN hygiene:** Zero writes to user home directories (caches
    default to in-memory or user-supplied paths), zero non-standard
    evaluation `R CMD check` notes, clean S3
    `print`/`summary`/`autoplot` contracts.
