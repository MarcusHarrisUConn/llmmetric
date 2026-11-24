## R CMD check results

Checked locally on macOS (`aarch64-apple-darwin23`, R 4.6.1) with both:

- `rcmdcheck::rcmdcheck()` (full check including PDF manual)
- `rcmdcheck::rcmdcheck(args = c("--as-cran"), env = c("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false", "_R_CHECK_LIMIT_CORES_" = "true"))`

Results: **0 errors | 0 warnings | 0 notes**.

## Test, Coverage & Spelling Verification

- `devtools::test()`: all unit tests, branch-coverage tests, adversarial security tests, peer red-team regression tests (`test-peer-redteam.R`), numerical edge-case tests, and Monte Carlo coverage proofs (standard PPI++, clustered/multilevel PPI++, active Horvitz-Thompson IPW-PPI++, and cross-fitted non-linear calibration) pass with 0 failures, 0 warnings, and 0 skips on macOS, Ubuntu (`devel`, `release`, `oldrel-1`), and Windows (`release`).
- `covr::package_coverage()`: > 92% line coverage across the package.
- `spelling::spell_check_package()`: 0 misspellings (domain terminology maintained in `inst/WORDLIST`).
- All examples, tests, and 5 executable vignettes run completely offline without network access, write only to `tempdir()` with full cleanup, respect `<= 2` CPU cores, and leave global RNG/options state untouched.

## Downstream dependencies

This is a new submission (version 0.5.1); there are no reverse dependencies.
