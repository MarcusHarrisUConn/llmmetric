## R CMD check results

Checked locally on macOS (`aarch64-apple-darwin23`, R 4.6.1) with both:

- `rcmdcheck::rcmdcheck()` (full check including PDF manual)
- `rcmdcheck::rcmdcheck(args = c("--as-cran"), env = c("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false", "_R_CHECK_LIMIT_CORES_" = "true"))`

Results: **0 errors | 0 warnings | 0 notes**.

## Test & Spelling Verification

- `devtools::test()`: all unit tests, adversarial security tests, numerical edge-case tests, and Monte Carlo coverage proofs pass with 0 failures, 0 warnings, and 0 skips.
- `spelling::spell_check_package()`: 0 misspellings (domain terminology maintained in `inst/WORDLIST`).
- All examples, tests, and vignettes run completely offline without network access, write only to `tempdir()` with full cleanup, respect `<= 2` CPU cores, and leave global RNG/options state untouched.

## Downstream dependencies

This is a new submission (version 0.3.0); there are no reverse dependencies.
