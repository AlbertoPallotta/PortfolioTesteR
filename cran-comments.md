## Test environments
* Local Windows 10, R 4.2.1 — PASS
* win-builder (R-devel) — PASS
* R-hub v2 (GitHub Actions): windows (R-devel), macos (R-devel), ubuntu-release — PASS

## R CMD check results
0 errors | 0 warnings | 1 note

* NOTE: "unable to verify current time" — known Windows timestamp check quirk (system clock), not package-specific.

## Resubmission (0.1.1)
* Fix for previous NOTE “Documented arguments not in \usage”:
  The internal helper `calculate_daily_values()` is not part of the user API and is documented with `@keywords internal` and `@noRd`. A stale Rd page existed from an earlier state and has now been removed. No public-facing changes.

## Spelling NOTE justification
CRAN’s spell check flagged the words “backtest”, “backtesting”, and “drawdowns” in DESCRIPTION. These are established terms in quantitative finance and portfolio management used widely in academic and practitioner literature, and are appropriate for the target audience. We prefer to keep them as-is.
