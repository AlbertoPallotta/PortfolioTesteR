## Test environments
* Local Windows 10, R 4.2.1 — PASS
* R-hub v2 (GitHub Actions):
  - windows (R-devel) — PASS
  - macos (R-devel) — PASS
  - ubuntu-release — PASS
* win-builder (R-devel) — (triggered via devtools::check_win_devel())

## R CMD check results
0 errors | 0 warnings | 1 note

* NOTE: "unable to verify current time" — Windows timestamp check quirk, not package-specific.
* Examples, vignettes, and tests run offline using bundled data.
* Optional dependencies in Suggests (quantmod, RSQLite, rvest) are guarded with `requireNamespace()` and called as `pkg::fun()`.
