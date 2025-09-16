# CRAN submission: PortfolioTesteR 0.1.1

## Test environments
* Local Windows 10, R 4.2.1 — PASS
* win-builder (R-devel) — PASS
* R-hub v2 (GitHub Actions): Windows (R-devel), macOS (R-devel), Ubuntu (release) — PASS

## R CMD check results
0 errors | 0 warnings | 0 notes

All examples/vignettes build successfully. Any Quarto console output seen locally does not affect checks.

## Resubmission (0.1.1)
* Replaced all \dontrun{} with \donttest{} or @examplesIf(...); re-roxygenized docs.
* Added @importFrom stats aggregate for `wf_stitch()`; NAMESPACE regenerated.
* Fixed S3 docs for print methods (x, ... + aliases): param_grid_result, wf_optimization_result, backtest_result.
* Removed DBI from examples; SQLite examples now guard with @examplesIf(requireNamespace("RSQLite", quietly = TRUE) && file.exists("sp500.db")).
* New optional APIs: run_param_grid(), metric_sharpe(), run_walk_forward(), wf_stitch(), wf_report().

## Additional notes
* roxygen2 re-run; NAMESPACE and Rd are up to date.

