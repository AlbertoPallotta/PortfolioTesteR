# PortfolioTesteR 0.1.2 — CRAN submission

## Release summary
This release adds an ML backtesting pipeline and sequence (DL-ready) utilities, plus documentation and CRAN hygiene improvements.

- **Tabular ML:** `roll_fit_predict()`, `ml_backtest()`, `tune_ml_backtest()`.
- **Sequence pipeline:** `roll_fit_predict_seq()`, `ml_backtest_seq()`, `cv_tune_seq()` (purged/embargoed CV), `wf_sweep_tabular()`.
- **Diagnostics:** `evaluate_scores()`, `ic_series()`, `coverage_by_date()`, `membership_stability()`, `turnover_by_date()`.
- **CRAN-proofing:** examples use `\donttest{}` or `@examplesIf()` for network/DB; data.table NSE globals declared; ASCII-only code; vignettes use `rmarkdown::html_vignette` (no Quarto dependency at build time).

## Test environments
* Windows (local): R 4.2.1, x86_64-w64-mingw32  
* macOS (local): R 4.x (if applicable)  
* Linux (GitHub Actions / local): R release  
<!-- If you ran them, keep the next two lines; otherwise delete. -->
<!-- * win-builder: R-devel — OK -->
<!-- * rhub: Ubuntu 22.04 (R-release), Fedora (R-devel) — OK -->

## R CMD check results
0 errors | 0 warnings | 0 notes

## Notes for CRAN
* Examples that would download data or run heavier ML are wrapped in `\donttest{}` and/or `@examplesIf()` (conditional on suggested packages and network).
* Tests that would be slow or depend on online data are guarded with `testthat::skip_on_cran()`.
* Vignettes are lightweight and use `rmarkdown::html_vignette`; no external services are called during build.
* Non-ASCII tokens were removed from code (e.g., replaced `×` with `x` in docs).
* No reverse dependencies.
