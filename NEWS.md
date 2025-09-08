# PortfolioTesteR 0.1.0 (2025-09-08)

- Initial CRAN release.
- All examples, tests, and vignettes run **offline** using bundled data.
- Web/DB helpers moved to **Suggests** and guarded with `requireNamespace()`; calls use `pkg::fun()`.
- Added minimal test suite (testthat 3e) covering momentum → selection → weights → backtest.
- Documented datasets: `sample_prices_weekly`, `sample_prices_daily`, `sample_sp500_sectors`.
- Fixed roxygen cross-references and data docs; improved DESCRIPTION.
- Added R-hub v2 and win-builder checks; 0 errors / 0 warnings (one benign Windows time NOTE).
