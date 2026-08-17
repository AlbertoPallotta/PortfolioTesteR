# PortfolioTesteR 0.1.4

## New
- `ml_backtest_multi()` — run ML backtests across multiple horizons/schedules.
- `pt_collect_results()` — one-call collector for coverage, IC (raw & OOS-only), turnover, perf tables, and cost sweeps.
- `scores_oos_only()` — keep scores only on OOS decision dates.

## Improvements
- Vignettes stabilized on knitr/rmarkdown (no Quarto dependency during checks).
- Expanded `R/globals.R` to silence data.table NSE NOTES.
- Regenerated docs with UTF-8 to avoid Non-ASCII warnings.

## Compatibility
- No breaking API changes.

