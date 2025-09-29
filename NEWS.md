# PortfolioTesteR 0.1.2

## New
- **Tabular ML pipeline**
  - `roll_fit_predict()` — rolling fit/predict for pooled / per-symbol / per-group workflows.
  - `ml_backtest()` — one-call wrapper to go from features → scores → weights → backtest.
  - `tune_ml_backtest()` — quick hyper-parameter sweeps (e.g., `top_k`, `temperature`, `method`, `transform`).

- **Sequence (deep-learning-ready) pipeline**
  - `roll_fit_predict_seq()` — rolling fit/predict over flattened sequence features (steps-by-p).
  - `ml_backtest_seq()` — wrapper for sequences with in-sample normalization and standard selection/weighting/caps.
  - **CV for sequences:** `cv_tune_seq()` implements purged/embargoed K-fold inside the IS window.
  - **Walk-forward sweeps:** `wf_sweep_tabular()` reports distributions of OOS metrics across WF windows.

- **Diagnostics**
  - `membership_stability()` — turnover-free view of portfolio membership drift.
  - `turnover_by_date()` — simple turnover series for backtests.
  - `evaluate_scores()`, `ic_series()`, `coverage_by_date()` — score quality & coverage utilities.

- **Utilities / examples**
  - `demo_sector_map()` and `validate_group_map()` for group-aware modeling.
  - RAM safety guards in sequence builders (caps via `max_samples`, `mem_budget_gb`, informative warnings).

## Improvements
- Consistent IS-only normalization for ML workflows; leakage guards tightened.
- Examples use `\donttest{}` and `@examplesIf()` for any network/DB access; heavy code paths kept tiny by default.
- Vignettes build with standard `rmarkdown::html_vignette`; removed Quarto shell probes.
- `globals.R` extended to silence `data.table` NSE NOTES on CRAN.
- Documentation pass: clarified “steps-by-p” wording; added missing parameters and aligned argument names.

## Compatibility
- **No breaking changes.** Existing strategies and vignettes continue to work.
- Minor doc/API tidy: `validate_no_leakage(features, labels, verbose)` standardized.

---

# PortfolioTesteR 0.1.1

*Initial CRAN release.*

## New
- Walk-forward optimization: `run_walk_forward()` with `wf_report()` and overlap-safe `wf_stitch()`.
- Parameter grid optimization: `run_param_grid()` plus helper metric `metric_sharpe()`.
- S3 print methods for optimization results: `print.wf_optimization_result()` and `print.param_grid_result()`.

## Improvements
- Examples: replaced all `\dontrun{}` with `\donttest{}` and wrapped network/DB cases in `@examplesIf(...)`.
- SQL examples no longer require `DBI`; they now guard on `RSQLite` and a local database file.
- Added helper utilities for packaged examples: `list_examples()` and `run_example()`.
- Data adapters:
  - New `sql_adapter_adjust


