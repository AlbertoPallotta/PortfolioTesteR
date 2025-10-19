# PortfolioTesteR 0.1.3

# PortfolioTesteR 0.1.2

## New Features
- **Tabular ML pipeline**
  - `roll_fit_predict()` — rolling fit/predict for pooled/per-symbol/per-group workflows
  - `ml_backtest()` — one-call wrapper from features → scores → weights → backtest
  - `tune_ml_backtest()` — quick hyperparameter sweeps (e.g., `top_k`, `temperature`)
  
- **Sequence pipeline (deep learning ready)**
  - `roll_fit_predict_seq()` — rolling fit/predict over flattened sequence features
  - `ml_backtest_seq()` — sequence wrapper with in-sample normalization
  - `cv_tune_seq()` — purged/embargoed K-fold CV inside IS window
  - `wf_sweep_tabular()` — walk-forward sweeps with OOS metric distributions
  
- **Diagnostics**
  - `membership_stability()` — portfolio membership drift analysis
  - `turnover_by_date()` — turnover time series
  - `evaluate_scores()`, `ic_series()`, `coverage_by_date()` — score quality utilities
  - `select_top_k_scores_by_group()` — group-aware selection
  
- **Utilities**
  - `demo_sector_map()` and `validate_group_map()` for group-aware modeling
  - RAM safety guards in sequence builders (`max_samples`, `mem_budget_gb`)

## Improvements
- Consistent in-sample normalization for ML workflows
- Examples use `\donttest{}` and `@examplesIf()` for network/DB access
- Vignettes build via `rmarkdown::html_vignette`
- Extended `globals.R` to silence data.table NSE notes
- Documentation improvements and parameter alignment

## Compatibility
- **No breaking changes** — existing strategies continue to work
- Minor API standardization in `validate_no_leakage()`

---

# PortfolioTesteR 0.1.1

*Previous CRAN release*

## New Features
- Walk-forward optimization: `run_walk_forward()` with `wf_report()`
- Parameter grid optimization: `run_param_grid()`
- S3 print methods for optimization results

## Improvements
- Examples wrapped in `\donttest{}` for CRAN compliance
- SQL examples guard on RSQLite availability
- Added `list_examples()` and `run_example()` helpers
