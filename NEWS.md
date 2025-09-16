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
  - New `sql_adapter_adjusted()` for adjusted prices.
  - New `load_mixed_symbols()` to load equities together with VIX safely.
  - `convert_to_nweeks()` handles monthly via `n = 4`; `sql_adapter()` monthly path fixed.
- Backtest printing/plotting tidied; drawdown calc hardened via `safe_divide()`.

## Documentation / Namespace
- Added `@param x` and `@param ...` to S3 print methods to satisfy CRAN checks.
- `wf_stitch()` now declares `@importFrom stats aggregate` (updated NAMESPACE).
- Expanded CSV adapter and validation examples.

## Compatibility
- No breaking API changes; existing examples and vignettes continue to work.
