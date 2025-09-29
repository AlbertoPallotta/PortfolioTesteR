## Test environments
* local Windows 10, R 4.2.1
* GitHub Actions (ubuntu-latest, macOS-latest, windows-latest), R release
* [optional: win-builder (devel and release)]

## R CMD check results
0 errors | 0 warnings | 0 notes

## This is a resubmission
This is an update from version 0.1.1 to 0.1.2, adding ML backtesting functionality.

## Major changes
* Added ML backtesting pipeline (`roll_fit_predict()`, `ml_backtest()`, `tune_ml_backtest()`)
* Added sequence/deep learning utilities (`roll_fit_predict_seq()`, `ml_backtest_seq()`, `cv_tune_seq()`)
* Added diagnostic functions (`evaluate_scores()`, `ic_series()`, `coverage_by_date()`)

## CRAN compliance
* Examples using network/ML wrapped in `\donttest{}` or `@examplesIf()`
* Tests requiring external resources use `testthat::skip_on_cran()`
* Vignettes use `rmarkdown::html_vignette` (no Quarto dependency)
* Fixed all non-ASCII characters in documentation
* Added data.table NSE globals to avoid NOTEs

## Reverse dependencies
There are no reverse dependencies.

