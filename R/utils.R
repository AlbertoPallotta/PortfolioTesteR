# utils.R
# Utility functions for safe operations in momentum strategies library
# These functions prevent common errors with NA values, zero division, etc.

###############################################################################
# SAFE ARITHMETIC OPERATIONS
###############################################################################

#' Safe Division with NA and Zero Handling
#'
#' @description
#' Performs division with automatic handling of NA values, zeros, and infinity.
#' Returns 0 for division by zero and NA cases.
#'
#' @param numerator Numeric vector
#' @param denominator Numeric vector
#'
#' @return Numeric vector with safe division results
#' @export
#' @examples
#' safe_divide(c(10, 0, NA, 5), c(2, 0, 5, NA))  # Returns c(5, 0, 0, 0)
safe_divide <- function(numerator, denominator) {
  result <- numerator / denominator
  result[is.na(result) | is.infinite(result)] <- 0
  return(result)
}
###############################################################################
# SAFE LOGICAL OPERATIONS
###############################################################################

#' Safe ANY Operation with NA Handling
#'
#' @description
#' Performs any() operation that returns FALSE when all values are NA
#' instead of NA.
#'
#' @param x Logical vector
#' @param ... Additional arguments passed to any()
#'
#' @return Logical value (never NA)
#' @keywords internal
safe_any <- function(x, ...) {
  if (all(is.na(x))) return(FALSE)
  return(any(x, na.rm = TRUE, ...))
}

# Safe logical AND that handles NAs
safe_and <- function(x, y) {
  result <- rep(FALSE, length(x))
  valid_idx <- !is.na(x) & !is.na(y)
  result[valid_idx] <- x[valid_idx] & y[valid_idx]
  return(result)
}

###############################################################################
# DATA TYPE OPERATIONS
###############################################################################

#' Ensure Data.Table Without Mutation
#'
#' @description
#' Converts input to data.table if needed, always returning a copy
#' to prevent accidental data mutation. Core safety function used
#' throughout the library.
#'
#' @param data Data.frame or data.table
#'
#' @return Copy of data as data.table
#' @export
#' @examples
#' data("sample_prices_weekly")
#' dt <- ensure_dt_copy(sample_prices_weekly)  # Safe to modify dt
ensure_dt_copy <- function(data) {
  if (!is.data.table(data)) {
    return(as.data.table(data))
  } else {
    return(copy(data))
  }
}

###############################################################################
# VALIDATION HELPERS
###############################################################################

# Check if all values are valid (not NA or Inf)
all_valid <- function(x) {
  all(!is.na(x) & !is.infinite(x))
}

# Count valid (non-NA) values
count_valid <- function(x) {
  sum(!is.na(x))
}

###############################################################################
# ADDITIONAL SAFETY FUNCTIONS
###############################################################################

# Safe sum that returns 0 if all values are NA
safe_sum <- function(x, ...) {
  if (all(is.na(x))) return(0)
  return(sum(x, na.rm = TRUE, ...))
}

# Safe mean that returns NA if all values are NA
safe_mean <- function(x, ...) {
  if (all(is.na(x))) return(NA_real_)
  return(mean(x, na.rm = TRUE, ...))
}

# Safe max that returns -Inf if all values are NA
safe_max <- function(x, ...) {
  if (all(is.na(x))) return(-Inf)
  return(max(x, na.rm = TRUE, ...))
}

# Safe min that returns Inf if all values are NA
safe_min <- function(x, ...) {
  if (all(is.na(x))) return(Inf)
  return(min(x, na.rm = TRUE, ...))
}

###############################################################################
# STRATEGY HELPER FUNCTIONS
###############################################################################

#' Convert Conditions to Selection Format
#'
#' @description
#' Converts condition matrices or data frames to standard selection format
#' with Date column and binary values. Handles NA by converting to 0.
#'
#' @param condition_matrix Matrix or data frame with conditions
#' @param date_column Optional Date vector if not in input
#'
#' @return Data.table in selection format (Date + binary columns)
#' @export
#' @examples
#' data("sample_prices_weekly")
#' ma20 <- calc_moving_average(sample_prices_weekly, 20)
#' above_ma <- filter_above(calc_distance(sample_prices_weekly, ma20), 0)
#' selection <- as_selection(above_ma, sample_prices_weekly$Date)
as_selection <- function(condition_matrix, date_column = NULL) {
  # Handle different input types
  if (is.data.frame(condition_matrix) && "Date" %in% names(condition_matrix)) {
    # Already has Date column
    result <- as.data.table(condition_matrix)
  } else if (is.matrix(condition_matrix) || is.data.frame(condition_matrix)) {
    # Convert to data.table and add Date
    result <- as.data.table(condition_matrix)
    if (!is.null(date_column)) {
      result$Date <- date_column
    } else {
      stop("Need to provide date_column for matrix input")
    }
  } else {
    stop("Input must be a matrix or data.frame")
  }

  # Convert logical to numeric and handle NA
  symbol_cols <- setdiff(names(result), "Date")
  for (col in symbol_cols) {
    # Convert logical to numeric
    if (is.logical(result[[col]])) {
      result[, (col) := as.numeric(get(col))]
    }
    # Convert NA to 0
    result[is.na(get(col)), (col) := 0]
  }

  return(result)
}

# Run complete strategy with natural condition syntax
run_strategy <- function(prices, selection_condition,
                         weighting_method = "equal",
                         signal_for_weighting = NULL,
                         initial_capital = 100000,
                         name = "Strategy",
                         skip_warmup = TRUE,
                         verbose = FALSE) {

  # Convert natural condition to selection format
  selected <- as_selection(selection_condition, prices$Date)

  # Optional: Skip warmup period
  if (skip_warmup) {
    symbol_cols <- setdiff(names(selected), "Date")
    # Find first row where at least one stock is selected
    first_valid <- which(rowSums(selected[, ..symbol_cols]) > 0)[1]

    if (!is.na(first_valid) && first_valid > 1 && verbose) {
      message(sprintf("Skipping %d warmup periods", first_valid - 1))
    }
  }

  # Apply weighting
  if (weighting_method == "equal") {
    weights <- weight_equally(selected)
  } else if (weighting_method == "signal" && !is.null(signal_for_weighting)) {
    signal_selection <- as_selection(signal_for_weighting, prices$Date)
    weights <- weight_by_signal(selected, signal_selection)
  } else if (weighting_method == "rank" && !is.null(signal_for_weighting)) {
    signal_selection <- as_selection(signal_for_weighting, prices$Date)
    weights <- weight_by_rank(selected, signal_selection)
  } else if (weighting_method == "volatility") {
    # For volatility weighting, we need additional parameters
    # This is a simplified version - you might want to expand this
    weights <- weight_by_volatility(selected, prices, low_vol_preference = TRUE)
  } else {
    stop("Invalid weighting method. Choose: 'equal', 'signal', 'rank', or 'volatility'")
  }

  # Run backtest
  run_backtest(prices, weights, initial_capital, name, verbose = verbose)
}
###############################################################################
# REGIME FILTER FUNCTIONS
###############################################################################

# Internal helper - efficient regime application
apply_regime_broadcast <- function(selection_matrix, regime_condition) {
  result <- ensure_dt_copy(selection_matrix)
  symbol_cols <- setdiff(names(result), "Date")

  regime_binary <- as.numeric(!is.na(regime_condition) & regime_condition)

  for (col in symbol_cols) {
    result[, (col) := get(col) * regime_binary]
  }

  return(result)
}

# Main user function - filter selections based on market regime
filter_when <- function(selection_matrix, condition, label = NULL) {
  result <- apply_regime_broadcast(selection_matrix, condition)

  if (!is.null(label) && interactive()) {
    active_pct <- sum(condition, na.rm = TRUE) / sum(!is.na(condition)) * 100
    message(sprintf("%s filter: active %.1f%% of time", label, active_pct))
  }

  return(result)
}


###############################################################################
# DOCUMENTATION
###############################################################################

# Print utility functions information
utils_info <- function() {
  cat("Momentum Strategies Library - Utility Functions\n")
  cat("==============================================\n\n")

  cat("Safe Arithmetic:\n")
  cat("  safe_divide(x, y) - Division with zero/NA protection\n\n")

  cat("Safe Logical:\n")
  cat("  safe_any(x) - any() that handles all NA\n")
  cat("  safe_and(x, y) - Logical AND with NA handling\n\n")

  cat("Data Operations:\n")
  cat("  ensure_dt_copy(data) - Ensure data.table without mutation\n\n")

  cat("Validation:\n")
  cat("  all_valid(x) - Check for NA/Inf values\n")
  cat("  count_valid(x) - Count non-NA values\n\n")

  cat("Additional:\n")
  cat("  safe_sum(), safe_mean(), safe_max(), safe_min()\n\n")

  cat("Strategy Helpers:\n")
  cat("  as_selection(condition, dates) - Convert conditions to selection format\n")
  cat("  run_strategy(...) - Complete strategy runner with natural syntax\n")
}

# Verify utils loaded
cat("[OK] Utility functions loaded from utils.R\n")
cat("[OK] Strategy helper functions included\n")



# Helper for NULL coalescing (add to utils.R if not exists)
`%||%` <- function(x, y) if (is.null(x)) y else x



#' Align Data to Strategy Timeframe
#'
#' @description
#' Aligns higher-frequency data to match strategy timeframe.
#'
#' @param high_freq_data Data frame to align
#' @param low_freq_dates Date vector from strategy
#' @param method Alignment method: "forward_fill", "nearest", or "interpolate"
#'
#' @return Aligned data frame
#' @export
#' @examples
#' data("sample_prices_weekly")
#' data("sample_prices_daily")
#' momentum <- calc_momentum(sample_prices_weekly, lookback = 12)
#' selected <- filter_top_n(momentum, 10)
#' # Create a stability signal from daily data
#' daily_vol <- calc_rolling_volatility(sample_prices_daily, lookback = 20)
#' stability_signal <- align_to_timeframe(daily_vol, sample_prices_weekly$Date)
#' weights <- weight_by_signal(selected, stability_signal)
align_to_timeframe <- function(high_freq_data, low_freq_dates, method = c("forward_fill", "nearest", "interpolate")) {
  # Align high-frequency data to low-frequency dates
  # OPTIMIZED VERSION using data.table rolling joins for speed
  #
  # This function solves the common problem of aligning indicators calculated
  # on one timeframe (e.g., daily) to a trading timeframe (e.g., weekly).
  # For example, align daily volatility calculations to weekly trading dates.
  #
  # Args:
  #   high_freq_data: data.frame/data.table with Date column and data columns
  #                   Example: daily prices or daily indicators
  #   low_freq_dates: Vector of dates to align to
  #                   Example: weekly trading dates
  #   method: Alignment method
  #     - "forward_fill": Use most recent available value (default)
  #                       Best for: prices, cumulative indicators
  #     - "nearest": Use value from nearest date (before or after)
  #                  Best for: volatility, correlations
  #     - "interpolate": Linear interpolation between values
  #                      Best for: smooth indicators like moving averages
  #
  # Returns:
  #   data.table with Date column matching low_freq_dates and all data
  #   columns from high_freq_data aligned according to method
  #
  # Examples:
  #   # Align daily volatility to weekly dates
  #   daily_vol <- calc_rolling_volatility(daily_prices, 20)
  #   weekly_vol <- align_to_timeframe(daily_vol, weekly_dates, "forward_fill")
  #
  #   # Align daily correlation for weekly strategy
  #   daily_corr <- calc_rolling_correlation(daily_prices, "SPY", 60)
  #   weekly_corr <- align_to_timeframe(daily_corr, weekly_dates, "forward_fill")

  method <- match.arg(method)

  # Ensure data.table for fast operations
  dt <- ensure_dt_copy(high_freq_data)
  setkey(dt, Date)  # Set key for ultra-fast date joins

  # Get column names (excluding Date)
  data_cols <- setdiff(names(dt), "Date")

  # Create target dates table
  target_dt <- data.table(Date = low_freq_dates)
  setkey(target_dt, Date)

  if (method == "forward_fill") {
    # Use data.table's rolling join with roll = TRUE
    # This finds the most recent date that is <= target date
    # MUCH faster than looping through dates
    result <- dt[target_dt, roll = TRUE]

  } else if (method == "nearest") {
    # Use data.table's rolling join with roll = "nearest"
    # This finds the closest date (either before or after)
    result <- dt[target_dt, roll = "nearest"]

  } else {  # interpolate
    # For interpolation, use R's approx() function
    # Already efficient for this use case
    result <- copy(target_dt)

    # Interpolate all columns
    for (col in data_cols) {
      interp_values <- approx(
        x = as.numeric(dt$Date),
        y = dt[[col]],
        xout = as.numeric(low_freq_dates),
        method = "linear",
        rule = 2  # Use nearest value for extrapolation beyond data range
      )$y

      result[, (col) := interp_values]
    }
  }

  # Ensure the result has exactly the requested dates
  # (important because rolling joins might have different dates)
  if (method != "interpolate") {
    result[, Date := low_freq_dates]
  }

  return(result)
}






#' Invert Signal Values for Preference Reversal
#'
#' @description
#' Transforms signal values using (1 - value) to reverse preference direction.
#' Useful when high values indicate something to avoid. For example, inverting
#' volatility makes low-vol stocks appear as high signals.
#'
#' @param signal_df Data frame with Date column and signal columns
#'
#' @return Data frame with inverted signal values
#' @export
#' @examples
#' data("sample_prices_weekly")
#' # Prefer low volatility stocks
#' volatility <- calc_rolling_volatility(sample_prices_weekly, 20)
#' stability_signal <- invert_signal(volatility)
#' # Select top 10 momentum stocks first
#' momentum <- calc_momentum(sample_prices_weekly, 12)
#' selected <- filter_top_n(momentum, 10)
#' # Weight by inverted volatility (low vol = high weight)
#' weights <- weight_by_signal(selected, stability_signal)
invert_signal <- function(signal_df) {
  # Input validation
  if (!is.data.frame(signal_df)) {
    stop("invert_signal: signal_df must be a data.frame or data.table")
  }

  if (!"Date" %in% names(signal_df)) {
    stop("invert_signal: signal_df must have a 'Date' column")
  }

  if (ncol(signal_df) < 2) {
    stop("invert_signal: signal_df must have at least one signal column besides Date")
  }

  # Create copy to avoid modifying input
  dt <- ensure_dt_copy(signal_df)

  # Get signal columns (all except Date)
  signal_cols <- setdiff(names(dt), "Date")

  # Apply inversion: new_value = 1 - old_value
  # This works for any numeric values, though normalized [0,1] is ideal
  dt[, (signal_cols) := lapply(.SD, function(x) 1 - x), .SDcols = signal_cols]

  return(dt)
}

