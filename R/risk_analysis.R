# risk_analysis.R
# Risk analysis and regime detection functions

#library(data.table)
#' Convert Continuous Indicator to Discrete Regimes
#'
#' @description
#' Transforms continuous indicators (like VIX, volatility, correlation) into
#' discrete regime categories for adaptive strategy behavior. Supports both
#' percentile-based and absolute value breakpoints.
#'
#' @param indicator Numeric vector or single-column data frame with indicator values
#' @param breakpoints Numeric vector of breakpoints. If all values are between
#'                    0 and 100, treated as percentiles; otherwise absolute values
#' @param labels Optional character vector of regime names. If NULL, generates
#'               automatic labels based on number of buckets
#' @param use_percentiles Use percentiles instead of fixed breakpoints
#' @return Factor vector with regime assignments for each observation
#' @export
#' @examples
#' # Create VIX-based market regimes
#' vix_regimes <- create_regime_buckets(
#'   vix_data$VIX,
#'   breakpoints = c(20, 30),
#'   labels = c("calm", "normal", "stressed")
#' )
#'
#' # Use percentiles for automatic thresholds
#' vol_regimes <- create_regime_buckets(
#'   volatility,
#'   breakpoints = c(25, 75),  # 25th and 75th percentiles
#'   labels = c("low", "medium", "high")
#' )
#'
#' # Four regimes with auto-generated labels
#' correlation_regimes <- create_regime_buckets(
#'   correlations,
#'   breakpoints = c(-0.2, 0.3, 0.7)
#' )  # Creates: low, normal, elevated, extreme
create_regime_buckets <- function(indicator, breakpoints, labels = NULL, use_percentiles = FALSE) {
  # Convert continuous indicator into discrete regime buckets
  #
  # Args:
  #   indicator: Numeric vector or single-column data.frame with values to bucket
  #   breakpoints: Numeric vector of breakpoints
  #   labels: Character vector of labels for each bucket (optional)
  #   use_percentiles: If TRUE, breakpoints are percentiles (0-100)
  #                   If FALSE, breakpoints are absolute values (default)

  # Extract values if input is a data structure
  if (is.data.frame(indicator) || is.data.table(indicator)) {
    if (ncol(indicator) > 1) {
      stop("create_regime_buckets: indicator must be a single column")
    }
    values <- indicator[[1]]
  } else {
    values <- indicator
  }

  # Remove NAs for calculations
  valid_values <- values[!is.na(values)]

  if (length(valid_values) == 0) {
    stop("create_regime_buckets: no valid values in indicator")
  }

  # Process breakpoints based on type
  if (use_percentiles) {
    # Treat as percentiles
    if (any(breakpoints <= 0 | breakpoints >= 100)) {
      stop("Percentile breakpoints must be between 0 and 100")
    }
    actual_breaks <- quantile(valid_values, probs = breakpoints / 100)
    actual_breaks <- c(min(valid_values) - 1e-10, actual_breaks, max(valid_values) + 1e-10)
  } else {
    # Treat as absolute values
    actual_breaks <- c(-Inf, breakpoints, Inf)
  }

  # Create labels if not provided
  n_buckets <- length(actual_breaks) - 1
  if (is.null(labels)) {
    if (n_buckets == 2) {
      labels <- c("low", "high")
    } else if (n_buckets == 3) {
      labels <- c("low", "normal", "high")
    } else if (n_buckets == 4) {
      labels <- c("low", "normal", "elevated", "extreme")
    } else {
      labels <- paste0("bucket_", 1:n_buckets)
    }
  }

  # Check label count
  if (length(labels) != n_buckets) {
    stop(sprintf("create_regime_buckets: need %d labels but got %d",
                 n_buckets, length(labels)))
  }

  # Cut the data
  regimes <- cut(values, breaks = actual_breaks, labels = labels, include.lowest = TRUE)

  return(regimes)
}
