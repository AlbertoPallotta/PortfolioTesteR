# R/optimization.R - Parameter Grid Optimization with Phase 1 & 2 optimizations


#' Calculate Sharpe Ratio with Frequency Detection
#'
#' @param bt Backtest result object with $returns and (optionally) $dates
#' @return Annualized Sharpe ratio
#' @export
metric_sharpe <- function(bt) {
  returns <- bt$returns
  if (length(returns) == 0 || all(is.na(returns))) return(NA_real_)

  freq_factor <- if (!is.null(bt$dates)) {
    .pt_detect_frequency(as.Date(bt$dates))
  } else {
    52
  }

  mu <- mean(returns, na.rm = TRUE)
  sdv <- stats::sd(returns, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) return(NA_real_)

  (mu / sdv) * sqrt(freq_factor)
}
#' Run Parameter Grid Optimization (safe + ergonomic)
#'
#' @param prices Data frame with Date + symbol columns
#' @param grid   Data frame (each row = a combo) OR a **named list** of vectors
#' @param builder Function(prices, params, ...) -> weights (Date + symbols)
#' @param metric  Scoring function(backtest) -> numeric. Defaults to metric_sharpe.
#' @param name_prefix String prefix for backtest names
#' @param verbose Logical
#' @param light_mode Logical: speed-ups in backtest
#' @param precompute_returns Logical: precompute log-returns once (light_mode only)
#' @param builder_args List of extra args forwarded to builder (e.g., caches)
#' @param n_cores Integer (kept for API compatibility; ignored here)
#' @return param_grid_result
#' @export
run_param_grid <- function(prices,
                           grid,
                           builder,
                           metric = NULL,
                           name_prefix = "Strategy",
                           verbose = FALSE,
                           light_mode = TRUE,
                           precompute_returns = TRUE,
                           builder_args = list(),
                           n_cores = 1) {

  # Accept grid as named list (normalize to data.frame)
  if (is.list(grid) && !is.data.frame(grid)) {
    if (is.null(names(grid)) || any(names(grid) == "")) {
      stop("'grid' list must be named, e.g., list(lookback = 1:4, n_top = 1:3)", call. = FALSE)
    }
    grid <- do.call(expand.grid, c(grid, stringsAsFactors = FALSE))
  }

  # Defaults & validation
  if (is.null(metric)) metric <- metric_sharpe
  if (!is.data.frame(prices)) stop("'prices' must be a data frame", call. = FALSE)
  if (!is.data.frame(grid))   stop("'grid' must be a data frame or a named list", call. = FALSE)
  if (!is.function(builder))  stop("'builder' must be a function", call. = FALSE)
  if (!is.function(metric))   stop("'metric' must be a function", call. = FALSE)
  if (nrow(grid) == 0)        stop("'grid' must have at least one row", call. = FALSE)

  # Guardrail: forbid cadence/timeframe knobs in the grid
  .pt_check_grid_params(grid, "parameter grid optimization")

  # Detect whether run_backtest supports 'track_transactions'
  rb_formals <- tryCatch(formals(run_backtest), error = function(e) NULL)
  rb_has_track <- is.list(rb_formals) && ("track_transactions" %in% names(rb_formals))

  # Phase 1: Precompute returns matrix once (optional)
  if (precompute_returns && light_mode) {
    P <- as.data.frame(prices)
    price_cols <- setdiff(names(P), "Date")
    if (length(price_cols) > 0L && nrow(P) > 1L) {
      price_matrix <- as.matrix(P[, price_cols, drop = FALSE])
      returns_matrix <- diff(log(price_matrix))
      builder_args$.precomputed_returns <- returns_matrix
    }
  }

  results <- vector("list", nrow(grid))
  scores  <- rep(NA_real_, nrow(grid))

  for (i in seq_len(nrow(grid))) {
    params <- as.list(grid[i, , drop = FALSE])

    if (verbose) {
      cat(sprintf("Testing %d/%d: %s\n",
                  i, nrow(grid),
                  paste(sprintf("%s=%s", names(params), params), collapse = ", ")))
    }

    tryCatch({
      # 1) Build weights
      weights <- if (length(builder_args) > 0) {
        do.call(builder, c(list(prices, params), builder_args))
      } else {
        builder(prices, params)
      }

      # 2) OPTIONAL: validate/align weights (but never fail-hard)
      if (exists(".pt_validate_weights", mode = "function", inherits = TRUE)) {
        weights2 <- tryCatch(
          .pt_validate_weights(prices, weights, context = sprintf("grid[%d]", i), allow_sparse = TRUE),
          error = function(e) {
            if (verbose) cat("  [validator skipped:", e$message, "]\n")
            NULL
          }
        )
        if (!is.null(weights2)) weights <- weights2
      }

      # 3) Basic sanity on weights
      if (is.null(weights) || nrow(weights) == 0) {
        if (verbose) cat("  [weights empty -> NA score]\n")
        scores[i]  <- NA_real_
        results[[i]] <- list(params = params, score = NA_real_, backtest = NULL)
      } else {
        # 4) Build args safely; only pass 'track_transactions' if supported
        rb_args <- list(
          prices = prices,
          weights = weights,
          initial_capital = 100000,
          name = paste0(name_prefix, "_", i),
          verbose = FALSE
        )
        if (light_mode && rb_has_track) {
          rb_args$track_transactions <- FALSE
        }

        bt <- do.call(run_backtest, rb_args)

        # 5) Score
        score <- metric(bt)
        scores[i] <- score
        results[[i]] <- list(params = params, score = score, backtest = bt)

        if (verbose && is.na(score)) cat("  [metric returned NA]\n")
      }
    }, error = function(e) {
      if (verbose) cat(sprintf("  Error: %s\n", e$message))
      scores[i] <- NA_real_
      results[[i]] <- list(params = params, score = NA_real_, error = e$message)
    })
  }

  # NA-safe best
  valid_scores <- scores[!is.na(scores)]
  if (length(valid_scores) == 0) {
    best_idx <- NULL; best_params <- NULL; best_score <- NA_real_; best_backtest <- NULL
  } else {
    best_idx     <- which.max(replace(scores, is.na(scores), -Inf))[1]
    best_params  <- results[[best_idx]]$params
    best_score   <- scores[best_idx]
    best_backtest<- results[[best_idx]]$backtest
  }

  structure(
    list(
      best_params = best_params,
      best_idx = best_idx,
      optimization_score = best_score,
      best_backtest = best_backtest,
      all_results = results,
      all_scores = scores,
      grid = grid,
      n_valid = sum(!is.na(scores)),
      n_tested = nrow(grid)
    ),
    class = "param_grid_result"
  )
}

#' Print a param_grid_result
#'
#' @param x A `param_grid_result` object returned by [run_param_grid()].
#' @param ... Additional arguments passed to methods (ignored).
#'
#' @return Invisibly returns `x`.
#' @export
#' @method print param_grid_result
#' @aliases print.param_grid_result
print.param_grid_result <- function(x, ...) {
  cat("Parameter Grid Optimization Result\n")
  cat("==================================\n")
  cat("Tested:", x$n_tested, "combinations\n")
  cat("Valid:", x$n_valid,  "results\n")
  if (!is.null(x$best_params)) {
    cat("\nBest parameters:\n")
    for (nm in names(x$best_params)) {
      cat(sprintf("  %s: %s\n", nm, x$best_params[[nm]]))
    }
    cat(sprintf("\nBest score: %.4f\n", x$optimization_score))
  } else {
    cat("\nNo valid results found\n")
  }
  invisible(x)
}

