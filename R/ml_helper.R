#' @keywords internal
#' @importFrom stats predict
NULL

# ===========================================
# R/ml_helper.R  — minimal, general ML helpers
# Safe to source from R/ml.R during development
# ===========================================

# ---- tiny internals ---------------------------------------------------------
# Don't redefine if already present elsewhere in the package
#' Internal infix: A or B if A is NULL
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Internal utilities
#' @keywords internal
#' @noRd
.ml_symcols <- function(D) setdiff(names(D), "Date")
#' @keywords internal
#' @noRd
.ml_first  <- function(x, default = NULL) if (length(x)) x[[1]] else default


#' Panel-safe binary operation on aligned wide panels
#'
#' Applies an elementwise binary operator to two date-aligned wide panels
#' (first column `Date`, other columns are symbols), preserving the `Date`
#' column and a consistent symbol set. Supports intersection or union of
#' column sets; missing entries introduced by `how="union"` are filled.
#'
#' @param A,B Data frames with a `Date` column and one column per symbol.
#' @param op Binary function to apply elementwise (e.g., `*`, `/`, `+`).
#' @param how Character; `"intersect"` (default) or `"union"` for the set
#'   of symbol columns to operate on.
#' @param fill Numeric; value used to fill gaps when `how="union"`.
#'
#' @return A data.frame with `Date` and the operated symbol columns.
#' @examples
#' \dontrun{
#' out <- ml_panel_op(mom12_panel, vol_panel, op = `*`)
#' }
#' @export
ml_panel_op <- function(A, B, op = `*`, how = c("intersect","union"), fill = NA_real_) {
  # Always work with plain data.frames (avoid data.table's j-eval)
  A <- as.data.frame(A, check.names = FALSE)
  B <- as.data.frame(B, check.names = FALSE)

  # Harmonize Date types and check alignment
  if (!inherits(A$Date, "Date")) A$Date <- as.Date(A$Date)
  if (!inherits(B$Date, "Date")) B$Date <- as.Date(B$Date)
  stopifnot(identical(A$Date, B$Date))

  how   <- match.arg(how)
  symsA <- setdiff(names(A), "Date")
  symsB <- setdiff(names(B), "Date")
  syms  <- if (how == "intersect") intersect(symsA, symsB) else union(symsA, symsB)
  if (!length(syms)) stop("ml_panel_op(): no symbol columns after ", how, ".")

  # Build numeric matrices column-by-column (works even if some cols are missing)
  if (how == "intersect") {
    aM <- as.matrix(A[, syms, drop = FALSE])
    bM <- as.matrix(B[, syms, drop = FALSE])
  } else {  # union
    aM <- matrix(fill, nrow = nrow(A), ncol = length(syms), dimnames = list(NULL, syms))
    bM <- matrix(fill, nrow = nrow(B), ncol = length(syms), dimnames = list(NULL, syms))
    existA <- intersect(syms, symsA)
    existB <- intersect(syms, symsB)
    if (length(existA)) aM[, existA] <- as.matrix(A[, existA, drop = FALSE])
    if (length(existB)) bM[, existB] <- as.matrix(B[, existB, drop = FALSE])
  }

  out <- data.frame(Date = A$Date, check.names = FALSE)
  out[syms] <- op(aM, bM)
  out
}

#' Reduce multiple panels with a binary operator
#'
#' Folds a list of panels using `ml_panel_op()` across a set of named panels.
#'
#' @param features List of panels (each a wide data frame with `Date`).
#' @param panels Character vector of names in `features` to reduce (length ≥ 2).
#' @param op Binary function to apply elementwise.
#' @param how Column-set policy passed to [ml_panel_op()].
#' @param fill Fill value for `how="union"`.
#'
#' @return A data.frame panel (wide) with the reduced result.
#' @examples
#' \dontrun{
#' # product of three panels
#' prod_panel <- ml_panel_reduce(X, c("mom12","vol","rsi14"), op = `*`)
#' }
#' @export
ml_panel_reduce <- function(features, panels, op = `*`, how = "intersect", fill = NA_real_) {
  stopifnot(length(panels) >= 2L)
  res <- features[[panels[[1]]]]
  for (k in 2:length(panels)) {
    nxt <- features[[panels[[k]]]]
    stopifnot(!is.null(res), !is.null(nxt))
    res <- ml_panel_op(res, nxt, op = op, how = how, fill = fill)
  }
  res
}


#' Add interaction panels to a feature list
#'
#' Builds new panels from existing ones via elementwise operations.
#' Specification accepts either a shorthand `list(new = c("A","B"))` (defaults to product),
#' or a structured form `list(new = list(panels=c("A","B","C"), op=`/`, how="intersect", fill=NA))`.
#'
#' @param features List of existing panels (wide data frames with `Date`).
#' @param interactions Named list describing interactions to add.
#'
#' @return The input `features` list with additional interaction panels.
#' @examples
#' \dontrun{
#' X2 <- ml_add_interactions(X, list(mom_vol = c("mom12","vol")))
#' }
#' @export
ml_add_interactions <- function(features, interactions) {
  for (nm in names(interactions)) {
    spec <- interactions[[nm]]
    if (is.character(spec) && length(spec) == 2L) {
      features[[nm]] <- ml_panel_reduce(features, spec, op = `*`)
    } else if (is.list(spec)) {
      panels <- spec$panels %||% .ml_first(spec)
      op     <- spec$op     %||% `*`
      how    <- spec$how    %||% "intersect"
      fill   <- if (!is.null(spec$fill)) spec$fill else NA_real_
      stopifnot(is.character(panels), length(panels) >= 2L)
      features[[nm]] <- ml_panel_reduce(features, panels, op = op, how = how, fill = fill)
    } else {
      stop(sprintf("Unsupported interaction spec for '%s'", nm))
    }
  }
  features
}




#' Prepare tabular features (weekly + aligned daily volatility)
#'
#' Constructs a minimal, leakage-safe set of tabular features commonly used
#' in cross-sectional ML: weekly momentum (12/26/52), RSI(14), distance from
#' 20-week MA, and daily rolling volatility aligned to weekly dates
#' (tokens `vol{N}d_walign`, e.g., `"vol20d_walign"`).
#'
#' All outputs are **lagged by one period** to avoid look-ahead in backtests.
#'
#' @param prices_weekly Wide panel with `Date` and symbol columns (weekly).
#' @param prices_daily Optional wide panel (daily) if `vol*d_walign` are included.
#' @param include Character vector of feature tokens to include.
#' @param interactions Optional named list passed to [ml_add_interactions()].
#'
#' @return A named list of panels (each a wide data.frame with `Date`).
#' @examples
#' \dontrun{
#' X <- ml_prepare_features(sample_prices_weekly, sample_prices_daily,
#'                          include = c("mom12","vol20d_walign","rsi14"))
#' }
#' @export
ml_prepare_features <- function(prices_weekly,
                                prices_daily = NULL,
                                include = c("mom12","mom26","mom52",
                                            "vol20d_walign","dist20","rsi14"),
                                interactions = NULL) {
  out <- list()

  # momentum (weekly panels)
  if ("mom12" %in% include) out$mom12 <- panel_lag(calc_momentum(prices_weekly, 12L), 1L)
  if ("mom26" %in% include) out$mom26 <- panel_lag(calc_momentum(prices_weekly, 26L), 1L)
  if ("mom52" %in% include) out$mom52 <- panel_lag(calc_momentum(prices_weekly, 52L), 1L)

  # daily volatility aligned to weekly dates
  vol_keys <- grep("^vol(\\d+)d_walign$", include, value = TRUE)
  if (length(vol_keys)) {
    stopifnot(!is.null(prices_daily))
    weekly_dates <- if (inherits(prices_weekly$Date, "Date")) prices_weekly$Date else as.Date(prices_weekly$Date)
    for (vk in vol_keys) {
      N <- as.integer(sub("^vol(\\d+)d_walign$", "\\1", vk))
      vol_d <- calc_rolling_volatility(prices_daily, N)
      if (!inherits(vol_d$Date, "Date")) vol_d$Date <- as.Date(vol_d$Date)
      aligned <- align_to_timeframe(vol_d, weekly_dates, "forward_fill")
      nm <- if (N == 20L) "vol" else paste0("vol", N, "d")
      out[[nm]] <- panel_lag(aligned, 1L)
    }
  }

  # distance to MA(20); RSI(14)
  if ("dist20" %in% include) {
    ma20 <- calc_moving_average(prices_weekly, 20L)
    out$dist20 <- panel_lag(calc_distance(prices_weekly, ma20), 1L)
  }
  if ("rsi14" %in% include) out$rsi14 <- panel_lag(calc_rsi(prices_weekly, 14L), 1L)

  # interactions
  if (!is.null(interactions) && length(interactions)) {
    out <- ml_add_interactions(out, interactions)
  }
  out
}



#' Model factory for tabular cross-sectional learners
#'
#' Returns a pair of closures `fit(X,y)` / `predict(model, X)` implementing
#' a chosen learner. Implementations are NA-aware and conservative:
#' `glmnet` ridge drops rows with any non-finite input; `ranger` and `xgboost`
#' keep NA in `X` as missing; the linear baseline uses `lm.fit`.
#'
#' @param type One of `"ridge"`, `"rf"`, `"xgboost"`, `"linear"`.
#' @param params List of model parameters (passed to backend; used by xgboost).
#' @param nrounds Integer boosting rounds (xgboost).
#' @param ... Additional arguments forwarded to the backend.
#'
#' @details
#' Optional dependencies: `glmnet` (ridge), `ranger` (rf), `xgboost` (xgboost).
#' If a backend is not available, use `"linear"` or install the package.
#'
#' @return A list with functions `fit` and `predict`.
#' @examples
#' \dontrun{
#' ridge <- ml_make_model("ridge")
#' m <- ridge$fit(X_is, y_is)
#' s <- ridge$predict(m, X_oos)
#' }
#' @export
ml_make_model <- function(type = c("ridge","rf","xgboost","linear"),
                          params = list(), nrounds = 200L, ...) {
  type <- match.arg(type)

  if (type == "ridge") {
    stopifnot(requireNamespace("glmnet", quietly = TRUE))
    fit <- function(X, y) {
      X <- as.matrix(X); y <- as.numeric(y)
      keep <- is.finite(y) & rowSums(!is.finite(X)) == 0
      glmnet::cv.glmnet(X[keep, , drop = FALSE], y[keep], alpha = 0, nfolds = 3, ...)
    }
    pred <- function(m, X) {
      X <- as.matrix(X); X[!is.finite(X)] <- 0
      as.numeric(predict(m, newx = X, s = "lambda.min"))
    }
    return(list(fit = fit, predict = pred))
  }

  if (type == "rf") {
    stopifnot(requireNamespace("ranger", quietly = TRUE))
    fit <- function(X, y) {
      X <- as.data.frame(X); y <- as.numeric(y)
      keep <- is.finite(y) & rowSums(!is.finite(as.matrix(X))) == 0
      ranger::ranger(y = y[keep], x = X[keep, , drop = FALSE], ...)
    }
    pred <- function(m, X) {
      X <- as.data.frame(X); X[!is.finite(as.matrix(X))] <- 0
      as.numeric(predict(m, data = X)$predictions)
    }
    return(list(fit = fit, predict = pred))
  }

  if (type == "xgboost") {
    stopifnot(requireNamespace("xgboost", quietly = TRUE))
    fit <- function(X, y) {
      X <- as.matrix(X); y <- as.numeric(y)
      keep <- is.finite(y)  # XGBoost can handle NA in X; keep NA in X as missing
      d <- xgboost::xgb.DMatrix(data = X[keep, , drop = FALSE], label = y[keep], missing = NA)
      xgboost::xgb.train(params = params, data = d, nrounds = as.integer(nrounds), verbose = 0)
    }
    pred <- function(m, X) {
      X <- as.matrix(X); X[!is.finite(X)] <- NA
      as.numeric(predict(m, newdata = X, missing = NA))
    }
    return(list(fit = fit, predict = pred))
  }

  # linear baseline
  fit <- function(X, y) {
    X <- as.matrix(X); y <- as.numeric(y)
    keep <- is.finite(y) & rowSums(!is.finite(X)) == 0
    Xc <- cbind(1, X[keep, , drop = FALSE])
    list(coef = stats::lm.fit(Xc, y[keep])$coefficients)
  }
  pred <- function(m, X) {
    X <- as.matrix(X); X[!is.finite(X)] <- 0
    as.numeric(cbind(1, X) %*% m$coef)
  }
  list(fit = fit, predict = pred)
}

#' NA-tolerant ensemble blender (row-wise)
#'
#' Creates an equal- or user-weighted blend of multiple model objects produced
#' by [ml_make_model()]. The returned `fit` trains each component; `predict`
#' combines component predictions with an NA-safe weighted average.
#'
#' @param ... Two or more model objects each with `$fit`/`$predict`.
#' @param weights Optional numeric vector of blend weights (recycled).
#'
#' @return A list with `$fit` and `$predict` closures for the ensemble.
#' @examples
#' \dontrun{
#' ens <- ml_make_ensemble(ml_make_model("ridge"),
#'                         ml_make_model("rf"),
#'                         ml_make_model("xgboost"))
#' }
#' @export
ml_make_ensemble <- function(..., weights = NULL) {
  parts <- list(...)
  stopifnot(length(parts) >= 2L)
  if (is.null(weights)) weights <- rep(1/length(parts), length(parts))

  fit <- function(X, y) lapply(parts, function(w) w$fit(X, y))

  pred <- function(fitted, X) {
    pred_list <- lapply(seq_along(parts), function(i) {
      as.numeric(parts[[i]]$predict(fitted[[i]], X))  # force numeric vector
    })
    n <- length(pred_list[[1]])
    # sanity: all same length
    if (any(vapply(pred_list, length, 1L) != n)) {
      stop("Ensemble models returned different-length predictions.")
    }
    # build P (n x m)
    P <- matrix(NA_real_, nrow = n, ncol = length(pred_list))
    for (i in seq_along(pred_list)) P[, i] <- pred_list[[i]]

    # weighted average with NA masking
    W <- matrix(weights, nrow = n, ncol = length(weights), byrow = TRUE)
    mask <- is.finite(P)
    W_eff <- W * mask
    num <- rowSums(P * W_eff, na.rm = TRUE)
    den <- rowSums(W_eff, na.rm = TRUE)
    out <- num / den
    out[den == 0] <- NA_real_
    out
  }

  list(fit = fit, predict = pred)
}




#' Deterministic sequence model factory (GRU/LSTM/CNN1D with linear fallback)
#'
#' Returns `fit/predict` closures for sequence models that consume flattened
#' tabular inputs (n × (steps × p)) and internally reshape to (n, steps, p).
#' If Keras/TensorFlow is unavailable, falls back to a linear baseline so
#' examples remain runnable on CPU-only machines.
#'
#' Determinism knobs: fixed seeds, `TF_DETERMINISTIC_OPS=1`, no shuffle,
#' `workers=1`, and a fixed `pred_batch_size` to minimise retracing.
#'
#' @param type One of `"linear"`, `"gru"`, `"lstm"`, `"cnn1d"`.
#' @param steps Integer sequence length (e.g., 26 for 6 months of weeks).
#' @param units Hidden units for GRU/LSTM or filters for CNN1D.
#' @param dense Optional integer vector of additional dense layers.
#' @param dropout Dropout rate for recurrent/CNN core.
#' @param epochs,batch_size Training settings.
#' @param lr Learning rate.
#' @param patience Early-stopping patience.
#' @param seed Integer seed.
#' @param deterministic Logical; set determinism knobs when TRUE.
#' @param pred_batch_size Fixed batch size used at prediction time.
#'
#' @details
#' Optional dependencies: `keras` and `tensorflow`. When not available,
#' the factory returns the linear fallback.
#'
#' @return A list with `$fit` and `$predict` closures.
#' @examples
#' \dontrun{
#' seq_gru <- ml_make_seq_model("gru", steps = 26, units = 16, epochs = 12)
#' }
#' @export
ml_make_seq_model <- function(type = c("linear","gru","lstm","cnn1d"),
                              steps = 26L, units = 16L, dense = NULL,
                              dropout = 0.1, epochs = 12L, batch_size = 128L,
                              lr = 0.01, patience = 2L, seed = 123L,
                              deterministic = TRUE, pred_batch_size = 256L) {
  type <- match.arg(type)
  use_keras <- (type != "linear") &&
    requireNamespace("keras", quietly = TRUE) &&
    isTRUE(try(keras::is_keras_available(), silent = TRUE))

  # ---- Linear fallback (no Keras required) ----
  if (!use_keras) {
    fit  <- function(X, y) {
      Xc <- cbind(1, as.matrix(X))
      list(type = "lin", coef = stats::lm.fit(Xc, as.numeric(y))$coefficients)
    }
    pred <- function(m, Xnew) {
      Xc <- cbind(1, as.matrix(Xnew))
      as.numeric(Xc %*% m$coef)
    }
    return(list(fit = fit, predict = pred))
  }

  # ---- Keras models (GRU/LSTM/CNN1D) ----
  fit <- function(X, y) {
    set.seed(seed)

    # Determinism knobs
    if (deterministic) {
      Sys.setenv(PYTHONHASHSEED = as.character(seed))
      Sys.setenv(TF_DETERMINISTIC_OPS = "1")
      if (requireNamespace("tensorflow", quietly = TRUE)) {
        tensorflow::tf$random$set_seed(seed)
        try(tensorflow::tf$config$experimental$enable_op_determinism(TRUE), silent = TRUE)
      }
      keras::backend()$clear_session()
    }

    X <- as.matrix(X); y <- as.numeric(y)
    X[!is.finite(X)] <- 0; y[!is.finite(y)] <- 0
    p <- max(1L, as.integer(ncol(X) / steps))
    X3 <- array(X, dim = c(nrow(X), steps, p))

    inp <- keras::layer_input(shape = c(steps, p))
    core <- switch(type,
                   gru   = keras::layer_gru(units = units, dropout = dropout)(inp),
                   lstm  = keras::layer_lstm(units = units, dropout = dropout)(inp),
                   cnn1d = {
                     x <- keras::layer_conv_1d(filters = units, kernel_size = 3,
                                               padding = "causal", activation = "relu")(inp)
                     keras::layer_global_average_pooling_1d()(x)
                   }
    )

    head <- core
    if (!is.null(dense)) for (h in dense)
      head <- keras::layer_dense(units = h, activation = "relu")(head)
    out  <- keras::layer_dense(units = 1)(head)

    mdl <- keras::keras_model(inp, out)
    keras::compile(mdl, optimizer = keras::optimizer_adam(learning_rate = lr), loss = "mse")

    cb <- keras::callback_early_stopping(patience = patience, restore_best_weights = TRUE)

    keras::fit(
      mdl, X3, y,
      batch_size = batch_size, epochs = epochs,
      validation_split = 0.1, verbose = 0,
      callbacks = list(cb),
      shuffle = if (!deterministic) TRUE else FALSE,
      use_multiprocessing = if (!deterministic) TRUE else FALSE,
      workers = if (deterministic) 1L else 2L
    )

    list(type = type, model = mdl, p = p, steps = steps)
  }


  pred <- function(m, Xnew) {
    X <- as.matrix(Xnew); X[!is.finite(X)] <- 0
    X3 <- array(X, dim = c(nrow(X), m$steps, m$p))
    # Fixed inference batch size to reduce TF retracing warnings
    as.numeric(predict(m$model, X3, batch_size = as.integer(pred_batch_size), verbose = 0))
  }

  list(fit = fit, predict = pred)
}




#' Keep only rows with at least one finite score (rebalance dates)
#'
#' Convenience filter used by IC diagnostics to align to formation dates.
#'
#' @param scores_dt Wide scores table (`Date` + symbols) or long format coerced.
#' @return A `data.table` subset containing only rows with any finite score.
#' @keywords internal
#' @noRd
.ml_score_dates <- function(scores_dt) {
  DT <- data.table::as.data.table(scores_dt)
  syms <- setdiff(names(DT), "Date")
  keep <- rowSums(is.finite(as.matrix(DT[, ..syms]))) > 0
  DT[keep]
}


#' Rank-IC series computed on score (rebalance) dates
#'
#' Wrapper around `ic_series()` that first filters scores to formation dates
#' using an internal filter that keeps rows where at least one score is finite.
#'
#' @param scores_dt Scores table (wide).
#' @param labels_dt Labels table (wide).
#' @param method Correlation method; `"spearman"` (default) or `"pearson"`.
#'
#' @return A data frame/data.table with `Date` and IC values.
#' @examples
#' \dontrun{
#' ic <- ml_ic_series_on_scores(res_xgb$scores, Y, method = "spearman")
#' }
#' @export
ml_ic_series_on_scores <- function(scores_dt, labels_dt, method = "spearman") {
  ic_series(.ml_score_dates(scores_dt), labels_dt, method = method)
}



#' Rolling rank-IC plot (rebalance dates; leakage-safe)
#'
#' Computes the IC time series via [ml_ic_series_on_scores()] and plots the
#' rolling mean IC over a specified window. Returns the rolling statistics
#' invisibly for further inspection.
#'
#' @param scores_dt Scores (wide).
#' @param labels_dt Labels (wide).
#' @param window Integer window length (default 26).
#' @param method Correlation method; `"spearman"` (default) or `"pearson"`.
#'
#' @return (Invisibly) a data frame with `Date`, `roll_mean`, `roll_sd`, `roll_ICIR`.
#' @examples
#' \dontrun{
#' ris <- ml_plot_ic_roll(res_xgb$scores, Y, window = 8L)
#' }
#' @export
ml_plot_ic_roll <- function(scores_dt, labels_dt, window = 26L, method = "spearman") {
  ics <- ml_ic_series_on_scores(scores_dt, labels_dt, method = method)
  ris <- roll_ic_stats(ics, window = window)  # returns roll_mean, roll_sd, roll_ICIR
  plot(ris$Date, ris$roll_mean, type = "l",
       main = paste0("Rolling IC (", window, "w)"),
       xlab = "", ylab = "IC mean")
  invisible(ris)
}

