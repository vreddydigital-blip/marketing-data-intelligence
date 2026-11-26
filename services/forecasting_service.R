# Forecasting service. Time series are aggregated and ordered strictly by
# time; evaluation uses a chronological holdout (the final H periods). Models:
# naive, seasonal naive, exponential smoothing, auto ARIMA (forecast pkg).

prepare_time_series <- function(df, date_col, metric_col, freq = c("day", "week", "month")) {
  freq <- match.arg(freq)
  if (!date_col %in% names(df) || !metric_col %in% names(df)) {
    stop("Selected date or metric column is not in the dataset.", call. = FALSE)
  }
  d <- as.Date(df[[date_col]])
  if (all(is.na(d))) stop("The selected date column could not be parsed as dates.", call. = FALSE)
  v <- suppressWarnings(as.numeric(df[[metric_col]]))
  period <- switch(freq,
    day = d,
    week = as.Date(cut(d, "week")),
    month = as.Date(format(d, "%Y-%m-01"))
  )
  agg <- stats::aggregate(v ~ period, data = data.frame(period = period, v = v),
                          FUN = function(z) sum(z, na.rm = TRUE))
  agg <- agg[order(agg$period), ]
  # fill missing periods with interpolated values, flag them
  full_periods <- switch(freq,
    day = seq(min(agg$period), max(agg$period), by = "day"),
    week = seq(min(agg$period), max(agg$period), by = "week"),
    month = seq(min(agg$period), max(agg$period), by = "month")
  )
  merged <- merge(data.frame(period = full_periods), agg, by = "period", all.x = TRUE)
  n_missing <- sum(is.na(merged$v))
  if (n_missing > 0 && sum(!is.na(merged$v)) > 2) {
    merged$v[is.na(merged$v)] <- stats::approx(which(!is.na(merged$v)), merged$v[!is.na(merged$v)],
                                               xout = which(is.na(merged$v)))$y
  }
  list(values = merged$v, periods = merged$period, n_missing_periods = n_missing,
       freq = freq, metric = metric_col)
}

ts_from_prepared <- function(prepared) {
  if (prepared$freq == "month") stats::ts(prepared$values, frequency = 12, start = c(lubridate::year(prepared$periods[1]), lubridate::month(prepared$periods[1])))
  else if (prepared$freq == "week") stats::ts(prepared$values, frequency = 52, start = as.numeric(prepared$periods[1]))
  else stats::ts(prepared$values, frequency = 7, start = as.numeric(prepared$periods[1]))
}

fit_forecast_models <- function(prepared, horizon = 6, seasonal_length = NULL) {
  values <- prepared$values
  n <- length(values)
  horizon <- min(horizon, max(2L, floor(n / 4)))
  ts_all <- ts_from_prepared(prepared)
  hist_train <- stats::window(ts_all, end = time(ts_all)[n - horizon])
  hist_test <- stats::window(ts_all, start = time(ts_all)[n - horizon + 1])
  if (length(hist_train) < 10) {
    stop(sprintf("Insufficient history: %d aggregated periods available; at least 10 + forecast horizon needed.",
                 n), call. = FALSE)
  }
  m <- list()
  m$Naive <- forecast::naive(hist_train, h = horizon)
  if (!is.null(seasonal_length) && seasonal_length > 1 && length(hist_train) > 2 * seasonal_length) {
    m$`Seasonal naive` <- forecast::snaive(hist_train, h = horizon, frequency = seasonal_length)
  }
  m$`Exponential smoothing` <- suppressWarnings(tryCatch(forecast::ets(hist_train), error = function(e) NULL))
  m$ARIMA <- suppressWarnings(tryCatch(
    forecast::auto.arima(hist_train, seasonal = !is.null(seasonal_length) && seasonal_length > 1),
    error = function(e) NULL))
  m <- Filter(function(z) !is.null(z), m)

  acc <- do.call(rbind, lapply(names(m), function(nm) {
    fc <- tryCatch(m[[nm]], error = function(e) NULL)
    if (is.null(fc) || length(fc$mean) < horizon) return(NULL)
    acc_tab <- tryCatch(forecast::accuracy(fc, hist_test), error = function(e) NULL)
    if (is.null(acc_tab)) return(NULL)
    row <- acc_tab["Test set", , drop = FALSE]
    data.frame(model = nm, RMSE = row[, "RMSE"], MAE = row[, "MAE"],
               MAPE = row[, "MAPE"], stringsAsFactors = FALSE)
  }))
  list(models = m, accuracy = acc, horizon = horizon,
       actual_test = hist_test, train = hist_train, ts_full = ts_all,
       seasonal_length = seasonal_length, n = n)
}

final_forecast <- function(fit_result, model_name, horizon) {
  mdl <- fit_result$models[[model_name]] %||% fit_result$models[[1]]
  suppressWarnings(forecast::forecast(mdl, h = horizon))
}

# Sudden regime changes in a series for the insight engine.
spend_jumps <- function(periods, values, threshold_pct = 25) {
  if (length(values) < 8) return(NULL)
  ma <- stats::filter(values, rep(1/3, 3), sides = 1)
  pct_change <- 100 * (values - dplyr::lag(values)) / dplyr::lag(values)
  idx <- which(abs(pct_change) >= threshold_pct)
  idx <- idx[idx > 1]
  if (!length(idx)) return(NULL)
  data.frame(period = periods[idx], change_pct = round(pct_change[idx], 1), stringsAsFactors = FALSE)
}
