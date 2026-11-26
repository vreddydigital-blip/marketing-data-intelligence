test_that("time series aggregation fills missing periods and stays chronological", {
  d <- data.frame(
    date = rep(seq(as.Date("2024-01-01"), by = "day", length.out = 30), each = 3),
    v = rep(1:30, each = 3) * 1.0
  )
  d <- d[c(1:80, 90:90), ]  # drop most of one day block to create a gap
  p <- prepare_time_series(d, "date", "v", freq = "day")
  expect_equal(length(p$values), 30)
  expect_false(any(is.na(p$values)))
  expect_true(all(diff(p$periods) >= 0))
})

test_that("models are evaluated on a chronological holdout, never shuffled", {
  d <- data.frame(
    date = rep(seq(as.Date("2024-01-01"), by = "day", length.out = 60), each = 2),
    v = rep(1:60, each = 2) * 2
  )
  p <- prepare_time_series(d, "date", "v", freq = "day")
  fr <- fit_forecast_models(p, horizon = 7, seasonal_length = 7)
  expect_true(!is.null(fr$accuracy) && nrow(fr$accuracy) >= 1)
  # naive holds the last training value; the test window is strictly after it
  t_end <- max(fr$train)
  expect_true(all(time(fr$actual_test) > t_end))
})

test_that("insufficient history is refused with a clear error", {
  d <- data.frame(date = seq(as.Date("2024-01-01"), by = "day", length.out = 8), v = 1:8)
  expect_error(fit_forecast_models(prepare_time_series(d, "date", "v", "day"), horizon = 4),
               "Insufficient history|horizon")
})

test_that("final forecast produces horizon rows with intervals ordered correctly", {
  d <- data.frame(date = rep(seq(as.Date("2024-01-01"), by = "day", length.out = 49), each = 2),
                  v = rep(cumsum(rnorm(49, 5, 2)), each = 2))
  p <- prepare_time_series(d, "date", "v", "day")
  fr <- fit_forecast_models(p, horizon = 6, seasonal_length = 7)
  fc <- final_forecast(fr, "ARIMA", 6)
  expect_equal(length(fc$mean), 6)
  expect_true(all(fc$lower[, 2] <= fc$mean) && all(fc$upper[, 2] >= fc$mean))
})
