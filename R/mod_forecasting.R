# Forecasting module: chronological holdout evaluation, model comparison,
# and forward forecast with prediction intervals from the best model.

mod_forecasting_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- bslib::card(
    bslib::card_header("Forecast setup"),
    bslib::card_body(
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(ns("date_col"), "Date column", choices = NULL)),
        shiny::column(3, shiny::selectInput(ns("metric"), "Metric to forecast", choices = NULL)),
        shiny::column(2, shinyWidgets::radioGroupButtons(ns("freq"), "Aggregate by", c("day", "week", "month"), size = "sm")),
        shiny::column(2, shiny::numericInput(ns("horizon"), "Forecast horizon", value = 6, min = 1, max = 60, step = 1)),
        shiny::column(2, shiny::actionButton(ns("run"), "Run forecast", class = "btn-primary margin-top"))
      ),
      shiny::uiOutput(ns("notes"))
    ),
    bslib::layout_column_wrap(width = 1, fill = FALSE,
      bslib::card(bslib::card_header("History and forecast"), bslib::card_body(plotly::plotlyOutput(ns("plot"), height = "440px"))),
      bslib::card(bslib::card_header("Backtest accuracy (chronological holdout)"), bslib::card_body(DT::DTOutput(ns("accuracy"))))
    )
  )
  ui
}

mod_forecasting_server <- function(id, dataset, forecast_out = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- shiny::reactive({ dataset$version; dataset$current })
    fit_result <- shiny::reactiveVal(NULL)
    prepared_ts <- shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::req(dataset$info)
      dcols <- dataset$info$date_cols
      shiny::updateSelectInput(session, "date_col", choices = dcols)
      shiny::updateSelectInput(session, "metric", choices = dataset$info$numeric_cols)
    })

    output$notes <- shiny::renderUI({
      pt <- prepared_ts()
      if (is.null(pt)) return(shiny::em("Select a date column and metric, then run the forecast."))
      if (pt$n_missing_periods > 0) {
        shiny::div(class = "alert alert-warning mb-2",
                   sprintf("%d missing period(s) were interpolated (total stays consistent; flagged here for transparency).", pt$n_missing_periods))
      } else if (length(pt$values) < 24) {
        shiny::div(class = "alert alert-warning mb-2",
                   sprintf("Only %d aggregated periods available - forecasts will be short-horizon and uncertain.", length(pt$values)))
      } else NULL
    })

    shiny::observeEvent(input$run, {
      res <- shiny_try({
        shiny::req(df(), input$date_col, input$metric)
        prepared <- prepare_time_series(df(), input$date_col, input$metric, freq = input$freq)
        prepared_ts(prepared)
        if (length(prepared$values) < input$horizon + 10) {
          stop(sprintf("Only %d periods available; need at least horizon + 10 for an honest backtest.",
                       length(prepared$values)), call. = FALSE)
        }
        withProgress(message = "Fitting models...", value = 0.4, {
          mltd <- fit_forecast_models(prepared, horizon = input$horizon,
                                      seasonal_length = if (input$freq == "month") 12 else if (input$freq == "week") 52 else 7)
        })
        fit_result(mltd)
        acc <- mltd$accuracy
        if (!is.null(acc) && nrow(acc) > 0) {
          best <- acc[order(acc$RMSE), ][1, ]
          mdl <- mltd$models[[best$model]]
          fc <- final_forecast(mltd, best$model, input$horizon)
          fit_result(mltd)
          last_forecast <- list(fc = fc, best_model = best$model, best_rmse = best$RMSE,
                                spd = prepared, acc = acc)
          forecast_val(last_forecast)
        }
        ""
      }, context = "forecasting")
      invisible(res)
    })

    forecast_val <- function(v) {
      fit_result_ <- fit_result()
      assign(".mdi_forecast", v, envir = session$userData)
      tab <- as.data.frame(v$fc$mean)
      names(tab) <- "point_forecast"
      idx <- switch(v$spd$freq,
        day = seq(v$spd$periods[length(v$spd$periods)], by = "day", length.out = length(v$fc$mean))[-1],
        week = seq(v$spd$periods[length(v$spd$periods)], by = "week", length.out = length(v$fc$mean))[-1],
        month = seq(v$spd$periods[length(v$spd$periods)], by = "month", length.out = length(v$fc$mean))[-1])
      if (length(tab$point_forecast) != length(idx)) idx <- idx[seq_len(nrow(tab))]
      tab$period <- idx
      tab$lower95 <- v$fc$lower[, 2]
      tab$upper95 <- v$fc$upper[, 2]
      state <- list(
        forecast_table = data.frame(period = tab$period, point_forecast = round(tab$point_forecast, 2),
                                    lower_95 = round(tab$lower95, 2), upper_95 = round(tab$upper95, 2)),
        summary = list(metric = v$spd$metric, horizon = input$horizon, best_model = v$best_model,
                       best_rmse = v$best_rmse, n = length(v$spd$values),
                       direction = if (utils::tail(v$fc$mean, 1) > utils::head(v$spd$values, 1)) "upward" else "downward",
                       direction_text = sprintf("%.1f%% over the last observed value",
                                                100 * (utils::tail(v$fc$mean, 1) - utils::tail(v$spd$values, 1)) / utils::tail(v$spd$values, 1)))
      )
      session$userData$.mdi_forecast_state <- state
      if (!is.null(forecast_out)) forecast_out(state)
    }

    output$plot <- plotly::renderPlotly({
      fr <- session$userData$.mdi_forecast
      prepared <- prepared_ts()
      shiny::req(prepared, fr)
      hist <- data.frame(period = prepared$periods, value = as.numeric(prepared$values), kind = "Historical")
      fc <- fr$fc
      fidx <- switch(fr$spd$freq,
        day = seq(fr$spd$periods[length(fr$spd$periods)], by = "day", length.out = length(fc$mean))[-1],
        week = seq(fr$spd$periods[length(fr$spd$periods)], by = "week", length.out = length(fc$mean))[-1],
        month = seq(fr$spd$periods[length(fr$spd$periods)], by = "month", length.out = length(fc$mean))[-1])
      fdat <- data.frame(period = fidx, value = as.numeric(fc$mean),
                         lower = as.numeric(fc$lower[, 2]), upper = as.numeric(fc$upper[, 2]),
                         kind = "Forecast")
      p <- ggplot2::ggplot(hist, ggplot2::aes(x = period, y = value)) +
        ggplot2::geom_line(color = "#345f9c") +
        ggplot2::geom_ribbon(data = fdat, ggplot2::aes(ymin = lower, ymax = upper), fill = "#f0a13e", alpha = .3) +
        ggplot2::geom_line(data = fdat, ggplot2::aes(x = period, y = value), color = "#d64f46", linetype = "dashed") +
        ggplot2::geom_point(data = fdat, ggplot2::aes(x = period, y = value), color = "#d64f46", size = 1.6) +
        ggplot2::labs(title = sprintf("Forecast: %s (model: %s)", fr$spd$metric, fr$best_model),
                      x = NULL, y = fr$spd$metric) + mdi_theme()
      plotly::ggplotly(p)
    })

    output$accuracy <- DT::renderDT({
      fr <- session$userData$.mdi_forecast
      shiny::req(fr)
      DT::datatable(round_df(fr$acc, 3), options = list(dom = "t"), rownames = FALSE,
                    caption = "Accuracy on the most recent periods held out from training (chronological split).")
    })
  })
}
