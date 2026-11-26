# Marketing analytics module: mapping, KPI cards, filters, comparisons,
# funnel, ranking, budget allocation.

mod_marketing_analytics_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- shiny::tagList(
    bslib::card(
      bslib::card_header("Filters"),
      bslib::card_body(
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("date_filter_ui"))),
          shiny::column(8, shiny::uiOutput(ns("cat_filter_ui"))),
          shiny::column(3, shinyWidgets::dropdown(
            shiny::uiOutput(ns("mapping_ui")), label = "Column mapping", shiny::icon("link"),
            size = "sm"))),
      )
    ),
    shiny::uiOutput(ns("kpi_cards")),
    bslib::navset_tab(
      bslib::nav_panel("Performance by campaign", DT::DTOutput(ns("campaign_table"))),
      bslib::nav_panel("Performance by channel", DT::DTOutput(ns("channel_table"))),
      bslib::nav_panel("Trends", bslib::card_body(
        shinyWidgets::radioGroupButtons(ns("grain"), "Grain", c("day", "week", "month"), size = "sm"),
        plotly::plotlyOutput(ns("trend_plot"), height = "420px"))),
      bslib::nav_panel("Spend vs conversions", plotly::plotlyOutput(ns("sc1"), height = "420px")),
      bslib::nav_panel("Revenue vs spend", plotly::plotlyOutput(ns("sc2"), height = "420px")),
      bslib::nav_panel("Funnel", plotly::plotlyOutput(ns("funnel"), height = "420px")),
      bslib::nav_panel("Efficiency ranking", plotly::plotlyOutput(ns("rank"), height = "420px")),
      bslib::nav_panel("Budget allocation", plotly::plotlyOutput(ns("budget"), height = "420px"))
    ),
    shiny::uiOutput(ns("missing_roles_note"))
  )
  ui
}

mod_marketing_analytics_server <- function(id, dataset, mapping_out = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- shiny::reactive({ dataset$version; dataset$current })

    auto_mapping <- shiny::reactive({
      shiny::req(df())
      detect_marketing_columns(df())
    })

    # user overrides for roles, applied on top of auto mapping
    mapping <- shiny::reactive({
      am <- auto_mapping()
      for (role in names(MARKETING_PATTERNS)) {
        sel <- input[[paste0("map_", role)]]
        if (!is.null(sel) && sel != "(not present)" && sel %in% names(df())) am[[role]] <- sel
      }
      am
    })

    output$mapping_ui <- shiny::renderUI({
      shiny::req(df())
      am <- auto_mapping()
      shiny::tagList(
        shiny::p("Map columns to marketing roles. Blank values disable the associated KPIs."),
        lapply(names(MARKETING_PATTERNS), function(role) {
          choices <- c("(not present)", names(df()))
          shiny::selectInput(ns(paste0("map_", role)),
                             caps_(role), choices = choices, selected = am[[role]] %||% "(not present)",
                             width = "100%")
        })
      )
    })

    caps_ <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

    output$date_filter_ui <- shiny::renderUI({
      m <- shiny::isolate(mapping())
      dcol <- m$date %||% (if (length(dataset$info$date_cols)) dataset$info$date_cols[1] else NULL)
      if (is.null(dcol)) return(NULL)
      rng <- range(as.Date(shiny::isolate(dataset$current)[[dcol]]), na.rm = TRUE)
      shiny::dateRangeInput(ns("dates"), "Date range", start = rng[1], end = rng[2])
    })

    output$cat_filter_ui <- shiny::renderUI({
      shiny::req(df())
      m <- mapping()
      cols <- intersect(unlist(m[c("campaign", "channel", "source")]), names(dataset$current))
      if (!length(cols)) return(shiny::p(shiny::em("No campaign/channel/source columns detected in this dataset.")))
      shiny::tagList(lapply(cols, function(cc) {
        ch <- unique(as.character(dataset$current[[cc]]))
        ch <- sort(ch[!is.na(ch)])
        shiny::selectInput(ns(paste0("flt_", cc)), caps_(cc), choices = c("(all)", ch), multiple = TRUE,
                           selected = "(all)")
      }))
    })

    filtered_df <- shiny::reactive({
      d <- df()
      shiny::req(d)
      m <- mapping()
      if (!is.null(m$date) && m$date %in% names(d) && !is.null(input$dates) && all(nchar(input$dates) > 4)) {
        dd <- as.Date(d[[m$date]])
        d <- d[!is.na(dd) & dd >= input$dates[1] & dd <= input$dates[2], , drop = FALSE]
      }
      for (cc in intersect(unlist(m[c("campaign", "channel", "source")]), names(d))) {
        sel <- input[[paste0("flt_", cc)]]
        if (!is.null(sel) && length(sel) && !"(all)" %in% sel) {
          d <- d[as.character(d[[cc]]) %in% sel | is.na(d[[cc]]), , drop = FALSE]
        }
      }
      d
    })

    kpis_now <- shiny::reactive({
      d <- filtered_df()
      shiny::req(d, nrow(d) > 0)
      list(summary = kpi_summary(d, mapping()), df = d)
    })

    output$kpi_cards <- shiny::renderUI({
      ks <- kpis_now()$summary
      t <- ks$totals; kp <- ks$kpis
      cards <- list(
        c("Total spend", fmt_money(t$spend)),
        c("Impressions", fmt_int(t$impressions)),
        c("Clicks", fmt_int(t$clicks)),
        c("Leads", fmt_int(t$leads)),
        c("Conversions", fmt_int(t$conversions)),
        c("Revenue", fmt_money(t$revenue)),
        c("CTR", fmt_pct(kp$ctr)),
        c("CPC", fmt_money(kp$cpc)),
        c("CPL", fmt_money(kp$cpl)),
        c("CPA", fmt_money(kp$cpa)),
        c("ROAS", if (is.na(kp$roas)) "N/A" else paste0(formatC(kp$roas, format = "f", digits = 2), "x"))
      )
      bslib::layout_column_wrap(
        width = 1/6, fill = FALSE,
        !!!lapply(cards, function(c1) bslib::value_box(title = c1[1], value = shiny::p(c1[2])))
      )
    })

    shiny::observe({
      m <- mapping()
      present <- kpi_roles_present(m)
      missing <- names(present)[!present]
      if (length(missing)) {
        output$missing_roles_note <- shiny::renderUI({
          shiny::div(class = "alert alert-secondary mt-2",
            shiny::HTML(paste0("<b>KPIs not available for this dataset:</b> ",
                               paste(KPI_FORMULAS[missing], collapse = " | "),
                               ". Required columns were not detected or the denominator metric is absent.")))
        })
      } else {
        output$missing_roles_note <- shiny::renderUI(NULL)
      }
    })

    output$campaign_table <- DT::renderDT({
      m <- mapping()
      shiny::req(m$campaign)
      DataTable2(round_df(grouped_performance(filtered_df(), m, m$campaign), 3))
    })
    output$channel_table <- DT::renderDT({
      m <- mapping()
      shiny::req(m$channel)
      DataTable2(round_df(grouped_performance(filtered_df(), m, m$channel), 3))
    })

    output$trend_plot <- plotly::renderPlotly({
      m <- mapping()
      series <- daily_series(filtered_df(), m, freq = input$grain)
      shiny::req(series, nrow(series) > 0)
      long <- tidyr::pivot_longer(series, cols = c("spend", "clicks", "conversions", "revenue"),
                                  names_to = "metric", values_to = "value")
      long <- long[long$metric %in% c("spend", "clicks", "conversions", "revenue"), ]
      p <- ggplot2::ggplot(long, ggplot2::aes(x = period, y = value, color = metric)) +
        ggplot2::geom_line() + ggplot2::labs(title = sprintf("%s totals by %s", "Metric", input$grain)) +
        mdi_theme()
      plotly::ggplotly(p)
    })

    output$sc1 <- plotly::renderPlotly({
      m <- mapping()
      shiny::req(m$campaign)
      perf <- grouped_performance(filtered_df(), m, m$campaign)
      shiny::req(perf, !all(is.na(perf$conversions)))
      p <- ggplot2::ggplot(perf, ggplot2::aes(x = spend, y = conversions, label = .data[[m$campaign]])) +
        ggplot2::geom_point(color = "#345f9c", size = 2.5) +
        ggplot2::labs(title = "Spend versus conversions (campaign level)") + mdi_theme()
      plotly::ggplotly(p)
    })

    output$sc2 <- plotly::renderPlotly({
      m <- mapping()
      shiny::req(m$campaign)
      perf <- grouped_performance(filtered_df(), m, m$campaign)
      shiny::req(perf, !all(is.na(perf$revenue)))
      p <- ggplot2::ggplot(perf, ggplot2::aes(x = spend, y = revenue, label = .data[[m$campaign]])) +
        ggplot2::geom_point(color = "#2f8f5b", size = 2.5) +
        ggplot2::labs(title = "Revenue versus spend (campaign level)") + mdi_theme()
      plotly::ggplotly(p)
    })

    output$funnel <- plotly::renderPlotly({
      f <- funnel_counts(filtered_df(), mapping())
      f <- f[!is.na(f$value), , drop = FALSE]
      shiny::req(nrow(f) >= 2)
      p <- ggplot2::ggplot(f, ggplot2::aes(x = stats::reorder(stage, -value), y = value)) +
        ggplot2::geom_col(fill = "#345f9c") +
        ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = "Count", title = "Conversion funnel (total volume)") +
        mdi_theme()
      plotly::ggplotly(p)
    })

    output$rank <- plotly::renderPlotly({
      m <- mapping()
      shiny::req(m$campaign)
      perf <- grouped_performance(filtered_df(), m, m$campaign)
      shiny::req(perf, !all(is.na(perf$roas)))
      perf <- perf[!is.na(perf$roas), ]
      shiny::req(nrow(perf) > 0)
      p <- ggplot2::ggplot(perf, ggplot2::aes(x = roas, y = stats::reorder(.data[[m$campaign]], roas))) +
        ggplot2::geom_col(fill = "#2f8f5b") +
        ggplot2::labs(x = "ROAS (revenue / spend)", y = NULL, title = "Campaign efficiency ranking") + mdi_theme()
      plotly::ggplotly(p)
    })

    output$budget <- plotly::renderPlotly({
      m <- mapping()
      shiny::req(m$campaign)
      perf <- grouped_performance(filtered_df(), m, m$campaign)
      perf <- perf[!is.na(perf$spend) & perf$spend > 0, ]
      shiny::req(nrow(perf) > 0)
      perf$share <- 100 * perf$spend / sum(perf$spend)
      p <- ggplot2::ggplot(perf, ggplot2::aes(x = "", y = share, fill = .data[[m$campaign]])) +
        ggplot2::geom_col(width = 1) +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::scale_fill_brewer(palette = "Blues") +
        ggplot2::labs(x = NULL, y = NULL, title = "Budget share by campaign (%)") + mdi_theme()
      plotly::ggplotly(p)
    })
  })
}
