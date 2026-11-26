# Marketing Data Intelligence - main application (UI + server).

shinyApp(
  ui = function(request) {
    bslib::page_navbar(
      title = shiny::span(shiny::icon("chart-line"), "Marketing Data Intelligence"),
      theme = app_theme(),
      sidebar = bslib::sidebar(
        width = 260,
        shiny::div(class = "text-muted", style = "padding-bottom: .5rem",
                   shiny::strong("Dataset"), shiny::br(), shiny::uiOutput("sb_dataset")),
        shiny::p(shiny::uiOutput("sb_quality")),
        shiny::hr(),
        shiny::p(class = "text-muted", "Marketing Data Intelligence - local-first marketing analytics. R ", as.character(getRversion()))
      ),
      id = "nav",
      bslib::nav_panel("Overview", mod_overview_ui("overview")),
      bslib::nav_panel("Data Upload", mod_upload_ui("upload")),
      bslib::nav_panel("Data Quality", mod_data_quality_ui("quality")),
      bslib::nav_panel("Data Cleaning", mod_cleaning_ui("cleaning")),
      bslib::nav_panel("Exploratory Data Analysis", mod_eda_ui("eda")),
      bslib::nav_panel("Visualizations", mod_visualizations_ui("viz")),
      bslib::nav_panel("Marketing Analytics", mod_marketing_analytics_ui("marketing")),
      bslib::nav_panel("Statistical Analysis", mod_statistics_ui("stats")),
      bslib::nav_panel("Machine Learning", mod_machine_learning_ui("ml")),
      bslib::nav_panel("Forecasting", mod_forecasting_ui("forecast")),
      bslib::nav_panel("AI-Style Insights", mod_insights_ui("insights")),
      bslib::nav_panel("Reports", mod_reports_ui("reports")),
      bslib::nav_panel("Settings", mod_settings_ui("settings"))
    )
  },
  server = function(input, output, session) {
    dataset <- new_dataset_state()

    shared <- shiny::reactiveValues(
      kpi_summary = NULL,
      mapping = NULL,
      stats_results = NULL,
      model_metrics = NULL,
      forecast_state = NULL,
      insights = NULL
    )

    mod_overview_server("overview", dataset)
    mod_upload_server("upload", dataset)
    mod_data_quality_server("quality", dataset)
    mod_cleaning_server("cleaning", dataset)
    mod_eda_server("eda", dataset)
    mod_visualizations_server("viz", dataset)
    mod_marketing_analytics_server("marketing", dataset)
    mod_statistics_server("stats", dataset,
      results_out = function(res) shared$stats_results <- c(shared$stats_results, list(res)))
    mod_machine_learning_server("ml", dataset,
      model_metrics_out = function(m) shared$model_metrics <- m)
    mod_forecasting_server("forecast", dataset,
      forecast_out = function(s) shared$forecast_state <- s)
    mod_insights_server("insights", dataset, shared_state = shiny::reactive(as.list(shared)))
    mod_reports_server("reports", dataset, shared_state = shiny::reactive(as.list(shared)))
    mod_settings_server("settings", dataset)

    output$sb_dataset <- shiny::renderUI({
      if (is.null(dataset$name)) "No dataset loaded" else esc(dataset$name)
    })
    output$sb_quality <- shiny::renderUI({
      q <- dataset$quality
      if (is.null(q)) return("")
      cls <- if (q$score >= 85) "success" else if (q$score >= 65) "warning" else "danger"
      shiny::HTML(sprintf('Quality: <span class="badge bg-%s">%s/100</span><br><small>internal diagnostic</small>', cls, q$score))
    })

    shiny::onSessionEnded(function() {
      mdi_log_info("session ended")
    })
  },
  onStart = function() {
    mdi_log_info("application started")
  }
)
