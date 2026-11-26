# Insights module: renders deterministic rule-based insights with evidence.

mod_insights_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Automated insights (rule-based engine)"),
    bslib::card_body(
      shiny::actionButton(ns("refresh"), "Generate insights", class = "btn-primary"),
      shiny::uiOutput(ns("insight_cards"))
    ),
    bslib::card_footer(
      shiny::HTML("<small>These rules run locally on your data. An AI model can be plugged in later without changing how insights are displayed.</small>")
    )
  )
}

mod_insights_server <- function(id, dataset, shared_state = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    insights <- shiny::reactiveVal(NULL)

    state_now <- shiny::reactive({
      shared <- shared_state()
      list(
        df = dataset$current,
        name = dataset$name,
        quality = dataset$quality,
        kpi_summary = if (!is.null(shared) && !is.null(shared$kpi_summary)) shared$kpi_summary else NULL,
        mapping = if (!is.null(shared) && !is.null(shared$mapping)) shared$mapping else detect_marketing_columns(dataset$current %||% data.frame()),
        correlations = if (!is.null(shared) && !is.null(shared$correlations)) shared$correlations else {
          if (!is.null(dataset$current)) correlation_matrix(dataset$current) else NULL
        },
        forecast_summary = if (!is.null(shared)) shared$forecast_summary else NULL,
        model_metrics = if (!is.null(shared)) shared$model_metrics else NULL
      )
    })

    shiny::observeEvent(input$refresh, {
      res <- shiny_try(generate_insights(state_now()), context = "insight generation")
      if (!is.null(res)) {
        insights(res)
        mdi_db_log_analysis(NULL, "insights", sprintf("%d insights generated", nrow(res)))
      }
    })

    output$insight_cards <- shiny::renderUI({
      ins <- insights()
      if (is.null(ins)) {
        return(shiny::div(class = "alert alert-light",
          shiny::em("No insights computed yet for this dataset. Click 'Generate insights'. The engine only reports findings backed by the data; otherwise it will say so explicitly.")))
      }
      do.call(bslib::layout_column_wrap, c(list(width = 1/2, fill = FALSE),
        lapply(seq_len(nrow(ins)), function(i) {
          row <- ins[i, ]
          bslib::card(
            bslib::card_header(esc(row$title)),
            bslib::card_body(
              shiny::p(row$explanation),
              shiny::p(shiny::strong("Metric: "), esc(row$metric)),
              shiny::p(shiny::strong("Evidence: "), esc(row$evidence)),
              shiny::p(shiny::strong("Recommended action: "), esc(row$action)),
              shiny::p(shiny::em(shiny::strong("Confidence / limitation: "), esc(row$confidence)))
            )
          )
        })
      ))
    })
  })
}
