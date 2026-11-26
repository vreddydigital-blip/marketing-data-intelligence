# Overview module: landing page, workflow steps, dataset snapshot.

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- shiny::tagList(
    bslib::layout_column_wrap(width = 1/3, fill = FALSE,
      bslib::value_box(title = "Dataset", value = shiny::uiOutput(ns("name_k")), showcase = shiny::icon("file-lines")),
      bslib::value_box(title = "Rows x Columns", value = shiny::uiOutput(ns("dims_k")), showcase = shiny::icon("table")),
      bslib::value_box(title = "Quality score", value = shiny::uiOutput(ns("quality_k")), showcase = shiny::icon("gauge"))
    ),
    bslib::card(
      bslib::card_header("Workflow"),
      bslib::card_body(shiny::uiOutput(ns("steps"))),
      bslib::card_footer(shiny::em("Start with Data Upload. Sample data can be loaded there for demonstration; the application shows real data once you upload yours."))
    )
  )
  ui
}

mod_overview_server <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    output$name_k <- shiny::renderUI({
      if (is.null(dataset$name)) shiny::em("No dataset loaded") else esc(dataset$name)
    })
    output$dims_k <- shiny::renderUI({
      if (is.null(dataset$current)) "0 x 0"
      else sprintf("%s x %s", fmt_int(nrow(dataset$current)), fmt_int(ncol(dataset$current)))
    })
    output$quality_k <- shiny::renderUI({
      if (is.null(dataset$quality)) "n/a" else sprintf("%s / 100", dataset$quality$score)
    })
    top_n <- function(nm, k = 3) {
      tab <- sort(table(dataset$current[[nm]]), decreasing = TRUE)
      if (!length(tab)) return(shiny::em("n/a"))
      paste(sprintf("%s (%d)", names(tab)[seq_len(min(k, length(tab)))], as.integer(tab)[seq_len(min(k, length(tab)))]), collapse = ", ")
    }
    output$steps <- shiny::renderUI({
      if (is.null(dataset$current)) {
        return(shiny::ol(
          shiny::li("Upload a dataset in the ", shiny::strong("Data Upload"), " tab (CSV/Excel, max ", sprintf("%d MB", MAX_UPLOAD_MB), ")."),
          shiny::li("Review ", shiny::strong("Data Quality"), " and repair issues in ", shiny::strong("Data Cleaning"), "."),
          shiny::li("Explore ", shiny::strong("EDA"), " and ", shiny::strong("Visualizations"), "."),
          shiny::li("Compute ", shiny::strong("Marketing Analytics"), " KPIs."),
          shiny::li("Run ", shiny::strong("Statistical Analysis"), ", ", shiny::strong("Machine Learning"), " and ", shiny::strong("Forecasting"), "."),
          shiny::li("Read ", shiny::strong("Insights"), " and export ", shiny::strong("Reports"), ".")
        ))
      }
      info_bits <- c(
        sprintf("Dataset: %s", dataset$name),
        sprintf("Rows: %s; Columns: %s", fmt_int(nrow(dataset$current)), fmt_int(ncol(dataset$current)))
      )
      shiny::tagList(
        shiny::p(shiny::strong("Current dataset snapshot (no synthetic data shown):")),
        shiny::ul(shiny::li(info_bits[1]), shiny::li(info_bits[2])),
        shiny::em("Details per module below.")
      )
    })
  })
}
