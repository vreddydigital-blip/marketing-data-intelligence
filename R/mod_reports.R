# Reports module: Excel, CSV, and HTML (R Markdown) downloads; PDF raises a
# clear, actionable error when no LaTeX toolchain exists.

mod_reports_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Export analysis"),
    bslib::card_body(
      shiny::p("Reports use the current (cleaned) dataset and all analysis state computed so far."),
      shiny::fluidRow(
        shiny::column(3, shiny::downloadButton(ns("dl_xlsx"), "Excel report", class = "btn-primary")),
        shiny::column(3, shiny::downloadButton(ns("dl_csv"), "CSV (dataset)", class = "btn-outline-primary")),
        shiny::column(3, shiny::downloadButton(ns("dl_html"), "HTML report", class = "btn-primary")),
        shiny::column(3, shiny::downloadButton(ns("dl_pdf"), "PDF report", class = "btn-outline-primary"))
      ),
      shiny::HTML('<div class="alert alert-secondary mt-2"><small>HTML reports are rendered with R Markdown. PDF requires a LaTeX toolchain (TinyTeX); if unavailable, you will see a clear message and can print the HTML to PDF instead.</small></div>')
    )
  )
}

build_state_for_reports <- function(dataset, shared_state, session) {
  shared <- shared_state()
  list(
    df = dataset$current,
    name = dataset$name,
    info = dataset$info,
    quality = dataset$quality,
    kpi_summary = shared$kpi_summary,
    mapping = shared$mapping,
    stats_results = shared$stats_results,
    model_metrics = shared$model_metrics,
    forecast_table = if (!is.null(shared$forecast_state)) shared$forecast_state$forecast_table else NULL,
    insights = shared$insights
  )
}

mod_reports_server <- function(id, dataset, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    get_state <- function() {
      st <- build_state_for_reports(dataset, shared_state, session)
      if (is.null(st$df)) stop("No dataset loaded. Load data first.", call. = FALSE)
      st
    }

    notify_rendered <- function(fname) {
      tryCatch(mdi_db_add_report(NULL, tools::file_ext(fname), fname), error = function(e) NULL)
      showNotification(sprintf("%s generated.", fname), type = "message", duration = 5)
    }

    output$dl_xlsx <- shiny::downloadHandler(
      filename = function() sprintf("mdi_report_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        shiny::withProgress(value = .3, message = "Building Excel report...", {
          st <- get_state()
          tryCatch({
            build_excel_report(st, file)
            notify_rendered(basename(file))
          }, error = function(e) {
            shiny::showNotification(shiny::HTML(sprintf("<b>Excel report failed:</b> %s", esc(conditionMessage(e)))), type = "error")
          })
        })
      }
    )

    output$dl_csv <- shiny::downloadHandler(
      filename = function() sprintf("mdi_dataset_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        st <- get_state()
        readr::write_csv(st$df, file)
      }
    )

    output$dl_html <- shiny::downloadHandler(
      filename = function() sprintf("mdi_report_%s.html", format(Sys.time(), "%Y%m%d_%H%M%S")),
      contentType = "text/html",
      content = function(file) {
        shiny::withProgress(value = .2, message = "Rendering HTML report (this takes a moment)...", {
          st <- get_state()
          tryCatch({
            build_html_report(st, file)
            notify_rendered(basename(file))
          }, error = function(e) {
            shiny::showNotification(shiny::HTML(sprintf("<b>HTML report failed:</b> %s", esc(conditionMessage(e)))), type = "error", duration = 12)
          })
        })
      }
    )

    output$dl_pdf <- shiny::downloadHandler(
      filename = function() sprintf("mdi_report_%s.pdf", format(Sys.time(), "%Y%m%d_%H%M%S")),
      contentType = "application/pdf",
      content = function(file) {
        tryCatch(pdf_error_if_unavailable(),
          error = function(e) {
            shiny::showNotification(shiny::HTML(sprintf("<b>PDF unavailable:</b> %s", esc(conditionMessage(e)))), type = "warning", duration = 15)
            writeLines("PDF toolchain not available.", file)
            return(invisible(NULL))
          })
      }
    )
  })
}
