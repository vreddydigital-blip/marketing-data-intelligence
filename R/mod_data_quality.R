# Data Quality module: issue table, documented 0-100 score with breakdown.

mod_data_quality_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- shiny::tagList(
    bslib::layout_column_wrap(
      width = NULL, fill = FALSE,
      style = css_grid_3(),
      bslib::card(shiny::uiOutput(ns("score_card"))),
      bslib::card(bslib::card_header("Score breakdown"), bslib::card_body(shiny::uiOutput(ns("breakdown")))),
      bslib::card(bslib::card_header("Counts"), bslib::card_body(shiny::uiOutput(ns("metrics"))))
    ),
    bslib::card(
      bslib::card_header("Issues detected"),
      bslib::card_body(DT::DTOutput(ns("issues"))),
      bslib::card_footer(
        shiny::HTML("<small>The score is an internal diagnostic produced by this application, not an industry-standard benchmark. Components: completeness 40 pts, uniqueness 20 pts, type consistency 20 pts, validity 20 pts.</small>")
      )
    )
  )
  ui
}

css_grid_3 <- function() "width: 1fr; display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 1rem;"

mod_data_quality_server <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- shiny::reactive({ dataset$version; dataset$current })

    output$score_card <- shiny::renderUI({
      shiny::req(dataset$quality)
      score <- dataset$quality$score
      cls <- if (score >= 85) "success" else if (score >= 65) "warning" else "danger"
      bslib::card_body(
        shiny::h5("Data Quality Score"),
        shiny::p(shiny::HTML(sprintf('<span class="badge bg-%s fs-2">%s / 100</span>', cls, score))),
        shiny::p(shiny::em("Internal diagnostic score (0-100). Not an industry benchmark. Based on completeness, uniqueness, type consistency, and validity."))
      )
    })

    output$breakdown <- shiny::renderUI({
      shiny::req(dataset$quality$breakdown)
      b <- dataset$quality$breakdown
 shiny::tagList(lapply(seq_len(nrow(b)), function(i) {
        pct <- b$points[i] / b$max_points[i]
        shiny::div(class = "mb-2",
          shiny::p(shiny::strong(b$component[i]),
                   shiny::HTML(sprintf(" <small class=\"text-muted\">%s/%s pts - %s</small>",
                                       b$points[i], b$max_points[i], esc(b$explanation[i])))),
          shiny::div(class = "progress", `aria-label` = esc(b$component[i]),
                     shiny::div(class = "progress-bar", style = sprintf("width:%.0f%%", 100 * pct), role = "progressbar"))
        )
      }))
    })

    output$metrics <- shiny::renderUI({
      shiny::req(dataset$quality$metrics)
      m <- dataset$quality$metrics
      items <- list(
        c("Missing cells", fmt_int(m$missing_cells)),
        c("Duplicate rows", fmt_int(m$duplicate_rows)),
        c("Whitespace-affected cells", fmt_int(m$whitespace_cells)),
        c("Negative metric values", fmt_int(m$negative_metric_values)),
        c("Numbers stored as text", fmt_int(m$number_stored_as_text))
      )
      bslib::card_body(lapply(items, function(it) shiny::p(shiny::strong(it[1]), shiny::HTML(": "), it[2])))
    })

    output$issues <- DT::renderDT({
      shiny::req(dataset$quality)
      DT::datatable(dataset$quality$issues, options = list(pageLength = 10, dom = "tp"),
                    rownames = FALSE, caption = "Issues found in the currently loaded dataset")
    })
  })
}
