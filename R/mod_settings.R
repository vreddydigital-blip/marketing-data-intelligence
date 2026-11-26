# Settings module: appearance, limits, persistence controls.

mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- shiny::tagList(
    bslib::card(
      bslib::card_header("Application settings"),
      bslib::card_body(
        shinyWidgets::switchInput(ns("dark"), "Dark theme", value = FALSE, inline = TRUE),
        shiny::br(),
        shiny::p(shiny::HTML(sprintf("Upload limit: <b>%d MB</b> (set <code>MDI_MAX_UPLOAD_MB</code> in <code>.Renviron</code> to change).", MAX_UPLOAD_MB))),
        shiny::p(shiny::HTML(sprintf("Data directory: <b>%s</b> (temporary by default; uploads are not permanently stored unless you explicitly save a session below.)", esc(mdi_data_dir()))))
      )
    ),
    bslib::card(
      bslib::card_header("Session persistence"),
      bslib::card_body(
        shiny::p("Saving a session records the dataset description and analysis history in the local SQLite database. The full dataset is only stored if you tick the box below; uploads remain temporary otherwise."),
        shinyWidgets::awesomeCheckbox(ns("store_data"), "Also store the current dataset in the database", value = FALSE),
        shiny::actionButton(ns("save_session"), "Save analysis session", class = "btn-primary"),
        shiny::hr(),
        DT::DTOutput(ns("history"))
      )
    )
  )
  ui
}

mod_settings_server <- function(id, dataset, dataset_id = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    saved_msg <- shiny::eventReactive(input$save_session, {
      res <- shiny_try({
        shiny::req(dataset$current)
        id <- dataset_id()
        if (is.null(id)) {
          id <- mdi_db_add_dataset(
            name = gsub("[^A-Za-z0-9_-]", "_", dataset$name %||% "session"),
            display_name = dataset$name %||% "session",
            n_rows = nrow(dataset$current), n_cols = ncol(dataset$current),
            size_bytes = as.numeric(object.size(dataset$current)),
            is_sample = dataset$is_sample,
            stored_path = if (input$store_data) ".sessions" else NA_character_,
            metadata = list(quality_score = as.character(dataset$quality$score),
                            ops = as.character(length(dataset$history)))
          )
        }
        for (h in dataset$history) {
          mdi_db_log_cleaning_op(id, h$label, h$message)
        }
        mdi_db_log_analysis(id, "report", sprintf("session saved: %s", dataset$name))
        "Session saved to SQLite."
      }, context = "saving session")
      res %||% "Session saved."
    })

    shiny::observeEvent(input$dark, {
      tryCatch({
        theme <- bslib::bs_theme_update(app_theme(), `color-mode` = if (isTRUE(input$dark)) "dark" else "light")
        session$setCurrentTheme(theme)
      },
      error = function(e) mdi_log_warn(paste0("theme switch unavailable: ", conditionMessage(e))))
    })

    output$history <- DT::renderDT({
      DT::datatable(mdi_db_list_history(), options = list(pageLength = 8, dom = "tp"), rownames = FALSE)
    })
  })
}
