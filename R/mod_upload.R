# Data Upload module: validation, reading, sheet selection, preview, dataset
# info panel, sample datasets (clearly labelled synthetic demonstration data).

mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- shiny::tagList(
    bslib::card(
      bslib::card_header("File upload"),
      bslib::card_body(
        shiny::fileInput(ns("file"), "Upload CSV or Excel file",
                         accept = c(".csv", ".xls", ".xlsx"),
                         buttonLabel = shiny::span(shiny::icon("upload"), "Choose file"),
                         placeholder = "No file selected"),
        shinyWidgets::numericInputIcon(ns("max_rows_read"), "Read limit (rows)", value = 200000, min = 1000, max = 5000000, step = 1000, icon = shiny::icon("list-ol")),
        shiny::uiOutput(ns("sheet_selector_ui")),
        shiny::div(class = "text-muted",
                   sprintf("Allowed: csv, xls, xlsx. Max size: %d MB. Files are processed as data only and never executed.",
                           MAX_UPLOAD_MB))
      )
    ),
    bslib::card(
      bslib::card_header("Rename columns"),
      bslib::card_body(
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("ren_from"), "Column", choices = NULL)),
          shiny::column(6, shiny::textInput(ns("ren_to"), "New name"))
        ),
        shiny::actionButton(ns("ren_btn"), "Preview rename", class = "btn-outline-primary")
      )
    ),
    bslib::card(
      bslib::card_header("Preview (first 100 rows) and dataset info"),
      bslib::card_body(
        shiny::uiOutput(ns("info_line")),
        DT::DTOutput(ns("preview"))
      )
    ),
    bslib::card(
      bslib::card_header("Demonstration data (synthetic)"),
      bslib::card_body(
        shiny::p("The samples below are generated synthetic datasets for demonstration and testing. They are clearly labelled and entirely fictional."),
        shiny::actionButton(ns("load_marketing"), "Load sample: marketing_campaigns", class = "btn-outline-primary me-1 mb-1"),
        shiny::actionButton(ns("load_sales"), "Load sample: sales_data", class = "btn-outline-primary me-1 mb-1"),
        shiny::actionButton(ns("load_customers"), "Load sample: customer_data", class = "btn-outline-primary me-1 mb-1")
      )
    )
  )
  ui
}

mod_upload_server <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sheets <- shiny::reactive({
      req(input$file)
      excel_sheets_safe(input$file$datapath)
    })

    output$sheet_selector_ui <- shiny::renderUI({
      req(input$file)
      sh <- sheets()
      shiny::tagList(
        if (length(sh) > 1) shiny::selectInput(ns("sheet"), "Excel sheet", choices = sh),
        shiny::actionButton(ns("confirm"), "Load dataset", class = "btn-primary")
      )
    })

    observe_load <- function(path, filename, sheet, display_name, is_sample, size_bytes = NA) {
      res <- shiny_try({
        private_check <- validate_upload_file(path, filename, max_mb = if (is_sample) 1e6 else MAX_UPLOAD_MB)
        if (!private_check$ok) stop(paste(private_check$message, collapse = " "), call. = FALSE)
        shiny::withProgress(message = "Reading data...", value = 0.3, {
          df <- read_data_file(path, filename, sheet = sheet)
        })
        set_dataset(dataset, df, display_name, is_sample = is_sample)
        ""
      }, context = "upload")
      invisible(res)
    }

    shiny::observeEvent(input$confirm, {
      shiny::req(input$file)
      observe_load(input$file$datapath, input$file$name,
                   sheet = if (!is.null(input$sheet)) input$sheet else NULL,
                   display_name = input$file$name, is_sample = FALSE,
                   size_bytes = input$file$size)
    })

    # rename BEFORE loading? No - operates on the current dataset.
    shiny::observe({ shiny::req(df_current()); shiny::updateSelectInput(session, "ren_from", choices = names(df_current())) })
    df_current <- shiny::reactive({ dataset$version; dataset$current })

    shiny::observeEvent(input$ren_btn, {
      res <- shiny_try({
        shiny::req(df_current(), nzchar(input$ren_to))
        r <- rename_columns(df_current(), setNames(input$ren_to, input$ren_from))
        shiny::showModal(shiny::modalDialog(
          title = "Preview: rename column",
          shiny::p(r$message),
          footer = shiny::tagList(
            shiny::actionButton(ns("ren_apply"), "Apply", class = "btn-primary"),
            shiny::modalButton("Cancel")), easyClose = TRUE))
        r
      }, context = "column rename")
      if (!is.null(res)) assign(".mdi_pending", res, envir = session$userData)
    })
    shiny::observeEvent(input$ren_apply, {
      pending <- session$userData$.mdi_pending
      shiny::removeModal()
      if (is.null(pending)) return(NULL)
      apply_cleaning(dataset, pending, "Rename column")
      assign(".mdi_pending", NULL, envir = session$userData)
      shiny::showNotification("Column renamed.", type = "message", duration = 4)
    })

    output$info_line <- shiny::renderUI({
      shiny::req(df_current())
      i <- dataset$info
      blobs <- list(
        c("Rows", fmt_int(i$n_rows)),
        c("Columns", fmt_int(i$n_cols)),
        c("Memory (MB)", format(i$size_mb, digits = 3)),
        c("Duplicate rows", fmt_int(i$duplicate_rows)),
        c("Date columns", if (length(i$date_cols)) paste(i$date_cols, collapse = ", ") else "none detected"),
        c("Numeric columns", if (length(i$numeric_cols)) paste(utils::head(i$numeric_cols, 6), collapse = ", ") else "none"),
        c("Categorical columns", if (length(i$categorical_cols)) paste(utils::head(i$categorical_cols, 6), collapse = ", ") else "none"))
      shiny::tagList(lapply(blobs, function(b) shiny::span(class = "me-3", shiny::strong(b[1]), shiny::HTML(": "), b[2])))
    })

    output$preview <- DT::renderDT({
      shiny::req(df_current())
      DT::datatable(preview_table(df_current(), 100),
                    options = list(pageLength = 10, dom = "tp", scrollX = TRUE), rownames = FALSE)
    })

    load_sample <- function(fname, display_name) {
      p <- file.path(app_root_dir(), "sample_data", fname)
      if (!file.exists(p)) {
        shiny::showNotification("Sample dataset not found on disk.", type = "error")
        return(NULL)
      }
      observe_load(p, fname, sheet = NULL, display_name = paste(display_name, "(synthetic sample)"), is_sample = TRUE)
    }

    shiny::observeEvent(input$load_marketing, load_sample("marketing_campaigns.csv", "Marketing campaigns"))
    shiny::observeEvent(input$load_sales, load_sample("sales_data.csv", "Sales data"))
    shiny::observeEvent(input$load_customers, load_sample("customer_data.csv", "Customer data"))
  })
}
