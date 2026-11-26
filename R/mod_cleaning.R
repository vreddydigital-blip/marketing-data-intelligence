# Cleaning module: previews each operation in a modal before applying to the
# working copy; reset restores the untouched original.

mod_cleaning_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- shiny::tagList(
    bslib::card(
      bslib::card_header("Cleaning operations"),
      bslib::card_body(
        shiny::fluidRow(
          shiny::column(4,
            shiny::h6("Structure"),
            shiny::actionButton(ns("dedupe"), "Remove duplicate rows", class = "btn-outline-primary mb-1 w-100"),
            shiny::selectInput(ns("drop_cols"), "Columns to remove", choices = NULL, multiple = TRUE),
            shiny::actionButton(ns("drop_cols_btn"), "Preview: remove columns", class = "btn-outline-primary mb-1 w-100"),
            shiny::selectInput(ns("trim_col"), "Trim whitespace (column)", choices = NULL),
            shiny::actionButton(ns("trim_btn"), "Preview: trim whitespace", class = "btn-outline-primary w-100")
          ),
          shiny::column(4,
            shiny::h6("Missing values"),
            shiny::selectInput(ns("impute_num_col"), "Numeric column to impute", choices = NULL),
            shinyWidgets::radioGroupButtons(ns("impute_num_method"), "Method", choices = c("mean", "median"), selected = "median", size = "sm"),
            shiny::actionButton(ns("impute_num_btn"), "Preview: impute numeric", class = "btn-outline-primary mb-1 w-100"),
            shiny::selectInput(ns("impute_cat_col"), "Categorical column to impute", choices = NULL),
            shinyWidgets::radioGroupButtons(ns("impute_cat_method"), "Method", choices = c("mode", "missing_category"), selected = "missing_category", size = "sm"),
            shiny::actionButton(ns("impute_cat_btn"), "Preview: impute categorical", class = "btn-outline-primary w-100")
          ),
          shiny::column(4,
            shiny::h6("Rows and values"),
            shiny::selectInput(ns("filter_col"), "Filter column", choices = NULL),
            shiny::selectInput(ns("filter_op"), "Operator", choices = c("==", "!=", ">", ">=", "<", "<=", "contains", "not NA")),
            shiny::textInput(ns("filter_val"), "Value"),
            shiny::actionButton(ns("filter_btn"), "Preview: filter rows", class = "btn-outline-primary mb-1 w-100"),
            shiny::selectInput(ns("transform_col"), "Numeric column to transform", choices = NULL),
            shinyWidgets::radioGroupButtons(ns("transform_kind"), "Transform", choices = c("log", "sqrt", "zscore"), size = "sm"),
            shiny::actionButton(ns("transform_btn"), "Preview: transform", class = "btn-outline-primary mb-1 w-100"),
            shiny::selectInput(ns("cat_std_col"), "Standardise categories (column)", choices = NULL),
            shinyWidgets::radioGroupButtons(ns("cat_std_style"), "Style", choices = c("lower", "title_case", "upper", "snake"), selected = "lower", size = "sm"),
            shiny::actionButton(ns("cat_std_btn"), "Preview: standardise", class = "btn-outline-primary w-100")
          )
        )
      )
    ),
    bslib::card(
      bslib::card_header("Session history"),
      bslib::card_body(shiny::uiOutput(ns("history"))),
      bslib::card_footer(shiny::actionButton(ns("reset"), "Reset to original data", class = "btn-warning"))
    )
  )
  ui
}

mod_cleaning_server <- function(id, dataset, dataset_id = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- shiny::reactive({ dataset$version; dataset$current })

    shiny::observe({
      shiny::req(df())
      cols <- names(df())
      num_cols <- intersect(cols, dataset$info$numeric_cols)
      cat_cols <- intersect(cols, dataset$info$categorical_cols)
      shiny::updateSelectInput(session, "drop_cols", choices = cols)
      shiny::updateSelectInput(session, "trim_col", choices = cat_cols)
      shiny::updateSelectInput(session, "impute_num_col", choices = num_cols)
      shiny::updateSelectInput(session, "impute_cat_col", choices = cat_cols)
      shiny::updateSelectInput(session, "filter_col", choices = cols)
      shiny::updateSelectInput(session, "transform_col", choices = num_cols)
      shiny::updateSelectInput(session, "cat_std_col", choices = cat_cols)
    })

    preview_and_apply <- function(op_label, result) {
      if (is.null(result)) return(invisible(NULL))
      shiny::showModal(shiny::modalDialog(
        title = paste("Preview:", op_label),
        shiny::p(shiny::strong(op_label)),
        shiny::p(result$message),
        shiny::p(shiny::HTML(sprintf("Rows: %d - %d, Columns: %d - %d",
                                     nrow(dataset$current), nrow(result$data),
                                     ncol(dataset$current), ncol(result$data)))),
        footer = shiny::tagList(
          shiny::actionButton(ns("apply_confirm"), "Apply", class = "btn-primary"),
          shiny::modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
      set_cached <- get0(".mdi_pending", envir = session$userData, ifnotfound = NULL)
      assign(".mdi_pending", list(label = op_label, result = result), envir = session$userData)
    }

    shiny::observeEvent(input$apply_confirm, {
      pending <- session$userData$.mdi_pending
      shiny::removeModal()
      if (is.null(pending)) return(NULL)
      apply_cleaning(dataset, pending$result, pending$label, dataset_id())
      assign(".mdi_pending", NULL, envir = session$userData)
      shiny::showNotification(paste0("Applied: ", pending$label), type = "message", duration = 5)
    })

    spark <- shiny::reactive({ input$dedupe; NULL })
    output$is_ready <- shiny::renderText(if (is.null(df())) "" else "ready")
    shiny::observeEvent(input$dedupe, preview_and_apply("Remove duplicate rows",
      remove_duplicate_rows(df())))
    shiny::observeEvent(input$drop_cols_btn, preview_and_apply("Remove columns",
      drop_columns(df(), input$drop_cols)))
    shiny::observeEvent(input$trim_btn, preview_and_apply("Trim whitespace",
      trim_whitespace_columns(df(), input$trim_col)))
    shiny::observeEvent(input$impute_num_btn, preview_and_apply("Impute numeric missing",
      impute_numeric(df(), input$impute_num_col, method = input$impute_num_method)))
    shiny::observeEvent(input$impute_cat_btn, preview_and_apply("Impute categorical missing",
      impute_categorical(df(), input$impute_cat_col, method = input$impute_cat_method)))
    shiny::observeEvent(input$filter_btn, preview_and_apply("Filter rows",
      apply_row_filters(df(), list(list(column = input$filter_col, operator = input$filter_op,
                                       value = input$filter_val)))))
    shiny::observeEvent(input$transform_btn, preview_and_apply("Transform column",
      transform_columns(df(), setNames(list(input$transform_kind), input$transform_col))))
    shiny::observeEvent(input$cat_std_btn, preview_and_apply("Standardise categories",
      standardize_categories(df(), input$cat_std_col, style = input$cat_std_style)))

    shiny::observeEvent(input$reset, {
      shiny::req(dataset$original)
      set_dataset(dataset, dataset$original, dataset$name, is_sample = dataset$is_sample)
      shiny::showNotification("Reset to the originally loaded data.", type = "message", duration = 5)
    })

    output$history <- shiny::renderUI({
      if (!length(dataset$history)) return(shiny::em("No cleaning operations applied yet."))
      shiny::tagList(lapply(rev(dataset$history), function(h) {
        shiny::p(shiny::strong(h$label), shiny::HTML(" - "), esc(h$message))
      }))
    })
  })
}
