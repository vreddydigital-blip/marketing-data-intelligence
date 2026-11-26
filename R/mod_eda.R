# EDA module: dataset summary, numeric and categorical tables, missingness.

mod_eda_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(title = "Rows", value = shiny::uiOutput(ns("rows_k")), showcase = shiny::icon("table")),
      bslib::value_box(title = "Columns", value = shiny::uiOutput(ns("cols_k")), showcase = shiny::icon("table-columns")),
      bslib::value_box(title = "Duplicate rows", value = shiny::uiOutput(ns("dup_k")), showcase = shiny::icon("copy"))
    ),
    bslib::navset_tab(
      bslib::nav_panel("Column overview", DT::DTOutput(ns("col_table"))),
      bslib::nav_panel("Numeric statistics", DT::DTOutput(ns("num_table"))),
      bslib::nav_panel("Categorical statistics", DT::DTOutput(ns("cat_table"))),
      bslib::nav_panel("Missing values", bslib::card_body(DT::DTOutput(ns("miss_table")), plotly::plotlyOutput(ns("miss_plot"), height = "320px")))
    )
  )
  ui
}

mod_eda_server <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    df <- shiny::reactive({ dataset$version; dataset$current })

    output$rows_k <- shiny::renderUI({
      if (is.null(df())) "-"
      else fmt_int(dataset$info$n_rows)
    })
    output$cols_k <- shiny::renderUI({ if (is.null(df())) "-" else fmt_int(dataset$info$n_cols) })
    output$dup_k  <- shiny::renderUI({ if (is.null(df())) "-" else fmt_int(dataset$info$duplicate_rows) })

    output$col_table <- DT::renderDT({
      shiny::req(dataset$info)
      cols <- do.call(rbind, lapply(dataset$info$columns, function(c) {
        data.frame(column = c$name, type = c$class, missing = c$missing,
                   `missing %` = c$missing_pct, unique = c$unique,
                   flags = paste0(c(
                     if (c$empty) "empty", if (c$constant) "constant",
                     if (c$is_date) "date", if (c$is_numeric) "numeric"),
                   collapse = ", "), stringsAsFactors = FALSE)
      }))
      DataTable2(cols)
    })

    output$num_table <- DT::renderDT({
      shiny::req(df())
      s <- numeric_summary(df())
      if (is.null(s)) return(DataTable2(data.frame(note = "No numeric columns found")))
      DataTable2(round_df(s, 4))
    })

    output$cat_table <- DT::renderDT({
      shiny::req(df())
      s <- categorical_summary(df())
      if (is.null(s)) return(DataTable2(data.frame(note = "No categorical columns found")))
      DataTable2(s)
    })

    output$miss_table <- DT::renderDT({
      shiny::req(df())
      DataTable2(missingness_summary(df()))
    })

    output$miss_plot <- plotly::renderPlotly({
      shiny::req(df())
      m <- missingness_summary(df())
      m <- m[order(-m$missing), ]
      p <- ggplot2::ggplot(m, ggplot2::aes(x = stats::reorder(column, missing), y = missing)) +
        ggplot2::geom_col(fill = "#345f9c") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Missing values", title = "Missing values per column") +
        mdi_theme()
      plotly::ggplotly(p)
    })
  })
}

DataTable2 <- function(df) {
  DT::datatable(df, options = list(pageLength = 10, dom = "tpl", scrollX = TRUE), rownames = FALSE)
}

round_df <- function(df, d = 4) {
  df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, d) else x)
  df
}

mdi_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom")
}
