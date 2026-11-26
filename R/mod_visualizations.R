# Interactive visualisation builder: user-driven chart type + variables,
# rendered with ggplot2 and displayed through plotly.

mod_visualizations_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- bslib::card(
    bslib::card_header("Chart builder"),
    bslib::card_body(
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(ns("type"), "Chart type",
          choices = c("Histogram", "Density", "Box plot", "Bar chart", "Scatter", "Correlation heatmap", "Time series"))),
        shiny::column(2, shiny::uiOutput(ns("x_ui"))),
        shiny::column(2, shiny::uiOutput(ns("y_ui"))),
        shiny::column(2, shiny::uiOutput(ns("fill_ui"))),
        shiny::column(3, shiny::uiOutput(ns("extra_ui")))
      ),
      plotly::plotlyOutput(ns("plot"), height = "480px")
    )
  )
  ui
}

mod_visualizations_server <- function(id, dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- shiny::reactive({ dataset$version; dataset$current })

    num_cols <- shiny::reactive({
      shiny::req(df())
      names(df())[vapply(df(), is.numeric, logical(1))]
    })
    cat_cols <- shiny::reactive({
      shiny::req(df())
      names(df())[vapply(df(), function(x) is.character(x) || is.factor(x), logical(1))]
    })
    date_cols <- shiny::reactive({
      shiny::req(df())
      names(df())[vapply(df(), function(x) inherits(x, c("Date", "POSIXct", "POSIXt", "POSIXlt")), logical(1))]
    })

    output$x_ui <- shiny::renderUI({
      shiny::req(df())
      choices <- switch(input$type,
        "Histogram" = num_cols(), "Density" = num_cols(), "Box plot" = num_cols(),
        "Bar chart" = c(cat_cols(), num_cols()), "Scatter" = num_cols(),
        "Time series" = date_cols(),
        c(num_cols(), cat_cols()))
      shiny::selectInput(ns("x"), "X variable", choices = choices)
    })
    output$y_ui <- shiny::renderUI({
      if (!(input$type %in% c("Scatter", "Time series"))) return(NULL)
      shiny::selectInput(ns("y"), "Y variable", choices = num_cols())
    })
    output$fill_ui <- shiny::renderUI({
      if (!(input$type %in% c("Box plot", "Bar chart", "Scatter", "Density"))) return(NULL)
      shiny::selectInput(ns("fill"), "Colour by (optional)", choices = c("(none)", cat_cols()))
    })
    output$extra_ui <- shiny::renderUI({
      if (input$type != "Time series") return(NULL)
      shiny::selectInput(ns("grain"), "Aggregate by",
        choices = c("day", "week", "month"), selected = "day")
    })

    build_plot <- function() {
      shiny::req(df(), input$x)
      t <- input$type; x <- input$x
      fill <- if (!is.null(input$fill) && input$fill != "(none)") input$fill else NULL
      p <- switch(t,
        Histogram = ggplot2::ggplot(df(), ggplot2::aes_string(x = x)) +
          ggplot2::geom_histogram(bins = 30, fill = "#345f9c", color = "white"),
        Density = ggplot2::ggplot(df(), ggplot2::aes_string(x = x)) +
          ggplot2::geom_density(fill = "#345f9c", alpha = .5),
        `Box plot` = {
          if (is.null(fill)) ggplot2::ggplot(df(), ggplot2::aes(x = "", y = .data[[x]])) +
            ggplot2::geom_boxplot(fill = "#345f9c") +
            ggplot2::labs(x = NULL)
          else ggplot2::ggplot(df(), ggplot2::aes(x = .data[[input$fill]], y = .data[[x]], fill = .data[[input$fill]])) +
            ggplot2::geom_boxplot()
        },
        `Bar chart` = {
          dat <- df()
          counts <- dplyr::count(dat, .bar = as.character(dat[[x]]))
          top <- dplyr::slice_max(counts, n, n = 25)
          ggplot2::ggplot(top, ggplot2::aes(x = stats::reorder(.bar, n), y = n)) +
            ggplot2::geom_col(fill = "#345f9c") +
            ggplot2::coord_flip() +
            ggplot2::labs(x = x, y = "count")
        },
        Scatter = {
          shiny::req(input$y)
          a <- ggplot2::aes(x = .data[[x]], y = .data[[input$y]])
          if (!is.null(fill)) a <- ggplot2::aes(x = .data[[x]], y = .data[[input$y]], color = .data[[input$fill]])
          ggplot2::ggplot(df(), a) + ggplot2::geom_point(alpha = .7)
        },
        `Correlation heatmap` = {
          m <- correlation_matrix(df(), method = "pearson")
          shiny::req(m)
          pairs <- expand.grid(Var1 = colnames(m), Var2 = colnames(m), stringsAsFactors = FALSE)
          pairs$value <- as.vector(m[cbind(match(pairs$Var1, colnames(m)), match(pairs$Var2, colnames(m)))])
          ggplot2::ggplot(pairs, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
            ggplot2::geom_tile() +
            ggplot2::scale_fill_gradient2(low = "#d64f46", mid = "white", high = "#345f9c", limits = c(-1, 1), na.value = "grey90", name = "r")
        },
        `Time series` = {
          shiny::req(input$y, input$grain)
          d <- df()
          per <- as.Date(cut(as.Date(d[[x]]), input$grain))
          agg <- stats::aggregate(.v ~ per, data = data.frame(per = per, .v = as.numeric(d[[input$y]])),
                                  FUN = function(z) sum(z, na.rm = TRUE))
          ggplot2::ggplot(agg, ggplot2::aes(x = per, y = .v)) +
            ggplot2::geom_line(color = "#345f9c") + ggplot2::geom_point(color = "#345f9c") +
            ggplot2::labs(x = NULL, y = input$y, title = sprintf("Total %s by %s", input$y, input$grain))
        }
      )
      p + mdi_theme()
    }

    output$plot <- plotly::renderPlotly({
      p <- shiny_try(build_plot(), context = "chart")
      shiny::req(p)
      plotly::ggplotly(p)
    })
  })
}
