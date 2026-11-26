# Statistical analysis module: guided tests with hypotheses, results tables,
# effect sizes, assumption warnings, and significance vs practical relevance.

mod_statistics_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header("Choose an analysis"),
      bslib::card_body(
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(ns("method"), "Method", choices = c(
              "Correlation (Pearson)", "Correlation (Spearman)", "Chi-square test",
              "Independent t-test", "Paired t-test", "Mann-Whitney test",
              "One-way ANOVA", "Kruskal-Wallis test", "Linear regression", "Multiple linear regression"
            ))
          ),
          shiny::column(8, shiny::uiOutput(ns("args_ui")))
        ),
        shiny::actionButton(ns("run"), "Run analysis", class = "btn-primary"),
        bslib::card_footer(shiny::em("Statistical significance (p < 0.05) is not the same as practical business significance; effect sizes are shown to judge magnitude. Correlation never implies causation."))
      )
    ),
    shiny::uiOutput(ns("hypotheses")),
    shiny::uiOutput(ns("results_ui"))
  )
}

mod_statistics_server <- function(id, dataset, results_out = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- shiny::reactive({ dataset$version; dataset$current })
    num_cols <- shiny::reactive({ shiny::req(df()); names(df())[vapply(df(), is.numeric, logical(1))] })
    cat_cols <- shiny::reactive({ shiny::req(df()); names(df())[vapply(df(), function(x) is.character(x) || is.factor(x), logical(1))] })
    last_result <- shiny::reactiveVal(NULL)

    output$args_ui <- shiny::renderUI({
      shiny::req(df())
      m <- input$method
      if (grepl("Correlation", m)) {
        shiny::tagList(
          shiny::selectInput(ns("v1"), "Variable 1", choices = num_cols()),
          shiny::selectInput(ns("v2"), "Variable 2", choices = num_cols()))
      } else if (m %in% c("Chi-square test")) {
        shiny::tagList(
          shiny::selectInput(ns("g1"), "Variable 1 (categorical)", choices = cat_cols()),
          shiny::selectInput(ns("g2"), "Variable 2 (categorical)", choices = cat_cols()))
      } else if (m %in% c("Independent t-test", "Paired t-test", "Mann-Whitney test", "One-way ANOVA", "Kruskal-Wallis test")) {
        shiny::tagList(
          shiny::selectInput(ns("val"), "Value (numeric)", choices = num_cols()),
          shiny::selectInput(ns("grp"), "Group (categorical)", choices = cat_cols()))
      } else {
        shiny::tagList(
          shiny::selectInput(ns("tgt"), "Target (numeric)", choices = num_cols()),
          shiny::selectInput(ns("prs"), "Predictors", choices = names(df()), multiple = TRUE))
      }
    })

    shiny::observeEvent(input$run, {
      res <- shiny_try({
        shiny::req(df())
        m <- input$method
        out <- switch(m,
          "Correlation (Pearson)" = correlation_analysis(df(), input$v1, input$v2, "pearson"),
          "Correlation (Spearman)" = correlation_analysis(df(), input$v1, input$v2, "spearman"),
          "Chi-square test" = chi_square_test(df(), input$g1, input$g2),
          "Independent t-test" = t_test_contrast(df(), input$val, input$grp, paired = FALSE),
          "Paired t-test" = t_test_contrast(df(), input$val, input$grp, paired = TRUE),
          "Mann-Whitney test" = mann_whitney_test(df(), input$val, input$grp),
          "One-way ANOVA" = anova_test(df(), input$val, input$grp),
          "Kruskal-Wallis test" = kruskal_test(df(), input$val, input$grp),
          "Linear regression" = linreg_frame(df(), input$tgt, input$prs),
          "Multiple linear regression" = linreg_frame(df(), input$tgt, input$prs)
        )
        shiny::validate(shiny::need(nrow(out) > 0, "No result returned."))
        out
      }, context = "statistical analysis")
      if (!is.null(res)) {
        last_result(res)
        if (!is.null(results_out)) results_out(res)
      }
    })

    output$hypotheses <- shiny::renderUI({
      shiny::req(input$method)
      key <- switch(input$method,
        "Correlation (Pearson)" = "corr_pearson",
        "Correlation (Spearman)" = "corr_spearman",
        "Chi-square test" = "chi_square",
        "Independent t-test" = "t_test",
        "Paired t-test" = "t_test_paired",
        "Mann-Whitney test" = "mann_whitney",
        "One-way ANOVA" = "anova",
        "Kruskal-Wallis test" = "kruskal",
        "regression")
      h <- TEST_HYPOTHESES[[key]]
      shiny::div(class = "alert alert-light",
        shiny::strong("H0: "), shiny::span(h$H0), shiny::br(),
        shiny::strong("H1: "), shiny::span(h$H1))
    })

    output$results_ui <- shiny::renderUI({
      res <- last_result()
      if (is.null(res)) return(shiny::em("Run an analysis to see results."))
      tab <- keep_display_cols(res)
      warning_html <- if ("warning" %in% names(res) && any(nzchar(res$warning[1] %||% ""))) {
        shiny::div(class = "alert alert-warning mt-2", esc(res$warning[1]))
      } else if (!is.null(res$diagnostics)) {
        shiny::div(class = "alert alert-warning mt-2",
                   esc(res$diagnostics$vif_warning), shiny::br(), esc(res$diagnostics$shapiro_note))
      } else NULL
      shiny::tagList(
        bslib::card(bslib::card_header("Test result"), bslib::card_body(DT::DTOutput(ns("res_table")))),
        warning_html,
        shiny::p(class = "text-muted",
          shiny::HTML("Interpretation: a p-value below 0.05 means the result is unlikely under the null hypothesis in <i>this</i> dataset. Judge practical relevance with the effect size, not the p-value alone.")),
      )
    })

    output$res_table <- DT::renderDT({
      res <- last_result()
      shiny::req(res)
      r <- keep_display_cols(res)
      if (inherits(r, "mdi_multi")) {
        output$res_table_fit <- DT::renderDT({ DT::datatable(r$model_fit, options = list(dom = "t"), rownames = FALSE) })
        DT::datatable(r$coefficients, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
      } else {
        DT::datatable(r, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
      }
    })

    keep_display_cols <- function(res) {
      if (!is.null(res$coefficients)) {
        coef_tab <- round_df(as.data.frame(res$coefficients), 4)
        sel <- intersect(c("r.squared", "adj.r.squared", "sigma", "statistic", "p.value", "nobs"), names(res$glance))
        model_fit <- round_df(as.data.frame(res$glance)[, sel, drop = FALSE], 4)
        out <- list(coefficients = coef_tab, model_fit = model_fit)
        class(out) <- "mdi_multi"
        return(out)
      }
      round_df(as.data.frame(res), 4)
    }

    linreg_frame <- function(d, tgt, prs) {
      if (is.null(prs) || length(prs) == 0) stop("Select at least one predictor.", call. = FALSE)
      linear_regression(d, tgt, prs)
    }
  })
}
