# Machine learning module: guided supervised workflow + clustering.
# Training is triggered explicitly by a button (never reruns on data change);
# preprocessing is fitted on training data only; seeds are fixed.

mod_machine_learning_ui <- function(id) {
  ns <- shiny::NS(id)
  ui <- shiny::tagList(
    bslib::navset_tab(
      bslib::nav_panel("Supervised learning",
        bslib::card(
          bslib::card_header("Setup"),
          bslib::card_body(
            shiny::fluidRow(
              shiny::column(3, shiny::selectInput(ns("target"), "Target variable", choices = NULL)),
              shiny::column(3, shiny::selectInput(ns("features"), "Predictors", choices = NULL, multiple = TRUE, size = 8, selectize = FALSE)),
              shiny::column(2, shiny::selectInput(ns("model"), "Model", choices = NULL)),
              shiny::column(2, shinyWidgets::numericInputIcon(ns("seed"), "Seed", value = 42, min = 1, max = 99999, icon = shiny::icon("seedling"))),
              shiny::column(2, shiny::p(shiny::uiOutput(ns("problem_type"))))
            ),
            shiny::actionButton(ns("train"), "Train model", class = "btn-primary"),
            shiny::uiOutput(ns("warnings_ui"))
          )
        ),
        bslib::layout_column_wrap(width = 1/2, fill = FALSE,
          bslib::card(bslib::card_header("Test-set metrics"), bslib::card_body(DT::DTOutput(ns("metrics")))),
          bslib::card(bslib::card_header("Feature importance"), bslib::card_body(plotly::plotlyOutput(ns("importance"), height = "320px")))
        ),
        bslib::card(bslib::card_header("Confusion matrix"), bslib::card_body(DT::DTOutput(ns("confmat")), shiny::verbatimTextOutput(ns("implem"))))
      ),
      bslib::nav_panel("Clustering",
        bslib::card(
          bslib::card_header("K-means clustering"),
          bslib::card_body(
            shiny::fluidRow(
              shiny::column(4, shiny::selectInput(ns("clu_features"), "Features (numeric preferred)", choices = NULL, multiple = TRUE)),
              shiny::column(2, shiny::numericInput(ns("clu_k"), "Number of clusters", value = 3, min = 2, max = 15)),
              shiny::column(2, shiny::numericInput(ns("clu_seed"), "Seed", value = 42, min = 1)),
              shiny::column(2, shiny::actionButton(ns("cluster_btn"), "Run clustering", class = "btn-primary margin-top"))
            ),
            plotly::plotlyOutput(ns("cluster_plot"), height = "380px"),
            DT::DTOutput(ns("cluster_profiles"))
          )
        )
      )
    ),
    shiny::HTML('<div class="alert alert-secondary mt-2"><small>Models estimate patterns from past data; predictions are never guaranteed outcomes. Preprocessing is fitted on training data only to prevent leakage.</small></div>')
  )
  ui
}

mod_machine_learning_server <- function(id, dataset, model_metrics_out = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- shiny::reactive({ dataset$version; dataset$current })
    last_run <- shiny::reactiveVal(NULL)
    validation_warnings <- shiny::reactiveVal(character(0))

    shiny::observe({
      shiny::req(df())
      cols <- names(df())
      shiny::updateSelectInput(session, "target", choices = cols)
      shiny::updateSelectInput(session, "features", choices = setdiff(cols, input$target %||% ""), selected = setdiff(cols, c(input$target, dataset$info$date_cols[1])))
      shiny::updateNumericInput(session, "clu_k", max = min(15, max(2, nrow(df()) - 1)))
    })
    shiny::observe({
      t <- input$target
      shiny::req(df(), t)
      shiny::updateSelectInput(session, "features", choices = setdiff(names(df()), t))
    })

    problem_type <- shiny::reactive({
      shiny::req(input$target)
      tryCatch(detect_problem_type(df(), input$target), error = function(e) NULL)
    })

    output$problem_type <- shiny::renderUI({
      pt <- problem_type()
      if (is.null(pt)) return(shiny::em("Select a target."))
      label <- c(regression = "Regression", classification_binary = "Binary classification",
                 classification_multiclass = "Multiclass classification")[pt]
      shiny::HTML(sprintf('<span class="badge bg-info">%s</span>', label))
    })

    engine_choices <- shiny::reactive({
      pt <- problem_type()
      shiny::req(pt)
      if (pt == "regression") c("Linear regression", "Ridge/Lasso (glmnet)", "Random forest (ranger)")
      else if (pt == "classification_binary") c("Logistic regression", "Regularised logistic (glmnet)", "Random forest (ranger)")
      else c("Random forest (ranger)", "Regularised logistic (glmnet)")
    })
    shiny::observe({
      shiny::req(problem_type())
      shiny::updateSelectInput(session, "model", choices = engine_choices())
    })

    shiny::observeEvent(input$train, {
      pt <- problem_type()
      ok <- shiny_try({
        shiny::req(df(), input$target, length(input$features) >= 1)
        warns <- validate_ml_problem(df(), input$target, input$features, pt)
        if (length(warns) > 0 && any(grepl("ID-like|constant|fewer than 2", warns))) {
          validation_warnings(warns)
          stop(sprintf("Cannot train: %s", paste(warns, collapse = "; ")), call. = FALSE)
        }
        engine <- ml_engines[[pt]][[which(vapply(ml_engines[[pt]], function(e) e$name == input$model, logical(1)))[1]]]
        shiny::withProgress(message = "Training model...", value = 0.4, {
          set.seed(input$seed)
          res <- train_single_model(df(), input$target, input$features, pt, engine, seed = input$seed)
        })
        validation_warnings(warns)
        # metrics table
        run <- list(type = pt, engine_name = engine$name, result = res,
                    metrics = as.data.frame(shaped_metrics(res)))
        last_run(run)
        if (!is.null(model_metrics_out)) model_metrics_out(metrics_summary_frame(run))
        mdi_db_log_analysis(NULL, "machine_learning",
          sprintf("%s / %s / seed %d", pt, engine$name, input$seed))
        shiny::showNotification("Model trained.", type = "message", duration = 4)
        ""
      }, context = "model training")
      invisible(ok)
    })

    shaped_metrics <- function(res) {
      m <- res$metrics_test
      m
    }

    metrics_summary_frame <- function(run) {
      est <- as.data.frame(run$result$metrics_test)
      est$.metric <- as.character(est$.metric)
      fmt <- paste0(sprintf("%.3f", est$.estimate), collapse = ", ")
      data.frame(model = run$engine_name,
                 summary = paste(sprintf("%s=%s", est$.metric, sprintf("%.3f", est$.estimate)), collapse = ","),
                 stringsAsFactors = FALSE)
    }

    output$metrics <- DT::renderDT({
      run <- last_run()
      shiny::req(run)
      m <- as.data.frame(run$result$metrics_test)
      names(m) <- c("metric", "estimate", "estimator")
      DT::datatable(m, options = list(dom = "t"), rownames = FALSE)
    })

    output$importance <- plotly::renderPlotly({
      run <- last_run()
      shiny::req(run)
      imp <- regression_feature_importance(run$result)
      shiny::req(!is.null(imp) && nrow(imp) > 0)
      head_n <- utils::head(imp, 15)
      p <- ggplot2::ggplot(head_n, ggplot2::aes(x = importance, y = stats::reorder(feature, importance))) +
        ggplot2::geom_col(fill = "#345f9c") +
        ggplot2::labs(x = "Importance", y = NULL, title = "Top features (test-split model)") + mdi_theme()
      plotly::ggplotly(p)
    })

    output$confmat <- DT::renderDT({
      run <- last_run()
      shiny::req(run)
      cm <- run$result$confusion
      shiny::req(!is.null(cm))
      t <- as.data.frame(cm$table)
      DT::datatable(t, options = list(dom = "t"), rownames = FALSE)
    })

    output$implem <- shiny::renderText({
      run <- last_run()
      if (is.null(run)) return("")
      sprintf("Trained on %d rows, tested on %d rows (seed=%d). Elastic-net penalty fixed at 0.1 for regularised models; tune manually for production use.",
              run$result$n_train, run$result$n_test, run$result$seed)
    })

    output$warnings_ui <- shiny::renderUI({
      w <- validation_warnings()
      if (!length(w)) return(NULL)
      shiny::div(class = "alert alert-warning mt-2",
                 shiny::HTML(paste("<ul>", paste0("<li>", esc(w), "</li>", collapse = ""), "</ul>")))
    })

    # ---- clustering ----
    clu_result <- shiny::reactiveVal(NULL)
    shiny::observe({
      shiny::req(df())
      shiny::updateSelectInput(session, "clu_features", choices = names(df()),
                               selected = dataset$info$numeric_cols[seq_len(min(4, length(dataset$info$numeric_cols)))])
    })
    shiny::observeEvent(input$cluster_btn, {
      res <- shiny_try({
        shiny::req(df(), length(input$clu_features) >= 1)
        shiny::withProgress(message = "Clustering...", value = 0.4, {
          run_clustering(df(), input$clu_features, k = input$clu_k, seed = input$clu_seed)
        })
      }, context = "clustering")
      if (!is.null(res)) clu_result(res)
    })
    output$cluster_plot <- plotly::renderPlotly({
      res <- clu_result()
      shiny::req(res)
      pc <- stats::prcomp(res$X)$x[, seq_len(min(2, ncol(res$X)))]
      dat <- data.frame(pc1 = pc[, 1], pc2 = if (ncol(pc) > 1) pc[, 2] else pc[, 1], cluster = factor(res$kmeans$cluster))
      p <- ggplot2::ggplot(dat, ggplot2::aes(x = pc1, y = pc2, color = cluster)) +
        ggplot2::geom_point(alpha = .7) +
        ggplot2::labs(title = sprintf("K-means clusters (first 2 PCs). Silhouette: %s",
                                      if (is.na(res$silhouette)) "n/a" else sprintf("%.3f", res$silhouette)),
                      x = "PC1", y = "PC2", color = "Cluster") + mdi_theme()
      plotly::ggplotly(p)
    })
    output$cluster_profiles <- DT::renderDT({
      res <- clu_result()
      shiny::req(res)
      DT::datatable(res$profiles, options = list(dom = "t", scrollX = TRUE), rownames = FALSE,
                    caption = "Cluster profiles (means of numeric features)")
    })
  })
}
