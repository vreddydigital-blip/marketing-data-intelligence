# Report generation: Excel workbook, CSV export, and R Markdown HTML (with
# optional PDF when a LaTeX toolchain is present). All rendering happens in a
# temporary directory; on failure a clear error is raised, never silent.

qc <- function(x, default = "-") {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(as.character(x[1]))) default else as.character(x[1])
}

build_excel_report <- function(state, path) {
  df <- state$df
  if (is.null(df)) stop("No dataset is loaded; nothing to report.", call. = FALSE)
  wb <- openxlsx::createWorkbook()
  info <- state$info
  add_sheet <- function(name, data, header = NULL) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, data)
    openxlsx::setColWidths(wb, name, cols = seq_len(max(1, ncol(data))), widths = "auto")
  }

  # 1. Summary
  overview <- data.frame(
    item = c("Dataset", "Rows", "Columns", "Size (MB)", "Duplicate rows", "Data quality score", "Generated"),
    value = c(qc(state$name), nrow(df), ncol(df),
              round(if (is.null(info)) NA else info$size_mb, 2),
              if (is.null(info)) NA else info$duplicate_rows,
              qc(state$quality$score), format(Sys.time(), "%Y-%m-%d %H:%M %Z"))
  )
  add_sheet("Summary", overview)

  # 2. Data quality
  if (!is.null(state$quality)) {
    add_sheet("Quality_breakdown", state$quality$breakdown)
    add_sheet("Quality_issues", state$quality$issues)
  }

  # 3. KPI summary
  if (!is.null(state$kpi_summary)) {
    ks <- state$kpi_summary
    kpi_tab <- data.frame(
      KPI = names(k_list_out(ks)),
      value = unlist(k_list_out(ks)),
      row.names = NULL
    )
    add_sheet("KPIs", kpi_tab)
    if (!is.null(state$mapping$campaign)) {
      add_sheet("Campaigns", grouped_performance(df, state$mapping, state$mapping$campaign))
    }
    if (!is.null(state$mapping$channel)) {
      add_sheet("Channels", grouped_performance(df, state$mapping, state$mapping$channel))
    }
  }

  # 4. Statistics results
  if (!is.null(state$stats_results) && length(state$stats_results)) {
    all_stats <- do.call(rbind, lapply(names(state$stats_results), function(nm) {
      r <- state$stats_results[[nm]]
      data.frame(analysis = nm, test = r$test, statistic = r$statistic[1],
                 p_value = r$p_value[1], effect_size = r$effect_size[1])
    }))
    add_sheet("Statistics", all_stats)
  }

  # 5. Model evaluation
  if (!is.null(state$model_metrics) && nrow(state$model_metrics) > 0) {
    add_sheet("Models", state$model_metrics)
  }

  # 6. Forecast results
  if (!is.null(state$forecast_table) && nrow(state$forecast_table) > 0) {
    add_sheet("Forecast", state$forecast_table)
  }

  # 7. Insights
  if (!is.null(state$insights) && nrow(state$insights) > 0) {
    add_sheet("Insights", state$insights)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

k_list_out <- function(ks) {
  out <- ks$kpis
  out[vapply(out, function(z) !is.null(z) && !is.na(z), logical(1))]
}

build_csv_report <- function(state, path, dataset_only = TRUE) {
  df <- state$df
  if (is.null(df)) stop("No dataset is loaded; nothing to export.", call. = FALSE)
  readr::write_csv(df, path)
  invisible(path)
}

find_template <- function(name = "report_html.Rmd") {
  p <- file.path(app_root_dir(), "reports", "templates", name)
  if (!file.exists(p)) stop(sprintf("Report template '%s' is missing from the deployment.", name), call. = FALSE)
  p
}

build_html_report <- function(state, path) {
  ctx <- build_report_context(state)
  tpl <- find_template("report_html.Rmd")
  out_dir <- file.path(app_root_dir(), "data", "reports")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ok <- tryCatch({
    suppressWarnings(rmarkdown::render(tpl, output_file = basename(path), output_dir = out_dir,
                                       params = list(ctx = ctx), envir = new.env(parent = globalenv()),
                                       quiet = TRUE))
    TRUE
  }, error = function(e) {
    mdi_log_error(paste0("HTML render failed: ", conditionMessage(e)))
    stop(sprintf("HTML report generation failed: %s", conditionMessage(e)), call. = FALSE)
  })
  fs::file_copy(ok_path(out_dir, basename(path)), path, overwrite = TRUE)
  invisible(path)
}

ok_path <- function(dir, file) {
  p <- file.path(dir, file)
  if (!file.exists(p)) stop("Rendered report file not found after rendering.", call. = FALSE)
  p
}

pdf_toolchain_available <- function() {
  nzchar(Sys.which("pdflatex")) || (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex())
}

pdf_error_if_unavailable <- function() {
  if (!pdf_toolchain_available()) {
    stop(paste0("PDF generation needs a LaTeX toolchain (e.g. install TinyTeX with tinytex::install_tinytex()). ",
                "Until then, use the HTML report and print it to PDF from your browser."), call. = FALSE)
  }
}

build_report_context <- function(state) {
  list(
    dataset_name = qc(state$name),
    generated = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
    n_rows = if (is.null(state$df)) 0 else nrow(state$df),
    n_cols = if (is.null(state$df)) 0 else ncol(state$df),
    quality_score = state$quality$score %||% NA_real_,
    kpis = if (is.null(state$kpi_summary)) list() else unlist_kpis(state$kpi_summary),
    insights = state$insights,
    stats_results = state$stats_results,
    model_metrics = state$model_metrics,
    forecast_table = state$forecast_table,
    df = state$df
  )
}

unlist_kpis <- function(ks) {
  out <- ks$kpis
  out <- out[vapply(out, function(z) !is.null(z) && !is.na(z), logical(1))]
  out
}
