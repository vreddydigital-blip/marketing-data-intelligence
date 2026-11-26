# Central error handling: converts failures into friendly UI messages and logs
# technical details (without dataset contents or stack traces in the prod UI).

friendly_error <- function(e, context = "operation") {
  msg <- conditionMessage(e)
  mdi_log_error(paste0("failure during ", context, ": ", msg))
  msg
}

try_friendly <- function(expr, context = "operation", fallback = character(0)) {
  tryCatch(expr, error = function(e) {
    msg <- friendly_error(e, context)
    if (length(fallback)) fallback else msg
  })
}

need_data <- function(df, min_rows = 1) {
  if (is.null(df)) {
    return("No dataset loaded. Upload data in the 'Data Upload' tab or load a sample dataset.")
  }
  if (nrow(df) < min_rows) {
    return(sprintf("The dataset has only %d row(s); at least %d are required for this analysis.",
                   nrow(df), min_rows))
  }
  NULL
}

shiny_try <- function(expr, context = "operation") {
  tryCatch(
    expr,
    error = function(e) {
      msg <- friendly_error(e, context)
      shiny::showNotification(
        shiny::HTML(sprintf("<b>Something went wrong during the %s.</b><br>%s",
                            esc(context), esc(msg))),
        type = "error", duration = 8)
      NULL
    }
  )
}
