# Central application logging. Never logs dataset contents or secrets, only
# events, sizes, and column counts.

mdi_log_level <- function() {
  lvl <- toupper(Sys.getenv("MDI_LOG_LEVEL", unset = "INFO"))
  match(lvl, c("DEBUG", "INFO", "WARN", "ERROR"), nomatch = 2L)
}

mdi_log_dir <- function() {
  d <- file.path(mdi_data_dir(), "logs")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

mdi_log_line <- function(level, message) {
  if (match(level, c("DEBUG", "INFO", "WARN", "ERROR"), nomatch = 2L) < mdi_log_level()) return(invisible(NULL))
  line <- sprintf("%s [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, message)
  tryCatch(cat(line, "\n", sep = "", file = file.path(mdi_log_dir(), "app.log"), append = TRUE),
           error = function(e) NULL)
  if (level %in% c("WARN", "ERROR")) message(line)
  invisible(NULL)
}

mdi_log_debug <- function(...) mdi_log_line("DEBUG", paste0(...))
mdi_log_info <- function(...) mdi_log_line("INFO", paste0(...))
mdi_log_warn <- function(...) mdi_log_line("WARN", paste0(...))
mdi_log_error <- function(...) mdi_log_line("ERROR", paste0(...))
