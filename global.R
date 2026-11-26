# Marketing Data Intelligence - global setup.
# Sources all utils, services, and modules and sets safe global options.
#
# Idempotency guard: global.R is sourced directly by tests and by
# R/zzz_mdi_bootstrap.R (which runs when Shiny's R/ autoloading is active,
# the default since shiny 1.14, where global.R itself is no longer evaluated).

if (!identical(getOption("mdi.bootstrapped"), TRUE) &&
    !identical(getOption("mdi.bootstrapping"), TRUE)) {
  options(mdi.bootstrapping = TRUE)

  app_root_dir <- function() {
    getOption("mdi.app_root", default = {
      args <- commandArgs(trailingOnly = FALSE)
      file_flag <- grep("^--file=", args, value = TRUE)
      if (length(file_flag)) {
        dirname(normalizePath(sub("^--file=", "", file_flag[1]), winslash = "/", mustWork = FALSE))
      } else if (nzchar(Sys.getenv("SHINY_SERVER_APP_DIR"))) {
        Sys.getenv("SHINY_SERVER_APP_DIR")
      } else {
        normalizePath(getwd(), winslash = "/", mustWork = FALSE)
      }
    })
  }

  # NOTE: database.R also defines app_root_dir; keep the option-based version here
  # stable and skip re-sourcing conflicts.
  source_if_exists <- function(path) {
    p <- file.path(app_root_dir(), path)
    if (!file.exists(p)) {
      warning(sprintf("Missing source file: %s (application may be incompletely deployed)", path), call. = FALSE)
      return(invisible(FALSE))
    }
    source(p, local = globalenv())
    invisible(TRUE)
  }

  base_paths <- c("utils", "services", "database", "R")
  for (d in base_paths) {
    files <- list.files(file.path(app_root_dir(), d), pattern = "\\.[rR]$", full.names = TRUE, recursive = TRUE)
    for (f in files) {
      if (basename(f) == "database.R") next
      source(f, local = globalenv())
    }
  }
  source(file.path(app_root_dir(), "database", "database.R"), local = globalenv())

  options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)

  tryCatch(mdi_db_init(), error = function(e) {
    mdi_log_error(paste0("database init failed: ", conditionMessage(e)))
  })

  options(mdi.bootstrapping = NULL, mdi.bootstrapped = TRUE)
}
