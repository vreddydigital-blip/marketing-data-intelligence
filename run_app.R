#!/usr/bin/env Rscript
# Marketing Data Intelligence launcher.
# Usage: Rscript run_app.R          (serves at http://127.0.0.1:3838)
#        or open this folder in RStudio and press "Run App"

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The 'shiny' package is not installed. See docs/INSTALL_WINDOWS.md for setup instructions.",
       call. = FALSE)
}

args <- commandArgs(trailingOnly = FALSE)
file_flag <- grep("^--file=", args, value = TRUE)
if (length(file_flag)) setwd(dirname(normalizePath(sub("^--file=", "", file_flag[1]))))

port <- as.integer(Sys.getenv("MDI_PORT", unset = "3838"))
host <- Sys.getenv("MDI_HOST", unset = "127.0.0.1")

message(sprintf("Starting Marketing Data Intelligence at http://%s:%d ...", host, port))
shiny::runApp(appDir = ".", host = host, port = port, launch.browser = interactive())
