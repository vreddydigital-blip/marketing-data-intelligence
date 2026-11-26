#!/usr/bin/env Rscript
# Wake a sleeping shinyapps.io instance and wait until the app is serving.
#
# Usage (from the project root):
#   Rscript tools/wake_app.R
#   Rscript tools/wake_app.R https://<account>.shinyapps.io/<app-name>/
#   Rscript tools/wake_app.R --timeout=180
#
# The URL can also be provided through the MDI_APP_URL environment variable.
# Priority: command-line URL argument > MDI_APP_URL > built-in default.
#
# How it works: a single HTTP GET to the app's root URL is enough to make
# shinyapps.io start the instance. Cold starts take roughly 15-60 seconds on
# the free tier. While the instance is starting, the provider serves a generic
# loading page instead of the app, so this script keeps polling until the
# served page is the real application (title tag check), then reports how long
# the wake-up took.
#
# Note: this is an on-demand wake-up utility, not a keep-alive pinger. Keeping
# the app awake 24/7 would consume the available active hours within days and
# is treated as artificial traffic by the hosting provider.

url_arg <- Sys.getenv("MDI_APP_URL", "https://vreddy-analytics.shinyapps.io/marketing-data-intelligence/")
timeout_s <- 180L
marker <- "Marketing Data Intelligence"

args <- commandArgs(trailingOnly = TRUE)
for (a in args) {
  if (grepl("^--timeout=[0-9]+$", a)) {
    timeout_s <- as.integer(sub("^--timeout=", "", a))
  } else if (grepl("^https?://", a)) {
    url_arg <- a
  }
}
if (is.na(timeout_s) || timeout_s < 10L) timeout_s <- 10L
if (timeout_s > 900L) timeout_s <- 900L

fetch_body <- function(u) {
  con <- url(u, open = "rb", method = "libcurl",
             headers = c("User-Agent" = "mdi-wake/1.0"))
  on.exit(if (!is.null(con)) try(close(con), silent = TRUE), add = TRUE)
  raw <- readBin(con, what = "raw", n = 3e6)
  rawToChar(raw)
}

sleeping <- function(body) {
  if (is.null(body) || !nzchar(body)) return(TRUE)
  !(grepl(paste0("<title>[^<]*", marker, "[^<]*</title>"), body) ||
    grepl(marker, body, fixed = TRUE))
}

cat("Waking:", url_arg, "(timeout", timeout_s, "s)\n")
started <- Sys.time()
attempt <- 0L
ready <- FALSE
repeat {
  attempt <- attempt + 1L
  body <- suppressWarnings(tryCatch(fetch_body(url_arg), error = function(e) NULL))
  if (!sleeping(body)) {
    ready <- TRUE
    break
  }
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  if (elapsed >= timeout_s) break
  cat(sprintf("  attempt %d: instance still starting (%.0fs elapsed); retrying in 10s\n",
              attempt, elapsed))
  Sys.sleep(10)
}

elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
if (ready) {
  cat(sprintf("READY: app is serving (took %.0fs, %d attempt(s))\n", elapsed, attempt))
  invisible(quit(status = 0L))
} else {
  cat(sprintf("NOT READY after %.0fs and %d attempt(s): instance did not respond with the app page.\n", elapsed, attempt))
  cat("Check the app dashboard at https://www.shinyapps.io/admin/ and recent deploy logs.\n")
  invisible(quit(status = 1L))
}
