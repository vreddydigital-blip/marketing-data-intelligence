#!/usr/bin/env Rscript
# Publish the Marketing Data Intelligence app to shinyapps.io (or any
# rsconnect-compatible host such as Posit Connect).
#
# Credentials are read from environment variables - never hardcode them:
#   SHINYAPPS_NAME, SHINYAPPS_TOKEN, SHINYAPPS_SECRET
# (see .Renviron.example; create your account at https://www.shinyapps.io)
#
# Usage:
#   Rscript deploy.R                        # deploys to shinyapps.io
#   Rscript deploy.R my-app-name            # custom app name

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install rsconnect first: install.packages('rsconnect')", call. = FALSE)
}

account_name <- Sys.getenv("SHINYAPPS_NAME")
token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")
if (!nzchar(account_name) || !nzchar(token) || !nzchar(secret)) {
  stop("Set SHINYAPPS_NAME, SHINYAPPS_TOKEN, and SHINYAPPS_SECRET environment variables",
       " (copy .Renviron.example to .Renviron and fill them in).", call. = FALSE)
}

app_name <- if (length(commandArgs(TRUE))) {
  commandArgs(TRUE)[1]
} else {
  "marketing-data-intelligence"
}

root <- (function() {
  d <- getwd()
  for (i in 1:5) {
    if (file.exists(file.path(d, "app.R"))) return(normalizePath(d, winslash = "/"))
    d <- dirname(d)
  }
  stop("Run this script from inside the application directory.", call. = FALSE)
})()

message(sprintf("Deploying '%s' to shinyapps.io account '%s' ...", app_name, account_name))

rsconnect::setAccountInfo(name = account_name, token = token, secret = secret)
rsconnect::deployApp(
  appDir = root,
  appName = app_name,
  appTitle = "Marketing Data Intelligence",
  forceUpdate = TRUE
)

message("Deployed. Check the shinyapps.io dashboard for the application URL.")
message("Note: free-tier instances are 1 GB RAM / 25 active hours per month and")
message("data under MDI_DATA_DIR is ephemeral across deploys and idle restarts.")
