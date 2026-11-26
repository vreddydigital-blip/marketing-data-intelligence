# Deployment Guide

The app is a standard R Shiny application; any Shiny host works. Two
recommended paths, no paid APIs required.

## Option A: shinyapps.io (managed, has a free tier)

1. Create a free account at https://www.shinyapps.io (free tier exists but is
   subject to provider change — check current allowances; do not assume it
   remains free forever).
2. In R:
   ```r
   install.packages("rsconnect")
   rsconnect::setAccountInfo(name = "<your-name>",
                             token = Sys.getenv("SHINYAPPS_TOKEN"),        # from env var, do not hardcode
                             secret = Sys.getenv("SHINYAPPS_SECRET"))
   deployApp(appDir = ".", appName = "marketing-data-intelligence")
   ```
3. Notes:
   - Set `MDI_DATA_DIR` writable storage considerations: shinyapps.io rebuilds
     the instance, so SQLite/data files are **ephemeral**. Use the explicit
     "Save session" flow knowing data resets between deploys.
   - The 2-core/1-GB free instances are suitable for demo datasets, not bulk ETL.
   - Startup repos the renv.lock file: include it so deployment restores tested versions.

## Option B: ShinyProxy in Docker (self-hosted, free software)

1. Containerise:
   ```dockerfile
   FROM rocker/shiny:4.4.1
   RUN Rscript -e 'install.packages(c("shiny","bslib","shinyWidgets","shinyjs","tidyverse","readxl","janitor","data.table","plotly","DT","broom","rstatix","car","tidymodels","ranger","glmnet","forecast","openxlsx","writexl","DBI","RSQLite","rmarkdown"))'
   COPY . /srv/shiny-server/marketing-data-intelligence
   ```
   (Add `forecast` extras like fable/tsibble if used; rocker images include pandoc.)
2. Run ShinyProxy (open source) on a VM; it provides auth (LDAP/OAuth) and
   container-per-user isolation — recommended for anything user-facing.
3. Database: mount a volume for `MDI_DATA_DIR` so SQLite persists across
   restarts, or point `MDI_DB_ENGINE`/future PostgreSQL support at a managed DB.

## Environment configuration

| Variable | Default | Meaning |
| --- | --- | --- |
| `MDI_MAX_UPLOAD_MB` | 50 | upload limit |
| `MDI_DATA_DIR` | `data` | SQLite, logs, reports |
| `MDI_LOG_LEVEL` | INFO | DEBUG/INFO/WARN/ERROR |
| `MDI_DB_ENGINE` | sqlite | database backend |
| `MDI_PORT` / `MDI_HOST` | 3838 / 127.0.0.1 | listener (run_app.R) |

## Restart behaviour

- Shiny apps restart on idle (shinyapps.io) or container restart (ShinyProxy).
  Ephemeral state (in-memory dataset) is lost; saved sessions in SQLite
  survive when the data directory is on persistent storage.
- Logs append to `data/logs/app.log`; rotate or ship them with the platform.
