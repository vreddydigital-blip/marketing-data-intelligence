# Windows Installation Guide (Windows 10, 8 GB RAM, Core i5, SSD)

## 1. Install R

1. Go to https://cran.r-project.org/bin/windows/base/
2. Download and run `R-4.x.x-win.exe`. Accept the defaults (installs to
   `C:\Program Files\R\R-4.x.x`).
3. R works fine without admin rights if you install to your user folder.

## 2. Install RStudio (optional but recommended)

1. Download from https://posit.co/download/rstudio-desktop/ and install.
2. Open RStudio once; it will find your R installation.

## 3. Get the project

Unzip or `git clone` this repository, e.g. to `C:\projects\marketing-data-intelligence`.

## 4. Install packages

Open the project folder in RStudio and run in the R console:

```r
install.packages(c(
  "shiny", "bslib", "shinyWidgets", "shinyjs",
  "tidyverse", "readxl", "janitor", "data.table",
  "plotly", "DT", "broom", "rstatix", "car",
  "tidymodels", "ranger", "glmnet",
  "forecast", "fable", "tsibble",
  "openxlsx", "writexl", "DBI", "RSQLite",
  "rmarkdown", "testthat", "jsonlite", "renv", "fs"
))
```

On an i5 with an SSD this takes roughly 10–20 minutes. RTools is not needed:
all listed packages ship pre-compiled Windows binaries.

## 5. Dependency snapshots with renv (optional)

`renv.lock` records the exact tested package versions. To adopt it:

```r
install.packages("renv")
renv::init()          # creates a project library
renv::restore()       # installs versions from renv.lock
```

## 6. Database

Nothing to install: the app uses SQLite via the RSQLite package and creates
`data/marketing_intelligence.sqlite` on first run.

## 7. Start the application

Either:
- RStudio: open `app.R` and press **Run App**, or
- Command line: `Rscript run_app.R`

The console shows `http://127.0.0.1:3838` — open it in your browser.

## 8. Configuration

Copy `.Renviron.example` to `.Renviron` and adjust:

- `MDI_MAX_UPLOAD_MB` (default 50)
- `MDI_DATA_DIR` (default `data` inside the project)
- `MDI_LOG_LEVEL` (DEBUG, INFO, WARN, ERROR)

## 9. Troubleshooting

| Problem | Fix |
| --- | --- |
| `there is no package called 'shiny'` | Re-run step 4 in the same R that RStudio uses (`.libPaths()` shows where) |
| Upload rejected despite small file | Check `MDI_MAX_UPLOAD_MB`; the limit also appears on the upload card |
| HTML report fails | Ensure `rmarkdown` + `pandoc` are installed (pandoc ships with RStudio) |
| PDF report says toolchain unavailable | Install TinyTeX: `tinytex::install_tinytex()`, or print the HTML to PDF from the browser |
| Excel import errors | Re-save the sheet as a plain table (no merged cells in the header row) |
| Port 3838 busy | Set `MDI_PORT=3839` in `.Renviron` before starting |
| Slow with very large files | Keep files under ~200 MB; the app warns beyond practical limits and samples for clustering |
