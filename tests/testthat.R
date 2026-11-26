# Test runner for the Marketing Data Intelligence application.
# Run from anywhere with: Rscript tests/testthat.R

find_app_root <- function() {
  dir <- getwd()
  for (i in 1:5) {
    if (file.exists(file.path(dir, "app.R"))) return(normalizePath(dir, winslash = "/"))
    dir <- dirname(dir)
  }
  stop("Could not locate the application root (no app.R found walking up).", call. = FALSE)
}

root <- find_app_root()
data_dir <- file.path(tempdir(), paste0("mdi_tests_", format(Sys.time(), "%H%M%S")))
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(MDI_DATA_DIR = data_dir)
options(mdi.app_root = root)

suppressPackageStartupMessages({
  library(shiny)
  library(testthat)
})
source(file.path(root, "global.R"), local = globalenv())

results <- test_dir(file.path(root, "tests", "testthat"), reporter = "summary")
cat("All tests passed.\n")
quit(save = "no", status = 0L)
