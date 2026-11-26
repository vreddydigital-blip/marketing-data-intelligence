# Database abstraction layer. SQLite for the initial version; the connection
# factory below is the only place that names the engine, so PostgreSQL can be
# swapped in later without touching callers. All queries are parameterised.

mdi_data_dir <- function() {
  d <- Sys.getenv("MDI_DATA_DIR", unset = "data")
  if (nzchar(d) && !grepl("^([A-Za-z]:[\\\\/]|/|\\\\\\\\)", d)) {
    # resolve relative paths against the application directory, not the cwd
    d <- file.path(app_root_dir(), d)
  }
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  normalizePath(d, winslash = "/", mustWork = FALSE)
}

app_root_dir <- function() {
  getOption("mdi.app_root", default = normalizePath(getwd(), winslash = "/", mustWork = FALSE))
}

mdi_db_path <- function() file.path(mdi_data_dir(), "marketing_intelligence.sqlite")

mdi_db_connect <- function() {
  drv <- Sys.getenv("MDI_DB_ENGINE", unset = "sqlite")
  if (identical(drv, "sqlite")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), mdi_db_path())
  } else {
    stop("Unsupported MDI_DB_ENGINE '", drv, "'. Supported engines: sqlite.", call. = FALSE)
  }
  con
}

mdi_db_init <- function() {
  schema <- system.file("schema.sql", package = "mdi")
  if (!nzchar(schema)) schema <- file.path(app_root_dir(), "database", "schema.sql")
  con <- mdi_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sql <- paste(readLines(schema, warn = FALSE), collapse = "\n")
  statements <- strsplit(sql, ";\\s*(\n|$)")[[1]]
  for (s in statements) {
    if (nzchar(trimws(s))) tryCatch(DBI::dbExecute(con, s),
      error = function(e) mdi_log_error(paste0("schema init: ", conditionMessage(e))))
  }
  invisible(TRUE)
}

# Returns the id of the inserted dataset row.
mdi_db_add_dataset <- function(name, display_name, n_rows, n_cols, size_bytes,
                               is_sample = FALSE, stored_path = NA_character_, metadata = list()) {
  con <- mdi_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con,
    "INSERT INTO datasets (name, display_name, n_rows, n_cols, size_bytes, is_sample, stored_path) VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(name, display_name, as.integer(n_rows), as.integer(n_cols),
                  as.integer(size_bytes), as.integer(is.logical(is_sample) && is_sample), stored_path)
  )
  id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
  if (length(metadata)) {
    md <- data.frame(
      dataset_id = rep(id, length(metadata)),
      key = names(metadata),
      value = vapply(metadata, function(x) paste(as.character(x), collapse = ", "), character(1))
    )
    DBI::dbAppendTable(con, "dataset_metadata", md)
  }
  invisible(id)
}

mdi_db_log_cleaning_op <- function(dataset_id, operation, details = "") {
  con <- mdi_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con,
    "INSERT INTO cleaning_operations (dataset_id, operation, details) VALUES (?, ?, ?)",
    params = list(as.integer(dataset_id), operation, substr(details, 1, 500)))
  invisible(TRUE)
}

mdi_db_log_analysis <- function(dataset_id, module, summary) {
  con <- mdi_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con,
    "INSERT INTO analysis_history (dataset_id, module, summary) VALUES (?, ?, ?)",
    params = list(as.integer(dataset_id), module, substr(summary, 1, 500)))
  invisible(TRUE)
}

mdi_db_add_model_run <- function(dataset_id, problem_type, target, model_name,
                                 metrics, seed, n_train, n_test) {
  con <- mdi_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con,
    "INSERT INTO model_runs (dataset_id, problem_type, target, model_name, metrics_json, seed, n_train, n_test)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(as.integer(dataset_id), problem_type, target, model_name,
                  jsonlite::toJSON(metrics, auto_unbox = TRUE), as.integer(seed),
                  as.integer(n_train), as.integer(n_test)))
  invisible(TRUE)
}

mdi_db_add_report <- function(dataset_id, report_type, file_name) {
  con <- mdi_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con,
    "INSERT INTO report_history (dataset_id, report_type, file_name) VALUES (?, ?, ?)",
    params = list(as.integer(dataset_id), report_type, file_name))
  invisible(TRUE)
}

mdi_db_setting_get <- function(key, default = NULL) {
  con <- mdi_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  res <- tryCatch(
    DBI::dbGetQuery(con, "SELECT value FROM app_settings WHERE key = ?", params = list(key)),
    error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0) default else res$value[1]
}

mdi_db_setting_set <- function(key, value) {
  con <- mdi_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con,
    "INSERT INTO app_settings (key, value) VALUES (?, ?)
     ON CONFLICT(key) DO UPDATE SET value = excluded.value, updated_at = datetime('now')",
    params = list(key, value))
  invisible(TRUE)
}

mdi_db_list_history <- function(limit = 50) {
  con <- mdi_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tryCatch(
    DBI::dbGetQuery(con, paste0(
      "SELECT h.created_at, h.module, h.summary, COALESCE(d.display_name, '-') AS dataset ",
      "FROM analysis_history h LEFT JOIN datasets d ON d.id = h.dataset_id ",
      "ORDER BY h.id DESC LIMIT ?"), params = list(as.integer(limit))),
    error = function(e) { mdi_log_error(paste0("history query: ", conditionMessage(e))); data.frame() })
}
