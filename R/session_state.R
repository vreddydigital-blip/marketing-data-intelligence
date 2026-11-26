# Shared session state helpers. `dataset` is a shiny reactiveValues shared by
# all modules through app.R. Original (uploaded) data is immutable; only
# `current` changes via tracked cleaning operations.

new_dataset_state <- function() {
  shiny::reactiveValues(
    original = NULL,   # immutable copy of what was uploaded
    current  = NULL,   # working copy used by all analyses
    name = NULL,
    is_sample = FALSE,
    info = NULL,
    quality = NULL,
    history = list(),
    version = 0L        # bumped on every change so reactives can key off it
  )
}

refresh_dataset_derived <- function(dataset) {
  df <- dataset$current
  if (is.null(df)) {
    dataset$info <- NULL; dataset$quality <- NULL
    dataset$history <- list(); dataset$version <- dataset$version + 1L
    return(invisible(NULL))
  }
  mapping <- detect_marketing_columns(df)
  metric_cols <- unname(unlist(mapping[c("spend", "revenue", "impressions", "clicks",
                                         "conversions", "leads", "sessions", "users")]))
  dataset$info <- describe_dataset(df)
  dataset$quality <- compute_quality_report(df, metric_cols = metric_cols)
  dataset$version <- dataset$version + 1L
  invisible(NULL)
}

set_dataset <- function(dataset, df, name, is_sample = FALSE, keep_original = FALSE) {
  dataset$original <- if (keep_original && !is.null(dataset$original)) dataset$original else df
  dataset$current <- df
  dataset$name <- name
  dataset$is_sample <- is_sample
  dataset$history <- list()
  refresh_dataset_derived(dataset)
  mdi_log_info(paste0("dataset loaded: name parts only; rows=", nrow(df), " cols=", ncol(df), " sample=", is_sample))
}

apply_cleaning <- function(dataset, result, op_label, dataset_id = NULL) {
  dataset$current <- result$data
  dataset$history <- c(dataset$history, list(list(label = op_label, message = result$message)))
  refresh_dataset_derived(dataset)
  if (!is.null(dataset_id)) tryCatch(mdi_db_log_cleaning_op(dataset_id, op_label, result$message),
                                     error = function(e) NULL)
  invisible(NULL)
}
