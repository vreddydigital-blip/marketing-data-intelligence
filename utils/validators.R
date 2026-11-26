# Upload and input validation. Uploaded content is data only, never code.

ALLOWED_EXTENSIONS <- c("csv", "xls", "xlsx")
MAX_UPLOAD_MB <- max(5, suppressWarnings(as.integer(Sys.getenv("MDI_MAX_UPLOAD_MB", unset = "50"))), na.rm = TRUE)

validate_upload_file <- function(path, filename, max_mb = MAX_UPLOAD_MB) {
  checks <- list(ok = TRUE, message = character(0))
  ext <- tolower(tools::file_ext(filename))
  if (length(ext) == 0 || is.na(ext) || !ext %in% ALLOWED_EXTENSIONS) {
    checks$ok <- FALSE
    checks$message <- c(checks$message,
      sprintf("Unsupported file type '.%s'. Allowed types: %s.",
              esc__fb(ext), paste(ALLOWED_EXTENSIONS, collapse = ", ")))
  }
  size_mb <- file.size(path) / (1024^2)
  if (is.na(size_mb)) {
    checks$ok <- FALSE
    checks$message <- c(checks$message, "Uploaded file could not be read.")
  } else if (size_mb > max_mb) {
    checks$ok <- FALSE
    checks$message <- c(checks$message,
      sprintf("File is %.1f MB and exceeds the %d MB limit.", size_mb, max_mb))
  } else if (size_mb <= 0) {
    check_dir_for_empty_file <- TRUE
    checks$ok <- FALSE
    checks$message <- c(checks$message, "File is empty.")
  }
  checks
}

esc__fb <- function(x) ifelse(is.null(x) || length(x) == 0, "", as.character(x))

# Internal safe name: prevents path traversal via crafted filenames.
safe_internal_name <- function(filename) {
  base <- tools::file_path_sans_ext(basename(filename))
  ext <- tolower(tools::file_ext(filename))
  base <- gsub("[^A-Za-z0-9_-]", "_", substr(base, 1, 60))
  sprintf("upload_%s_%s.%s", base, format(Sys.time(), "%Y%m%d_%H%M%S"), ext)
}

validate_required_columns <- function(df, required, context = "dataset") {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(sprintf("%s is missing required column(s): %s", context,
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}
