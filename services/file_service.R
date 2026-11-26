# Dataset import: reads CSV/XLS/XLSX, handles encodings and delimiters, and
# builds a standardised dataset description. Uploaded files are read as data
# only; content is never evaluated.

#' Read an uploaded data file into a data frame.
#' @param path  Path to the (already validated) file on disk.
#' @param filename Original client filename.
#' @param sheet Optional Excel sheet name.
read_data_file <- function(path, filename, sheet = NULL) {
  if (!file.exists(path)) stop("Temporary upload no longer exists. Please upload the file again.", call. = FALSE)
  ext <- tolower(tools::file_ext(filename))
  df <- switch(ext,
    csv  = read_csv_any(path),
    xls  = read_excel_any(path, sheet),
    xlsx = read_excel_any(path, sheet),
    stop(sprintf("Unsupported file type '.%s'.", ext), call. = FALSE)
  )
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    stop("The file contains no rows or no columns. Check that the file holds a data table.", call. = FALSE)
  }
  names(df) <- trimws(names(df))
  df
}

read_csv_any <- function(path) {
  # Delimiter sniffing from the header line: a line with more ';' than ','
  # separators is treated as semicolon-delimited first.
  header <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) NA_character_)
  semi_first <- !is.na(header) &&
    lengths(regmatches(header, gregexpr(";", header, fixed = TRUE))) >
    lengths(regmatches(header, gregexpr(",", header, fixed = TRUE)))
  if (semi_first) {
    tmp <- tryCatch(
      readr::read_delim(path, delim = ";", show_col_types = FALSE, progress = FALSE),
      error = function(e) NULL)
    if (!is.null(tmp)) return(as.data.frame(tmp))
  }
  tmp <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE, num_threads = 1),
    error = function(e) NULL)
  if (!is.null(tmp) && ncol(tmp) > 0) return(as.data.frame(tmp))
  # encoding fallback (Latin-1) then delimiter probing with ';'
  tmp <- tryCatch(readr::read_csv(path, locale = readr::locale(encoding = "Latin1"),
                                  show_col_types = FALSE, progress = FALSE),
                  error = function(e) NULL)
  if (!is.null(tmp) && ncol(tmp) > 0) return(as.data.frame(tmp))
  tmp <- tryCatch(readr::read_delim(path, delim = ";", show_col_types = FALSE, progress = FALSE),
                  error = function(e) NULL)
  if (!is.null(tmp) && ncol(tmp) > 0) return(as.data.frame(tmp))
  stop("Could not parse the CSV file. Ensure it is a plain-text, comma- or semicolon-delimited file.", call. = FALSE)
}

read_excel_any <- function(path, sheet = NULL) {
  sheets <- excel_sheets_safe(path)
  if (length(sheets) == 0) stop("Could not read the Excel file (no readable sheets).", call. = FALSE)
  if (is.null(sheet)) sheet <- sheets[1]
  if (!sheet %in% sheets) {
    stop(sprintf("Sheet '%s' not found. Available sheets: %s", sheet, paste(sheets, collapse = ", ")), call. = FALSE)
  }
  df <- readxl::read_excel(path, sheet = sheet)
  as.data.frame(df)
}

excel_sheets_safe <- function(path) {
  tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
}

#' Standardised dataset	description: types, missingness, uniqueness, flags.
describe_dataset <- function(df) {
  info <- list(
    n_rows = nrow(df),
    n_cols = ncol(df),
    size_mb = as.numeric(object.size(df)) / (1024^2),
    duplicate_rows = sum(duplicated(df))
  )
  info$columns <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    list(
      name = nm,
      class = class(x)[1],
      missing = sum(is.na(x)),
      missing_pct = round(100 * mean(is.na(x)), 2),
      unique = length(unique(x[!is.na(x)])),
      empty = all(is.na(x)),
      constant = (!all(is.na(x))) && length(unique(x[!is.na(x)])) <= 1,
      is_numeric = is.numeric(x),
      is_date = inherits(x, c("Date", "POSIXct", "POSIXt", "POSIXlt"))
    )
  })
  info$date_cols <- names(df)[vapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXt", "POSIXlt")), logical(1))]
  info$numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  info$categorical_cols <- setdiff(names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))],
                                   info$date_cols)
  dupe_names <- names(df)[duplicated(names(df))]
  info$duplicate_col_names <- dupe_names
  info$empty_cols <- vapply(info$columns, function(c) isTRUE(c$empty), logical(1))
  names(info$empty_cols) <- vapply(info$columns, function(c) c$name, character(1))
  info
}

#' Preview as an HTML-ready data frame (first 100 rows).
preview_table <- function(df, n = 100) utils::head(df, n)
