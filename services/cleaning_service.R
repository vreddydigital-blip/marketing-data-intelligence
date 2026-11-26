# Data cleaning operations. All functions are pure: they take and return data
# frames. The module layer previews results before they are applied, and the
# original uploaded data is never modified.

remove_duplicate_rows <- function(df) {
  n0 <- nrow(df)
  out <- dplyr::distinct(df)
  list(data = out, changed = n0 - nrow(out), message = sprintf("Removed %d duplicate row(s).", n0 - nrow(out)))
}

drop_columns <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(list(data = df, changed = 0, message = "No selected columns found."))
  out <- df[, setdiff(names(df), cols), drop = FALSE]
  list(data = out, changed = length(cols), message = sprintf("Removed column(s): %s.", paste(cols, collapse = ", ")))
}

rename_columns <- function(df, mapping) {
  mapping <- mapping[!vapply(mapping, function(x) !nzchar(x) || is.na(x), logical(1))]
  mapping <- mapping[names(mapping) %in% names(df)]
  if (!length(mapping)) return(list(data = df, changed = 0, message = "No valid renames supplied."))
  idx <- match(names(mapping), names(df))
  names(df)[idx] <- unname(mapping)
  list(data = df, changed = length(mapping),
       message = sprintf("Renamed: %s.", paste(sprintf("%s -> %s", names(mapping), unname(mapping)), collapse = ", ")))
}

replace_missing_values <- function(df, cols, fill_value) {
  cols <- intersect(cols, names(df))
  for (nm in cols) {
    x <- df[[nm]]
    if (is.numeric(x)) {
      value <- suppressWarnings(as.numeric(fill_value))
      if (is.na(value)) next
    } else {
      value <- as.character(fill_value)
    }
    x[is.na(x)] <- value
    df[[nm]] <- x
  }
  list(data = df, changed = length(cols), message = sprintf("Replaced missing values in: %s.", paste(cols, collapse = ", ")))
}

impute_numeric <- function(df, cols, method = c("mean", "median")) {
  method <- match.arg(method)
  cols <- intersect(cols, names(df))
  n_filled <- 0L
  for (nm in cols) {
    x <- df[[nm]]
    if (!is.numeric(x)) next
    value <- if (identical(method, "mean")) mean(x, na.rm = TRUE) else stats::median(x, na.rm = TRUE)
    idx <- is.na(x)
    x[idx] <- value
    n_filled <- n_filled + sum(idx)
    df[[nm]] <- x
  }
  list(data = df, changed = n_filled,
       message = sprintf("Imputed %d numeric missing value(s) with the %s.", n_filled, method))
}

impute_categorical <- function(df, cols, method = c("mode", "missing_category")) {
  method <- match.arg(method)
  cols <- intersect(cols, names(df))
  n_filled <- 0L
  for (nm in cols) {
    x <- if (is.factor(df[[nm]])) as.character(df[[nm]]) else df[[nm]]
    if (!is.character(x)) next
    idx <- is.na(x)
    if (!sum(idx)) next
    value <- if (identical(method, "missing_category")) "Missing" else {
      tab <- table(x[!idx])
      if (!length(tab)) "Missing" else names(sort(tab, decreasing = TRUE))[1]
    }
    x[idx] <- value
    df[[nm]] <- x
    n_filled <- n_filled + sum(idx)
    df[[nm]] <- x
  }
  list(data = df, changed = n_filled,
       message = sprintf("Imputed %d categorical missing value(s) (%s).", n_filled, method))
}

convert_column_types <- function(df, conversions) {
  # conversions: named list col -> one of "numeric", "character", "date", "factor"
  applied <- 0L
  for (nm in names(conversions)) {
    if (!nm %in% names(df)) next
    x <- df[[nm]]
    target <- conversions[[nm]]
    new <- switch(target,
      numeric   = suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", as.character(x)))),
      character = as.character(x),
      date      = parse_date_column(x),
      factor    = factor(x),
      x
    )
    df[[nm]] <- new
    applied <- applied + 1L
  }
  list(data = df, changed = applied, message = sprintf("Converted %d column(s).", applied))
}

# Parse a mixed character/numeric column into Date. Handles ISO dates and
# Excel-style numeric origins.
parse_date_column <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt", "POSIXlt"))) return(as.Date(x))
  s <- trimws(as.character(x))
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d %H:%M:%S")
  out <- as.Date(rep(NA_character_, length(s)))
  for (f in fmts) {
    idx <- is.na(out)
    if (!any(idx)) break
    out[idx] <- suppressWarnings(as.Date(s[idx], format = f))
  }
  if (all(is.na(out)) && is.numeric(x) && all(x[!is.na(x)] > 1e4 & x[!is.na(x)] < 1e6, na.rm = TRUE)) {
    return(as.Date(x, origin = "1899-12-30"))  # Excel serial dates
  }
  out
}

trim_whitespace_columns <- function(df, cols = NULL) {
  cols <- intersect(cols %||% names(df), names(df))
  n_changed <- 0L
  for (nm in cols) {
    x <- df[[nm]]
    if (is.character(x) || is.factor(x)) {
      new <- trimws(if (is.factor(x)) as.character(x) else x)
      n_changed <- n_changed + sum(new != as.character(x), na.rm = TRUE)
      df[[nm]] <- if (is.factor(x)) factor(new) else new
    }
  }
  list(data = df, changed = n_changed, message = sprintf("Trimmed whitespace: %d value(s) changed.", n_changed))
}

standardize_categories <- function(df, cols, style = c("title_case", "lower", "upper", "snake")) {
  style <- match.arg(style)
  cols <- intersect(cols, names(df))
  n_changed <- 0L
  for (nm in cols) {
    x <- if (is.factor(df[[nm]])) as.character(df[[nm]]) else if (is.character(df[[nm]])) df[[nm]] else NULL
    if (is.null(x)) next
    new <- switch(style,
      title_case = gsub("\\b([a-z])", "\\U\\1", tolower(trimws(x)), perl = TRUE),
      lower = tolower(trimws(x)),
      upper = toupper(trimws(x)),
      snake = gsub("[^A-Za-z0-9]+", "_", tolower(trimws(x)))
    )
    n_changed <- n_changed + sum(new != x, na.rm = TRUE)
    df[[nm]] <- new
  }
  list(data = df, changed = n_changed, message = sprintf("Standardised %d value(s) as %s.", n_changed, style))
}

# Safe row filtering: conditions are structured values (column, operator, value),
# never raw R code; nothing user-supplied is evaluated.
apply_row_filters <- function(df, conditions) {
  keep <- rep(TRUE, nrow(df))
  applied <- 0
  for (cond in conditions) {
    col <- cond$column
    if (!col %in% names(df)) next
    x <- df[[col]]
    val <- cond$value
    ok <- switch(cond$operator,
      "=="     = as.character(x) == as.character(val),
      "!="     = as.character(x) != as.character(val),
      ">"      = as.numeric(x) > suppressWarnings(as.numeric(val)),
      ">="     = as.numeric(x) >= suppressWarnings(as.numeric(val)),
      "<"      = as.numeric(x) < suppressWarnings(as.numeric(val)),
      "<="     = as.numeric(x) <= suppressWarnings(as.numeric(val)),
      "contains" = grepl(as.character(val), as.character(x), fixed = TRUE),
      "not NA"  = !is.na(x),
      rep(TRUE, nrow(df))
    )
    ok[is.na(ok)] <- if (cond$operator %in% c("not NA", "!=")) TRUE else FALSE
    keep <- keep & ok
    applied <- applied + 1L
  }
  removed <- sum(!keep)
  list(data = df[keep, , drop = FALSE], changed = removed,
       message = sprintf("Applied %d filter(s); removed %d row(s).", applied, removed))
}

detect_outliers_iqr <- function(df, cols) {
  cols <- intersect(cols, names(df)[vapply(df, is.numeric, logical(1))])
  results <- lapply(cols, function(nm) {
    x <- df[[nm]][!is.na(df[[nm]])]
    if (length(x) < 4) return(list(column = nm, n_outliers = 0L, lower = NA_real_, upper = NA_real_))
    q <- stats::quantile(x, c(.25, .75), names = FALSE)
    iqr <- q[2] - q[1]
    lo <- q[1] - 1.5 * iqr
    hi <- q[2] + 1.5 * iqr
    list(column = nm, n_outliers = sum(x < lo | x > hi), lower = lo, upper = hi)
  })
  setNames(results, cols)
}

remove_outliers <- function(df, cols) {
  bounds <- detect_outliers_iqr(df, cols)
  keep <- rep(TRUE, nrow(df))
  for (b in bounds) {
    if (b$n_outliers == 0) next
    x <- df[[b$column]]
    keep <- keep & (is.na(x) | (x >= b$lower & x <= b$upper))
  }
  removed <- sum(!keep)
  list(data = df[keep, , drop = FALSE], changed = removed,
       message = sprintf("Removed %d outlier row(s) using the 1.5 x IQR rule.", removed))
}

# Square-root / log / z-score transforms, applied to selected numeric columns.
transform_columns <- function(df, transforms) {
  # transforms: named list col -> "log", "sqrt", or "zscore"
  applied <- 0L
  for (nm in names(transforms)) {
    if (!nm %in% names(df) || !is.numeric(df[[nm]])) next
    x <- df[[nm]]
    new <- switch(transforms[[nm]],
      log = if (all(x[!is.na(x)] > 0, na.rm = TRUE)) log(x) else log1p(pmax(x, 0)),
      sqrt = if (all(x[!is.na(x)] >= 0, na.rm = TRUE)) sqrt(x) else sqrt(pmax(x, 0)),
      zscore = suppressWarnings((x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)),
      x
    )
    df[[nm]] <- new
    applied <- applied + 1L
  }
  list(data = df, changed = applied, message = sprintf("Applied %d transformation(s).", applied))
}
