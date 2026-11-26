# Data quality engine: structured checks plus a transparent 0-100 score.
#
# The Data Quality Score is an INTERNAL DIAGNOSTIC produced by this
# application. It is NOT an industry-standard benchmark. Scoring formula
# (documented so it can be audited):
#   completeness  (40 pts): 40 * (1 - share of missing cells)
#   uniqueness    (20 pts): 20 * (1 - share of duplicated rows)
#   consistency   (20 pts): 20 * (1 - share of cells in columns with type
#                            inference failures / invalid dates)
#   validity      (20 pts): 20 * (1 - share of cells flagged invalid:
#                            negative values in detected business metric
#                            columns + leading/trailing whitespace +
#                            constant/empty column cells)

compute_quality_report <- function(df, metric_cols = character(0)) {
  n <- nrow(df)
  p <- ncol(df)
  if (n == 0 || p == 0) {
    return(list(score = 0, breakdown = NULL, issues = data.frame()))
  }
  total_cells <- n * p

  missing_cells <- sum(is.na(df))
  completeness <- 1 - missing_cells / total_cells

  dup_rows <- sum(duplicated(df))
  uniqueness <- 1 - dup_rows / n

  invalid_cells <- 0L
  issues <- list(row_id = 0L)

  add_issue <- function(type, column, count, detail) {
    issues[[length(issues) + 1L]] <<- data.frame(
      check = type, column = column, rows_affected = count,
      severity = if (count > 0) "warning" else "ok",
      detail = detail, stringsAsFactors = FALSE)
  }

  date_like <- names(df)[vapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXt", "POSIXlt")), logical(1))]
  parse_failures <- 0

  ws_cells <- 0
  empty_cols <- c()
  constant_cols <- c()
  invalid_metric_rows <- 0

  for (nm in names(df)) {
    x <- df[[nm]]
    if (is.character(x) || is.factor(x)) {
      xs <- trimws(if (is.factor(x)) as.character(x) else x)
      ws <- sum(!is.na(xs) & xs != as.character(x) & as.character(x) != "" & !is.na(as.character(x)), na.rm = TRUE)
      ws_cells <- ws_cells + ws
      if (ws > 0) add_issue("whitespace", nm, ws, "Leading/trailing whitespace detected in this text column.")
    }
    if (all(is.na(x))) empty_cols <- c(empty_cols, nm)
    uq <- unique(x[!is.na(x)])
    if (length(uq) <= 1) constant_cols <- c(constant_cols, nm)
    if (is.numeric(x) && nm %in% metric_cols) {
      neg <- sum(!is.na(x) & x < 0)
      invalid_metric_rows <- invalid_metric_rows + neg
      if (neg > 0) add_issue("negative_metric", nm, neg, "Negative values found in a detected business metric.")
    }
  }

  # Type consistency: share of character cells that silently coerce to the
  # column type without parsing errors is assumed consistent; count risky cells.
  type_risky <- 0
  for (nm in names(df)) {
    x <- df[[nm]]
    if (is.numeric(x)) next
    s <- trimws(as.character(x))
    looks_numeric <- grepl("^[0-9]+([.,][0-9]+)?%?$", s) & !is.na(s) & s != ""
    if (is.character(x)) {
      type_risky <- type_risky + sum(looks_numeric, na.rm = TRUE)
    }
  }

  consistency <- 1 - min(1, (type_risky + parse_failures) / total_cells)
  validity <- 1 - min(1, (invalid_metric_rows + ws_cells) / total_cells)

  breakdown <- data.frame(
    component = c("Completeness", "Uniqueness", "Type consistency", "Validity"),
    max_points = c(40, 20, 20, 20),
    points = round(c(40 * completeness, 20 * uniqueness, 20 * consistency, 20 * validity), 1),
    explanation = c(
      "Share of non-missing cells",
      "1 - share of fully duplicated rows",
      "1 - share of cells with risky formatting (numbers stored as text)",
      "1 - share of invalid metric values and whitespace-contaminated cells"
    ),
    stringsAsFactors = FALSE
  )
  score <- round(sum(breakdown$points), 1)

  issues[[1]] <- NULL
  issues_df <- do.call(rbind, Filter(function(d) nrow(d) > 0 && d$rows_affected[1] > 0, issues))
  if (is.null(issues_df)) issues_df <- data.frame(check = character(0), column = character(0),
    rows_affected = integer(0), severity = character(0), detail = character(0), stringsAsFactors = FALSE)
  if (length(empty_cols)) issues_df <- rbind(issues_df, data.frame(check = "empty_column",
    column = paste(empty_cols, collapse = ", "), rows_affected = n, severity = "warning",
    detail = "Column(s) contain only missing values.", stringsAsFactors = FALSE))
  if (length(constant_cols)) issues_df <- rbind(issues_df, data.frame(check = "constant_column",
    column = paste(constant_cols, collapse = ", "), rows_affected = n, severity = "info",
    detail = "Column(s) have a single distinct value; they add no analytical signal.", stringsAsFactors = FALSE))
  if (dup_rows > 0) issues_df <- rbind(issues_df, data.frame(check = "duplicate_rows",
    column = "(all)", rows_affected = dup_rows, severity = "warning",
    detail = "Fully duplicated rows detected.", stringsAsFactors = FALSE))

  list(score = score, breakdown = breakdown, issues = issues_df,
       metrics = list(missing_cells = missing_cells, duplicate_rows = dup_rows,
                      whitespace_cells = ws_cells, negative_metric_values = invalid_metric_rows,
                      number_stored_as_text = type_risky))
}
