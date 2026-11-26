# Exploratory data analysis: descriptive statistics for numeric and
# categorical columns, correlation matrices, and missingness summaries.

numeric_summary <- function(df, cols = NULL) {
  cols <- intersect(cols %||% info_numeric_cols(df), names(df))
  if (!length(cols)) return(NULL)
  do.call(rbind, lapply(cols, function(nm) {
    x <- as.numeric(df[[nm]])
    x <- x[!is.na(x)]
    if (!length(x)) return(NULL)
    data.frame(
      column = nm,
      n = length(x), missing = sum(is.na(df[[nm]])),
      mean = mean(x), median = stats::median(x), sd = if (length(x) > 1) stats::sd(x) else NA_real_,
      min = min(x), q1 = stats::quantile(x, .25, names = FALSE),
      q3 = stats::quantile(x, .75, names = FALSE),
      max = max(x),
      iqr = stats::IQR(x),
      skewness = skewness_moment(x),
      kurtosis = if (length(x) > 3) excess_kurtosis_moment(x) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

info_numeric_cols <- function(df) names(df)[vapply(df, is.numeric, logical(1))]

skewness_moment <- function(x) {
  n <- length(x); if (n < 3) return(NA_real_)
  m <- mean(x); s <- stats::sd(x); if (is.na(s) || s == 0) return(NA_real_)
  n / ((n - 1) * (n - 2)) * sum(((x - m) / s)^3)
}

excess_kurtosis_moment <- function(x) {
  n <- length(x); if (n < 4) return(NA_real_)
  m <- mean(x); s <- stats::sd(x); if (is.na(s) || s == 0) return(NA_real_)
  (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3)) * sum(((x - m) / s)^4) - 3 * ((n - 1)^2) / ((n - 2) * (n - 3))
}

categorical_summary <- function(df, cols = NULL, max_categories = 10) {
  cols <- intersect(cols %||% names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))], names(df))
  if (!length(cols)) return(NULL)
  do.call(rbind, lapply(cols, function(nm) {
    x <- if (is.factor(df[[nm]])) as.character(df[[nm]]) else as.character(df[[nm]])
    tab <- sort(table(x[!is.na(x)]), decreasing = TRUE)
    if (!length(tab)) return(NULL)
    data.frame(
      column = nm,
      n_unique = length(tab),
      top_category = names(tab)[1],
      top_count = as.integer(tab[1]),
      top_share = round(as.numeric(tab[1]) / sum(tab), 3),
      stringsAsFactors = FALSE
    )
  }))
}

correlation_matrix <- function(df, cols = NULL, method = c("pearson", "spearman")) {
  method <- match.arg(method)
  cols <- intersect(cols %||% info_numeric_cols(df), names(df))
  if (length(cols) < 2) return(NULL)
  m <- suppressWarnings(stats::cor(df[, cols, drop = FALSE], use = "pairwise.complete.obs", method = method))
  m
}

missingness_summary <- function(df) {
  data.frame(
    column = names(df),
    missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    pct = round(100 * vapply(df, function(x) mean(is.na(x)), numeric(1)), 2),
    stringsAsFactors = FALSE
  )
}
