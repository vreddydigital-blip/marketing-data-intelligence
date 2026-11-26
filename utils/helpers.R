# General helpers shared by services and modules.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Safe division: returns NA where the denominator is 0 or NA, never Inf/NaN.
safe_div <- function(numerator, denominator) {
  num <- as.numeric(numerator)
  den <- as.numeric(denominator)
  out <- rep(NA_real_, max(length(num), length(den)))
  ok <- !is.na(den) & den != 0 & !is.na(num)
  out[ok] <- num[ok] / den[ok]
  out
}

esc <- function(x) {
  if (is.null(x)) return(NULL)
  htmltools::htmlEscape(as.character(x))
}

seq_cols <- function(df, cols) cols[cols %in% names(df)]

has_cols <- function(df, cols) all(cols %in% names(df))

n_missing <- function(df) sum(is.na(df))

dupe_rows <- function(df) sum(duplicated(df))
