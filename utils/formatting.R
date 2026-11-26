# Number formatting helpers used by KPI cards and tables.

fmt_int <- function(x) formatC(as.numeric(x), format = "d", big.mark = ",", )

fmt_num <- function(x, digits = 2) {
  if (is.null(x)) return("-")
  out <- formatC(as.numeric(x), format = "f", digits = digits, big.mark = ",")
  out[is.na(x)] <- "N/A (division by 0 or missing denominator)"
  out
}

fmt_money <- function(x, digits = 2) {
  x <- as.numeric(x)
  out <- paste0("$", formatC(abs(x), format = "f", digits = digits, big.mark = ","))
  out[!is.na(x) & x < 0] <- paste0("-", out[!is.na(x) & x < 0])
  out[is.na(x)] <- "N/A"
  out
}

fmt_pct <- function(x, digits = 2) {
  x <- as.numeric(x)
  out <- paste0(formatC(x, format = "f", digits = digits), "%")
  out[is.na(x)] <- "N/A"
  out
}

replace_na_char <- function(x, replacement = "-") { x[is.na(x)] <- replacement; x }
