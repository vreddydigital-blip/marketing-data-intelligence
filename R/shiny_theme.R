# bslib theme definitions (Bootstrap 5, light/dark color modes).

app_theme <- function(dark = FALSE) {
  bslib::bs_theme(
    version = 5,
    preset = "cosmo",
    `color-mode` = if (dark) "dark" else "light"
  )
}
