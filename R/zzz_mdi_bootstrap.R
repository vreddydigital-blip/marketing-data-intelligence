# Bootstrap hook for Shiny's R/ autoloading.
#
# Since shiny 1.14, global.R is NOT evaluated when shiny.autoload.r is TRUE
# (the default): only files in the app's R/ directory are auto-sourced. This
# file sorts last alphabetically, so all modules are already defined; it then
# sources global.R, which loads utils, services, and the database layer.
#
# On older shiny versions global.R is sourced before autoloading and the
# mdi.bootstrapped guard makes this file a no-op.

if (!identical(getOption("mdi.bootstrapped"), TRUE) &&
    !identical(getOption("mdi.bootstrapping"), TRUE)) {
  find_mdi_root <- function() {
    dir <- getwd()
    for (i in 1:6) {
      if (file.exists(file.path(dir, "global.R"))) return(normalizePath(dir, winslash = "/"))
      dir <- dirname(dir)
    }
    getwd()
  }
  source(file.path(find_mdi_root(), "global.R"), local = globalenv())
}
