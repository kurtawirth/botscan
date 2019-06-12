# Delay loading botometer module ------------------------------------------

# Model from http://j.mp/2WXWsUJ  :
botometer <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to botometer:
  botometer <<- reticulate::import("botometer", delay_load = TRUE)
}

# Model from http://j.mp/2WNNbON  :
# the_module <- NULL
# .onLoad <- function(libname, pkgname) {
#   the_module <<- reticulate::import_from_path("the_py_module", system.file("python", "the_py_package", package = packageName(), mustWork = TRUE))
# }