# Initialization functions

# Set a progress handler
.onLoad <- function(libname, pkgname) {
  progressr::handlers("debug")
}
