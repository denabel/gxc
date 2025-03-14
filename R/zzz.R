# Initialization functions

# Set progress handler
.onLoad <- function(libname, pkgname) {
  progressr::handlers("debug")
}
