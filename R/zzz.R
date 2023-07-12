#' Detects if the user is using Rstudio or not
#' @param libname character. The name of the library where the package is loaded.
#' @param pkgname character. The name of the package.
.onAttach <- function(libname, pkgname) {
  if (Sys.getenv("RSTUDIO") == "1") {
    options(ide_var = "RStudio")
  } else {
    options(ide_var = "Other")
  }
  packageStartupMessage("IDE has been correctly identified for com() function")
}