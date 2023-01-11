#' Create folders structure for new R project.
#'
#' This function creates a directory structure for new R project.
#'
#' @param mode character indicating the type of structure to be created. Default is "default".
#'
#' @export
setup_folders <- function(mode = "default") {
  if (mode == "default") {
    dir.create("input")
    dir.create("output")
  } else {
    stop("The 'mode' parameter is probably incorrect. At the moment only 'default' mode is supported.",
         "Please enter the value 'default' in the 'mode' option.")
  }
}