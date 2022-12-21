#' Checks the validity of the geometry
#'
#' @param layer is a sf object.
#'
#' @importFrom sf st_is_valid
#'
#' @export
geom_is_valid <- function(layer) {
  res <- sf::st_is_valid(layer)
  nb_bad <- sum(res == FALSE)
  out <- ifelse(nb_bad == 0, TRUE, FALSE)
  return(out)
}

#' Fix geom with cleangeo buffer methode for sf object
#'
#' @param poly is a sf object (polygon or multipolygos).
#'
#' @importFrom magrittr `%>%`
#' @importFrom sf as_Spatial
#' @importFrom cleangeo clgeo_Clean
#' @importFrom methods as
#'
#' @export
fix_geom <- function(poly) {
  poly2 <- poly %>%
    as_Spatial() %>% # reformat data for using clgeo library (from sp format)
    cleangeo::clgeo_Clean(strategy = "BUFFER") %>%
    as("sf") # reformat into sp format
  return(poly2)
}

