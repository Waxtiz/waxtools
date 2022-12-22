#' Check the validity of the geometry of a simple features object
#'
#' @description
#' This function checks the validity of the geometry of a given simple features (sf) object.
#' It does this by using the `st_is_valid()` function from the `sf` package, which checks whether the geometries of the
#' input object are topologically valid.
#'
#' @param layer an sf object.
#'
#' @return a logical value indicating whether the geometry of the input object is valid (`TRUE`) or not (`FALSE`).
#'
#' @importFrom sf st_is_valid
#'
#' @export
geom_is_valid <- function(layer) {
  res <- st_is_valid(layer)
  nb_bad <- sum(res == FALSE)
  out <- ifelse(nb_bad == 0, TRUE, FALSE)
  return(out)
}

#' Fix invalid geometries in a simple features object using the buffer method
#'
#' @description
#' This function fixes invalid geometries in a given simple features (sf) object using the buffer method from the
#' `cleangeo` package. The input object must be a polygon or multipolygon. The function first reformats the input object
#' into a different spatial object format using the `as_Spatial()` function from `sf`, and then uses the `clgeo_Clean()`
#' function from `cleangeo` to apply the buffer method to fix the invalid geometries. Finally, the function reformats
#' the object back into an sf object.
#'
#' @param poly an sf object containing polygons or multipolygons.
#'
#' @return an sf object with invalid geometries fixed using the buffer method.
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

