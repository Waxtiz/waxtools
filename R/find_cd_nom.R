#' Send a query to the TAXREF API
#'
#' @param url is there query formulate in url format
#'
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom httr content_type_json
#' @importFrom jsonlite fromJSON
#'
use_taxref_api <- function(url) {
  output <- content(
    GET(url),             # url correspond a l'url a interroger
    as = "text",          # type de la sortie renvoyee
    content_type_json(),  # type de la reponse de l'url
    encoding = "UTF-8"    # encodage de la reponse de l'url
  )
  out_json <- fromJSON(output)
}

#' Parse the url to make a query to the taxref api
#'
#' @param sp is a character string corresponding to the name of a taxon (species or other).
#' @param lang defines whether 'sp' is a vernacular (fr) or scientific (latin) name
#'
#' @importFrom utils URLencode
parse_url_for_taxref_api <- function(sp, lang) {
  if (lang == "sci") {
    paste0('https://taxref.mnhn.fr/api/taxa/fuzzyMatch?term=',
           URLencode(sp))
  } else if (lang == "fr") {
    paste0("https://taxref.mnhn.fr/api/taxa/search?frenchVernacularNames=",
           URLencode(sp),
           "&page=1&size=5000")
  }
}

#' send the query to the TAXREF API and formulate the response
#'
#' @param sp is a character string corresponding to the name of a taxon (species or other).
#' @param lang defines whether 'sp' is a vernacular (fr) or scientific (latin) name
query_to_taxref_api <- function(sp, lang) {
  url <- parse_url_for_taxref_api(sp, lang)
  output <- use_taxref_api(url)
  output$`_embedded`$taxa$id
}

#' @importFrom plyr ldply
#' @rdname query_to_taxref_api
query_loop_to_taxref_api <- function(sp, lang) {
  list <- list()
  for (i in seq_along(sp)) {
    list[[length(list)+1]] <- query_to_taxref_api(sp[i], lang)
    names(list)[i] <- sp[i]
  }
  output <- ldply(list, data.frame)
  colnames(output) <- c("sp", "cd_nom")
  return(output)
}

#' Find the CD_NAME of a species from single or liste of taxon name (use the taxref API)
#'
#' @param sp is a character string corresponding to the name of a taxon (species or other).
#' @param lang defines whether 'sp' is a vernacular (fr) or scientific (latin) name
#'
#' @export
find_cd_nom <- function(sp, lang = "sci") {
  if (length(sp) == 1) {
    query_to_taxref_api(sp, lang)
  } else if (length(sp) > 1) {
    query_loop_to_taxref_api(sp, lang)
  } else {
    stop("sp is probably not good")
  }
}
