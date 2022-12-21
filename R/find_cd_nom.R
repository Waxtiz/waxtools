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
#' @param group_fr defines if a filter must be made on the taxon during the query (amphibians, mammals, birds...)
#'
#' @importFrom utils URLencode
parse_url_for_taxref_api <- function(sp, lang, group_fr) {
  if (lang == "sci") {
    paste0('https://taxref.mnhn.fr/api/taxa/fuzzyMatch?term=',
           URLencode(sp))
  } else if (lang == "fr") {
    if (!is.null(group_fr)) {
      paste0("https://taxref.mnhn.fr/api/taxa/search?frenchVernacularNames=",
             URLencode(sp),
             "&territories=fr&domain=continental&vernacularGroups=Amphibiens&page=1&size=100")
    } else {
      paste0("https://taxref.mnhn.fr/api/taxa/search?frenchVernacularNames=",
             URLencode(sp),
             "&territories=fr&domain=continental&page=1&size=100")
    }
  }
}

#' send the query to the TAXREF API and formulate the response
#'
#' @param sp is a character string corresponding to the name of a taxon (species or other).
#' @param lang defines whether 'sp' is a vernacular (fr) or scientific (latin) name.
#' @param group_fr defines if a filter must be made on the taxon during the query (amphibians, mammals, birds...)
query_to_taxref_api <- function(sp, lang, group_fr) {
  url <- parse_url_for_taxref_api(sp, lang, group_fr)
  output <- use_taxref_api(url)
  res <- output$`_embedded`$taxa
  res <- res[,-length(res)]

  # Add filter for return unique CD_REF (if a single choice)
  if (nrow(res) > 1) {
    if (length(unique(res$referenceId)) == 1) {
      res[res$id == unique(res$referenceId),]
    } else {
      res
    }
  } else {
    res
  }
}

#' @importFrom plyr ldply
#' @rdname query_to_taxref_api
query_loop_to_taxref_api <- function(sp, lang, group_fr) {
  list <- list()
  for (i in seq_along(sp)) {
    res <- query_to_taxref_api(sp[i], lang, group_fr)
    col_names <- colnames(res)
    list[[length(list)+1]] <- res
    names(list)[i] <- sp[i]
  }
  output <- ldply(list, data.frame)
  colnames(output) <- c("sp", col_names)
  return(output)
}

#' Find the CD_NAME of a species from single or liste of taxon name (use the taxref API)
#'
#' @param sp is a character string corresponding to the name of a taxon (species or other).
#' @param lang defines whether 'sp' is a vernacular (fr) or scientific (latin) name
#' @param group_fr defines if a filter must be made on the taxon during the query (amphibians, mammals, birds...)
#'
#' @examples
#' # run query for find a single name of specie
#' find_cd_nom("sonneur a ventre jaune", "fr")
#'
#' # or use a liste
#' sp_list <- c("Erithacus rubecula", "Gypaetus barbatus", "Pyrrhocorax pyrrhocorax",
#'              "Phasianus colchicus", "Circaetus gallicus", "Neophron percnopterus")
#' find_cd_nom(sp_list, "sci")
#'
#' @export
find_cd_nom <- function(sp, lang = "sci", group_fr = NULL) {
  if (length(sp) == 1) {
    query_to_taxref_api(sp, lang, group_fr) # @TODO: faire une fonction qui liste les groupes FR
  } else if (length(sp) > 1) {
    query_loop_to_taxref_api(sp, lang, group_fr)
  } else {
    stop("sp is probably not good")
  }
}
