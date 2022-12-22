#' Send a query to the TAXREF API
#'
#' @description
#' This function sends a query in URL format to the TAXREF API and returns the response in JSON format.
#' It can be used to retrieve taxonomic information from the database.
#'
#' @param url is there query formulate in url format
#'
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom httr content_type_json
#' @importFrom jsonlite fromJSON
#'
#' @return A JSON containing the response from the TAXREF API.
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
#' @description
#' This function generates a URL to query the TAXREF API using a taxon name and optional parameters. The name can be either a scientific or vernacular name in French, and the query can be filtered by group (e.g. amphibians, mammals). The URL generated can be used to retrieve taxonomic information from the database.
#'
#' @param sp is a character string corresponding to the name of a taxon (species or other).
#' @param lang defines whether 'sp' is a vernacular (fr) or scientific (latin) name
#' @param group_fr defines if a filter must be made on the taxon during the query (amphibians, mammals, birds...)
#'
#' @importFrom utils URLencode
#'
#' @examples
#' \dontrun{
#' sp <- "Anas platyrhynchos"
#' lang <- "sci"
#' group_fr <- NULL
#' parse_url_for_taxref_api(sp, lang, group_fr)
#'
#' sp <- "canard colvert"
#' lang <- "fr"
#' group_fr <- "Oiseaux"
#' parse_url_for_taxref_api(sp, lang, group_fr)
#'}
#'
#' @return A character string containing the URL to query the TAXREF API.
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

#' Query the TAXREF API and format the response
#'
#' This function sends a query to the TAXREF API using the sp and lang parameters to search for a specific taxon. If lang is set to "fr", the function also allows the user to filter the search results by group_fr. The function then formats the response from the API and returns a data frame with the relevant information.
#'
#' @param sp A character string representing the name of the taxon to search for. This can be either a vernacular or scientific name, depending on the value of lang.
#' @param lang A character string indicating whether sp is a vernacular ("fr") or scientific ("sci") name.
#' @param group_fr (optional) A character string defining the taxonomic group to filter the search results by. This parameter is only used if lang is set to "fr".
#'
#' @return A data frame containing information about the taxon(s) matching the search criteria.
#'
query_to_taxref_api <- function(sp, lang, group_fr) {
  url <- parse_url_for_taxref_api(sp, lang, group_fr)
  output <- use_taxref_api(url)
  res <- output$`_embedded`$taxa
  res <- res[,-length(res)]

  if (is.null(output$`_embedded`)) {
    warning("No match found for this query")
    return(NULL)
  }

  if (nrow(res) > 1) {
    if (length(unique(res$referenceId)) == 1) {
      res[res$id == unique(res$referenceId),]
    } else {
      # Get unique CD_REF and corresponding ids
      unique_cd_ref <- unique(res$referenceId)
      # Only keep rows with unique ids
      res[res$id %in% unique_cd_ref,]
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
  tryCatch({
    check_before_run_query(sp, lang, group_fr)
    if (length(sp) == 1) {
    query_to_taxref_api(sp, lang, group_fr)
  } else if (length(sp) > 1) {
    query_loop_to_taxref_api(sp, lang, group_fr)
  }
  }, error = function(e) {
    # Print error message
    message(paste0("An error has occurred:\n", e))
  })
}


#' List available taxon groups for query to TAXREF API
#'
#' This function lists the available taxon groups that can be used to filter the query to the TAXREF API.
#'
#' @return A character vector containing the available taxon groups.
#'
#' @export
list_group_fr <- function() {
  url <- "https://taxref.mnhn.fr/api/operationalGroups"
  output <- use_taxref_api(url)
  output$`_embedded`$operationalGroups$vernacularGroup2
}

#' Verify input parameters before running the query to the TAXREF API
#'
#' This function verifies that the input parameters sp, lang, and group_fr are correctly formatted and compatible
#' with each other before running the query to the TAXREF API.
#'
#' @param sp A character string or list of character strings corresponding to the name(s) of a taxon (species or other).
#' @param lang A character string defining whether 'sp' is a vernacular ('fr') or scientific ('sci') name.
#' Default value is 'sci'.
#' @param group_fr A character string defining if a filter must be made on the taxon during the query (amphibians,
#' mammals, birds...) If specified, lang must be set to 'fr'.
#'
#' @return Returns TRUE if the input parameters are correctly formatted and compatible with each other, and stops
#' the function with an error message otherwise.
#'
#' @examples
#' \dontrun{
#' check_before_run_query(sp = "Puma concolor", lang = "sci")
#' check_before_run_query(sp = c("Puma concolor", "Anas platyrhynchos"), lang = "sci")
#' check_before_run_query(sp = "Puma concolor", lang = "fr", group_fr = "Mammifères")
#' check_before_run_query(sp = "Puma concolor", lang = "fr")
#' check_before_run_query(sp = "Puma concolor", lang = "fr", group_fr = NULL)
#' check_before_run_query(sp = "Puma concolor", lang = "sci", group_fr = "Mammifères")
#' }
#'
check_before_run_query <- function(sp, lang = "sci", group_fr = NULL) {
  if (is.character(sp) || is.list(sp)) {
    if (lang %in% c("fr", "sci")) {
      if (lang == "sci" && is.null(group_fr)) {
        TRUE
      } else if (lang == "fr") {
          if (is.character(group_fr) || is.null(group_fr)) {
            TRUE
          } else {
            stop("group_fr must be a character string or NULL value")
          }
      } else {
        stop("If group_fr is specified, lang must be set to 'fr'")
      }
    } else {
      stop("lang must be set to 'fr' or 'sci'")
    }
  } else {
    stop("sp must be a character string or list of character")
  }
}