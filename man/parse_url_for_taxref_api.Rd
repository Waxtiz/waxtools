% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_cd_nom.R
\name{parse_url_for_taxref_api}
\alias{parse_url_for_taxref_api}
\title{Parse the url to make a query to the taxref api}
\usage{
parse_url_for_taxref_api(sp, lang, group_fr)
}
\arguments{
\item{sp}{is a character string corresponding to the name of a taxon (species or other).}

\item{lang}{defines whether 'sp' is a vernacular (fr) or scientific (latin) name}

\item{group_fr}{defines if a filter must be made on the taxon during the query (amphibians, mammals, birds...)}
}
\value{
A character string containing the URL to query the TAXREF API.
}
\description{
This function generates a URL to query the TAXREF API using a taxon name and optional parameters. The name can be either a scientific or vernacular name in French, and the query can be filtered by group (e.g. amphibians, mammals). The URL generated can be used to retrieve taxonomic information from the database.
}
\examples{
\dontrun{
sp <- "Anas platyrhynchos"
lang <- "sci"
group_fr <- NULL
parse_url_for_taxref_api(sp, lang, group_fr)

sp <- "canard colvert"
lang <- "fr"
group_fr <- "Oiseaux"
parse_url_for_taxref_api(sp, lang, group_fr)
}

}
