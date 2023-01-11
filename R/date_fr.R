#' Formats date in French format
#'
#' This function takes a date as input and reformats it in French format (dd/mm/yyyy)
#'
#' @param date a date object to format in French format
#' @param sep character separator. Default is "/".
#'
#' @return a character vector of the date reformatted in French format
#'
#' @importFrom lubridate day month year
#'
#' @export
date_fr <- function(date, sep = "/") {

  day <- day(date)
  month <- month(date)
  year <- year(date)

  day <- ifelse(day < 10, paste0("0", day), day)
  month <- ifelse(month < 10, paste0("0", month), month)

  paste0(day, sep,
         month, sep,
         year)
}