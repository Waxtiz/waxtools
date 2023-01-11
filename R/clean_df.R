#' Clean column names of a data frame
#'
#' This function takes a data frame as input and applies several cleaning operations on the column names of the
#' data frame. These operations include removing spaces, converting to lower case, converting to Latin-ASCII,
#' and applying a regular expression to clean up the names of the columns.
#'
#' @param df A data frame. The data frame to clean the column names of.
#' @param remove_space A logical indicating if spaces should be removed from the column names of the data frame.
#' Default is TRUE.
#' @param lower_case A logical indicating if the column names should be converted to lower case. Default is TRUE.
#' @param latin_ascii A logical indicating if the column names should be converted to Latin-ASCII. Default is TRUE.
#' @param regex_colnames A logical indicating if the column names should be cleaned up using a predefined regular
#' expression pattern. Default is TRUE.
#'
#' @return a data frame with cleaned column names.
#'
#' @importFrom stringr str_to_lower str_squish
#' @importFrom stringi stri_trans_general
#'
#' @export
clean_colnames <- function(df, remove_space = TRUE, lower_case = TRUE, latin_ascii = TRUE, regex_colnames = TRUE) {

  col_names <- colnames(df)

  if (remove_space) {
    col_names <- stringr::str_squish(col_names)
  }
  if (lower_case) {
    col_names <- stringr::str_to_lower(col_names)
  }
  if (latin_ascii) {
    col_names <- stringi::stri_trans_general(col_names, "Latin-ASCII")
  }
  if (regex_colnames) {
    # remove all "x." or "X." in column names (sometimes added by R)
    col_names <- stringr::str_replace_all(col_names, "x.|X.", "")
    # deletes all non-alphanumeric characters at the end or beginning of the column name
    col_names <- stringr::str_replace_all(col_names,   "^[^[:alnum:]]+", "")
    col_names <- stringr::str_replace_all(col_names, "[^[:alnum:]]+$", "")
    # replace the characters "." or ".." (sometimes added by R) by "_"
    col_names <- stringr::str_replace_all(col_names, "\\.\\.|\\.", "_")
  }

  colnames(df) <- col_names
  return(df)
}



#' Clean a data frame
#'
#' This function takes a data frame as input and applies several cleaning operations on it. These operations include
#' removing spaces, converting to lower case, converting to Latin-ASCII, and cleaning column names using the function
#' `clean_colnames()`.
#'
#' @param df A data frame. The data frame to clean.
#' @param remove_space A logical indicating if spaces should be removed from the data frame. Default is TRUE.
#' @param lower_case A logical indicating if the data frame should be converted to lower case. Default is TRUE.
#' @param latin_ascii A logical indicating if the data frame should be converted to Latin-ASCII. Default is TRUE.
#' @param clean_colnames A logical indicating if the column names should also be cleaned. Default is TRUE.
#' @param regex_colnames A logical indicating if the column names should be cleaned up using a predefined regular
#' expression pattern. Default is TRUE.
#'
#' @return a data frame with cleaned and formatted data.
#'
#' @importFrom stringr str_to_lower str_squish
#' @importFrom stringi stri_trans_general
#'
#' @export
clean_df <- function(df, remove_space = TRUE, lower_case = TRUE, latin_ascii = TRUE,
                     clean_colnames = TRUE, regex_colnames = TRUE) {

  if (remove_space) {
    df <- lapply(df, str_squish)
  }
  if (lower_case) {
    df <- lapply(df, str_to_lower)
  }
  if (latin_ascii) {
    df <- lapply(df, stri_trans_general, "Latin-ASCII")
  }
  if (clean_colnames) {
    df <- clean_colnames(df, remove_space, lower_case, latin_ascii, regex_colnames)
  }
  if (sum(duplicated(df) == T) > 0) {
    warning("Be careful, duplicate rows have been detected in the dataset (with the duplicated() function).")
  }

  as.data.frame(df)
}