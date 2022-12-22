#' Clean a data frame
#'
#' @param df is the data frame to clean
#' @param remove_space is a logical indicating if spaces should be removed from the data frame
#' @param lower_case is a logical indicating if the data frame should be converted to lower case
#' @param latin_ascii is a logical indicating if the data frame should be converted to Latin-ASCII
#'
#' @importFrom stringr str_to_lower
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_squish
#'
#' @export
clean_df <- function(df, remove_space = TRUE, lower_case = TRUE, latin_ascii = TRUE) {
  if (remove_space) {
    df <- lapply(df, str_squish)
  }
  if (lower_case) {
    df <- lapply(df, str_to_lower)
  }
  if (latin_ascii) {
    df <- lapply(df, stri_trans_general, "Latin-ASCII")
  }
  as.data.frame(df)
}