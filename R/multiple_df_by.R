#' Multiplies a dataframe by values
#'
#' @param df is a dataframe.
#' @param by is liste of value (liste or column).
#'
#' @importFrom plyr ldply
#'
#' @export
multiple_df_by <- function(df, by) {
  list_df <- list()
  for (i in seq_along(by)) {
    list_df[[length(list_df)+1]] <- df
    names(list_df)[i] <- by[i]
  }
  res <- ldply(list_df, data.frame)
  return(res)
}