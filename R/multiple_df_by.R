#' Duplicate a data frame a number of times equal to the length of a given list
#'
#' This function takes a data frame and a list of values and returns a new data frame by duplicating the original
#' data frame a number of times equal to the length of the list. The function also adds a new column to the end of the
#' data frame containing the values from the list. This can be useful for multiplying a calendar by a list of people,
#' for example.
#'
#' @param df a data frame to be duplicated.
#' @param by a list of values to be added as a new column in the duplicated data frames.
#'
#' @return a new data frame with the original data frame duplicated a number of times equal to the length of the list,
#' and a new column added containing the values from the list.
#' @importFrom plyr ldply
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' multiple_df_by(df, c("Alice", "Bob", "Eve"))
#'   # returns 3 copies of the original data frame,
#'   # each with a new column at the end containing the values "Alice",
#'   # "Bob", and "Eve", respectively
#'}
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