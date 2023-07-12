#' Calculate cumulative richness of species over time
#'
#' @description
#' The calc_rich_sp() function calculates the cumulative richness of species observed over time.
#' It takes as input a data frame df with columns sp and date, containing species names and dates, respectively.
#' The function checks that the specified columns are present in the data frame and of the appropriate types,
#' and orders the data frame by date. It then calculates the cumulative richness by counting the number of occurrences
#' of each date, and returns the result as a data frame with columns cumulative_richness and date.
#'
#' @param df A data frame with columns `sp` and `date`.
#' @param sp The name of the column in `df` containing species names.
#' @param date The name of the column in `df` containing dates.
#'
#' @return A data frame with columns `cumulative_richness` and `date`.
#'
#' @examples
#' df <- data.frame(
#'   sp = c("Erithacus rubecula", "Gypaetus barbatus", "Gypaetus barbatus",
#'          "Gypaetus barbatus", "Otus scops", "Otus scops"),
#'   date = as.Date(c("2020-01-01", "2020-01-12", "2020-01-22",
#'                   "2020-02-04", "2020-02-04", "2020-02-21")),
#'   stringsAsFactors = FALSE
#' )
#'
#' calc_rich_sp(df, sp = "sp", date = "date")
#'
#' @export
calc_rich_sp <- function(.data, sp, date) {
  sp <- quo_name(enquo(sp))
  date <- quo_name(enquo(date))

  # Order .data by date
  .data <- .data[order(.data[[date]]),]

  # Get list of unique species
  unique_species <- unique(.data[, sp])

  # Find first occurrence of each species
  indices <- sapply(unique_species, function(x) match(x, .data[, sp]))

  # Count number of occurrences of each date
  dates <- .data[indices, date]
  occurrences <- table(dates)

  # Calculate cumulative sum of unique species observed over time
  cumulative_richness <- cumsum(occurrences)

  cumulative_richness <- as.data.frame(cumulative_richness)
  cumulative_richness$date <- row.names(cumulative_richness)
  rownames(cumulative_richness) <- seq.int(nrow(cumulative_richness))

  # Return result
  return(cumulative_richness)
}
