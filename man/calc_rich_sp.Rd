% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_rich_sp.R
\name{calc_rich_sp}
\alias{calc_rich_sp}
\title{Calculate cumulative richness of species over time}
\usage{
calc_rich_sp(.data, sp, date)
}
\arguments{
\item{sp}{The name of the column in `df` containing species names.}

\item{date}{The name of the column in `df` containing dates.}

\item{df}{A data frame with columns `sp` and `date`.}
}
\value{
A data frame with columns `cumulative_richness` and `date`.
}
\description{
The calc_rich_sp() function calculates the cumulative richness of species observed over time.
It takes as input a data frame df with columns sp and date, containing species names and dates, respectively.
The function checks that the specified columns are present in the data frame and of the appropriate types,
and orders the data frame by date. It then calculates the cumulative richness by counting the number of occurrences
of each date, and returns the result as a data frame with columns cumulative_richness and date.
}
\examples{
df <- data.frame(
  sp = c("Erithacus rubecula", "Gypaetus barbatus", "Gypaetus barbatus",
         "Gypaetus barbatus", "Otus scops", "Otus scops"),
  date = as.Date(c("2020-01-01", "2020-01-12", "2020-01-22",
                  "2020-02-04", "2020-02-04", "2020-02-21")),
  stringsAsFactors = FALSE
)

calc_rich_sp(df, sp = "sp", date = "date")

}
