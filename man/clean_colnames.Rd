% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_df.R
\name{clean_colnames}
\alias{clean_colnames}
\title{Clean column names of a data frame}
\usage{
clean_colnames(
  df,
  remove_space = TRUE,
  lower_case = TRUE,
  latin_ascii = TRUE,
  regex_colnames = TRUE
)
}
\arguments{
\item{df}{A data frame. The data frame to clean the column names of.}

\item{remove_space}{A logical indicating if spaces should be removed from the column names of the data frame.
Default is TRUE.}

\item{lower_case}{A logical indicating if the column names should be converted to lower case. Default is TRUE.}

\item{latin_ascii}{A logical indicating if the column names should be converted to Latin-ASCII. Default is TRUE.}

\item{regex_colnames}{A logical indicating if the column names should be cleaned up using a predefined regular
expression pattern. Default is TRUE.}
}
\value{
a data frame with cleaned column names.
}
\description{
This function takes a data frame as input and applies several cleaning operations on the column names of the
data frame. These operations include removing spaces, converting to lower case, converting to Latin-ASCII,
and applying a regular expression to clean up the names of the columns.
}
