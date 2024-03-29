% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/v3_entities.R
\name{v3_entity_api}
\alias{v3_entity_api}
\title{V3 Entity API data}
\usage{
v3_entity_api(
  start_date = Sys.Date() - 1,
  end_date = Sys.Date(),
  return_message = T
)
}
\arguments{
\item{start_date}{start date in YMD format}

\item{end_date}{if not \code{NULL} end date}

\item{return_message}{if \code{TRUE} return message}
}
\description{
Acquires data from the GDELT
entity mention API for period of specified dates
}
