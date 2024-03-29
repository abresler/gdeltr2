% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdelt_event_gkg.R
\name{gkg_tv_days}
\alias{gkg_tv_days}
\title{Gets GKG TV data for specified days}
\usage{
gkg_tv_days(
  dates = c("2016-06-01", "2016-02-01"),
  only_most_recent = F,
  return_message = T
)
}
\arguments{
\item{dates}{specified dates, year, month day format}

\item{only_most_recent}{returns only the most recent period
\code{c(T, F)}}

\item{return_message}{}
}
\description{
Gets GKG TV data for specified days
}
