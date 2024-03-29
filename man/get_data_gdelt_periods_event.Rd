% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdelt_event_gkg.R
\name{get_data_gdelt_periods_event}
\alias{get_data_gdelt_periods_event}
\title{Returns GDELT event data for a given periods}
\usage{
get_data_gdelt_periods_event(
  periods = c(1983, 1989),
  file_directory = NULL,
  folder_name = "gdelt_data",
  remove_files = T,
  empty_trash = T,
  return_message = T
)
}
\description{
Returns GDELT event data for a given periods
}
\examples{
get_data_gdelt_periods_event (periods = c(1983))
}
