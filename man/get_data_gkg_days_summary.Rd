% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdelt_event_gkg.R
\name{get_data_gkg_days_summary}
\alias{get_data_gkg_days_summary}
\title{Gets days summary GDELT GKG data by table}
\usage{
get_data_gkg_days_summary(
  dates = c("2016-06-01"),
  is_count_file = c(T, F),
  file_directory = NULL,
  folder_name = "gdelt_data",
  remove_files = T,
  empty_trash = T,
  nest_data = F,
  return_message = T
)
}
\arguments{
\item{return_message}{}
}
\description{
Gets days summary GDELT GKG data by table
}
\examples{
get_data_gkg_days_summary(dates = c("2016-07-18"), is_count_file = c(T, F))
}
