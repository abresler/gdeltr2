% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdelt_event_gkg.R
\name{parse_gkg_mentioned_organizations}
\alias{parse_gkg_mentioned_organizations}
\title{Returns long or wide mentioned organizations from a GKG data frame}
\usage{
parse_gkg_mentioned_organizations(
  gdelt_data,
  organization_column = "organizations",
  filter_na = T,
  return_wide = T
)
}
\arguments{
\item{organization_column}{options \code{c('organization', 'organizations', 'organizations.count', 'organizationsCharLoc', 'charLoc'))}}

\item{return_wide}{}
}
\description{
Returns long or wide mentioned organizations from a GKG data frame
}
