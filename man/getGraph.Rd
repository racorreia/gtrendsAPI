% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getGraph.R
\name{getGraph}
\alias{getGraph}
\title{Extracts Google Trends search interest data}
\usage{
getGraph(
  terms,
  geo = NULL,
  property = NULL,
  category = NULL,
  startDate = NULL,
  endDate = NULL,
  api.key
)
}
\arguments{
\item{terms}{The search term(s) of interest. Up to five search terms can be queried simultaneously.}

\item{geo}{Applies a geographical restriction to data query. Parameter should be a string, please refer to ISO-3166-2 for the supported values.}

\item{property}{Defines the Google property to query data from. Options are web/images/news/froogle(Shopping)/youtube. Parameter should be a string, defaults to 'web' when unspecified.}

\item{category}{Defines a category filter for data extraction. Please consult the Trends Explore page for possible legal input here. Parameter should be a string, defaults to 'All categories' when unspecified.}

\item{startDate}{Defines the starting date for data extraction. Start date should be a month and a year in the format YYYY-MM e.g. 2010-01. Parameter should be a string, defaults to '2004-01' when unspecified.}

\item{endDate}{Defines the end date for data extraction. End date should be a month and a year in the format YYYY-MM e.g. 2010-01. Parameter should be a string, defaults to the current month and year when unspecified.}

\item{api.key}{Google API access key.}
}
\value{
Returns a time series of Google Trends search volume.
}
\description{
Extracts Google Trends search interest data
}
\examples{
\dontrun{
getGraph("apple", api.key = "YOUR_API_KEY")

getGraph(c("apple","pear","orange"), geo = "US", property = "news",
category = "Food & Drink", startDate = "2014-01", endDate = "2014-12",
api.key = "YOUR_API_KEY")
}
}
