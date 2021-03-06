% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetCountyData.R
\name{GetCountyData}
\alias{GetCountyData}
\title{Get data for counties}
\usage{
GetCountyData(
  include.regions = FALSE,
  remove.holidays = TRUE,
  states = FALSE,
  remote = FALSE
)
}
\arguments{
\item{include.regions}{include regions in addition to counties}

\item{remove.holidays}{remove major holidays}

\item{states}{get data for states}

\item{remote}{a logical value, if \code{TRUE} download all data from remotes, otherwise use local data}
}
\value{
a \code{\link[data.table]{data.table}} object \code{county.dt}
}
\description{
Get data for counties
}
