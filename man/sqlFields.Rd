% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/accessfun.r
\name{sqlFields}
\alias{sqlFields}
\title{Read Tables and Fields under Each Table in the Database}
\usage{
sqlFields(channel, ...)
}
\arguments{
\item{channel}{connection handle as returned by \code{\link{odbcConnect}}.}

\item{...}{arguments to be passed to \code{\link{file.copy}}.}
}
\value{
A list of the fields in each table will be returned.
}
\description{
Create a list of the fields in each table.
}

