\name{sqlFields}
\alias{sqlFields}
\title{Read Tables and Fields under Each Table in the Database}
\usage{
  sqlFields(channel, tableType = "TABLE", ...)
}
\arguments{
  \item{channel}{connection handle as returned by
  \code{\link{odbcConnect}}}

  \item{tableType}{specify zero or more types in separate
  elements of a character vector}

  \item{...}{arguments to be passed to
  \code{\link{file.copy}}}
}
\value{
  A list of the fields in each table will be returned
}
\description{
  Create a list of the fields in each table.
}

