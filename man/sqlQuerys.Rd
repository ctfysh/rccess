\name{sqlQuerys}
\alias{sqlQuerys}
\title{Call with Batch SQL Codes}
\usage{
  sqlQuerys(channel, queries, ...)
}
\arguments{
  \item{channel}{connection handle as returned by
  \code{\link{odbcConnect}}.}

  \item{query}{any valid SQL statement.}

  \item{...}{arguments to be passed to
  \code{\link{sqlQuery}}.}
}
\value{
  Values are same with \code{\link{sqlQuery}}.
}
\description{
  Submit each SQL query to a *.mdb database, and retrieve
  the results.
}

