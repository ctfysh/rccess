\name{createdb}
\alias{createdb}
\title{Create a New *.mdb Database}
\usage{
createdb(dbname = "newdb", extension = ".mdb",
  emptydb = system.file(paste("emptydb/EmptyDB", extension, sep = ""), package
  = "rccess"), dir = getwd(), ...)
}
\arguments{
  \item{dbname}{the database name which you create.}

  \item{extension}{the extension of the database.}

  \item{emptydb}{the empty database you will copy.}

  \item{dir}{the directory of your new created database.}

  \item{...}{arguments to be passed to
  \code{\link{file.copy}}.}
}
\value{
A new *.mdb database.
}
\description{
A new *.mdb database will be created by copying the
"EmptyDB.mdb" file with your database name.
}

