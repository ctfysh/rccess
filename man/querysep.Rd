% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/accessfun.r
\name{querysep}
\alias{querysep}
\title{Extract SQL Codes}
\usage{
querysep(file)
}
\arguments{
\item{file}{the filename of SQL codes.}
}
\value{
A string vector with several single SQL codes.
}
\description{
Separete the SQL codes into several single ones.
}
\details{
This function is used to separete the SQL codes into several single ones
which can be easily called by Mircrosoft Access with R package RODBC.
}

