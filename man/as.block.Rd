% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constructors.R
\name{as.block}
\alias{as.block}
\title{Block Objects}
\usage{
as.block(x)
}
\arguments{
\item{x}{an object of type \code{NULL} or \code{character} or \code{block}.}
}
\value{
an object of class \code{block}.
}
\description{
In pandoc \code{block} objects are used as container for \code{inline} objects and to give them specific roles. Objects of the classes \code{NULL} and \code{character} can be coerced to \code{block}.
}
\examples{
as.block("some text")
as.block(NULL)
}
