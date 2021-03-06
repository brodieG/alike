% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type.R
\name{type_of}
\alias{type_of}
\title{A Fuzzier Version \code{\link{typeof}}}
\usage{
type_of(object)
}
\arguments{
\item{object}{the object to check the type of}
}
\value{
character(1L) the type of the object
}
\description{
Numerics that are equivalent to integers (e.g \code{x == floor(x)}) are
classified as integers, and builtin and special functions are reported as
closures.
}
\examples{

type_of(1.0001)                     # numeric
type_of(1.0)                        # integer (`typeof` returns numeric)
type_of(1)                          # integer (`typeof` returns numeric)
type_of(sum)                        # closure (`typeof` returns builtin)
type_of(`$`)                        # closure (`typeof` returns special)
}
