% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/juantools.R
\name{as.num.char}
\alias{as.num.char}
\title{Convert a string to numeric}
\usage{
as.num.char(x)
}
\arguments{
\item{x}{A string representing a number}
}
\value{
x A numeric representation of x
}
\description{
Convert any character to numeric, drop commas, dollar signs, etc
}
\examples{
as.num.char('$1,000.00')

}
