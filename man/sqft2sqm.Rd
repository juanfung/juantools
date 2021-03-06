% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/juantools.R
\name{sqft2sqm}
\alias{sqft2sqm}
\title{Convert numeric square feet to square meters}
\usage{
sqft2sqm(x, reverse = FALSE)
}
\arguments{
\item{x}{Numeric; assumed units are sq ft}

\item{reverse}{Logical; if \code{TRUE} then x is converted to sq ft;
default is \code{FALSE}}
}
\value{
x Numeric, converted to appropriate units
}
\description{
This function can convert from square feet to square meters and
vice versa
}
\examples{
sqft2sqm(1000)
sqft2sqm(sqft2sqm(1000), reverse=TRUE)

}
