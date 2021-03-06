% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/juantools.R
\name{avg}
\alias{avg}
\title{Compute mean by ignoring NAs}
\usage{
avg(x)
}
\arguments{
\item{x}{Numeric vector}
}
\value{
Mean of x values, ignoring NAs


}
\description{
Compute mean, with option na.rm=TRUE by default
}
\examples{
x = c(2, 5, NA, 7)
mean(x)
avg(x)

}
