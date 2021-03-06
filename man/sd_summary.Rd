% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/juantools.R
\name{sd_summary}
\alias{sd_summary}
\title{Summary statistics for vector with standard deviation}
\usage{
sd_summary(x, ...)
}
\arguments{
\item{x}{A numeric vector}

\item{...}{Other arguments passed to summary}
}
\value{
Summary + standard deviation
}
\description{
Function that appends standard deviation to vector summary
statistics, and ignores NAs by default
}
\examples{
x = c(2, 5, NA, 7)
sd_summary(x)

}
