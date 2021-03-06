% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Chebyshev_distance.R
\name{Chebyshev_distance}
\alias{Chebyshev_distance}
\title{Chebyshev distance}
\usage{
Chebyshev_distance(x, y)
}
\arguments{
\item{x}{a vector containing the track record of the underlying asset/strategy}

\item{y}{a vector containing the track record of the underlying asset/strategy}
}
\value{
The Chebyshev distance of the two vectors
}
\description{
Calculates the Chebyshev distance
}
\examples{

x = rnorm(1000)
y = rnorm(1000)

chebyshev_dist = Chebyshev_distance(x, y)

}
\references{
https://en.wikipedia.org/wiki/Chebyshev_distance
}
\author{
Tasos Grivas <tasos@openriskcalculator.com>
}
