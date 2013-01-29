\name{points.eTS}
\alias{points.eTS}
\title{Points function for eTS objects}
\usage{
  \method{points}{eTS} (x, ...)
}
\arguments{
  \item{x}{the \code{\link{eTS}} object}

  \item{...}{parameters passed to \code{points.zoo}}
}
\description{
  Function to plot \code{\link{eTS}} objects into an
  existing plot. It just calls the points function of the
  \code{zoo} package.
}
\details{
  TODO: Does this work in the multivariate case?
}

