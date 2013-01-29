\name{plot.eTS}
\alias{plot.eTS}
\title{Plot function for eTS objects}
\usage{
  \method{plot}{eTS} (x, ...)
}
\arguments{
  \item{x}{the \code{\link{eTS}} object}

  \item{...}{parameters passed to \code{plot.zoo}}
}
\description{
  Function to plot \code{\link{eTS}} objects. It just calls
  the plot function of the \code{zoo} package.
}
\details{
  TODO: Does this work in the multivariate case?
}

