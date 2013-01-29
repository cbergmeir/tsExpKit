\name{multiplyIntervalBoundaries}
\alias{multiplyIntervalBoundaries}
\title{Determine new boundaries of a multiplied interval}
\usage{
  multiplyIntervalBoundaries(minX, maxX, minY, maxY)
}
\arguments{
  \item{minX}{the lower bound of the first interval}

  \item{maxX}{the upper bound of the first interval}

  \item{minY}{the lower bound of the second interval}

  \item{maxY}{the upper bound of the second interval}
}
\description{
  This function is a helper function to solve the following
  problem: You have two variables that you multiply, and
  you know for each variable the interval of values it can
  take. Then, this function calculates the interval of
  values the product can take.
}
\examples{
multiplyIntervalBoundaries(0, 25, -2, 4.5)
}

