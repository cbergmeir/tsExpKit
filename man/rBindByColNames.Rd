\name{rBindByColNames}
\alias{rBindByColNames}
\title{Row-bind together two tables, taking into account the column names}
\usage{
  rBindByColNames(x, y)
}
\arguments{
  \item{x}{first table}

  \item{y}{second table}
}
\value{
  the two tables rbind-ed together
}
\description{
  This function is very powerful and an important utility
  function of the package. This helper function implements
  a special case of \code{rbind}, where columns in both
  tables that have the same name are joined, and columns
  that only exist in one of the two source tables result in
  "NA" values for the rows in which they don't exist.
}
\seealso{
  \code{\link{cBindByRowNames}}
}

