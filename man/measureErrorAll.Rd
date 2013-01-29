\name{measureErrorAll}
\alias{measureErrorAll}
\title{This function calculates all error measures that are available}
\usage{
  measureErrorAll(prediction, reference, benchmark)
}
\arguments{
  \item{prediction}{a vector containing the predictions}

  \item{reference}{a vector containing the "true" reference
  values}

  \item{benchmark}{if needed by the error measure, a
  benchmark forecast, such as the naive forecast (always
  take the last known value as forecast).}
}
\value{
  a \code{data.frame} containing all the measures
}
\description{
  This function calls \code{\link{measureError}} for all
  error measures available.
}

