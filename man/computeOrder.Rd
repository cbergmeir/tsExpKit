\name{computeOrder}
\alias{computeOrder}
\title{Compute the order from a lags vector}
\usage{
  computeOrder(lags)
}
\arguments{
  \item{lags}{a vector containing the lags, e.g.
  \code{lags=-4:0}.}
}
\description{
  This function computes simply \code{max(lags)-min(lags)}.
}

