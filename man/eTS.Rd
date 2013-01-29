\name{eTS}
\alias{eTS}
\alias{getDims}
\alias{getDims.eTS}
\alias{getInvValues}
\alias{getInvValues.eTS}
\alias{getLags}
\alias{getLags.eTS}
\alias{getNaiveForecast}
\alias{getNaiveForecast.eTS}
\alias{getnValues}
\alias{getnValues.eTS}
\alias{getOrder}
\alias{getOrder.eTS}
\alias{getSelTargets}
\alias{getSelTargets.eTS}
\alias{getTargets}
\alias{getTargets.eTS}
\alias{getValues}
\alias{getValues.eTS}
\title{Constructor for objects of the embedded time series class}
\usage{
  eTS(x, lags, name, targets)

  getDims(obj, ...)

  getOrder(obj, ...)

  getLags(obj, ...)

  getnValues(obj, ...)

  getSelTargets(obj, ...)

  \method{getDims}{eTS} (obj, ...)

  \method{getOrder}{eTS} (obj, ...)

  \method{getLags}{eTS} (obj, ...)

  \method{getnValues}{eTS} (obj, ...)

  \method{getSelTargets}{eTS} (obj, ...)

  getInvValues(obj, ...)

  \method{getInvValues}{eTS} (obj, ...)

  getValues(obj, ...)

  \method{getValues}{eTS} (obj, ...)

  getTargets(obj, ...)

  \method{getTargets}{eTS} (obj, ...)

  getNaiveForecast(obj, ...)

  \method{getNaiveForecast}{eTS} (obj, ...)
}
\arguments{
  \item{x}{data to construct the time series from. If this
  is not an \code{\link{aTS}} object, then the data is
  directly passed to \code{as.zooreg} from the zoo
  package.}

  \item{lags}{a vector containing all the lags, e.g.
  \code{lags=c(-4:0)}}

  \item{name}{is only used, if input is not an aTS object}

  \item{targets}{columns from the targets that are to be
  selected}

  \item{obj}{the \code{eTS} object}

  \item{...}{additional parameters. Currently not used by
  any of the methods.}
}
\description{
  The \code{eTS} time series class can hold a time series
  in its embedded form and provides methods for extracting
  and information from it and for manipulating it.
}

