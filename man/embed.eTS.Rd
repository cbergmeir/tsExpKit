\name{embed.eTS}
\alias{embed.eTS}
\title{Embed a time series.}
\usage{
  embed.eTS(x, m, d, lags)
}
\arguments{
  \item{x}{the times series}

  \item{m}{embedding parameter (same as in tseriesChaos)}

  \item{d}{embedding parameter (same as in tseriesChaos)}

  \item{lags}{embedding parameter (same as in
  tseriesChaos)}
}
\description{
  Generate a zoo object that contains a matrix with lagged
  versions of the time series. This function is copied from
  tseriesChaos and adapted to work for zooreg objects.
}

