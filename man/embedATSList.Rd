\name{embedATSList}
\alias{embedATSList}
\title{Embed an \code{\link{ATSList}}}
\usage{
  embedATSList(atsList, ...)
}
\arguments{
  \item{atsList}{the \code{\link{ATSList}} to which to
  apply the function to}

  \item{...}{parameters passed to the \code{\link{eTS}}
  constructor}
}
\description{
  This method constructs from every \code{\link{aTS}}
  object in the list an \code{\link{eTS}} object.
}

