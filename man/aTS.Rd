\name{aTS}
\alias{[.aTS}
\alias{aTS}
\alias{getComment}
\alias{getComment.aTS}
\alias{getName}
\alias{getName.aTS}
\alias{getTimeStamps}
\alias{getTimeStamps.aTS}
\alias{nSamples}
\alias{nSamples.aTS}
\alias{setComment}
\alias{setComment.aTS}
\alias{setName}
\alias{setName.aTS}
\title{Constructor for objects of the attributed time series class}
\usage{
  aTS(x, ..., name = deparse(substitute(x)),
    comment = NULL)

  getTimeStamps(obj, ...)

  \method{getTimeStamps}{aTS} (obj, ...)

  aTS(x, ..., name=deparse(substitute(x)), comment=NULL)

  getName(obj, ...)

  \method{getName}{aTS} (obj, ...)

  setName(obj, ...)

  \method{setName}{aTS} (obj, name, ...)

  getComment(obj, ...)

  \method{getComment}{aTS} (obj, ...)

  setComment(obj, ...)

  \method{setComment}{aTS} (obj, comment, ...)

  nSamples(obj, ...)

  \method{nSamples}{aTS} (obj, ...)
}
\arguments{
  \item{x}{data to construct the time series from. This is
  directly passed to \code{as.zooreg} from the zoo
  package.}

  \item{...}{parameters passed to the \code{as.zooreg}
  function}

  \item{name}{a name for the time series}

  \item{comment}{a commentary describing the contents of
  the series}

  \item{obj}{the \code{\link{aTS}} object}
}
\value{
  a new aTS object
}
\description{
  The attributed time series class (\code{aTS}) extends the
  \code{zooreg} class from the zoo package. It implements
  its own mechanism for attributes, similar to xts, see
  \code{\link{atsAttributes}}. Furthermore, there are the
  following convenience methods for getting and setting
  some special attributes directly: \code{getName},
  \code{setName}, \code{getComment}, \code{setComment}.
  Also, there is a method \code{getTimeStamps}, which in
  directly returns the result of the \code{index} function
  from \code{zoo}. The method \code{nSamples} returns the
  number of samples, i.e., the number of rows of the data
  matrix. Furthermore, there is the index operator
  \code{[]} defined on the class.
}
\examples{
ats <- aTS(lynx)

getTimeStamps(ats)
nSamples(ats)


aTSattr(ats, "test") <- 5
atsAttributes(ats)
aTSattr(ats, "test")
}

