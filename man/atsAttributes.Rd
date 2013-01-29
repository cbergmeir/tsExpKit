\name{aTSattr}
\alias{aTSattr}
\alias{aTSattr<-}
\alias{aTSattr<-.aTS}
\alias{atsAttributes}
\alias{atsAttributes<-}
\alias{atsAttributes<-.aTS}
\title{The aTS attribute mechanism}
\usage{
  aTSattr(aTS, name)

  aTSattr(aTS, name) <- value

  aTSattr(aTS, name) <- value

  atsAttributes(aTS)

  atsAttributes(aTS) <- value

  atsAttributes(aTS) <- value
}
\arguments{
  \item{aTS}{the \code{\link{aTS}} object}

  \item{name,}{the name of the attribute}

  \item{value}{the value of the atsAttribute}
}
\value{
  the value of the attribute
}
\description{
  The attributes mechanism of the \code{\link{aTS}} class
  works as follows: All attributes are stored in a
  conventional R attribute named \code{aTSattr}, which is a
  list. To get all attributes, i.e., this list, call
  \code{atsAttributes} on the time series. You can replace
  the whole list using the \code{atsAttributes<-} method.
  To get and set single attributes, use the methods
  \code{aTSattr}, and \code{aTSattr<-}.

  Set all ats attributes
}
\examples{
ats <- aTS(lynx)

getTimeStamps(ats)
nSamples(ats)


aTSattr(ats, "test") <- 5
atsAttributes(ats)
aTSattr(ats, "test")
}

