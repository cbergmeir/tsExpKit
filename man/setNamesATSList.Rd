\name{setNamesATSList}
\alias{setNamesATSList}
\title{Set the names of the \code{\link{ATSList}} correctly}
\usage{
  setNamesATSList(atsList)
}
\arguments{
  \item{atsList}{the \code{\link{ATSList}} for which the
  names to set}
}
\description{
  This method calls for every \code{\link{aTS}} object in
  the list its \code{\link{getName}} method, and the list
  element's names accordingly. E.g., if you have an aTS
  object of the \code{lynx} series, after applying this
  function to the \code{\link{ATSList}}, you can access it
  with atsList[[lynx]], instead of atsList[[1]].
}

