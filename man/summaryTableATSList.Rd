\name{summaryTableATSList}
\alias{summaryTableATSList}
\title{Calculate a summary table of an \code{\link{ATSList}}}
\usage{
  summaryTableATSList(atsList, ...)
}
\arguments{
  \item{atsList}{the \code{\link{ATSList}} to which to
  apply the function to}

  \item{...}{parameters passed to
  \code{\link{oneLineSummary}}}
}
\description{
  This method calls for every \code{\link{aTS}} object in
  the list the function \code{\link{oneLineSummary}}, and
  returns a table that consists of all these summaries.
}

