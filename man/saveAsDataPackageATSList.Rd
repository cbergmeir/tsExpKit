\name{saveAsDataPackageATSList}
\alias{saveAsDataPackageATSList}
\title{Save an \code{\link{ATSList}} as a (new) data package}
\usage{
  saveAsDataPackageATSList(atsList, dataPath,
    dataPackageName, genSummaryPlot = TRUE,
    genDescStatTab = TRUE, genStatNonlinTab = FALSE,
    genStatOtherTestsTab = FALSE, genSingeFilesDir = TRUE,
    minLength = 10)
}
\arguments{
  \item{atsList}{the \code{\link{ATSList}} to use}

  \item{dataPath}{the path to which to save the data
  package to}

  \item{dataPackageName}{the name for the data package}

  \item{genSummaryPlot}{if \code{TRUE},
  \code{\link{savePlotsATSList}} is used to generate one
  pdf containing all series.}

  \item{genDescStatTab}{if \code{TRUE}, the "DescStat"
  table is generated}

  \item{genStatNonlinTab}{if \code{TRUE}, the "StatNonlin"
  table is generated}

  \item{genStatOtherTestsTab}{if \code{TRUE}, the
  "OtherTests" table is generated}

  \item{genSingeFilesDir}{if \code{TRUE}, the series are
  also saved in single-file mode to disk}

  \item{minLength}{series below this length will be
  omitted. For some of the measures calculated for the
  characteristics tables, a certain minimal length of the
  series might be necessary.}
}
\description{
  This function automatizes the import of an
  \code{\link{ATSList}} to a data repository as a new data
  package, by calling sequentially
  \code{\link{saveATSList}} with single-file and
  all-in-one-file mode, \code{\link{savePlotsATSList}}, and
  \code{\link{summaryTableATSList}} with the according
  types.
}

