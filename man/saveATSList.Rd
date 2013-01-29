\name{saveATSList}
\alias{saveATSList}
\title{Save an \code{\link{ATSList}} to an ATS directory}
\usage{
  saveATSList(atsList, dataPath, name, singleFiles = FALSE)
}
\arguments{
  \item{atsList}{the \code{\link{ATSList}} to save to disk}

  \item{dataPath}{the path to which to save to}

  \item{name}{the name under which to save the list}

  \item{singleFiles}{if \code{TRUE}, every series is saved
  as a single file, using \code{\link{save.aTS}}.
  Otherwise, all series are saved in one file, using the
  normal \code{save} function.}
}
\description{
  This function saves an \code{\link{ATSList}} to an ATS
  directory.
}

