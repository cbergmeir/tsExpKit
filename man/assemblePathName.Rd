\name{assemblePathName}
\alias{assemblePathName}
\title{Utility function to assemble a path name from two path strings}
\usage{
  assemblePathName(path1, path2)
}
\arguments{
  \item{path1}{string that represents first path}

  \item{path2}{string that represents second path}
}
\value{
  the assembled path name
}
\description{
  OBSOLETE, use system function \code{file.path} instead.
}
\details{
  Assembles two path names, e.g. a path and a filename.
  Automatically determines, if a backslash is needed.
}

