\name{normalize}
\alias{normalize}
\alias{normalize.aTS}
\title{normalize an \code{\link{aTS}} object}
\usage{
  normalize(obj, ...)

  \method{normalize}{aTS} (obj, type = "norm",
    normParams = type, ...)
}
\arguments{
  \item{obj}{the \code{\link{aTS}} object}

  \item{...}{additional function parameters (currently not
  used)}

  \item{type}{the type of the normalization}

  \item{normParams}{normalization parameters to be used.
  When omitted, the type is used and the parameters are
  calculated from the data.}
}
\description{
  This function calls the function \code{\link{normalize}}
  on the \code{coredata} of the \code{\link{aTS}} object
}

