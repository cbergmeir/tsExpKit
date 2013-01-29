\name{initParallelMode}
\alias{initParallelMode}
\title{This function determines and initializes automatically the adequate
apply function for parallel processing}
\usage{
  initParallelMode(config, packagesToLoad = c("tsExpKit"),
    sgeQueueName = "muylarga", sgeRemoveFiles = TRUE)
}
\arguments{
  \item{config}{the known configurations. See
  \code{\link{getConfigDefault}} and
  \code{\link{getConfigHercules}} for examples.}

  \item{packagesToLoad}{the packages that have to be loaded
  for versions of applies that launch new R processes.}

  \item{sgeQueueName}{for SGE: the name of the queue to
  use.}

  \item{sgeRemoveFiles}{for SGE: should the temporary files
  be deleted?}
}
\description{
  This function can be used to initialize and/or
  auto-determine the apply function to be used, in order to
  run parallel executions. The function checks the hostname
  of the machine it is running on, and then chooses from
  the config the apropriate apply function, and initializes
  it if necessary. E.g., if running on hercules, the apply
  function to be used is one from the \code{Rsge} package,
  to send parallel jobs to hercules' SGE queue. If run on a
  multicore linux system, a version of apply from the
  \code{multicore} package is used. For debugging, it is
  often necessary to turn off parallel execution.
}
\details{
  TODO: Parallel execution using the \code{snow} package is
  implemented but probably currently not working.
}

