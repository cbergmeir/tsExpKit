\name{assembleExperimentsPath}
\alias{assembleExperimentsPath}
\title{Assemble a path name for an experiment run}
\usage{
  assembleExperimentsPath(resultsComment,
    resultsPath = "/home/ts/results/")
}
\arguments{
  \item{resultsComment}{a string that will be part of the
  directory name, naming the experiments}

  \item{resultsPath}{the path where to generate the
  subdirectory for the results}
}
\value{
  a string which is a path of to the directory to be
  created for the experiments
}
\description{
  This function helps you giving your experiment runs
  meaningful names. It composes a name from the current
  data, the name of the machine the experiments were run
  on, your user name, and a given comment
}

