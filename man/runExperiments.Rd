\name{runExperiments}
\alias{runExperiments}
\title{Start a defined set of experiments.}
\usage{
  runExperiments(usedData, expPath, methodsDefinitions,
    tsApply = lapply, seed = 5, predictFunc = predict)
}
\arguments{
  \item{usedData}{a structure which defines the data to use
  for the experiments. Usually generated with the
  \code{\link{genUsedData}} function.}

  \item{expPath}{the path to write the experiments to. A
  reasonable path can be generated using the function
  \code{\link{assembleExperimentsPath}}.}

  \item{methodsDefinitions}{a structure that defines all
  the prediction methods you want to use. See the demo code
  for how to do it.}

  \item{tsApply}{the apply function to be used. If using
  the script on different machines with different
  parallelization capabilities, you probably want to
  automatically choose this function using
  \code{\link{initParallelMode}}.}

  \item{seed}{this seed value is set for every execution of
  a predictor.}

  \item{predictFunc}{the predict function to be used. Here,
  you can add special processing of some of the predictors
  don't use the standard format of predict functions.}
}
\value{
  the results of all experiments are returned in a list,
  but -- more important -- they are saved to disk. The
  structure in the newly created directory \code{expPath}
  is: \item{usedData}{The usedData structure is saved both
  in plain text and in the ".RData" format. This structure
  contains information on all input data used, including
  md5 hashes of the input data files.} \item{models}{a
  subdirectory with an ".RData" file for every model
  trained. Take into account that this model may or may not
  be used for further predictions. This depends on the
  implementation of the model (if it is implemented in pure
  R or in C/C++)} \item{predictions}{an ".RData" file for
  every prediction of every model, containing the
  predictions on the respective test set for this model}
  \item{results}{tables calculated using the predictions
  and the respective reference and benchmark values: All
  available error measures are calculated both for the
  training set (fit, "modEval") and the test set
  ("predEval"). For the test set, there are also
  denormalized versions calculated "Denorm". Furthermore,
  there may be some special tables as the value containing
  the R package and version for each of the models (or the
  hash of the git commit, if available).}
}
\description{
  This is one of the most important functions of the
  package. It uses the data defined by \code{usedData}, a
  structure which you can generate using the
  \code{\link{genUsedData}} function, takes the defined
  methods in the \code{methodsDefinitions} structure, and
  applies all methods defined to all data defined. When
  giving a parallel apply function in \code{tsApply}, the
  experiments are run in parallel. All outputs of the
  experiments are written to the output directory defined
  in \code{expPath}.
}
\details{
  TODO: Describe in more detail how to create the
  \code{methodsDefinitions} structure.
}

