\name{runPredictions}
\alias{runPredictions}
\title{Calculates predictions from a list of models and input data, models are on disk}
\usage{
  runPredictions(usedData, expPath, predictFunc = predict)
}
\arguments{
  \item{usedData}{the usedData structure to use (see
  \code{\link{genUsedData}})}

  \item{expPath}{both the output path, and the path where
  the models are loaded from. See
  \code{\link{assembleExperimentsPath}} and
  \code{\link{runExperiments}}}

  \item{predictFunc}{the function that is applied to the
  model to get the prediction}
}
\description{
  This function uses a bunch of models, created with
  \code{\link{runExperiments}}, and input data defined in
  \code{usedData}, together with a prediction function to
  calculate predictions for all data (in the test sets)
  applying all the trained models. The difference between
  this function and the function
  \code{\link{runPrediction}} is, that the models are
  loaded from disk. Thus, it can be ran indepentdently of
  the experiments, and e.g., can also be used if the
  experiments haven't terminated yet, were aborted, etc.
  The drawback is, that not all models survive a save/load
  cycle without problems.
}
\details{
  TODO: This function is not needed/used any more, as the
  predictions are calculated directly in
  \code{\link{runExperiments}} after model building. Maybe
  there are still some cases, when for some reason
  prediction failed, but models are there, so that this
  function can be used to calculate them.
}
\seealso{
  \code{\link{genUsedData}}, \code{\link{runExperiments}},
  \code{\link{runExperiments}}, \code{\link{runPrediction}}
}

