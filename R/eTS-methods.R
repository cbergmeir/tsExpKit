
#' Function to plot \code{\link{eTS}} objects. It just calls the plot function of the \code{zoo} package. 
#' 
#' TODO: Does this work in the multivariate case?
#' 
#' @title Plot function for eTS objects
#' @param x the \code{\link{eTS}} object
#' @param ... parameters passed to \code{plot.zoo}
#' @S3method plot eTS
#' @method plot eTS
plot.eTS <- function(x,...) {
  zoo:::plot.zoo(x[,getnValues(x)+1],...)
}

#' Function to plot \code{\link{eTS}} objects into an existing plot. It just calls the points function of the \code{zoo} package. 
#' 
#' TODO: Does this work in the multivariate case?
#' 
#' @param x the \code{\link{eTS}} object
#' @param ... parameters passed to \code{points.zoo}
#' @title Points function for eTS objects
#' @S3method points eTS
#' @method points eTS
points.eTS <- function(x,...) {
  zoo:::points.zoo(x[,getnValues(x)+1],...)
}

#' @param order For \code{\link{eTS}} objects: If the \code{order} is missing, it is extracted for the attributes of the series.
#' @rdname computePartitions
#' @S3method computePartitions eTS
#' @method computePartitions eTS
computePartitions.eTS <- function(obj, order, ...) {
  
  if(missing(order)) order <- getOrder(obj)
  
  NextMethod(obj, order=order,...)
}


  
##This implementation of normalization functions makes only sense for "classic" regression,
##or time series forcasting with only exogenous variables
#
#normalize <- function(obj, ...) UseMethod("normalize")
#
##' @S3method normalize eTS
##' @method normalize eTS
#normalize.eTS <- function(obj, type="norm", normParamsValues=type, normParamsTargets=type) {
#  
#  res <- obj
#  
#  normValues <- normalizeData(getValues(obj), normParamsValues)
#  normTargets <- normalizeData(getTargets(obj), normParamsTargets)
#  
#  coredata(res) <- cbind(normValues, normTargets)
#  
#  aTSattr(res, "normParamsValues") <- attr(normValues, "normParams")
#  aTSattr(res, "normParamsTargets") <- attr(normTargets, "normParams")
#  res
#}
#
#getNormParamsValues <- function(obj, ...) UseMethod("getNormParamsValues")
#getNormParamsTargets <- function(obj, ...) UseMethod("getNormParamsTargets")
#
##' @S3method getNormParamsValues eTS
##' @method getNormParamsValues eTS
#getNormParamsValues.eTS <- function(obj) {
#  aTSattr(obj, "normParamsValues")
#}
#
##' @S3method getNormParamsTargets eTS
##' @method getNormParamsTargets eTS
#getNormParamsTargets.eTS <- function(obj) {
#  aTSattr(obj, "normParamsTargets")
#}

