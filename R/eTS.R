#' Generate a zoo object that contains a matrix with lagged versions of the time series.
#' This function is copied from tseriesChaos and adapted to work for zooreg objects.
#' 
#' @param x the times series
#' @param m embedding parameter (same as in tseriesChaos)
#' @param d embedding parameter (same as in tseriesChaos)
#' @param lags embedding parameter (same as in tseriesChaos)
#' @title Embed a time series.
#' @export 
embed.eTS <- function (x, m, d, lags) {
  
  if (is.null(colnames(x))) {
    vars <- sprintf("V%d", 1:NCOL(x))
  }
  else {
    vars <- colnames(x)
  }
  
  if(!inherits(x, "zooreg")) {
    x <- as.zooreg(x)  
  }
  
  if (missing(lags)) {
    
    n <- length(x) - (m - 1) * d
    
    if (n <= 0) 
      stop("Not enough points to handle these parameters")
        
    lags <- ((1:m) - 1) * d
  }
  names <- sprintf("%s/%d", vars, lags[1])
  res <- lag(x, lags[1])
  if (length(lags) > 1) {
    for (i in 2:length(lags)) {
      res <- merge(res, lag(x, lags[i]), all=FALSE)
      names <- append(names, sprintf("%s/%d", vars, lags[i]))
    }
  }
  #res <- matrix(res, nr = nrow(res), nc = ncol(res))
  colnames(res) <- names
  res
}

#' This function computes simply \code{max(lags)-min(lags)}.
#' 
#' @title Compute the order from a lags vector
#' @param lags a vector containing the lags, e.g. \code{lags=-4:0}.
#' @export
computeOrder <- function(lags)
  max(lags)-min(lags)

#' The \code{eTS} time series class can hold a time series in its embedded form and provides methods for 
#' extracting and information from it and for manipulating it. 
#' 
#' @title Constructor for objects of the embedded time series class
#' @param x data to construct the time series from. If this is not an \code{\link{aTS}} object, 
#' then the data is directly passed to \code{as.zooreg} from the zoo package. 
#' @param lags a vector containing all the lags, e.g. \code{lags=c(-4:0)}
#' @param name is only used, if input is not an aTS object
#' @param targets columns from the targets that are to be selected 
# @return an eTS object 
#' @export
eTS <- function(x, lags, name, targets) { #, indices=-1) { # m, d=1, steps=d
  
  if(missing(name))
    name <- deparse(substitute(x))
  
  order <- computeOrder(lags)
  
  if(!inherits(x, "zooreg")) {
    x <- as.zooreg(x)  
  }
  
  dims <- ncol(as.matrix(coredata(x)))
    
  if(missing(targets)) {
    selTargets <- 1:dims
  } else {
    selTargets <- targets
  } 
    
  #if(NCOL(x)>1)
  #  stop("only univariate time series are allowed")
  
  if(any(is.na(x)))
    stop("missing values not allowed")
  
  if( missing(lags) )
    stop("missing argument: 'lags' is necessary")
  
  if(order > length(x))
    stop("time series too small to handle these embedding parameters")
  
  eSeries <- embed.eTS(x, lags=lags )
  
  #colnames(eSeries) <- paste("Lag", lags, sep="")
  
  #eTS <- zoo(eSeries, index(x)[(order+1):length(x)])
  #eTS <- aTS(eSeries, index(x)[(order+1):length(x)], name=name)

  eTS <- aTS(eSeries, name=name)
    
  index(eTS) <- index(x)[(order+1):nrow(as.matrix(coredata(x)))]
  
  #eTS
  
  #if input was already an ats object, use its attributes
  if(inherits(x, "aTS")) {
    atsAttributes(eTS) <- atsAttributes(x)  
  }
  
  class(eTS) <- c("eTS", class(eTS))
  
  
  #length(lags)-1
  atsAttributes(eTS) <- append(atsAttributes(eTS), list(lags=lags, order=order, dims=dims, selTargets=selTargets, nValues=ncol(eTS)-dims))

  eTS
}

#' @param obj the \code{eTS} object
#' @param ... additional parameters. Currently not used by any of the methods.
#' @export 
#' @rdname eTS
getDims <- function(obj, ...) UseMethod("getDims")

#' @export 
#' @rdname eTS
getOrder <- function(obj, ...) UseMethod("getOrder")

#' @export 
#' @rdname eTS
getLags <- function(obj, ...) UseMethod("getLags")

#' @export 
#' @rdname eTS
getnValues <- function(obj, ...) UseMethod("getnValues")

#' @export 
#' @rdname eTS
getSelTargets <- function(obj, ...) UseMethod("getSelTargets")

#' @S3method getDims eTS
#' @method getDims eTS
#' @rdname eTS
getDims.eTS <- function(obj, ...) {
  
  #if dims is not set, return 1
  #this is for compatibility with 
  #ets objects that were generated 
  #with older versions of the function
  
  if(is.null(aTSattr(obj, "dims"))) 
    res <- 1
  else 
    res <- aTSattr(obj, "dims")
  
  res
}

#' @S3method getOrder eTS
#' @method getOrder eTS
#' @rdname eTS
getOrder.eTS <- function(obj, ...) aTSattr(obj, "order")

#' @S3method getLags eTS
#' @method getLags eTS
#' @rdname eTS
getLags.eTS <- function(obj, ...) aTSattr(obj, "lags")

#' @S3method getnValues eTS
#' @method getnValues eTS
#' @rdname eTS
getnValues.eTS <- function(obj, ...) aTSattr(obj, "nValues")

#' @S3method getSelTargets eTS
#' @method getSelTargets eTS
#' @rdname eTS
getSelTargets.eTS <- function(obj, ...) {
  
  #if dims is not set, return 1
  #this is for compatibility with 
  #ets objects that were generated 
  #with older versions of the function
  
  if(is.null(aTSattr(obj, "selTargets"))) 
    res <- 1
  else 
    res <- aTSattr(obj, "selTargets")
  
  res    
}

#' @export 
#' @rdname eTS
getInvValues <- function(obj, ...) UseMethod("getInvValues")

#' @S3method getInvValues eTS
#' @method getInvValues eTS
#' @rdname eTS
getInvValues.eTS <- function(obj, ...) {
  
  #data <- coredata(obj)
  #as.matrix(data[, rev(1:(ncol(data)-1))])
  data <- getValues(obj)
  data[, ncol(data):1]
}

#' @export 
#' @rdname eTS
getValues <- function(obj, ...) UseMethod("getValues")

#' @S3method getValues eTS
#' @method getValues eTS
#' @rdname eTS
getValues.eTS <- function(obj, ...) {
  
  data <- coredata(obj)
  as.matrix(data[, 1:getnValues(obj)])
  
  #as.matrix(data[, 1:(ncol(data)-1)])
  
}

#' @export 
#' @rdname eTS
getTargets <- function(obj, ...) UseMethod("getTargets")

#' @S3method getTargets eTS
#' @method getTargets eTS
#' @rdname eTS
getTargets.eTS <- function(obj, ...) {
  
  data <- coredata(obj)
  #data[, ncol(data)]
  data[, getnValues(obj)+getSelTargets(obj)]
  
}

#' @export
#' @rdname eTS
getNaiveForecast <- function(obj, ...) UseMethod("getNaiveForecast")

#' @S3method getNaiveForecast eTS
#' @method getNaiveForecast eTS
#' @rdname eTS
getNaiveForecast.eTS <- function(obj, ...) {
  #getInvValues(obj)[,1]
  
  data <- coredata(obj)
  data[, (getnValues(obj)-getDims(obj))+getSelTargets(obj)]
}

#TODO: what is this method good for?
#getdX1 <- function(obj, ...) {
#  getTargets(obj)
#}


getDiffInvValues <- function(obj, ...) {
  diff(getInvValues(obj))
}

getDiffTargets <- function(obj, ...) {
  diff(getTargets(obj))
}

calculateLinearCoefficients <- function(A, b){
  #x <- lm.fit(x = A, y = b)$coefficients
  #x <- lm(b ~ . -1, data = data.frame(A))$coefficients  
  x <- as.vector(ginv(A) %*% b)
  x
}

#' LM linearity testing against 2 regime genericStar. Performs a 3rd order Taylor expansion LM test.
#'
#' TODO: This function is from tsDyn
#' 
#' @param obj the time series object
#' @param ... additional arguments (not used)
#' @export 
linearityTest <- function(obj, ...) UseMethod("linearityTest")


#' @param rob TODO: What is this parameter?
#' @param sig the significance level
#' @param trace generate debug output?
#' @S3method linearityTest eTS
#' @method linearityTest eTS
#' @rdname linearityTest
linearityTest.eTS <- function(obj, rob=FALSE, sig=0.95, trace=TRUE, ...)
{
  
  nSamples <- nSamples(obj)
  
  # Regressors under the null
  xH0 <- cbind(1, getInvValues(obj))
  
  # Get the transition variable
  xx <- getInvValues(obj)
  
  # Linear Model (null hypothesis)
  #linearModel <- lm(getTargets(obj) ~ . , data=data.frame(xH0))
  #u <- linearModel$residuals
  
  c <- calculateLinearCoefficients(xH0, getTargets(obj))  
  dim(c) <- c(NCOL(xH0), 1)
  u <- getTargets(obj) - xH0 %*% c
  
  # lower tail has to be TRUE!, in original ncstar, 
  # test is not working correctly (can be seen by application of ncstar to a linear series..)
  #res <- linearityTestOnData(u = u, xx = xx, xH0 = xH0, nSamples = nSamples, sig = sig, useResiduals=TRUE, lower.tail=FALSE)
  res <- linearityTestOnData(u = u, xx = xx, xH0 = xH0, nSamples = nSamples, sig = sig)
  res
}


linearityTestOnData <- function(u, xx, xH0, nSamples, sig, useResiduals=FALSE) {
  
  SSE0 <- sum(u^2)
  
  # Regressors under the alternative
  xH1 <- cbind(xx^ 2, xx^3, xx^4)
  
  # Standarize the regressors
  Z <- cbind(xH0, xH1);
  nZ <- NCOL(Z);
  sdZ <- sd(Z)
  dim(sdZ) <- c(1, nZ)
  sdZ <- kronecker(matrix(1, nSamples, 1), sdZ) # repeat sdZ nSamples rows
  Z[,2:nZ] <- Z[,2:nZ] / sdZ[,2:nZ]
  
  # Compute the rank of Z
  s <- svd(Z);
  tol <- max(dim(Z)) * s[1]$d * 2.2204e-16
  rZ <- qr(Z, tol)$rank
  if(rZ < NCOL(Z)) warning("Multicollinearity problem.\n")
  
  # Nonlinear model (alternative hypothesis)
#  if(useResiduals) {
#    
#    nonlinearModel <- lm(u ~ ., data=data.frame(Z));
#    e_t <- nonlinearModel$residuals;
#    SSE1 <- sum(e_t^2)
#    
#  } else {
    
    c <- calculateLinearCoefficients(Z, u)  
    dim(c) <- c(NCOL(Z), 1);
    v <- u - Z %*% c;
    SSE1 <- sum(v^2);
    
#  }
  
  # Compute the test statistic
  nxH0 <- NCOL(xH0)
  nxH1 <- NCOL(xH1)
  
  F = ((SSE0 - SSE1) / nxH1) / (SSE1 / (nSamples - nxH1 - nxH0))
  
  # Look up the statistic in the table, get the p-value
  pValue <- pf(F, nxH1, nSamples - nxH0 - nxH1, lower.tail = TRUE)
  
  if (pValue >= sig) {
    return(list(isSignificant = TRUE,  pValue = pValue))
  }
  else {
    return(list(isSignificant = FALSE, pValue = pValue))
  }
}

#getXXYY <- function(obj, ...) UseMethod("getXXYY")
#
#getXXYY.nlar.struct <- function(obj, ...) {
#	x <- obj$x
#	m <- obj$m
#	d <- obj$d
#	steps <- obj$steps
#	embedd(x, lags=c((0:(m-1))*(-d), steps) )
#}

#getXX <- function(obj, ...)
#	getXXYY(obj,...)[ , 1:obj$m , drop=FALSE]

#getYY <- function(obj, ...)
#	getXXYY(obj, ...)[ , obj$m+1]

#getdXXYY <- function(obj, ...) UseMethod("getdXXYY")

#getdXXYY.nlar.struct <- function(obj,same.dim=FALSE, ...) {
#	x <- obj$x
#	m<-if(same.dim) obj$m-1 else obj$m
#	d <- obj$d
#	steps <- obj$steps
#	embedd(x, lags=c((0:(m-1))*(-d), steps) )
#}
#
#getdXX <- function(obj, ...)
#	diff(getdXXYY(obj,...))[ , 1:obj$m , drop=FALSE]
#
#getdYY <- function(obj, ...)
#	diff(getdXXYY(obj, ...))[ , obj$m+1]
#
#getdX1 <- function(obj, ...)
#	getdXXYY(obj,...)[ -1, 1, drop=FALSE]
#
#getNUsed <- function(obj, ...)
#	UseMethod("getNUsed")
