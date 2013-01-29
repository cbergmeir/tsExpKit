#' The attributed time series class (\code{aTS}) extends the \code{zooreg} class from the zoo package. 
#' It implements its own mechanism for attributes, similar to xts, see \code{\link{atsAttributes}}. Furthermore, 
#' there are the following convenience methods for getting and setting some special attributes directly:
#' \code{getName}, \code{setName}, \code{getComment}, \code{setComment}. Also, there is a method \code{getTimeStamps}, which in directly returns the 
#' result of the \code{index} function from \code{zoo}. The method \code{nSamples} returns the number of samples, i.e., 
#' the number of rows of the data matrix. Furthermore, there is the index operator \code{[]} defined on the class.
#' 
#' @title Constructor for objects of the attributed time series class
#' @param x data to construct the time series from. This is directly passed to \code{as.zooreg} from the zoo package.
#' @param ... parameters passed to the \code{as.zooreg} function
#' @param name a name for the time series
#' @param comment a commentary describing the contents of the series
#' @return a new aTS object 
#' @aliases [.aTS
#' @examples
#' ats <- aTS(lynx)
#' 
#' getTimeStamps(ats)
#' nSamples(ats)
#' 
#' 
#' aTSattr(ats, "test") <- 5
#' atsAttributes(ats)
#' aTSattr(ats, "test")
#' 
#' @export
aTS <- function(x, ..., name=deparse(substitute(x)), comment=NULL) {

#  if(missing(name))
#    name <- deparse(substitute(x))
  
  if(!inherits(x, "zooreg")) {
    aTS <- as.zooreg(x, ...)  
  } else {
    aTS <- x
    if(!missing(...))
     warning("The object is already a zooreg object, additional parameters are ignored.")
  } 
    
    
  
  #aTS <- as.zooreg(x, ...)
  
  class(aTS) <- c("aTS", class(aTS))
  atsAttributes(aTS) <- list(name=name, comment=comment)
  
  #the frequency is not used and causes a lot of trouble
  #attr(aTS, "frequency") <- 1
  
  aTS
}

# @title Get method for the time stamps
#' @param obj the \code{\link{aTS}} object
# @param ... additional function parameters (currently not used)
#' @export 
#' @rdname aTS
getTimeStamps <- function(obj, ...) UseMethod("getTimeStamps")

# @return the time stamps 
#' @S3method getTimeStamps aTS
#' @method getTimeStamps aTS
#' @rdname aTS
getTimeStamps.aTS <- function(obj, ...) {
  index(obj)
}

# @title Get an ats attribute by its name
#' @param aTS the \code{\link{aTS}} object
#' @param name, the name of the attribute
#' @rdname atsAttributes
#' @return the value of the attribute
#' @export 
aTSattr <- function(aTS, name){
  atsAttributes(aTS)[[name]]
}

# @title Set an ats attribute by its name
#' @rdname atsAttributes
# @aliases "aTSattr<-"
#' @usage aTSattr(aTS, name) <- value 
#' @export "aTSattr<-"
`aTSattr<-` <- function(aTS, name, value) UseMethod('aTSattr<-')


#' @S3method "aTSattr<-" aTS
#' @method "aTSattr<-" aTS
# @param aTS the \code{\link{aTS}} object
# @param name the name of the atsAttribute
#' @usage aTSattr(aTS, name) <- value 
#' @param value the value of the atsAttribute
#' @rdname atsAttributes
`aTSattr<-.aTS` <- function(aTS, name, value){
  tmpAttrs <- atsAttributes(aTS)
  tmpAttrs[[name]] <- value
  atsAttributes(aTS) <- tmpAttrs
  aTS
}

#' The attributes mechanism of the \code{\link{aTS}} class works as follows: 
#' All attributes are stored in a conventional R attribute named \code{aTSattr}, which is a list. To get all attributes, i.e., this list, call \code{atsAttributes} on the time series.
#' You can replace the whole list using the \code{atsAttributes<-} method. To get and set single attributes, use the methods \code{aTSattr}, and \code{aTSattr<-}.
#'  
#' @title The aTS attribute mechanism
#' @export 
#' @rdname atsAttributes
#' @examples
#' ats <- aTS(lynx)
#' 
#' getTimeStamps(ats)
#' nSamples(ats)
#' 
#' 
#' aTSattr(ats, "test") <- 5
#' atsAttributes(ats)
#' aTSattr(ats, "test")
#' 
atsAttributes <- function(aTS){
  attr(aTS, "aTSattr")
}

#' @title Set all ats attributes
# @name setatsAttributes
#' @usage atsAttributes(aTS) <- value 
#' @export "atsAttributes<-"
#' @rdname atsAttributes
`atsAttributes<-` <- function(aTS,value) UseMethod('atsAttributes<-')

#' @S3method "atsAttributes<-" aTS
#' @method "atsAttributes<-" aTS
#' @usage atsAttributes(aTS) <- value 
#' @rdname atsAttributes
`atsAttributes<-.aTS` <- function(aTS, value){
  attr(aTS, "aTSattr") <- value
  aTS
}

#' @S3method "[" aTS
#' @method "[" aTS
# TODO: The following is a hack to stop R CMD check complaining.
#' @usage aTS(x, ..., name=deparse(substitute(x)), comment=NULL)
#' @rdname aTS
# @usage obj[...] 
# @usage "["(obj, ...)
"[.aTS" <- function(obj, ...) {
  
#  .Class <- "zoo"
#  res <- NextMethod(.Generic)
  res <- NextMethod(...)
  #, indices, idx=0
  
  #make the zoo object to an aTS object again
  #class(res) <- c("eTS", class(res))
  class(res) <- class(obj)
  atsAttributes(res) <- atsAttributes(obj)
  
  res
}

# @param obj the \code{\link{aTS}} object
# @param ... additional function parameters (currently not used)
#' @export 
#' @rdname aTS
getName <- function(obj, ...) UseMethod("getName")

#' @S3method getName aTS
#' @method getName aTS
#' @rdname aTS
getName.aTS <- function(obj, ...) aTSattr(obj, "name")

# @param obj the \code{\link{aTS}} object
# @param ... additional function parameters (currently not used)
#' @export 
#' @rdname aTS
setName <- function(obj, ...) UseMethod("setName")

#' @S3method setName aTS
#' @method setName aTS
#' @rdname aTS
setName.aTS <- function(obj, name, ...) {
  aTSattr(obj, "name") <- name
  obj
}

# @param obj the \code{\link{aTS}} object
# @param ... additional function parameters (currently not used)
#' @export 
#' @rdname aTS
getComment <- function(obj, ...) UseMethod("getComment")

#' @S3method getComment aTS
#' @method getComment aTS
#' @rdname aTS
getComment.aTS <- function(obj, ...) aTSattr(obj, "comment")

# @param obj the \code{\link{aTS}} object
# @param ... additional function parameters (currently not used)
#' @export 
#' @rdname aTS
setComment <- function(obj, ...) UseMethod("setComment")

#' @S3method setComment aTS
#' @method setComment aTS
#' @rdname aTS
setComment.aTS <- function(obj, comment, ...) {
  aTSattr(obj, "comment") <- comment
  obj
}


# @param obj the \code{\link{aTS}} object
# @param ... additional function parameters (currently not used)
#' @export 
#' @rdname aTS
nSamples <- function(obj, ...) UseMethod("nSamples")

#' @S3method nSamples aTS
#' @method nSamples aTS
#' @rdname aTS
nSamples.aTS <- function(obj, ...) {
  
  if(is.matrix(coredata(obj)))
    res <- nrow(coredata(obj))
  else
    res <- length(coredata(obj))
  res
}


