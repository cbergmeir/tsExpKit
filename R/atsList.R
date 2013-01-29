#' An ATSList is simply a \code{list} of \code{\link{aTS}} objects. However, there are 
#' a lot of useful functions that can be used together with such a list, namely
#' \code{\link{setNamesATSList}}, \code{\link{summaryTableATSList}}, 
#' \code{\link{computePartitionsATSList}}, \code{\link{groupATSListByPartitions}}, and 
#' \code{\link{embedATSList}}. 
#' Furthermore, for I/O, there are: \code{\link{loadDifToATSList}}, 
#' \code{\link{loadDatDirToATSList}}, \code{\link{loadDataDirToATSList}}, \code{\link{loadATSDir}}, 
#' \code{\link{loadATSDir}}, \code{\link{saveATSList}}, \code{\link{savePlotsATSList}}, 
#' and \code{\link{saveAsDataPackageATSList}}. 
#' 
#' @title A list of \code{\link{aTS}} objects
#' @name ATSList
NULL

#' This method calls for every \code{\link{aTS}} object in the list 
#' its \code{\link{getName}} method, and the list element's names accordingly. 
#' E.g., if you have an aTS object of the \code{lynx} series, after applying this 
#' function to the \code{\link{ATSList}}, you can access it with atsList[[lynx]], instead of 
#' atsList[[1]].   
#' 
#' @title Set the names of the \code{\link{ATSList}} correctly
#' @param atsList the \code{\link{ATSList}} for which the names to set
#' @export
setNamesATSList <- function(atsList) {
  names(atsList) <- lapply(atsList, getName)
  atsList
}

#' This method calls for every \code{\link{aTS}} object in the list 
#' the function \code{\link{oneLineSummary}}, and returns a table that consists of
#' all these summaries.
#' 
#' @title Calculate a summary table of an \code{\link{ATSList}}
#' @param atsList the \code{\link{ATSList}} to which to apply the function to
#' @param ... parameters passed to \code{\link{oneLineSummary}}
#' @export
summaryTableATSList <- function(atsList, ...) {
  
  tab <- NULL
  for(ats in atsList) {
    tab <- rbind(tab, oneLineSummary(ats, ...))
  }
  tab
}


#' This method calls for every \code{\link{aTS}} object in the \code{\link{ATSList}} 
#' the function \code{\link{computePartitions}}.
#' 
#' @title Compute partitions for an \code{\link{ATSList}}
#' @param atsList the \code{\link{ATSList}} to which to apply the function to
#' @param ... parameters passed to \code{\link{computePartitions}}
#' @export
computePartitionsATSList <- function(atsList, ...) {

  lapply(atsList, computePartitions, ...)
}


#' This method sorts the ATSList according to its partitions. 
#' It uses the "tsname" attribute of the \code{\link{aTS}} objects for that.
#' 
#' TODO: Where is this used?
#' 
#' @title Group an \code{\link{ATSList}} by its partitions
#' @param atsList the \code{\link{ATSList}} to which to apply the function to
#' @return a list with two hierarchies
#' @export
groupATSListByPartitions <- function(atsList) {
  
  res <- list()
  
  for(atsName in names(atsList)) {
    
    origTsName <- aTSattr(atsList[[atsName]], "tsname")
    
    res[[origTsName]][[atsName]] <- atsList[[atsName]]
  }
  res
}


#' This method constructs from every \code{\link{aTS}} object in the list 
#' an \code{\link{eTS}} object.
#' 
#' @title Embed an \code{\link{ATSList}}
#' @param atsList the \code{\link{ATSList}} to which to apply the function to
#' @param ... parameters passed to the \code{\link{eTS}} constructor
#' @export
embedATSList <- function(atsList, ...) {

  etsList <- list()
  
  for(ats in atsList) {
    
    etsList[[getName(ats)]] <- eTS(ats, ...)
    
  }
  etsList  
}


#---------------------------------
# Plot the shiller data
#---------------------------------
#pdf(file=sprintf("%s/Shiller.pdf",resultsDir),onefile=TRUE,width=20,height=3*8)
#par(mfrow=c(8,1))
#
##par(mfrow=c(3,1))
#
#for (i in 2:8)  {
#  
#  #i <- names(exampleSeries)[1]
#  #exampleSeries[["shiller.dat"]][,ncol(exampleSeries[["shiller.dat"]])]
#  
#  plot(exampleSeries[["shiller.dat"]][,i],type="l", xlab="", ylab="")
#}
#dev.off()
#
#shiller.dat[1:5,]
