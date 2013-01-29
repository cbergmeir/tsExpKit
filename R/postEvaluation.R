#' This function can be used to unite evaluations of different experiment runs, both accross
#' datasets and methods, for later evaluation.
#' 
#' TODO: Currently no examples available. Consult with
#' the maintainer if you can't figure out how to use this function.
#' 
#' @title Create a table for evaluation of various experiment runs
#' @param resultCollection a list containing the paths to the different experiment runs
#' @param measure the filename of the measure, e.g. \code{"predEval_RMSE.csv"}.
#' @param na.rm if \code{TRUE}, cases where NAs are present are removed 
#' @export
assembleMeasureTable <- function(resultCollection, measure, na.rm=TRUE) {

  totTab <- NULL
  
  for(results in resultCollection) {
  
#  tables <- lapply(resultCollection, function(results) {
        res <- NULL
        for(i in results) {
          
          tab <- read.csv(assemblePathName(i, measure))
          tab <- firstColToColname(tab)
          
          res <- rBindByColNames(res, tab)
        }
        
        totTab <- cBindByRowNames(totTab, res)
#      })
  }
  
  if(na.rm) totTab <- totTab[!(apply(totTab, 1, function(x) {any(is.na(x))})),]
  totTab
}

#colMeans <- apply(totTab, 2, mean)
#as.matrix(colMeans[order(colMeans)])
#
#colSums <- apply(totTab, 2, sum)
#as.matrix(colSums[order(colSums)])
#
#ranks <- apply(totTab, 1, rank)
#
#colnames(ranks) <- NULL
#ranks
#
#rankSums <- apply(ranks, 1, sum)
#as.matrix(rankSums[order(rankSums)])
#
#
#
#write.table(max(totTab)-totTab, file="/home/bergmeir/statisticalTesting/ncstar_simseries.csv", quote=FALSE, sep=",", qmethod="double")
#
#totTabNoNames <- totTab
#rownames(totTabNoNames) <- NULL
#t(totTabNoNames)
#
#ranks <- ranks[c(1,2,6,8),]
