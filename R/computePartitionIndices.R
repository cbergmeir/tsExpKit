#example code
#part <- computePartitionIndices(types=c("lastBlock"), len=length(lynx), order=1, numPartitions=0, ratioValSet=0)
#part
#
#part <- computePartitionIndices(types=c("lastBlock", "blockedCV"), len=length(lynx), order=2, numPartitions=5, ratioValSet=0.2)
#part 
#
#part$inSetTrainIndicesO$blockedCV == part$inSetTrainIndices$blockedCV
#part$inSetTrainIndicesO$CV == part$inSetTrainIndices$CV
#
#
#part <- computePartitionIndices(types=c("lastBlock"), len=length(lynx), order=0, numPartitions=0, ratioValSet=0, ratioLB=0)
#part

# x: a time series
# name: the name of the time series
# lags: the lags to use
# partitions: the numer of cv and blocked cv partitions to generate
# ratio: how many percent of the data are to be used for OOS validation

#' This function computes the indices that can later be used to partition the time series into 
#' training and test sets, for different validation schemes. Currently supported 
#' are: "lastBlock", "CV", "noDepCV", "blockedCV". 
#' 
#' @title Compute the indices for the partitioning scheme 
#' @param len the length of the target series
#' @param type one of \code{c("lastBlock", "CV", "noDepCV", "blockedCV")}
#' @param order the embedding dimension, if the indices are computed for a series that is not embedded. 
#' If the indices are used with an embedded series, this has to be set to zero.  
#' @param ratioLB if between zero and one, defines the ratio of data that are used as test set. 
#' If greater one, defines the absolute value of instances in the test set.
#' @param numPartitions the numer of partitions to generate, for the cross-validation schemes
#' @param ratioValSet if an additional validation set is generated, its ratio (zero to not generate additional validation set)
#' @param seed the seed to use for randomly partitioning the data during cross-validation
#' @param notEmbedded if this flag is true, the indices are shifted by ``order'' into the future, so that 
#' the results are the indices in the not embedded series, of the values which are the target values in the embedded series.
# @example: dataPartitions <- generateDataPartitions(lynx, "lynx", c(2,3,4), 5, 0.8)
#' @export
computePartitionIndices <- function(len, type="lastBlock", order=0, ratioLB=0.15, numPartitions=0, ratioValSet=0, seed=1, notEmbedded=FALSE) {

  partitions <- list()
  
  unadjustedInSetLength <- floor((1-ratioValSet)*len) - order
  
  if(type == "lastBlock") {
    
    if(ratioLB > 1) {
      
      inSetLength <- unadjustedInSetLength
      partitionLength <- floor(ratioLB)
      
    } else {
      
      inSetLength <- unadjustedInSetLength
      partitionLength <- floor(ratioLB*inSetLength)
      
    }
    
  } else {
    
    inSetLength <- unadjustedInSetLength - (unadjustedInSetLength %% numPartitions)
    partitionLength <- inSetLength / numPartitions
    
  }
  
  inSetData <- 1:inSetLength
  
  if((inSetLength + order + 1) > (len-order)) 
    partitions[["outSetData"]] <- NULL
  else
    partitions[["outSetData"]] <- (inSetLength + order + 1):(len-order) 
  
  if(type == "CV" || type == "noDepCV") {

    partitions <- list()
    #attr(partitions, "shortname") <- 
        shortname <- paste("CV-",numPartitions, sep="")
    #class(partitions) <- "CV"

    set.seed(seed)
    shuffledIndices <- sample(inSetData,length(inSetData))
    
    for(t in 1:numPartitions)  {
      tempTest <- ((t-1)*partitionLength+1):(t*partitionLength)
      #tempTrain <- (1:nrow(inSetE))[-tempTest]
      tempTrain <- inSetData[-tempTest]
      partitions[["inSetTrainData"]] <- cbind(partitions[["inSetTrainData"]], shuffledIndices[tempTrain])
      partitions[["inSetTestData"]] <- cbind(partitions[["inSetTestData"]], shuffledIndices[tempTest])
    }
    
    if(type == "noDepCV")  {
      
      partitionsCV <- partitions

      partitions <- list()
      #attr(partitions, "shortname") <- 
          shortname <- paste("nDCV-",numPartitions, sep="")
      #class(partitions) <- "noDepCV"

      inSetTrainDataNoDepCV <- NULL
      inSetTestDataNoDepCV <- NULL
      
      diffZero <- FALSE
      
      for(t in 1:numPartitions)  {
        
        tempTest <- partitionsCV[["inSetTestData"]][,t]
        
        forbiddenIndices <- vector()
        for(tInd in tempTest) {
          forbiddenIndices <- union(forbiddenIndices, (max(1,(tInd - order))):(tInd+order))
        }
        
        diff <- setdiff(partitionsCV[["inSetTrainData"]][,t], forbiddenIndices)
        
        if(length(diff) == 0) {
          
          diffZero <- TRUE
          break;
        }
        
        tempTrain <- vector(length = length(partitionsCV[["inSetTrainData"]][,t]))
        tempTrain[1:length(tempTrain)] <- NA
        tempTrain[1:length(diff)] <- diff
        
        inSetTrainDataNoDepCV <- cbind(inSetTrainDataNoDepCV, tempTrain)
        inSetTestDataNoDepCV <- cbind(inSetTestDataNoDepCV, tempTest)
        
      }
      
      if(!diffZero) {
        
        partitions[["inSetTrainData"]] <- inSetTrainDataNoDepCV
        partitions[["inSetTestData"]] <- inSetTestDataNoDepCV
        
        colnames(partitions[["inSetTrainData"]]) <- NULL
        colnames(partitions[["inSetTestData"]]) <- NULL 
        
      }    
    }
    
  } else if(type == "blockedCV") {
    
    partitions <- list()
    #attr(partitions, "shortname") <- 
        shortname <- paste("bCV-",numPartitions, sep="")
    #class(partitions) <- "blockedCV"

  for(t in 1:numPartitions)  {
    #tempTest <- ((t-1)*partitionLength-order+1):(t*partitionLength+order)
    tempTest <- max(1,((t-1)*partitionLength-order+1)):min((t*partitionLength+order),numPartitions*partitionLength)
    #tempTrain <- (1:nrow(inSetE))[-tempTest]
    tempTrain <- inSetData[-tempTest]
    
    #remove dependencies
    tempTest <- ((t-1)*partitionLength+1):(t*partitionLength)
    #if (t==1) tempTest <- ((t-1)*partitionLength+1):(t*partitionLength-order)
    #else if(t==numPartitions) tempTest <- ((t-1)*partitionLength+order+1):(t*partitionLength)
    #else tempTest <- ((t-1)*partitionLength+order+1):(t*partitionLength-order)
    
    if(t != 1 && t != numPartitions) {
      
      tempTrainFilled <- vector(length = length(partitions[["inSetTrainData"]][,1]))
      tempTrainFilled[1:length(tempTrainFilled)] <- NA
      tempTrainFilled[1:length(tempTrain)] <- tempTrain
      tempTrain <- tempTrainFilled
    }
    
    partitions[["inSetTrainData"]] <- cbind(partitions[["inSetTrainData"]], as.vector(tempTrain))
    partitions[["inSetTestData"]] <- cbind(partitions[["inSetTestData"]], as.vector(tempTest))
  }
  #partitions[["turns"]][["blockedCV"]] <- sprintf("blockedCV%02d",1:numPartitions)
  
  } else if(type == "lastBlock") {
    
    partitions <- list()
    #attr(partitions, "shortname") <- 
        shortname <- paste("lB-", ratioLB, sep="")
    #class(partitions) <- "lastBlock"
    
    if((inSetLength - partitionLength + order + 1) > inSetLength) {
      #tempTest <- NULL
      #tempTrain <- inSetData
    
      partitions[["inSetTrainData"]] <- inSetData
      partitions[["inSetTestData"]] <- NULL
      
    } else {
      tempTest <- (inSetLength - partitionLength + 1) : inSetLength
      tempTrain <- inSetData[-tempTest]
      tempTest <- (inSetLength - partitionLength + order + 1) : inSetLength
      
      partitions[["inSetTrainData"]] <- as.matrix(tempTrain)
      partitions[["inSetTestData"]] <- as.matrix(tempTest)
      
    }
    
#    if((inSetLength - partitionLength + order + 1) > inSetLength) 
#      tempTest <- NULL
#    else


    
  }  

  if(ratioValSet != 0) {
    
    partitions[["valSetData"]] <- (inSetLength + order + 1):len
  }

##to get the target indices in the unembedded series:
#
if(notEmbedded) {
  partitions <- lapply(partitions, function(part) { 
        part + order
      })
}
  
#  partitions[["outSetData"]] <- partitions[["outSetData"]]  
#  partitions[["inSetTrainData"]] <- partitions[["inSetTrainData"]]
#  partitions[["inSetTestData"]] <- partitions[["inSetTestData"]]

# this is now done by setting order=0 and doing the embedding afterwards..  
#partitions[["outSetData"]] <- partitions[["outSetData"]] + order #(inSetLength+2*order + 1):(len)
#partitions <- lapply(partitions, function(part) {
#      if(is.list(part)) {
#        res <- lapply(part, function(x) x + order)
#        attr(res, "shortname") <- attr(part, "shortname")        
#      } else
#        res <- part + order
#      res
#    })


#partitions[["inSetTestData"]] <- lapply(partitions[["inSetTestData"]], function(x) x + order)
  
  #rudimentary time stamping
  #partitions[["timeStampO"]] <- 1:len
  #partitions[["timeStampE"]] <- partitions[["timeStampO"]][1:nrow(eData)] + (order-1)
  
  #attr(partitions, "shortname") <- shortname
  #class(partitions) <- type
  
  partitionParameters <- list(type=type,
      shortname=shortname,
      order=order, ratioLB=ratioLB, 
      numPartitions=numPartitions, 
      ratioValSet=ratioValSet, seed=seed)
  
  attr(partitions, "partitionParameters") <- partitionParameters
  
  partitions
}

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#computePartitionIndices <- function(type="lastBlock", len, order=0, ratio=0.15, numPartitions=0, ratioValSet=0) {
#
#  if(ratioValSet == 0){
#    inIndices <- 1 : (len - order)
#    outIndices <- NULL
#  } else {
#    inIndices <- 1 : (trunc(len * (1-ratioValSet)) - 1 - order)
#    outIndices <- trunc((len * (1-ratioValSet))) : len    
#    
#  }
#    
#  if(type == "none") {
#    trainIndices <- inIndices
#    testIndices <- NULL
#  } else if(type == "lb") {
#    
#    trainIndices <- 1 : (trunc(length[inIndices] * (1-ratio)) - 1 - order)
#    outIndices <- trunc((len * (1-ratioValSet))) : len
#    
#  } else if(type == "bCV") {
#    
#  }
#  
#  
#  return(list(trainIndices=trainIndices, testIndices=testIndices))
#  
#
#}
#
#len <- 114
#order <- 1
#ratioValSet <- 0.2


#' This function is very similar to \code{\link{computeTrainingAndTestIndices}}. 
#' It splits the given indices to indices for a training and a test set. Test set 
#' is taken from the end of the data. If the data is to be shuffled, this should
#'  be done before calling this function.
#' 
#' @title Function to split data indices into indices for training and test set
#' @param indices the indices to be used
#' @param ratio ratio of training and test sets (default: 15\% of the data is used for testing)
#' @return a named list with the following elements:
#' \item{trainIndices}{a matrix containing the training indices}
#' \item{testIndices}{a matrix containing the test indices}
#' @export
computeTrainingAndTestIndices <- function(indices, ratio=0.15) {
  
  #indices <- computeEmbeddedIndices(ts, lags)
  trainIndices <- indices[1 : (length(indices) * (1-ratio))]
  testIndices <- indices[-trainIndices]
  
  return(list(trainIndices=trainIndices, testIndices=testIndices))
  
}

#' This function is very similar to \code{\link{computeTrainingAndTestIndices}}, 
#' but instead of a vector of indices, it takes a length and an embedding order.
#' 
#' @title Function to split data into training and test set for last block evaluation.
#' @param len the length of the time series to be used
#' @param order the order of the embedding to be used
#' @param ratio the ratio of training and test set
#' @return a named list with the following elements:
#' \item{trainIndices}{a matrix containing the training indices}
#' \item{testIndices}{a matrix containing the test indices}
#' @export
computeIndicesLastBlockEval <- function(len, order, ratio=0.15) {
    
    #indices <- computeEmbeddedIndices(ts, lags)
    trainIndices <- 1 : (trunc(len * (1-ratio)) - 1 - order)
    testIndices <- (len * (1-ratio)) : len
    
    return(list(trainIndices=trainIndices, testIndices=testIndices))
    
  }
