#' Rudimentary parser for KEEL files.
#' 
#' Comments are only allowed in the header
#' @param filename the name of the KEEL inputfile
#' @param noColumns the number of columns
#' @export
loadKeelFile <- function(filename, noColumns=1)  {
  
  allData <- scan(filename, what="list", multi.line=TRUE)
  
  resColNames <- NULL
  
  mode <- "head"
  data <- NULL
  nc <- 0
  attributeNames <- NULL
  relNameSearch <- FALSE
  relNameBegin <- 0
  relName <- NULL
  
  for(i in 1:length(allData)) {
    
    if(mode == "head")
    {
      firstSymbol <- substr(allData[i],1,1)
      if(!is.na(firstSymbol) && firstSymbol == "@") {
        
        if(relNameSearch) {
          relName <- paste(allData[relNameBegin:(i-1)], collapse="")
          relNameSearch <- FALSE
        }
        
        if (substr(allData[i],2,10) == "attribute") {
          nc <- nc + 1
          print(allData[i+1])
          attributeNames <- c(attributeNames, allData[i+1])
        } else if (substr(allData[i],2,5) == "data") {
          mode <- "body"
        } else if (substr(allData[i],2,9) == "relation") {
          relNameSearch <- TRUE
          relNameBegin <- i + 1
        }
      }
      
    } else if(mode == "body")  {
      s <- allData[i]
      
      #if there is some delimiter at the end, e.g. a komma, remove it
      if(is.na(as.numeric(substr(s,nchar(s),nchar(s)))))
      {
        s <- substr(s,1,nchar(s)-1)
      }
      data <- c(data, as.numeric(s))
    }
  }
  
  if(nc != 0) {
    noColumns <- nc
    resColNames <- attributeNames    
  }
  
  if(noColumns > 1) {
    data <- matrix(data,ncol=noColumns,byrow=TRUE)  
    colnames(data) <- resColNames
    
    #remove TimeStamp
    if(colnames(data)[1] == "TimeStamp") {
      data <- data[, 2:ncol(data)] 
    }
    
  }
    
  attr(data, "comment") <- relName
  data  
}
