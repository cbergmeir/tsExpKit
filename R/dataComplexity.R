#library(combinat)

# @importFrom combinat permn 

#' Calculates the permutation entropy of the given order for the given series.
#' 
#' @title calculate the permutation entropy of a given time series
#' @param ts the time series to which to apply the function
#' @param ord the order of the permutation entropy
#' @export
permutationEntropy <- function(ts, ord=4) {

  y <- ts
  ly <- length(ts)
  
  permlist <- permn(1:ord);
  vec <- seq(0,0,length=length(permlist))
  
  for (j in 1:(ly-ord)) {
    
    sort.res <- sort.int(y[j:(j+ord-1)], index.return = TRUE);
    iv <- sort.res$ix
    
    for (jj in 1:length(permlist)) {
      if (sum(abs(permlist[[jj]]-iv))==0) {
        vec[jj] <- vec[jj] + 1 ;
      } 
    }
  }
  
  p <- apply(as.matrix(vec/(ly-ord)), 1, function (x) {max(1/ly, x)})
  e <- -sum(p * log(p))/(ord-1)
  e
  
}