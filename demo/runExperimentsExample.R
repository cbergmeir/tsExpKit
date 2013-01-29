#nohup Rscript /home/bergmeir/ts-code/scripts/runExperimentsExample.R &

library("tsExpKit")

config <- cbind(getConfigDefault(), getConfigHercules())

config <- cbind(config, pc=c("nenufar8.ugr.es","none",1), stringsAsFactors=FALSE)
config <- cbind(config, notebook=c("nenufar8.1","multi",6), stringsAsFactors=FALSE)
config <- cbind(config, node30=c("node30","multi",8), stringsAsFactors=FALSE)

expPath <- assembleExperimentsPath(resultsComment = "Example_experiment_run_on_benchmarks", resultsPath = "/home/ts/results/") 

useCases <- list(
#   wmtsa=list(name="wmtsa",
#       dataRepository="/home/ts/datasets/repos/public", 
#       dataPackage="wmtsa", 
#       partPath="modSelProc/lB-0.2/norm/emb-4"), 
    NNGC1_dataset_D1_V1=list(name="NNGC1_dataset_D1_V1",
        dataRepository="/home/ts/datasets/repos/public", 
        dataPackage="NNGC1_dataset_D1_V1", 
        partPath="modSelProc/lB-0.2/norm/emb-4")    
#    benchmark1=list(name="benchmark1",
#        dataRepository="/home/ts/datasets/repos/public", 
#        dataPackage="benchmark", 
#        partPath="modSelProc/lB-0.2/norm/emb-4")                
)

usedData <- genUsedData(useCases, onlyUseStationarySeries=FALSE, removeDuplicatesByDescStatTable=FALSE, useSelectionFile=FALSE)

#selectionFile <- paste("/home/ts/results/selectedTS.", hostname, ".csv",sep="")
##selectionFile <- paste("/home/ts/results/selectedTS_M4.", hostname, ".csv",sep="")
#usedData <- genUsedData(useCases, onlyUseStationarySeries=FALSE, removeDuplicatesByDescStatTable=FALSE, 
#                                        useSelectionFile=TRUE, selectionFile = selectionFile)

methodsDefinitions <- list()


#------------------------------------------------
#nnet
#------------------------------------------------
library(nnet)
parameterGrid <- expand.grid(c(3,5,9,15), c(0.00316, 0.0147, 0.1))
colnames(parameterGrid) <- c("size", "decay")
rownames(parameterGrid) <- paste("nnet-", apply(parameterGrid, 1, function(x) {paste(x,sep="", collapse="-")}), sep="")
#parameterGrid <- as.matrix(parameterGrid)
#parameterList <- split(parameterGrid, row(parameterGrid))
#names(parameterList) <- paste("nnet-", apply(parameterGrid, 1, function(x) {paste(x,sep="", collapse="-")}), sep="")

for(i in 1:nrow(parameterGrid)) {
  currPS <- parameterGrid[i,]
  shortName <- rownames(currPS)[1]
    
  methodsDefinitions[[shortName]] <- list(funcname="nnet", shortname=shortName, trainDataType="separated",
      params=append(as.list(currPS), list(maxit=10000, linout=TRUE)))  
}

#svm
library(e1071)
parameterGrid <- expand.grid(c(10, 100), c(0.001, 0.01, 0.2))
colnames(parameterGrid) <- c("cost", "gamma")
rownames(parameterGrid) <- paste("svr-", apply(parameterGrid, 1, function(x) {paste(x,sep="", collapse="-")}), sep="")
#parameterGrid <- as.matrix(parameterGrid)
#parameterList <- split(parameterGrid, row(parameterGrid))
#names(parameterList) <- paste("nnet-", apply(parameterGrid, 1, function(x) {paste(x,sep="", collapse="-")}), sep="")

for(i in 1:nrow(parameterGrid)) {
  currPS <- parameterGrid[i,]
  shortName <- rownames(currPS)[1]
  
  methodsDefinitions[[shortName]] <- list(funcname="svm", shortname=shortName, trainDataType="separated",
      params=as.list(currPS))  
}

myLinearModel <- function(x,y) {
  lm(y ~ . -1, data = as.data.frame(x))
}

methodsDefinitions[["AR"]] <- list(funcname="myLinearModel", shortname="AR", trainDataType="separated",
    params=list())  

library(lars)

myLasso <- function(x,y, tsExpKitFraction=0.1, ...) {
  res <- lars(x,y, ...)
  res$tsExpKitFraction <- tsExpKitFraction
  res
}


methodsDefinitions[["lasso_0.1"]] <- list(funcname="myLasso", shortname="lasso.0.1", trainDataType="separated",
    params=list(type="lasso", tsExpKitFraction=0.1, normalize=FALSE))  

methodsDefinitions[["lasso_0.36"]] <- list(funcname="myLasso", shortname="lasso.0.36", trainDataType="separated",
    params=list(type="lasso", tsExpKitFraction=0.36, normalize=FALSE))  

methodsDefinitions[["lasso_0.63"]] <- list(funcname="myLasso", shortname="lasso.0.63", trainDataType="separated",
    params=list(type="lasso", tsExpKitFraction=0.63, normalize=FALSE))  

methodsDefinitions[["lasso_0.9"]] <- list(funcname="myLasso", shortname="lasso.0.9", trainDataType="separated",
    params=list(type="lasso", tsExpKitFraction=0.9, normalize=FALSE))  

#------------------------------------------------

require("mda")

methodsDefinitions[["MARS"]] <- list(funcname="mars", shortname="MARS", trainDataType="separated", params=list())  


########################################################################
# Neural Network Models                                                #
########################################################################

library(RSNNS)

#MPLs
#parameterGrid <- expand.grid(c(25,30), c(0.005, 0.025, 0.1))
#parameterGrid <- expand.grid(c(3,5,9,15), c(0.00316, 0.0147, 0.1))
parameterGrid <- expand.grid(c(3,5,9,15), c("c(0.00316,0)","c(0.0147,0)","c(0.1,0)"), stringsAsFactors=FALSE)
#parameterGrid <- expand.grid(c(3,5,7,9), c("c(0.001,0)","c(0.005,0)","c(0.01,0)"), stringsAsFactors=FALSE)
colnames(parameterGrid) <- c("size", "learnFuncParams")
rownames(parameterGrid) <- paste("rsnns-mlp-", apply(parameterGrid, 1, function(x) {paste(x,sep="", collapse="-")}), sep="")

for(i in 1:nrow(parameterGrid)) {
  currPS <- parameterGrid[i,]
  shortName <- rownames(currPS)[1]
  
  methodsDefinitions[[shortName]] <- list(funcname="mlp", shortname=shortName, trainDataType="separated",
      params=append(lapply(as.list(currPS), function(x) {eval(parse(text=x))}),list(maxit=1000, linOut=TRUE)))  
}

#ELMANs
#parameterGrid <- expand.grid(c(3,5,9,15), c(0.00316, 0.0147, 0.1), stringsAsFactors=FALSE)
parameterGrid <- expand.grid(c("c(3,3)","c(5,5)","c(9,9)", "c(15,15)"), c(0.00316, 0.0147, 0.1), stringsAsFactors=FALSE)
#parameterGrid <- expand.grid(c("c(15,15)","c(20,20)","c(30,30)"), c(0.00316,0.002))
colnames(parameterGrid) <- c("size", "learnFuncParams")
rownames(parameterGrid) <- paste("rsnns-elman-", apply(parameterGrid, 1, function(x) {paste(x,sep="", collapse="-")}), sep="")

for(i in 1:nrow(parameterGrid)) {
  currPS <- parameterGrid[i,]
  shortName <- rownames(currPS)[1]
  
  methodsDefinitions[[shortName]] <- list(funcname="elman", shortname=shortName, trainDataType="separated",
      params=append(lapply(as.list(currPS), function(x) {eval(parse(text=x))}),list(maxit=1000, linOut=TRUE)))  
}



usedData
methodsDefinitions


#-----------------------------------------------
# Run the experiments
#-----------------------------------------------


myPredict <- function(mod, newdata, ...) {
  
  if("lars" %in% class(mod)) {
    res <- predict(mod, newdata, type = "fit", mode = "fraction", s = mod$tsExpKitFraction)$fit    
  } else if("lm" %in% class(mod)) {
    res <- predict(mod, newdata=as.data.frame(newdata), ...)
  } else {
    res <- predict(mod, newdata, ...)
  }
  res
}


res <- runExperiments(usedData, expPath, methodsDefinitions, tsApply=initParallelMode(config), seed=5, predictFunc=myPredict)


#-----------------------------------------------
# Perform the evaluations
#-----------------------------------------------

resPath <- assemblePathName(expPath, "results")
dir.create(resPath, showWarnings=FALSE, recursive=TRUE)

modelFiles <- list.files(assemblePathName(expPath, "models"), full.names=TRUE, recursive=TRUE, pattern="+\\.RData")
modEval <- evaluateModels(modelFiles)
for(name in names(modEval))
  write.csv(modEval[[name]], file = assemblePathName(resPath, paste("modEval_", name, ".csv", sep="")))

predictionFiles <- list.files(assemblePathName(expPath, "predictions"), full.names=TRUE, recursive=TRUE, pattern="+\\.RData")
#predEval <- evaluatePredictions(predictionFiles, dataPath, denorm=FALSE)
predEval <- evaluatePredictions(predictionFiles, usedData, denorm=FALSE)
for(name in names(predEval))
  write.csv(predEval[[name]], file = assemblePathName(resPath, paste("predEval_", name, ".csv", sep="")))

predEvalDenorm <- evaluatePredictions(predictionFiles, usedData, denorm=TRUE)
for(name in names(predEvalDenorm))
  write.csv(predEvalDenorm[[name]], file = assemblePathName(resPath, paste("predEvalDenorm_", name, ".csv", sep="")))


