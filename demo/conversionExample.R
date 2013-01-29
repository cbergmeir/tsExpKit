library(tsExpKit)
library(RKEEL)

dataRepositoryPath <- "/home/ts/datasets/repos/sandbox"

library(TSA)
data(ar2.s)
atsList <- list(aTS(rnorm(200)), aTS(ar2.s), aTS(lynx), aTS(log(lynx)))
atsList <- setNamesATSList(atsList)
dataPackageName <- "benchmark"
importATSListIntoDataRepository(atsList, dataRepositoryPath, dataPackageName, #partSuffix="lB-0.2",
		partitionParametersKeelExp = list(type=c("lastBlock"), order=0, ratioLB=0.2, ratioValSet=0),
		partitionParametersNorm = list(type=c("lastBlock"), order=0, ratioLB=0.2, ratioValSet=0),
		embeddingParameters = list(lags=-6:0), genDescStatTab=TRUE, genStatNonlinTab=TRUE, genStatOtherTestsTab=FALSE, minLength=0)


embedNormLBDir <- embedATSDir("/home/ts/datasets/repos/sandbox/benchmark/benchmark/modSelProc/lB-0.2/norm", embeddingParameters=list(lags=c(-4,-3,-2,-1,0))) 

atsList <- loadATSDir("/home/ts/datasets/repos/sandbox/benchmark/benchmark/modSelProc/lB-0.2/norm")

atsList_2 <- loadATSDir("/home/ts/datasets/repos/sandbox/benchmark/benchmark/modSelProc/lB-0.2/norm/emb-4")