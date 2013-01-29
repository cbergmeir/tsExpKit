library(tsExpKit)
library(nnet)

ets <- normalize(eTS(lynx, lags=-4:0))

partitions <- computePartitions(ets, type="lastBlock")

#names(partitions)
#partitions$inSetTrainData[[1]]
#partitions$inSetTestData[[1]]

mod <- nnet(getValues(partitions$inSetTrainData[[1]]), getTargets(partitions$inSetTrainData[[1]]), size=5, decay=0.1, maxit=1000, linout=TRUE)

pred <- predict(mod, getValues(partitions$inSetTestData[[1]]))

reference <- getTargets(partitions$inSetTestData[[1]])
benchmark <- getNaiveForecast(partitions$inSetTestData[[1]])

measureErrorAll(prediction=pred, reference=reference, benchmark=benchmark)    


plot(ets)

lines(getTimeStamps(partitions$inSetTestData[[1]]), reference, col="green")
lines(getTimeStamps(partitions$inSetTestData[[1]]), benchmark, col="red")
lines(getTimeStamps(partitions$inSetTestData[[1]]), pred, col="blue")