##

## Week 2 - DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA

FluTrain = read.csv("FluTrain.csv")
FluTest = read.csv("FluTest.csv")
attach(FluTrain)

## which week corresponds to the highest percentage of ILI-related physician visits
## Which week corresponds to the highest percentage of ILI-related query fraction?
levels(factor(Week[which.max(ILI)]))
levels(factor(Week[which.max(Queries)]))

plot(FluTrain$Queries, log(FluTrain$ILI))

FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)

## percentage of ILI-related physician visits for the week of March 11, 2012?
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

res_test <- PredTest1[grep("^2012-03-11", FluTest$Week)]
actual <- FluTest$ILI[grep("^2012-03-11", FluTest$Week)]
relative.err <- (actual-res_test)/act
relative.err

SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
RMSE
## Can use sqrt(mean((PredTest1-FluTest$ILI)^2))


## Training time series model
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)
sum(is.na(FluTrain$ILILag2))

## There is a strong positive relationship between log(ILILag2) and log(ILI).
plot(ILILag2~log(ILI))


## Train a linear regression model on the FluTrain dataset to predict 
## the log of the ILI variable using the Queries variable as well as 
## the log of the ILILag2 variable

FluTrend2 = lm(log(ILI)~log(ILILag2)+Queries, data=FluTrain)
summary(FluTrend2)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

# fill in the missing values in test set
FluTest$ILILag2[1:2] <- FluTrain$ILI[(nrow(FluTrain)-1):nrow(FluTrain)]
## From nrow(FluTrain), we see that there are 417 observations in the training set. Therefore, we need to run the following two commands:
## ALSO
## FluTest$ILILag2[1] = FluTrain$ILI[416]
## FluTest$ILILag2[2] = FluTrain$ILI[417]

# Obtain test set predictions of the ILI variable from the FluTrend2 model
predFluTest <- exp(predict(FluTrend2, newdata=FluTest))
rmse <- sqrt(mean((predFluTest-FluTest$ILI)^2))
rmse

## The test-set RMSE of FluTrend2 is 0.294, as opposed to the 0.749 
## value obtained by the FluTrend1 model.



