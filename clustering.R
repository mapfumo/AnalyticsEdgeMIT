## HIERARCHICAL CLUSTERING
dailykos <- read.csv("dailykos.csv")
## Compute the distances (using method="euclidean"), and use hclust to build the 
## model (using method="ward")
kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D") # Hierarchical clustering
plot(kosHierClust)

# Pick 7 clusters
clusterGroups = cutree(kosHierClust, k = 7)
HierCluster = split(dailykos, clusterGroups)
HierCluster1 <- subset(dailykos, clusterGroups==1)
HierCluster2 <- subset(dailykos, clusterGroups==2)
HierCluster3 <- subset(dailykos, clusterGroups==3)
HierCluster4 <- subset(dailykos, clusterGroups==4)
HierCluster5 <- subset(dailykos, clusterGroups==5)
HierCluster6 <- subset(dailykos, clusterGroups==6)
HierCluster7 <- subset(dailykos, clusterGroups==7)
sort(table(clusterGroups), decreasing=T)

tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))


## K-MEANS CLUSTERING
set.seed(1000)
KmeansCluster = kmeans(dailykos, centers=7)
sort(table(KmeansCluster$cluster), decreasing=T)
KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)
KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)
KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)
KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)
KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)
KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)
KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7)

table(clusterGroups, KmeansCluster$cluster)




tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))


##############################################################################################
##############################################################################################
####  MARKET SEGMENTATION FOR AIRLINES
airlines = read.csv("AirlinesCluster.csv")
summary(airlines)
library(caret)
preproc = preProcess(airlines)
# Normalize
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

distances = dist(airlinesNorm, method="euclidean")
hierClust = hclust(distances, method="ward.D")
plot(hierClust)

# Pick 5 clusters
clusterGroups = cutree(hierClust, k = 5)
HierCluster = split(airlinesNorm, clusterGroups)
# Number of data points in cluster 1
nrow(HierCluster[[1]])
table(clusterGroups)
sort(tapply(airlines$Balance, clusterGroups, mean))
sort(tapply(airlines$QualMiles, clusterGroups, mean))
sort(tapply(airlines$BonusMiles, clusterGroups, mean))
sort(tapply(airlines$BonusTrans, clusterGroups, mean))
sort(tapply(airlines$FlightMiles, clusterGroups, mean))
sort(tapply(airlines$FlightTrans, clusterGroups, mean))
sort(tapply(airlines$DaysSinceEnrol, clusterGroups, mean))


k = 5
set.seed(88)
KmeansCluster <- kmeans(airlinesNorm, centers = k, iter.max=1000)
airlinesClusters <- KmeansCluster$cluster
table(airlinesClusters)
KmeansCluster$centers




####################################################################
## PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT
###################################################################
stocks = read.csv("StocksCluster.csv")
#What proportion of the observations have positive returns in December?
prop.table(table(stocks$PositiveDec > 0))
cor(stocks)
#Which month (from January through November) has the largest 
#mean return across all observations in the dataset?
sort(apply(stocks,2,mean))
## Could also use 
summary(stocks)

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
logfit <- glm(PositiveDec ~ ., data=stocksTrain, family=binomial)
table(predict(logfit, type = "response") > 0.5 , stocksTrain$PositiveDec)
(990+3640)/length(stocksTrain$PositiveDec)

PredictTest = predict(logfit, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, PredictTest > 0.5)
(417 + 1553)/(417 + 1160 + 344 + 1553) 

table(stocksTest$PositiveDec)
1897/(1577 + 1897) 

##########################################
#### Clustering Stocks
## Now, let's cluster the stocks. The first step in this process is to 
## remove the dependent variable using the following commands:
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
## Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

set.seed(144)
km <- kmeans(normTrain, centers = 3 )
sort(table(km$cluster))

library(flexclust)
km.kcca      <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest  <- predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)
stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

#### Build logistic regression models.
model1 <- glm(PositiveDec ~., data = stocksTrain1, family = binomial)
model2 <- glm(PositiveDec ~., data = stocksTrain2, family = binomial)
model3 <- glm(PositiveDec ~., data = stocksTrain3, family = binomial)

df <- data.frame("model1" = model1$coefficients, "model2" = model2$coefficients, 
                 "model3" = model3$coefficients)
df

PredictTest1 = predict(model1, newdata = stocksTest1, type="response")
PredictTest2 = predict(model2, newdata = stocksTest2, type="response")
PredictTest3 = predict(model3, newdata = stocksTest3, type="response")
table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)


AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)



















