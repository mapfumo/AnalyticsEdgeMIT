census = read.csv("~/data-science/MITx-The-Analytics-Edge/data-files/AnalyticsEdgeMIT/census.csv")
require(caTools)
set.seed(2000)
split <- sample.split(census$over50k, SplitRatio = .6)
train <- subset(census, split == T)
test <- subset(census, split == F)
fit.glm <- glm(over50k ~ ., data = train, family = "binomial")

summary(fit.glm)

glmPred <- predict(fit.glm, newdata = test, type = "response")
table(test$over50k, glmPred > .5)
(9051 + 1888)/ nrow(test)

## Baseline accuracy
table(test$over50k)/ nrow(test)

## How strong is this logistic regression model? ##
require(ROCR)
ROCRpred <- prediction(glmPred, test$over50k)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = T, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-.2, 1, .7))

auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
auc

require(rpart)
require(rpart.plot)
CART1 <- rpart(over50k ~ ., data = train, method = "class")
prp(CART1)

## Accuracy of CART ##
CARTpred <- predict(CART1, newdata = test, type = "class")
table(test$over50k, CARTpred)
(9243 + 1596)/ (9243 + 1596 + 470 + 1482)

## ROC and AUC of the CART model ##
CARTpred2 <- predict(CART1, newdata = test)
ROCRpred2 <- prediction(CARTpred2[, 2], test$over50k)
ROCRperf2 <- performance(ROCRpred2, "tpr", "fpr")
plot(ROCRperf2, colorize = T, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-.2, 1, .7))

auc2 <- as.numeric(performance(ROCRpred2, "auc")@y.values)
auc2


## Random Forests ##
set.seed(1)
trainSmall <- train[sample(nrow(train), 2000), ]
require(randomForest)
forest1 <- randomForest(over50k ~ ., data = trainSmall)

## Make predictions according to the random forests model ##
forestPreds <- predict(forest1, newdata = test)
table(test$over50k, forestPreds)
##         forestPreds
##           <=50K  >50K
##    <=50K   9614    99
##    >50K    2028  1050
(9614 + 1050)/ nrow(test)
## [1] 0.8337112

## What are the most frequently used variables in the random forest splits ##
vu <- varUsed(forest1, count = T)
vusorted <- sort(vu, decreasing = F, index.return = T)
dotchart(vusorted$x, names(forest1$forest$xlevels[vusorted$ix]), col="blue")

varImpPlot(forest1)

require(caret)
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
## Warning: package 'ggplot2' was built under R version 3.1.3
require(e1071)
## Loading required package: e1071
numFolds <- trainControl(method = "cv", number = 10)
CARTgrid <- expand.grid(.cp = seq(.002, .1, .002))
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = CARTgrid)

CART2 <- rpart(over50k ~ ., data = train, method = "class", cp = .002)
CARTpred3 <- predict(CART2, newdata = test, type = "class")
table(test$over50k, CARTpred3)
##         CARTpred3
##           <=50K  >50K
##    <=50K   9178   535
##    >50K    1240  1838
(9178 + 1838)/ nrow(test)


## How many splits are there
prp(CART2)










