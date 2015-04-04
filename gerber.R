gerber = read.csv("/home/tony/data-science/MITx-The-Analytics-Edge/data-files/AnalyticsEdgeMIT/gerber.csv")
# Proportion of voters from dataset
prop.table(table(gerber$voting))

tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

library(rpart)
library(rpart.plot) #prp plotting
library(caTools)
library(e1071)
library(caret)
library(randomForest)
library(ROCR) #prediction

## Build a logistic regression model for voting using the four treatment group
## variables as the independent variables
logReg <- glm(voting ~ hawthorne + civicduty + self + neighbors, 
              data=gerber, family="binomial")
logPred <- predict(logReg, type="response")
table(gerber$voting, logPred > 0.3)
## (134513+51966)/(134513+100875+56730+51966) = 0.542
table(gerber$voting, logPred > 0.5)
## 235388/(235388+108696) = 0.6841004

ROCRpred = prediction(logPred, gerber$voting)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
## This is a weak predictive model

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
## There are no splits in the tree, because none of the variables make a big enough
## effect to be split on.

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, 
                   data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)


CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)
# men
0.345818-0.302795
# women
0.334176-0.290456


LogModelSex = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(LogModelSex)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
predict(LogModel2, newdata=Possibilities, type="response")

##############################################
### Letter Recognition

letters = read.csv("/home/tony/data-science/MITx-The-Analytics-Edge/data-files/AnalyticsEdgeMIT/letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
lettersTrain <- subset(letters, split==TRUE)
lettersTest <- subset(letters, split==FALSE)

CARTb = rpart(isB ~ . - letter, data=lettersTrain, method="class")
CARTbPredict <- predict(CARTb, newdata=lettersTest, type="class")
table(lettersTest$isB, CARTbPredict)

## Building a random forest
set.seed(1000)
lettersRF <- randomForest(isB ~ . - letter, data=lettersTrain)
lettersRFPredict <- predict(lettersRF, newdata=lettersTest, type="class")
table(lettersTest$isB, lettersRFPredict)


# Multivariable classification
#letters <- letters[-18]
letters$letter = as.factor(letters$letter)
set.seed(2000)
splitM = sample.split(letters$letter, SplitRatio = 0.5)
lettersTrainM <- subset(letters, splitM==T)
lettersTestM <- subset(letters, splitM==F)
table(lettersTestM$letter)

CARTm = rpart(letter ~ .-isB, data=lettersTrainM, method="class")
CARTmPredict <- predict(CARTm, newdata=lettersTestM, type="class")
table(lettersTest$letter, CARTmPredict)



CARTletter = rpart(letter ~ . - isB, data=train2, method="class")













