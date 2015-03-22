##

## Week 2 - READING TEST SCORES

# load data
pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")
nrow(pisaTrain)

#Gender averages
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

summary(pisaTrain)

# Remove NA
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTrain)
nrow(pisaTest)

# Reference levels factors
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# Build linear model
lmScore <- lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)

#  (RMSE) of lmScore
SSE <- sum(lmScore$residuals^2)
RMSE <- sqrt(SSE/nrow(pisaTrain))
#Alternative method of getting RMSE
sqrt(mean(lmScore$residuals^2))


# Prediction
predTest <- predict(lmScore, newdata=pisaTest)
summary(predTest)

SSE.test <- sum((predTest - pisaTest$readingScore)^2)
RMSE.test <- sqrt(SSE.test/nrow(pisaTest))


# BASELINE PREDICTION AND TEST-SET SSE
base <- mean(pisaTrain$readingScore)
SST.test <- sum((base - pisaTest$readingScore)^2)
SST.test

R2.test <- 1 - SSE.test/SST.test
R2.test

