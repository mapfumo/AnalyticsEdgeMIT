##
## Author: Antony Mapfumo
## Date: 22 March 2015

## Week 2 - Climate change - Linear Regression
## In this problem, we will attempt to study the relationship 
## between average global temperature and several other factors.

# load and subset the data into training and test
climate <- read.csv("climate_change.csv")
climate_test <- subset(climate, Year > 2006) # testing set
climate <- subset(climate, Year <= 2006) # training set

# training set linear model, exclude Year and month
training_model <- lm(Temp ~ . - Year - Month, data=climate)
summary(training_model)
SSE <- sum(training_model$residuals^2)
SST <- sum((climate$Temp-mean(climate$Temp))^2)
R2 <- 1 - SSE/SST
RMSE <- sqrt(SSE/nrow(climate))

# multicolinearity
cor(climate$N2O, climate)
cor(climate$CFC.11, climate)
cor(climate) #can calculate all correlations using this

# Given that the correlations are so high, let us focus on the N2O 
# variable and build a model with only MEI, TSI, 
# Aerosols and N2O as independent variables.
training_model_2 <- lm(Temp ~ N2O + MEI + TSI + Aerosols, data=climate)
summary(training_model_2) 

# Automatically building the model using the step function
training_model_Step <- step(training_model)
summary(training_model_Step)

# Predict training_model_Step
predicted <- predict(training_model_Step, newdata=climate_test)
RMSE.test <- sqrt(SSE.test/nrow(climate_test))
# SST is based on the "baseline" model trained from the training set
SST.test <- sum((mean(climate$Temp)-climate_test$Temp)^2)
R2.test <- 1 - SSE.test/SST.test

## It is interesting to note that the step function does not address the 
# collinearity of the variables, except that adding highly correlated 
# variables will not improve the R2 significantly. The consequence of 
# this is that the step function will not necessarily produce a very 
# interpretable model - just a model that has balanced quality and 
# simplicity for a particular weighting of quality and simplicity (AIC).
