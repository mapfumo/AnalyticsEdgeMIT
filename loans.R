## Week 3
loans <- read.csv("loans.csv")
str(loans)
summary(loans)

# %age loans not paid in full
prop.table(table(loan$not.fully.paid))

#install.packages("mice")
library(mice)
#loans <- read.csv("loans_imputed.csv")
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

# split data
library(caTools)
set.seed(144)
split <- sample.split(loans$not.fully.paid, SplitRatio=0.7)
train <- subset(loans, split)
test <- subset(loans, !split)
# logistic regression model using all independent variables
log1 <- glm(not.fully.paid ~., data=train, family="binomial")
summary(log1) # int.rate is not significant, counter-intuitively

test$predict.risk <- predict(log1, newdata=test, type="response")
table(test$not.fully.paid, test$predict.risk > 0.5)
# accuracy
mean(test$not.fully == is.numeric(test$predict.risk > 0.5))
# baseline
mean(test$not.fully == 0)

fico_coef <- -9.406e-03
A <- 700
B <- 710
fico_coef*A-fico_coef*B

exp(fico_coef * A)/ exp(fico_coef * B)


test$predicted.risk <- predict(log1, newdata = test, type = "response")
table(test$not.fully.paid, test$predicted.risk >= .5)

model2 <- glm(not.fully.paid ~ int.rate, data = train, family = binomial)
summary(model2)


predict2 <- predict(model2, newdata = test, type = "response")
max(predict2)

table(predict2 > .5)

ROCRpred2 <- prediction(predict2, test$not.fully.paid)
auc2 = as.numeric(performance(ROCRpred2, "auc")@y.values)
auc2

test$profit <- exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] <- -1
summary(test$profit)
max(test$profit) * 10

highInterest <- subset(test, int.rate > 0.15)
mean(highInterest$profit)

table(highInterest$not.fully.paid)
110/ (110 + 327)


cutoff <- sort(highInterest$predicted.risk, decreasing=FALSE)[100]
cutoff

selectedLoans <- subset(highInterest, predicted.risk <= cutoff)
nrow(selectedLoans)















