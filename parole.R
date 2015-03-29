## Week 3 - Parole

parole <- read.csv("parole.csv")
str(parole)
summary(parole)

# How many parole violators
table(parole$violator)
# convert factor vaiables
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole)

# split the dataset
library(caTools)
set.seed(144)
split <- sample.split(parole$violator, SplitRatio=0.7)
train <- subset(parole, split==TRUE)
test <- subset(parole, !split)

# build logistic regression model
log1 <- glm(violator ~., data=train, family="binomial")
summary(log1)

male_parolee <- list(male=1, race=1, age=50, state=1, time.served=3, max.sentence=12,
            multiple.offenses=0, crime=2)
male_parolee$state <- factor(male_parolee$state, levels=1:4)
male_parolee$crime <- factor(male_parolee$crime, levels=1:4)

exp(predict(log1, newdata=data.frame(male_parolee)))
predict(log1, newdata=data.frame(male_parolee), type="response")

pred.test <- predict(log1, newdata=test, type="response")
summary(pred.test)
table(test$violator, pred.test >= 0.5)

# sensitivity
12/(11+12) # 0.5217391
# specificity
167/(167+12) # 0.9329609
# accuracy
(167+12)/nrow(test)

table(test$violator)


# ROCR AUC score
library(ROCR)
roc1 <- prediction(pred.test, test$violator)
performance(roc1, "auc")@y.values # 0.8945834
plot(performance(roc1, "tpr", "fpr"), colorize=T,
     print.cutoffs.at=seq(0,1,0.1))











