library(SnowballC)
library(tm)
tweets = read.csv("/library/Data Science/MITx-The-Analytics-Edge/data-files/tweets.csv", stringsAsFactors=F)
tweets$Negative = as.factor(tweets$Avg<=-1)
table(tweets$Negative)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus = tm_map(corpus, stemDocument)
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies, lowfreq = 20)  # find most frequent terms/words
sparse = removeSparseTerms(frequencies, 0.995) # remove less frequent terms
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~., data=trainSparse, method="class") # build a classification model
prp(tweetCART)
predictCART = predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART) # confusion matrix
(294+18)/length(predictCART)
table(testSparse$Negative) # baseline model
300/(300+55)

# How about random forest model
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative~., data=trainSparse)
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)
(291+26)/length(predictRF)

# Logistic Model
tweetLg = glm(Negative~., data=trainSparse, family=binomial)
predictLg = predict(tweetLg, newdata=testSparse, type="response")
table(testSparse$Negative, predictLg>=0.5)
(257+34)/length(predictLg)
