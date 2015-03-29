songs = read.csv("songs.csv")

#songs per year
table(songs$year)
#or
nrow(songs[songs$year==2010,])

# Michael Jackson songs
nrow(songs[songs$artistname=="Michael Jackson",])
# or 
sum(songs$artistname == "Michael Jackson")
michael = subset(songs, songs$artistname == "Michael Jackson" & songs$Top10==1)
michael[michael$artistname == "Michael Jackson", c("songtitle", "Top10")]

table(songs$timesignature)
max(table(songs$timesignature))

# highest pitch
head(songs$songtitle[order(songs$tempo, decreasing=T)])
# or
which.max(songs$tempo)
songs$songtitle[6206]

# dataset splitting
SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)


# Creating the prediction model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain <- SongsTrain[, !names(SongsTrain) %in% nonvars]
SongsTest <- SongsTest[, !names(SongsTest) %in% nonvars]
str(SongsTrain)

# build logistic regression
SongsLog1  <- glm(Top10 ~ ., data=SongsTrain, family="binomial")
summary(SongsLog1)

# Creating the Prediction Model


SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family="binomial")
summary(SongsLog3)

# predicting on test set
pred.test <- predict(SongsLog3, newdata=SongsTest, type="response") > 0.45
table(SongsTest$Top10, pred.test)
mean(as.numeric(pred.test) == SongsTest$Top10) #0.8793566


# baseline model: pick most frequent outcome
mean(SongsTest$Top10 == names(sort(table(SongsTest$Top10), dec=T))[1]) #8418231
# or
table(SongsTest$Top10)
314/(314+59)

309/(309+5)

