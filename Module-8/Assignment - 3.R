# QUESTION 1.1

emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)

# QUESTION 1.2

table(emails$spam)

# QUESTION 1.3

emails$text[1]

# QUESTION 1.5

max(nchar(emails$text))

# QUESTION 1.6

which.min(nchar(emails$text))

# QUESTION 2.1

corpus = VCorpus(VectorSource(emails$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
summary(dtm)

# QUESTION 2.2

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

# QUESTION 2.3

sort(colSums(emailsSparse))
# OR
which.max(colSums(emailsSparse))

# QUESTION 2.4

emailsSparse$spam = emails$spam
sort(colSums(subset(emailsSparse, spam == 0)))

# QUESTION 2.5

sort(colSums(subset(emailsSparse, spam == 1)))

# QUESTION 3.1
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam~., data=train, family="binomial")
spamCART = rpart(spam~., data=train, method="class")
set.seed(123)
spamRF = randomForest(spam~., data=train)

predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]

table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)

# QUESTION 3.2

summary(spamLog)

# QUESTION 3.3

prp(spamCART)

# QUESTION 3.4

table(train$spam, predTrainLog > 0.5)
(3052+954)/nrow(train)

# QUESTION 3.5

predictionTrainLog = prediction(predTrainLog, train$spam)
as.numeric(performance(predictionTrainLog, "auc")@y.values)

# QUESTION 3.6

table(train$spam, predTrainCART > 0.5)
(2885+894)/nrow(train)

# QUESTION 3.7

predictionTrainCART = prediction(predTrainCART, train$spam)
as.numeric(performance(predictionTrainCART, "auc")@y.values)

# QUESTION 3.8

table(train$spam, predTrainRF > 0.5)
(3013+914)/nrow(train)

# QUESTION 3.9

predictionTrainRF = prediction(predTrainRF, train$spam)
as.numeric(performance(predictionTrainRF, "auc")@y.values)

# QUESTION 4.1

predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)[,2]
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]
table(test$spam, predTestLog > 0.5)
(1257+376)/nrow(test)

# QUESTION 4.2

predictionTestLog = prediction(predTestLog, test$spam)
as.numeric(performance(predictionTestLog, "auc")@y.values)

# QUESTION 4.3

table(test$spam, predTestCART > 0.5)
(1228+386)/nrow(test)

# QUESTION 4.4
predictionTestCART = prediction(predTestCART, test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values)

# QUESTION 4.5

table(test$spam, predTestRF > 0.5)
(1290+385)/nrow(test)

# QUESTION 4.6

predictionTestRF = prediction(predTestRF, test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values)
