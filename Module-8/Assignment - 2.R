# QUESTION 1.1
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
summary(nchar(trials$abstract)) # or
max(nchar(trials$abstract))

# QUESTION 1.2
table(nchar(trials$abstract) == 0)

# QUESTION 1.3
trials$title[which.min(nchar(trials$title))]

# QUESTION 2.1
corpusTitle = VCorpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))
tr(dtmTitle)
str(dtmAbstract)

# QUESTION 2.3
csAbstract = colSums(dtmAbstract)
which.max(csAbstract)

# QUESTION 3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

# QUESTION 3.2
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
str(dtm)

# QUESTION 3.3
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
table(train$trial)
730/(730+572)

# QUESTION 3.4
trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)

# QUESTION 3.5
predTrain = predict(trialCart)[,2]
summary(predTrain)

# QUESTION 3.7
table(train$trial, predTrain >= 0.5)
#accuracy 
(631+441)/(631+441+99+131)
#sensitivity 
441/(441+131)
# specificity 
631/(631+99)

# QUESTION 4.1
predTest = predict(trialCART, newdata=test)[,2]
table(test$trial, predTest >= 0.5)
(261+162)/(261+162+83+52)

# QUESTION 4.2
library(ROCR)
pred = prediction(predTest, test$trial)
as.numeric(performance(pred, "auc")@y.values)
