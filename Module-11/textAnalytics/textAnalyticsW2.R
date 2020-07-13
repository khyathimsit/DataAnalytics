#Reading the data
Energies = read.csv("energy_readings.csv", stringsAsFactors = FALSE)
str(Energies)

responses = subset(Energies , responsive == 1)
nrow(responses)/nrow(Energies)
#Ans: 0.1625731

#To preprocess the data
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(Energies$email))
corpus
corpus = tm_map(corpus, tolower)
corpus  = tm_map(corpus, removePunctuation)
corpus  = tm_map(corpus,removeWords, c(stopwords("english")))
corpus  = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.95)
PureEnergies = as.data.frame(as.matrix(dtm))
PureEnergies$responsive = Energies$responsive
str(PureEnergies)
#building the model
library(caTools)
set.seed(1500)
split = sample.split(PureEnergies$responsive , SplitRatio = 0.75)
Energy_train = subset(PureEnergies, split == TRUE)
Energy_test = subset(PureEnergies, split == FALSE)
library(rpart)
library(rpart.plot)
emailCart = rpart(responsive ~ ., data = Energy_train, method = "class")
prp(emailCart)

#Making predictions
pred = predict(emailCart, newdata = Energy_test)
pred
pred.prob = pred[, 2]
table(Energy_test$responsive, pred.prob > 0.5)
table(Energy_test$responsive, pred.prob > 0.7)
table(Energy_test$responsive, pred.prob > 0.9)

#calculating accuracies
#for 0.6
table(Energy_test$responsive, pred.prob > 0.6)
(174+16)/(174+5+19+16)
#Ans: 0.8878505

#for 0.8
table(Energy_test$responsive, pred.prob > 0.8)
(174+16)/(174+5+19+16)
#Ans: 0.8878505

#ROC curver
library(ROCR)
predROCR = prediction(pred.prob, Energy_test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values

#Ans: 0.743735