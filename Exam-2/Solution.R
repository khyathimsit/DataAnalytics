#Logistic Regression and Trees (Classification Problems) 
#Q 1
bank = read.csv("bank-full.csv")
library(caTools)
set.seed(1000)
split1 = sample.split(bank$y, SplitRatio = 0.6)
train = subset(bank, split1 == TRUE)
test = subset(bank, split1 == FALSE)

model1 <- glm(y ~ age+balance+campaign+duration,data=train,family="binomial")
summary(model1)

#Q 3
prediction1=predict(model1,newdata = test,type="response")
library(ROCR)
ROCRpredTest = prediction(prediction1,test$y)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


#Q 2
table(test$y, prediction1 > 0.5)
345/(1771+345)
15727/(15727+242)

#Q 4
library(rpart)
library(rpart.plot)
CART1 = rpart(y ~ age+balance+duration, data=bank, method = "class")
prp(CART1)

#Q 5
prediction2=predict(model2,newdata = test,type="response")
library(ROCR)
ROCRpredTest = prediction(prediction2,test$y)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

#Q 6
table(bank$marital=='married' & bank$job=='technician')
4052/(41159+4052)




#Text Analytics
energy = read.csv("energy_readings.csv")
#Q 1
str(energy)
table(energy$responsive)

#Q 2
corpus = VCorpus(VectorSource(energy$email))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus,PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
dtm
sparse_DTM <- removeSparseTerms(dtm, 0.95)
values <- as.data.frame(as.matrix(sparse_DTM))
colnames(values) <- make.names(colnames(values))
values$responsive <- energy$responsive
split <- sample.split(values$responsive, SplitRatio = 0.75)
train <- subset(values, split == TRUE)
test <- subset(values, split == FALSE)
email1 <- rpart(responsive ~ . , data = train, method = "class")
prp(email1)

#Q 3
predict_energy <- predict(email1, newdata = test)
predict_energy.prob <- predict_energy[ , 2]


#Q 4
CART_acc1 <- table(test$responsive,predict_energy.prob > 0.6)
(173+18)/(173+18+6+17)
CART_acc2 <- table(test$responsive,predict_energy.prob >= 0.8)
(173+17)/(173+17+18+6)

#Q 5
predROCR <- prediction(predict_energy.prob, test$responsive)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE, lwd = 4)
auc_CART <- as.numeric(performance(predROCR, "auc")@y.values)
auc_CART




#Q1
climate = read.csv("climate_change.csv")
str(climate)

#Q2
library(ggplot2)
library(ggmap)
library(maps)
suppressPackageStartupMessages(library(ggplot2))

scatterplot = ggplot(climate, aes(x = N2O, y = Temp))
scatterplot + geom_line()
