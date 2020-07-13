#Reading the data

Bank_Data = read.csv("bank-full.csv")

#Install and load caTools library to split the data

library(caTools)
set.seed(1000)
split = sample.split(Bank_Data$y, SplitRatio = 0.6)
Bank_Train = subset(Bank_Data, split == TRUE)
Bank_Test = subset(Bank_Data, split == FALSE)

#bulding logistric regression model

model1 = glm(y ~ age + balance + campaign + duration, data = Bank_Train, family = "binomial")
summary(model1)
model2 = glm(y ~ age + balance + duration, data = Bank_Train, family = "binomial")
summary(model2)

#Ans AIC model1: 16262; AIC model2: 16407
#Best model is model1, because it has lower AIC

#Calculating Sensitivity and Specificity

predictmodel1 = predict(model1, type ="response")
table(Bank_Train$y, predictmodel1 >= 0.5)
522/(522+2651)
23605/(23605+348)
#Ans: Sensitivity: 0.1645131; Specificity: 0.9854715

#Make preditions on test set
predict1Test = predict(model1, type="response", Bank_Test)  
table(Bank_Test$y, predict1Test >= 0.5)
library(ROCR)
ROCRpred = prediction(predict1Test, Bank_Test$y)
as.numeric(performance(ROCRpred, "auc")@y.values)
#Ans: AUC value = 0.8179142

#Building Cart Model
library(rpart)
library(rpart.plot)
CARTmodel = rpart(y ~ age + balance + duration, data=Bank_Train) 
prp(CARTmodel)
#Ans: Number of splits: 2

#Making Predictions on model2 using test data
predict2Test = predict(model2, type="response", Bank_Test)
table(Bank_Test$y, predict2Test >= 0.5)
library(ROCR)
ROCRpred2 = prediction(predict2Test, Bank_Test$y)
as.numeric(performance(ROCRpred2, "auc")@y.values)
#Ans: AUC value for model2 : 0.8097155

#Proportion of married and technician job
subset1 = subset(Bank_Data, job == "technician" & marital == "married")
nrow(subset1)
nrow(Bank_Data)
4052/45211
#Ans: proportion = 0.08962421