census = read.csv("census.csv")

#Q 1.1
library(caTools)
set.seed(2000)
split1 = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split1==TRUE)
test = subset(census, split1==FALSE)
census_logmodel = glm( over50k ~ . , family="binomial", data = train)
summary(census_logmodel)

#Q 1.2
predictTest = predict(census_logmodel, newdata = test, type = "response")
table(test$over50k, predictTest >= 0.5)
(9051+1888)/(9051+662+1190+1888)

# Q 1.3
table(test$over50k)
9713/(9713+3078)

# Q 1.4
library(ROCR)
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCpred, "auc")@y.values)

# Q 2.1, 2.2, 2.3
library(rpart)
library(rpart.plot)
censustree = rpart( over50k ~ . , method="class", data = train)
prp(censustree)

# Q 2.4
predictTest = predict(censustree, newdata = test, type = "class")
table(test$over50k, predictTest)
(9243+1596)/(9243+470+1482+1596)

# Q 2.6
library(ROCR)
predictTest = predict(censustree, newdata = test)
predictTest = predictTest[,2]

 # Compute the AUC:
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Q 3.1
set.seed(1)
censusrf = randomForest(over50k ~ . , data = trainSmall)
predictTest = predict(censusrf, newdata=test)
table(test$over50k, predictTest)
(9614+1050)/nrow(test)

# Q 3.2
vu = varUsed(MODEL, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(MODEL$forest$xlevels[vusorted$ix]))


# Q 3.3
varImpPlot(MODEL)

# Q 4.1
library(caret)
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train( over50k ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )


# Q 4.2
model = rpart(over50k~., data=train, method="class", cp=0.002)
predictTest = predict(model, newdata=test, type="class")
table(test$over50k, predictTest)
(9178+1838)/(9178+535+1240+1838) 

# Q 4.3
prp(model)