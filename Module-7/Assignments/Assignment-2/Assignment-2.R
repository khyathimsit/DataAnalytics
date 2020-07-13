letters = read.csv("letters_ABPR.csv")

#Q 1.1
letters$isB = as.factor(letters$letter == "B")
set.seed(1000)
split1 = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split1 == TRUE)
test = subset(letters, split1 == FALSE)
table(test$isB)
1175/(1175+383)

#Q 1.2 What is the accuracy of the CART model on the test set?
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
predictions = predict(CARTb, newdata=test, type="class")
table(test$isB, predictions)
(1118+340)/(1118+57+43+340)

#Q 1.3
RFb = randomForest(isB ~ . - letter, data=train)
predictions = predict(RFb, newdata=test)
table(test$isB, predictions)
(1163+374)/(1163+12+9+374)


#Q 2.1
letters$letter = as.factor( letters$letter )
set.seed(2000)
split1 = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, split1 == TRUE)
test2 = subset(letters, split1 == FALSE)
table(test2$letter)
401/nrow(test)

#Q 2.2
CARTletter = rpart(letter ~ . - isB, data=train2, method="class")
predictLetter = predict(CARTletter, newdata=test2, type="class")
table(test2$letter, predictLetter)
(348+318+363+340)/nrow(test2)

#Q 2.3
set.seed(1000)
RFletter = randomForest(letter ~ . - isB, data=train2)
predictLetter = predict(RFletter, newdata=test2)
table(test2$letter, predictLetter)
(390+380 +393+364)/nrow(test2) 