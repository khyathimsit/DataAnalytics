pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

#Q1
str(pisaTrain)

#Q2
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

#Q3
colSums(is.na(pisaTrain)) > 0
colnames(pisaTrain)[colSums(is.na(pisaTrain)) > 0]

#Q4
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)

#Q5
levels(pisaTrain$raceeth)

#Q6
#Here we have male, grade and raceeth. Male has only 2 levels(0,1). So it is not a valid one.
# raceeth is an unordered factor and grade has different levels (8,9,10,..).

#Q10
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore ~. , data = pisaTrain)
summary(lmScore)

#Q11
lmScore = lm(readingScore ~. , data = pisaTrain)
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

#Q12
summary(lmScore)
#grade(estimate)-(StudentA_grade-StudentB_grade)

#Q13
summary(lmScore)

#Q14
redTest = predict(lmScore, newdata = pisaTest)

#Q15
SSE = sum((redTest - pisaTest$readingScore)^2)

#Q16
RMSE = sqrt(SSE/nrow(pisaTest))

#Q17
mean(pisaTrain$readingScore)

#Q18
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)

#Q19
testSetRsquared = 1 - SSE/SST
