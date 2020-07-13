FluTrain = read.csv("FluTrain.csv")

# QUESTION 1

hist(FluTrain$ILI, breaks = 100)

# QUESTION 2, 3

plot(log(FluTrain$ILI), log(FluTrain$Queries))

# QUESTION 4

FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

# QUESTION 5

cor(log(FluTrain$ILI), FluTrain$Queries)
# correalation_value ^ 2 =  R-squaredvalue

# QUESTION 6

FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

# QUESTION 7

subset(FluTest, Week=="2012-03-11 - 2012-03-17")
# relative_error = (Observed ILI - Estimated ILI)/Observed ILI

(2.293422 - 2.187378)/2.293422

# QUESTION 8

SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
RMSE

# QUESTION 9

# the value of -2 passed to lag means to return 2 observations before the current one;
# a positive value would have returned future observations.
# na.pad=TRUE means to add missing values for the first two weeks of our dataset, where we can't compute the data from 2 weeks earlier.

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)

# QUESTION 10

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

# QUESTION 11, 12, 13

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

# QUESTION 14, 15, 16

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)

# QUESTION 17

FluTrain$ILI[nrow(FluTrain)-1]

# QUESTION 18

FluTrain$ILI[nrow(FluTrain)]

# QUESTION 19

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2 - FluTest$ILI)^2)
SSE

RMSE = sqrt(SSE/nrow(FluTest))
RMSE

# QUESTION 20

SSE = sum((PredTest1 - FluTest$ILI)^2)
SSE

RMSE = sqrt(SSE/nrow(FluTest))
RMSE

