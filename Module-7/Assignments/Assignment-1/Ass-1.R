gerber = read.csv("gerber.csv")

#Q 1.1
table(gerber$voting)
108696/(108696+235388)
#Ans: 0.3158996

#Q 1.2:
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$neighbors, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
#Ans: 0.3779482

#Q 1.3
model = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)
summary(model)
#Ans: civicduty, hawthorne, self, neighbours

#Q 1.4
predict_model <- predict(model, type="response")
table(gerber$voting, predict_model >= 0.3)
(134513+51966)/ (134513+51966+100875+56730)
#Ans: 0.5419578

#Q 1.5
table(gerber$voting, predict_model >= 0.5)
235388/(235388+108696)
#Ans: 0.6841004

#Q 2.1
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

#Q 2.2
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)

#Q 2.3
CARTmodel3 <- rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data = gerber, cp = 0.0)
prp(CARTmodel3)

#Q 3.1
controlmodel <- rpart(voting ~ control, data = gerber, cp = 0.0)
controlSexmodel <- rpart(voting ~ control + sex, data = gerber, cp = 0.0)
prp(controlmodel, digits=6)

#Q 3.2
prp(controlSexmodel, digits=6)

#Q 3.3
cs_logReg <- glm(voting ~ sex + control, data = gerber, family = binomial)
summary(cs_logReg)

#Q 3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(cs_logReg, newdata=Possibilities, type="response")

#Q 3.5
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)