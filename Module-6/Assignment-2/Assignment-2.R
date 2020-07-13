parole = read.csv("parole.csv")

#Q1
str(parole)
#Ans: 675

#Q2:
sum(parole$violator==1)
#Ans: 78

#Q3:
table(parole$state)
table(parole$crime)

#Q4:
parole$state<-as.factor(parole$state)
parole$crime<-as.factor(parole$crime)
summary(parole)

#Q5:
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
str(train)
str(test)
#Ans: 70%-Training,30%-Testing

#Q9:
model1 <- glm(violator ~.,data=train,family="binomial")
summary(model1)
#Ans: race,state4.multiple.offenses

#Q11:
-4.2411574 + 0.3869904*1 + 0.8867192*1 - 0.0001756*50 + 0.4433007*0 + 0.8349797*0 - 3.3967878*0 - 0.1238867*3 + 0.0802954*12 + 1.6119919*0 + 0.6837143*1 - 0.2781054*0 - 0.0117627*0
exp(-1.700629)
#Ans: 0.1825687

#Q12:
1/(1+exp(1.700629))
#Ans: 0.1543831

#Q13:
prediction1=predict(model1,newdata = test,type="response")
summary(prediction1)
#Ans: 0.907300

#Q14
table(test$violator,prediction1>=0.5)
12/(12+11)
#Ans:0.5217391

#Q15
167/(12+167)
#Ans:0.9329609

#Q16
(12+167)/(167+12+11+12)
#Ans: 0.8861386

#Q17
table(test$violator)
179/(179+23)
#Ans: 0.8861386
