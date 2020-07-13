training_data = read.csv("machine.csv");
#Q1 --- Compute the model R2 (the "Multiple R-squared" value)? Consider the entire dataset as training dataset 
lmmodel = lm(training_data$PRP ~ training_data$MYCT + training_data$MMIN +training_data$MMAX + training_data$CACH + training_data$CHMIN +training_data$CHMAX + training_data$X, data = training_data)
summary(lmmodel)
#ans:0.87

#Q2 --- Which variables are significant in the model? We will consider a variable significant only if the p-value is below 0.05
summary(lmmodel)
#Ans: training_data$MYCT
#     training_data$MMIN
#     training_data$MMAX
#     training_data$CACH 
#     training_data$CHMAX

#Q3 --- Compute the correlations between all the variables in the training set. Which of the following independent variables is MMAX highly correlated with (absolute correlation greater than 0.7)
machine = training_data
machine$VendorName = NULL
machine$ModelName = NULL
cor(machine)
#Ans: MMIN(0.75)
#     PRP(0.8)

#Q4 --- Which of the independent variable is highly correlated with PRP?
machine = training_data
machine$VendorName = NULL
machine$ModelName = NULL
cor(machine)
#Ans :MMAX(0.865)

#Q5 --- Given that the correlations are so high, let us focus on the MMAX variable and build a model with only variables which have correlation is between -0.3 to 0.3 with MMAX. Compute the coefficient of MMAX in this reduced model.
model = lm(training_data$PRP ~ training_data$MMAX + training_data$MYCT, data = training_data)
summary(model)
#Ans: 0.12

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        -3.998e+01  1.067e+01  -3.748 0.000232 ***
 # training_data$MMAX  1.200e-02  5.178e-04  23.180  < 2e-16 ***
 # training_data$MYCT  1.587e-02  2.332e-02   0.680 0.496965  

#Q6: Compute the above reduced model R2
summary(model)
#Ans: 0.7498
#Multiple R-squared:  0.7498

#Q7: Compute the R2 value of the model produced by the step function
func= step(model)
summary(func)
#Ans: 0.7492
#Multiple R-squared:  0.7492

#Q8:Split your data set into train and test sets(75: 25). Compute the testing set R2 using the model produced from the step function on trained data set. (Set seed = 6) 
install.packages(caTools)
library(caTools)
set.seed(6)
sample = sample.int(n=nrow(machine), size = floor(0.75*nrow(machine)),replace=F)
train = machine[sample,]
test = machine[-sample]
model = lm(training_data$PRP ~ training_data$MMAX + training_data$MYCT, data = training_data)
prediction = predict(model,newdata = train)