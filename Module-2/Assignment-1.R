climate_change = read.csv("climate_change.csv")

#Splitting the training and testing data

training_data = subset(climate_change, Year < 2007)
testing_data = subset(climate_change, Year >2006)

#Creating the model
model = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = training_data)

#Q1,2
summary(model)

#Q4
cor(training_data) 
 # or
cor(training_data$N2O, training_data)

#Q5
cor(training_data) 
# or
cor(training_data$CFC.11,training_data)

#Q6,7
model = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = training_data)
summary(model)

#Q8,9
old_model = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = training_data)
new_model = step(old_model)
summary(new_model)

#Q10
prediction =predict(new_model, newdata = testing_data)
SSE = sum((prediction - testing_data$Temp)^2)
SST = sum((mean(training_data$Temp) - testing_data$Temp)^2)
R2 = 1 - SSE/SST

