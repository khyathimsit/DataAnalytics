statedata = read.csv("statedata.csv")

# to plot 
plot(statedata$x, statedata$y)

# the highest average high school graduation rate of all the states in the region

which.max(tapply(statedata$HS.Grad, statedata$state.region, mean))

# a boxplot of the murder rate by region

boxplot(statedata$Murder ~ statedata$state.region) 

# see that there is an outlier in the Northeast region of the boxplot you just generated. Which state does this correspond to?

NortheastData = subset(statedata, state.region == "Northeast")
table(NortheastData$state.abb, NortheastData$Murder)

# to create a linear regression model

regModel = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(regModel)

# to plot

plot(statedata$Income, statedata$Life.Exp)

#state actually has the lowest life expectancy

statedata$state.name[which.min(statedata$Life.Exp)]

# For which state do we make the largest absolute error

sort(abs(regModel$residuals))


################################

# Forecasting

Elantra = read.csv("elantra.csv")

#split the data

ElantraTrain = subset(Elantra, Year <= 2012)
ElantraTest = subset(Elantra, Year > 2012)

# linear regression model
ElantraLM = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, data=ElantraTrain)

summary(ElantraLM)

# to add month as new variable to model

ElantraLM = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data=ElantraTrain)

# the Month variable modeled as a factor variable

ElantraTrain$MonthFactor = as.factor(ElantraTrain$Month)
ElantraTest$MonthFactor = as.factor(ElantraTest$Month)
ElantraLM = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFactor, data=ElantraTrain)

# correlations

cor(ElantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

# to predict the model

PredictTest = predict(ElantraLM, newdata=ElantraTest)
SSE = sum((PredictTest - ElantraTest$ElantraSales)^2)

