# TO read the data
WHO = read.csv("WHO.csv")

#To understand the structure of the data
str(WHO)

#Normal plotting in base R
plot(WHO$GNI, WHO$FertilityRate)

#Installing ggplot packages
install.packages("ggplot2")
library(ggplot2)

#We should create ggplot with 3 elements...data, aesthetic, geometric
#First create ggplot object using first two elements i.e data and aesthetic

scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))

#Now add the geometric element to the object... In this case it is points
scatterplot + geom_point()

#For plotting using lines
scatterplot + geom_line()

#Now add color, size, shape to geometric points
#Shapes have numbers related to their shape
scatterplot + geom_point(color = "blue", size = 3, shape = 17)
#Darkredstars
scatterplot + geom_point(color = "darkred", size = 3, shape = 8)
#Adding title
scatterplot + geom_point(color = "darkred", size = 3, shape = 8) + ggtitle("Fertility Rate vs GNI")

#To add to pdf
#first store the plot in a variable
fertilityplot = scatterplot + geom_point(color = "darkred", size = 3, shape = 8) + ggtitle("Fertility Rate vs GNI")
pdf("FinalPlotexample1.pdf")
print(fertilityplot)
dev.off()

#Color the points according to the region
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

#Color the points according to the life expectancy
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

#linearmodel
model1 = lm(Under15 ~ FertilityRate, data = WHO)
summary(model1)
model2 = lm(Under15 ~ log(FertilityRate), data = WHO)
summary(model2)

#To plot linear regression line using ggplot, we need to add another layer
#By default ggplot gives 95% confidence plot in lm, we can change that using level parameter in stat_smooth
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)

#To delete confidence level
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE, color = "orange")