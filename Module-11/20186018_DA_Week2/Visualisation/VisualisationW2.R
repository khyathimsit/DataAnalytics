#reading the data

Climatechanges = read.csv("climate_change.csv")
str(Climatechanges)
table(Climatechanges$Year)

#plotting
install.packages("ggplot2")
library(ggplot2)
Scatterplot = ggplot(Climatechanges, aes(x=N2O, y= Temp)) + geom_line()
Scatterplot + labs(x = "Concentration of N2O", y = "Temperature")

#Building model
model = lm(Temp ~ Aerosols, data = Climatechanges)
summary(model)
ggplot(Climatechanges, aes(x=Aerosols, y= Temp)) + geom_line()
ggplot(Climatechanges, aes(x=Aerosols, y= Temp)) + geom_line() + stat_smooth(method = "lm")

#intl.csv
INTL = read.csv("intl.csv")
str(INTL)
#Bar chart
ggplot(INTL, aes(x=Region, y = PercentOfIntl)) + geom_bar(stat = "identity", fill = "red") + geom_text(aes(label=PercentOfIntl)) + ylab("International Students Percentage") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))

#Pie Chart
ggplot(INTL, aes(x="", y=PercentOfIntl, fill=Region))+ geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)