mvt <- read.csv("mvtWeek1.csv")
View(mvt)

#Q1,2,3
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

hist(mvt$Date, breaks = 100)

#Q4
boxplot(mvt$Date ~ mvt$Arrest)

#Q5
table(mvt$Arrest, mvt$Year==2001)
2152/(18517+2152)

#Q6
table(mvt$Arrest, mvt$Year==2007)
1212/(13068+1212)

#Q7
table(mvt$Arrest, mvt$Year==2012)
550/(13542+550)

#Q8
sort(table(mvt$LocationDescription))

Top5<-subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")

str(Top5)

#Q9
which.max(table(Top5$Weekday[Top5$LocationDescription == "GAS STATION"]))

#Q10
 which.min(table(Top5$Weekday[Top5$LocationDescription=="DRIVEWAY - RESIDENTIAL"]))