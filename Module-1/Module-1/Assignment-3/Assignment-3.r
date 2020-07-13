CPS = read.csv("CPSData.csv")
summary(CPS)

#Q1
str(CPS)

#Q2
which.max(table(CPS$Industry))

#Q3
which.min(sort(table(CPS$State)))

#Q4
which.max(sort(table(CPS$State)))

#Q5
prop.table(table(CPS$Citizenship))
0.88832615+0.05386818

#Q6
table(CPS$Race, CPS$Hispanic)

#Q7
summary(CPS)

#Q8
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

#Q9,10
table(CPS$State, is.na(CPS$MetroAreaCode))

#Q11
table(CPS$Region, is.na(CPS$MetroAreaCode))

#Q12,13
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap<-read.csv("MetroAreaCodes.csv")
CountryMap<-read.csv("CountryCodes.csv")

#Q14
str(MetroAreaMap)

#Q15
str(CountryMap)

CPS<-merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
#Q16,17
str(CPS)

#Q18
sort(table(CPS$MetroArea))

#Q19
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

#Q20
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

#Q21
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

#Q22
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

#Q23
summary(CPS)

#Q24
sort(table(CPS$Country))

#Q25
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

#Q26
sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))

#Q27
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))

#Q28
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))
