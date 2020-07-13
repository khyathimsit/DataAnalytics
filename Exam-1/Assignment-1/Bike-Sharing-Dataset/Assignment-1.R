hour = read.csv("hour.csv")

#Q1
str(hour)
(or)
nrow(hour)

#Q2
str(hour)

#Q3
max(hour$temp) - min(hour$temp)

#Q4
table(hour$workingday)

#Q5
nrow(subset(hour, season == 1))

#Q6
summary(hour)

#Q7
DateConvert = strftime(hour$dteday, "%Y-%m-%d")
count = subset(hour,DateConvert>= "2012-05-18" & DateConvert <= "2012-05-21")
sum(count$cnt)

#Q8
tapply(hour$cnt,hour$dteday,mean)
head(sort(tapply(hour$cnt,hour$dteday,mean)))

#Q9
day = subset(hour, hour$yr==0)
sort(tapply(day$cnt, day$weekday,mean))

#Q10
hist(hour$cnt,breaks = 100)
(or)
boxplot(hour$cnt ~ hour$yr)

#Q11
meanval = mean(hour$cnt)
dayval = subset(hour, hour$yr==1)
value = subset(dayval, dayval$cnt>meanval)
table1 = table(value$hr)
